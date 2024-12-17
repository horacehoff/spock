#![allow(clippy::too_many_lines)]
#[path = "types/array.rs"]
mod array;
#[path = "functions/builtin.rs"]
mod builtin;
#[path = "types/file.rs"]
mod file;
#[path = "types/float.rs"]
mod float;
#[path = "types/integer.rs"]
mod integer;
#[path = "functions/namespaces.rs"]
mod namespaces;
#[path = "parser/parser.rs"]
mod parser;
#[path = "parser/parser_functions.rs"]
mod parser_functions;
mod preprocess;
#[path = "types/string.rs"]
mod string;
mod util;

use crate::array::array_ops;
use crate::builtin::builtin_functions;
use crate::float::float_ops;
use crate::integer::integer_ops;
use crate::namespaces::namespace_functions;
use crate::parser::{parse_code, BasicOperator, Types};
use crate::parser_functions::parse_functions;
use crate::preprocess::preprocess;
use crate::string::{string_ops, to_title_case};
use crate::util::{error, get_printable_form};
use branches::likely;
use branches::unlikely;
use gxhash::HashMap;
use smol_str::{SmolStr, StrExt as _, ToSmolStr as _};
use snmalloc_rs::SnMalloc;
use std::fs;
use std::io::Write as _;
use std::time::Instant;
use unroll::unroll_for_loops;

#[global_allocator]
static ALLOC: SnMalloc = SnMalloc;

#[unroll_for_loops]
#[inline]
fn process_stack(
    stack_in: &[Types],
    variables: &HashMap<SmolStr, Types>,
    functions: &[(SmolStr, &[SmolStr], &[Types])],
) -> Types {
    let mut output: Types = match stack_in.first().unwrap_or(&Types::Integer(0)) {
        Types::VariableIdentifier(ref var) => variables
            .get(var)
            .unwrap_or_else(|| {
                error(&format!("Unknown variable '{var}'"), "");
                std::process::exit(1)
            })
            .clone(),
        Types::Wrap(ref x) => process_stack(x, variables, functions),
        other => {
            if !matches!(
                other,
                Types::FunctionCall(_, _)
                    | Types::NamespaceFunctionCall(_, _, _)
                    | Types::Priority(_)
                    | Types::Array(_, _, _)
            ) {
                other.clone()
            } else {
                preprocess(variables, functions, other)
            }
        }
    };
    let mut current_operator: BasicOperator = BasicOperator::Null;
    let mut process: Types;
    for p_element in stack_in.iter().skip(1) {
        let element = match p_element {
            Types::VariableIdentifier(var) => variables.get(var).unwrap_or_else(|| {
                error(&format!("Unknown variable '{var}'"), "");
                std::process::exit(1)
            }),
            Types::Wrap(x) => &process_stack(x, variables, functions),
            other => {
                if matches!(
                    other,
                    Types::FunctionCall(_, _)
                        | Types::NamespaceFunctionCall(_, _, _)
                        | Types::Priority(_)
                        | Types::Array(_, _, _)
                ) {
                    process = preprocess(variables, functions, other);
                    &process
                } else {
                    other
                }
            }
        };

        match element {
            Types::Operation(ref op) => current_operator = *op,
            Types::Integer(ref x) => output = integer_ops(*x, &output, current_operator),
            Types::String(ref x) => output = string_ops(x, &output, current_operator),
            Types::Float(ref x) => output = float_ops(*x, &output, current_operator),
            Types::Array(ref x, ref is_parsed, ref is_suite) => {
                output = array_ops(x, &output, current_operator)
            }
            Types::Property(ref x, ref y) => {
                let args: Vec<Types> = y
                    .iter()
                    .map(|arg| process_stack(std::slice::from_ref(arg), variables, functions))
                    .collect();
                match output {
                    Types::String(ref str) => string_props!(str, args, x, output),
                    Types::Float(num) => float_props!(num, args, x, output),
                    Types::Integer(num) => integer_props!(num, args, x, output),
                    Types::Array(ref arr, ref is_parsed, ref is_suite) => {
                        array_props!(arr, args, x, output)
                    }

                    // OBJECTS
                    Types::File(ref filepath) => file_props!(filepath, args, x, output),
                    _ => error(
                        &format!(
                            "Unknown function '{x}' for object {}",
                            get_printable_type!(output)
                        ),
                        "",
                    ),
                }
            }
            Types::Or(ref x) | Types::And(ref x) => {
                let parsed_exp = process_stack(x, variables, functions);
                if_let!(
                    likely,
                    Types::Bool(inbool),
                    output,
                    {
                        if_let!(likely, Types::Bool(sidebool), parsed_exp, {
                            output = Types::Bool(if matches!(element, Types::Or(_)) {
                                inbool || sidebool
                            } else {
                                inbool && sidebool
                            });
                        }, else {
                            error(format!("{parsed_exp:?} is not a Boolean").as_str(), "");
                        });
                    }, else
                    {
                        error(format!("{output:?} is not a Boolean").as_str(), "");
                    }
                );
            }
            Types::Null => {
                if output == Types::Null {
                    match current_operator {
                        BasicOperator::Equal => output = Types::Bool(true),
                        BasicOperator::NotEqual => output = Types::Bool(false),
                        _ => error(
                            &format!(
                                "Cannot perform operation '{current_operator:?}' between Null and Null"
                            ),
                            "",
                        ),
                    }
                }
            }
            _ => error(&format!("TODO {element:?}"), ""),
        }
    }
    output
}

#[inline(always)]
fn process_line_logic(
    line_array: &[Types],
    variables: &mut HashMap<SmolStr, Types>,
    functions: &[(SmolStr, &[SmolStr], &[Types])],
) -> Types {
    for line in line_array.iter() {
        match line {
            Types::Wrap(ref x) => {
                let x = process_line_logic(x, variables, functions);
                if x != Types::Null {
                    return x;
                }
            }
            Types::VariableDeclaration(ref x, ref y, ref is_redeclare) => {
                if !is_redeclare {
                    variables.insert(x.to_smolstr(), process_stack(y, variables, functions));
                } else {
                    let calculated = process_stack(y, variables, functions);
                    if let Some(x) = variables.get_mut(x) {
                        *x = calculated;
                    }
                }
            }
            // Types::VariableRedeclaration(ref x, ref y) => {
            //     let calculated = process_stack(y, variables, functions);
            //     if let Some(x) = variables.get_mut(x) {
            //         *x = calculated;
            //     }
            // }
            Types::NamespaceFunctionCall(ref namespace, ref y, ref z) => {
                let args: Vec<Types> = z
                    .iter()
                    .map(|arg| {
                        if let Types::Wrap(x) = &arg {
                            process_stack(x, variables, functions)
                        } else {
                            process_stack(std::slice::from_ref(arg), variables, functions)
                        }
                    })
                    .collect();
                if unlikely(!namespace_functions(namespace, y, &args).1) {
                    error(
                        &format!("Unknown function '{}'", namespace.join(".") + "." + y),
                        "",
                    );
                };
            }
            Types::FunctionCall(ref x, ref y) => {
                let args: Vec<Types> = y
                    .iter()
                    .map(|arg| process_stack(std::slice::from_ref(arg), variables, functions))
                    .collect();
                let (_, matched) = builtin_functions(x, &args);
                if x == "executeline" && !matched {
                    assert_args_number!("executeline", args.len(), 1_usize);
                    if_let!(likely, Types::String(line), &args[0], {
                        process_stack(&parse_code(line)[0], variables, functions);
                    }, else {
                        error(&format!("Cannot execute line {:?}", &args[0]), "");
                    });
                } else if !matched {
                    let target_function: &(SmolStr, &[SmolStr], &[Types]) = functions
                        .iter()
                        .find(|func| func.0 == *x)
                        .unwrap_or_else(|| {
                            error(&format!("Unknown function '{x}'"), "");
                            std::process::exit(1)
                        });
                    assert_args_number!(&x, args.len(), target_function.1.len());
                    let mut target_args: HashMap<SmolStr, Types> = target_function
                        .1
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| (arg.to_smolstr(), args[i].clone()))
                        .collect();

                    process_function(target_function.2, &mut target_args, functions);
                }
            }
            Types::PropertyFunction(ref a, ref b, ref c, ref d) => {
                let result = process_line_logic(
                    &[Types::FunctionCall(a.to_smolstr(), b.clone())],
                    variables,
                    functions,
                );
                return process_stack(
                    &[result, Types::Property(c.to_smolstr(), d.clone())],
                    variables,
                    functions,
                );
            }
            Types::Condition(ref x, ref y, ref z) => {
                if let Types::Bool(true) = process_stack(x, variables, functions) {
                    let out = process_line_logic(y, variables, functions);
                    if Types::Null != out {
                        // if out != Types::Break {
                        return out;
                        // }
                        // error("Cannot break in a conditional statement","Remove the \"break\" statement");
                    }
                } else {
                    for else_block in z {
                        if else_block.0.is_empty()
                            || process_stack(&else_block.0, variables, functions)
                                == Types::Bool(true)
                        {
                            let out = process_line_logic(&else_block.1, variables, functions);
                            if out != Types::Null {
                                return out;
                            }
                        }
                    }
                }
            }
            Types::Loop(ref x, ref y, ref z) => {
                let loop_array = process_stack(y, variables, functions);
                if let Types::Array(target_array, false, false) = loop_array {
                    variables.insert(x.to_smolstr(), Types::Null);
                    for elem in target_array {
                        if let Some(value) = variables.get_mut(x) {
                            *value = elem;
                        }

                        let out: Types = process_line_logic(z, variables, functions);
                        if out != Types::Null {
                            variables.remove(x);
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    variables.remove(x);
                } else if let Types::String(ref target_string) = loop_array {
                    variables.insert(x.to_smolstr(), Types::Null);
                    for elem in target_string.chars() {
                        if let Some(value) = variables.get_mut(x) {
                            *value = Types::String(elem.to_smolstr());
                        }

                        let out: Types = process_line_logic(z, variables, functions);
                        if out != Types::Null {
                            variables.remove(x);
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    variables.remove(x);
                }
            }
            Types::While(ref x, ref y) => {
                while let Types::Bool(true) = process_stack(x, variables, functions) {
                    let out = process_line_logic(y, variables, functions);
                    if out != Types::Null {
                        if out == Types::Break {
                            break;
                        }
                        return out;
                    }
                }
            }
            Types::FunctionReturn(ref x) => {
                return process_stack(x, variables, functions);
            }
            Types::Break => {
                return Types::Break;
            }
            _ => error(&format!("TODO {line:?}"), ""),
        }
    }
    Types::Null
}

// Everything below this comment is very experimental and work-in-progress as of this moment
// fn simplify(lines: Vec<Types>, store: bool, current_num: i8) -> (Vec<Types>, i8) {
//     let mut test: Vec<Types> = vec![];
//     let mut i: i8 = current_num+1;
//     for x in lines {
//         match x {
//             Types::VariableDeclaration(x, y) => {
//                 let result = simplify(y, true,i);
//                 i = result.1+1;
//                 test.extend(result.0);
//                 test.push(VAR_STORE(x, result.1));
//             }
//             Types::VariableRedeclaration(x, y) => {
//                 let result = simplify(y, true,i);
//                 i = result.1+1;
//                 test.extend(result.0);
//                 test.push(VAR_REPLACE(x, result.1));
//             }
//             Types::FunctionCall(x, y) => {
//                 let mut args: Vec<i8> = vec![];
//                 for x in y {
//                     if let Types::Wrap(w) = x {
//                         let result = simplify(w, true, i);
//                         i = result.1+1;
//                         test.extend(result.0);
//                         args.push(result.1);
//                     } else {
//                         let result = simplify(vec![x], true, i);
//                         i = result.1+1;
//                         test.extend(result.0);
//                         args.push(result.1);
//                     }
//                 }
//                 test.push(FUNC_CALL(x, args));
//             }
//             Types::FunctionReturn(ret) => {
//                 let result = simplify(ret, true, i);
//                 i = result.1+1;
//                 test.extend(result.0);
//                 test.push(FUNC_RETURN(result.1));
//             }
//             Types::While(condition, code) => {
//                 let result = simplify(condition, false, i);
//                 i = result.1+1;
//                 // test.extend(result.0);
//                 let code = simplify(code, false, i);
//                 i = code.1+1;
//                 test.push(WHILE_BLOCK(result.0, code.0))
//             }
//
//             _ => test.push(x),
//         }
//     }
//     if store {
//         test.insert(0, Types::STARTSTORE(i+1));
//         test.push(Types::STOP(i+1))
//     }
//     (test, i+1)
// }
//
// fn execute(
//     lines: &Vec<Types>,
//     functions: &[(SmolStr, &[SmolStr], &[Types])],
//     variables: &mut HashMap<SmolStr, Types>,
// ) -> Types {
//     let mut register: HashMap<i8, Types> = Default::default();
//     let mut operator: HashMap<i8, BasicOperator> = Default::default();
//     let mut default_output: Types = Types::Null;
//     let mut default_operator: BasicOperator = BasicOperator::Null;
//
//     let mut depth: Vec<i8> = vec![];
//     for p_instruction in lines {
//         let mut instruction: &Types = &Types::Null;
//         log!("PINSTRUCT {p_instruction:?}");
//         let matched:(Types,bool);
//         let temp_value:Types;
//         let other_temp:&Types;
//         match p_instruction {
//             Types::VariableIdentifier(x) => {
//                 other_temp = variables.get(x).unwrap();
//                 instruction = other_temp;
//             }
//             Types::FUNC_CALL(func_name, y) => {
//                 // if !depth.is_empty() {
//
//                 let args: Vec<Types> = y
//                     .iter()
//                     .map(|arg| register.remove(arg).unwrap_or(Types::Null))
//                     .collect();
//                 matched = builtin_functions(&func_name, &args);
//                 // check if function is a built-in function, else search it among user-defined functions
//                 if matched.1 {
//                     instruction = &matched.0;
//                 }
//                 // else if func_name == "executeline" {
//                 //     assert_args_number!("executeline", args.len(), 1);
//                 //     if let Types::String(line) = &args[0] {
//                 //         return process_stack(&parse_code(line)[0], variables, functions);
//                 //     }
//                 //     error(&format!("Cannot execute {:?}", &args[0]), "");
//                 // }
//                 else if func_name == "int" {
//                     assert_args_number!("int", args.len(), 1);
//                     if let Types::String(str) = &args[0] {
//                         temp_value = Types::Integer(str.parse::<i64>().unwrap_or_else(|_| {
//                             error(&format!("Cannot convert String '{str}' to Integer",), "");
//                             std::process::exit(1)
//                         }));
//                         instruction = &temp_value;
//                     } else if let Types::Float(float) = &args[0] {
//                         temp_value = Types::Integer(float.round() as i64);
//                         instruction = &temp_value;
//                     }
//                     error(
//                         &format!(
//                             "Cannot convert {} to Integer",
//                             get_printable_type!(&args[0])
//                         ),
//                         "",
//                     );
//                 } else if func_name == "str" {
//                     assert_args_number!("str", args.len(), 1);
//                     if let Types::Integer(int) = &args[0] {
//                         temp_value = Types::String(int.to_smolstr());
//                         instruction = &temp_value;
//                     } else if let Types::Float(float) = &args[0] {
//                         temp_value = Types::String(float.to_smolstr());
//                         instruction = &temp_value;
//                     } else if let Types::Bool(boolean) = &args[0] {
//                         temp_value = Types::String(if *boolean {
//                             "true".to_smolstr()
//                         } else {
//                             "false".to_smolstr()
//                         });
//                         instruction = &temp_value;
//                     } else if let Types::Array(_) = &args[0] {
//                         temp_value = Types::String(get_printable_form(&args[0]));
//                         instruction = &temp_value;
//                     }
//                     error(
//                         &format!("Cannot convert {} to String", get_printable_type!(&args[0])),
//                         "",
//                     );
//                 } else if func_name == "float" {
//                     assert_args_number!("float", args.len(), 1);
//                     if let Types::String(str) = &args[0] {
//                         temp_value = Types::Float(str.parse::<f64>().unwrap_or_else(|_| {
//                             error(&format!("Cannot convert String '{str}' to Float",), "");
//                             std::process::exit(1)
//                         }));
//                         instruction = &temp_value;
//                     } else if let Types::Integer(int) = &args[0] {
//                         temp_value = Types::Float(*int as f64);
//                         instruction = &temp_value;
//                     }
//                     error(
//                         &format!("Cannot convert {} to Float", get_printable_type!(&args[0])),
//                         "",
//                     );
//                 } else {
//                     let target_function: &(SmolStr, &[SmolStr], &[Types]) = functions
//                         .iter()
//                         .find(|func| func.0 == *func_name)
//                         .unwrap_or_else(|| {
//                             error(&format!("Unknown function '{func_name}'"), "");
//                             std::process::exit(1)
//                         });
//                     assert_args_number!(&func_name, args.len(), target_function.1.len());
//                     let mut target_args: HashMap<SmolStr, Types> = target_function
//                         .1
//                         .iter()
//                         .enumerate()
//                         .map(|(i, arg)| (arg.to_smolstr(), args[i].clone()))
//                         .collect();
//                     temp_value = process_function(target_function.2, &mut target_args, functions);
//                     instruction = &temp_value;
//                     // }
//                 }
//             }
//             _ => instruction = p_instruction,
//         };
//         match instruction {
//             Types::STARTSTORE(x) => {
//                 depth.push(*x);
//                 register.insert(*x, Types::Null);
//             }
//             Types::STOP(ref x) => {
//                 depth.pop();
//             }
//             Types::VAR_STORE(x, ref y) => {
//                 variables.insert(x.to_smolstr(), register.remove(y).unwrap());
//             }
//             Types::VAR_REPLACE(x, ref y) => {
//                 let value = register.remove(y).unwrap();
//                 if let Some(x) = variables.get_mut(x) {
//                     *x = value;
//                 }
//             }
//             Types::FUNC_CALL(ref func_name, ref y) => {
//                 if depth.is_empty() {
//                     let args: Vec<Types> =
//                         y.iter().map(|arg| register.remove(arg).unwrap()).collect();
//                     let (_, matched) = builtin_functions(&func_name, &args);
//                     if func_name == "executeline" && !matched {
//                         assert_args_number!("executeline", args.len(), 1_usize);
//                         if_let!(likely, Types::String(line), &args[0], {
//                             // process_stack(&parse_code(line)[0], &variables, functions);
//                         }, else {
//                             error(&format!("Cannot execute line {:?}", &args[0]), "");
//                         });
//                     } else if !matched {
//                         let target_function: &(SmolStr, &[SmolStr], &[Types]) = functions
//                             .iter()
//                             .find(|func| func.0 == *func_name)
//                             .unwrap_or_else(|| {
//                                 error(&format!("Unknown function '{func_name}'"), "");
//                                 std::process::exit(1)
//                             });
//                         assert_args_number!(&func_name, args.len(), target_function.1.len());
//                         let mut target_args: HashMap<SmolStr, Types> = target_function
//                             .1
//                             .iter()
//                             .enumerate()
//                             .map(|(i, arg)| (arg.to_smolstr(), args[i].clone()))
//                             .collect();
//
//                         process_function(target_function.2, &mut target_args, functions);
//                     }
//                 } else {
//                 }
//             }
//             Types::FUNC_RETURN(ref return_term) => {
//                 return register.remove(&return_term).unwrap();
//             }
//             Types::WHILE_BLOCK(condition, code) => {
//                 while execute(&condition, functions, variables) == Types::Bool(true) {
//                     // println!("EXECUTING WHILEBLOCK");
//                     execute(&code, functions, variables);
//                 }
//             }
//             _ => {
//                 if depth.is_empty() {
//                     if let Types::Operation(op) = instruction {
//                         default_operator = *op;
//                     } else {
//                         log!("INSTRUCTION {instruction:?}");
//                         if default_output == Types::Null {
//                             default_output = instruction.clone();
//                             continue;
//                         }
//                         match instruction {
//                             Types::Integer(ref int) => {
//                                 default_output = integer_ops(*int, &default_output, default_operator.clone())
//                             }
//                             Types::Float(ref float) => {
//                                 default_output = float_ops(*float, &default_output, default_operator.clone())
//                             }
//                             _ => todo!(),
//                         }
//                     }
//                 } else {
//                     let last_depth = depth.last().unwrap();
//                     if let Some(x) = register.get_mut(last_depth) {
//                         if matches!(x, Types::Null) {
//                             *x = instruction.clone();
//                             continue;
//                         }
//
//                         if let Types::Operation(op) = instruction {
//                             operator.insert(*last_depth, *op);
//                         } else {
//                             match instruction {
//                                 Types::Integer(ref int) => {
//                                     *x = integer_ops(*int, x, operator.remove(last_depth).unwrap())
//                                 }
//                                 Types::Float(ref float) => {
//                                     *x = float_ops(*float, x, operator.remove(last_depth).unwrap())
//                                 }
//                                 _ => todo!(),
//                             }
//                         }
//                         // *x = instruction;
//                     }
//                 }
//             }
//         }
//         // println!("REGISTER {register:?}");
//         // println!("DEPTH {depth:?}");
//         // println!("VARS {variables:?}");
//         // println!("---");
//     }
//     default_output.clone()
// }

fn process_function(
    lines: &[Types],
    variables: &mut HashMap<SmolStr, Types>,
    functions: &[(SmolStr, &[SmolStr], &[Types])],
) -> Types {
    let processed = process_line_logic(lines, variables, functions);
    if processed != Types::Null {
        return processed;
    }
    // let simple = simplify(lines.to_vec(), false, 0).0;
    // log!(
    //     "SIMPLIFY:\n{}\n----------",
    //     simple
    //         .iter()
    //         .map(|x| format!("{x:?}"))
    //         .collect::<Vec<String>>()
    //         .join("\n")
    // );
    // let result = execute(&simple, functions, variables);
    // if result != Types::Null {
    //     return result;
    // }
    Types::Null
}

fn main() {
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args().skip(1).collect();
    //     if args.is_empty() {
    //         println!(
    //             "
    //   ______   ______   .___  ___. .______    __    __  .___________. _______
    //  /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
    // |  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
    // |  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
    // |  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
    //  \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
    // \x1b[3mLive long and prosper!\x1b[0m\n- Spock
    //
    // To run a file, run: `compute <file>`
    // To get help, run `compute -h`
    //         "
    //         );
    //         return;
    //     } else if args == vec!["-h"] {
    //         println!(
    //             "
    //   ______   ______   .___  ___. .______    __    __  .___________. _______
    //  /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
    // |  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
    // |  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
    // |  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
    //  \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
    // \x1b[3mHelp me, Obi-Wan Kenobi. You’re my only hope.\x1b[0m\n- Princess Leia
    //
    // compute [filename] [-c]
    //
    // positional arguments:
    //   filename
    //
    // options:
    //   -c, --clear-cache    Delete the cache folder
    //         "
    //         );
    //         return;
    //     } else if args.len() >= 2
    //         && (&args[1] == "-c" || &args[1] == "--clear-cache")
    //         && Path::new(".compute").exists()
    //     {
    //         remove_dir_all(Path::new(".compute")).unwrap_or_else(|_| {
    //             error("Failed to delete the cache folder (.compute)", "");
    //             std::process::exit(1)
    //         });
    //     }
    //     let arg = args.first().unwrap();

    let content = fs::read_to_string("example.compute").unwrap_or_else(|_| {
        error(&format!("Unable to read file '{args:?}'"), "");
        std::process::exit(1)
    });

    let now = Instant::now();

    let temp_funcs = parse_functions(content.trim(), true);
    let mut main_function: (SmolStr, &[SmolStr], Vec<Types>) = Default::default();

    let partial_convert: Vec<(SmolStr, &[SmolStr], Vec<Types>)> = temp_funcs
        .iter()
        .map(|(name, args, lines)| {
            let stack: Vec<Types> = lines
                .iter()
                .flat_map(|line| line.iter().map(|x| (*x).clone()))
                .collect();
            if name == "main" {
                main_function = (name.clone(), args.as_slice(), stack);
                return (name.clone(), args.as_slice(), vec![]);
            }
            return (name.clone(), args.as_slice(), stack);
        })
        .filter(|(name, _, _)| name != "main")
        .collect();

    let converted: Vec<(SmolStr, &[SmolStr], &[Types])> = partial_convert
        .iter()
        .map(|(name, args, lines)| (name.to_smolstr(), *args, lines.as_slice()))
        .collect();

    let functions: &[(SmolStr, &[SmolStr], &[Types])] = converted.as_slice();

    log!("PARSED IN: {:.2?}", now.elapsed());
    // log!("MAIN\n{}", main_function.2.iter().map(|x| format!("{x:?}")).collect::<Vec<String>>().join("\n"));

    let now = Instant::now();

    let mut vars: HashMap<SmolStr, Types> = HashMap::default();
    process_function(&main_function.2, &mut vars, functions);

    log!("EXECUTED IN: {:.2?}", now.elapsed());
    log_release!("TOTAL: {:.2?}", totaltime.elapsed());
}
