#[path = "types/array.rs"]
mod array;
#[path = "types/file.rs"]
mod file;
#[path = "types/float.rs"]
mod float;
#[path = "types/integer.rs"]
mod integer;
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
use crate::float::float_ops;
use crate::integer::integer_ops;
use crate::namespaces::namespace_functions;
use crate::parser::{parse_code, BasicOperator, Types, Variable};
use crate::parser_functions::parse_functions;
use crate::preprocess::preprocess;
use crate::string::string_ops;
use crate::util::{error, get_printable_form};
use branches::unlikely;
use const_currying::const_currying;
use inflector::Inflector;
use std::fs::remove_dir_all;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::time::Instant;
use std::{fs, io};
use branches::likely;
use smol_str::{SmolStr, StrExt, ToSmolStr};
use unroll::unroll_for_loops;

#[const_currying]
fn builtin_functions(
    x: &str,
    #[maybe_const(dispatch = args, consts = [[Parser:Expr; 0]])] args: &Vec<Types>,
) -> (Types, bool) {
    if x == "print" {
        assert_args_number!("print", args.len(), 1);
        if let Types::String(str) = &args[0] {
            println!("{}", str);
        } else {
            println!("{}", get_printable_form(&args[0]));
        }

        (Types::Null, true)
    } else if x == "abs" {
        assert_args_number!("abs", args.len(), 1);
        match &args[0] {
            Types::Float(val) => return (Types::Float(val.abs()), true),
            Types::Integer(val) => return (Types::Integer(val.abs()), true),
            _ => error(
                &format!("Cannot get absolute value of {:?} type", &args[0]),
                "Change type",
            ),
        }
        (Types::Null, true)
    } else if x == "round" {
        assert_args_number!("round", args.len(), 1);
        match &args[0] {
            Types::Float(val) => return (Types::Integer(val.round() as i64), true),
            Types::Integer(val) => return (Types::Integer(*val), true),
            _ => error(
                &format!("Cannot round {} type", get_printable_type!(&args[0])),
                "Change type",
            ),
        }
        (Types::Null, true)
    } else if x == "len" {
        assert_args_number!("len", args.len(), 1);
        match &args[0] {
            Types::String(val) => {
                return (Types::Integer(val.len() as i64), true);
            }
            Types::Array(val) => {
                return (Types::Integer(val.len() as i64), true);
            }
            _ => error(
                &format!(
                    "Cannot get length of type {}",
                    get_printable_type!(&args[0])
                ),
                "Change type",
            ),
        }
        (Types::Null, true)
    } else if x == "input" {
        assert_args_number!("input", args.len(), 0, 1);
        if args.len() == 1 {
            if_let!(likely, Types::String(prompt), &args[0], {
                print!("{}", prompt);
            }, else {
                error(
                    &format!("Cannot print {} type", get_printable_type!(&args[0])),
                    "Change type",
                );
            });
        }
        io::stdout().flush().unwrap();
        return (
            Types::String(
                BufReader::new(io::stdin())
                    .lines()
                    .next()
                    .expect(error_msg!("Failed to read input"))
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
            ),
            true,
        );
    } else if x == "type" {
        assert_args_number!("type", args.len(), 1);
        return (Types::String(get_printable_type!(&args[0]).into()), true);
    } else if x == "hash" {
        assert_args_number!("hash", args.len(), 1);
        (
            Types::String(
                blake3::hash(
                    bincode::serialize(&args[0])
                        .expect(error_msg!(format!(
                            "Failed to compute hash of object {:?}",
                            &args[0]
                        )))
                        .as_ref(),
                ).to_smolstr(),
            ),
            true,
        )
    } else if x == "sqrt" {
        assert_args_number!("sqrt", args.len(), 1);
        if let Types::Integer(int) = args[0] {
            return (Types::Float((int as f64).sqrt()), true);
        } else if let Types::Float(float) = args[0] {
            return (Types::Float(float.sqrt()), true);
        } else {
            error(
                format!("Cannot calculate the square root of {:?}", args[0]).as_str(),
                "",
            );
            (Types::Null, false)
        }
    } else if x == "the_answer" {
        println!("42, the answer to the Ultimate Question of Life, the Universe, and Everything.");
        (Types::Integer(42), true)
    } else if x == "range" {
        assert_args_number!("sqrt", args.len(), 1, 3);
        if args.len() == 1 {
            if_let!(likely, Types::Integer(lim), args[0], {
                (Types::Array((0..lim).map(Types::Integer).collect()), true)
            }, else {
                error("Invalid range limit", "");
                (Types::Null, false)
            })
        } else if args.len() == 2 {
            if_let!(likely, Types::Integer(lim), args[0], {
                if_let!(Types::Integer(upplim), args[1], {
                    (
                        Types::Array((lim..upplim).map(Types::Integer).collect()),
                        true,
                    )
                }, else {
                    error("Invalid range limit", "");
                    (Types::Null, false)
                })
            }, else {
                error("Invalid range start", "");
                (Types::Null, false)
            })
        } else if args.len() == 3 {
            if_let!(likely, Types::Integer(start), args[0], {
                if_let!(Types::Integer(stop), args[1], {
                    if_let!(Types::Integer(step), args[2], {
                        if unlikely(step == 0) {
                            error("Step cannot be zero", "");
                            (Types::Null, false)
                        } else {
                            let range = if step > 0 {
                                (start..stop).step_by(step as usize)
                            } else {
                                (stop..start).step_by((-step) as usize)
                            };
                            (Types::Array(range.map(Types::Integer).collect()), true)
                        }
                    }, else {
                        error("Invalid range step", "");
                        (Types::Null, false)
                    })
                }, else {
                    error("Invalid range limit", "");
                    (Types::Null, false)
                })
            }, else {
                error("Invalid range start", "");
                (Types::Null, false)
            })
        } else {
            error("Invalid range arguments", "");
            (Types::Null, false)
        }
    } else {
        (Types::Null, false)
    }
}


#[unroll_for_loops]
fn process_stack(
    stack_in: &Vec<Types>,
    variables: &Vec<Variable>,
    functions: &Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)>,
) -> Types {
    let mut output: Types = Types::Null;
    let mut current_operator: BasicOperator = BasicOperator::Null;
    for p_element in stack_in {
        let mut element: &Types = p_element;
        let mut val: Types = Types::Null;
        if let Types::VariableIdentifier(ref var) = element {
            let variable = &variables
                .iter()
                .find(|x| x.name.eq(var))
                .expect(&error_msg!(format!("Unknown variable '{}'", var)));
            element = &variable.value
        } else {
            val = preprocess(variables, functions, element);
            if val != Types::Null {
                element = &val;
            }
        }

        if output != Types::Null {
            match element {
                Types::Operation(op) => {
                    current_operator = *op;
                }
                Types::OR(ref x) => {
                    let parsed_exp = process_stack(&x, variables, functions);
                    if_let!(
                        likely,
                        Types::Bool(inbool),
                        output,
                        {
                            if_let!(likely, Types::Bool(sidebool), parsed_exp, {
                                output = Types::Bool(inbool || sidebool)
                            }, else {
                                error(format!("{:?} is not a Boolean", parsed_exp).as_str(), "");
                            });
                        }, else
                        {
                            error(format!("{:?} is not a Boolean", output).as_str(), "");
                        }
                    );
                }
                Types::AND(ref x) => {
                    let parsed_exp = process_stack(&x, variables, functions);
                    if_let!(
                        likely,
                        Types::Bool(inbool),
                        output,
                        {
                            if_let!(likely, Types::Bool(sidebool), parsed_exp, {
                                output = Types::Bool(inbool && sidebool)
                            }, else {
                                error(format!("{:?} is not a Boolean", parsed_exp).as_str(), "");
                            });
                        }, else
                        {
                            error(format!("{:?} is not a Boolean", output).as_str(), "");
                        }
                    )
                }
                Types::String(x) => {
                    output = string_ops(x.to_smolstr(), output, current_operator);
                }
                Types::Float(x) => {
                    output = float_ops(*x, output, current_operator);
                }
                Types::Integer(x) => {
                    output = integer_ops(*x, output, current_operator);
                }
                Types::Array(x) => output = array_ops(x.to_owned(), output, current_operator),
                Types::Null => {
                    if output == Types::Null {
                        match current_operator {
                            BasicOperator::Equal => output = Types::Bool(true),
                            BasicOperator::NotEqual => output = Types::Bool(false),
                            _ => error(
                                &format!(
                                    "Cannot perform operation '{:?}' between Null and Null",
                                    current_operator
                                ),
                                "",
                            ),
                        }
                    }
                }
                Types::Property(x) => {
                    // TODO
                    todo!("Properties aren't implented yet!")
                }
                Types::PropertyFunction(ref x, ref y) => {
                    let args: Vec<Types> = y
                        .iter()
                        .map(|arg| process_stack(&arg, &variables, &functions))
                        .collect();

                    if let Types::String(ref str) = &output {
                        string_props!(str, args, x, output);
                        // str.to_smolstr()
                    } else if let Types::Float(num) = output {
                        float_props!(num, args, x, output);
                    } else if let Types::Integer(num) = output {
                        integer_props!(num, args, x, output);
                    } else if let Types::Array(ref arr) = output {
                        array_props!(arr, args, x, output);
                    } else if let Types::File(ref filepath) = &output {
                        file_props!(filepath, args, x, output);
                    }
                }
                _ => todo!(),
            }
        } else {
            output = element.to_owned();
        }
    }
    output
}

#[unroll_for_loops]
#[const_currying]
fn process_function(
    lines: &Vec<Vec<Types>>,
    variables: &mut Vec<Variable>,
    expected_variables_len: usize,
    name: &str,
    #[maybe_const(dispatch = args, consts = [[Parser:Expr; 0]])] functions: &Vec<(
        SmolStr,
        Vec<SmolStr>,
        Vec<Vec<Types>>,
    )>,
    extra_variables: Option<Vec<Variable>>
) -> Types {
    if unlikely(variables.len() != expected_variables_len) {
        error(
            &format!(
                "Careful! Function '{}' expected {} arguments, but received {}",
                name,
                expected_variables_len,
                variables.len()
            ),
            "Remove the excess arguments",
        )
    }
    let mut temp_vars: Vec<Variable> = if let Some(var) = extra_variables {var} else {vec![]};

    let mut global_vars: Vec<Variable> = [
        &variables[..],
        &temp_vars[..],

    ].concat();
    macro_rules! update_global {
        () => {
            global_vars = [
                &variables[..],
                &temp_vars[..],
            ].concat();
        };
    }

    for instructions in lines {
        for instruction in instructions {
            match instruction {
                Types::VariableDeclaration(x, y) => {
                    temp_vars.push(Variable {
                        name: x.to_smolstr(),
                        value: process_stack(&y, &variables, &functions),
                    });
                    update_global!();
                },
                Types::VariableRedeclaration(x, y) => {
                    if let Some(position) = variables.iter().position(|var| var.name == *x) {
                        let processed = process_stack(&y, &global_vars, &functions);
                        variables[position].value = processed;
                    } else if let Some(position) = temp_vars
                        .iter()
                        .position(|var| var.name == *x) {
                        let processed = process_stack(&y, &global_vars, &functions);
                        temp_vars[position].value = processed;
                    } else {
                        error(&format!("Variable {x} does not exist"),"");
                    }
                    update_global!();
                }
                Types::NamespaceFunctionCall(ref namespace, ref y, ref z) => {
                    let args: Vec<Types> = z
                        .iter()
                        .map(|arg| process_stack(&arg, &global_vars, &functions))
                        .collect();
                    if unlikely(!namespace_functions(&namespace, &y, &args).1) {
                        error(
                            &format!("Unknown function '{}'", namespace.join(".") + "." + &y),
                            "",
                        );
                    };
                }
                Types::FunctionCall(x, y) => {
                    // println!("{:?}", y);
                    let args: Vec<Types> = y
                        .iter()
                        .map(|arg| process_stack(arg, &global_vars, functions))
                        .collect();

                    let matched = builtin_functions(&x, &args);
                    if x == "executeline" && !matched.1 {
                        assert_args_number!("executeline", args.len(), 1);
                        if_let!(likely, Types::String(line), &args[0], {
                            process_stack(&parse_code(line)[0], &global_vars, &functions);
                            continue;
                        }, else {
                            error(&format!("Cannot execute line {:?}", &args[0]), "")
                        })
                    } else if !matched.1 {
                        let target_function: &(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>) = functions
                            .into_iter()
                            .filter(|func| func.0 == *x)
                            .next()
                            .expect(error_msg!(&format!("Unknown function '{}'", x)));
                        assert_args_number!(&x, args.len(), target_function.1.len());
                        let mut target_args: Vec<Variable> = target_function
                            .1
                            .iter()
                            .enumerate()
                            .map(|(i, arg)| Variable {
                                name: arg.to_smolstr(),
                                value: args[i].clone(),
                            })
                            .collect();
                        let len = target_args.len();
                        process_function(
                            &target_function.2,
                            &mut target_args,
                            len,
                            &target_function.0,
                            functions,
                            None
                        );
                    }
                }
                Types::FunctionReturn(x) => {
                    return process_stack(x, &global_vars, functions);
                }
                Types::Condition(x, y, z) => {
                    if process_stack(x, &variables, functions) == Types::Bool(true) {
                        let len = global_vars.len();
                        let out = process_function(y, &mut global_vars, len, name, functions,None);
                        if Types::Null != out {
                            return out;
                        }
                    } else {
                        for else_block in z {
                            if else_block.0.len() == 0 {
                                let len = global_vars.len();
                                let out = process_function(
                                    &else_block.1,
                                    &mut global_vars,
                                    len,
                                    name,
                                    functions,
                                    None
                                );
                                if out != Types::Null {
                                    return out;
                                }
                            }
                            if process_stack(&else_block.0, &variables, &functions)
                                == Types::Bool(true)
                            {
                                let len = global_vars.len();
                                let out = process_function(
                                    &else_block.1,
                                    &mut global_vars,
                                    len,
                                    name,
                                    functions,
                                    None
                                );
                                if out != Types::Null {
                                    return out;
                                }
                            }
                        }
                    }
                }
                Types::Loop(x, y, z) => {
                    let loop_array = process_stack(&y, &variables, &functions);
                    if let Types::Array(target_array) = loop_array {
                        for elem in target_array {
                            let len = global_vars.len();
                            let out_expr = process_function(z, &mut global_vars, len, name, functions, Some(vec![Variable{name: x.to_smolstr(), value: elem}]));

                            if out_expr != Types::Null {
                                return out_expr;
                            }
                        }
                    } else if let Types::String(ref target_string) = loop_array {
                        for elem in target_string.chars() {
                            let len = global_vars.len();
                            let out_expr = process_function(z, &mut global_vars, len, name, functions,Some(vec![Variable{name: x.to_smolstr(), value:Types::String(elem.to_smolstr())}]));
                            if out_expr != Types::Null {
                                return out_expr;
                            }
                        }
                    }
                }
                Types::While(x, y) => {
                    while process_stack(x, &variables, functions) == Types::Bool(true) {
                        let len = global_vars.len();
                        let out = process_function(y, &mut global_vars, len, name, functions,None);
                        if Types::Null != out {
                            return out;
                        }
                    }
                }
                _ => {
                    process_stack(&instructions, &variables, &functions);
                    break;
                }
            }
        }
    }
    Types::Null
}

fn main() {
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.len() == 0 {
        println!(
            "
  ______   ______   .___  ___. .______    __    __  .___________. _______
 /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
|  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
|  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
|  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
 \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
\x1b[3mLive long and prosper!\x1b[0m\n- Spock

To run a file, run: `compute <file>`
To get help, run `compute -h`
        "
        );
        return;
    } else if args == vec!["-h"] {
        println!(
            "
  ______   ______   .___  ___. .______    __    __  .___________. _______
 /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
|  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
|  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
|  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
 \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
\x1b[3mHelp me, Obi-Wan Kenobi. You’re my only hope.\x1b[0m\n- Princess Leia

compute [filename] [-c]

positional arguments:
  filename

options:
  -c, --clear-cache    Delete the cache folder
        "
        );
        return;
    } else if args.len() >= 2
        && (&args[1] == "-c" || &args[1] == "--clear-cache")
        && Path::new(".compute").exists()
    {
        remove_dir_all(Path::new(".compute"))
            .expect(error_msg!("Failed to delete the cache folder (.compute)"));
    }
    let arg = args.first().unwrap();

    let content =
        fs::read_to_string(arg).expect(error_msg!(format!("Unable to read file '{}'", arg)));

    let now = Instant::now();
    let functions: Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)> =
        parse_functions(content.trim(), true);
    log!("PARSED IN: {:.2?}", now.elapsed());
    log!("FUNCTIONS {:?}", functions);

    let main_instructions = functions
        .clone()
        .into_iter()
        .filter(|function| function.0 == "main")
        .collect::<Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)>>();

    let now = Instant::now();
    // thread::Builder::new()
    //     // 16MB stack size
    //     .stack_size(16 * 1024 * 1024)
    //     .spawn(move || {
    //         process_function(&main_instructions[0].2, &mut vec![], 0, "main", &functions);
    //     })
    //     .unwrap()
    //     .join()
    //     .unwrap();
    process_function(&main_instructions[0].2, &mut vec![], 0, "main", &functions,None);
    println!("EXECUTED IN: {:.2?}", now.elapsed());
    println!("TOTAL: {:.2?}", totaltime.elapsed());
}
