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
use crate::parser::{parse_code, FunctionPropertyCallBlock, Instr, Operator, Types};
use crate::parser_functions::parse_functions;
use crate::preprocess::preprocess;
use crate::string::{string_ops, to_title_case};
use crate::util::{error, get_printable_form, get_type, op_to_symbol, print_form, split_vec};
use branches::likely;
use branches::unlikely;
use colored::Colorize;
// use smol_str::{SmolStr, StrExt as _, ToSmolStr as _};
// use smartstring::alias::String;
// use gxhash::HashMapExt;
use compact_str::ToCompactString;
use internment::Intern;
use snmalloc_rs::SnMalloc;
use std::fs;
use std::fs::{remove_dir_all, File};
use std::io::{BufReader, Read, Write as _};
use std::ops::{Add, Deref};
use std::path::Path;
use std::time::Instant;
use unroll::unroll_for_loops;

#[global_allocator]
static ALLOC: SnMalloc = SnMalloc;

#[unroll_for_loops]
// #[inline(always)]
fn process_stack(
    stack_in: &[Types],
    variables: &Vec<(String, Types)>,
    functions: &[(String, &[String], &[Types])],
) -> Types {
    let mut output: Types = match stack_in.first().unwrap_or(&Types::Integer(0)) {
        Types::VariableIdentifier(ref var) => variables
            .iter()
            .find(|(name, _)| name == var)
            .unwrap_or_else(|| {
                error(&format!("Unknown variable '{var}'"), "");
                std::process::exit(1)
            })
            .1
            .clone(),
        Types::Wrap(ref x) => process_stack(x, variables, functions),
        other => {
            if !matches!(
                other,
                Types::FunctionCall(_)
                    | Types::NamespaceFunctionCall(_)
                    | Types::Priority(_)
                    | Types::Array(_, _, _)
            ) {
                other.clone()
            } else {
                preprocess(other, variables, functions)
            }
        }
    };
    let mut current_operator: Operator = Operator::Null;
    for p_element in stack_in.iter().skip(1) {
        let element: &Types = match p_element {
            // Types::VariableIdentifier(ref var) => variables.get(var).unwrap_or_else(|| {
            //     error(&format!("Unknown variable '{var}'"), "");
            //     std::process::exit(1)
            // }),
            Types::VariableIdentifier(ref var) => {
                &variables
                    .iter()
                    .find(|(name, _)| name == var)
                    .unwrap_or_else(|| {
                        error(&format!("Unknown variable '{var}'"), "");
                        std::process::exit(1)
                    })
                    .1
            }
            Types::Wrap(ref x) => &process_stack(x, variables, functions),
            other => {
                if !matches!(
                    other,
                    Types::FunctionCall(_)
                        | Types::NamespaceFunctionCall(_)
                        | Types::Priority(_)
                        | Types::Array(_, true, false)
                        | Types::Array(_, false, true)
                ) {
                    other
                } else {
                    &preprocess(other, variables, functions)
                }
            }
        };

        match element {
            Types::Operation(ref op) => current_operator = *op,
            Types::Integer(ref x) => {
                output = integer_ops(*x, output, current_operator);
            }
            Types::String(ref x) => output = string_ops(x, output, current_operator),
            Types::Float(ref x) => output = float_ops(*x, output, current_operator),
            Types::Array(ref x, _, false) => output = array_ops(x, output, current_operator),
            Types::Property(ref block) => {
                let args: Vec<Types> = util::split_vec_box(&block.args, Types::Separator)
                    .iter()
                    .map(|w| process_stack(w, variables, functions))
                    .collect();
                match output {
                    Types::String(ref str) => string_props!(str, args, block.name, output),
                    Types::Float(num) => float_props!(num, args, block.name, output),
                    Types::Integer(num) => integer_props!(num, args, block.name, output),
                    Types::Array(ref arr, _, false) => {
                        array_props!(arr, args, block.name, output)
                    }

                    // OBJECTS
                    Types::File(ref filepath) => {
                        file_props!(filepath, args, block.name, output)
                    }
                    _ => error(
                        &format!(
                            "Unknown function '{}' for object {}",
                            block.name,
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
                        Operator::Equal => output = Types::Bool(true),
                        Operator::NotEqual => output = Types::Bool(false),
                        _ => error(
                            &format!(
                                "Cannot perform operation '{current_operator:?}' between Null and Null"
                            ),
                            "",
                        ),
                    }
                }
            }
            _ => error(&format!("TODO STACK {element:?}"), ""),
        }
    }
    output
}

// #[inline(always)]
fn process_lines(
    line_array: &[Types],
    variables: &mut Vec<(String, Types)>,
    functions: &[(String, &[String], &[Types])],
) -> Types {
    for line in line_array {
        match line {
            Types::VariableDeclaration(ref block) => {
                if !block.is_declared {
                    variables.push((
                        block.name.parse().unwrap(),
                        process_stack(&block.value, variables, functions),
                    ));
                } else {
                    let calculated = process_stack(&block.value, variables, functions);
                    // if let Some(x) = variables.get_mut((&block.name, _)) {
                    //     *x = calculated;
                    // }
                    // variables.fin
                    if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.name) {
                        x.1 = calculated;
                    }
                }
            }
            Types::NamespaceFunctionCall(ref block) => {
                let args: Vec<Types> = util::split_vec_box(&block.args, Types::Separator)
                    .iter()
                    .map(|w| process_stack(w, variables, functions))
                    .collect();
                if unlikely(!namespace_functions(&block.namespace, &block.name, &args).1) {
                    error(
                        &format!(
                            "Unknown function '{}'",
                            block.namespace.join(".") + "." + block.name.as_str()
                        ),
                        "",
                    );
                };
            }
            Types::FunctionCall(ref block) => {
                let args: Vec<Types> = util::split_vec_box(&block.args, Types::Separator)
                    .iter()
                    .map(|x| process_stack(x, variables, functions))
                    .collect();
                let (_, matched) = builtin_functions(&block.name, &args);
                if block.name == "executeline" && !matched {
                    assert_args_number!("executeline", args.len(), 1_usize);
                    if_let!(likely, Types::String(line), &args[0], {
                        process_stack(&parse_code(line)[0], variables, functions);
                    }, else {
                        error(&format!("Cannot execute line {:?}", &args[0]), "");
                    });
                } else if !matched {
                    let target_function: &(String, &[String], &[Types]) = functions
                        .iter()
                        .find(|func| func.0 == *block.name)
                        .unwrap_or_else(|| {
                            error(&format!("Unknown function '{}'", block.name), "");
                            std::process::exit(1)
                        });
                    assert_args_number!(block.name, args.len(), target_function.1.len());
                    let mut target_args: Vec<(String, Types)> = target_function
                        .1
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| (arg.parse().unwrap(), args[i].clone()))
                        .collect();

                    process_lines(target_function.2, &mut target_args, functions);
                }
            }
            Types::PropertyFunction(ref block) => {
                return process_stack(
                    &[
                        process_lines(
                            &[Types::FunctionCall(Box::from(FunctionPropertyCallBlock {
                                name: block.func1_name.parse().unwrap(),
                                args: block.func1_args.clone(),
                            }))],
                            variables,
                            functions,
                        ),
                        Types::Property(Box::from(FunctionPropertyCallBlock {
                            name: block.func2_name.parse().unwrap(),
                            args: block.func2_args.clone(),
                        })),
                    ],
                    variables,
                    functions,
                );
            }
            Types::Condition(ref block) => {
                // let data = block;
                if process_stack(&block.condition, variables, functions) == Types::Bool(true) {
                    let out = process_lines(&block.code, variables, functions);
                    if Types::Null != out {
                        // if out != Types::Break {
                        return out;
                        // }
                        // error("Cannot break in a conditional statement","Remove the \"break\" statement");
                    }
                } else {
                    for else_block in &block.else_blocks {
                        if else_block.0.is_empty()
                            || process_stack(&else_block.0, variables, functions)
                                == Types::Bool(true)
                        {
                            let out = process_lines(&else_block.1, variables, functions);
                            if out != Types::Null {
                                return out;
                            }
                        }
                    }
                }
            }
            Types::Loop(ref block) => {
                let loop_array = process_stack(&block.arr, variables, functions);
                if let Types::Array(target_array, false, false) = loop_array {
                    variables.push((block.id.parse().unwrap(), Types::Null));
                    for elem in target_array {
                        // if let Some(value) = variables.get_mut(&block.id) {
                        //     *value = elem;
                        // }
                        if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.id) {
                            x.1 = elem;
                        }

                        let out: Types = process_lines(&block.code, variables, functions);
                        if out != Types::Null {
                            variables.swap_remove(
                                variables
                                    .iter()
                                    .position(|(name, _)| *name == block.id)
                                    .unwrap(),
                            );
                            // variables.remove(&block.id);
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    // variables.remove(&block.id);
                    variables.swap_remove(
                        variables
                            .iter()
                            .position(|(name, _)| *name == block.id)
                            .unwrap(),
                    );
                } else if let Types::String(ref target_string) = loop_array {
                    variables.push((block.id.parse().unwrap(), Types::Null));
                    for elem in target_string.chars() {
                        // if let Some(value) = variables.get_mut(&block.id) {
                        //     *value = Types::String(elem.to_smolstr());
                        // }
                        if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.id) {
                            x.1 = Types::String(elem.to_string().parse().unwrap());
                        }

                        let out: Types = process_lines(&block.code, variables, functions);
                        if out != Types::Null {
                            // variables.remove(&block.id);
                            variables.swap_remove(
                                variables
                                    .iter()
                                    .position(|(name, _)| *name == block.id)
                                    .unwrap(),
                            );
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    // variables.remove(&block.id);
                    variables.swap_remove(
                        variables
                            .iter()
                            .position(|(name, _)| *name == block.id)
                            .unwrap(),
                    );
                }
            }
            Types::While(ref block) => {
                while let Types::Bool(true) = process_stack(&block.condition, variables, functions)
                {
                    // let now = Instant::now();
                    let out = process_lines(&block.code, variables, functions);
                    // println!("WHILE ITERATION {:.2?}", now.elapsed());
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
            Types::Wrap(ref x) => {
                let x = process_lines(x, variables, functions);
                if x != Types::Null {
                    return x;
                }
            }
            _ => error(&format!("TODO LINE LOGIC {line:?}"), ""),
        }
    }
    Types::Null
}

// pub fn store_instr(inp: Vec<Instructions>) -> Vec<Instructions> {
//     let mut new = vec![];
//     for x in inp {
//         if matches!(x, )
//     }
// }

fn types_to_instr(x: Types) -> Instr {
    match x {
        Types::Integer(int) => return Instr::Integer(int),
        Types::Bool(bool) => return Instr::Bool(bool),
        Types::VariableIdentifier(id) => {
            return Instr::VariableIdentifier(Intern::<String>::from_ref(&id))
        }
        Types::Operation(op) => return Instr::Operation(op),
        // Types::String(str) => return Instr::String(Intern::from(str)),
        _ => todo!("{:?}", x),
    }
}

#[inline(never)]
fn simplify(lines: Vec<Types>, store: bool, locals: &mut Vec<(u16, String)>) -> Vec<Instr> {
    let mut test: Vec<Instr> = vec![];
    for x in lines {
        match x {
            Types::VariableDeclaration(block) => {
                let x = block.name;
                let y = block.value;
                let result = simplify(Vec::from(y), true, locals);
                test.extend(result);
                if block.is_declared {
                    test.push(Instr::VarUpdate(Intern::<String>::from(x)));
                } else {
                    test.push(Instr::VarStore(Intern::<String>::from(x)));
                }
            }
            Types::FunctionCall(block) => {
                let name = block.name;
                let y = block.args;
                for x in split_vec(Vec::from(y), Types::Separator) {
                    let result = simplify(x, true, locals);
                    test.extend(result);
                    test.push(Instr::StoreArg);
                }
                test.push(Instr::FuncCall(Intern::<String>::from_ref(&name)));
            }
            Types::FunctionReturn(ret) => {
                let result = simplify(Vec::from(ret), true, locals);
                test.extend(result);
                test.push(Instr::FuncReturn);
            }
            Types::Condition(block) => {
                let condition = simplify(Vec::from(block.condition), true, locals);
                let in_code = simplify(Vec::from(block.code), false, locals);
                let added = in_code.len();
                test.extend(condition);
                test.push(Instr::If(added as u16));
                test.extend(in_code);
                // if block.else_blocks.len() > 0 {
                //     for else_block in block.else_blocks {
                //         let condition = simplify(Vec::from(else_block.0), true, i);
                //         i = condition.1 + 1;
                //         let in_code = simplify(Vec::from(else_block.1), false, i);
                //         i = in_code.1 + 1;
                //         let added = in_code.0.len();
                //     }
                // }
            }
            Types::While(block) => {
                let condition = simplify(Vec::from(block.condition), true, locals);
                let in_code = simplify(Vec::from(block.code), false, locals);
                let added = in_code.len();
                let sum = condition.len() + 1 + added;
                test.extend(condition);
                test.push(Instr::If((added + 1) as u16));
                test.extend(in_code);
                test.push(Instr::Jump(sum as u16, true))
            }
            Types::String(str) => {
                let len = get_biggest_locals_id(locals);
                locals.push((len as u16, str));
                // locals.push(((locals.len() + 1) as u16, str));
                test.push(Instr::String(len as u16));
            }
            _ => test.push(types_to_instr(x)),
        }
    }
    if store {
        test.insert(0, Instr::Store);
        // test.insert(0, Instr::STORE(i + 1));
        test.push(Instr::StopStore)
        // test.push(Types::STOP(i + 1))
    }
    (test)
}

macro_rules! check_register_adress {
    ($elem: expr, $depth: expr, $i: expr, $register: expr) => {
        if $register.len() < $depth as usize {
            $register.push($elem);
            $i += 1;
            continue;
        }
    };
}

#[inline(always)]
fn pre_match(
    input: Instr,
    variables: &mut Vec<(Intern<String>, Instr)>,
    depth: u8,
    func_args: &mut Vec<Instr>,
    functions: &Vec<(
        Intern<String>,
        Vec<Intern<String>>,
        Vec<Instr>,
        Vec<(u16, String)>,
    )>,
) -> Instr {
    match input {
        Instr::VariableIdentifier(ref id) => *variables
            .iter()
            .find_map(|(x, instr)| if x == id { Some(instr) } else { None })
            .unwrap_or_else(|| panic!("{}", error_msg!(format!("Variable '{id}' does not exist")))),
        // Function call that should return something (because depth > 0)
        Instr::FuncCall(ref name) => {
            if depth == 0 {
                return input;
            }
            match name.as_str() {
                // "str" => {
                //     assert_args_number!("str", func_args.len(), 1);
                //     match func_args.remove(0) {
                //         Instr::Bool(bool) => Instr::String(Intern::from(
                //             {
                //                 if bool {
                //                     "true"
                //                 } else {
                //                     "false"
                //                 }
                //             }
                //             .to_string(),
                //         )),
                //         Instr::Integer(int) => Instr::String(Intern::from(int.to_string())),
                //         Instr::Float(float) => Instr::String(Intern::from(float.to_string())),
                //         Instr::String(_) => func_args[0],
                //         _ => todo!(),
                //     }
                // }
                _ => {
                    if let Some(func) = functions
                        .iter()
                        .find(|(func_name, _, _, _)| name == func_name)
                    {
                        let expected_args = &func.1;
                        if expected_args.len() != func_args.len() {
                            error(
                                &format!(
                                    "Function '{}' expected {} arguments, but received {}",
                                    func.0,
                                    expected_args.len(),
                                    func_args.len()
                                ),
                                "",
                            );
                        }

                        let args: Vec<(Intern<String>, Instr)> = expected_args
                            .iter()
                            .enumerate()
                            .map(|(x, y)| (*y, func_args.remove(x)))
                            .collect();
                        // log!("ARGS IS {args:?}");
                        // log!("FUNCTION ARGS IS {func_args:?}");
                        // let return_obj = execute(&func.2, functions, args);
                        // println!("RETURNING {return_obj:?}");
                        // return return_obj;
                        Instr::Null
                    } else {
                        error(&format!("Unknown function '{}'", name.red()), "");
                        panic!()
                    }
                }
            }
        }
        _ => input,
    }
}

// VERY SLOW -> NEED TO REMOVE IN THE FUTURE
fn get_biggest_locals_id(locals: &Vec<(u16, String)>) -> u16 {
    *locals.iter().map(|(id, _)| id).max().unwrap_or(&0) + 1
}

// #[inline(never)]
fn execute(
    lines: &Vec<Instr>,
    functions: &Vec<(
        Intern<String>,
        Vec<Intern<String>>,
        Vec<Instr>,
        Vec<(u16, String)>,
    )>,
    args: Vec<(Intern<String>, Instr)>,
    locals: &mut Vec<(u16, String)>,
) -> Instr {
    // keeps track of items
    let mut stack: Vec<Instr> = Vec::new();
    // keeps track of function args
    let mut args_list: Vec<Instr> = Vec::new();
    // keeps track of current "storing" depth (e.g STORE,...,STORE,... will have depth=2 after the second "STORE")
    // unclear if really needed
    let mut depth: u8 = 0;
    // keeps track of operators according to depth
    // unclear if a vec is needed
    let mut operator: Vec<Operator> = Vec::new();
    // keeps track of variables
    let mut variables: Vec<(Intern<String>, Instr)> = args;
    let mut line: usize = 0;
    let total_len = lines.len();
    while line < total_len {
        match pre_match(
            lines[line],
            &mut variables,
            depth,
            &mut args_list,
            &functions,
        ) {
            Instr::Store => depth += 1,
            Instr::StopStore => {
                depth -= 1;
            }
            Instr::Operation(ref op) => {
                if depth > 0 {
                    operator.push(*op)
                }
            }
            // VARIABLE IS ALREADY STORED
            Instr::VarUpdate(ref str) => {
                if let Some(elem) = variables.iter_mut().find(|(id, _)| *id == *str) {
                    *elem = (elem.0, stack.pop().unwrap())
                } else {
                    error(&format!("Unknown variable '{}'", str.red()), "");
                }
            }
            // VARIABLE DECLARATION
            Instr::VarStore(ref str) => {
                if let Some(idx) = variables.iter().position(|(name, _)| name == str) {
                    variables.get_mut(idx).unwrap().1 = stack.pop().unwrap();
                }
                variables.push((*str, stack.pop().unwrap()))
            }
            Instr::If(ref jump_size) => {
                let condition = stack.pop().unwrap();
                if condition == Instr::Bool(false) {
                    line += *jump_size as usize;
                } else if condition != Instr::Bool(true) {
                    error(&format!("'{:?}' is not a boolean", &condition), "");
                }
            }
            Instr::Jump(ref jump_size, ref neg) => {
                if *neg {
                    line -= *jump_size as usize;
                    continue;
                }
                line += *jump_size as usize;
                continue;
            }
            Instr::StoreArg => args_list.push(stack.pop().unwrap()),
            // Function call that shouldn't return anything
            Instr::FuncCall(ref name) => {
                if depth == 0 {
                    // println!("ARGS REG IS {args_register:?}");
                    let func_name = name.as_str();
                    if func_name == "print" {
                        println!("{}", print_form(&args_list.remove(0), locals))
                    } else {
                    }
                }
                // log!("ITS ME!!");
                // log!("HI");
            }
            Instr::FuncReturn => {
                return stack.pop().unwrap();
            }
            // PRIMITIVE TYPES
            Instr::Integer(ref int) => {
                check_register_adress!(Instr::Integer(*int), depth, line, stack);
                let index = stack.len() - 1;
                if let Some(elem) = stack.get_mut(index) {
                    match elem {
                        Instr::Integer(parent) => {
                            match operator.pop().unwrap() {
                                Operator::Add => *elem = Instr::Integer(*parent + int),
                                Operator::Sub => *elem = Instr::Integer(*parent - int),
                                Operator::Divide => {
                                    assert_ne!(
                                        int,
                                        &0,
                                        "{}",
                                        error_msg!(format!("Division by zero ({int} / 0)"))
                                    );
                                    *elem = math_to_type!(*parent as f64 / *int as f64);
                                }
                                Operator::Multiply => *elem = Instr::Integer(*parent * int),
                                Operator::Power => *elem = Instr::Integer(parent.pow(*int as u32)),
                                Operator::Modulo => *elem = Instr::Integer(*parent % int),
                                Operator::Equal => *elem = Instr::Bool(parent == int),
                                Operator::NotEqual => *elem = Instr::Bool(parent != int),
                                Operator::Inferior => *elem = Instr::Bool(*parent < *int),
                                Operator::InferiorEqual => *elem = Instr::Bool(*parent <= *int),
                                Operator::Superior => *elem = Instr::Bool(*parent > *int),
                                Operator::SuperiorEqual => *elem = Instr::Bool(*parent >= *int),

                                // AND
                                // OR
                                other => error(
                                    &format!(
                                        "Operation not supported:\n{} {} {}",
                                        "Integer".blue(),
                                        op_to_symbol(other).red(),
                                        "Integer".blue()
                                    ),
                                    "",
                                ),
                            }
                        }
                        Instr::Float(parent) => match operator.pop().unwrap() {
                            Operator::Add => *elem = Instr::Float(*parent + *int as f64),
                            Operator::Sub => *elem = Instr::Float(*parent - *int as f64),
                            Operator::Divide => {
                                assert_ne!(
                                    int,
                                    &0,
                                    "{}",
                                    error_msg!(format!("Division by zero ({int} / 0)"))
                                );
                                *elem = Instr::Float(*parent / *int as f64)
                            }
                            Operator::Multiply => *elem = Instr::Float(*parent * *int as f64),
                            Operator::Power => *elem = Instr::Float(parent.powf(*int as f64)),
                            Operator::Modulo => *elem = Instr::Float(*parent % *int as f64),
                            Operator::Equal => *elem = Instr::Bool(*parent == *int as f64),
                            Operator::NotEqual => *elem = Instr::Bool(*parent != *int as f64),
                            Operator::Inferior => *elem = Instr::Bool(*parent < *int as f64),
                            Operator::InferiorEqual => *elem = Instr::Bool(*parent <= *int as f64),
                            Operator::Superior => *elem = Instr::Bool(*parent > *int as f64),
                            Operator::SuperiorEqual => *elem = Instr::Bool(*parent >= *int as f64),
                            other => error(
                                &format!(
                                    "Operation not supported:\n{} {} {}",
                                    "Float".blue(),
                                    op_to_symbol(other).red(),
                                    "Integer".blue()
                                ),
                                "",
                            ),
                        },
                        Instr::String(parent) => match operator.pop().unwrap() {
                            Operator::Multiply => {
                                let index = locals.iter().position(|(id, _)| id == parent).unwrap();
                                let str = locals.get_mut(index).unwrap();
                                str.1 = str.1.repeat(*int as usize);
                            }
                            other => error(
                                &format!(
                                    "Operation not supported:\n{} {} {}",
                                    "String".blue(),
                                    op_to_symbol(other).red(),
                                    "Integer".blue()
                                ),
                                "",
                            ),
                        },
                        _ => error(&format!("Cannot add Integer to {}", get_type(*elem)), ""),
                    }
                } else {
                    error(
                        "[COMPUTE] UNABLE TO RETRIEVE FROM REGISTER",
                        "This is probably a Compute bug",
                    );
                }
            }
            Instr::Float(ref float) => {
                check_register_adress!(Instr::Float(*float), depth, line, stack);
                let index = stack.len() - 1;
                if let Some(elem) = stack.get_mut(index) {
                    match elem {
                        Instr::Integer(parent) => {
                            match operator.pop().unwrap() {
                                Operator::Add => *elem = Instr::Float(*parent as f64 + float),
                                Operator::Sub => *elem = Instr::Float(*parent as f64 - float),
                                Operator::Divide => {
                                    assert_ne!(
                                        float,
                                        &0.0,
                                        "{}",
                                        error_msg!(format!("Division by zero ({float} / 0)"))
                                    );
                                    *elem = math_to_type!(*parent as f64 / *float);
                                }
                                Operator::Multiply => *elem = Instr::Float(*parent as f64 * float),
                                Operator::Power => {
                                    *elem = Instr::Float(parent.pow(*float as u32) as f64)
                                }
                                Operator::Modulo => *elem = Instr::Float(*parent as f64 % float),
                                Operator::Equal => *elem = Instr::Bool(*parent as f64 == *float),
                                Operator::NotEqual => *elem = Instr::Bool(*parent as f64 != *float),
                                Operator::Inferior => {
                                    *elem = Instr::Bool((*parent as f64) < (*float))
                                }
                                Operator::InferiorEqual => {
                                    *elem = Instr::Bool(*parent as f64 <= *float)
                                }
                                Operator::Superior => *elem = Instr::Bool(*parent as f64 > *float),
                                Operator::SuperiorEqual => {
                                    *elem = Instr::Bool(*parent as f64 >= *float)
                                }

                                // AND
                                // OR
                                other => error(
                                    &format!(
                                        "Operation not supported:\n{} {} {}",
                                        "Integer".blue(),
                                        op_to_symbol(other).red(),
                                        "Float".blue()
                                    ),
                                    "",
                                ),
                            }
                        }
                        Instr::Float(parent) => {
                            match operator.pop().unwrap() {
                                Operator::Add => *elem = Instr::Float(*parent + *float),
                                Operator::Sub => *elem = Instr::Float(*parent - *float),
                                Operator::Divide => {
                                    assert_ne!(
                                        float,
                                        &0.0,
                                        "{}",
                                        error_msg!(format!("Division by zero ({float} / 0)"))
                                    );
                                    *elem = Instr::Float(*parent / *float)
                                }
                                Operator::Multiply => *elem = Instr::Float(*parent * *float),
                                Operator::Power => *elem = Instr::Float(parent.powf(*float)),
                                Operator::Modulo => *elem = Instr::Float(*parent % *float),
                                Operator::Equal => *elem = Instr::Bool(*parent == *float),
                                Operator::NotEqual => *elem = Instr::Bool(*parent != *float),
                                Operator::Inferior => *elem = Instr::Bool(*parent < *float),
                                Operator::InferiorEqual => *elem = Instr::Bool(*parent <= *float),
                                Operator::Superior => *elem = Instr::Bool(*parent > *float),
                                Operator::SuperiorEqual => *elem = Instr::Bool(*parent >= *float),

                                // AND
                                // OR
                                other => error(
                                    &format!(
                                        "Operation not supported:\n{} {} {}",
                                        "Float".blue(),
                                        op_to_symbol(other).red(),
                                        "Float".blue()
                                    ),
                                    "",
                                ),
                            }
                        }
                        _ => error(&format!("Cannot add Float to {}", get_type(*elem)), ""),
                    }
                } else {
                    error(
                        "[COMPUTE] UNABLE TO RETRIEVE FROM REGISTER",
                        "This is probably a Compute bug",
                    );
                }
            }
            Instr::String(str) => {
                check_register_adress!(Instr::String(str), depth, line, stack);
                let index = stack.len() - 1;
                if let Some(elem) = stack.get_mut(index) {
                    println!("STRING ADDING TO {elem:?}");
                    match elem {
                        Instr::String(parent) => match operator.pop().unwrap() {
                            Operator::Add => {
                                let parent_index =
                                    locals.iter().position(|(id, _)| id == parent).unwrap();

                                let current_index =
                                    locals.iter().position(|(id, _)| *id == str).unwrap();
                                let base_string = locals.remove(current_index);

                                if let Some(parent_string) = locals.get_mut(parent_index) {
                                    parent_string.1 = (parent_string.1.to_string() + &base_string.1)
                                }
                            }
                            other => error(
                                &format!(
                                    "Operation not supported:\n{} {} {}",
                                    "String".blue(),
                                    op_to_symbol(other).red(),
                                    "String".blue()
                                ),
                                "",
                            ),
                        },
                        Instr::Integer(parent) => match operator.pop().unwrap() {
                            Operator::Multiply => {
                                let current_index =
                                    locals.iter().position(|(id, _)| *id == str).unwrap();
                                if let Some(base_string) = locals.get_mut(current_index) {
                                    base_string.1 = base_string.1.repeat(*parent as usize);
                                }
                            }
                            other => error(
                                &format!(
                                    "Operation not supported:\n{} {} {}",
                                    "Integer".blue(),
                                    op_to_symbol(other).red(),
                                    "String".blue()
                                ),
                                "",
                            ),
                        },
                        _ => error(&format!("Cannot add String to {}", get_type(*elem)), ""),
                    }
                } else {
                    error(
                        "[COMPUTE] UNABLE TO RETRIEVE FROM REGISTER",
                        "This is probably a Compute bug",
                    );
                }
            }
            Instr::Bool(ref bool) => {
                check_register_adress!(Instr::Bool(*bool), depth, line, stack);
            }
            _ => {}
        }
        line += 1;
        // log!("---\nREGISTER {register:?}\nVARIABLES {variables:?}")
    }
    Instr::Null
}

fn main() {
    dbg!(std::mem::size_of::<Instr>());
    // dbg!(std::mem::size_of::<BasicOperator>());
    // dbg!(std::mem::size_of::<Types>());
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args()
        .skip(1)
        .map(|x| x.parse::<String>().unwrap())
        .collect();
    if args.is_empty() {
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
        remove_dir_all(Path::new(".compute")).unwrap_or_else(|_| {
            error("Failed to delete the cache folder (.compute)", "");
            std::process::exit(1)
        });
    }
    let arg = args.first().unwrap();

    let content = fs::read_to_string("example.compute").unwrap_or_else(|_| {
        error(&format!("Unable to read file '{args:?}'"), "");
        std::process::exit(1)
    });
    let now = Instant::now();

    let hash = blake3::hash(content.as_bytes()).to_string();

    let mut functions: Vec<(
        Intern<String>,
        Vec<Intern<String>>,
        Vec<Instr>,
        Vec<(u16, String)>,
    )>;

    if !Path::new(&format!(".compute/{}", hash)).exists() {
        // BEGIN PARSE
        let temp_funcs = parse_functions(content.trim(), true);
        log!("FUNCS: {temp_funcs:?}");
        functions = temp_funcs
            .iter()
            .map(|(name, args, lines)| {
                let first_stack: Vec<Types> = lines
                    .iter()
                    .flat_map(|line| line.iter().map(|x| (*x).clone()))
                    .collect();
                let mut locals: Vec<(u16, String)> = vec![];
                let final_stack = simplify(first_stack, false, &mut locals);
                return (
                    Intern::from(name.clone()),
                    args.iter().map(|x| Intern::from(x.to_string())).collect(),
                    final_stack,
                    locals,
                );
            })
            .collect();

        let data = bincode::serialize(&functions).unwrap();
        fs::create_dir_all(".compute/").unwrap();
        File::create(format!(".compute/{}", hash))
            .unwrap()
            .write_all(&data)
            .unwrap();
        // END PARSE
    } else {
        let file = File::open(&format!(".compute/{}", hash)).unwrap();
        let mut reader = BufReader::with_capacity(128 * 1024, file);
        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer).unwrap();

        functions = bincode::deserialize(&buffer).expect(error_msg!(
            "Failed to read from cache",
            "Delete the .compute folder"
        ));
    }

    let mut main_function = functions.swap_remove(
        functions
            .iter()
            .position(|(x, _, _, _)| **x == "main")
            .unwrap(),
    );

    log!("PARSED IN: {:.2?}", now.elapsed());
    log!("FUNCS: {:?}", functions);
    log!("MAIN: {:?}", main_function);
    let now = Instant::now();
    execute(&main_function.2, &functions, vec![], &mut main_function.3);

    log_release!("EXECUTED IN: {:.2?}", now.elapsed());
    log!("TOTAL: {:.2?}", totaltime.elapsed());
}
