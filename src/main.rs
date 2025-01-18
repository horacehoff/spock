#![allow(clippy::too_many_lines)]
extern crate core;
use std::fmt::Write;

mod instr_set;
#[path = "parser/parser.rs"]
mod parser;
#[path = "parser/parser_functions.rs"]
mod parser_functions;
mod util;

use crate::instr_set::Integer;
use crate::parser::{FunctionsSlice, Operator};
use crate::parser_functions::parse_functions;
use crate::util::{error, print_form};
use colored::Colorize;
use instr_set::Instr;
use internment::Intern;
use likely_stable::{likely, unlikely};
use mimalloc::MiMalloc;
use std::fs;
use std::fs::remove_dir_all;
use std::io::{Read, Write as _};
use std::ops::{Add, Deref};
use std::path::Path;
use std::time::Instant;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

// #[unroll_for_loops]
// // #[inline(always)]
// fn process_stack(
//     stack_in: &[ParserInstr],
//     variables: &Vec<(String, ParserInstr)>,
//     functions: &[(String, &[String], &[ParserInstr])],
// ) -> ParserInstr {
//     let mut output: ParserInstr = match stack_in.first().unwrap_or(&ParserInstr::Integer(0)) {
//         ParserInstr::VariableIdentifier(ref var) => variables
//             .iter()
//             .find(|(name, _)| name == var)
//             .unwrap_or_else(|| {
//                 error(&format!("Unknown variable '{var}'"), "");
//                 std::process::exit(1)
//             })
//             .1
//             .clone(),
//         ParserInstr::Wrap(ref x) => process_stack(x, variables, functions),
//         other => {
//             if !matches!(
//                 other,
//                 ParserInstr::FunctionCall(_)
//                     | ParserInstr::NamespaceFunctionCall(_)
//                     | ParserInstr::Priority(_)
//                     | ParserInstr::Array(_, _, _)
//             ) {
//                 other.clone()
//             } else {
//                 preprocess(other, variables, functions)
//             }
//         }
//     };
//     let mut current_operator: Operator = Operator::Null;
//     for p_element in stack_in.iter().skip(1) {
//         let element: &ParserInstr = match p_element {
//             // Types::VariableIdentifier(ref var) => variables.get(var).unwrap_or_else(|| {
//             //     error(&format!("Unknown variable '{var}'"), "");
//             //     std::process::exit(1)
//             // }),
//             ParserInstr::VariableIdentifier(ref var) => {
//                 &variables
//                     .iter()
//                     .find(|(name, _)| name == var)
//                     .unwrap_or_else(|| {
//                         error(&format!("Unknown variable '{var}'"), "");
//                         std::process::exit(1)
//                     })
//                     .1
//             }
//             ParserInstr::Wrap(ref x) => &process_stack(x, variables, functions),
//             other => {
//                 if !matches!(
//                     other,
//                     ParserInstr::FunctionCall(_)
//                         | ParserInstr::NamespaceFunctionCall(_)
//                         | ParserInstr::Priority(_)
//                         | ParserInstr::Array(_, true, false)
//                         | ParserInstr::Array(_, false, true)
//                 ) {
//                     other
//                 } else {
//                     &preprocess(other, variables, functions)
//                 }
//             }
//         };
//
//         match element {
//             ParserInstr::Operation(ref op) => current_operator = *op,
//             ParserInstr::Integer(ref x) => {
//                 output = integer_ops(*x, output, current_operator);
//             }
//             ParserInstr::String(ref x) => output = string_ops(x, output, current_operator),
//             ParserInstr::Float(ref x) => output = float_ops(*x, output, current_operator),
//             ParserInstr::Array(ref x, _, false) => output = array_ops(x, output, current_operator),
//             ParserInstr::Property(ref block) => {
//                 let args: Vec<ParserInstr> =
//                     util::split_vec_box(&block.args, ParserInstr::Separator)
//                         .iter()
//                         .map(|w| process_stack(w, variables, functions))
//                         .collect();
//                 match output {
//                     ParserInstr::String(ref str) => string_props!(str, args, block.name, output),
//                     ParserInstr::Float(num) => float_props!(num, args, block.name, output),
//                     ParserInstr::Integer(num) => integer_props!(num, args, block.name, output),
//                     ParserInstr::Array(ref arr, _, false) => {
//                         array_props!(arr, args, block.name, output)
//                     }
//
//                     // OBJECTS
//                     ParserInstr::File(filepath) => {
//                         file_props!(filepath.as_str(), args, block.name, output)
//                     }
//                     _ => error(
//                         &format!(
//                             "Unknown function '{}' for object {}",
//                             block.name,
//                             get_printable_type!(output)
//                         ),
//                         "",
//                     ),
//                 }
//             }
//             ParserInstr::Or(ref x) | ParserInstr::And(ref x) => {
//                 let parsed_exp = process_stack(x, variables, functions);
//                 if_let!(
//                     likely,
//                     ParserInstr::Bool(inbool),
//                     output,
//                     {
//                         if_let!(likely, ParserInstr::Bool(sidebool), parsed_exp, {
//                             output = ParserInstr::Bool(if matches!(element, ParserInstr::Or(_)) {
//                                 inbool || sidebool
//                             } else {
//                                 inbool && sidebool
//                             });
//                         }, else {
//                             error(format!("{parsed_exp:?} is not a Boolean").as_str(), "");
//                         });
//                     }, else
//                     {
//                         error(format!("{output:?} is not a Boolean").as_str(), "");
//                     }
//                 );
//             }
//             ParserInstr::Null => {
//                 if output == ParserInstr::Null {
//                     match current_operator {
//                         Operator::Equal => output = ParserInstr::Bool(true),
//                         Operator::NotEqual => output = ParserInstr::Bool(false),
//                         _ => error(
//                             &format!(
//                                 "Cannot perform operation '{current_operator:?}' between Null and Null"
//                             ),
//                             "",
//                         ),
//                     }
//                 }
//             }
//             _ => error(&format!("TODO STACK {element:?}"), ""),
//         }
//     }
//     output
// }
//
// // #[inline(always)]
// fn process_lines(
//     line_array: &[ParserInstr],
//     variables: &mut Vec<(String, ParserInstr)>,
//     functions: &[(String, &[String], &[ParserInstr])],
// ) -> ParserInstr {
//     for line in line_array {
//         match line {
//             ParserInstr::VariableDeclaration(ref block) => {
//                 if !block.is_declared {
//                     variables.push((
//                         block.name.parse().unwrap(),
//                         process_stack(&block.value, variables, functions),
//                     ));
//                 } else {
//                     let calculated = process_stack(&block.value, variables, functions);
//                     // if let Some(x) = variables.get_mut((&block.name, _)) {
//                     //     *x = calculated;
//                     // }
//                     // variables.fin
//                     if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.name) {
//                         x.1 = calculated;
//                     }
//                 }
//             }
//             ParserInstr::NamespaceFunctionCall(ref block) => {
//                 let args: Vec<ParserInstr> =
//                     util::split_vec_box(&block.args, ParserInstr::Separator)
//                         .iter()
//                         .map(|w| process_stack(w, variables, functions))
//                         .collect();
//                 if unlikely(!namespace_functions(&block.namespace, &block.name, &args).1) {
//                     error(
//                         &format!(
//                             "Unknown function '{}'",
//                             block.namespace.join(".") + "." + block.name.as_str()
//                         ),
//                         "",
//                     );
//                 };
//             }
//             ParserInstr::FunctionCall(ref block) => {
//                 let args: Vec<ParserInstr> =
//                     util::split_vec_box(&block.args, ParserInstr::Separator)
//                         .iter()
//                         .map(|x| process_stack(x, variables, functions))
//                         .collect();
//                 let (_, matched) = builtin_functions(&block.name, &args);
//                 if block.name == "executeline" && !matched {
//                     assert_args_number!("executeline", args.len(), 1_usize);
//                     if_let!(likely, ParserInstr::String(line), &args[0], {
//                         process_stack(&parse_code(line)[0], variables, functions);
//                     }, else {
//                         error(&format!("Cannot execute line {:?}", &args[0]), "");
//                     });
//                 } else if !matched {
//                     let target_function: &(String, &[String], &[ParserInstr]) = functions
//                         .iter()
//                         .find(|func| func.0 == *block.name)
//                         .unwrap_or_else(|| {
//                             error(&format!("Unknown function '{}'", block.name), "");
//                             std::process::exit(1)
//                         });
//                     assert_args_number!(block.name, args.len(), target_function.1.len());
//                     let mut target_args: Vec<(String, ParserInstr)> = target_function
//                         .1
//                         .iter()
//                         .enumerate()
//                         .map(|(i, arg)| (arg.parse().unwrap(), args[i].clone()))
//                         .collect();
//
//                     process_lines(target_function.2, &mut target_args, functions);
//                 }
//             }
//             ParserInstr::PropertyFunction(ref block) => {
//                 return process_stack(
//                     &[
//                         process_lines(
//                             &[ParserInstr::FunctionCall(Box::from(
//                                 FunctionPropertyCallBlock {
//                                     name: block.func1_name.parse().unwrap(),
//                                     args: block.func1_args.clone(),
//                                 },
//                             ))],
//                             variables,
//                             functions,
//                         ),
//                         ParserInstr::Property(Box::from(FunctionPropertyCallBlock {
//                             name: block.func2_name.parse().unwrap(),
//                             args: block.func2_args.clone(),
//                         })),
//                     ],
//                     variables,
//                     functions,
//                 );
//             }
//             ParserInstr::Condition(ref block) => {
//                 // let data = block;
//                 if process_stack(&block.condition, variables, functions) == ParserInstr::Bool(true)
//                 {
//                     let out = process_lines(&block.code, variables, functions);
//                     if ParserInstr::Null != out {
//                         // if out != Types::Break {
//                         return out;
//                         // }
//                         // error("Cannot break in a conditional statement","Remove the \"break\" statement");
//                     }
//                 } else {
//                     for else_block in &block.else_blocks {
//                         if else_block.0.is_empty()
//                             || process_stack(&else_block.0, variables, functions)
//                                 == ParserInstr::Bool(true)
//                         {
//                             let out = process_lines(&else_block.1, variables, functions);
//                             if out != ParserInstr::Null {
//                                 return out;
//                             }
//                         }
//                     }
//                 }
//             }
//             ParserInstr::Loop(ref block) => {
//                 let loop_array = process_stack(&block.arr, variables, functions);
//                 if let ParserInstr::Array(target_array, false, false) = loop_array {
//                     variables.push((block.id.parse().unwrap(), ParserInstr::Null));
//                     for elem in target_array {
//                         // if let Some(value) = variables.get_mut(&block.id) {
//                         //     *value = elem;
//                         // }
//                         if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.id) {
//                             x.1 = elem;
//                         }
//
//                         let out: ParserInstr = process_lines(&block.code, variables, functions);
//                         if out != ParserInstr::Null {
//                             variables.swap_remove(
//                                 variables
//                                     .iter()
//                                     .position(|(name, _)| *name == block.id)
//                                     .unwrap(),
//                             );
//                             // variables.remove(&block.id);
//                             if out == ParserInstr::Break {
//                                 break;
//                             }
//                             return out;
//                         }
//                     }
//                     // variables.remove(&block.id);
//                     variables.swap_remove(
//                         variables
//                             .iter()
//                             .position(|(name, _)| *name == block.id)
//                             .unwrap(),
//                     );
//                 } else if let ParserInstr::String(ref target_string) = loop_array {
//                     variables.push((block.id.parse().unwrap(), ParserInstr::Null));
//                     for elem in target_string.chars() {
//                         // if let Some(value) = variables.get_mut(&block.id) {
//                         //     *value = Types::String(elem.to_smolstr());
//                         // }
//                         if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.id) {
//                             x.1 = ParserInstr::String(elem.to_string().parse().unwrap());
//                         }
//
//                         let out: ParserInstr = process_lines(&block.code, variables, functions);
//                         if out != ParserInstr::Null {
//                             // variables.remove(&block.id);
//                             variables.swap_remove(
//                                 variables
//                                     .iter()
//                                     .position(|(name, _)| *name == block.id)
//                                     .unwrap(),
//                             );
//                             if out == ParserInstr::Break {
//                                 break;
//                             }
//                             return out;
//                         }
//                     }
//                     // variables.remove(&block.id);
//                     variables.swap_remove(
//                         variables
//                             .iter()
//                             .position(|(name, _)| *name == block.id)
//                             .unwrap(),
//                     );
//                 }
//             }
//             ParserInstr::While(ref block) => {
//                 while let ParserInstr::Bool(true) =
//                     process_stack(&block.condition, variables, functions)
//                 {
//                     // let now = Instant::now();
//                     let out = process_lines(&block.code, variables, functions);
//                     // println!("WHILE ITERATION {:.2?}", now.elapsed());
//                     if out != ParserInstr::Null {
//                         if out == ParserInstr::Break {
//                             break;
//                         }
//                         return out;
//                     }
//                 }
//             }
//             ParserInstr::FunctionReturn(ref x) => {
//                 return process_stack(x, variables, functions);
//             }
//             ParserInstr::Break => {
//                 return ParserInstr::Break;
//             }
//             ParserInstr::Wrap(ref x) => {
//                 let x = process_lines(x, variables, functions);
//                 if x != ParserInstr::Null {
//                     return x;
//                 }
//             }
//             _ => error(&format!("TODO LINE LOGIC {line:?}"), ""),
//         }
//     }
//     ParserInstr::Null
// }

macro_rules! check_register_adress {
    ($elem: expr, $depth: expr, $i: expr, $register: expr) => {
        // if ($register.len() as u16) < $depth || $depth == 0 {
        //     $register.push($elem);
        //     $i += 1;
        //     continue;
        // }
        $register.push($elem);
        $i += 1;
        continue;
    };
}

#[inline(always)]
fn pre_match(
    input: Instr,
    variables: &mut [(Intern<String>, Instr)],
    depth: u16,
    func_args: &mut Vec<Instr>,
    functions: &FunctionsSlice,
    str_pool: &mut Vec<Intern<String>>,
) -> Instr {
    match input {
        Instr::VariableIdentifier(id) => variables[id as usize].1,
        // Function call that should return something (because depth > 0)
        Instr::FuncCall(name) => {
            if depth == 0 {
                return input;
            }
            let name = str_pool[name as usize].as_str();
            println!("ARGS ARE {func_args:?}");
            match name {
                "str" => {
                    assert_args_number!("str", func_args.len(), 1);
                    let element = func_args.remove(0);
                    match element {
                        Instr::Bool(bool) => {
                            str_pool.push(Intern::from(
                                {
                                    if bool {
                                        "true"
                                    } else {
                                        "false"
                                    }
                                }
                                .to_string(),
                            ));
                            return Instr::String((str_pool.len() - 1) as u32);
                        }
                        Instr::Integer(int) => {
                            str_pool.push(Intern::from(int.to_string()));
                            return Instr::String((str_pool.len() - 1) as u32);
                        }
                        Instr::Float(float) => {
                            str_pool.push(Intern::from(float.to_string()));
                            return Instr::String((str_pool.len() - 1) as u32);
                        }
                        Instr::String(_) => element,
                        _ => todo!(),
                    }
                }
                _ => {
                    if let Some(func) = functions
                        .iter()
                        .find(|(func_name, _, _, _, _)| name == **func_name)
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
                        let mut vars = func.4.clone();
                        execute(&func.2, functions, args, str_pool, &mut vars)
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

fn int_int(parent: Integer, child: Integer, op: Operator) -> Instr {
    match op {
        Operator::Add => Instr::Integer(parent + child),
        Operator::Sub => Instr::Integer(parent - child),
        Operator::Divide => Instr::Integer(parent / child),
        Operator::Multiply => Instr::Integer(parent * child),
        Operator::Power => Instr::Integer(parent.pow(child as u32)),
        Operator::Modulo => Instr::Integer(parent % child),
        Operator::Equal => Instr::Bool(parent == child),
        Operator::NotEqual => Instr::Bool(parent != child),
        Operator::Null => todo!("NO OP"),
        Operator::And => panic!("INT => AND"),
        Operator::Inferior => Instr::Bool(parent < child),
        Operator::InferiorEqual => Instr::Bool(parent <= child),
        Operator::Or => panic!("INT => OR"),
        Operator::Superior => Instr::Bool(parent > child),
        Operator::SuperiorEqual => Instr::Bool(parent >= child),
    }
}

fn execute(
    lines: &[Instr],
    functions: &FunctionsSlice,
    args: Vec<(Intern<String>, Instr)>,
    str_pool: &mut Vec<Intern<String>>,
    vars_pool: &mut [Intern<String>],
) -> Instr {
    util::print_instructions(lines);
    let mut stack: Vec<Instr> = Vec::with_capacity(
        lines
            .iter()
            .filter(|elem| matches!(elem, Instr::Store))
            .count(),
    );
    // keeps track of function args
    let mut args_list: Vec<Instr> = Vec::new();
    // keeps track of current "storing" depth (e.g STORE,...,STORE,... will have depth=2 after the second "STORE")
    // unclear if really needed
    let mut depth: u16 = 0;
    // keeps track of operators according to depth
    // unclear if a vec is needed
    let mut operator: Vec<Operator> = Vec::with_capacity(
        lines
            .iter()
            .filter(|elem| matches!(elem, Instr::Operation(_)))
            .count(),
    );
    // keeps track of variables
    let mut variables: Vec<(Intern<String>, Instr)> =
        vars_pool.iter().map(|id| (*id, Instr::Null)).collect();
    if !args.is_empty() {
        for (id, true_val) in variables.iter_mut() {
            if let Some(elem) = args.iter().find(|(x, _)| x == id) {
                *true_val = elem.1;
            }
        }
    }
    let mut line: usize = 0;
    let total_len = lines.len();
    while line < total_len {
        // println!("VARIABLES ARE {variables:?}");
        // println!("STACK IS {stack:?}");
        match pre_match(
            lines[line],
            &mut variables,
            depth,
            &mut args_list,
            functions,
            str_pool,
        ) {
            Instr::Store => depth += 1,
            Instr::StopStore => depth -= 1,
            Instr::Operation(op) => {
                let o2 = stack.pop().unwrap();
                let o1 = stack.pop().unwrap();
                match o1 {
                    Instr::Integer(parent) => match o2 {
                        Instr::Integer(child) => stack.push(int_int(parent, child, op)),
                        _ => {}
                    },
                    _ => {}
                }
                // operator.push(op)
            }
            Instr::VarSet(str) => {
                assert!(!stack.is_empty(), "[COMPUTE BUG] Stack empty");
                variables[str as usize].1 = stack.pop().unwrap();
            }
            Instr::If(jump_size) => {
                assert!(!stack.is_empty(), "[COMPUTE BUG] Stack empty");
                let condition = stack.pop().unwrap();
                if condition == Instr::Bool(false) {
                    line += jump_size as usize;
                } else if unlikely(condition != Instr::Bool(true)) {
                    error(&format!("'{:?}' is not a boolean", &condition), "");
                }
            }
            Instr::Jump(neg, jump_size) => {
                if neg {
                    line -= jump_size as usize;
                    continue;
                }
                line += jump_size as usize;
                continue;
            }
            Instr::StoreArg => args_list.push(stack.pop().unwrap()),
            // Function call that shouldn't return anything
            Instr::FuncCall(name) => {
                if likely(depth == 0) {
                    let func_name = str_pool[name as usize];
                    if *func_name == "print" {
                        assert_eq!(
                            args_list.len(),
                            1,
                            "Invalid number of arguments, expected 1, got {}",
                            args_list.len()
                        );
                        println!("{}", print_form(&args_list.remove(0), str_pool))
                    } else {
                    }
                } else {
                    todo!("Depth > 0 ??? (func_call)")
                }
            }
            Instr::FuncReturn => {
                return stack.pop().unwrap();
            }
            // PRIMITIVE TYPES
            Instr::Integer(int) => {
                // if depth == 0 {
                //     stack.push(Instr::Integer(int));
                //     line += 1;
                //     continue;
                // } else {
                check_register_adress!(Instr::Integer(int), depth, line, stack);
                // // }
                // let index = stack.len() - 1;
                // let elem = stack.get_mut(index).unwrap();
                // match elem {
                //     Instr::Integer(parent) => {
                //         match operator.pop().unwrap() {
                //             Operator::Add => *elem = Instr::Integer(*parent + int),
                //             Operator::Sub => *elem = Instr::Integer(*parent - int),
                //             Operator::Divide => {
                //                 assert_ne!(
                //                     int,
                //                     0,
                //                     "{}",
                //                     error_msg!(format!("Division by zero ({int} / 0)"))
                //                 );
                //                 *elem = math_to_type!(*parent as f32 / int as f32);
                //             }
                //             Operator::Multiply => *elem = Instr::Integer(*parent * int),
                //             Operator::Power => *elem = Instr::Integer(parent.pow(int as u32)),
                //             Operator::Modulo => *elem = Instr::Integer(*parent % int),
                //             Operator::Equal => *elem = Instr::Bool(*parent == int),
                //             Operator::NotEqual => *elem = Instr::Bool(*parent != int),
                //             Operator::Inferior => *elem = Instr::Bool(*parent < int),
                //             Operator::InferiorEqual => *elem = Instr::Bool(*parent <= int),
                //             Operator::Superior => *elem = Instr::Bool(*parent > int),
                //             Operator::SuperiorEqual => *elem = Instr::Bool(*parent >= int),
                //
                //             // AND
                //             // OR
                //             other => error(
                //                 &format!(
                //                     "Operation not supported:\n{} {} {}",
                //                     "Integer".blue(),
                //                     op_to_symbol(other).red(),
                //                     "Integer".blue()
                //                 ),
                //                 "",
                //             ),
                //         }
                //     }
                //     Instr::Float(parent) => match operator.pop().unwrap() {
                //         Operator::Add => *elem = Instr::Float(*parent + int as f32),
                //         Operator::Sub => *elem = Instr::Float(*parent - int as f32),
                //         Operator::Divide => {
                //             assert_ne!(
                //                 int,
                //                 0,
                //                 "{}",
                //                 error_msg!(format!("Division by zero ({int} / 0)"))
                //             );
                //             *elem = Instr::Float(*parent / int as f32)
                //         }
                //         Operator::Multiply => *elem = Instr::Float(*parent * int as f32),
                //         Operator::Power => *elem = Instr::Float(parent.powf(int as f32)),
                //         Operator::Modulo => *elem = Instr::Float(*parent % int as f32),
                //         Operator::Equal => *elem = Instr::Bool(*parent == int as f32),
                //         Operator::NotEqual => *elem = Instr::Bool(*parent != int as f32),
                //         Operator::Inferior => *elem = Instr::Bool(*parent < int as f32),
                //         Operator::InferiorEqual => *elem = Instr::Bool(*parent <= int as f32),
                //         Operator::Superior => *elem = Instr::Bool(*parent > int as f32),
                //         Operator::SuperiorEqual => *elem = Instr::Bool(*parent >= int as f32),
                //         other => error(
                //             &format!(
                //                 "Operation not supported:\n{} {} {}",
                //                 "Float".blue(),
                //                 op_to_symbol(other).red(),
                //                 "Integer".blue()
                //             ),
                //             "",
                //         ),
                //     },
                //     Instr::String(parent) => match operator.pop().unwrap() {
                //         Operator::Multiply => {
                //             let str = str_pool.get_mut(*parent as usize).unwrap();
                //             *str = Intern::from(str.repeat(int as usize));
                //         }
                //         other => error(
                //             &format!(
                //                 "Operation not supported:\n{} {} {}",
                //                 "String".blue(),
                //                 op_to_symbol(other).red(),
                //                 "Integer".blue()
                //             ),
                //             "",
                //         ),
                //     },
                //     _ => error(&format!("Cannot add Integer to {}", get_type(*elem)), ""),
                // }
            }
            Instr::Float(float) => {
                check_register_adress!(Instr::Float(float), depth, line, stack);
                // let index = stack.len() - 1;
                // if let Some(elem) = stack.get_mut(index) {
                //     match elem {
                //         Instr::Integer(parent) => {
                //             match operator.pop().unwrap() {
                //                 Operator::Add => *elem = Instr::Float(*parent as f32 + float),
                //                 Operator::Sub => *elem = Instr::Float(*parent as f32 - float),
                //                 Operator::Divide => {
                //                     assert_ne!(
                //                         float,
                //                         0.0,
                //                         "{}",
                //                         error_msg!(format!("Division by zero ({float} / 0)"))
                //                     );
                //                     *elem = math_to_type!(*parent as f32 / float);
                //                 }
                //                 Operator::Multiply => *elem = Instr::Float(*parent as f32 * float),
                //                 Operator::Power => {
                //                     *elem = Instr::Float(parent.pow(float as u32) as f32)
                //                 }
                //                 Operator::Modulo => *elem = Instr::Float(*parent as f32 % float),
                //                 Operator::Equal => *elem = Instr::Bool(*parent as f32 == float),
                //                 Operator::NotEqual => *elem = Instr::Bool(*parent as f32 != float),
                //                 Operator::Inferior => {
                //                     *elem = Instr::Bool((*parent as f32) < (float))
                //                 }
                //                 Operator::InferiorEqual => {
                //                     *elem = Instr::Bool(*parent as f32 <= float)
                //                 }
                //                 Operator::Superior => *elem = Instr::Bool(*parent as f32 > float),
                //                 Operator::SuperiorEqual => {
                //                     *elem = Instr::Bool(*parent as f32 >= float)
                //                 }
                //
                //                 // AND
                //                 // OR
                //                 other => error(
                //                     &format!(
                //                         "Operation not supported:\n{} {} {}",
                //                         "Integer".blue(),
                //                         op_to_symbol(other).red(),
                //                         "Float".blue()
                //                     ),
                //                     "",
                //                 ),
                //             }
                //         }
                //         Instr::Float(parent) => {
                //             match operator.pop().unwrap() {
                //                 Operator::Add => *elem = Instr::Float(*parent + float),
                //                 Operator::Sub => *elem = Instr::Float(*parent - float),
                //                 Operator::Divide => {
                //                     assert_ne!(
                //                         float,
                //                         0.0,
                //                         "{}",
                //                         error_msg!(format!("Division by zero ({float} / 0)"))
                //                     );
                //                     *elem = Instr::Float(*parent / float)
                //                 }
                //                 Operator::Multiply => *elem = Instr::Float(*parent * float),
                //                 Operator::Power => *elem = Instr::Float(parent.powf(float)),
                //                 Operator::Modulo => *elem = Instr::Float(*parent % float),
                //                 Operator::Equal => *elem = Instr::Bool(*parent == float),
                //                 Operator::NotEqual => *elem = Instr::Bool(*parent != float),
                //                 Operator::Inferior => *elem = Instr::Bool(*parent < float),
                //                 Operator::InferiorEqual => *elem = Instr::Bool(*parent <= float),
                //                 Operator::Superior => *elem = Instr::Bool(*parent > float),
                //                 Operator::SuperiorEqual => *elem = Instr::Bool(*parent >= float),
                //
                //                 // AND
                //                 // OR
                //                 other => error(
                //                     &format!(
                //                         "Operation not supported:\n{} {} {}",
                //                         "Float".blue(),
                //                         op_to_symbol(other).red(),
                //                         "Float".blue()
                //                     ),
                //                     "",
                //                 ),
                //             }
                //         }
                //         _ => error(&format!("Cannot add Float to {}", get_type(*elem)), ""),
                //     }
                // } else {
                //     error(
                //         "[COMPUTE] UNABLE TO RETRIEVE FROM REGISTER",
                //         "This is probably a Compute bug",
                //     );
                // }
            }
            Instr::String(str) => {
                check_register_adress!(Instr::String(str), depth, line, stack);
                // let index = stack.len() - 1;
                // if let Some(elem) = stack.get_mut(index) {
                //     match elem {
                //         Instr::String(parent) => match operator.pop().unwrap() {
                //             Operator::Add => {
                //                 let base_string = str_pool[str as usize];
                //                 if let Some(parent_string) = str_pool.get_mut(*parent as usize) {
                //                     let str = concat_string!(
                //                         parent_string.as_str(),
                //                         base_string.as_str()
                //                     );
                //                     *parent_string = Intern::from(str);
                //                 }
                //             }
                //             other => error(
                //                 &format!(
                //                     "Operation not supported:\n{} {} {}",
                //                     "String".blue(),
                //                     op_to_symbol(other).red(),
                //                     "String".blue()
                //                 ),
                //                 "",
                //             ),
                //         },
                //         Instr::Integer(parent) => match operator.pop().unwrap() {
                //             Operator::Multiply => {
                //                 if let Some(base_string) = str_pool.get_mut(str as usize) {
                //                     *base_string =
                //                         Intern::from(base_string.repeat(*parent as usize));
                //                 }
                //             }
                //             other => error(
                //                 &format!(
                //                     "Operation not supported:\n{} {} {}",
                //                     "Integer".blue(),
                //                     op_to_symbol(other).red(),
                //                     "String".blue()
                //                 ),
                //                 "",
                //             ),
                //         },
                //         _ => error(&format!("Cannot add String to {}", get_type(*elem)), ""),
                //     }
                // } else {
                //     error(
                //         "[COMPUTE] UNABLE TO RETRIEVE FROM REGISTER",
                //         "This is probably a Compute bug",
                //     );
                // }
            }
            Instr::Bool(bool) => {
                check_register_adress!(Instr::Bool(bool), depth, line, stack);
            }
            _ => {}
        }
        line += 1;
    }
    // println!("VARIABLES ARE {variables:?}");
    Instr::Null
}

fn main() {
    dbg!(size_of::<Instr>());
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

    let content = fs::read_to_string(arg).unwrap_or_else(|_| {
        error(&format!("Unable to read file '{args:?}'"), "");
        std::process::exit(1)
    });

    let now = Instant::now();
    let mut functions = parse_functions(content, true);
    let mut main_function = functions.swap_remove(
        functions
            .iter()
            .position(|(x, _, _, _, _)| **x == "main")
            .unwrap(),
    );
    log!("PARSED IN: {:.2?}", now.elapsed());

    let now = Instant::now();
    execute(
        &main_function.2,
        &functions,
        vec![],
        &mut main_function.3,
        &mut main_function.4,
    );

    log_release!("EXECUTED IN: {:.2?}", now.elapsed());
    log!("TOTAL: {:.2?}", totaltime.elapsed());
}
