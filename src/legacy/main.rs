#![allow(clippy::too_many_lines)]
#[path = "parser/functions.rs"]
mod functions;
mod instr_set;
#[path = "parser/parser.rs"]
mod parser;
mod util;

use crate::functions::parse_functions;
use crate::instr_set::{Float, Integer};
use crate::parser::{FunctionsSlice, Operator};
use crate::util::{get_type, op_to_symbol, print_form};
use colored::Colorize;
use concat_string::concat_string;
use instr_set::Instr;
use internment::Intern;
use likely_stable::unlikely;
use mimalloc::MiMalloc;
use std::fs;
use std::time::Instant;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

macro_rules! check_args {
    ($name: expr, $args:expr, $num: expr) => {
        assert_eq!(
            $args.len(),
            $num,
            "Function '{}' expected {} arguments, got {}",
            $name,
            $num,
            $args.len()
        );
    };
}

macro_rules! last_index {
    ($x: expr) => {
        ($x.len() - 1)
    };
}

#[inline(always)]
fn pre_match(
    input: Instr,
    variables: &mut [(Intern<String>, Instr)],
    func_args: &mut Vec<Instr>,
    functions: &FunctionsSlice,
    str_pool: &mut Vec<Intern<String>>,
    consts: &[Instr],
) -> Instr {
    match input {
        Instr::VariableIdentifier(id) => variables[id as usize].1,
        // Function call that should return something (because depth > 0)
        Instr::FuncCall(true, name) => match str_pool[name as usize].as_str() {
            "str" => {
                check_args!("str", func_args, 1);
                let element = func_args.remove(0);
                match element {
                    Instr::Bool(bool) => {
                        str_pool.push(Intern::from(
                            { if bool { "true" } else { "false" } }.to_string(),
                        ));
                        Instr::String((str_pool.len() - 1) as u32)
                    }
                    Instr::Integer(int) => {
                        str_pool.push(Intern::from(int.to_string()));
                        Instr::String((str_pool.len() - 1) as u32)
                    }
                    Instr::Float(float) => {
                        str_pool.push(Intern::from(float.to_string()));
                        Instr::String((str_pool.len() - 1) as u32)
                    }
                    Instr::String(_) => element,
                    _ => todo!(),
                }
            }
            func => {
                if let Some(func) = functions
                    .iter()
                    .find(|(func_name, _, _, _, _, _)| func == **func_name)
                {
                    let expected_args = &func.1;
                    check_args!("str", func_args, expected_args.len());
                    let mut pool = func.3.clone();
                    let args: Vec<(Intern<String>, Instr)> = expected_args
                        .iter()
                        .enumerate()
                        .map(|(x, y)| {
                            let arg_val = func_args.remove(x);
                            if let Instr::String(str) = arg_val {
                                pool.push(str_pool[str as usize]);
                                (*y, Instr::String((pool.len() - 1) as u32))
                            } else {
                                (*y, arg_val)
                            }
                        })
                        .collect();
                    let return_value =
                        execute(&func.2, functions, &args, &mut pool, &func.4, &func.5);
                    if let Instr::String(str) = return_value {
                        str_pool.push(pool[str as usize]);
                        return Instr::String((str_pool.len() - 1) as u32);
                    }
                    return_value
                } else {
                    error!(format_args!("Unknown function '{}'", func.red()));
                }
            }
        },
        _ => input,
    }
}

#[inline(always)]
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
        Operator::Inferior => Instr::Bool(parent < child),
        Operator::InferiorEqual => Instr::Bool(parent <= child),
        Operator::Superior => Instr::Bool(parent > child),
        Operator::SuperiorEqual => Instr::Bool(parent >= child),
        _ => unreachable!(),
    }
}

#[inline(always)]
fn float_float(parent: Float, child: Float, op: Operator) -> Instr {
    match op {
        Operator::Add => Instr::Float(parent + child),
        Operator::Sub => Instr::Float(parent - child),
        Operator::Divide => Instr::Float(parent / child),
        Operator::Multiply => Instr::Float(parent * child),
        Operator::Power => Instr::Float(parent.powf(child)),
        Operator::Modulo => Instr::Float(parent % child),
        Operator::Equal => Instr::Bool(parent == child),
        Operator::NotEqual => Instr::Bool(parent != child),
        Operator::Inferior => Instr::Bool(parent < child),
        Operator::InferiorEqual => Instr::Bool(parent <= child),
        Operator::Superior => Instr::Bool(parent > child),
        Operator::SuperiorEqual => Instr::Bool(parent >= child),
        _ => unreachable!(),
    }
}

#[inline(always)]
fn str_str(parent: u32, child: u32, op: Operator, str_pool: &mut Vec<Intern<String>>) -> Instr {
    match op {
        Operator::Add => {
            let base_string = str_pool[child as usize];
            let parent_string = str_pool.get_mut(parent as usize).unwrap();
            *parent_string =
                Intern::from(concat_string!(parent_string.as_str(), base_string.as_str()));
            Instr::String(parent)
        }
        Operator::Equal => Instr::Bool(str_pool[child as usize] == str_pool[parent as usize]),
        Operator::NotEqual => Instr::Bool(str_pool[child as usize] != str_pool[parent as usize]),
        other => {
            error!(format_args!(
                "Operation not implemented: String {} String",
                op_to_symbol(other)
            ));
        }
    }
}

fn execute(
    lines: &[Instr],
    functions: &FunctionsSlice,
    args: &[(Intern<String>, Instr)],
    str_pool: &mut Vec<Intern<String>>,
    vars_pool: &[Intern<String>],
    consts: &[Instr],
) -> Instr {
    util::print_instructions(lines);
    let mut stack: Vec<Instr> = Vec::with_capacity(10);
    // keeps track of function args
    let mut args_list: Vec<Instr> = Vec::with_capacity(10);
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
        match pre_match(
            lines[line],
            &mut variables,
            &mut args_list,
            functions,
            str_pool,
            consts,
        ) {
            Instr::Operation(op) => {
                let o2 = stack.pop().unwrap();
                let o1 = stack.last_mut().unwrap();
                match (*o1, o2) {
                    (Instr::Integer(parent), Instr::Integer(child)) => {
                        *o1 = int_int(parent, child, op)
                    }
                    (Instr::Float(parent), Instr::Float(child)) => {
                        *o1 = float_float(parent, child, op)
                    }
                    (Instr::String(parent), Instr::String(child)) => {
                        *o1 = str_str(parent, child, op, str_pool);
                    }
                    _ => {
                        error!(format_args!(
                            "Operation not implemented: {} {} {}",
                            get_type(*o1),
                            op_to_symbol(op),
                            get_type(o2)
                        ));
                    }
                }
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
                    error!(format_args!("'{:?}' is not a boolean", &condition));
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
            Instr::FuncCall(false, name) => {
                // println!("---");
                // println!("STR POOL IS {str_pool:?}");
                // println!("ID IS  {name:?}");
                // println!("QUERIED NAME IS   {:?}", str_pool[name as usize]);
                // println!("---");
                let func_name = str_pool[name as usize];
                match func_name.as_str() {
                    "print" => {
                        check_args!("str", args_list, 1);
                        println!("{}", print_form(&args_list.remove(0), str_pool))
                    }
                    function => {
                        if let Some(func) =
                            functions.iter().find(|(x, _, _, _, _, _)| **x == function)
                        {
                            let expected_args = &func.1;
                            check_args!(function, args_list, expected_args.len());
                            let mut pool = func.3.clone();
                            let args: Vec<(Intern<String>, Instr)> = expected_args
                                .iter()
                                .enumerate()
                                .map(|(x, y)| {
                                    let arg_val = args_list.remove(x);
                                    if let Instr::String(str) = arg_val {
                                        pool.push(str_pool[str as usize]);
                                        (*y, Instr::String((pool.len() - 1) as u32))
                                    } else {
                                        (*y, arg_val)
                                    }
                                })
                                .collect();
                            execute(&func.2, functions, &args, &mut pool, &func.4, &func.5);
                        } else {
                            error!(format_args!("Unknown function '{}'", function.red()));
                        }
                    }
                }
            }
            Instr::FuncReturn => return stack.pop().unwrap(),
            // PRIMITIVE TYPES
            other => stack.push(other),
        }
        line += 1;
    }
    Instr::Null
}

fn main() {
    dbg!(size_of::<Instr>());
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args().skip(1).collect();
    // if args.is_empty() {
    //     println!(
    //         "
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
    //     );
    //     return;
    // } else if args == vec!["-h"] {
    //     println!(
    //         "
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
    //     );
    //     return;
    // } else if args.len() >= 2
    //     && (&args[1] == "-c" || &args[1] == "--clear-cache")
    //     && Path::new(".compute").exists()
    // {
    //     remove_dir_all(Path::new(".compute")).unwrap_or_else(|_| {
    //         error!("Failed to delete the cache folder (.compute)");
    //     });
    // }
    // let arg = args.first().unwrap();

    let content = fs::read_to_string("../../test.spock").unwrap_or_else(|_| {
        error!(format_args!("Unable to read file '{args:?}'"), "");
    });

    let now = Instant::now();
    let mut functions = parse_functions(content, true);
    let mut main_function = functions.swap_remove(
        functions
            .iter()
            .position(|(x, _, _, _, _, _)| **x == "main")
            .unwrap(),
    );
    log!("PARSED IN: {:.2?}", now.elapsed());

    let now = Instant::now();
    execute(
        &main_function.2,
        &functions,
        &[],
        &mut main_function.3,
        &mut main_function.4,
        &main_function.5,
    );

    log_release!("EXECUTED IN: {:.2?}", now.elapsed());
    log!("TOTAL: {:.2?}", totaltime.elapsed());
}
