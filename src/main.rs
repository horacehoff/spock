use crate::display::format_data;
use crate::parser::parse;
use crate::util::get_type;
use builtin_funcs::FUNCS;
use colored::Colorize;
use concat_string::concat_string;
use inline_colorization::*;
use internment::Intern;
use likely_stable::{if_likely, likely};
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::io::Write;
use std::time::Instant;

mod builtin_funcs;
mod display;
mod parser;
mod util;

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Data {
    Number(f64),
    Bool(bool),
    String(Intern<String>),
    Array(u16),
    Null,
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Instr {
    Print(u16),

    // LOGIC
    // size -- is_neg
    Jmp(u16, bool),
    // condition id -- size
    Cmp(u16, u16),
    // CopyArg(u16, u16),
    Mov(u16, u16),

    // OPS
    Add(u16, u16, u16),
    Mul(u16, u16, u16),
    Sub(u16, u16, u16),
    Div(u16, u16, u16),
    Mod(u16, u16, u16),
    Pow(u16, u16, u16),
    Eq(u16, u16, u16),
    NotEq(u16, u16, u16),
    Sup(u16, u16, u16),
    SupEq(u16, u16, u16),
    Inf(u16, u16, u16),
    InfEq(u16, u16, u16),
    BoolAnd(u16, u16, u16),
    BoolOr(u16, u16, u16),
    Neg(u16, u16),

    // Funcs
    Abs(u16, u16),
    Num(u16, u16),
    Str(u16, u16),
    Bool(u16, u16),
    Input(u16, u16),

    StoreFuncArg(u16),
    ApplyFunc(u8, u16, u16),

    ArrayMov(u16, u16, u16),
    // different than ArrayMov => looks into the consts
    ArrayMod(u16, u16, u16),
    GetIndex(u16, u16, u16),
}

fn execute(
    instructions: &[Instr],
    consts: &mut [Data],
    func_args_count: usize,
    arrays: &mut HashMap<u16, Vec<Data>>,
) {
    println!("INSTR ARE {instructions:?}");
    let mut func_args: Vec<u16> = Vec::with_capacity(func_args_count);

    let len = instructions.len();
    let mut i: usize = 0;
    while i < len {
        match instructions[i] {
            Instr::Jmp(size, is_neg) => {
                if is_neg {
                    i -= size as usize;
                } else {
                    i += size as usize;
                }
                continue;
            }
            Instr::Cmp(cond_id, size) => {
                if let Data::Bool(false) = consts[cond_id as usize] {
                    i += size as usize;
                    continue;
                }
            }
            Instr::Add(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Number(parent + child);
                    }
                    (Data::String(parent), Data::String(child)) => {
                        let result = concat_string!(*parent, *child);
                        consts[dest as usize] = Data::String(Intern::from(result));
                    }
                    (Data::Array(a), Data::Array(b)) => {
                        let id = arrays.len() as u16;
                        arrays.insert(id, [arrays[&a].clone(), arrays[&b].clone()].concat());
                        consts[dest as usize] = Data::Array(id);
                    }
                    _ => {
                        error_b!(format_args!(
                            "UNSUPPORTED OPERATION: {} + {}",
                            format_data(first_elem, arrays),
                            format_data(second_elem, arrays)
                        ));
                    }
                }
            }
            Instr::Mul(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent * child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} * {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::Div(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent / child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} / {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::Sub(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent - child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} - {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::Mod(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent % child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} % {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::Pow(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent.powf(child));
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} ^ {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::Eq(o1, o2, dest) => {
                let val = consts[o1 as usize] == consts[o2 as usize];
                consts[dest as usize] = Data::Bool(val);
            }
            Instr::NotEq(o1, o2, dest) => {
                let val = consts[o1 as usize] != consts[o2 as usize];
                consts[dest as usize] = Data::Bool(val);
            }
            Instr::Sup(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent > child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} > {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::SupEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent >= child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} >= {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::Inf(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent < child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} < {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::InfEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent <= child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} <= {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::BoolAnd(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent && child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} && {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::BoolOr(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent || child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} || {:?}",
                        format_data(first_elem, arrays), format_data(second_elem, arrays)
                    ));
                }}
            }
            Instr::Mov(tgt, dest) => {
                consts[dest as usize] = consts[tgt as usize];
            }
            Instr::Neg(tgt, dest) => {
                let tgt = consts[tgt as usize];
                if_likely! {let Data::Number(x) = tgt => {
                    consts[dest as usize] = Data::Number(-x);
                } else {
                    error_b!(format_args!("UNSUPPORTED OPERATION: -{}", format_data(tgt, arrays)));
                }}
            }
            Instr::Print(target) => {
                let elem = consts[target as usize];
                println!("{}", format_data(elem, arrays));
            }
            Instr::Abs(tgt, dest) => {
                if let Data::Number(x) = consts[tgt as usize] {
                    consts[dest as usize] = Data::Number(x.abs());
                }
            }
            Instr::Num(tgt, dest) => {
                let base = consts[tgt as usize];
                match base {
                    Data::String(str) => {
                        consts[dest as usize] =
                            Data::Number(str.parse::<f64>().unwrap_or_else(|_| {
                                error_b!(format_args!("CANNOT CONVERT '{str}' TO NUMBER"));
                            }));
                    }
                    Data::Number(_) => consts[dest as usize] = base,
                    other => {
                        error_b!(format_args!(
                            "CANNOT CONVERT {} TO NUMBER",
                            format_data(other, arrays)
                        ));
                    }
                }
            }
            Instr::Str(tgt, dest) => {
                consts[dest as usize] =
                    Data::String(Intern::from(format_data(consts[tgt as usize], arrays)));
            }
            Instr::Bool(tgt, dest) => {
                let base = consts[tgt as usize];
                if let Data::String(str) = base {
                    consts[dest as usize] = Data::Bool(str.parse::<bool>().unwrap_or_else(|_| {
                        error_b!(format_args!("CANNOT CONVERT {str} TO BOOL"));
                    }));
                } else {
                    error_b!(format_args!(
                        "CANNOT CONVERT {} TO BOOL",
                        format_data(base, arrays)
                    ));
                }
            }
            Instr::Input(msg, dest) => {
                let base = consts[msg as usize];
                if let Data::String(str) = base {
                    println!("{str}");
                    std::io::stdout().flush().unwrap();
                    let mut line = String::new();
                    std::io::stdin().read_line(&mut line).unwrap();
                    consts[dest as usize] = Data::String(Intern::from(line.trim().to_string()));
                } else {
                    error_b!(format_args!(
                        "{color_red}{}{color_reset} is not a string",
                        format_data(base, arrays)
                    ));
                }
            }
            Instr::StoreFuncArg(id) => func_args.push(id),
            Instr::ApplyFunc(fctn_id, tgt, dest) => {
                FUNCS[fctn_id as usize](tgt, dest, consts, &mut func_args, arrays);
            }
            Instr::ArrayMov(tgt, dest, idx) => {
                arrays.get_mut(&dest).unwrap()[idx as usize] = consts[tgt as usize];
                print!("ARRAYS {arrays:?}");
            }
            Instr::ArrayMod(tgt, dest, idx) => {
                if let Data::Number(index) = consts[idx as usize] {
                    let elem = arrays.get_mut(&dest).unwrap();
                    if likely(elem.len() > index as usize) {
                       elem[index as usize] = consts[tgt as usize];
                        print!("ARRAYS {arrays:?}");
                    } else {
                        error_b!(format_args!(
                            "Trying to get index {color_red}{}{color_reset} but Array has {} elements",
                            index,
                            elem.len()
                        ));
                    }

                }
            }
            Instr::GetIndex(tgt, index, dest) => {
                let target = consts[tgt as usize];
                if let Data::Number(idx) = consts[index as usize] {
                    let idx = idx as usize;
                    match target {
                        Data::Array(x) => {
                            let arr = &arrays[&x];
                            if likely(arr.len() > idx) {
                                consts[dest as usize] = arr[idx];
                            } else {
                                error_b!(format_args!(
                                    "Trying to get index {color_red}{}{color_reset} but Array {} has {} elements",
                                    idx,
                                    format_data(target, arrays).blue(),
                                    arr.len()
                                ));
                            }
                        }
                        Data::String(str) => {
                            if likely(str.len() > idx) {
                                consts[dest as usize] = Data::String(Intern::from(
                                    str.get(idx..(idx + 1)).unwrap().to_string(),
                                ));
                            } else {
                                error_b!(format_args!(
                                    "Trying to get index {color_red}{}{color_reset} but String \"{}\" has {} letters",
                                    idx,
                                    format_data(target, arrays).blue(),
                                    str.len()
                                ));
                            }
                        }
                        other => {
                            error_b!(format_args!("Cannot index {}", get_type(other).red()));
                        }
                    }
                }
            }
        }
        i += 1;
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    Pow,
    Eq,
    NotEq,
    Sup,
    SupEq,
    Inf,
    InfEq,
    BoolAnd,
    BoolOr,
    Neg,
}

// Live long and prosper
fn main() {
    let mut contents = std::fs::read_to_string("test.spock").unwrap();
    contents = contents
        .lines()
        .filter_map(|mut line| {
            if line.starts_with("//") {
                return None;
            } else if let Some(idx) = line.find("//") {
                let mut in_str = false;
                for c in line.chars().take(idx + 2) {
                    if c == '"' {
                        in_str = !in_str;
                    } else if !in_str && c == '/' {
                        line = &line[..idx];
                    }
                }
            }
            Some(line)
        })
        .collect::<Vec<&str>>()
        .join("\r\n");
    print!("{contents:?}");

    let (instructions, mut consts, mut arrays) = parse(&contents);

    let now = Instant::now();

    let mut func_args_count = 0;
    for x in &instructions {
        if matches!(x, Instr::StoreFuncArg(_)) {
            func_args_count += 1;
        } else if matches!(x, Instr::ApplyFunc(_, _, _)) {
            func_args_count = 0;
        }
    }
    execute(&instructions, &mut consts, func_args_count, &mut arrays);

    println!("EXEC TIME {:.2?}", now.elapsed());
}
