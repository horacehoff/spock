use crate::display::format_data;
use crate::parser::parse;
use crate::util::get_type;
use builtin_funcs::FUNCS;
use concat_string::concat_string;
use fnv::FnvHashMap;
use inline_colorization::*;
use internment::Intern;
use likely_stable::{if_likely, likely, unlikely};
use std::cmp::PartialEq;
use std::fs;
use std::fs::File;
use std::hint::unreachable_unchecked;
use std::io::Write;
use std::time::Instant;

mod builtin_funcs;
mod display;
mod ops;
mod optimizations;
mod parser;
mod tests;
mod util;

#[cfg(feature = "int")]
pub type Num = i64;

#[cfg(not(feature = "int"))]
pub type Num = f64;

#[macro_export]
macro_rules! is_float {
    ($if:expr, $else:expr) => {{
        #[cfg(feature = "int")]
        {
            $else
        }

        #[cfg(not(feature = "int"))]
        {
            $if
        }
    }};
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Data {
    Number(Num),
    Bool(bool),
    String(Intern<String>),
    Array(u16),
    Null,
    File(Intern<String>),
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
    InfCmp(u16, u16, u16),
    InfEqCmp(u16, u16, u16),
    SupCmp(u16, u16, u16),
    SupEqCmp(u16, u16, u16),
    EqCmp(u16, u16, u16),
    NotEqCmp(u16, u16, u16),

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

    // General functions
    Type(u16, u16),
    Num(u16, u16),
    Str(u16, u16),
    Bool(u16, u16),
    Input(u16, u16),
    Floor(u16, u16),
    Sqrt(u16, u16),
    // start,end,dest
    Range(u16, u16, u16),
    // path - dest - create?
    IoOpen(u16, u16, u16),
    IoDelete(u16),

    StoreFuncArg(u16),
    ApplyFunc(u8, u16, u16),

    ArrayMov(u16, u16, u16),
    // different than ArrayMov => looks into the consts
    ArrayMod(u16, u16, u16),
    GetIndex(u16, u16, u16),

    Call(u16, u16),    // function_start_index, return_target_id
    Ret(u16, u16),     // return obj id -- return target id
    MovAnon(u16, u16), // same than mov, used because mov can be changed by the parser

    TheAnswer(u16),
    // array - element
    Push(u16, u16),
    // array/str - dest
    Len(u16, u16),
    // tgt - separator - dest
    Split(u16, u16, u16),

    // TEMP - NEVER APPEARS IN FINAL CODE
    Break(u16),
    Continue(u16),
}

// struct CallFrame {
//     ret_addr: u16,
//     to_return:u16
// }
//  ===
type CallFrame = (usize, u16);

pub fn execute(
    instructions: &[Instr],
    consts: &mut [Data],
    func_args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
    call_stack: &mut Vec<CallFrame>,
) {
    let mut stuff: Vec<Data> = Vec::with_capacity(consts.len() * call_stack.capacity());
    let len = instructions.len();
    let mut i: usize = 0;
    while i < len {
        match instructions[i] {
            Instr::Jmp(size, is_neg) => {
                if is_neg {
                    i -= size as usize;
                    continue;
                } else {
                    i += size as usize;
                    continue;
                }
            }
            Instr::Cmp(cond_id, size) => {
                if let Data::Bool(false) = consts[cond_id as usize] {
                    i += size as usize;
                    continue;
                }
            }

            // funcs
            Instr::Call(x, y) => {
                call_stack.push((i + 1, y));
                i = x as usize;
                stuff.extend_from_slice(consts.as_ref());
                continue;
            }
            Instr::Ret(x, y) => {
                let val = consts[x as usize];
                if let Some((ret_i, dest)) = call_stack.pop() {
                    let consts_part = stuff.split_off(stuff.len() - consts.len());
                    consts.copy_from_slice(&consts_part);
                    consts[dest as usize] = val;
                    i = ret_i;
                    continue;
                } else {
                    consts[y as usize] = val;
                }
            }
            Instr::Add(o1, o2, dest) => match (consts[o1 as usize], consts[o2 as usize]) {
                (Data::Number(parent), Data::Number(child)) => {
                    consts[dest as usize] = Data::Number(parent + child);
                }
                (Data::String(parent), Data::String(child)) => {
                    let result = concat_string!(*parent, *child);
                    consts[dest as usize] = Data::String(Intern::from(result));
                }
                (Data::Array(a), Data::Array(b)) => {
                    let id = arrays.len() as u16;
                    let arr_a = &arrays[&a];
                    let arr_b = &arrays[&b];

                    let mut combined = Vec::with_capacity(arr_a.len() + arr_b.len());
                    combined.extend_from_slice(arr_a);
                    combined.extend_from_slice(arr_b);
                    arrays.insert(id, combined);
                    consts[dest as usize] = Data::Array(id);
                }
                (a, b) => {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {} + {}",
                        format_data(a, arrays),
                        format_data(b, arrays)
                    ));
                }
            },
            Instr::Mul(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent * child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} * {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::Div(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent / child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} / {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::Sub(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent - child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} - {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::Mod(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent % child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} % {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::Pow(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(is_float!(parent.powf(child), parent.pow(child as u32)));
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} ^ {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::Eq(o1, o2, dest) => {
                consts[dest as usize] = Data::Bool(consts[o1 as usize] == consts[o2 as usize]);
            }
            Instr::EqCmp(o1, o2, jump_size) => {
                if consts[o1 as usize] != consts[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::NotEq(o1, o2, dest) => {
                let val = consts[o1 as usize] != consts[o2 as usize];
                consts[dest as usize] = Data::Bool(val);
            }
            Instr::NotEqCmp(o1, o2, jump_size) => {
                if consts[o1 as usize] == consts[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::Sup(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent > child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} > {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::SupCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent <= child {
                        i += jump_size as usize;
                        continue;
                    }
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} > {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::SupEq(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent >= child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} >= {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::SupEqCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent < child {
                        i += jump_size as usize;
                        continue;
                    }
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} >= {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::Inf(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent < child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} < {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::InfCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent >= child {
                    i += jump_size as usize;
                    continue;
                }
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} < {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::InfEq(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent <= child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} <= {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::InfEqCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent > child {
                        i += jump_size as usize;
                        continue;
                    }
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} <= {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::BoolAnd(o1, o2, dest) => {
                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent && child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} && {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::BoolOr(o1, o2, dest) => {
                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent || child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} || {:?}",
                        format_data(consts[o1 as usize], arrays), format_data(consts[o2 as usize], arrays)
                    ));
                }}
            }
            Instr::Mov(tgt, dest) | Instr::MovAnon(tgt, dest) => {
                consts[dest as usize] = consts[tgt as usize];
            }
            Instr::Neg(tgt, dest) => {
                // let tgt = consts[tgt as usize];
                if_likely! {let Data::Number(x) = consts[tgt as usize] => {
                    consts[dest as usize] = Data::Number(-x);
                } else {
                    error_b!(format_args!("UNSUPPORTED OPERATION: -{}", format_data(consts[tgt as usize], arrays)));
                }}
            }
            Instr::Print(target) => {
                println!("{}", format_data(consts[target as usize], arrays));
            }
            Instr::Type(tgt, dest) => {
                consts[dest as usize] = Data::String(Intern::from(get_type(consts[tgt as usize])));
            }
            Instr::Num(tgt, dest) => match consts[tgt as usize] {
                Data::String(str) => {
                    consts[dest as usize] = Data::Number(str.parse::<Num>().unwrap_or_else(|_| {
                        error_b!(format_args!("CANNOT CONVERT '{str}' TO NUMBER"));
                    }));
                }
                Data::Number(num) => consts[dest as usize] = Data::Number(num),
                other => {
                    error_b!(format_args!(
                        "CANNOT CONVERT {} TO NUMBER",
                        format_data(other, arrays)
                    ));
                }
            },
            Instr::Str(tgt, dest) => {
                consts[dest as usize] =
                    Data::String(Intern::from(format_data(consts[tgt as usize], arrays)));
            }
            Instr::Bool(tgt, dest) => {
                let base = consts[tgt as usize];
                if_likely! {let Data::String(str) = base => {
                    consts[dest as usize] = Data::Bool(str.parse::<bool>().unwrap_or_else(|_| {
                        error_b!(format_args!("CANNOT CONVERT {str} TO BOOL"));
                    }));
                } else {
                    error_b!(format_args!(
                        "CANNOT CONVERT {} TO BOOL",
                        format_data(base, arrays)
                    ));
                }}
            }
            Instr::Input(msg, dest) => {
                let base = consts[msg as usize];
                if_likely! {let Data::String(str) = base => {
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
                }}
            }
            Instr::StoreFuncArg(id) => func_args.push(id),
            Instr::ApplyFunc(fctn_id, tgt, dest) => {
                FUNCS[fctn_id as usize](tgt, dest, consts, func_args, arrays);
            }
            // takes tgt from consts, moves it to dest-th array at idx-th index
            Instr::ArrayMov(tgt, dest, idx) => {
                arrays.get_mut(&dest).unwrap()[idx as usize] = consts[tgt as usize];
            }
            // takes tgt from consts, idx from consts,
            Instr::ArrayMod(tgt, dest, idx) => {
                if_likely! {let Data::Number(index) = consts[idx as usize] => {
                    let requested_mod = consts[dest as usize];
                    if let Data::Array(array_id) = consts[tgt as usize] {
                        let array = arrays.get_mut(&array_id).unwrap();
                        print!("ARRAY is {array:?}");
                        if likely(array.len() > index as usize) {
                            array[index as usize] = requested_mod;
                        } else {
                            error_b!(format_args!(
                                "Trying to get index {color_red}{}{color_reset} but Array has {} elements",
                                index,
                                array.len()
                            ));
                        }
                    } else if let Data::String(str_id) = consts.get_mut(tgt as usize).unwrap() {
                        if_likely!{let Data::String(letter) = requested_mod => {
                            if likely(str_id.len() > index as usize) {
                                let mut temp = str_id.to_string();
                                temp.remove(index as usize);
                                temp.insert_str(index as usize, &letter);
                                *str_id = Intern::from(temp);
                            } else {
                                error_b!(format_args!(
                                    "Trying to get index {color_red}{}{color_reset} but String \"{color_blue}{}{color_reset}\" has {} characters",
                                    index,
                                    str_id,
                                    str_id.len()
                                ));
                            }
                        } else {
                            error_b!(format_args!(
                                "Cannot replace type {color_blue}Letter{color_reset} by type {color_red}{}{color_reset}",
                                get_type(requested_mod)
                            ));
                        }}
                    } else {
                        error_b!(format_args!(
                            "Cannot index type {color_red}{}{color_reset}",
                            get_type(consts[tgt as usize])
                        ));
                    }
                }}
            }
            // takes tgt from  consts, index is index, dest is consts index destination
            Instr::GetIndex(tgt, index, dest) => {
                if_likely! {let Data::Number(idx) = consts[index as usize] => {
                    let idx = idx as usize;
                    match consts[tgt as usize] {
                        Data::Array(x) => {
                            let arr = &arrays[&x];
                            if likely(arr.len() > idx) {
                                consts[dest as usize] = arr[idx];
                            } else {
                                error_b!(format_args!(
                                    "Trying to get index {color_red}{}{color_reset} but Array {color_blue}{}{color_reset} has {} elements",
                                    idx,
                                    format_data(Data::Array(x), arrays),
                                    arr.len()
                                ));
                            }
                        }
                        Data::String(str) => {
                            if likely(str.len() > idx) {
                                consts[dest as usize] = Data::String(Intern::from(
                                    str.get(idx..=idx).unwrap().to_string(),
                                ));
                            } else {
                                error_b!(format_args!(
                                    "Trying to get index {color_red}{}{color_reset} but String \"{color_blue}{}{color_reset}\" has {} letters",
                                    idx,
                                    format_data(Data::String(str), arrays),
                                    str.len()
                                ));
                            }
                        }
                        other => {
                            error_b!(format_args!(
                                "Cannot index {color_red}{}{color_reset}",
                                get_type(other)
                            ));
                        }
                    }
                } else {
                    error_b!(format_args!(
                        "{} is not a valid index",
                        format_data(consts[index as usize], arrays)
                    ));
                }}
            }
            Instr::Range(min, max, dest) => {
                if_likely! {
                    let Data::Number(x) = consts[min as usize] => {
                        if_likely! {let Data::Number(y) = consts[max as usize] => {
                            let id = arrays.len() as u16;
                            arrays.insert(id, (x as u64..y as u64).map(|x| Data::Number(x as Num)).collect());
                            consts[dest as usize] = Data::Array(id);
                        }}
                    }
                }
            }
            Instr::IoOpen(path, dest, create) => {
                if_likely! {let Data::String(str) = consts[path as usize] => {
                    if_likely!{let Data::Bool(create) = consts[create as usize] => {
                        if create {
                            File::create(str.as_str()).unwrap_or_else(|_| {
                                error_b!(format_args!("Cannot create file {color_red}{str}{color_reset}"));
                            });
                        } else if unlikely(!fs::exists(str.as_str()).unwrap_or_else(|_| {
                            error_b!(format_args!("Cannot check existence of file {color_red}{str}{color_reset}"));
                        })) {
                            error_b!(format_args!("File {color_red}{str}{color_reset} does not exist"));
                        }
                        consts[dest as usize] = Data::File(str);
                    } else {
                        error_b!(format_args!("Invalid create option: {color_red}{}{color_reset}", format_data(consts[create as usize], arrays)));
                    }}
                } else {
                    error_b!(format_args!("Invalid file path: {color_red}{}{color_reset}", format_data(consts[path as usize], arrays)));
                }}
            }
            Instr::IoDelete(path) => {
                if_likely! {let Data::String(str) = consts[path as usize] => {
                    fs::remove_file(str.as_str()).unwrap_or_else(|_| {
                        error_b!(format_args!("Cannot remove file {color_red}{str}{color_reset}"));
                    });
                } else {
                    error_b!(format_args!("Invalid file path: {color_red}{}{color_reset}", format_data(consts[path as usize], arrays)));
                }}
            }
            Instr::Floor(tgt, dest) => {
                if_likely! {let Data::Number(x) = consts[tgt as usize] => {
                    consts[dest as usize] = Data::Number(is_float!(x.floor(),x));
                } else {
                    error_b!(format_args!(
                        "Cannot floor {color_red}{}{color_reset}",
                        format_data(consts[tgt as usize], arrays)
                    ));
                }}
            }
            Instr::TheAnswer(dest) => {
                println!(
                    "The answer to the Ultimate Question of Life, the Universe, and Everything is 42."
                );
                consts[dest as usize] = Data::Number(42.0 as Num);
            }
            Instr::Push(array, element) => {
                if_likely! {let Data::Array(id) = consts[array as usize] => {
                    arrays
                        .get_mut(&id)
                        .unwrap()
                        .push(consts[element as usize]);
                } else {
                    error_b!(format_args!("Cannot push element to {color_red}{}{color_reset}", format_data(consts[array as usize], arrays)));
                }}
            }
            Instr::Len(tgt, dest) => {
                consts[dest as usize] = match consts[tgt as usize] {
                    Data::Array(arr) => Data::Number(arrays[&arr].len() as Num),
                    Data::String(str) => Data::Number(str.chars().count() as Num),
                    x => {
                        error_b!(format_args!(
                            "Cannot get length of type {color_red}{}{color_reset}",
                            get_type(x)
                        ));
                    }
                };
            }
            Instr::Sqrt(tgt, dest) => {
                if_likely! {let Data::Number(num) = consts[tgt as usize] => {
                    consts[dest as usize] = Data::Number(is_float!(num.sqrt(), num.isqrt()))
                } else {
                    error_b!(format_args!("Cannot compute square root of {color_red}{}{color_reset}", format_data(consts[tgt as usize], arrays)));
                }}
            }
            Instr::Split(tgt, sep, dest) => {
                if let Data::String(str) = consts[tgt as usize] {
                    if let Data::String(separator) = consts[sep as usize] {
                        let id = arrays.len() as u16;
                        arrays.insert(
                            id,
                            str.split(separator.as_str())
                                .map(|x| Data::String(Intern::from_ref(x)))
                                .collect(),
                        );
                        consts[dest as usize] = Data::Array(id);
                    }
                }
            }
            Instr::Break(_) | Instr::Continue(_) => unsafe { unreachable_unchecked() },
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

fn clean_contents(inp: &str, base_name: &str) -> String {
    inp.lines()
        .filter_map(|mut line| {
            if line.starts_with("//") {
                None
            } else if let Some(idx) = line.find("//") {
                let mut in_str = false;
                for c in line.chars().take(idx + 2) {
                    if c == '"' {
                        in_str = !in_str;
                    } else if !in_str && c == '/' {
                        line = &line[..idx];
                    }
                }
                Some(line.to_string())
            } else if line.starts_with("import") {
                let import_path = line
                    .trim_start_matches("import")
                    .trim_end_matches(';')
                    .trim();
                if import_path != base_name {
                    let path = clean_contents(&fs::read_to_string(import_path).unwrap_or_else(|_| {
                        error!(line, format_args!("Unable to read & import file '{color_red}{import_path}{color_reset}'"));
                    }), base_name);
                    Some(path)
                } else {
                    None
                }
            } else {
                Some(line.to_string())
            }
        })
        .collect::<Vec<String>>()
        .join("\r\n")
}

fn get_vec_capacity(instructions: &[Instr]) -> (usize, usize) {
    let mut func_args_count = 0;
    let mut func_args_count_max = 0;
    let mut call_stack_count = 0;
    let mut call_stack_count_max = 0;
    for x in instructions {
        if matches!(x, Instr::StoreFuncArg(_)) {
            func_args_count += 1;
        } else if matches!(x, Instr::ApplyFunc(_, _, _)) && func_args_count > func_args_count_max {
            func_args_count_max = func_args_count;
            func_args_count = 0;
        }

        if matches!(x, Instr::Call(_, _)) {
            call_stack_count += 1;
        } else if matches!(x, Instr::Ret(_, _)) && call_stack_count > call_stack_count_max {
            call_stack_count_max = call_stack_count;
            call_stack_count -= 1;
        }
    }
    (func_args_count_max, call_stack_count_max)
}

// Live long and prosper
fn main() {
    let mut contents = fs::read_to_string("test.spock").unwrap();
    contents = clean_contents(&contents, "test.spock");
    print!("{contents:?}");

    let (instructions, mut consts, mut arrays) = parse(&contents);

    let (func_args_count, call_stack_count) = get_vec_capacity(&instructions);

    println!("INSTR {instructions:?}");
    println!("CONSTS {consts:?}");
    print!("ARRAYS {arrays:?}");
    print!("FUNC_ARGS_COUNT {func_args_count:?}");

    let now = Instant::now();
    execute(
        &instructions,
        &mut consts,
        &mut Vec::with_capacity(func_args_count),
        &mut arrays,
        &mut Vec::with_capacity(call_stack_count * 2),
    );
    println!("EXEC TIME {:.2?}", now.elapsed());
}
