use crate::display::format_data;
use crate::parser::parse;
use crate::util::get_type;
use ariadne::*;
use builtin_funcs::FUNCS;
use concat_string::concat_string;
use fnv::FnvHashMap;
use inline_colorization::*;
use internment::Intern;
use likely_stable::{if_likely, likely, unlikely};
use parser::*;
use std::cmp::PartialEq;
use std::fmt::Arguments;
use std::fs;
use std::fs::File;
use std::hint::unreachable_unchecked;
use std::io::Write;
use std::time::Instant;

mod builtin_funcs;
mod display;
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
    CallFunc(u8, u16, u16),

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
    // array - index
    Remove(u16, u16),
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
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
) {
    let mut call_frames: Vec<Data> = Vec::with_capacity(consts.len() * call_stack.capacity());
    let mut i: usize = 0;

    let instr_op_error = |instr: Instr, op: &str, l: Data, r: Data| {
        let (_, start, end) = instr_src.iter().find(|(x, _, _)| x == &instr).unwrap();
        parser_error!(
            filename,
            src,
            *start,
            *end,
            "Invalid operation",
            format_args!(
                "Cannot perform operation {color_bright_blue}{style_bold}{} {color_red}{op}{color_bright_blue} {}{color_reset}{style_reset}",
                get_type(l),
                get_type(r)
            )
        );
    };

    let instr_type_error = |instr: Instr, expected: &str, r: Data| {
        let (_, start, end) = instr_src.iter().find(|(x, _, _)| x == &instr).unwrap();
        parser_error!(
            filename,
            src,
            *start,
            *end,
            "Invalid type",
            format_args!(
                "Expected {} but found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                expected,
                get_type(r),
            )
        );
    };

    let error = |instr: Instr, general_error: &str, msg: Arguments| {
        let (_, start, end) = instr_src.iter().find(|(x, _, _)| x == &instr).unwrap();
        parser_error!(filename, src, *start, *end, general_error, msg);
    };

    while i < instructions.len() {
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
                call_frames.extend_from_slice(consts.as_ref());
                continue;
            }
            Instr::Ret(x, y) => {
                let val = consts[x as usize];
                if let Some((ret_i, dest)) = call_stack.pop() {
                    let consts_part = call_frames.split_off(call_frames.len() - consts.len());
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
                    instr_op_error(Instr::Add(o1, o2, dest), "+", a, b);
                }
            },
            Instr::Mul(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent * child);
                } else {
                    instr_op_error(Instr::Mul(o1, o2, dest), "*",consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::Div(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent / child);
                } else {
                    instr_op_error(Instr::Div(o1, o2, dest), "/", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::Sub(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent - child);
                } else {
                   instr_op_error(Instr::Sub(o1, o2, dest), "-", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::Mod(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(parent % child);
                } else {
                    instr_op_error(Instr::Mod(o1, o2, dest), "%", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::Pow(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Number(is_float!(parent.powf(child), parent.pow(child as u32)));
                } else {
                    instr_op_error(Instr::Pow(o1, o2, dest), "^", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            // FIX FOR ARRAYS
            Instr::Eq(o1, o2, dest) => {
                consts[dest as usize] = Data::Bool(consts[o1 as usize] == consts[o2 as usize]);
            }
            // FIX FOR ARRAYS
            Instr::EqCmp(o1, o2, jump_size) => {
                if consts[o1 as usize] != consts[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::NotEq(o1, o2, dest) => {
                consts[dest as usize] = Data::Bool(consts[o1 as usize] != consts[o2 as usize]);
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
                    instr_op_error(Instr::Sup(o1, o2, dest), ">", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::SupCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent <= child {
                        i += jump_size as usize;
                        continue;
                    }
                } else {
                    instr_op_error(Instr::SupCmp(o1, o2, jump_size), ">",consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::SupEq(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent >= child);
                } else {
                    instr_op_error(Instr::SupEq(o1, o2, dest), ">=", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::SupEqCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent < child {
                        i += jump_size as usize;
                        continue;
                    }
                } else {
                    instr_op_error(Instr::SupEqCmp(o1, o2, jump_size), ">=", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::Inf(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent < child);
                } else {
                    instr_op_error(Instr::Inf(o1, o2, dest), "<", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::InfCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent >= child {
                    i += jump_size as usize;
                    continue;
                }
                } else {
                    instr_op_error(Instr::InfCmp(o1, o2, jump_size), "<", consts[o1 as usize], consts[o2 as usize]);

                }}
            }
            Instr::InfEq(o1, o2, dest) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent <= child);
                } else {
                    instr_op_error(Instr::InfEq(o1, o2, dest), "<=", consts[o1 as usize], consts[o2 as usize]);

                }}
            }
            Instr::InfEqCmp(o1, o2, jump_size) => {
                if_likely! {let (Data::Number(parent), Data::Number(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    if parent > child {
                        i += jump_size as usize;
                        continue;
                    }
                } else {
                    instr_op_error(Instr::InfEqCmp(o1, o2, jump_size), "<=", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::BoolAnd(o1, o2, dest) => {
                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent && child);
                } else {
                    instr_op_error(Instr::BoolAnd(o1, o2, dest), "&&", consts[o1 as usize], consts[o2 as usize]);
                }}
            }
            Instr::BoolOr(o1, o2, dest) => {
                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (consts[o1 as usize], consts[o2 as usize]) => {
                    consts[dest as usize] = Data::Bool(parent || child);
                } else {
                    instr_op_error(Instr::BoolOr(o1, o2, dest), "||", consts[o1 as usize], consts[o2 as usize]);
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
                    let (_, start, end) = instr_src.iter().find(|(x, _, _)| x == &Instr::Neg(tgt, dest)).unwrap();
                    parser_error!(
                        filename,
                        src,
                        *start,
                        *end,
                        "Invalid operation",
                        format_args!(
                            "Cannot negate {style_bold}{color_bright_blue}{}{color_reset}{style_reset}",
                            get_type(consts[tgt as usize])
                        )
                    );
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
                        error(
                            Instr::Num(tgt, dest),
                            "Invalid type",
                            format_args!(
                                "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a number",
                                str
                            ),
                        )
                    }));
                }
                Data::Number(num) => consts[dest as usize] = Data::Number(num),
                other => {
                    error(
                        Instr::Num(tgt, dest),
                        "Invalid type",
                        format_args!(
                            "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a number",
                            format_data(other, arrays)
                        ),
                    );
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
                        error(
                            Instr::Bool(tgt, dest),
                            "Invalid type",
                            format_args!(
                                "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a boolean",
                                str
                            ),
                        );
                        false
                    }));
                } else {
                    error(
                        Instr::Bool(tgt, dest),
                        "Invalid type",
                        format_args!(
                            "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a boolean",
                            format_data(base, arrays)
                        ),
                    );
                }}
            }
            Instr::Input(msg, dest) => {
                if_likely! {let Data::String(str) = consts[msg as usize] => {
                    println!("{str}");
                    std::io::stdout().flush().unwrap();
                    let mut line = String::new();
                    std::io::stdin().read_line(&mut line).unwrap();
                    consts[dest as usize] = Data::String(Intern::from(line.trim().to_string()));
                } else {
                    instr_type_error(Instr::Input(msg, dest), "string", consts[msg as usize]);
                }}
            }
            Instr::StoreFuncArg(id) => func_args.push(id),
            Instr::CallFunc(fctn_id, tgt, dest) => {
                FUNCS[fctn_id as usize](
                    tgt,
                    dest,
                    consts,
                    func_args,
                    arrays,
                    instr_src,
                    src,
                    filename,
                    Instr::CallFunc(fctn_id, tgt, dest),
                );
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
                            error(
                                Instr::ArrayMod(tgt, dest, idx),
                                "Invalid index",
                                format_args!(
                                    "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but array has {} elements",
                                    index,
                                    array.len()
                                ),
                            );
                        }
                    } else if let Data::String(str) = consts.get_mut(tgt as usize).unwrap() {
                        if_likely!{let Data::String(letter) = requested_mod => {
                            if likely(str.len() > index as usize) {
                                let mut temp = str.to_string();
                                temp.remove(index as usize);
                                temp.insert_str(index as usize, &letter);
                                *str = Intern::from(temp);
                            } else {
                                error(
                                    Instr::ArrayMod(tgt, dest, idx),
                                    "Invalid index",
                                    format_args!(
                                        "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but string has {} characters",
                                        index,
                                        str.len()
                                    ),
                                );
                            }
                        } else {
                            error(
                                Instr::ArrayMod(tgt, dest, idx),
                                "Invalid type",
                                format_args!(
                                    "Cannot insert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} in string",
                                    get_type(requested_mod)
                                ),
                            );
                        }}
                    } else {
                        error(
                            Instr::ArrayMod(tgt, dest, idx),
                            "Invalid type",
                            format_args!(
                                "Cannot index type {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                get_type(consts[tgt as usize])
                            ),
                        );
                    }
                }}
            }
            // takes tgt from  consts, index is index, dest is consts index destination
            Instr::GetIndex(tgt, index, dest) => {
                if_likely! {let Data::Number(idx) = consts[index as usize] => {
                    let idx = idx as usize;
                    match consts[tgt as usize] {
                        Data::Array(x) => {
                            let array = &arrays[&x];
                            if likely(array.len() > idx) {
                                consts[dest as usize] = array[idx];
                            } else {
                               error(
                                    Instr::GetIndex(tgt, index, dest),
                                    "Invalid index",
                                    format_args!(
                                        "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but array has {} elements",
                                        idx,
                                        array.len()
                                    ),
                                );
                            }
                        }
                        Data::String(str) => {
                            if likely(str.len() > idx) {
                                consts[dest as usize] = Data::String(Intern::from(
                                    str.get(idx..=idx).unwrap().to_string(),
                                ));
                            } else {
                                error(
                                    Instr::GetIndex(tgt, index, dest),
                                    "Invalid index",
                                    format_args!(
                                        "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but string has {} characters",
                                        idx,
                                        str.len()
                                    ),
                                );
                            }
                        }
                        other => {
                            error(
                                Instr::GetIndex(tgt, index, dest),
                                "Invalid type",
                                format_args!(
                                    "Cannot index {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    get_type(other)
                                ),
                            );
                        }
                    }
                } else {
                    error(
                        Instr::GetIndex(tgt, index, dest),
                        "Invalid index",
                        format_args!(
                            "{color_bright_blue}{style_bold}{}{color_reset}{style_reset} is not a valid index",
                             format_data(consts[index as usize], arrays)
                        ),
                    );
                }}
            }
            Instr::Range(min, max, dest) => {
                if_likely! {let Data::Number(x) = consts[min as usize] => {
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
                                // error_b!(format_args!("Cannot create file {color_red}{str}{color_reset}"));
                                todo!()
                            });
                        } else if unlikely(!fs::exists(str.as_str()).unwrap_or_else(|_| {
                                todo!()
                            // error_b!(format_args!("Cannot check existence of file {color_red}{str}{color_reset}"));
                        })) {
                            // error_b!(format_args!("File {color_red}{str}{color_reset} does not exist"));
                        }
                        consts[dest as usize] = Data::File(str);
                    } else {
                        // error_b!(format_args!("Invalid create option: {color_red}{}{color_reset}", format_data(consts[create as usize], arrays)));
                    }}
                } else {
                    // error_b!(format_args!("Invalid file path: {color_red}{}{color_reset}", format_data(consts[path as usize], arrays)));
                }}
            }
            Instr::IoDelete(path) => {
                if_likely! {let Data::String(str) = consts[path as usize] => {
                    fs::remove_file(str.as_str()).unwrap_or_else(|_| {
                        // error_b!(format_args!("Cannot remove file {color_red}{str}{color_reset}"));
                        todo!()
                    });
                } else {
                    todo!()
                    // error_b!(format_args!("Invalid file path: {color_red}{}{color_reset}", format_data(consts[path as usize], arrays)));
                }}
            }
            Instr::Floor(tgt, dest) => {
                if_likely! {let Data::Number(x) = consts[tgt as usize] => {
                    consts[dest as usize] = Data::Number(is_float!(x.floor(),x));
                } else {
                    error(Instr::Floor(tgt, dest), "Invalid type", format_args!(
                        "Cannot floor type {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        get_type(consts[tgt as usize])
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
                    error(Instr::Push(array, element), "Invalid type", format_args!("Cannot push element to {color_bright_blue}{style_bold}{}{color_reset}{style_reset}", format_data(consts[array as usize], arrays)));
                }}
            }
            Instr::Len(tgt, dest) => {
                match consts[tgt as usize] {
                    Data::Array(arr) => {
                        consts[dest as usize] = Data::Number(arrays[&arr].len() as Num)
                    }
                    Data::String(str) => {
                        consts[dest as usize] = Data::Number(str.chars().count() as Num)
                    }
                    x => {
                        error(
                            Instr::Len(tgt, dest),
                            "Invalid type",
                            format_args!(
                                "Cannot get length of type {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                get_type(x)
                            ),
                        );
                    }
                };
            }
            Instr::Sqrt(tgt, dest) => {
                if_likely! {let Data::Number(num) = consts[tgt as usize] => {
                    consts[dest as usize] = Data::Number(is_float!(num.sqrt(), num.isqrt()))
                } else {
                    error(Instr::Sqrt(tgt, dest), "Invalid type", format_args!("Cannot compute square root of {color_bright_blue}{style_bold}{}{color_reset}{style_reset}", format_data(consts[tgt as usize], arrays)));
                }}
            }
            Instr::Split(tgt, sep, dest) => match consts[tgt as usize] {
                Data::String(str) => {
                    if_likely! { let Data::String(separator) = consts[sep as usize] => {
                        let id = arrays.len() as u16;
                        arrays.insert(
                            id,
                            str.split(separator.as_str())
                                .map(|x| Data::String(Intern::from_ref(x)))
                                .collect(),
                        );
                        consts[dest as usize] = Data::Array(id);
                    } else {
                        error(Instr::Split(tgt, sep, dest), "Invalid separator", format_args!("Invalid string separator: {color_bright_blue}{style_bold}{}{color_reset}{style_reset}", format_data(consts[sep as usize], arrays)));
                    }}
                }
                Data::Array(array_id) => {
                    let array = arrays[&array_id].to_vec();
                    let split = array.split(|x| x == &consts[sep as usize]);
                    let base_id = arrays.len() as u16;
                    arrays.extend(
                        split
                            .enumerate()
                            .map(|(i, x)| (base_id + i as u16, x.to_vec())),
                    );
                    let id = arrays.len() as u16;
                    arrays.insert(id, (base_id..id).map(|x| Data::Array(x)).collect());
                    consts[dest as usize] = Data::Array(id);
                }
                invalid => {
                    error(
                        Instr::Split(tgt, sep, dest),
                        "Invalid type",
                        format_args!(
                            "Cannot split type {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            get_type(invalid)
                        ),
                    );
                }
            },
            Instr::Remove(array, idx) => {
                if_likely! {let Data::Number(idx) = consts[idx as usize] => {
                        arrays.get_mut(&array).unwrap().remove(idx as usize);
                } else {
                    error(
                        Instr::Remove(array, idx),
                        "Invalid index",
                        format_args!(
                            "{color_bright_blue}{style_bold}{}{color_reset}{style_reset} is not a valid index",
                             format_data(consts[idx as usize], arrays)
                        ),
                    );
                }
                }
            }
            Instr::Break(_) | Instr::Continue(_) => unsafe { unreachable_unchecked() },
        }
        i += 1;
    }
}

fn get_vec_capacity(instructions: &[Instr]) -> (usize, usize) {
    let (mut func_args, mut max_func_args) = (0, 0);
    let (mut call_depth, mut max_call_depth) = (0usize, 0usize);

    for instr in instructions {
        match instr {
            Instr::StoreFuncArg(_) => func_args += 1,
            Instr::CallFunc(_, _, _) => {
                max_func_args = max_func_args.max(func_args);
                func_args = 0;
            }
            Instr::Call(_, _) => {
                call_depth += 1;
                max_call_depth = max_call_depth.max(call_depth);
            }
            Instr::Ret(_, _) => {
                call_depth = call_depth.saturating_sub(1);
            }
            _ => {}
        }
    }
    (max_func_args, max_call_depth)
}

// Live long and prosper
fn main() {
    let args: Vec<String> = std::env::args().collect();

    #[cfg(debug_assertions)]
    let filename = "test.spock";

    #[cfg(not(debug_assertions))]
    let filename = args.get(1).unwrap_or_else(|| {
        println!("{}", util::SPOCK_LOGO);
        std::process::exit(0);
    });

    let contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        error_b!(format_args!(
            "Unable to read contents of file {color_red}{filename}{color_reset}"
        ));
    });

    // #[cfg(debug_assertions)]
    let now = Instant::now();

    let (instructions, mut consts, mut arrays, instr_src) = parse(&contents, filename);

    let (func_args_count, call_stack_count) = get_vec_capacity(&instructions);

    // #[cfg(debug_assertions)]
    println!("PARSING TIME {:.2?}", now.elapsed());

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
        &mut Vec::with_capacity(call_stack_count),
        &instr_src,
        &contents,
        &filename,
    );
    let end = now.elapsed();
    println!("EXEC TIME {:.2?}", end);
}
