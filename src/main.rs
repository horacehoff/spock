use crate::data::Data;
use crate::display::{format_data, parser_error};
use crate::instr::Instr;
use crate::instr::LibFunc;
use crate::parser::parse;
use crate::util::error;
use crate::util::likely;
use crate::util::unlikely;
use concat_string::concat_string;
use inline_colorization::*;
use parser::*;
use slab::Slab;
use std::fs::File;
use std::hint::black_box;
use std::io::Write;
use std::time::Instant;
use std::{fs, i32};

#[path = "./data.rs"]
mod data;
#[path = "./util/display.rs"]
mod display;
#[path = "./parser/functions.rs"]
mod functions;
#[path = "./instr.rs"]
mod instr;
#[path = "./parser/method_calls.rs"]
mod method_calls;
#[path = "./parser/optimizations.rs"]
mod optimizations;
#[path = "./parser/parser.rs"]
mod parser;
#[path = "./type_system.rs"]
mod type_system;
#[path = "./util/util.rs"]
mod util;

pub type ArrayStorage = Slab<Vec<Data>>;

#[repr(C)]
struct CallFrame {
    return_addr: u32,
    return_reg: u16,
}

pub fn execute(
    instructions: &[Instr],
    registers: &mut [Data],
    arrays: &mut ArrayStorage,
    instr_src: &[(Instr, usize, usize)],
    src: (&str, &str),
    fn_registers: &[Vec<u16>],
) {
    macro_rules! fatal_error {
        ($instr: expr,$err:expr,$msg:expr) => {
            let (_, start, end) = instr_src.iter().find(|(x, _, _)| x == &$instr).unwrap();
            parser_error(src, *start, *end, $err, $msg, "");
        };
    }
    let mut i: usize = 0;
    let mut args: Vec<u16> = Vec::with_capacity(
        instructions
            .iter()
            .filter(|x| matches!(x, Instr::StoreFuncArg(_)))
            .count(),
    );
    let call_depth = instructions
        .iter()
        .filter(|x| matches!(x, Instr::CallFunc(_, _) | Instr::SaveFrame(_, _, _)))
        .count();
    let mut call_frames: Vec<CallFrame> = Vec::with_capacity(call_depth);
    let mut recursion_stack: Vec<Data> = Vec::with_capacity(call_depth * registers.len());

    let stdout = std::io::stdout();
    let mut handle = stdout.lock();

    let len = instructions.len();
    while i < len {
        match instructions[i] {
            Instr::Jmp(size) => {
                i += size as usize;
                continue;
            }
            Instr::JmpBack(size) => {
                i -= size as usize;
                continue;
            }
            Instr::CallFunc(new_loc, return_id) => {
                call_frames.push(CallFrame {
                    return_addr: i as u32,
                    return_reg: return_id,
                });
                i = new_loc as usize;
                continue;
            }
            Instr::CallFuncRecursive(new_loc, _) => {
                i = new_loc as usize;
                continue;
            }
            Instr::VoidReturn => {
                i = call_frames.pop().unwrap().return_addr as usize;
                continue;
            }
            Instr::SaveFrame(relative_func_loc, return_register, fn_id) => {
                call_frames.push(CallFrame {
                    return_addr: (i + relative_func_loc as usize) as u32,
                    return_reg: return_register,
                });
                // Create a "snapshot" of the registers, so as to be able to reset them when the function returns.
                recursion_stack.extend(
                    fn_registers[fn_id as usize]
                        .iter()
                        .map(|&r| registers[r as usize]),
                );
            }
            Instr::Return(tgt) => {
                let call_frame = call_frames.pop().unwrap();
                i = call_frame.return_addr as usize;
                registers[call_frame.return_reg as usize] = registers[tgt as usize];
            }
            Instr::RecursiveReturn(tgt, fn_id) => {
                let call_frame = call_frames.pop().unwrap();
                let temp = registers[tgt as usize];
                let regs = &fn_registers[fn_id as usize];
                let base = recursion_stack.len() - regs.len();
                for (reg, &saved) in regs.iter().zip(&recursion_stack[base..]) {
                    registers[*reg as usize] = saved;
                }
                unsafe {
                    recursion_stack.set_len(base);
                }
                i = call_frame.return_addr as usize;
                registers[call_frame.return_reg as usize] = temp;
            }
            Instr::ConditionalJmp(cond_id, size) => {
                if registers[cond_id as usize] == Data::FALSE {
                    i += size as usize;
                    continue;
                }
            }
            Instr::Mov(tgt, dest) => registers[dest as usize] = registers[tgt as usize],
            Instr::AddFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() + registers[o2 as usize].as_float()).into();
            }
            Instr::AddInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() + registers[o2 as usize].as_int()).into();
            }
            Instr::AddStr(o1, o2, dest) => {
                registers[dest as usize] = concat_string!(
                    registers[o1 as usize].as_str(),
                    registers[o2 as usize].as_str()
                )
                .into();
            }
            Instr::AddArray(o1, o2, dest) => {
                let arr_a = &arrays[registers[o1 as usize].as_array() as usize];
                let arr_b = &arrays[registers[o2 as usize].as_array() as usize];

                let mut combined = Vec::with_capacity(arr_a.len() + arr_b.len());
                combined.extend_from_slice(arr_a);
                combined.extend_from_slice(arr_b);
                let id = arrays.insert(combined);
                registers[dest as usize] = Data::array(id as u32);
            }
            Instr::MulFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() * registers[o2 as usize].as_float()).into();
            }
            Instr::MulInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() * registers[o2 as usize].as_int()).into();
            }
            Instr::DivFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() / registers[o2 as usize].as_float()).into();
            }
            Instr::DivInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() / registers[o2 as usize].as_int()).into();
            }
            Instr::SubFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() - registers[o2 as usize].as_float()).into();
            }
            Instr::SubInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() - registers[o2 as usize].as_int()).into();
            }
            Instr::ModFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() % registers[o2 as usize].as_float()).into();
            }
            Instr::ModInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() % registers[o2 as usize].as_int()).into();
            }
            Instr::PowFloat(o1, o2, dest) => {
                registers[dest as usize] = (registers[o1 as usize]
                    .as_float()
                    .powf(registers[o2 as usize].as_float()))
                .into();
            }
            Instr::PowInt(o1, o2, dest) => {
                registers[dest as usize] = (registers[o1 as usize]
                    .as_int()
                    .pow(registers[o2 as usize].as_int() as u32))
                .into();
            }
            Instr::Eq(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize] == registers[o2 as usize]).into();
            }
            Instr::ArrayEq(o1, o2, dest) => {
                registers[dest as usize] = (arrays[registers[o1 as usize].as_array() as usize]
                    == arrays[registers[o2 as usize].as_array() as usize])
                    .into();
            }
            Instr::EqJmp(o1, o2, jump_size) => {
                if registers[o1 as usize] != registers[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayEqJmp(o1, o2, jump_size) => {
                if arrays[registers[o1 as usize].as_array() as usize]
                    != arrays[registers[o2 as usize].as_array() as usize]
                {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::NotEq(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize] != registers[o2 as usize]).into();
            }
            Instr::ArrayNotEq(o1, o2, dest) => {
                registers[dest as usize] = (arrays[registers[o1 as usize].as_array() as usize]
                    != arrays[registers[o2 as usize].as_array() as usize])
                    .into();
            }
            Instr::NotEqJmp(o1, o2, jump_size) => {
                if registers[o1 as usize] == registers[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayNotEqJmp(o1, o2, jump_size) => {
                if arrays[registers[o1 as usize].as_array() as usize]
                    == arrays[registers[o2 as usize].as_array() as usize]
                {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() > registers[o2 as usize].as_float()).into();
            }
            Instr::SupInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() > registers[o2 as usize].as_int()).into();
            }
            Instr::SupFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() <= registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupIntJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_int() <= registers[o2 as usize].as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupEqFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() >= registers[o2 as usize].as_float()).into();
            }
            Instr::SupEqInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() >= registers[o2 as usize].as_int()).into();
            }
            Instr::SupEqFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() < registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupEqIntJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_int() < registers[o2 as usize].as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() < registers[o2 as usize].as_float()).into();
            }
            Instr::InfInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() < registers[o2 as usize].as_int()).into();
            }
            Instr::InfFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() >= registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfIntJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_int() >= registers[o2 as usize].as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfEqFloat(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_float() <= registers[o2 as usize].as_float()).into();
            }
            Instr::InfEqInt(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_int() <= registers[o2 as usize].as_int()).into();
            }
            Instr::InfEqFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() > registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfEqIntJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_int() > registers[o2 as usize].as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::BoolAnd(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_bool() && registers[o2 as usize].as_bool()).into();
            }
            Instr::BoolOr(o1, o2, dest) => {
                registers[dest as usize] =
                    (registers[o1 as usize].as_bool() || registers[o2 as usize].as_bool()).into();
            }
            Instr::NegFloat(tgt, dest) => {
                registers[dest as usize] = (-registers[tgt as usize].as_float()).into();
            }
            Instr::NegInt(tgt, dest) => {
                registers[dest as usize] = (-registers[tgt as usize].as_int()).into();
            }
            Instr::Print(target) => {
                let tgt = registers[target as usize];
                if tgt.is_str() {
                    writeln!(handle, "{}", tgt.as_str()).unwrap();
                } else if tgt.is_int() {
                    writeln!(handle, "{}", tgt.as_int()).unwrap();
                } else if tgt.is_float() {
                    writeln!(handle, "{}", tgt.as_float()).unwrap();
                } else if tgt.is_bool() {
                    writeln!(handle, "{}", tgt.as_bool()).unwrap();
                } else if tgt.is_file() {
                    writeln!(handle, "{}", tgt.as_file()).unwrap();
                } else if tgt.is_array() {
                    writeln!(
                        handle,
                        "{}",
                        concat_string!(
                            "[",
                            arrays[tgt.as_array() as usize]
                                .iter()
                                .map(|x| format_data(*x, Some(arrays), true))
                                .collect::<Vec<_>>()
                                .join(","),
                            "]"
                        )
                    )
                    .unwrap();
                }
            }
            Instr::Float(tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_int() {
                    registers[dest as usize] = (reg.as_int() as f64).into();
                } else if reg.is_str() {
                    let str = reg.as_str();
                    registers[dest as usize] = (str.parse::<f64>().unwrap_or_else(|_| {
                    fatal_error!(
                        Instr::Float(tgt, dest),
                        "Invalid type",
                        &format!(
                            "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a Float",
                            str
                        )
                    );
                    })).into();
                } else {
                    unreachable!()
                }
            }
            Instr::Int(tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_float() {
                    registers[dest as usize] = (reg.as_float().round() as i32).into();
                } else if reg.is_str() {
                    let str = reg.as_str();
                    registers[dest as usize] = (str.parse::<i32>().unwrap_or_else(|_| {
                    fatal_error!(
                        Instr::Int(tgt, dest),
                        "Invalid type",
                        &format!(
                            "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into an Integer",
                            str
                        )
                    );
                    })).into();
                } else {
                    unreachable!()
                }
            }
            Instr::Str(tgt, dest) => {
                registers[dest as usize] =
                    format_data(registers[tgt as usize], Some(arrays), false).into();
            }
            Instr::Bool(tgt, dest) => {
                let str = registers[tgt as usize].as_str();
                registers[dest as usize] = (str.parse::<bool>().unwrap_or_else(|_| {
                   fatal_error!(
                        Instr::Bool(tgt, dest),
                        "Invalid type",
                        &format!(
                            "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a Boolean",
                            str
                        )
                    );
                })).into();
            }
            Instr::Input(msg, dest) => {
                let str = registers[msg as usize].as_str();
                println!("{str}");
                std::io::stdout().flush().unwrap();
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap();
                registers[dest as usize] = (line.trim().to_string()).into();
            }
            Instr::StoreFuncArg(id) => args.push(id),
            // takes tgt from registers, moves it to dest-th array at idx-th index
            Instr::ArrayMov(tgt, dest, idx) => {
                arrays.get_mut(dest as usize).unwrap()[idx as usize] = registers[tgt as usize];
            }
            // takes tgt from registers, idx from registers,
            Instr::ArrayMod(tgt, dest, idx) => {
                let index = registers[idx as usize].as_int();
                let requested_mod = registers[dest as usize];
                let array_id = registers[tgt as usize].as_array();
                let array = arrays.get_mut(array_id as usize).unwrap();
                if likely(array.len() > index as usize) {
                    array[index as usize] = requested_mod;
                } else {
                    fatal_error!(
                        Instr::ArrayMod(tgt, dest, idx),
                        "Invalid index",
                        &format!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but array has {} elements",
                            index,
                            array.len()
                        )
                    );
                }
            }
            Instr::StrMod(tgt, dest, idx) => {
                let index = registers[idx as usize].as_int();
                let str = registers[tgt as usize].as_str();
                let letter = registers[dest as usize].as_str();
                if likely(str.len() > index as usize) {
                    let mut temp = str.to_string();
                    temp.remove(index as usize);
                    temp.insert_str(index as usize, &letter);
                    registers[tgt as usize] = temp.into();
                } else {
                    fatal_error!(
                        Instr::StrMod(tgt, dest, idx),
                        "Invalid index",
                        &format!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but string has {} characters",
                            index,
                            str.len()
                        )
                    );
                }
            }
            // takes tgt from  registers, index is index, dest is registers index destination
            Instr::ArrayGet(tgt, index, dest) => {
                let idx = registers[index as usize].as_int();
                let x = registers[tgt as usize].as_array();
                let array = &arrays[x as usize];
                if likely(array.len() > idx as usize) {
                    registers[dest as usize] = array[idx as usize];
                } else {
                    fatal_error!(
                        Instr::ArrayGet(tgt, index, dest),
                        "Invalid index",
                        &format!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but Array has {} elements",
                            idx,
                            array.len()
                        )
                    );
                }
            }
            Instr::ArrayStrGet(tgt, index, dest) => {
                let idx = registers[index as usize].as_int();
                let str = registers[tgt as usize].as_str();
                if likely(str.len() > idx as usize) {
                    registers[dest as usize] = str.get(idx as usize..=idx as usize).unwrap().into();
                } else {
                    fatal_error!(
                        Instr::ArrayGet(tgt, index, dest),
                        "Invalid index",
                        &format!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but String has {} characters",
                            idx,
                            str.len()
                        )
                    );
                }
            }
            Instr::Range(min, max, dest) => {
                let x = registers[min as usize].as_int();
                let y = registers[max as usize].as_int();
                let id = arrays.insert((x..y).map(|x| x.into()).collect());
                registers[dest as usize] = Data::array(id as u32);
            }
            Instr::IoOpen(path, dest, create) => {
                let str = registers[path as usize].as_str();
                let create = registers[create as usize].as_bool();
                if likely(create) {
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
                registers[dest as usize] = Data::file(&str);
            }
            Instr::IoDelete(path) => {
                let str = registers[path as usize].as_str();
                fs::remove_file(str.as_str()).unwrap_or_else(|_| {
                    // error_b!(format_args!("Cannot remove file {color_red}{str}{color_reset}"));
                    todo!()
                });
            }
            Instr::Floor(tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().floor().into()
            }

            Instr::TheAnswer(dest) => {
                println!(
                    "The answer to the Ultimate Question of Life, the Universe, and Everything is 42."
                );
                registers[dest as usize] = 42.into();
            }
            Instr::Push(array, element) => {
                arrays
                    .get_mut(registers[array as usize].as_array() as usize)
                    .unwrap()
                    .push(registers[element as usize]);
            }
            Instr::Len(tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_array() {
                    registers[dest as usize] = (arrays[reg.as_array() as usize].len() as i32).into()
                } else if reg.is_str() {
                    registers[dest as usize] = (reg.as_str().chars().count() as i32).into()
                }
            }
            Instr::SqrtFloat(tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().sqrt().into()
            }
            Instr::Split(tgt, sep, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let separator = registers[sep as usize].as_str();
                    let id =
                        arrays.insert(str.split(separator.as_str()).map(|x| x.into()).collect());
                    registers[dest as usize] = Data::array(id as u32);
                } else if reg.is_array() {
                    let base_id = arrays.len() as u16;
                    // get the array and split it
                    arrays[reg.as_array() as usize]
                        .to_vec()
                        .split(|x| x == &registers[sep as usize])
                        .for_each(|x| {
                            arrays.insert(x.to_vec());
                        });
                    let id = arrays.insert(
                        (base_id..arrays.len() as u16)
                            .map(|x| Data::array(x as u32))
                            .collect::<Vec<Data>>(),
                    );
                    registers[dest as usize] = Data::array(id as u32);
                }
            }
            Instr::Remove(array, idx) => {
                arrays
                    .get_mut(array as usize)
                    .unwrap()
                    .remove(registers[idx as usize].as_int() as usize);
            }
            // uppercase
            Instr::CallLibFunc(LibFunc::Uppercase, tgt, dest) => {
                let str = registers[tgt as usize].as_str();
                registers[dest as usize] = str.to_uppercase().into();
            }
            // lowercase
            Instr::CallLibFunc(LibFunc::Lowercase, tgt, dest) => {
                let str = registers[tgt as usize].as_str();
                registers[dest as usize] = str.to_lowercase().into();
            }
            // contains
            Instr::CallLibFunc(LibFunc::Contains, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let arg = registers[args.swap_remove(0) as usize].as_str();
                    registers[dest as usize] = str.contains(arg.as_str()).into();
                } else if reg.is_array() {
                    let arg = registers[args.swap_remove(0) as usize];
                    registers[dest as usize] =
                        arrays[reg.as_array() as usize].contains(&arg).into();
                }
            }
            // trim
            Instr::CallLibFunc(LibFunc::Trim, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_str().trim().into();
            }
            // trim_sequence
            Instr::CallLibFunc(LibFunc::TrimSequence, tgt, dest) => {
                let arg = registers[args.swap_remove(0) as usize].as_str();
                let chars: Vec<char> = arg.chars().collect();
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .trim_matches(&chars[..])
                    .into();
            }
            // index
            Instr::CallLibFunc(LibFunc::Index, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let arg = registers[args.swap_remove(0) as usize].as_str();
                    registers[dest as usize] = (str.find(arg.as_str()).unwrap_or_else(|| {
                            fatal_error!(instructions[i],"Item not found",&format!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}\"{}\"{color_reset}", arg, str));
                        }) as i32).into();
                } else if reg.is_array() {
                    let x = reg.as_array();
                    let arg = registers[args.swap_remove(0) as usize];
                    registers[dest as usize] = (arrays[x as usize].iter().position(|x| x == &arg).unwrap_or_else(|| {
                        fatal_error!(instructions[i], "Item not found",&format!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{}{color_reset}", arg, format_data(Data::array(x), Some(arrays),true)));
                    }) as i32).into();
                }
            }
            // is_num
            Instr::CallLibFunc(LibFunc::IsNum, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .parse::<f64>()
                    .is_ok()
                    .into()
            }
            // trim_left
            Instr::CallLibFunc(LibFunc::TrimLeft, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_str().trim_start().into();
            }
            // trim_right
            Instr::CallLibFunc(LibFunc::TrimRight, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_str().trim_end().into();
            }
            // trim_sequence_left
            Instr::CallLibFunc(LibFunc::TrimSequenceLeft, tgt, dest) => {
                let chars: Vec<char> = registers[args.swap_remove(0) as usize]
                    .as_str()
                    .chars()
                    .collect();
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .trim_start_matches(&chars[..])
                    .into();
            }
            // trim_sequence_right
            Instr::CallLibFunc(LibFunc::TrimSequenceRight, tgt, dest) => {
                let chars: Vec<char> = registers[args.swap_remove(0) as usize]
                    .as_str()
                    .chars()
                    .collect();
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .trim_end_matches(&chars[..])
                    .to_string()
                    .into();
            }
            // rindex
            Instr::CallLibFunc(LibFunc::RIndex, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let arg = registers[args.swap_remove(0) as usize].as_str();
                    registers[dest as usize] = (reg.as_str().rfind(arg.as_str()).unwrap_or_else(|| {
                        fatal_error!(instructions[i], "Item not found",&format!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}\"{}\"{color_reset}", arg, str));
                    }) as i32).into();
                } else if reg.is_array() {
                    let x = reg.as_array();
                    let arg = registers[args.swap_remove(0) as usize];
                    registers[dest as usize] = (arrays[reg.as_array() as usize].iter().rposition(|x| x == &arg).unwrap_or_else(|| {
                    fatal_error!(instructions[i],"Item not found",&format!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{}{color_reset}", arg, format_data(Data::array(x), Some(arrays),true)));
                    }) as i32).into();
                }
            }
            // repeat
            Instr::CallLibFunc(LibFunc::Repeat, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let arg = registers[args.swap_remove(0) as usize].as_int();
                    registers[dest as usize] = str.repeat(arg as usize).into();
                } else if reg.is_array() {
                    let x = reg.as_array();
                    let arg = registers[args.swap_remove(0) as usize].as_int();
                    registers[dest as usize] =
                        Data::array(arrays.insert(arrays[x as usize].repeat(arg as usize)) as u32);
                }
            }
            // round
            Instr::CallLibFunc(LibFunc::Round, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().round().into();
            }
            // abs
            Instr::CallLibFunc(LibFunc::Abs, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().abs().into();
            }
            // read
            Instr::CallLibFunc(LibFunc::ReadFile, tgt, dest) => {
                let path = registers[tgt as usize].as_file();
                registers[dest as usize] = std::fs::read_to_string(path.as_str())
                    .unwrap_or_else(|_| {
                        fatal_error!(
                            instructions[i],
                            "File does not exist or cannot be read",
                            &format!("Cannot read file {color_red}{path}{color_reset}")
                        );
                    })
                    .into();
            }
            // write
            Instr::CallLibFunc(LibFunc::WriteFile, tgt, dest) => {
                let path = registers[tgt as usize].as_file();
                let contents = registers[args.swap_remove(0) as usize].as_str();
                let truncate = registers[args.swap_remove(0) as usize].as_bool();
                fs::OpenOptions::new()
                                .write(true)
                                .truncate(truncate)
                                .open(path.as_str()).unwrap_or_else(|_| {
                                    fatal_error!(instructions[i],"File does not exist or cannot be opened",&format!("Cannot open file {color_red}{path}{color_reset}"));
                                }).write_all(contents.as_bytes()).unwrap_or_else(|_| {
                                    fatal_error!(instructions[i],"File does not exist or cannot be written to",&format!("Cannot write {color_red}{path}{color_reset} to file {color_blue}{path}{color_reset}"));
                            });
            }
            // reverse
            Instr::CallLibFunc(LibFunc::Reverse, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    registers[dest as usize] =
                        reg.as_str().chars().rev().collect::<String>().into();
                } else if reg.is_array() {
                    let id = reg.as_array();
                    arrays.get_mut(id as usize).unwrap().reverse();
                    registers[dest as usize] = Data::array(id);
                }
            }
        }
        i += 1;
    }
}

/// Live long and prosper
fn main() {
    #[cfg(debug_assertions)]
    let filename = "test.spock";

    #[cfg(not(debug_assertions))]
    let filename = &std::env::args().nth(1).unwrap_or_else(|| {
        println!("{}", util::SPOCK_LOGO);
        std::process::exit(0);
    });

    let contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        error(format!(
            "Unable to read contents of file {color_red}{filename}{color_reset}"
        ))
    });

    let now = Instant::now();

    let (instructions, mut registers, mut arrays, instr_src, fn_registers) =
        parse(&contents, filename);

    println!("PARSING TIME {:.2?}", now.elapsed());
    if std::env::args().len() > 2 && std::env::args().nth(2).unwrap() == "--bench" {
        benchmark(
            &instructions,
            &mut registers,
            &mut arrays,
            &instr_src,
            (filename, &contents),
            &fn_registers,
            10,
            150,
            std::env::args().len() > 3 && std::env::args().nth(3).unwrap() == "--verbose",
        );
    } else {
        let now = Instant::now();
        execute(
            &instructions,
            &mut registers,
            &mut arrays,
            &instr_src,
            (filename, &contents),
            &fn_registers,
        );
        println!(
            "EXECUTION TIME: {:.3}ms",
            now.elapsed().as_nanos() / 1000000
        );
    }
}

#[cold]
#[inline(never)]
pub fn benchmark(
    instructions: &[Instr],
    registers: &mut [Data],
    arrays: &mut ArrayStorage,
    instr_src: &[(Instr, usize, usize)],
    src: (&str, &str),
    fn_registers: &[Vec<u16>],
    warmup_runs: usize,
    samples_count: usize,
    verbose: bool,
) {
    let mut times_ns: Vec<u128> = Vec::with_capacity(samples_count);

    for _ in 0..warmup_runs {
        black_box(execute(
            black_box(instructions),
            black_box(&mut registers.to_vec()),
            black_box(&mut arrays.to_owned()),
            black_box(instr_src),
            black_box(src),
            black_box(fn_registers),
        ));
    }
    for _ in 0..samples_count {
        let registers = &mut registers.to_vec();
        let arrays = &mut arrays.to_owned();
        let now = Instant::now();
        black_box(execute(
            black_box(instructions),
            black_box(registers),
            black_box(arrays),
            black_box(instr_src),
            black_box(src),
            black_box(fn_registers),
        ));
        times_ns.push(now.elapsed().as_nanos());
    }
    if verbose {
        let mut sorted = times_ns.clone();
        sorted.sort_unstable();

        let min_ns = sorted[0];
        let max_ns = *sorted.last().unwrap();
        let median_ns = if samples_count.is_multiple_of(2) {
            (sorted[samples_count / 2 - 1] + sorted[samples_count / 2]) / 2
        } else {
            sorted[samples_count / 2]
        };

        let mean_ns = times_ns.iter().sum::<u128>() as f64 / samples_count as f64;

        let variance = times_ns
            .iter()
            .map(|&x| (x as f64 - mean_ns).powi(2))
            .sum::<f64>()
            / (samples_count as f64);
        let std_deviation = variance.sqrt();

        let std = std_deviation / (samples_count as f64).sqrt();
        let confidence_interval_margin = 1.96 * std;

        let lower_confidence_interval = mean_ns - confidence_interval_margin;
        let upper_confidence_interval = mean_ns + confidence_interval_margin;

        println!(
            "\nBENCHMARK RESULTS\n---{color_blue}Program{color_reset}---\n{color_blue}{}{color_reset}\n-------------",
            src.1
        );
        println!("Samples          : {}", samples_count);
        println!("Min              : {:.3} ms", min_ns as f64 / 1000000.0);
        println!("Max              : {:.3} ms", max_ns as f64 / 1000000.0);
        println!(
            "Median           : {:.3} ms",
            median_ns as f64 / 1_000_000.0
        );
        println!("Mean             : {:.3} ms", mean_ns / 1000000.0);
        println!("Stand. deviation : {:.3} ms", std_deviation / 1000000.0);
        println!(
            "95% Confidence interval for mean  : [{:.3} .. {:.3}] ms",
            lower_confidence_interval / 1000000.0,
            upper_confidence_interval / 1000000.0
        );
        println!(
            "{bg_red}{color_white}Average exec time: {:.3} ms (±{:.3} ms with 95% confidence){color_reset}{bg_reset}",
            mean_ns / 1000000.0,
            confidence_interval_margin / 1000000.0
        );
    }
}
