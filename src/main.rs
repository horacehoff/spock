use crate::data::Data;
use crate::data::FALSE;
use crate::data::NULL;
use crate::display::format_data;
use crate::errors::error;
use crate::errors::runtime_error;
use crate::instr::Instr;
use crate::instr::LibFunc;
use crate::parser::parse;
use crate::parser_data::DynamicLibFn;
use crate::util::likely;
use concat_string::concat_string;
use inline_colorization::*;
use mimalloc::MiMalloc;
use parser::*;
use std::any::Any;
use std::fs;
use std::hint::black_box;
use std::io::Write;
use std::time::Instant;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[path = "./data.rs"]
mod data;
#[path = "./util/display.rs"]
mod display;
#[path = "./util/errors.rs"]
mod errors;
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
#[path = "./parser/parser_data.rs"]
mod parser_data;
#[path = "./tests.rs"]
#[cfg(test)]
mod tests;
#[path = "./type_system.rs"]
mod type_system;
#[path = "./util/util.rs"]
mod util;

pub type ArrayStorage = Vec<Vec<Data>>;

fn alloc_array(array_pool: &mut ArrayStorage, free_arrays: &mut Vec<u16>) -> u32 {
    if let Some(id) = free_arrays.pop() {
        id as u32
    } else {
        array_pool.push(Vec::with_capacity(1));
        (array_pool.len() - 1) as u32
    }
}

struct CallFrame {
    return_addr: u32,
    return_reg: u16,
    callsite_id: u16,
}

pub fn execute(
    instructions: &[Instr],
    registers: &mut [Data],
    array_pool: &mut ArrayStorage,
    instr_src: &[(Instr, usize, usize)],
    src: (&str, &str),
    fn_registers: &[Vec<u16>],
    dyn_libs: &[DynamicLibFn],
    allocated_arg_count: usize,
    allocated_call_depth: usize,
) {
    let mut i: usize = 0;

    let mut args: Vec<u16> = Vec::with_capacity(allocated_arg_count);
    let mut call_frames: Vec<CallFrame> = Vec::with_capacity(allocated_call_depth);
    let mut recursion_stack: Vec<Data> = Vec::with_capacity(allocated_call_depth * registers.len());

    let stdout = std::io::stdout();
    let mut handle = stdout.lock();

    let mut free_arrays: Vec<u16> = Vec::with_capacity(array_pool.len());

    let mut arg_storage: Vec<u64> = Vec::new();

    let len = instructions.len();
    while i < len {
        // dbg!(&instructions[i]);
        match instructions[i] {
            Instr::Jmp(size) => {
                i += size as usize;
                continue;
            }
            Instr::JmpBack(size) => {
                i -= size as usize;
                continue;
            }
            Instr::Mov(tgt, dest) => registers[dest as usize] = registers[tgt as usize],
            Instr::CallFunc(new_loc, return_id) => {
                call_frames.push(CallFrame {
                    return_addr: i as u32,
                    return_reg: return_id,
                    callsite_id: 0,
                });
                i = new_loc as usize;
                continue;
            }
            Instr::CallFuncRecursive(new_loc, _) => {
                i = new_loc as usize;
                continue;
            }
            Instr::VoidReturn => {
                // Simply jump back to the callsite, since there's nothing to return
                i = unsafe {
                    let new_len = call_frames.len() - 1;
                    let ptr = call_frames.as_mut_ptr().add(new_len);
                    call_frames.set_len(new_len);
                    ptr.read().return_addr as usize
                };
            }
            Instr::SaveFrame(relative_func_loc, return_register, callsite_id) => {
                call_frames.push(CallFrame {
                    return_addr: (i + relative_func_loc as usize) as u32,
                    return_reg: return_register,
                    callsite_id,
                });
                recursion_stack.extend(
                    fn_registers[callsite_id as usize]
                        .iter()
                        .map(|&r| registers[r as usize]),
                );
            }
            Instr::Return(tgt) => {
                // Pop the latest call frame, set the return value and jump back to the callsite
                let call_frame = unsafe {
                    let new_len = call_frames.len() - 1;
                    let ptr = call_frames.as_mut_ptr().add(new_len);
                    call_frames.set_len(new_len);
                    ptr.read()
                };
                i = call_frame.return_addr as usize;
                registers[call_frame.return_reg as usize] = registers[tgt as usize];
            }
            Instr::RecursiveReturn(tgt, fn_id) => {
                let call_frame = unsafe {
                    let new_len = call_frames.len() - 1;
                    let ptr = call_frames.as_mut_ptr().add(new_len);
                    call_frames.set_len(new_len);
                    ptr.read()
                };
                let temp = registers[tgt as usize];
                let regs = &fn_registers[call_frame.callsite_id as usize];
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
            Instr::FreeArray(id) => free_arrays.push(id),
            Instr::IsFalseJmp(cond_id, size) => {
                if registers[cond_id as usize] == FALSE {
                    i += size as usize;
                    continue;
                }
            }
            Instr::CallDynamicLibFunc(fn_id, dest) => {
                let func = &dyn_libs[fn_id as usize];
                let args_len = args.len();
                // Pointers are "owned" in arg_storage
                arg_storage.clear();

                for (idx, register_id) in args.drain(..args_len).enumerate() {
                    let data = registers[register_id as usize];
                    arg_storage.push({
                        let t = func.arg_types[idx].type_id();
                        if t == libffi::middle::Type::i32().type_id() {
                            data.as_int() as u64
                        } else if t == libffi::middle::Type::i64().type_id() {
                            data.as_int() as i64 as u64
                        } else if t == libffi::middle::Type::f32().type_id() {
                            (data.as_float() as f32).to_bits() as u64
                        } else if t == libffi::middle::Type::f64().type_id() {
                            data.as_float().to_bits()
                        } else if t == libffi::middle::Type::pointer().type_id() {
                            data.0
                        } else {
                            runtime_error(
                                instr_src,
                                src,
                                &instructions[i],
                                "Unknown argument type",
                                format_args!("Unknown argument type: {t:?}"),
                            );
                        }
                    });
                }

                // Args converted from Data to libffi args are stored here
                let mut ffi_args: Vec<libffi::middle::Arg> = Vec::with_capacity(args_len);
                for x in &arg_storage {
                    ffi_args.push(libffi::middle::Arg::new(x));
                }

                // Call the function, and convert the result back into Data
                registers[dest as usize] = unsafe {
                    let t = func.return_type.type_id();
                    if t == libffi::middle::Type::i32().type_id() {
                        func.cif.call::<i32>(func.ptr, &ffi_args).into()
                    } else if t == libffi::middle::Type::i64().type_id() {
                        (func.cif.call::<i64>(func.ptr, &ffi_args) as i32).into()
                    } else if t == libffi::middle::Type::f32().type_id() {
                        (func.cif.call::<f32>(func.ptr, &ffi_args) as f64).into()
                    } else if t == libffi::middle::Type::f64().type_id() {
                        func.cif.call::<f64>(func.ptr, &ffi_args).into()
                    } else if t == libffi::middle::Type::void().type_id() {
                        NULL
                    } else {
                        runtime_error(
                            instr_src,
                            src,
                            &instructions[i],
                            "Unknown return type",
                            format_args!("Unknown return type: {t:?}"),
                        );
                    }
                };
            }
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
                let arr_a = &array_pool[registers[o1 as usize].as_array() as usize];
                let arr_b = &array_pool[registers[o2 as usize].as_array() as usize];

                let mut combined = Vec::with_capacity(arr_a.len() + arr_b.len());
                combined.extend_from_slice(arr_a);
                combined.extend_from_slice(arr_b);
                let array_id = alloc_array(array_pool, &mut free_arrays);
                array_pool[array_id as usize] = combined;
                registers[dest as usize] = Data::array(array_id);
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
                registers[dest as usize] = (array_pool[registers[o1 as usize].as_array() as usize]
                    == array_pool[registers[o2 as usize].as_array() as usize])
                    .into();
            }
            Instr::NotEqJmp(o1, o2, jump_size) => {
                if registers[o1 as usize] != registers[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayNotEqJmp(o1, o2, jump_size) => {
                if array_pool[registers[o1 as usize].as_array() as usize]
                    != array_pool[registers[o2 as usize].as_array() as usize]
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
                registers[dest as usize] = (array_pool[registers[o1 as usize].as_array() as usize]
                    != array_pool[registers[o2 as usize].as_array() as usize])
                    .into();
            }
            Instr::EqJmp(o1, o2, jump_size) => {
                if registers[o1 as usize] == registers[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayEqJmp(o1, o2, jump_size) => {
                if array_pool[registers[o1 as usize].as_array() as usize]
                    == array_pool[registers[o2 as usize].as_array() as usize]
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
            Instr::InfEqFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() <= registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfEqIntJmp(o1, o2, jump_size) => {
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
            Instr::InfFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() < registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfIntJmp(o1, o2, jump_size) => {
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
            Instr::SupEqFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() >= registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupEqIntJmp(o1, o2, jump_size) => {
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
            Instr::SupFloatJmp(o1, o2, jump_size) => {
                if registers[o1 as usize].as_float() > registers[o2 as usize].as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupIntJmp(o1, o2, jump_size) => {
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
                } else if tgt.is_array() {
                    writeln!(
                        handle,
                        "{}",
                        concat_string!(
                            "[",
                            array_pool[tgt.as_array() as usize]
                                .iter()
                                .map(|x| format_data(x, Some(array_pool), false))
                                .collect::<Vec<_>>()
                                .join(","),
                            "]"
                        )
                    )
                    .unwrap();
                }
            }

            Instr::StoreFuncArg(id) => args.push(id),
            Instr::ArrayMov(new_elem_reg_id, array_id, idx) => {
                array_pool.get_mut(array_id as usize).unwrap()[idx as usize] =
                    registers[new_elem_reg_id as usize];
            }
            Instr::SetElementArray(array_reg_id, new_elem_reg_id, idx) => {
                let array = array_pool
                    .get_mut(registers[array_reg_id as usize].as_array() as usize)
                    .unwrap();
                let index = registers[idx as usize].as_int() as usize;
                if likely(array.len() > index) {
                    array[index] = registers[new_elem_reg_id as usize];
                } else {
                    runtime_error(
                        instr_src,
                        src,
                        &instructions[i],
                        "Invalid index",
                        format_args!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but array has {} elements",
                            index,
                            array.len()
                        ),
                    );
                }
            }
            Instr::SetElementString(string_reg_id, new_str_reg_id, idx) => {
                let index = registers[idx as usize].as_int();
                let source_string = registers[string_reg_id as usize].as_str();
                if likely(source_string.len() > index as usize) {
                    let mut temp = source_string.to_string();
                    temp.remove(index as usize);
                    temp.insert_str(index as usize, &registers[new_str_reg_id as usize].as_str());
                    registers[string_reg_id as usize] = temp.into();
                } else {
                    runtime_error(
                        instr_src,
                        src,
                        &instructions[i],
                        "Invalid index",
                        format_args!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but string has {} characters",
                            index,
                            source_string.len()
                        ),
                    );
                }
            }
            // takes tgt from  registers, index is index, dest is registers index destination
            Instr::GetIndexArray(array_reg_id, index, dest) => {
                let idx = registers[index as usize].as_int();
                let array = &array_pool[registers[array_reg_id as usize].as_array() as usize];
                if likely(array.len() > idx as usize) {
                    registers[dest as usize] = array[idx as usize];
                } else {
                    runtime_error(
                        instr_src,
                        src,
                        &instructions[i],
                        "Invalid index",
                        format_args!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but Array has {} elements",
                            idx,
                            array.len()
                        ),
                    );
                }
            }
            Instr::GetIndexString(tgt, index, dest) => {
                let idx = registers[index as usize].as_int();
                let str = registers[tgt as usize].as_str();
                if likely(str.len() > idx as usize) {
                    registers[dest as usize] = str.get(idx as usize..=idx as usize).unwrap().into();
                } else {
                    runtime_error(
                        instr_src,
                        src,
                        &instructions[i],
                        "Invalid index",
                        format_args!(
                            "Trying to get index {color_bright_blue}{style_bold}{}{color_reset}{style_reset} but String has {} characters",
                            idx,
                            str.len()
                        ),
                    );
                }
            }
            Instr::Push(array, element) => {
                array_pool
                    .get_mut(registers[array as usize].as_array() as usize)
                    .unwrap()
                    .push(registers[element as usize]);
            }
            Instr::Remove(array, idx) => {
                array_pool[registers[array as usize].as_array() as usize]
                    .remove(registers[idx as usize].as_int() as usize);
            }
            Instr::CallLibFunc(LibFunc::Uppercase, source_string_reg_id, dest_reg_id) => {
                registers[dest_reg_id as usize] = registers[source_string_reg_id as usize]
                    .as_str()
                    .to_uppercase()
                    .into();
            }
            Instr::CallLibFunc(LibFunc::Lowercase, source_string_reg_id, dest_reg_id) => {
                registers[dest_reg_id as usize] = registers[source_string_reg_id as usize]
                    .as_str()
                    .to_lowercase()
                    .into();
            }
            Instr::CallLibFunc(LibFunc::Contains, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let arg = registers[args.pop().unwrap() as usize].as_str();
                    registers[dest as usize] = str.contains(arg).into();
                } else if reg.is_array() {
                    let arg = registers[args.pop().unwrap() as usize];
                    registers[dest as usize] =
                        array_pool[reg.as_array() as usize].contains(&arg).into();
                }
            }
            Instr::CallLibFunc(LibFunc::Trim, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_str().trim().into();
            }
            Instr::CallLibFunc(LibFunc::TrimSequence, tgt, dest) => {
                let arg = registers[args.pop().unwrap() as usize].as_str();
                let chars: Vec<char> = arg.chars().collect();
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .trim_matches(&chars[..])
                    .into();
            }
            Instr::CallLibFunc(LibFunc::Find, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let element = registers[args.pop().unwrap() as usize].as_str();
                    registers[dest as usize] = if let Some(idx) = str.find(element) {
                        idx as i32
                    } else {
                        -1
                    }
                    .into();
                } else if reg.is_array() {
                    let arr_id = reg.as_array();
                    let element = registers[args.pop().unwrap() as usize];
                    registers[dest as usize] = if let Some(idx) = array_pool[arr_id as usize]
                        .iter()
                        .position(|x| x == &element)
                    {
                        idx as i32
                    } else {
                        -1
                    }
                    .into()
                }
            }
            Instr::CallLibFunc(LibFunc::IsFloat, tgt, dest) => {
                let num = registers[tgt as usize].as_str();
                registers[dest as usize] =
                    (num.parse::<f64>().is_ok() && num.parse::<i64>().is_err()).into();
            }
            Instr::CallLibFunc(LibFunc::IsInt, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .parse::<i64>()
                    .is_ok()
                    .into()
            }
            Instr::CallLibFunc(LibFunc::TrimLeft, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_str().trim_start().into();
            }
            Instr::CallLibFunc(LibFunc::TrimRight, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_str().trim_end().into();
            }
            Instr::CallLibFunc(LibFunc::TrimSequenceLeft, tgt, dest) => {
                let chars: Vec<char> = registers[args.pop().unwrap() as usize]
                    .as_str()
                    .chars()
                    .collect();
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .trim_start_matches(&chars[..])
                    .into();
            }
            Instr::CallLibFunc(LibFunc::TrimSequenceRight, tgt, dest) => {
                let chars: Vec<char> = registers[args.pop().unwrap() as usize]
                    .as_str()
                    .chars()
                    .collect();
                registers[dest as usize] = registers[tgt as usize]
                    .as_str()
                    .trim_end_matches(&chars[..])
                    .to_string()
                    .into();
            }
            Instr::CallLibFunc(LibFunc::Repeat, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str();
                    let repeat_count = registers[args.pop().unwrap() as usize].as_int();
                    registers[dest as usize] = str.repeat(repeat_count as usize).into();
                } else if reg.is_array() {
                    let repeat_count = registers[args.pop().unwrap() as usize].as_int();
                    let array_id = alloc_array(array_pool, &mut free_arrays);
                    array_pool[array_id as usize] =
                        array_pool[reg.as_array() as usize].repeat(repeat_count as usize);
                    registers[dest as usize] = Data::array(array_id);
                }
            }
            Instr::CallLibFunc(LibFunc::Round, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().round().into();
            }
            Instr::CallLibFunc(LibFunc::Abs, tgt, dest) => {
                let tgt = registers[tgt as usize];
                registers[dest as usize] = if tgt.is_float() {
                    tgt.as_float().abs().into()
                } else {
                    tgt.as_int().abs().into()
                }
            }
            // Instr::CallLibFunc(LibFunc::ReadFile, tgt, dest) => {
            //     let path = registers[tgt as usize].as_file();
            //     registers[dest as usize] = std::fs::read_to_string(path.as_str())
            //         .unwrap_or_else(|_| {
            //             runtime_error(
            //                 instr_src,
            //                 src,
            //                 &instructions[i],
            //                 "File does not exist or cannot be read",
            //                 format_args!("Cannot read file {color_red}{path}{color_reset}"),
            //             );
            //         })
            //         .into();
            // }
            // Instr::CallLibFunc(LibFunc::WriteFile, tgt, dest) => {
            //     let path = registers[tgt as usize].as_file();
            //     let contents = registers[args.pop().unwrap() as usize].as_str();
            //     let truncate = registers[args.pop().unwrap() as usize].as_bool();
            //     fs::OpenOptions::new()
            //                     .write(true)
            //                     .truncate(truncate)
            //                     .open(path.as_str()).unwrap_or_else(|_| {
            //                         runtime_error(instr_src,
            //                         src,
            //                         &instructions[i],"File does not exist or cannot be opened",format_args!("Cannot open file {color_red}{path}{color_reset}"));
            //                     }).write_all(contents.as_bytes()).unwrap_or_else(|_| {
            //                         runtime_error(instr_src,
            //                         src,
            //                         &instructions[i],"File does not exist or cannot be written to",format_args!("Cannot write {color_red}{path}{color_reset} to file {color_blue}{path}{color_reset}"));
            //                 });
            // }
            Instr::CallLibFunc(LibFunc::Reverse, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    registers[dest as usize] =
                        reg.as_str().chars().rev().collect::<String>().into();
                } else if reg.is_array() {
                    let id = reg.as_array();
                    array_pool[id as usize].reverse();
                    // registers[dest as usize] = Data::array(id);
                }
            }
            Instr::CallLibFunc(LibFunc::SqrtFloat, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().sqrt().into()
            }
            Instr::CallLibFunc(LibFunc::Float, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_int() {
                    registers[dest as usize] = (reg.as_int() as f64).into();
                } else if reg.is_str() {
                    let str = reg.as_str();
                    registers[dest as usize] = (str.parse::<f64>().unwrap_or_else(|_| {
                    runtime_error(instr_src,
                    src,
                    &
                        instructions[i],
                        "Invalid type",
                        format_args!(
                            "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a Float",
                            str
                        )
                    );
                    })).into();
                }
            }
            Instr::CallLibFunc(LibFunc::Int, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_float() {
                    registers[dest as usize] = (reg.as_float().round() as i32).into();
                } else if reg.is_str() {
                    let str = reg.as_str();
                    registers[dest as usize] = (str.parse::<i32>().unwrap_or_else(|_| {
                        runtime_error(instr_src, src, &instructions[i], "Invalid type", format_args!(
                            "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into an Integer",
                            str
                        ));
                    })).into();
                }
            }
            Instr::CallLibFunc(LibFunc::Str, tgt, dest) => {
                registers[dest as usize] =
                    format_data(&registers[tgt as usize], Some(array_pool), false).into();
            }
            Instr::CallLibFunc(LibFunc::Bool, tgt, dest) => {
                let str = registers[tgt as usize].as_str();
                registers[dest as usize] = (str.parse::<bool>().unwrap_or_else(|_| {
                    runtime_error(instr_src, src, &instructions[i], "Invalid type", format_args!(
                        "Cannot convert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} into a Boolean",
                        str
                    ));
                })).into();
            }
            Instr::CallLibFunc(LibFunc::Input, tgt, dest) => {
                let str_msg = registers[tgt as usize].as_str();
                println!("{str_msg}");
                std::io::stdout().flush().unwrap();
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap();
                registers[dest as usize] = (line.trim()).into();
            }
            Instr::CallLibFunc(LibFunc::Floor, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().floor().into()
            }
            Instr::CallLibFunc(LibFunc::TheAnswer, _, dest) => {
                writeln!(handle, "The answer to the Ultimate Question of Life, the Universe, and Everything is 42.").unwrap();
                registers[dest as usize] = 42.into();
            }
            Instr::CallLibFunc(LibFunc::Len, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_array() {
                    registers[dest as usize] =
                        (array_pool[reg.as_array() as usize].len() as i32).into()
                } else if reg.is_str() {
                    registers[dest as usize] = (reg.as_str().chars().count() as i32).into()
                }
            }
            Instr::CallLibFunc(LibFunc::StartsWith, source_register, dest_register) => {
                registers[dest_register as usize] = registers[source_register as usize]
                    .as_str()
                    .starts_with(registers[args.pop().unwrap() as usize].as_str())
                    .into();
            }
            Instr::CallLibFunc(LibFunc::EndsWith, source_register, dest_register) => {
                registers[dest_register as usize] = registers[source_register as usize]
                    .as_str()
                    .ends_with(registers[args.pop().unwrap() as usize].as_str())
                    .into();
            }
            Instr::CallLibFunc(LibFunc::Replace, source_register, dest_register) => {
                registers[dest_register as usize] = registers[source_register as usize]
                    .as_str()
                    .replace(
                        registers[args.pop().unwrap() as usize].as_str(),
                        registers[args.pop().unwrap() as usize].as_str(),
                    )
                    .into()
            }
            Instr::CallLibFunc(LibFunc::Split, source_register, dest_register) => {
                let source = registers[source_register as usize];
                let separator = args.pop().unwrap();
                if source.is_str() {
                    let output_str_register_id = array_pool.len() as u32;
                    array_pool.push(
                        source
                            .as_str()
                            .split(registers[separator as usize].as_str())
                            .map(|x| x.into())
                            .collect(),
                    );
                    registers[dest_register as usize] = Data::array(output_str_register_id);
                } else if source.is_array() {
                    // get the array and split it
                    let mut sub_array_ids: Vec<u32> = Vec::with_capacity(10);
                    array_pool[source.as_array() as usize]
                        .to_vec()
                        .split(|x| x == &registers[separator as usize])
                        .for_each(|x| {
                            let array_id = alloc_array(array_pool, &mut free_arrays);
                            array_pool[array_id as usize] = x.to_vec();
                            sub_array_ids.push(array_id);
                        });

                    let array_id = alloc_array(array_pool, &mut free_arrays);
                    array_pool[array_id as usize] = sub_array_ids
                        .iter()
                        .map(|id| Data::array(*id))
                        .collect::<Vec<Data>>();

                    registers[dest_register as usize] = Data::array(array_id);
                }
            }
            Instr::CallLibFunc(LibFunc::Range, max, dest) => {
                let min = if let Some(reg_id) = args.pop() {
                    registers[reg_id as usize].as_int()
                } else {
                    0
                };
                let max = registers[max as usize].as_int();
                let output_array_id = array_pool.len() as u32;
                array_pool.push((min..max).map(|x| x.into()).collect());
                registers[dest as usize] = Data::array(output_array_id);
            }
            Instr::CallLibFunc(LibFunc::JoinStringArray, tgt, dest) => {
                let separator = if let Some(arg) = args.pop() {
                    registers[arg as usize].as_str()
                } else {
                    ""
                };
                let array = &array_pool[registers[tgt as usize].as_array() as usize];
                let total_len: usize = array.iter().map(|x| x.as_str().len()).sum::<usize>()
                    + separator
                        .len()
                        .saturating_mul(array.len().saturating_sub(1));
                let mut output = String::with_capacity(total_len);
                for (i, x) in array.iter().enumerate() {
                    if i > 0 {
                        output.push_str(separator);
                    }
                    output.push_str(x.as_str());
                }
                registers[dest as usize] = output.into();
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
        error(
            format_args!("Unable to read contents of file {color_red}{filename}{color_reset}")
                .as_str()
                .unwrap(),
        )
    });

    let now = Instant::now();

    let (
        instructions,
        mut registers,
        mut arrays,
        instr_src,
        fn_registers,
        fn_dyn_libs,
        allocated_arg_count,
        allocated_call_depth,
    ) = parse(&contents, filename);

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
            &fn_dyn_libs,
            allocated_arg_count,
            allocated_call_depth,
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
            &fn_dyn_libs,
            allocated_arg_count,
            allocated_call_depth,
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
    fn_dyn_libs: &[DynamicLibFn],
    allocated_arg_count: usize,
    allocated_call_depth: usize,
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
            black_box(fn_dyn_libs),
            black_box(allocated_arg_count),
            black_box(allocated_call_depth),
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
            black_box(fn_dyn_libs),
            black_box(allocated_arg_count),
            black_box(allocated_call_depth),
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
