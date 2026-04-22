use crate::Data;
use crate::DynamicLibFn;
use crate::ErrType;
use crate::FALSE;
use crate::Instr;
use crate::LibFunc;
use crate::NULL;
use crate::alloc_array;
use crate::format_data;
use crate::fs;
use crate::likely;
use crate::parser_data::Pools;
use crate::throw_error;
use crate::util::unlikely;
use smol_str::SmolStr;
use smol_str::ToSmolStr;
use std::any::Any;
use std::hint::cold_path;
use std::io::Write;

pub type ArrayPool = Vec<Vec<Data>>;
pub type StringPool = Vec<String>;

struct CallFrame {
    return_addr: u32,
    return_reg: u16,
    callsite_id: u16,
}

pub fn execute(
    instructions: &[Instr],
    registers: &mut [Data],
    Pools {
        array_pool,
        string_pool,
    }: &mut Pools,
    instr_src: &[(Instr, (usize, usize), u16)],
    sources: &[(SmolStr, String)],
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
    let mut free_strings: Vec<u16> = Vec::with_capacity(string_pool.len());

    let mut arg_storage: Vec<u64> = Vec::new();

    macro_rules! str {
        ($e: expr) => {
            Data::str(
                $e,
                array_pool,
                string_pool,
                registers,
                &recursion_stack,
                &mut free_strings,
            )
        };
    }
    macro_rules! string {
        ($e: expr) => {
            Data::string(
                $e,
                array_pool,
                string_pool,
                registers,
                &recursion_stack,
                &mut free_strings,
            )
        };
    }

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
            Instr::Mov(tgt, dest) => registers[dest as usize] = registers[tgt as usize],
            Instr::SetInt(dest, n) => registers[dest as usize] = n.into(),
            Instr::SetBool(dest, b) => registers[dest as usize] = b.into(),
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
            Instr::RecursiveReturn(tgt) => {
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
                            throw_error(
                                instr_src,
                                sources,
                                &instructions[i],
                                ErrType::Custom(
                                    format_args!("Invalid argument type: {t:?}").to_smolstr(),
                                ),
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
                        throw_error(
                            instr_src,
                            sources,
                            &instructions[i],
                            ErrType::Custom(
                                format_args!("Invalid return type: {t:?}").to_smolstr(),
                            ),
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
                let l = registers[o1 as usize].as_str(string_pool);
                let r = registers[o2 as usize].as_str(string_pool);
                let mut s = String::with_capacity(l.len() + r.len());
                s.push_str(l);
                s.push_str(r);
                registers[dest as usize] = string!(s);
            }
            Instr::AddArray(o1, o2, dest) => {
                let arr_a = &array_pool[registers[o1 as usize].as_array()];
                let arr_b = &array_pool[registers[o2 as usize].as_array()];

                let mut combined = Vec::with_capacity(arr_a.len() + arr_b.len());
                combined.extend_from_slice(arr_a);
                combined.extend_from_slice(arr_b);
                let array_id =
                    alloc_array(array_pool, &mut free_arrays, registers, &recursion_stack);
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
                registers[dest as usize] = (array_pool[registers[o1 as usize].as_array()]
                    == array_pool[registers[o2 as usize].as_array()])
                .into();
            }
            Instr::NotEqJmp(o1, o2, jump_size) => {
                if registers[o1 as usize] != registers[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayNotEqJmp(o1, o2, jump_size) => {
                if array_pool[registers[o1 as usize].as_array()]
                    != array_pool[registers[o2 as usize].as_array()]
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
                registers[dest as usize] = (array_pool[registers[o1 as usize].as_array()]
                    != array_pool[registers[o2 as usize].as_array()])
                .into();
            }
            Instr::EqJmp(o1, o2, jump_size) => {
                if registers[o1 as usize] == registers[o2 as usize] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayEqJmp(o1, o2, jump_size) => {
                if array_pool[registers[o1 as usize].as_array()]
                    == array_pool[registers[o2 as usize].as_array()]
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
                    writeln!(handle, "{}", tgt.as_str(string_pool)).unwrap();
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
                        format_args!(
                            "[{}]",
                            array_pool[tgt.as_array()]
                                .iter()
                                .map(|x| format_data(x, array_pool, string_pool, false))
                                .collect::<Vec<_>>()
                                .join(","),
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
                    .get_mut(registers[array_reg_id as usize].as_array())
                    .unwrap();
                let index = registers[idx as usize].as_int() as usize;
                if likely(array.len() > index) {
                    array[index] = registers[new_elem_reg_id as usize];
                } else {
                    throw_error(
                        instr_src,
                        sources,
                        &instructions[i],
                        ErrType::IndexOutOfBounds(array.len(), index),
                    );
                }
            }
            Instr::SetElementString(string_reg_id, new_str_reg_id, idx) => {
                let index = registers[idx as usize].as_int() as usize;
                let source_string = registers[string_reg_id as usize].as_str(string_pool);
                if likely(source_string.len() > index) {
                    let mut temp = source_string.to_string();
                    temp.remove(index);
                    temp.insert_str(
                        index,
                        registers[new_str_reg_id as usize].as_str(string_pool),
                    );
                    registers[string_reg_id as usize] = string!(temp);
                } else {
                    throw_error(
                        instr_src,
                        sources,
                        &instructions[i],
                        ErrType::IndexOutOfBounds(source_string.len(), index),
                    );
                }
            }
            // takes tgt from  registers, index is index, dest is registers index destination
            Instr::GetIndexArray(array_reg_id, index, dest) => {
                let idx = registers[index as usize].as_int() as usize;
                let array = &array_pool[registers[array_reg_id as usize].as_array()];
                if likely(array.len() > idx) {
                    registers[dest as usize] = array[idx];
                } else {
                    throw_error(
                        instr_src,
                        sources,
                        &instructions[i],
                        ErrType::IndexOutOfBounds(array.len(), idx),
                    );
                }
            }
            Instr::GetIndexString(tgt, index, dest) => {
                let idx = registers[index as usize].as_int() as usize;
                let str = registers[tgt as usize].as_str(string_pool);
                if likely(str.len() > idx) {
                    registers[dest as usize] = str!(str.get(idx..=idx).unwrap());
                } else {
                    throw_error(
                        instr_src,
                        sources,
                        &instructions[i],
                        ErrType::IndexOutOfBounds(str.len(), idx),
                    );
                }
            }
            Instr::Push(array, element) => {
                array_pool
                    .get_mut(registers[array as usize].as_array())
                    .unwrap()
                    .push(registers[element as usize]);
            }
            Instr::Remove(array, idx) => {
                let arr = &mut array_pool[registers[array as usize].as_array()];
                let index = registers[idx as usize].as_int() as usize;
                if unlikely(index >= arr.len()) {
                    throw_error(
                        instr_src,
                        sources,
                        &instructions[i],
                        ErrType::IndexOutOfBounds(arr.len(), index),
                    );
                }
                arr.remove(index);
            }
            Instr::CallLibFunc(LibFunc::Uppercase, source_string_reg_id, dest_reg_id) => {
                registers[dest_reg_id as usize] = string!(
                    registers[source_string_reg_id as usize]
                        .as_str(string_pool)
                        .to_uppercase()
                );
            }
            Instr::CallLibFunc(LibFunc::Lowercase, source_string_reg_id, dest_reg_id) => {
                registers[dest_reg_id as usize] = string!(
                    registers[source_string_reg_id as usize]
                        .as_str(string_pool)
                        .to_lowercase()
                );
            }
            Instr::CallLibFunc(LibFunc::Contains, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    let arg = registers[args.pop().unwrap() as usize].as_str(string_pool);
                    registers[dest as usize] = str.contains(arg).into();
                } else if reg.is_array() {
                    let arg = registers[args.pop().unwrap() as usize];
                    registers[dest as usize] = array_pool[reg.as_array()].contains(&arg).into();
                }
            }
            Instr::CallLibFunc(LibFunc::Trim, tgt, dest) => {
                registers[dest as usize] = str!(registers[tgt as usize].as_str(string_pool).trim());
            }
            Instr::CallLibFunc(LibFunc::TrimSequence, tgt, dest) => {
                let arg = registers[args.pop().unwrap() as usize].as_str(string_pool);
                let chars: Vec<char> = arg.chars().collect();
                registers[dest as usize] = str!(
                    registers[tgt as usize]
                        .as_str(string_pool)
                        .trim_matches(&chars[..])
                );
            }
            Instr::CallLibFunc(LibFunc::Find, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    let element = registers[args.pop().unwrap() as usize].as_str(string_pool);
                    registers[dest as usize] = if let Some(idx) = str.find(element) {
                        idx as i32
                    } else {
                        -1
                    }
                    .into();
                } else if reg.is_array() {
                    let arr_id = reg.as_array();
                    let element = registers[args.pop().unwrap() as usize];
                    registers[dest as usize] =
                        if let Some(idx) = array_pool[arr_id].iter().position(|x| x == &element) {
                            idx as i32
                        } else {
                            -1
                        }
                        .into()
                }
            }
            Instr::CallLibFunc(LibFunc::IsFloat, tgt, dest) => {
                let num = registers[tgt as usize].as_str(string_pool);
                registers[dest as usize] =
                    (num.parse::<f64>().is_ok() && num.parse::<i64>().is_err()).into();
            }
            Instr::CallLibFunc(LibFunc::IsInt, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize]
                    .as_str(string_pool)
                    .parse::<i64>()
                    .is_ok()
                    .into()
            }
            Instr::CallLibFunc(LibFunc::TrimLeft, tgt, dest) => {
                registers[dest as usize] =
                    str!(registers[tgt as usize].as_str(string_pool).trim_start());
            }
            Instr::CallLibFunc(LibFunc::TrimRight, tgt, dest) => {
                registers[dest as usize] =
                    str!(registers[tgt as usize].as_str(string_pool).trim_end());
            }
            Instr::CallLibFunc(LibFunc::TrimSequenceLeft, tgt, dest) => {
                let chars: Vec<char> = registers[args.pop().unwrap() as usize]
                    .as_str(string_pool)
                    .chars()
                    .collect();
                registers[dest as usize] = str!(
                    registers[tgt as usize]
                        .as_str(string_pool)
                        .trim_start_matches(&chars[..])
                );
            }
            Instr::CallLibFunc(LibFunc::TrimSequenceRight, tgt, dest) => {
                let chars: Vec<char> = registers[args.pop().unwrap() as usize]
                    .as_str(string_pool)
                    .chars()
                    .collect();
                registers[dest as usize] = str!(
                    registers[tgt as usize]
                        .as_str(string_pool)
                        .trim_end_matches(&chars[..])
                );
            }
            Instr::CallLibFunc(LibFunc::Repeat, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    let repeat_count = registers[args.pop().unwrap() as usize].as_int();
                    registers[dest as usize] = string!(str.repeat(repeat_count as usize));
                } else if reg.is_array() {
                    let repeat_count = registers[args.pop().unwrap() as usize].as_int();
                    let array_id =
                        alloc_array(array_pool, &mut free_arrays, registers, &recursion_stack);
                    array_pool[array_id as usize] =
                        array_pool[reg.as_array()].repeat(repeat_count as usize);
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
            Instr::CallLibFunc(LibFunc::Reverse, tgt, dest) => {
                registers[dest as usize] = string!(
                    registers[tgt as usize]
                        .as_str(string_pool)
                        .chars()
                        .rev()
                        .collect::<String>()
                );
            }
            Instr::CallLibFuncVoid(LibFunc::Reverse, tgt, _) => {
                array_pool[registers[tgt as usize].as_array()].reverse();
            }
            Instr::CallLibFunc(LibFunc::SqrtFloat, tgt, dest) => {
                registers[dest as usize] = registers[tgt as usize].as_float().sqrt().into()
            }
            Instr::CallLibFunc(LibFunc::Float, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_int() {
                    registers[dest as usize] = (reg.as_int() as f64).into();
                } else if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    registers[dest as usize] = (str.parse::<f64>().unwrap_or_else(|_| {
                        cold_path();
                        throw_error(
                            instr_src,
                            sources,
                            &instructions[i],
                            ErrType::FloatParsingError,
                        );
                    }))
                    .into();
                }
            }
            Instr::CallLibFunc(LibFunc::Int, tgt, dest) => {
                let reg = registers[tgt as usize];
                if reg.is_float() {
                    registers[dest as usize] = (reg.as_float() as i32).into();
                } else if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    registers[dest as usize] = (str.parse::<i32>().unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], (*e.kind()).into());
                    }))
                    .into();
                }
            }
            Instr::CallLibFunc(LibFunc::Str, tgt, dest) => {
                registers[dest as usize] = str!(&format_data(
                    &registers[tgt as usize],
                    array_pool,
                    string_pool,
                    false
                ));
            }
            Instr::CallLibFunc(LibFunc::Bool, tgt, dest) => {
                let str = registers[tgt as usize].as_str(string_pool);
                registers[dest as usize] = (str.parse::<bool>().unwrap_or_else(|_| {
                    cold_path();
                    throw_error(
                        instr_src,
                        sources,
                        &instructions[i],
                        ErrType::BoolParsingError,
                    );
                }))
                .into();
            }
            Instr::CallLibFunc(LibFunc::Input, tgt, dest) => {
                let str_msg = registers[tgt as usize].as_str(string_pool);
                println!("{str_msg}");
                std::io::stdout().flush().unwrap();
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap();
                registers[dest as usize] = str!(line.trim());
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
                    registers[dest as usize] = (array_pool[reg.as_array()].len() as i32).into()
                } else if reg.is_str() {
                    registers[dest as usize] = (reg.as_str(string_pool).len() as i32).into()
                }
            }
            Instr::CallLibFunc(LibFunc::StartsWith, source_register, dest_register) => {
                registers[dest_register as usize] = registers[source_register as usize]
                    .as_str(string_pool)
                    .starts_with(registers[args.pop().unwrap() as usize].as_str(string_pool))
                    .into();
            }
            Instr::CallLibFunc(LibFunc::EndsWith, source_register, dest_register) => {
                registers[dest_register as usize] = registers[source_register as usize]
                    .as_str(string_pool)
                    .ends_with(registers[args.pop().unwrap() as usize].as_str(string_pool))
                    .into();
            }
            Instr::CallLibFunc(LibFunc::Replace, source_register, dest_register) => {
                registers[dest_register as usize] = string!(
                    registers[source_register as usize]
                        .as_str(string_pool)
                        .replace(
                            registers[args.pop().unwrap() as usize].as_str(string_pool),
                            registers[args.pop().unwrap() as usize].as_str(string_pool),
                        )
                );
            }
            Instr::CallLibFunc(LibFunc::Split, source_register, dest_register) => {
                let source = registers[source_register as usize];
                let separator = args.pop().unwrap();
                if source.is_str() {
                    let output_str_register_id = array_pool.len() as u32;
                    array_pool.push(
                        source
                            .as_str(string_pool)
                            .split(registers[separator as usize].as_str(string_pool))
                            .map(|x| str!(x))
                            .collect(),
                    );
                    registers[dest_register as usize] = Data::array(output_str_register_id);
                } else if source.is_array() {
                    // get the array and split it
                    let mut sub_array_ids: Vec<u32> = Vec::with_capacity(10);
                    array_pool[source.as_array()]
                        .to_vec()
                        .split(|x| x == &registers[separator as usize])
                        .for_each(|x| {
                            let array_id = alloc_array(
                                array_pool,
                                &mut free_arrays,
                                registers,
                                &recursion_stack,
                            );
                            array_pool[array_id as usize] = x.to_vec();
                            sub_array_ids.push(array_id);
                        });

                    let array_id =
                        alloc_array(array_pool, &mut free_arrays, registers, &recursion_stack);
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
                    registers[arg as usize].as_str(string_pool)
                } else {
                    ""
                };
                let array = &array_pool[registers[tgt as usize].as_array()];
                let total_len: usize = array
                    .iter()
                    .map(|x| x.as_str(string_pool).len())
                    .sum::<usize>()
                    + separator
                        .len()
                        .saturating_mul(array.len().saturating_sub(1));
                let mut output = String::with_capacity(total_len);
                for (i, x) in array.iter().enumerate() {
                    if i > 0 {
                        output.push_str(separator);
                    }
                    output.push_str(x.as_str(string_pool));
                }
                registers[dest as usize] = string!(output);
            }
            // -----
            // FILE SYSTEM FUNCTIONS
            // -----
            Instr::CallLibFunc(LibFunc::FsRead, path, dest_reg_id) => {
                registers[dest_reg_id as usize] = string!(
                    fs::read_to_string(registers[path as usize].as_str(string_pool))
                        .unwrap_or_else(|e| {
                            cold_path();
                            throw_error(instr_src, sources, &instructions[i], e.kind().into())
                        })
                );
            }
            Instr::CallLibFunc(LibFunc::FsExists, path, dest_reg_id) => {
                registers[dest_reg_id as usize] =
                    fs::exists(registers[path as usize].as_str(string_pool))
                        .unwrap_or_else(|e| {
                            cold_path();
                            throw_error(instr_src, sources, &instructions[i], e.kind().into())
                        })
                        .into()
            }
            // Overwrites a file, will create it if it doesn't exist
            Instr::CallLibFuncVoid(LibFunc::FsWrite, path, contents) => {
                fs::write(
                    registers[path as usize].as_str(string_pool),
                    registers[contents as usize].as_str(string_pool),
                )
                .unwrap_or_else(|e| {
                    cold_path();
                    throw_error(instr_src, sources, &instructions[i], e.kind().into())
                });
            }
            // Appends to a file, will create if it doesn't exist
            Instr::CallLibFuncVoid(LibFunc::FsAppend, path, contents) => {
                fs::OpenOptions::new()
                    .append(true)
                    .open(registers[path as usize].as_str(string_pool))
                    .unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], e.kind().into())
                    })
                    .write_all(registers[contents as usize].as_str(string_pool).as_bytes())
                    .unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], e.kind().into())
                    });
            }
            // Deletes the file located at `path`, throwing an error if it doesn't exist.
            Instr::CallLibFuncVoid(LibFunc::FsDelete, path, _) => {
                fs::remove_file(registers[path as usize].as_str(string_pool)).unwrap_or_else(|e| {
                    cold_path();
                    throw_error(instr_src, sources, &instructions[i], e.kind().into())
                });
            }
            // Deletes the empty directory located at `path`
            Instr::CallLibFuncVoid(LibFunc::FsDeleteDir, path, _) => {
                fs::remove_dir(registers[path as usize].as_str(string_pool)).unwrap_or_else(|e| {
                    cold_path();
                    throw_error(instr_src, sources, &instructions[i], e.kind().into())
                });
            }
            Instr::CallLibFunc(LibFunc::Argv, _, dest) => {
                let array_id = array_pool.len() as u32;
                array_pool.push(std::env::args().skip(2).map(|s| string!(s)).collect());
                registers[dest as usize] = Data::array(array_id);
            }
            Instr::CallLibFuncVoid(LibFunc::Sort, tgt, _) => {
                let array = &mut array_pool[registers[tgt as usize].as_array()];
                if !array.is_empty() {
                    if array[0].is_int() {
                        array.sort_unstable_by_key(|x| x.as_int());
                    } else if array[0].is_float() {
                        array.sort_unstable_by(|a, b| {
                            a.as_float()
                                .partial_cmp(&b.as_float())
                                .unwrap_or(std::cmp::Ordering::Equal)
                        });
                    } else if array[0].is_str() {
                        array.sort_unstable_by(|a, b| {
                            a.as_str(string_pool).cmp(b.as_str(string_pool))
                        });
                    }
                }
            }
            instr => unreachable!("Unhandled instruction: {instr:?}"),
        }
        i += 1;
    }
}
