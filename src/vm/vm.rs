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
use crate::instr::LibFuncVoid;
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
    let mut array_live: Vec<bool> = Vec::new();
    let mut string_live: Vec<bool> = Vec::new();

    let mut dyn_lib_args: Vec<u64> = Vec::new();

    let mut gc_string_threshold: u32 = 256;
    let mut gc_array_threshold: u32 = 256;

    macro_rules! str {
        ($e: expr) => {
            Data::str(
                $e,
                array_pool,
                string_pool,
                registers,
                &recursion_stack,
                &mut free_strings,
                &mut gc_string_threshold,
                &mut string_live,
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
                &mut gc_string_threshold,
                &mut string_live,
            )
        };
    }

    macro_rules! r {
        ($i:expr) => {
            unsafe { *registers.get_unchecked($i as usize) }
        };
    }
    macro_rules! w {
        ($i:expr) => {
            unsafe { registers.get_unchecked_mut($i as usize) }
        };
    }

    loop {
        match unsafe { *instructions.get_unchecked(i) } {
            Instr::Jmp(size) => {
                i += size as usize;
                continue;
            }
            Instr::JmpBack(size) => {
                i -= size as usize;
                continue;
            }
            Instr::Mov(tgt, dest) => *w!(dest) = r!(tgt),
            Instr::SetInt(dest, n) => *w!(dest) = n.into(),
            Instr::SetBool(dest, b) => *w!(dest) = b.into(),
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
                recursion_stack.extend(fn_registers[callsite_id as usize].iter().map(|&r| r!(r)));
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
                *w!(call_frame.return_reg) = r!(tgt);
            }
            Instr::RecursiveReturn(tgt) => {
                let call_frame = unsafe {
                    let new_len = call_frames.len() - 1;
                    let ptr = call_frames.as_mut_ptr().add(new_len);
                    call_frames.set_len(new_len);
                    ptr.read()
                };
                let temp = r!(tgt);
                let regs = &fn_registers[call_frame.callsite_id as usize];
                let base = recursion_stack.len() - regs.len();
                for (reg, &saved) in regs.iter().zip(&recursion_stack[base..]) {
                    *w!(*reg) = saved;
                }
                unsafe {
                    recursion_stack.set_len(base);
                }
                i = call_frame.return_addr as usize;
                *w!(call_frame.return_reg) = temp;
            }
            Instr::IsFalseJmp(cond_id, size) => {
                if r!(cond_id) == FALSE {
                    i += size as usize;
                    continue;
                }
            }
            Instr::CallDynamicLibFunc(fn_id, dest) => {
                let func = &dyn_libs[fn_id as usize];
                let args_len = args.len();
                // Pointers are "owned" in dyn_lib_args
                dyn_lib_args.clear();

                for (idx, register_id) in args.drain(..args_len).enumerate() {
                    let data = r!(register_id);
                    dyn_lib_args.push({
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
                for x in &dyn_lib_args {
                    ffi_args.push(libffi::middle::Arg::new(x));
                }

                // Call the function, and convert the result back into Data
                *w!(dest) = unsafe {
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
                *w!(dest) = (r!(o1).as_float() + r!(o2).as_float()).into();
            }
            Instr::AddInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() + r!(o2).as_int()).into();
            }
            Instr::AddStr(o1, o2, dest) => {
                let _d1 = r!(o1);
                let _d2 = r!(o2);
                let l = _d1.as_str(string_pool);
                let r = _d2.as_str(string_pool);
                let mut s = String::with_capacity(l.len() + r.len());
                s.push_str(l);
                s.push_str(r);
                *w!(dest) = string!(s);
            }
            Instr::AddArray(o1, o2, dest) => {
                let arr_a = &array_pool[r!(o1).as_array()];
                let arr_b = &array_pool[r!(o2).as_array()];

                let mut combined = Vec::with_capacity(arr_a.len() + arr_b.len());
                combined.extend_from_slice(arr_a);
                combined.extend_from_slice(arr_b);
                let array_id = alloc_array(
                    array_pool,
                    &mut free_arrays,
                    registers,
                    &recursion_stack,
                    &mut gc_array_threshold,
                    &mut array_live,
                );
                array_pool[array_id as usize] = combined;
                *w!(dest) = Data::array(array_id);
            }
            Instr::MulFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() * r!(o2).as_float()).into();
            }
            Instr::MulInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() * r!(o2).as_int()).into();
            }
            Instr::DivFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() / r!(o2).as_float()).into();
            }
            Instr::DivInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() / r!(o2).as_int()).into();
            }
            Instr::SubFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() - r!(o2).as_float()).into();
            }
            Instr::SubInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() - r!(o2).as_int()).into();
            }
            Instr::ModFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() % r!(o2).as_float()).into();
            }
            Instr::ModInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() % r!(o2).as_int()).into();
            }
            Instr::PowFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float().powf(r!(o2).as_float())).into();
            }
            Instr::PowInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int().pow(r!(o2).as_int() as u32)).into();
            }
            Instr::IncInt(reg) => {
                *w!(reg) = (r!(reg).as_int() + 1).into();
            }
            Instr::Eq(o1, o2, dest) => {
                *w!(dest) = (r!(o1) == r!(o2)).into();
            }
            Instr::ArrayEq(o1, o2, dest) => {
                *w!(dest) = (array_pool[r!(o1).as_array()] == array_pool[r!(o2).as_array()]).into();
            }
            Instr::NotEqJmp(o1, o2, jump_size) => {
                if r!(o1) != r!(o2) {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayNotEqJmp(o1, o2, jump_size) => {
                if array_pool[r!(o1).as_array()] != array_pool[r!(o2).as_array()] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::NotEq(o1, o2, dest) => {
                *w!(dest) = (r!(o1) != r!(o2)).into();
            }
            Instr::ArrayNotEq(o1, o2, dest) => {
                *w!(dest) = (array_pool[r!(o1).as_array()] != array_pool[r!(o2).as_array()]).into();
            }
            Instr::EqJmp(o1, o2, jump_size) => {
                if r!(o1) == r!(o2) {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::ArrayEqJmp(o1, o2, jump_size) => {
                if array_pool[r!(o1).as_array()] == array_pool[r!(o2).as_array()] {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() > r!(o2).as_float()).into();
            }
            Instr::SupInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() > r!(o2).as_int()).into();
            }
            Instr::InfEqFloatJmp(o1, o2, jump_size) => {
                if r!(o1).as_float() <= r!(o2).as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfEqIntJmp(o1, o2, jump_size) => {
                if r!(o1).as_int() <= r!(o2).as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupEqFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() >= r!(o2).as_float()).into();
            }
            Instr::SupEqInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() >= r!(o2).as_int()).into();
            }
            Instr::InfFloatJmp(o1, o2, jump_size) => {
                if r!(o1).as_float() < r!(o2).as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfIntJmp(o1, o2, jump_size) => {
                if r!(o1).as_int() < r!(o2).as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfIntJmpBack(o1, o2, jump_size) => {
                if r!(o1).as_int() < r!(o2).as_int() {
                    i -= jump_size as usize;
                    continue;
                }
            }
            Instr::InfFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() < r!(o2).as_float()).into();
            }
            Instr::InfInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() < r!(o2).as_int()).into();
            }
            Instr::SupEqFloatJmp(o1, o2, jump_size) => {
                if r!(o1).as_float() >= r!(o2).as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupEqIntJmp(o1, o2, jump_size) => {
                if r!(o1).as_int() >= r!(o2).as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::InfEqFloat(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_float() <= r!(o2).as_float()).into();
            }
            Instr::InfEqInt(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_int() <= r!(o2).as_int()).into();
            }
            Instr::SupFloatJmp(o1, o2, jump_size) => {
                if r!(o1).as_float() > r!(o2).as_float() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::SupIntJmp(o1, o2, jump_size) => {
                if r!(o1).as_int() > r!(o2).as_int() {
                    i += jump_size as usize;
                    continue;
                }
            }
            Instr::BoolAnd(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_bool() && r!(o2).as_bool()).into();
            }
            Instr::BoolOr(o1, o2, dest) => {
                *w!(dest) = (r!(o1).as_bool() || r!(o2).as_bool()).into();
            }
            Instr::NegFloat(tgt, dest) => {
                *w!(dest) = (-r!(tgt).as_float()).into();
            }
            Instr::NegInt(tgt, dest) => {
                *w!(dest) = (-r!(tgt).as_int()).into();
            }
            Instr::Print(target) => {
                let tgt = r!(target);
                if tgt.is_str() {
                    writeln!(handle, "{}", tgt.as_str(string_pool)).unwrap();
                } else if tgt.is_int() {
                    writeln!(handle, "{}", tgt.as_int()).unwrap();
                } else if tgt.is_float() {
                    writeln!(handle, "{}", tgt.as_float()).unwrap();
                } else if tgt.is_bool() {
                    writeln!(handle, "{}", tgt.as_bool()).unwrap();
                } else if tgt.is_array() {
                    let array = &array_pool[tgt.as_array()];
                    write!(handle, "[").unwrap();
                    for (idx, item) in array.iter().enumerate() {
                        if idx != 0 {
                            write!(handle, ",").unwrap();
                        }
                        write!(
                            handle,
                            "{}",
                            format_data(item, array_pool, string_pool, false)
                        )
                        .unwrap();
                    }
                    writeln!(handle, "]").unwrap();
                }
            }

            Instr::StoreFuncArg(id) => args.push(id),
            Instr::ArrayMov(new_elem_reg_id, array_id, idx) => {
                array_pool.get_mut(array_id as usize).unwrap()[idx as usize] = r!(new_elem_reg_id);
            }
            Instr::SetElementArray(array_reg_id, new_elem_reg_id, idx) => {
                let array = array_pool.get_mut(r!(array_reg_id).as_array()).unwrap();
                let index = r!(idx).as_int() as usize;
                if likely(array.len() > index) {
                    array[index] = r!(new_elem_reg_id);
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
                let index = r!(idx).as_int() as usize;
                let temp_str_reg_id = r!(string_reg_id);
                let source_string = temp_str_reg_id.as_str(string_pool);
                if likely(source_string.len() > index) {
                    let mut temp = source_string.to_string();
                    temp.remove(index);
                    temp.insert_str(index, r!(new_str_reg_id).as_str(string_pool));
                    *w!(string_reg_id) = string!(temp);
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
                let idx = r!(index).as_int() as usize;
                let array = &array_pool[r!(array_reg_id).as_array()];
                if likely(array.len() > idx) {
                    *w!(dest) = array[idx];
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
                let idx = r!(index).as_int() as usize;
                let _d_tgt = r!(tgt);
                let str = _d_tgt.as_str(string_pool);
                if likely(str.len() > idx) {
                    *w!(dest) = str!(str.get(idx..=idx).unwrap());
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
                    .get_mut(r!(array).as_array())
                    .unwrap()
                    .push(r!(element));
            }
            Instr::Remove(array, idx) => {
                let arr = &mut array_pool[r!(array).as_array()];
                let index = r!(idx).as_int() as usize;
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
                *w!(dest_reg_id) =
                    string!(r!(source_string_reg_id).as_str(string_pool).to_uppercase());
            }
            Instr::CallLibFunc(LibFunc::Lowercase, source_string_reg_id, dest_reg_id) => {
                *w!(dest_reg_id) =
                    string!(r!(source_string_reg_id).as_str(string_pool).to_lowercase());
            }
            Instr::CallLibFunc(LibFunc::Contains, tgt, dest) => {
                let reg = r!(tgt);
                if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    let temp_arg = r!(args.pop().unwrap());
                    let arg = temp_arg.as_str(string_pool);
                    *w!(dest) = str.contains(arg).into();
                } else if reg.is_array() {
                    let arg = r!(args.pop().unwrap());
                    *w!(dest) = array_pool[reg.as_array()].contains(&arg).into();
                }
            }
            Instr::CallLibFunc(LibFunc::Trim, tgt, dest) => {
                *w!(dest) = str!(r!(tgt).as_str(string_pool).trim());
            }
            Instr::CallLibFunc(LibFunc::TrimSequence, tgt, dest) => {
                let temp_arg = r!(args.pop().unwrap());
                let arg = temp_arg.as_str(string_pool);
                let chars: Vec<char> = arg.chars().collect();
                *w!(dest) = str!(r!(tgt).as_str(string_pool).trim_matches(&chars[..]));
            }
            Instr::CallLibFunc(LibFunc::Find, tgt, dest) => {
                let reg = r!(tgt);
                if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    let temp_elem = r!(args.pop().unwrap());
                    let element = temp_elem.as_str(string_pool);
                    *w!(dest) = if let Some(idx) = str.find(element) {
                        idx as i32
                    } else {
                        -1
                    }
                    .into();
                } else if reg.is_array() {
                    let arr_id = reg.as_array();
                    let element = r!(args.pop().unwrap());
                    *w!(dest) =
                        if let Some(idx) = array_pool[arr_id].iter().position(|x| x == &element) {
                            idx as i32
                        } else {
                            -1
                        }
                        .into()
                }
            }
            Instr::CallLibFunc(LibFunc::IsFloat, tgt, dest) => {
                let temp_tgt = r!(tgt);
                let num = temp_tgt.as_str(string_pool);
                *w!(dest) = (num.parse::<f64>().is_ok() && num.parse::<i64>().is_err()).into();
            }
            Instr::CallLibFunc(LibFunc::IsInt, tgt, dest) => {
                *w!(dest) = r!(tgt).as_str(string_pool).parse::<i64>().is_ok().into()
            }
            Instr::CallLibFunc(LibFunc::TrimLeft, tgt, dest) => {
                *w!(dest) = str!(r!(tgt).as_str(string_pool).trim_start());
            }
            Instr::CallLibFunc(LibFunc::TrimRight, tgt, dest) => {
                *w!(dest) = str!(r!(tgt).as_str(string_pool).trim_end());
            }
            Instr::CallLibFunc(LibFunc::TrimSequenceLeft, tgt, dest) => {
                let chars: Vec<char> = r!(args.pop().unwrap())
                    .as_str(string_pool)
                    .chars()
                    .collect();
                *w!(dest) = str!(r!(tgt).as_str(string_pool).trim_start_matches(&chars[..]));
            }
            Instr::CallLibFunc(LibFunc::TrimSequenceRight, tgt, dest) => {
                let chars: Vec<char> = r!(args.pop().unwrap())
                    .as_str(string_pool)
                    .chars()
                    .collect();
                *w!(dest) = str!(r!(tgt).as_str(string_pool).trim_end_matches(&chars[..]));
            }
            Instr::CallLibFunc(LibFunc::Repeat, tgt, dest) => {
                let reg = r!(tgt);
                if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    let repeat_count = r!(args.pop().unwrap()).as_int();
                    *w!(dest) = string!(str.repeat(repeat_count as usize));
                } else if reg.is_array() {
                    let repeat_count = r!(args.pop().unwrap()).as_int();
                    let array_id = alloc_array(
                        array_pool,
                        &mut free_arrays,
                        registers,
                        &recursion_stack,
                        &mut gc_array_threshold,
                        &mut array_live,
                    );
                    array_pool[array_id as usize] =
                        array_pool[reg.as_array()].repeat(repeat_count as usize);
                    *w!(dest) = Data::array(array_id);
                }
            }
            Instr::CallLibFunc(LibFunc::Round, tgt, dest) => {
                *w!(dest) = r!(tgt).as_float().round().into();
            }
            Instr::CallLibFunc(LibFunc::Abs, tgt, dest) => {
                let tgt = r!(tgt);
                *w!(dest) = if tgt.is_float() {
                    tgt.as_float().abs().into()
                } else {
                    tgt.as_int().abs().into()
                }
            }
            Instr::CallLibFunc(LibFunc::Reverse, tgt, dest) => {
                *w!(dest) = string!(
                    r!(tgt)
                        .as_str(string_pool)
                        .chars()
                        .rev()
                        .collect::<String>()
                );
            }
            Instr::CallLibFuncVoid(LibFuncVoid::Reverse, tgt, _) => {
                array_pool[r!(tgt).as_array()].reverse();
            }
            Instr::CallLibFunc(LibFunc::SqrtFloat, tgt, dest) => {
                *w!(dest) = r!(tgt).as_float().sqrt().into()
            }
            Instr::CallLibFunc(LibFunc::Float, tgt, dest) => {
                let reg = r!(tgt);
                if reg.is_int() {
                    *w!(dest) = (reg.as_int() as f64).into();
                } else if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    *w!(dest) = (str.parse::<f64>().unwrap_or_else(|_| {
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
                let reg = r!(tgt);
                if reg.is_float() {
                    *w!(dest) = (reg.as_float() as i32).into();
                } else if reg.is_str() {
                    let str = reg.as_str(string_pool);
                    *w!(dest) = (str.parse::<i32>().unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], (*e.kind()).into());
                    }))
                    .into();
                }
            }
            Instr::CallLibFunc(LibFunc::Str, tgt, dest) => {
                let value = r!(tgt);
                *w!(dest) = if value.is_str() {
                    value
                } else if value.is_int() {
                    string!(value.as_int().to_string())
                } else if value.is_float() {
                    string!(value.as_float().to_string())
                } else if value.is_bool() {
                    str!(if value.as_bool() { "true" } else { "false" })
                } else {
                    str!(&format_data(&value, array_pool, string_pool, false))
                };
            }
            Instr::CallLibFunc(LibFunc::Bool, tgt, dest) => {
                let temp_tgt = r!(tgt);
                let str = temp_tgt.as_str(string_pool);
                *w!(dest) = (str.parse::<bool>().unwrap_or_else(|_| {
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
                let temp_tgt = r!(tgt);
                let str_msg = temp_tgt.as_str(string_pool);
                println!("{str_msg}");
                std::io::stdout().flush().unwrap();
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap();
                *w!(dest) = str!(line.trim());
            }
            Instr::CallLibFunc(LibFunc::Floor, tgt, dest) => {
                *w!(dest) = r!(tgt).as_float().floor().into()
            }
            Instr::CallLibFunc(LibFunc::TheAnswer, _, dest) => {
                writeln!(handle, "The answer to the Ultimate Question of Life, the Universe, and Everything is 42.").unwrap();
                *w!(dest) = 42.into();
            }
            Instr::CallLibFunc(LibFunc::Len, tgt, dest) => {
                let reg = r!(tgt);
                if reg.is_array() {
                    *w!(dest) = (array_pool[reg.as_array()].len() as i32).into()
                } else if reg.is_str() {
                    *w!(dest) = (reg.as_str(string_pool).len() as i32).into()
                }
            }
            Instr::CallLibFunc(LibFunc::StartsWith, source_register, dest_register) => {
                *w!(dest_register) = r!(source_register)
                    .as_str(string_pool)
                    .starts_with(r!(args.pop().unwrap()).as_str(string_pool))
                    .into();
            }
            Instr::CallLibFunc(LibFunc::EndsWith, source_register, dest_register) => {
                *w!(dest_register) = r!(source_register)
                    .as_str(string_pool)
                    .ends_with(r!(args.pop().unwrap()).as_str(string_pool))
                    .into();
            }
            Instr::CallLibFunc(LibFunc::Replace, source_register, dest_register) => {
                *w!(dest_register) = string!(r!(source_register).as_str(string_pool).replace(
                    r!(args.pop().unwrap()).as_str(string_pool),
                    r!(args.pop().unwrap()).as_str(string_pool),
                ));
            }
            Instr::CallLibFunc(LibFunc::Split, source_register, dest_register) => {
                let source = r!(source_register);
                let separator = args.pop().unwrap();
                if source.is_str() {
                    let output_str_reg_id = alloc_array(
                        array_pool,
                        &mut free_arrays,
                        registers,
                        &recursion_stack,
                        &mut gc_array_threshold,
                        &mut array_live,
                    );
                    array_pool[output_str_reg_id as usize] = source
                        .as_str(string_pool)
                        .split(r!(separator).as_str(string_pool))
                        .map(|x| str!(x))
                        .collect();
                    *w!(dest_register) = Data::array(output_str_reg_id);
                } else if source.is_array() {
                    let source_array_id = source.as_array();
                    let separator = r!(separator);
                    let source_array = &array_pool[source_array_id];

                    // Find the source slice ranges that will become output arrays
                    let mut split_ranges: Vec<(usize, usize)> =
                        Vec::with_capacity(source_array.len());

                    let mut start = 0;
                    for (idx, item) in source_array.iter().enumerate() {
                        if *item == separator {
                            split_ranges.push((start, idx));
                            start = idx + 1;
                        }
                    }
                    split_ranges.push((start, source_array.len()));

                    // Allocate one pooled array per recorded range and copy just that segment
                    let mut sub_array_ids: Vec<u32> = Vec::with_capacity(split_ranges.len());
                    for (start, end) in split_ranges {
                        let dest_array_id = alloc_array(
                            array_pool,
                            &mut free_arrays,
                            registers,
                            &recursion_stack,
                            &mut gc_array_threshold,
                            &mut array_live,
                        ) as usize;
                        // Use split_at_mut to copy directly from the source array without having to clone it
                        if dest_array_id < source_array_id {
                            let (left, right) = array_pool.split_at_mut(source_array_id);
                            left[dest_array_id].extend_from_slice(&right[0][start..end]);
                        } else {
                            let (left, right) = array_pool.split_at_mut(dest_array_id);
                            right[0].extend_from_slice(&left[source_array_id][start..end]);
                        }
                        sub_array_ids.push(dest_array_id as u32);
                    }

                    let array_id = alloc_array(
                        array_pool,
                        &mut free_arrays,
                        registers,
                        &recursion_stack,
                        &mut gc_array_threshold,
                        &mut array_live,
                    );
                    array_pool[array_id as usize] = sub_array_ids
                        .iter()
                        .map(|id| Data::array(*id))
                        .collect::<Vec<Data>>();

                    *w!(dest_register) = Data::array(array_id);
                }
            }
            Instr::CallLibFunc(LibFunc::Range, max, dest) => {
                let min = if let Some(reg_id) = args.pop() {
                    r!(reg_id).as_int()
                } else {
                    0
                };
                let max = r!(max).as_int();
                let output_array_id = alloc_array(
                    array_pool,
                    &mut free_arrays,
                    registers,
                    &recursion_stack,
                    &mut gc_array_threshold,
                    &mut array_live,
                );
                array_pool[output_array_id as usize].clear();
                array_pool[output_array_id as usize].extend((min..max).map(Data::from));
                *w!(dest) = Data::array(output_array_id);
            }
            Instr::CallLibFunc(LibFunc::JoinStringArray, tgt, dest) => {
                let temp_separator: Option<Data> = args.pop().map(|arg| r!(arg));
                let separator = temp_separator
                    .as_ref()
                    .map_or("", |d| d.as_str(string_pool));
                let array = &array_pool[r!(tgt).as_array()];
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
                *w!(dest) = string!(output);
            }
            // -----
            // FILE SYSTEM FUNCTIONS
            // -----
            Instr::CallLibFunc(LibFunc::FsRead, path, dest_reg_id) => {
                *w!(dest_reg_id) = string!(
                    fs::read_to_string(r!(path).as_str(string_pool)).unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], e.kind().into())
                    })
                );
            }
            Instr::CallLibFunc(LibFunc::FsExists, path, dest_reg_id) => {
                *w!(dest_reg_id) = fs::exists(r!(path).as_str(string_pool))
                    .unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], e.kind().into())
                    })
                    .into()
            }
            // Overwrites a file, will create it if it doesn't exist
            Instr::CallLibFuncVoid(LibFuncVoid::FsWrite, path, contents) => {
                fs::write(
                    r!(path).as_str(string_pool),
                    r!(contents).as_str(string_pool),
                )
                .unwrap_or_else(|e| {
                    cold_path();
                    throw_error(instr_src, sources, &instructions[i], e.kind().into())
                });
            }
            // Appends to a file, will create if it doesn't exist
            Instr::CallLibFuncVoid(LibFuncVoid::FsAppend, path, contents) => {
                fs::OpenOptions::new()
                    .append(true)
                    .open(r!(path).as_str(string_pool))
                    .unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], e.kind().into())
                    })
                    .write_all(r!(contents).as_str(string_pool).as_bytes())
                    .unwrap_or_else(|e| {
                        cold_path();
                        throw_error(instr_src, sources, &instructions[i], e.kind().into())
                    });
            }
            // Deletes the file located at `path`, throwing an error if it doesn't exist.
            Instr::CallLibFuncVoid(LibFuncVoid::FsDelete, path, _) => {
                fs::remove_file(r!(path).as_str(string_pool)).unwrap_or_else(|e| {
                    cold_path();
                    throw_error(instr_src, sources, &instructions[i], e.kind().into())
                });
            }
            // Deletes the empty directory located at `path`
            Instr::CallLibFuncVoid(LibFuncVoid::FsDeleteDir, path, _) => {
                fs::remove_dir(r!(path).as_str(string_pool)).unwrap_or_else(|e| {
                    cold_path();
                    throw_error(instr_src, sources, &instructions[i], e.kind().into())
                });
            }
            Instr::CallLibFunc(LibFunc::Argv, _, dest) => {
                let array_id = alloc_array(
                    array_pool,
                    &mut free_arrays,
                    registers,
                    &recursion_stack,
                    &mut gc_array_threshold,
                    &mut array_live,
                );
                array_pool[array_id as usize] = std::env::args()
                    .skip(2)
                    .map(|s| string!(s))
                    .collect::<Vec<Data>>();
                *w!(dest) = Data::array(array_id);
            }
            Instr::CallLibFuncVoid(LibFuncVoid::Sort, tgt, _) => {
                let array = &mut array_pool[r!(tgt).as_array()];
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
            Instr::Halt => break,
        }
        i += 1;
    }
}
