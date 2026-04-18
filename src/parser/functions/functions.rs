use crate::Instr;
use crate::check_args;
use crate::check_args_range;
use crate::data::Data;
use crate::data::NULL;
use crate::debug;
use crate::errors::ErrType;
use crate::errors::throw_parser_error;
use crate::get_id;
use crate::instr::LibFunc;
use crate::parser::Expr;
use crate::parser::alloc_register;
use crate::parser::compile_expr;
use crate::parser::for_each_read_reg;
use crate::parser::free_register;
use crate::parser::get_tgt_ids;
use crate::parser::move_to_id;
use crate::parser_data::FnSignature;
use crate::parser_data::FunctionImpl;
use crate::parser_data::ParserData;
use crate::parser_data::Pools;
use crate::parser_data::Variable;
use crate::type_system::DataType;
use crate::type_system::check_poly;
use crate::type_system::infer_type;
use crate::type_system::track_returns;
use inline_colorization::*;
use smol_str::SmolStr;
use smol_str::ToSmolStr;
use std::slice;

pub fn handle_functions(
    output: &mut Vec<Instr>,
    v: &mut Vec<Variable>,
    p: &ParserData,

    // method call data
    args: &[Expr],
    namespace: &[SmolStr],
    markers: &(usize, usize),
    args_indexes: &[(usize, usize)],
    offset: u16,
) -> Option<u16> {
    let (
        registers,
        fns,
        Pools {
            array_pool: _,
            string_pool,
        },
        instr_src,
        fn_registers,
        _,
        src,
        _,
        _,
        dyn_libs,
        allocated_arg_count,
        allocated_call_depth,
        const_registers,
        free_registers,
        _,
        current_src_file,
    ) = p.destructure();

    let mut check_arg_type = |arg_idx: usize, expected: &[DataType]| {
        let infered = infer_type(&args[arg_idx], v, fns, src, dyn_libs);
        if !{
            if let DataType::Poly(polytype) = &infered {
                polytype.iter().all(|x| expected.contains(x))
            } else {
                expected.contains(&infered)
            }
        } {
            throw_parser_error(
                src,
                &args_indexes[arg_idx],
                ErrType::Custom(
                format_args!(
                    "Expected {color_bright_blue}{style_bold}{}{color_reset}{style_reset}, found {color_bright_red}{style_bold}{}{color_reset}{style_reset}",
                    expected
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join("{color_reset}{style_reset} or {color_bright_blue}{style_bold}"),
                    infered
                ).to_smolstr())
            );
        }
    };
    let len = namespace.len() - 1;
    let name = namespace[len].as_str();
    let namespace = &namespace[0..len];
    if namespace.is_empty() {
        match name {
            "print" => {
                for arg in args {
                    let id = get_id(arg, v, p, output, None, false, false, offset);
                    output.push(Instr::Print(id));
                    free_register(id, free_registers, v, const_registers);
                }
            }
            "type" => {
                check_args!(args, 1, name, src, markers);
                let infered = infer_type(&args[0], v, fns, src, dyn_libs);
                registers.push(Data::p_str(&infered.to_string(), string_pool));
            }
            "float" => {
                check_args!(args, 1, name, src, markers);
                check_arg_type(0, &[DataType::String, DataType::Int]);
                let id = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::Float,
                    id,
                    alloc_register(registers, free_registers),
                ));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "int" => {
                check_args!(args, 1, name, src, markers);
                check_arg_type(0, &[DataType::String, DataType::Float]);
                let id = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::Int,
                    id,
                    alloc_register(registers, free_registers),
                ));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "str" => {
                check_args!(args, 1, name, src, markers);
                let id = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::Str,
                    id,
                    alloc_register(registers, free_registers),
                ));
            }
            "bool" => {
                check_args!(args, 1, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                let id = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::Bool,
                    id,
                    alloc_register(registers, free_registers),
                ));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "input" => {
                check_args_range!(args, 0, 1, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                let id = if args.is_empty() {
                    registers.push(Data::p_str("", string_pool));
                    (registers.len() - 1) as u16
                } else {
                    get_id(&args[0], v, p, output, None, false, false, offset)
                };
                free_register(id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::Input,
                    id,
                    alloc_register(registers, free_registers),
                ));
            }
            "range" => {
                check_args_range!(args, 1, 2, name, src, markers);
                check_arg_type(0, &[DataType::Int]);
                if args.len() != 1 {
                    check_arg_type(1, &[DataType::Int]);
                }

                let id_first_arg = get_id(&args[0], v, p, output, None, false, false, offset);
                let source_reg_id = if args.len() == 1 {
                    id_first_arg
                } else {
                    let id_second_arg = get_id(&args[1], v, p, output, None, false, false, offset);
                    output.push(Instr::StoreFuncArg(id_first_arg));
                    *allocated_arg_count += 1;
                    id_second_arg
                };
                free_register(id_first_arg, free_registers, v, const_registers);
                free_register(source_reg_id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::Range,
                    source_reg_id,
                    alloc_register(registers, free_registers),
                ));
            }
            "the_answer" => {
                check_args!(args, 0, name, src, markers);
                output.push(Instr::CallLibFunc(
                    LibFunc::TheAnswer,
                    0,
                    alloc_register(registers, free_registers),
                ));
            }
            "argv" => {
                check_args!(args, 0, name, src, markers);
                output.push(Instr::CallLibFunc(
                    LibFunc::Argv,
                    0,
                    alloc_register(registers, free_registers),
                ));
            }
            fn_name => {
                // Lookup function by name in function registry
                // Registry is (fn_name, fn_args, fn_code, fn_data (per implementation: loc, args_loc, arg_types) )
                let function_id = fns
                    .iter_mut()
                    .position(|func| func.name == fn_name)
                    .unwrap_or_else(|| {
                        throw_parser_error(src, markers, ErrType::UnknownFunction(fn_name));
                    });
                // Retrieve list of args, code, and function data (loc, args_loc, arg_types)
                let fn_id = fns[function_id].id;
                let is_recursive = fns[function_id].is_recursive;
                let fn_args = &fns[function_id].args;
                let fn_code = &fns[function_id].code;
                let fn_impls = &fns[function_id].impls;
                let fn_returns_void = &fns[function_id].returns_void;

                let args_len = fn_args.len();
                check_args!(args, args_len, fn_name, src, markers);

                // Infer arg types
                let infered_arg_types = args
                    .iter()
                    .map(|x| infer_type(x, v, fns, src, dyn_libs))
                    .collect::<Vec<DataType>>();

                // Try to check if function has already been compiled for these specific arg types
                let fn_loc_data = if let Some(idx) = fn_impls
                    .iter()
                    .position(|fn_impl| *fn_impl.arg_types == infered_arg_types)
                {
                    &fn_impls[idx]
                } else {
                    // If it hasn't, compile it (which adds it to the function's implementation list)
                    compile_function(
                        output,
                        v,
                        p,
                        function_id,
                        fn_args,
                        fn_name,
                        &infered_arg_types,
                        args,
                        fn_code,
                        fn_id,
                        is_recursive,
                        offset,
                    );
                    fns.get(function_id).unwrap().impls.last().unwrap()
                };

                let saveframe_loc = output.len();
                let callsite_id = if is_recursive {
                    let id = fn_registers.len() as u16;
                    fn_registers.push(Vec::new());
                    output.push(Instr::SaveFrame(0, 0, 0));
                    *allocated_call_depth += 2;
                    Some(id)
                } else {
                    None
                };
                // Move evaluated call args into the expected arg slots
                let fn_args = fn_loc_data.args_loc.to_vec();
                for (x, tgt_id) in fn_args.iter().enumerate() {
                    let start_len = output.len();

                    let arg_id =
                        get_id(&args[x], v, p, output, Some(*tgt_id), false, false, offset);
                    debug!("MOVING ARG TO {tgt_id}");
                    if output.len() != start_len {
                        move_to_id(output, *tgt_id);
                    } else {
                        output.push(Instr::Mov(arg_id, *tgt_id))
                    }
                }
                if !is_recursive {
                    fn_registers
                        .get_mut(fn_id as usize)
                        .unwrap()
                        .extend(get_tgt_ids(&output[saveframe_loc..]));
                }
                let loc = fn_loc_data.loc;

                let return_register_id = if !*fn_returns_void {
                    alloc_register(registers, free_registers)
                } else {
                    0
                };
                if is_recursive {
                    output.push(Instr::CallFuncRecursive(loc, return_register_id));
                } else {
                    output.push(Instr::CallFunc(loc, return_register_id));
                    *allocated_call_depth += 2;
                }
                debug!("REGLEN {}", registers.len() - 1);

                if is_recursive {
                    debug!("OUTPUT LEN IS {}", output.len() - 1);
                    output[saveframe_loc] = Instr::SaveFrame(
                        (output.len() - 1 - saveframe_loc) as u16,
                        return_register_id,
                        callsite_id.unwrap(),
                    );
                }

                return Some(return_register_id);
            }
        }
    } else if namespace == ["fs"] {
        match name {
            "read" => {
                check_args!(args, 1, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                let id = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::FsRead,
                    id,
                    alloc_register(registers, free_registers),
                ));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "exists" => {
                check_args!(args, 1, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                let id = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(id, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(
                    LibFunc::FsExists,
                    id,
                    alloc_register(registers, free_registers),
                ));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "write" => {
                check_args!(args, 2, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                check_arg_type(1, &[DataType::String]);
                let filepath = get_id(&args[0], v, p, output, None, false, false, offset);
                let contents = get_id(&args[1], v, p, output, None, false, false, offset);
                free_register(filepath, free_registers, v, const_registers);
                free_register(contents, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(LibFunc::FsWrite, filepath, contents));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "append" => {
                check_args!(args, 2, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                check_arg_type(1, &[DataType::String]);
                let filepath = get_id(&args[0], v, p, output, None, false, false, offset);
                let contents = get_id(&args[1], v, p, output, None, false, false, offset);
                free_register(filepath, free_registers, v, const_registers);
                free_register(contents, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(LibFunc::FsAppend, filepath, contents));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "delete" => {
                check_args!(args, 1, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                let path = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(path, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(LibFunc::FsDelete, path, 0));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            "delete_dir" => {
                check_args!(args, 1, name, src, markers);
                check_arg_type(0, &[DataType::String]);
                let path = get_id(&args[0], v, p, output, None, false, false, offset);
                free_register(path, free_registers, v, const_registers);
                output.push(Instr::CallLibFunc(LibFunc::FsDeleteDir, path, 0));
                instr_src.push((*output.last().unwrap(), *markers, current_src_file));
            }
            name => {
                throw_parser_error(src, markers, ErrType::UnknownFunction(name));
            }
        }
    } else if let Some(lib) = dyn_libs.iter().find(|l| l.name == namespace[0]) {
        if let Some(FnSignature {
            name: fn_name,
            args: fn_args,
            return_type: fn_return_type,
            id,
        }) = lib.fns.iter().find(|x| x.name == name)
        {
            check_args!(args, fn_args.len(), fn_name, src, markers);
            for (i, a) in fn_args.iter().enumerate() {
                check_arg_type(i, slice::from_ref(a));
            }

            for arg in args {
                let arg_id = get_id(arg, v, p, output, None, false, false, offset);
                output.push(Instr::StoreFuncArg(arg_id));
                // This may break stuff
                free_register(arg_id, free_registers, v, const_registers);
                *allocated_arg_count += 1;
            }

            let register_id = if fn_return_type == &DataType::Null {
                0u16
            } else {
                registers.push(NULL);
                (registers.len() - 1) as u16
            };
            output.push(Instr::CallDynamicLibFunc(*id, register_id));
            instr_src.push((
                Instr::CallDynamicLibFunc(*id, register_id),
                *markers,
                current_src_file,
            ));
        }
    } else {
        throw_parser_error(
            src,
            markers,
            ErrType::UnknownNamespace(
                namespace
                    .iter()
                    .map(|x| (*x).to_string())
                    .collect::<Vec<String>>()
                    .join("::")
                    .as_str(),
            ),
        );
    }
    None
}

fn compile_function(
    output: &mut Vec<Instr>,
    v: &mut Vec<Variable>,
    p: &ParserData,
    function_id: usize,
    fn_args: &[SmolStr],
    fn_name: &str,
    infered_arg_types: &[DataType],
    args: &[Expr],
    fn_code: &[Expr],
    fn_id: u16,
    is_recursive: bool,
    offset: u16,
) {
    let (
        registers,
        fns,
        _,
        _,
        fn_registers,
        _,
        src,
        _,
        _,
        dyn_libs,
        _,
        _,
        _,
        _,
        sources,
        current_src_file,
    ) = p.destructure();
    debug!("CREATING FUNCTION {fn_name}, ARG TYPES ARE {infered_arg_types:?}");
    // Use the function's own source file for error reporting inside the function body
    let fn_src_file = fns[function_id].src_file;
    let fn_src: (&str, &str) = if fn_src_file != current_src_file {
        let name: &str = sources[fn_src_file as usize].0.as_str();
        let content: &str = sources[fn_src_file as usize].1.as_str();
        (name, content)
    } else {
        src
    };

    // Local vector vars and recorded_types to allow the inner body to type-check correctly
    let mut v_temp: Vec<Variable> = fn_args
        .iter()
        .enumerate()
        .map(|(i, x)| {
            let infered_type = infer_type(&args[i], v, fns, fn_src, dyn_libs);

            // Allocate a registers slot for each func arg
            registers.push(NULL);
            Variable {
                name: x.clone(),
                register_id: (registers.len() - 1) as u16,
                infered_type,
            }
        })
        .collect();

    // Get the arg destination ids
    let args_loc = v_temp.iter().map(|x| x.register_id).collect::<Vec<u16>>();

    // Temporarily jump over function to prevent executing it right now
    // This is a placeholder that's modified later on
    output.push(Instr::Jmp(0));
    let jump_idx = output.len() - 1;

    // Record start location for the compiled func body
    let fn_start = output.len();
    let loc = fn_start as u16 + offset;

    let mut arg_types: Vec<usize> = Vec::with_capacity(args.len());
    args.iter().enumerate().for_each(|(i, x)| {
        let infered_type = infer_type(x, v, fns, fn_src, dyn_libs);
        arg_types.push(v.len());
        // 0 => placeholder id, it's never used
        v.push(Variable {
            name: fn_args[i].clone(),
            register_id: 0,
            infered_type,
        });
    });
    let fn_type = track_returns(fn_code, v, fns, fn_src, fn_name, true, dyn_libs);
    let return_type = if !fn_type.is_empty() {
        // If function returns anything, check if it returns the same thing each time
        check_poly(DataType::Poly(Box::from(fn_type)))
    } else {
        // If function doesn't return anything, return nothing
        DataType::Null
    };

    arg_types.iter().for_each(|i| {
        v.remove(*i);
    });

    // Add this func specialization to the func's metadata, storing start location, location of args, and infered arg types
    fns.get_mut(function_id).unwrap().impls.push(FunctionImpl {
        loc,
        args_loc: Box::from(args_loc),
        arg_types: Box::from(infered_arg_types),
        return_type,
    });

    // Compile the function into instructions using local vars
    let parsed = compile_expr(
        fn_code,
        &mut v_temp,
        &ParserData {
            is_parsing_recursive: is_recursive,
            parsing_fn_id: Some(fn_id),
            src: fn_src,
            current_src_file: fn_src_file,
            ..*p
        },
        output.len() as u16,
    );

    if is_recursive {
        let all_written_regs: Vec<u16> = get_tgt_ids(&parsed);

        // For each recursive call, only save registers that are read between that call's return and the end of the function
        for (pos, instr) in parsed.iter().enumerate() {
            if matches!(instr, Instr::CallFuncRecursive(_, _)) {
                // Walk backwards to find this call's SaveFrame and its callsite_id
                let callsite_id = parsed[..pos]
                    .iter()
                    .rev()
                    .find_map(|i| match i {
                        Instr::SaveFrame(_, _, cid) => Some(*cid),
                        _ => None,
                    })
                    .unwrap();

                let mut live_regs: Vec<u16> = Vec::new();
                for after_instr in &parsed[pos + 1..] {
                    for_each_read_reg(*after_instr, |reg| {
                        if all_written_regs.contains(&reg) {
                            live_regs.push(reg);
                        }
                    });
                }
                live_regs.sort_unstable();
                live_regs.dedup();
                *fn_registers.get_mut(callsite_id as usize).unwrap() = live_regs;
            }
        }
    } else {
        fn_registers
            .get_mut(fn_id as usize)
            .unwrap()
            .extend(get_tgt_ids(&parsed));
    }

    output.extend(parsed);

    output.push(Instr::VoidReturn);

    // Fix the placeholder Jmp(0) to skip over the function body
    *output.get_mut(jump_idx).unwrap() = Instr::Jmp((output.len() - fn_start + 1) as u16);
}
