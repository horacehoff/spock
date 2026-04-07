use crate::Instr;
use crate::check_args;
use crate::check_args_range;
use crate::data::FALSE;
use crate::data::NULL;
use crate::debug;
use crate::display::format_expr;
use crate::errors::parser_error;
use crate::get_id;
use crate::instr::LibFunc;
use crate::parser::Expr;
use crate::parser::get_tgt_ids;
use crate::parser::move_to_id;
use crate::parser::parser_to_instr_set;
use crate::parser_data::FnSignature;
use crate::parser_data::FunctionImpl;
use crate::parser_data::ParserData;
use crate::parser_data::Variable;
use crate::type_system::DataType;
use crate::type_system::infer_type;
use inline_colorization::*;
use smol_str::SmolStr;
use std::slice;

pub fn handle_functions(
    output: &mut Vec<Instr>,
    v: &mut Vec<Variable>,
    p: &ParserData,

    // method call data
    args: &[Expr],
    namespace: &[SmolStr],
    start: usize,
    end: usize,
    args_indexes: &[(usize, usize)],
) -> Option<u16> {
    let (
        registers,
        fns,
        _,
        instr_src,
        fn_registers,
        _,
        src,
        _,
        _,
        dyn_libs,
        allocated_arg_count,
        allocated_call_depth,
    ) = p.destructure();

    let mut check_type = |arg: usize, expected: &[DataType]| {
        let infered = infer_type(&args[arg], v, fns, src, p);
        if !{
            if let DataType::Poly(polytype) = &infered {
                polytype.iter().all(|x| expected.contains(x))
            } else {
                expected.contains(&infered)
            }
        } {
            parser_error(
                src,
                args_indexes[arg].0,
                args_indexes[arg].1,
                "Invalid type",
                format_args!(
                    "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                    expected
                        .iter()
                        .map(|x| x.to_string().to_lowercase())
                        .collect::<Vec<String>>()
                        .join(" or "),
                    infered
                ),
                None,
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
                    let id = get_id(arg, v, p, output, None, false);
                    output.push(Instr::Print(id));
                }
            }
            "type" => {
                check_args!(args, 1, "type", src, start, end);
                let infered = infer_type(&args[0], v, fns, src, p);
                registers.push(infered.to_string().into());
            }
            "float" => {
                check_args!(args, 1, "float", src, start, end);
                check_type(0, &[DataType::String, DataType::Int]);
                let id = get_id(&args[0], v, p, output, None, false);
                registers.push(NULL);
                instr_src.push((
                    Instr::CallLibFunc(LibFunc::Float, id, (registers.len() - 1) as u16),
                    start,
                    end,
                ));
                output.push(Instr::CallLibFunc(
                    LibFunc::Float,
                    id,
                    (registers.len() - 1) as u16,
                ));
            }
            "int" => {
                check_args!(args, 1, "int", src, start, end);
                check_type(0, &[DataType::String, DataType::Float]);
                let id = get_id(&args[0], v, p, output, None, false);
                registers.push(NULL);
                instr_src.push((
                    Instr::CallLibFunc(LibFunc::Int, id, (registers.len() - 1) as u16),
                    start,
                    end,
                ));
                output.push(Instr::CallLibFunc(
                    LibFunc::Int,
                    id,
                    (registers.len() - 1) as u16,
                ));
            }
            "str" => {
                check_args!(args, 1, "str", src, start, end);
                let id = get_id(&args[0], v, p, output, None, false);
                registers.push(NULL);
                output.push(Instr::CallLibFunc(
                    LibFunc::Str,
                    id,
                    (registers.len() - 1) as u16,
                ));
            }
            "bool" => {
                check_args!(args, 1, "bool", src, start, end);
                check_type(0, &[DataType::String, DataType::Bool]);
                let id = get_id(&args[0], v, p, output, None, false);
                registers.push(NULL);
                instr_src.push((
                    Instr::CallLibFunc(LibFunc::Bool, id, (registers.len() - 1) as u16),
                    start,
                    end,
                ));
                output.push(Instr::CallLibFunc(
                    LibFunc::Bool,
                    id,
                    (registers.len() - 1) as u16,
                ));
            }
            "input" => {
                check_args_range!(args, 0, 1, "input", src, start, end);
                check_type(0, &[DataType::String]);
                let id = if args.is_empty() {
                    registers.push(String::new().into());
                    (registers.len() - 1) as u16
                } else {
                    get_id(&args[0], v, p, output, None, false)
                };
                registers.push(NULL);
                output.push(Instr::CallLibFunc(
                    LibFunc::Input,
                    id,
                    (registers.len() - 1) as u16,
                ));
            }
            "range" => {
                check_args_range!(args, 1, 2, "range", src, start, end);
                if args.len() == 1 {
                    check_type(0, &[DataType::Int]);
                    let id_x = get_id(&args[0], v, p, output, None, false);
                    registers.push(0.into());
                    registers.push(NULL);
                    output.push(Instr::Range(
                        (registers.len() - 2) as u16,
                        id_x,
                        (registers.len() - 1) as u16,
                    ));
                } else {
                    check_type(0, &[DataType::Int]);
                    check_type(1, &[DataType::Int]);
                    let id_x = get_id(&args[0], v, p, output, None, false);
                    let id_y = get_id(&args[1], v, p, output, None, false);
                    registers.push(NULL);
                    output.push(Instr::Range(id_x, id_y, (registers.len() - 1) as u16));
                }
            }
            "floor" => {
                check_args!(args, 1, "floor", src, start, end);
                check_type(0, &[DataType::Float]);
                let id = get_id(&args[0], v, p, output, None, false);
                registers.push(NULL);
                output.push(Instr::CallLibFunc(
                    LibFunc::Floor,
                    id,
                    (registers.len() - 1) as u16,
                ));
            }
            "the_answer" => {
                check_args!(args, 0, "the_answer", src, start, end);
                registers.push(NULL);
                output.push(Instr::CallLibFunc(
                    LibFunc::TheAnswer,
                    0,
                    (registers.len() - 1) as u16,
                ));
            }
            fn_name => {
                // Lookup function by name in function registry
                // Registry is (fn_name, fn_args, fn_code, fn_data (per implementation: loc, args_loc, arg_types) )
                let function_id = fns
                    .iter_mut()
                    .position(|func| func.name == fn_name)
                    .unwrap_or_else(|| {
                        parser_error(
                            src,
                            start,
                            end,
                            "Unknown function",
                            format_args!(
                                "Function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} does not exist or has not been declared yet"
                            ),
                            None
                        );
                    });
                // Retrieve list of args, code, and function data (loc, args_loc, arg_types)
                let fn_id = fns[function_id].id;
                let is_recursive = fns[function_id].is_recursive;
                let fn_args = &fns[function_id].args;
                let fn_code = &fns[function_id].code;
                let fn_impls = &fns[function_id].impls;
                let fn_returns_void = &fns[function_id].returns_void;

                let args_len = fn_args.len();
                check_args!(args, args_len, fn_name, src, start, end);

                // Infer arg types
                let infered_arg_types = args
                    .iter()
                    .map(|x| infer_type(x, v, fns, src, p))
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
                    );
                    fns.get(function_id).unwrap().impls.last().unwrap()
                };

                if is_recursive {
                    output.push(Instr::SaveFrame(0, 0, 0));
                    *allocated_call_depth += 2;
                }
                let saveframe_loc = output.len() - 1;
                // Move evaluated call args into the expected arg slots
                // if let Some(fn_args) = &fn_loc_data {
                let fn_args = fn_loc_data.args_loc.to_vec();
                for (x, tgt_id) in fn_args.iter().enumerate() {
                    let start_len = output.len();

                    let arg_id = get_id(&args[x], v, p, output, Some(*tgt_id), false);
                    debug!("MOVING ARG TO {tgt_id}");
                    // If get_id emitted code, adjust arg dest with move_to_id
                    if output.len() != start_len {
                        move_to_id(output, *tgt_id);
                    } else {
                        // Else just directly move the arg to the expected slot
                        output.push(Instr::Mov(arg_id, *tgt_id))
                    }
                }
                fn_registers
                    .get_mut(fn_id as usize)
                    .unwrap()
                    .extend(get_tgt_ids(&output[saveframe_loc..]));
                let loc = fn_loc_data.loc;

                let return_register_id = if !*fn_returns_void {
                    registers.push(NULL);
                    (registers.len() - 1) as u16
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
                        (registers.len() - 1) as u16,
                        fn_id,
                    );
                }

                // Return return slot address
                return Some((registers.len() - 1) as u16);
            }
        }
    } else if *namespace == ["io"] {
        match name {
            "open" => {
                check_args_range!(args, 1, 2, "open", src, start, end);
                registers.push(NULL);
                let arg_id = get_id(&args[0], v, p, output, None, false);

                let second_arg = if args.len() == 1 {
                    registers.push(FALSE);
                    (registers.len() - 1) as u16
                } else {
                    get_id(&args[1], v, p, output, None, false)
                };

                instr_src.push((
                    Instr::IoOpen(arg_id, (registers.len() - 1) as u16, second_arg),
                    start,
                    end,
                ));
                output.push(Instr::IoOpen(
                    arg_id,
                    (registers.len() - 1) as u16,
                    second_arg,
                ));
            }
            "delete" => {
                check_args!(args, 1, "delete", src, start, end);
                let arg_id = get_id(&args[0], v, p, output, None, false);
                instr_src.push((Instr::IoDelete(arg_id), start, end));
                output.push(Instr::IoDelete(arg_id));
            }
            _ => {
                parser_error(
                    src,
                    start,
                    end,
                    "Unknown function in namespace",
                    format_args!(
                        "Namespace {color_bright_blue}{style_bold}{}{color_reset}{style_reset} does not contain function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset}",
                        namespace
                            .iter()
                            .map(|x| (*x).to_string())
                            .collect::<Vec<String>>()
                            .join("::")
                    ),
                    None,
                );
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
            check_args!(args, fn_args.len(), fn_name, src, start, end);
            for (i, a) in fn_args.iter().enumerate() {
                check_type(i, slice::from_ref(a));
            }

            for arg in args {
                let arg_id = get_id(arg, v, p, output, None, false);
                output.push(Instr::StoreFuncArg(arg_id));
                *allocated_arg_count += 1;
            }

            let register_id = if fn_return_type == &DataType::Null {
                0u16
            } else {
                registers.push(NULL);
                (registers.len() - 1) as u16
            };
            output.push(Instr::CallDynLibFunc(*id, register_id));
            instr_src.push((Instr::CallDynLibFunc(*id, register_id), start, end));
        }
    } else {
        parser_error(
            src,
            start,
            end,
            "Unknown namespace",
            format_args!(
                "Namespace {color_bright_blue}{style_bold}{}{color_reset}{style_reset} does not exist",
                namespace
                    .iter()
                    .map(|x| (*x).to_string())
                    .collect::<Vec<String>>()
                    .join("::")
            ),
            None,
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
) {
    let (registers, fns, _, _, fn_registers, _, src, _, _, _, _, _) = p.destructure();
    debug!("CREATING FUNCTION {fn_name}, ARG TYPES ARE {infered_arg_types:?}");

    // Local vector vars and recorded_types to allow the inner body to type-check correctly
    let mut v_temp: Vec<Variable> = fn_args
        .iter()
        .enumerate()
        .map(|(i, x)| {
            let infered_type = infer_type(&args[i], v, fns, src, p);

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
    let loc = fn_start as u16;

    // Add this func specialization to the func's metadata, storing start location, location of args, and infered arg types
    fns.get_mut(function_id).unwrap().impls.push(FunctionImpl {
        loc,
        args_loc: Box::from(args_loc),
        arg_types: Box::from(infered_arg_types),
    });

    // Compile the function into instructions using local vars
    let parsed = parser_to_instr_set(
        fn_code,
        &mut v_temp,
        &ParserData {
            is_parsing_recursive: true,
            parsing_fn_id: Some(fn_id),
            ..*p
        },
    );
    let fn_temp_registers = get_tgt_ids(&parsed);
    fn_registers
        .get_mut(fn_id as usize)
        .unwrap()
        .extend(fn_temp_registers);
    // fns.get_mut(function_id).unwrap().5.extend(fn_registers);

    output.extend(parsed);

    // JmpLoad to return to the call site (which will also return a value if necessary)
    output.push(Instr::VoidReturn);

    // Fix the placeholder Jmp(0) to skip over the function body
    *output.get_mut(jump_idx).unwrap() = Instr::Jmp((output.len() - fn_start + 1) as u16);
}
