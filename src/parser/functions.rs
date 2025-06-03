use crate::Data;
use crate::Function;
use crate::FunctionState;
use crate::Instr;
use crate::Num;
use crate::check_args;
use crate::check_args_range;
use crate::display::format_expr;
use crate::get_id;
use crate::parser::Expr;
use crate::parser::move_to_id;
use crate::parser::parser_to_instr_set;
use crate::parser_error;
use crate::type_inference::DataType;
use crate::type_inference::infer_type;
use crate::util::format_datatype;
use ariadne::*;
use inline_colorization::*;
use internment::Intern;
use slab::Slab;

pub fn handle_functions(
    output: &mut Vec<Instr>,
    v: &mut Vec<(Intern<String>, u16)>,
    var_types: &mut Vec<(Intern<String>, DataType)>,
    consts: &mut Vec<Data>,
    fns: &mut Vec<Function>,
    fn_state: Option<&FunctionState>,
    // arrays
    arrs: &mut Slab<Vec<Data>>,
    id: u16,
    src: (&str, &str),
    instr_src: &mut Vec<(Instr, usize, usize)>,
    args: &[Expr],
    namespace: &[String],
    start: usize,
    end: usize,
    args_indexes: &[(usize, usize)],
) {
    let check_type = |arg: usize, expected: &[DataType]| {
        let infered = infer_type(&args[arg], var_types, fns);
        if !{
            if let DataType::Poly(polytype) = &infered {
                polytype.iter().all(|x| expected.contains(x))
            } else {
                expected.contains(&infered)
            }
        } {
            parser_error!(
                src.0,
                src.1,
                args_indexes[arg].0,
                args_indexes[arg].1,
                "Invalid type",
                format_args!(
                    "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                    expected
                        .into_iter()
                        .map(|x| format_datatype(x.clone()).to_lowercase())
                        .collect::<Vec<String>>()
                        .join(" or "),
                    format_datatype(infered)
                )
            );
        }
    };
    let len = namespace.len() - 1;
    let full_identifier = namespace.join("::");
    let name = namespace[len].as_str();
    let namespace = &namespace[0..len];
    if namespace.is_empty() {
        match name {
            "print" => {
                for arg in args {
                    let id = get_id(
                        arg, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                    );
                    output.push(Instr::Print(id));
                }
            }
            "type" => {
                check_args!(args, 1, "type", src.0, src.1, start, end);
                let infered = infer_type(&args[0], var_types, fns);
                consts.push(Data::String(Intern::from(format_datatype(infered))));
            }
            "num" => {
                check_args!(args, 1, "num", src.0, src.1, start, end);
                check_type(0, &[DataType::String, DataType::Number]);
                let id = get_id(
                    &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                );
                consts.push(Data::Null);
                instr_src.push((Instr::Num(id, (consts.len() - 1) as u16), start, end));
                output.push(Instr::Num(id, (consts.len() - 1) as u16));
            }
            "str" => {
                check_args!(args, 1, "str", src.0, src.1, start, end);
                let id = get_id(
                    &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                );
                consts.push(Data::Null);
                output.push(Instr::Str(id, (consts.len() - 1) as u16));
            }
            "bool" => {
                check_args!(args, 1, "bool", src.0, src.1, start, end);
                check_type(0, &[DataType::String, DataType::Bool]);
                let id = get_id(
                    &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                );
                consts.push(Data::Null);
                instr_src.push((Instr::Bool(id, (consts.len() - 1) as u16), start, end));
                output.push(Instr::Bool(id, (consts.len() - 1) as u16));
            }
            "input" => {
                check_args_range!(args, 0, 1, "input", src.0, src.1, start, end);
                check_type(0, &[DataType::String]);
                let id = if args.is_empty() {
                    consts.push(Data::String(Intern::from(String::new())));
                    (consts.len() - 1) as u16
                } else {
                    get_id(
                        &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src,
                        instr_src,
                    )
                };
                consts.push(Data::Null);
                output.push(Instr::Input(id, (consts.len() - 1) as u16));
            }
            "range" => {
                check_args_range!(args, 1, 2, "range", src.0, src.1, start, end);
                if args.len() == 1 {
                    check_type(0, &[DataType::Number]);
                    let id_x = get_id(
                        &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src,
                        instr_src,
                    );
                    consts.push(Data::Number(0.0 as Num));
                    consts.push(Data::Null);
                    output.push(Instr::Range(
                        (consts.len() - 2) as u16,
                        id_x,
                        (consts.len() - 1) as u16,
                    ));
                } else {
                    check_type(0, &[DataType::Number]);
                    check_type(1, &[DataType::Number]);
                    let id_x = get_id(
                        &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src,
                        instr_src,
                    );
                    let id_y = get_id(
                        &args[1], v, var_types, consts, output, fns, arrs, fn_state, id, src,
                        instr_src,
                    );
                    consts.push(Data::Null);
                    output.push(Instr::Range(id_x, id_y, (consts.len() - 1) as u16));
                }
            }
            "floor" => {
                check_args!(args, 1, "floor", src.0, src.1, start, end);
                check_type(0, &[DataType::Number]);
                let id = get_id(
                    &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                );
                consts.push(Data::Null);
                instr_src.push((Instr::Num(id, (consts.len() - 1) as u16), start, end));
                output.push(Instr::Num(id, (consts.len() - 1) as u16));
            }
            "the_answer" => {
                check_args!(args, 0, "the_answer", src.0, src.1, start, end);
                consts.push(Data::Null);
                output.push(Instr::TheAnswer((consts.len() - 1) as u16));
            }
            fn_name => {
                let mut fns_clone = fns.clone();
                let function_id = fns
                    .iter_mut()
                    .position(|(a, _, _, _, _)| *a == fn_name)
                    .unwrap_or_else(|| {
                        parser_error!(
                            src.0,
                            src.1,
                            start,
                            end,
                            "Unknown function",
                            format_args!(
                                "Function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} does not exist or has not been declared yet"
                            )
                        );
                    });
                let (_, fn_args, fn_code, fn_loc, fn_args_loc) = &fns[function_id].clone();
                let args_len = fn_args.len();
                check_args!(args, args_len, fn_name, src.0, src.1, start, end);

                if fn_loc.is_none() {
                    let mut vars: Vec<(Intern<String>, u16)> = Vec::new();
                    for (i, x) in fn_args.iter().enumerate() {
                        consts.push(Data::Null);
                        vars.push((Intern::from(x.clone()), (consts.len() - 1) as u16));
                        let infered = infer_type(&args[i], var_types, fns);
                        var_types.push((Intern::from(x.clone()), infered));
                    }
                    fns.get_mut(function_id).unwrap().4 =
                        Some(vars.iter().map(|(_, x)| *x).collect::<Vec<u16>>());
                    output.push(Instr::Jmp(0, false));
                    let jump_idx = output.len() - 1;
                    let fn_start = output.len();
                    fns.get_mut(function_id).unwrap().3 = Some(fn_start as u16);
                    let mut parsed = parser_to_instr_set(
                        fn_code, &mut vars, var_types, consts, &mut *fns, fn_state, arrs, id, src,
                        instr_src,
                    );
                    let len = parsed.len();
                    println!("PARSED IS {parsed:?}");
                    parsed.iter_mut().for_each(|x| {
                        println!("ENCOUNTERED {x:?}");
                        if let Instr::JmpSave(size, neg) = x {
                            *size += (output.len() + len - 3) as u16;
                        }
                    });
                    output.extend(parsed);
                    output.push(Instr::JmpLoad(false));
                    *output.get_mut(jump_idx).unwrap() =
                        Instr::Jmp((output.len() - fn_start + 1) as u16, false)
                }

                if let Some(fn_args) = fns[function_id].4.clone() {
                    for (x, tgt_id) in fn_args.iter().enumerate() {
                        let start_len = output.len();
                        let arg_id = get_id(
                            &args[x], v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src,
                        );

                        if output.len() != start_len {
                            move_to_id(output, *tgt_id);
                        } else {
                            output.push(Instr::Mov(arg_id, *tgt_id))
                        }
                    }
                }
                let loc = if let Some(fn_loc) = &fns[function_id].3 {
                    *fn_loc
                } else {
                    unreachable!()
                };

                println!("LOC IS {loc:?}");
                println!("OUTP LEN IS {}", output.len());
                println!("OUTPUT IS {output:?}");
                output.push(Instr::JmpSave((output.len() as u16) - loc, true));
            }
        }
    } else if *namespace == ["io"] {
        match name {
            "open" => {
                check_args_range!(args, 1, 2, "open", src.0, src.1, start, end);
                consts.push(Data::Null);
                let arg_id = get_id(
                    &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                );

                let second_arg = if args.len() == 1 {
                    consts.push(Data::Bool(false));
                    (consts.len() - 1) as u16
                } else {
                    get_id(
                        &args[1], v, var_types, consts, output, fns, arrs, fn_state, id, src,
                        instr_src,
                    )
                };

                instr_src.push((
                    Instr::IoOpen(arg_id, (consts.len() - 1) as u16, second_arg),
                    start,
                    end,
                ));
                output.push(Instr::IoOpen(arg_id, (consts.len() - 1) as u16, second_arg));
            }
            "delete" => {
                check_args!(args, 1, "delete", src.0, src.1, start, end);
                let arg_id = get_id(
                    &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                );
                instr_src.push((Instr::IoDelete(arg_id), start, end));
                output.push(Instr::IoDelete(arg_id));
            }
            _ => {
                parser_error!(
                    src.0,
                    src.1,
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
                    )
                );
            }
        }
    } else {
        parser_error!(
            src.0,
            src.1,
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
            )
        );
    }
}
