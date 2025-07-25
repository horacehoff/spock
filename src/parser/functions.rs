use crate::Data;
use crate::Instr;
use crate::Num;
use crate::check_args;
use crate::check_args_range;
use crate::display::format_expr;
use crate::get_id;
use crate::parser::Expr;
use crate::parser::ParserData;
use crate::parser::move_to_id;
use crate::parser::parser_to_instr_set;
use crate::parser_error;
use crate::type_inference::DataType;
use crate::type_inference::infer_type;
use ariadne::*;
use inline_colorization::*;
use internment::Intern;

pub fn handle_functions(
    output: &mut Vec<Instr>,
    v: &mut Vec<(Intern<String>, u16)>,
    (var_types, consts, fns, fn_state, arrays, block_id, src, instr_src): ParserData,

    // method call data
    args: &[Expr],
    namespace: &[String],
    start: usize,
    end: usize,
    args_indexes: &[(usize, usize)],
) -> Option<u16> {
    macro_rules! parser_data {
        () => {
            (
                var_types, consts, fns, fn_state, arrays, block_id, src, instr_src,
            )
        };
    }

    let mut check_type = |arg: usize, expected: &[DataType]| {
        let infered = infer_type(&args[arg], var_types, fns, src);
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
                        .iter()
                        .map(|x| x.to_string().to_lowercase())
                        .collect::<Vec<String>>()
                        .join(" or "),
                    infered.to_string()
                )
            );
        }
    };
    let len = namespace.len() - 1;
    // let full_identifier = namespace.join("::");
    let name = namespace[len].as_str();
    let namespace = &namespace[0..len];
    if namespace.is_empty() {
        match name {
            "print" => {
                for arg in args {
                    let id = get_id(arg, v, parser_data!(), output);
                    output.push(Instr::Print(id));
                }
            }
            "type" => {
                check_args!(args, 1, "type", src.0, src.1, start, end);
                let infered = infer_type(&args[0], var_types, fns, src);
                consts.push(Data::String(Intern::from(infered.to_string())));
            }
            "num" => {
                check_args!(args, 1, "num", src.0, src.1, start, end);
                check_type(0, &[DataType::String, DataType::Number]);
                let id = get_id(&args[0], v, parser_data!(), output);
                consts.push(Data::Null);
                instr_src.push((Instr::Num(id, (consts.len() - 1) as u16), start, end));
                output.push(Instr::Num(id, (consts.len() - 1) as u16));
            }
            "str" => {
                check_args!(args, 1, "str", src.0, src.1, start, end);
                let id = get_id(&args[0], v, parser_data!(), output);
                consts.push(Data::Null);
                output.push(Instr::Str(id, (consts.len() - 1) as u16));
            }
            "bool" => {
                check_args!(args, 1, "bool", src.0, src.1, start, end);
                check_type(0, &[DataType::String, DataType::Bool]);
                let id = get_id(&args[0], v, parser_data!(), output);
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
                    get_id(&args[0], v, parser_data!(), output)
                };
                consts.push(Data::Null);
                output.push(Instr::Input(id, (consts.len() - 1) as u16));
            }
            "range" => {
                check_args_range!(args, 1, 2, "range", src.0, src.1, start, end);
                if args.len() == 1 {
                    check_type(0, &[DataType::Number]);
                    let id_x = get_id(&args[0], v, parser_data!(), output);
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
                    let id_x = get_id(&args[0], v, parser_data!(), output);
                    let id_y = get_id(&args[1], v, parser_data!(), output);
                    consts.push(Data::Null);
                    output.push(Instr::Range(id_x, id_y, (consts.len() - 1) as u16));
                }
            }
            "floor" => {
                check_args!(args, 1, "floor", src.0, src.1, start, end);
                check_type(0, &[DataType::Number]);
                let id = get_id(&args[0], v, parser_data!(), output);
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
                let function_id = fns
                    .iter_mut()
                    .position(|(a, _, _, _)| *a == fn_name)
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
                let (_, fn_args, fn_code, fn_data) = &fns[function_id].clone();

                let infered_arg_types = args
                    .iter()
                    .map(|x| infer_type(x, var_types, fns, src))
                    .collect::<Vec<DataType>>();
                dbg!(&infered_arg_types);

                let mut fn_loc_data = if !fn_data.is_empty() {
                    fn_data
                        .iter()
                        .find(|(_, _, t)| t == &infered_arg_types)
                        .cloned()
                } else {
                    None
                };

                let args_len = fn_args.len();
                check_args!(args, args_len, fn_name, src.0, src.1, start, end);

                if fn_data.is_empty() || fn_loc_data.is_none() {
                    println!("CREATING FUNCTION {fn_name}, ARG TYPES ARE {infered_arg_types:?}");

                    let mut vars: Vec<(Intern<String>, u16)> = Vec::new();
                    let mut recorded_types: Vec<usize> = Vec::new();
                    for (i, x) in fn_args.iter().enumerate() {
                        consts.push(Data::Null);
                        vars.push((Intern::from(x.clone()), (consts.len() - 1) as u16));
                        let infered = infer_type(&args[i], var_types, fns, src);
                        recorded_types.push(var_types.len());
                        var_types.push((Intern::from(x.clone()), infered));
                        dbg!(&var_types);
                    }
                    let args_loc = vars.iter().map(|(_, x)| *x).collect::<Vec<u16>>();
                    output.push(Instr::Jmp(0));
                    let jump_idx = output.len() - 1;
                    let fn_start = output.len();
                    let loc = fn_start as u16;

                    fn_loc_data = Some((loc, args_loc.clone(), infered_arg_types.clone()));
                    fns.get_mut(function_id)
                        .unwrap()
                        .3
                        .push((loc, args_loc, infered_arg_types));

                    let parsed = parser_to_instr_set(fn_code, &mut vars, parser_data!());
                    println!("PARSED IS {parsed:?}");
                    output.extend(parsed);
                    output.push(Instr::JmpLoad);
                    *output.get_mut(jump_idx).unwrap() =
                        Instr::Jmp((output.len() - fn_start + 1) as u16);

                    recorded_types.iter().for_each(|x| {
                        if *x < var_types.len() {
                            var_types.remove(*x);
                        }
                    });
                }

                if let Some(fn_args) = &fn_loc_data {
                    let fn_args = fn_args.1.to_vec();
                    for (x, tgt_id) in fn_args.iter().enumerate() {
                        let start_len = output.len();
                        let arg_id = get_id(&args[x], v, parser_data!(), output);
                        println!("MOVING ARG TO {tgt_id}");
                        if output.len() != start_len {
                            move_to_id(output, *tgt_id);
                        } else {
                            output.push(Instr::Mov(arg_id, *tgt_id))
                        }
                    }
                }
                let loc = if let Some(fn_loc) = fn_loc_data {
                    fn_loc.0
                } else {
                    unreachable!()
                };

                println!("LOC IS {loc:?}");
                println!("OUTP LEN IS {}", output.len());
                println!("OUTPUT IS {output:?}");
                consts.push(Data::Null);
                output.push(Instr::JmpSave(loc, (consts.len() - 1) as u16));
                return Some((consts.len() - 1) as u16);
            }
        }
    } else if *namespace == ["io"] {
        match name {
            "open" => {
                check_args_range!(args, 1, 2, "open", src.0, src.1, start, end);
                consts.push(Data::Null);
                let arg_id = get_id(&args[0], v, parser_data!(), output);

                let second_arg = if args.len() == 1 {
                    consts.push(Data::Bool(false));
                    (consts.len() - 1) as u16
                } else {
                    get_id(&args[1], v, parser_data!(), output)
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
                let arg_id = get_id(&args[0], v, parser_data!(), output);
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
    None
}
