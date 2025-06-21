use crate::Data;
use crate::Instr;
use crate::check_args;
use crate::check_args_range;
use crate::display::format_expr;
use crate::get_id;
use crate::parser::Expr;
use crate::parser::ParserData;
use crate::parser_error;
use crate::type_inference::DataType;
use crate::type_inference::infer_type;
use ariadne::*;
use inline_colorization::*;
use internment::Intern;

pub fn handle_method_calls(
    output: &mut Vec<Instr>,
    v: &mut Vec<(Intern<String>, u16)>,
    (var_types, consts, fns, fn_state, arrays, block_id, src, instr_src): ParserData,

    obj: &Expr,
    args: &[Expr],
    namespace: &[String],
    start: usize,
    end: usize,
    args_indexes: &[(usize, usize)],
) {
    macro_rules! parser_data {
        () => {
            (
                var_types, consts, fns, fn_state, arrays, block_id, src, instr_src,
            )
        };
    }

    let len = namespace.len() - 1;
    let name = namespace[len].as_str();
    // not in use for now
    // let namespace = &namespace[0..len];

    let infered = infer_type(obj, var_types, fns, src);
    let id = get_id(obj, v, parser_data!(), output);

    macro_rules! add_args {
        () => {
            for arg in args {
                let arg_id = get_id(&arg, v, parser_data!(), output);
                output.push(Instr::StoreFuncArg(arg_id));
            }
        };
    }

    macro_rules! check_type {
        ($expected:pat,$expected_str:expr) => {
            if !{
                if let DataType::Poly(polytype) = &infered {
                    polytype.iter().all(|x| matches!(x, $expected))
                } else {
                    matches!(infered, $expected)
                }
            } {
                parser_error!(
                    src.0,
                    src.1,
                    start,
                    end,
                    "Invalid type",
                    format_args!(
                        "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        $expected_str,
                        infered
                    )
                );
            }
        };
    }

    macro_rules! check {
        ($expected:pat,$expected_str:expr, $args:expr) => {
            check_type!($expected, $expected_str);
            check_args!(
                args,
                $args,
                name,
                src.0,
                src.1,
                args_indexes[0].0,
                args_indexes.last().unwrap().1
            );
        };
        ($expected:pat,$expected_str:expr, $args_min:expr,$args_max:expr) => {
            check_type!($expected, $expected_str);
            check_args_range!(
                args,
                $args_min,
                $args_max,
                name,
                src.0,
                src.1,
                args_indexes[0].0,
                args_indexes.last().unwrap().1
            );
        };
    }

    match name {
        "uppercase" => {
            check!(DataType::String, "String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(0, id, f_id));
        }
        "lowercase" => {
            check!(DataType::String, "String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(1, id, f_id));
        }
        "len" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Len(id, f_id));
        }
        "contains" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 1);

            let arg_infered = infer_type(&args[0], var_types, fns, src);
            if infered == DataType::String && arg_infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered
                    )
                );
            }

            add_args!();

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(2, id, f_id));
        }
        "trim" => {
            check!(DataType::String, "String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(3, id, f_id));
        }
        "trim_sequence" => {
            check!(DataType::String, "String", 1);

            let infered = infer_type(&args[0], var_types, fns, src);
            if infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    )
                );
            }
            add_args!();

            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(4, id, f_id));
        }
        "index" => {
            check!(DataType::String | DataType::Array(_), "Array or String", 1);

            let arg_infered = infer_type(&args[0], var_types, fns, src);
            if let DataType::Array(array_type) = &infered {
                if **array_type != arg_infered {
                    parser_error!(
                        src.0,
                        src.1,
                        args_indexes[0].0,
                        args_indexes[0].1,
                        "Invalid type",
                        format_args!(
                            "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            array_type, infered, arg_infered
                        )
                    );
                }
            } else if arg_infered != infered {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered
                    )
                );
            }

            add_args!();

            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(5, id, f_id));
            instr_src.push((Instr::CallFunc(5, id, f_id), start, end))
        }
        "is_num" => {
            check!(DataType::String, "String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(6, id, f_id));
        }
        "trim_left" => {
            check!(DataType::String, "String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(7, id, f_id));
        }
        "trim_right" => {
            check!(DataType::String, "String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(8, id, f_id));
        }
        "trim_sequence_left" => {
            check!(DataType::String, "String", 1);

            let infered = infer_type(&args[0], var_types, fns, src);
            if infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    )
                );
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            add_args!();
            output.push(Instr::CallFunc(9, id, f_id));
        }
        "trim_sequence_right" => {
            check!(DataType::String, "String", 1);

            let infered = infer_type(&args[0], var_types, fns, src);
            if infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    )
                );
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            add_args!();
            output.push(Instr::CallFunc(10, id, f_id));
        }
        "rindex" => {
            check!(DataType::String | DataType::Array(_), "Array or String", 1);

            let arg_infered = infer_type(&args[0], var_types, fns, src);
            if let DataType::Array(array_type) = &infered {
                if **array_type != arg_infered {
                    parser_error!(
                        src.0,
                        src.1,
                        args_indexes[0].0,
                        args_indexes[0].1,
                        "Invalid type",
                        format_args!(
                            "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            array_type, infered, arg_infered,
                        )
                    );
                }
            } else if arg_infered != infered {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered,
                    )
                );
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            add_args!();
            output.push(Instr::CallFunc(11, id, f_id));
            instr_src.push((Instr::CallFunc(11, id, f_id), start, end))
        }
        "repeat" => {
            check!(DataType::String | DataType::Array(_), "Array or String", 1);

            let arg_infered = infer_type(&args[0], var_types, fns, src);
            if arg_infered != DataType::Number {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected Number, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered,
                    )
                );
            }

            add_args!();

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(12, id, f_id));
        }
        "push" => {
            check!(DataType::Array(_), "Array", 1);

            let arg_infered = infer_type(&args[0], var_types, fns, src);
            if let DataType::Array(array_type) = &infered {
                if **array_type != arg_infered {
                    parser_error!(
                        src.0,
                        src.1,
                        args_indexes[0].0,
                        args_indexes[0].1,
                        "Invalid type",
                        format_args!(
                            "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            array_type, infered, arg_infered
                        )
                    );
                }
            }

            let arg_id = get_id(&args[0], v, parser_data!(), output);
            output.push(Instr::Push(id, arg_id));
        }
        "sqrt" => {
            check!(DataType::Number, "Number", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Sqrt(id, f_id));
        }
        "round" => {
            check!(DataType::Number, "Number", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(13, id, f_id));
        }
        "abs" => {
            check!(DataType::Number, "Number", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(14, id, f_id));
        }
        // io::read
        "read" => {
            check!(DataType::File, "File", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(15, id, f_id));
            instr_src.push((Instr::CallFunc(15, id, f_id), start, end))
        }
        // io::write
        "write" => {
            check!(DataType::File, "File", 1, 2);

            let len = args.len();
            add_args!();
            if len == 1 {
                consts.push(Data::Bool(false));
                output.push(Instr::StoreFuncArg((consts.len() - 1) as u16));
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(16, id, f_id));
            instr_src.push((Instr::CallFunc(16, id, f_id), start, end))
        }
        "reverse" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 0);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(17, id, f_id));
        }
        "split" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 1);

            let arg_infered = infer_type(&args[0], var_types, fns, src);
            if let DataType::Array(array_type) = infered {
                if *array_type != arg_infered {
                    parser_error!(
                        src.0,
                        src.1,
                        args_indexes[0].0,
                        args_indexes[0].1,
                        "Invalid type",
                        format_args!(
                            "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            array_type, arg_infered
                        )
                    );
                }
            } else if infered != arg_infered {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered
                    )
                );
            }

            let arg_id = get_id(&args[0], v, parser_data!(), output);
            consts.push(Data::Null);
            output.push(Instr::Split(id, arg_id, (consts.len() - 1) as u16));
        }
        "remove" => {
            check!(DataType::Array(_), "Array", 1);

            let infered = infer_type(&args[0], var_types, fns, src);
            if infered != DataType::Number {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected Number, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    )
                );
            }

            let arg_id = get_id(&args[0], v, parser_data!(), output);
            instr_src.push((Instr::Remove(id, arg_id), start, end));
            output.push(Instr::Remove(id, arg_id));
        }
        _ => {
            parser_error!(
                src.0,
                src.1,
                start,
                end,
                "Unknown function",
                format_args!(
                    "Function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} does not exist"
                )
            );
        }
    }
}
