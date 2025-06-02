use crate::Data;
use crate::Function;
use crate::FunctionState;
use crate::Instr;
use crate::Num;
use crate::check_args;
use crate::check_args_range;
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

pub fn handle_method_calls(
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
    obj: &Expr,
    args: &[Expr],
    namespace: &[String],
    start: usize,
    end: usize,
    args_indexes: &[(usize, usize)],
) {
    let len = namespace.len() - 1;
    let name = namespace[len].as_str();
    // not in use for now
    let namespace = &namespace[0..len];

    let infered = infer_type(obj, var_types, fns);
    let id = get_id(
        obj, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
    );

    macro_rules! add_args {
        () => {
            for arg in args {
                let arg_id = get_id(
                    &arg, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
                );
                output.push(Instr::StoreFuncArg(arg_id));
            }
        };
    }

    macro_rules! check_obj_type {
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
                        format_datatype(infered)
                    )
                );
            }
        };
    }

    match name {
        "uppercase" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 0, "uppercase", src.0, src.1, start, end);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(0, id, f_id));
            // instr_src.push((Instr::CallFunc(0, id, f_id), start,end));
        }
        "lowercase" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 0, "lowercase", src.0, src.1, start, end);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(1, id, f_id));
            instr_src.push((Instr::CallFunc(1, id, f_id), start, end))
        }
        "len" => {
            check_obj_type!(DataType::Array(_) | DataType::String, "Array or String");
            check_args!(args, 0, "len", src.0, src.1, start, end);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            instr_src.push((Instr::Len(id, f_id), start, end));
            output.push(Instr::Len(id, f_id));
        }
        "contains" => {
            check_obj_type!(DataType::Array(_) | DataType::String, "Array or String");
            check_args!(args, 1, "contains", src.0, src.1, start, end);

            let arg_infered = infer_type(&args[0], var_types, fns);
            if infered == DataType::String && arg_infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        format_datatype(arg_infered)
                    )
                );
            }

            add_args!();

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(2, id, f_id));
            instr_src.push((Instr::CallFunc(2, id, f_id), start, end))
        }
        "trim" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 0, "trim", src.0, src.1, start, end);

            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(3, id, f_id));
            instr_src.push((Instr::CallFunc(3, id, f_id), start, end))
        }
        "trim_sequence" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 1, "trim_sequence", src.0, src.1, start, end);

            let infered = infer_type(&args[0], var_types, fns);
            if infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        format_datatype(infered)
                    )
                );
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            add_args!();
            output.push(Instr::CallFunc(4, id, f_id));
            instr_src.push((Instr::CallFunc(4, id, f_id), start, end))
        }
        "index" => {
            check_obj_type!(DataType::String | DataType::Array(_), "Array or String");
            check_args!(args, 1, "index", src.0, src.1, start, end);

            let arg_infered = infer_type(&args[0], var_types, fns);
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
                            format_datatype(*array_type.clone()),
                            format_datatype(infered),
                            format_datatype(arg_infered)
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
                        format_datatype(arg_infered)
                    )
                );
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            add_args!();
            output.push(Instr::CallFunc(5, id, f_id));
            instr_src.push((Instr::CallFunc(5, id, f_id), start, end))
        }
        "is_num" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 0, "is_num", src.0, src.1, start, end);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(6, id, f_id));
            instr_src.push((Instr::CallFunc(6, id, f_id), start, end))
        }
        "trim_left" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 0, "trim_left", src.0, src.1, start, end);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(7, id, f_id));
            instr_src.push((Instr::CallFunc(7, id, f_id), start, end))
        }
        "trim_right" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 0, "trim_right", src.0, src.1, start, end);
            let f_id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::CallFunc(8, id, f_id));
            instr_src.push((Instr::CallFunc(8, id, f_id), start, end))
        }
        "trim_sequence_left" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 1, "trim_sequence_left", src.0, src.1, start, end);

            let infered = infer_type(&args[0], var_types, fns);
            if infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        format_datatype(infered)
                    )
                );
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            add_args!();
            output.push(Instr::CallFunc(9, id, f_id));
            instr_src.push((Instr::CallFunc(9, id, f_id), start, end))
        }
        "trim_sequence_right" => {
            check_obj_type!(DataType::String, "String");
            check_args!(args, 1, "trim_sequence_right", src.0, src.1, start, end);

            let infered = infer_type(&args[0], var_types, fns);
            if infered != DataType::String {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        format_datatype(infered)
                    )
                );
            }

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            add_args!();
            output.push(Instr::CallFunc(10, id, f_id));
            instr_src.push((Instr::CallFunc(10, id, f_id), start, end))
        }
        "rindex" => {
            check_obj_type!(DataType::String | DataType::Array(_), "Array or String");
            check_args!(args, 1, "rindex", src.0, src.1, start, end);

            let arg_infered = infer_type(&args[0], var_types, fns);
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
                            format_datatype(*array_type.clone()),
                            format_datatype(infered),
                            format_datatype(arg_infered)
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
                        format_datatype(arg_infered)
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
            check_obj_type!(DataType::String | DataType::Array(_), "Array or String");

            check_args!(args, 1, "repeat", src.0, src.1, start, end);

            let arg_infered = infer_type(&args[0], var_types, fns);
            if arg_infered != DataType::Number {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected Number, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        format_datatype(arg_infered)
                    )
                );
            }

            add_args!();

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(12, id, f_id));
            instr_src.push((Instr::CallFunc(12, id, f_id), start, end))
        }
        "push" => {
            check_obj_type!(DataType::Array(_), "Array");

            check_args!(args, 1, "push", src.0, src.1, start, end);

            let arg_infered = infer_type(&args[0], var_types, fns);
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
                            format_datatype(*array_type.clone()),
                            format_datatype(infered),
                            format_datatype(arg_infered)
                        )
                    );
                }
            }

            let arg_id = get_id(
                &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );

            instr_src.push((Instr::Push(id, arg_id), start, end));
            output.push(Instr::Push(id, arg_id));
        }
        "sqrt" => {
            check_obj_type!(DataType::Number, "Number");
            check_args!(args, 0, "sqrt", src.0, src.1, start, end);

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            instr_src.push((Instr::Sqrt(id, f_id), start, end));
            output.push(Instr::Sqrt(id, f_id));
        }
        "round" => {
            check_obj_type!(DataType::Number, "Number");
            check_args!(args, 0, "round", src.0, src.1, start, end);

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(13, id, f_id));
            instr_src.push((Instr::CallFunc(13, id, f_id), start, end))
        }
        "abs" => {
            check_obj_type!(DataType::Number, "Number");
            check_args!(args, 0, "abs", src.0, src.1, start, end);

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(14, id, f_id));
            instr_src.push((Instr::CallFunc(14, id, f_id), start, end))
        }
        // io::read
        "read" => {
            check_obj_type!(DataType::File, "File");
            check_args!(args, 0, "read", src.0, src.1, start, end);

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(15, id, f_id));
            instr_src.push((Instr::CallFunc(15, id, f_id), start, end))
        }
        // io::write
        "write" => {
            check_obj_type!(DataType::File, "File");
            check_args_range!(args, 1, 2, "write", src.0, src.1, start, end);

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
            check_obj_type!(DataType::Array(_) | DataType::String, "Array or String");

            check_args!(args, 0, "reverse", src.0, src.1, start, end);

            let f_id = consts.len() as u16;
            consts.push(Data::Null);

            output.push(Instr::CallFunc(17, id, f_id));
            instr_src.push((Instr::CallFunc(17, id, f_id), start, end))
        }
        "split" => {
            check_obj_type!(DataType::Array(_) | DataType::String, "Array or String");

            check_args!(args, 1, "split", src.0, src.1, start, end);

            let arg_infered = infer_type(&args[0], var_types, fns);
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
                            format_datatype(*array_type),
                            format_datatype(arg_infered)
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
                        format_datatype(arg_infered)
                    )
                );
            }

            let arg_id = get_id(
                &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            consts.push(Data::Null);
            instr_src.push((
                Instr::Split(id, arg_id, (consts.len() - 1) as u16),
                start,
                end,
            ));
            output.push(Instr::Split(id, arg_id, (consts.len() - 1) as u16));
        }
        "remove" => {
            check_obj_type!(DataType::Array(_), "Array");
            check_args!(args, 1, "remove", src.0, src.1, start, end);

            let infered = infer_type(&args[0], var_types, fns);
            if infered != DataType::Number {
                parser_error!(
                    src.0,
                    src.1,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    format_args!(
                        "Expected Number, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        format_datatype(infered)
                    )
                );
            }

            let arg_id = get_id(
                &args[0], v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
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
