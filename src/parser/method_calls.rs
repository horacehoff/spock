use crate::Data;
use crate::Instr;
use crate::LibFunc;
use crate::Variable;
use crate::check_args;
use crate::check_args_range;
use crate::display::format_expr;
use crate::display::parser_error;
use crate::get_id;
use crate::parser::Expr;
use crate::parser::ParserData;
use crate::type_system::DataType;
use crate::type_system::infer_type;
use inline_colorization::*;

pub fn handle_method_calls(
    output: &mut Vec<Instr>,
    v: &mut Vec<Variable>,
    p: &ParserData,

    obj: &Expr,
    args: &[Expr],
    namespace: &[String],
    start: usize,
    end: usize,
    args_indexes: &[(usize, usize)],
) {
    let (registers, fns, _, instr_src, _, _, src, _, _) = p.destructure();

    let len = namespace.len() - 1;
    let name = namespace[len].as_str();
    // not in use for now
    // let namespace = &namespace[0..len];

    let infered = infer_type(obj, v, fns, src);
    let id = get_id(obj, v, p, output);

    macro_rules! add_args {
        () => {
            for arg in args {
                let arg_id = get_id(&arg, v, p, output);
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
                parser_error(
                    src,
                    start,
                    end,
                    "Invalid type",
                    &format!(
                        "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        $expected_str,
                        infered
                    ),
                    ""
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
                src,
                args_indexes[0].0,
                args_indexes.last().unwrap().1
            )
        };
        ($expected:pat,$expected_str:expr, $args_min:expr,$args_max:expr) => {
            check_type!($expected, $expected_str);
            check_args_range!(
                args,
                $args_min,
                $args_max,
                name,
                src,
                args_indexes[0].0,
                args_indexes.last().unwrap().1
            )
        };
    }

    match name {
        "uppercase" => {
            check!(DataType::String, "String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::Uppercase, id, f_id));
        }
        "lowercase" => {
            check!(DataType::String, "String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::Lowercase, id, f_id));
        }
        "len" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::Len(id, f_id));
        }
        "contains" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 1);

            let arg_infered = infer_type(&args[0], v, fns, src);
            if infered == DataType::String && arg_infered != DataType::String {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered
                    ),
                    "",
                );
            }

            add_args!();

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);

            output.push(Instr::CallLibFunc(LibFunc::Contains, id, f_id));
        }
        "trim" => {
            check!(DataType::String, "String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::Trim, id, f_id));
        }
        "trim_sequence" => {
            check!(DataType::String, "String", 1);

            let infered = infer_type(&args[0], v, fns, src);
            if infered != DataType::String {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    ),
                    "",
                );
            }
            add_args!();

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::TrimSequence, id, f_id));
        }
        "index" => {
            check!(DataType::String | DataType::Array(_), "Array or String", 1);

            let arg_infered = infer_type(&args[0], v, fns, src);
            if let DataType::Array(array_type) = &infered {
                if **array_type != arg_infered {
                    parser_error(
                        src,
                        args_indexes[0].0,
                        args_indexes[0].1,
                        "Invalid type",
                        &format!(
                            "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            array_type, infered, arg_infered
                        ),
                        "",
                    );
                }
            } else if arg_infered != infered {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered
                    ),
                    "",
                );
            }

            add_args!();

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::Index, id, f_id));
            instr_src.push((Instr::CallLibFunc(LibFunc::Index, id, f_id), start, end))
        }
        "is_num" => {
            check!(DataType::String, "String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::IsNum, id, f_id));
        }
        "trim_left" => {
            check!(DataType::String, "String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::TrimLeft, id, f_id));
        }
        "trim_right" => {
            check!(DataType::String, "String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::TrimRight, id, f_id));
        }
        "trim_sequence_left" => {
            check!(DataType::String, "String", 1);

            let infered = infer_type(&args[0], v, fns, src);
            if infered != DataType::String {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    ),
                    "",
                );
            }

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);

            add_args!();
            output.push(Instr::CallLibFunc(LibFunc::TrimSequenceLeft, id, f_id));
        }
        "trim_sequence_right" => {
            check!(DataType::String, "String", 1);

            let infered = infer_type(&args[0], v, fns, src);
            if infered != DataType::String {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    ),
                    "",
                );
            }

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);

            add_args!();
            output.push(Instr::CallLibFunc(LibFunc::TrimSequenceRight, id, f_id));
        }
        "rindex" => {
            check!(DataType::String | DataType::Array(_), "Array or String", 1);

            let arg_infered = infer_type(&args[0], v, fns, src);
            if let DataType::Array(array_type) = &infered {
                if **array_type != arg_infered {
                    parser_error(
                        src,
                        args_indexes[0].0,
                        args_indexes[0].1,
                        "Invalid type",
                        &format!(
                            "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            array_type, infered, arg_infered,
                        ),
                        "",
                    );
                }
            } else if arg_infered != infered {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered,
                    ),
                    "",
                );
            }

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);

            add_args!();
            output.push(Instr::CallLibFunc(LibFunc::RIndex, id, f_id));
            instr_src.push((Instr::CallLibFunc(LibFunc::RIndex, id, f_id), start, end))
        }
        "repeat" => {
            check!(DataType::String | DataType::Array(_), "Array or String", 1);

            let arg_infered = infer_type(&args[0], v, fns, src);
            if arg_infered != DataType::Int {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected Integer, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered,
                    ),
                    "",
                );
            }

            add_args!();

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);

            output.push(Instr::CallLibFunc(LibFunc::Repeat, id, f_id));
        }
        "push" => {
            check!(DataType::Array(_), "Array", 1);

            let arg_infered = infer_type(&args[0], v, fns, src);
            if let DataType::Array(array_type) = &infered
                && **array_type != arg_infered
            {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        array_type, infered, arg_infered
                    ),
                    "",
                );
            }

            let arg_id = get_id(&args[0], v, p, output);
            output.push(Instr::Push(id, arg_id));
        }
        "sqrt" => {
            check!(DataType::Float, "Number", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::SqrtFloat(id, f_id));
        }
        "round" => {
            check!(DataType::Float, "Number", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::Round, id, f_id));
        }
        "abs" => {
            check!(DataType::Float, "Number", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::Abs, id, f_id));
        }
        // io::read
        "read" => {
            check!(DataType::File, "File", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::ReadFile, id, f_id));
            instr_src.push((Instr::CallLibFunc(LibFunc::ReadFile, id, f_id), start, end))
        }
        // io::write
        "write" => {
            check!(DataType::File, "File", 1, 2);

            let len = args.len();
            add_args!();
            if len == 1 {
                registers.push(Data::FALSE);
                output.push(Instr::StoreFuncArg((registers.len() - 1) as u16));
            }

            let f_id = registers.len() as u16;
            registers.push(Data::NULL);

            output.push(Instr::CallLibFunc(LibFunc::WriteFile, id, f_id));
            instr_src.push((Instr::CallLibFunc(LibFunc::WriteFile, id, f_id), start, end))
        }
        "reverse" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 0);
            let f_id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(LibFunc::Reverse, id, f_id));
        }
        "split" => {
            check!(DataType::Array(_) | DataType::String, "Array or String", 1);

            let arg_infered = infer_type(&args[0], v, fns, src);
            if let DataType::Array(array_type) = infered {
                if *array_type != arg_infered {
                    parser_error(
                        src,
                        args_indexes[0].0,
                        args_indexes[0].1,
                        "Invalid type",
                        &format!(
                            "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            array_type, arg_infered
                        ),
                        "",
                    );
                }
            } else if infered != arg_infered {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        arg_infered
                    ),
                    "",
                );
            }

            let arg_id = get_id(&args[0], v, p, output);
            registers.push(Data::NULL);
            output.push(Instr::Split(id, arg_id, (registers.len() - 1) as u16));
        }
        "remove" => {
            check!(DataType::Array(_), "Array", 1);

            let infered = infer_type(&args[0], v, fns, src);
            if infered != DataType::Int {
                parser_error(
                    src,
                    args_indexes[0].0,
                    args_indexes[0].1,
                    "Invalid type",
                    &format!(
                        "Expected Integer, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered
                    ),
                    "",
                );
            }

            let arg_id = get_id(&args[0], v, p, output);
            instr_src.push((Instr::Remove(id, arg_id), start, end));
            output.push(Instr::Remove(id, arg_id));
        }
        _ => {
            parser_error(
                src,
                start,
                end,
                "Unknown function",
                &format!(
                    "Function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} does not exist"
                ),
                "",
            );
        }
    }
}
