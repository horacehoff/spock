use crate::namespaces::namespace_functions;
use crate::parser::{parse_code, ParserInstr};
use crate::util::{error, get_printable_form, split_vec_box};
use crate::{
    assert_args_number, builtin_functions, error_msg, get_printable_type, log, process_lines,
    process_stack,
};
use branches::likely;
// use smartstring::alias::String;

// #[unroll_for_loops]
// #[inline(always)]
pub fn preprocess(
    element: &ParserInstr,
    variables: &Vec<(String, ParserInstr)>,
    functions: &[(String, &[String], &[ParserInstr])],
) -> ParserInstr {
    match element {
        ParserInstr::FunctionCall(ref block) => {
            // replace function call by its result (return value)
            let args: Vec<ParserInstr> = split_vec_box(&block.args, ParserInstr::Separator)
                .iter()
                .map(|x| process_stack(x, variables, functions))
                .collect();
            let matched = builtin_functions(&block.name, &args);
            // check if function is a built-in function, else search it among user-defined functions
            if matched.1 {
                return matched.0;
            } else if block.name == "executeline" {
                assert_args_number!("executeline", args.len(), 1);
                if let ParserInstr::String(line) = &args[0] {
                    return process_stack(&parse_code(line)[0], variables, functions);
                }
                error(&format!("Cannot execute {:?}", &args[0]), "");
            } else if block.name == "int" {
                assert_args_number!("int", args.len(), 1);
                if let ParserInstr::String(str) = &args[0] {
                    return ParserInstr::Integer(str.parse::<i64>().unwrap_or_else(|_| {
                        error(&format!("Cannot convert String '{str}' to Integer",), "");
                        std::process::exit(1)
                    }));
                } else if let ParserInstr::Float(float) = &args[0] {
                    return ParserInstr::Integer(float.round() as i64);
                }
                error(
                    &format!(
                        "Cannot convert {} to Integer",
                        get_printable_type!(&args[0])
                    ),
                    "",
                );
            } else if block.name == "str" {
                assert_args_number!("str", args.len(), 1);
                if let ParserInstr::Integer(int) = &args[0] {
                    return ParserInstr::String(int.to_string().parse().unwrap());
                } else if let ParserInstr::Float(float) = &args[0] {
                    return ParserInstr::String(float.to_string().parse().unwrap());
                } else if let ParserInstr::Bool(boolean) = &args[0] {
                    return ParserInstr::String(if *boolean {
                        "true".parse().unwrap()
                    } else {
                        "false".parse().unwrap()
                    });
                } else if let ParserInstr::Array(_, _, false) = &args[0] {
                    return ParserInstr::String(get_printable_form(&args[0]));
                }
                error(
                    &format!("Cannot convert {} to String", get_printable_type!(&args[0])),
                    "",
                );
            } else if block.name == "float" {
                assert_args_number!("float", args.len(), 1);
                if let ParserInstr::String(str) = &args[0] {
                    return ParserInstr::Float(str.parse::<f64>().unwrap_or_else(|_| {
                        error(&format!("Cannot convert String '{str}' to Float",), "");
                        std::process::exit(1)
                    }));
                } else if let ParserInstr::Integer(int) = &args[0] {
                    return ParserInstr::Float(*int as f64);
                }
                error(
                    &format!("Cannot convert {} to Float", get_printable_type!(&args[0])),
                    "",
                );
            }

            let target_function: &(String, &[String], &[ParserInstr]) = functions
                .iter()
                .find(|func| func.0 == *block.name)
                .unwrap_or_else(|| {
                    error(&format!("Unknown function '{}'", block.name), "");
                    std::process::exit(1)
                });
            assert_args_number!(block.name, args.len(), target_function.1.len());
            let mut target_args: Vec<(String, ParserInstr)> = target_function
                .1
                .iter()
                .enumerate()
                .map(|(i, arg)| (arg.parse().unwrap(), args[i].clone()))
                .collect();
            return process_lines(target_function.2, &mut target_args, functions);
        }
        // Types::NamespaceFunctionCall(ref namespace, ref y, ref z) => {
        ParserInstr::NamespaceFunctionCall(ref block) => {
            // execute "namespace functions"
            let args: Vec<ParserInstr> = split_vec_box(&block.args, ParserInstr::Separator)
                .iter()
                .map(|w| process_stack(w, variables, functions))
                .collect();
            let namespace_funcs = namespace_functions(&block.namespace, &block.name, &args);
            if likely(namespace_funcs.1) {
                return namespace_funcs.0;
            }
            error(
                &format!(
                    "Unknown function {}",
                    (block.namespace.join(".") + ".") + block.name.as_str()
                ),
                "",
            );
        }

        ParserInstr::Priority(ref calc) => {
            // execute content inside parentheses before all the other content in the second loop
            return process_stack(calc, variables, functions);
        }
        ParserInstr::Array(ref y, true, false) => {
            // compute final value of arrays
            let mut new_array: Vec<ParserInstr> = Vec::new();
            for element in y {
                new_array.push(process_stack(
                    std::slice::from_ref(element),
                    variables,
                    functions,
                ));
            }
            return ParserInstr::Array(new_array, false, false);
        }
        ParserInstr::Array(ref y, false, true) => {
            // matches multiple arrays following one another => implies array indexing
            let arrays: &Vec<ParserInstr> = y;
            let target_array: ParserInstr =
                process_stack(&[arrays[0].clone()], variables, functions);
            // 1 - matches if the contents of the array have yet to be fully evaluated
            if let ParserInstr::Array(ref target_arr, true, false) = target_array {
                // compute the "final" value of the first/target array
                let mut array = Vec::new();
                for element in target_arr {
                    array.push(process_stack(
                        std::slice::from_ref(element),
                        variables,
                        functions,
                    ));
                }
                let mut output = ParserInstr::Null;
                // iterate over every array following the first one => they are indexes
                for target_index in arrays.iter().skip(1) {
                    if let ParserInstr::Array(ref target_index_arr, true, false) = target_index {
                        let mut index_array = Vec::new();
                        for element in target_index_arr {
                            index_array.push(process_stack(
                                std::slice::from_ref(element),
                                variables,
                                functions,
                            ));
                        }

                        if index_array.len() == 1 {
                            if let ParserInstr::Integer(intg) = index_array[0] {
                                if output == ParserInstr::Null {
                                    output = array[intg as usize].clone();
                                } else {
                                    log!("{:?}OUTPUT", output);
                                    if let ParserInstr::Array(sub_arr, _, false) = output.clone() {
                                        output = sub_arr[intg as usize].clone();
                                    } else if let ParserInstr::String(ref sub_str) = output.clone()
                                    {
                                        output = ParserInstr::String(
                                            sub_str
                                                .chars()
                                                .nth(intg as usize)
                                                .unwrap_or_else(|| {
                                                    error(
                                                        &format!("Failed to get letter n.{intg}"),
                                                        "",
                                                    );
                                                    std::process::exit(1)
                                                })
                                                .to_string()
                                                .parse()
                                                .unwrap(),
                                        );
                                    } else {
                                        error(
                                            &format!(
                                                "Cannot index {} type",
                                                get_printable_type!(output)
                                            ),
                                            "",
                                        );
                                    }
                                }
                            } else {
                                error(&format!("{:?} is not a valid index", index_array[0]), "");
                            }
                        } else {
                            error(&format!("{index_array:?} is not a valid index"), "");
                        }
                    } else {
                        error(&format!("{target_index:?} is not a valid index"), "");
                    }
                }
                return output;
            } else if let ParserInstr::Array(ref target_arr, false, false) = target_array {
                // 2 - matches if contents of target array have already been fully evaluated and the array only contains raw/basic values
                let mut output = ParserInstr::Null;
                for target_index in arrays.iter().skip(1) {
                    if let ParserInstr::Array(ref target_index_arr, true, false) = target_index {
                        let mut index_array = Vec::new();
                        for element in target_index_arr {
                            index_array.push(process_stack(
                                std::slice::from_ref(element),
                                variables,
                                functions,
                            ));
                        }

                        if index_array.len() == 1 {
                            if let ParserInstr::Integer(intg) = index_array[0] {
                                if output == ParserInstr::Null {
                                    output = target_arr[intg as usize].clone();
                                } else {
                                    log!("{:?}OUTPUT", output);
                                    if let ParserInstr::Array(ref sub_arr, _, false) = &output {
                                        output = sub_arr[intg as usize].clone();
                                    } else if let ParserInstr::String(ref sub_str) = &output {
                                        output = ParserInstr::String(
                                            sub_str
                                                .chars()
                                                .nth(intg as usize)
                                                .unwrap_or_else(|| {
                                                    error(
                                                        &format!("Failed to get letter n.{intg}"),
                                                        "",
                                                    );
                                                    std::process::exit(1)
                                                })
                                                .to_string()
                                                .parse()
                                                .unwrap(),
                                        );
                                    } else {
                                        error(
                                            &format!(
                                                "Cannot index {} type",
                                                get_printable_type!(output.clone())
                                            ),
                                            "",
                                        );
                                    }
                                }
                            } else {
                                error(&format!("{:?} is not a valid index", index_array[0]), "");
                            }
                        } else {
                            error(&format!("{index_array:?} is not a valid index"), "");
                        }
                    } else {
                        error(&format!("{target_index:?} is not a valid index"), "");
                    }
                }
                return output;
            } else if let ParserInstr::String(ref str) = target_array {
                // 3 - matches if "array" is a string => returns a letter
                let mut output = ParserInstr::Null;
                for target_index in arrays.iter().skip(1) {
                    if let ParserInstr::Array(ref target_index_arr, true, false) = target_index {
                        let mut index_array = Vec::new();
                        for element in target_index_arr {
                            index_array.push(process_stack(
                                std::slice::from_ref(element),
                                variables,
                                functions,
                            ));
                        }

                        if index_array.len() == 1 {
                            if let ParserInstr::Integer(intg) = index_array[0] {
                                if output == ParserInstr::Null {
                                    output = ParserInstr::String(
                                        str.chars()
                                            .nth(intg as usize)
                                            .unwrap_or_else(|| {
                                                error(
                                                    &format!("Failed to get letter n.{intg}"),
                                                    "",
                                                );
                                                std::process::exit(1)
                                            })
                                            .to_string()
                                            .parse()
                                            .unwrap(),
                                    );
                                } else if let ParserInstr::Array(ref sub_arr, _, false) =
                                    output.clone()
                                {
                                    output = sub_arr[intg as usize].clone();
                                } else if let ParserInstr::String(ref sub_str) = output.clone() {
                                    output = ParserInstr::String(
                                        sub_str
                                            .chars()
                                            .nth(intg as usize)
                                            .unwrap_or_else(|| {
                                                error(&format!("Cannot index '{sub_str}'"), "");
                                                std::process::exit(1)
                                            })
                                            .to_string()
                                            .parse()
                                            .unwrap(),
                                    );
                                } else {
                                    error(
                                        &format!(
                                            "Cannot index {} type",
                                            get_printable_type!(output)
                                        ),
                                        "",
                                    );
                                }
                            } else {
                                error(&format!("{:?} is not a valid index", index_array[0]), "");
                            }
                        } else {
                            error(&format!("{index_array:?} is not a valid index"), "");
                        }
                    } else {
                        error(&format!("{target_index:?} is not a valid index"), "");
                    }
                }
                return output;
            }
            error(
                &format!("Cannot index {} type", get_printable_type!(target_array)),
                "",
            );
        }

        _ => return ParserInstr::Null,
    }
    ParserInstr::Null
}
