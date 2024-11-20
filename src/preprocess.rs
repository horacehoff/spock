use crate::namespaces::namespace_functions;
use crate::parser::{parse_code, Expr, Variable};
use crate::util::{error, get_printable_form};
use crate::{
    assert_args_number, builtin_functions, error_msg, get_printable_type, log, process_function,
    process_stack,
};

// #[inline(always)]
pub fn preprocess(
    variables: &Vec<Variable>,
    functions: &Vec<(String, Vec<String>, Vec<Vec<Expr>>)>,
    element: &Expr,
) -> Expr {
    if let Expr::VariableIdentifier(var) = element {
           variables
            .iter()
            .filter(|x| x.name == *var)
            .next()
            .expect(&format!("Unknown variable '{}'", var)) 
               .value.clone()
    } else if let Expr::NamespaceFunctionCall(ref namespace, ref y, ref z) = element {
        // execute "namespace functions"
        let args: Vec<Expr> = z
            .iter()
            .map(|arg| process_stack(arg, &variables, &functions))
            .collect();

        let namespace_funcs = namespace_functions(&namespace, &y, &args);
        if namespace_funcs.1 {
            namespace_funcs.0
        } else {
            error(
                &format!("Unknown function {}", namespace.join(".") + "." + &y),
                "",
            );
            return Expr::Null;
        }
    } else if let Expr::FunctionCall(ref func_name, ref func_args) = element {
        // replace function call by its result (return value)
        let args: Vec<Expr> = func_args
            .iter()
            .map(|arg| process_stack(arg, &variables, &functions))
            .collect();
        let matched = builtin_functions(&func_name, &args);
        // check if function is a built-in function, else search it among user-defined functions
        if matched.1 {
            return matched.0;
        } else if func_name == "executeline" {
            assert_args_number!("executeline", args.len(), 1);
            if let Expr::String(line) = &args[0] {
                return process_stack(&parse_code(line)[0], &variables, &functions);
            } else {
                error(&format!("Cannot execute {:?}", &args[0]), "")
            }
        } else if func_name == "int" {
            assert_args_number!("int", args.len(), 1);
            if let Expr::String(str) = &args[0] {
                return Expr::Integer(str.parse::<i64>().expect(error_msg!(format!(
                    "Cannot convert String '{}' to Integer",
                    str
                ))));
            } else if let Expr::Float(float) = &args[0] {
                return Expr::Integer(float.round() as i64);
            } else {
                error(
                    &format!(
                        "Cannot convert {} to Integer",
                        get_printable_type!(&args[0])
                    ),
                    "",
                )
            }
        } else if func_name == "str" {
            assert_args_number!("str", args.len(), 1);
            if let Expr::Integer(int) = &args[0] {
                return Expr::String(int.to_string());
            } else if let Expr::Float(float) = &args[0] {
                return Expr::String(float.to_string());
            } else if let Expr::Bool(boolean) = &args[0] {
                return Expr::String(if *boolean {
                    "true".to_string()
                } else {
                    "false".to_string()
                });
            } else if let Expr::Array(arr) = &args[0] {
                return Expr::String(get_printable_form(&args[0]));
            } else {
                error(
                    &format!("Cannot convert {} to String", get_printable_type!(&args[0])),
                    "",
                )
            }
        } else if func_name == "float" {
            assert_args_number!("float", args.len(), 1);
            if let Expr::String(str) = &args[0] {
                return Expr::Float(str.parse::<f64>().expect(error_msg!(format!(
                    "Cannot convert String '{}' to Float",
                    str
                ))));
            } else if let Expr::Integer(int) = &args[0] {
                return Expr::Float(*int as f64);
            } else {
                error(
                    &format!("Cannot convert {} to Float", get_printable_type!(&args[0])),
                    "",
                )
            }
        }

        let target_function: &(String, Vec<String>, Vec<Vec<Expr>>) = functions
            .into_iter()
            .filter(|func| func.0 == *func_name)
            .next()
            .expect(&format!("Unknown function '{}'", func_name));
        assert_args_number!(&func_name, args.len(), target_function.1.len());
        let target_args: &Vec<Variable> = &target_function
            .1
            .iter()
            .enumerate()
            .map(|(i, arg)| Variable {
                name: arg.to_string(),
                value: args[i].to_owned(),
            })
            .collect();
        let result = process_function(
            &target_function.2,
            &target_args,
            &target_args,
            target_function.0.as_str(),
            &functions,
        );
        return result.0;
    } else if let Expr::Priority(calc) = element {
        // execute content inside parentheses before all the other content in the second loop
        return process_stack(&calc, &variables, &functions);
    } else if let Expr::ArrayParsed(y) = element {
        // compute final value of arrays
        let mut new_array: Vec<Expr> = vec![];
        for element in y.iter() {
            new_array.push(process_stack(&element, &variables, &functions));
        }
        return Expr::Array(new_array);
    } else if let Expr::ArraySuite(ref y) = element {
        // matches multiple arrays following one another => implies array indexing
        let arrays: &Vec<Expr> = y;
        let target_array: Expr = process_stack(&vec![arrays[0].clone()], &variables, &functions);
        // 1 - matches if the contents of the array have yet to be fully evaluated
        if let Expr::ArrayParsed(target_arr) = target_array {
            // compute the "final" value of the first/target array
            let mut array = vec![];
            for element in target_arr.iter() {
                array.push(process_stack(&element, &variables, &functions));
            }
            let mut output = Expr::Null;
            // iterate over every array following the first one => they are indexes
            for target_index in arrays.iter().skip(1) {
                if let Expr::ArrayParsed(target_index_arr) = target_index {
                    let mut index_array = vec![];
                    for element in target_index_arr.iter() {
                        index_array.push(process_stack(&element, &variables, &functions));
                    }

                    if index_array.len() == 1 {
                        if let Expr::Integer(intg) = index_array[0] {
                            if output == Expr::Null {
                                output = array[intg as usize].clone()
                            } else {
                                log!("{:?}OUTPUT", output);
                                if let Expr::Array(sub_arr) = output.clone() {
                                    output = sub_arr[intg as usize].clone()
                                } else if let Expr::String(sub_str) = output.clone() {
                                    output = Expr::String(
                                        sub_str.chars().nth(intg as usize).unwrap().to_string(),
                                    );
                                } else {
                                    error(
                                        &format!(
                                            "Cannot index {} type",
                                            get_printable_type!(output)
                                        ),
                                        "",
                                    )
                                }
                            }
                        } else {
                            error(&format!("{:?} is not a valid index", index_array[0]), "");
                        }
                    } else {
                        error(&format!("{:?} is not a valid index", index_array), "");
                    }
                } else {
                    error(&format!("{:?} is not a valid index", target_index), "");
                }
            }
            return output;
        } else if let Expr::Array(target_arr) = target_array {
            // 2 - matches if contents of target array have already been fully evaluated and the array only contains raw/basic values
            let mut output = Expr::Null;
            for target_index in arrays.iter().skip(1) {
                if let Expr::ArrayParsed(target_index_arr) = target_index {
                    let mut index_array = vec![];
                    for element in target_index_arr.iter() {
                        index_array.push(process_stack(&element, &variables, &functions));
                    }

                    if index_array.len() == 1 {
                        if let Expr::Integer(intg) = index_array[0] {
                            if output == Expr::Null {
                                output = target_arr[intg as usize].clone()
                            } else {
                                log!("{:?}OUTPUT", output);
                                if let Expr::Array(sub_arr) = &output {
                                    output = sub_arr[intg as usize].clone()
                                } else if let Expr::String(sub_str) = &output {
                                    output = Expr::String(
                                        sub_str.chars().nth(intg as usize).unwrap().to_string(),
                                    );
                                } else {
                                    error(
                                        &format!(
                                            "Cannot index {} type",
                                            get_printable_type!(output.clone())
                                        ),
                                        "",
                                    )
                                }
                            }
                        } else {
                            error(&format!("{:?} is not a valid index", index_array[0]), "");
                        }
                    } else {
                        error(&format!("{:?} is not a valid index", index_array), "");
                    }
                } else {
                    error(&format!("{:?} is not a valid index", target_index), "");
                }
            }
            return output;
        } else if let Expr::String(str) = target_array {
            // 3 - matches if "array" is a string => returns a letter
            let mut output = Expr::Null;
            for target_index in arrays.iter().skip(1) {
                if let Expr::ArrayParsed(target_index_arr) = target_index {
                    let mut index_array = vec![];
                    for element in target_index_arr.iter() {
                        index_array.push(process_stack(&element, &variables, &functions));
                    }

                    if index_array.len() == 1 {
                        if let Expr::Integer(intg) = index_array[0] {
                            if output == Expr::Null {
                                output = Expr::String(
                                    str.chars().nth(intg as usize).unwrap().to_string(),
                                );
                            } else {
                                if let Expr::Array(sub_arr) = output.clone() {
                                    output = sub_arr[intg as usize].clone()
                                } else if let Expr::String(sub_str) = output.clone() {
                                    output = Expr::String(
                                        sub_str
                                            .chars()
                                            .nth(intg as usize)
                                            .expect(error_msg!(format!(
                                                "Cannot index '{}'",
                                                sub_str
                                            )))
                                            .to_string(),
                                    );
                                } else {
                                    error(
                                        &format!(
                                            "Cannot index {} type",
                                            get_printable_type!(output)
                                        ),
                                        "",
                                    )
                                }
                            }
                        } else {
                            error(&format!("{:?} is not a valid index", index_array[0]), "");
                        }
                    } else {
                        error(&format!("{:?} is not a valid index", index_array), "");
                    }
                } else {
                    error(&format!("{:?} is not a valid index", target_index), "");
                }
            }
            return output;
            // *x = output;
        } else {
            error(
                &format!("Cannot index {} type", get_printable_type!(target_array)),
                "",
            );
            Expr::Null
        }
    } else {
        element.clone()
    }
}
