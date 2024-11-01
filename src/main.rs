mod parser;
mod parser_functions;
mod util;

use crate::parser::{parse_code, BasicOperator, Expr, Variable};
use crate::parser_functions::parse_functions;
use crate::util::error;
use inflector::Inflector;
use std::io::{BufRead, BufReader, Read, Write};
use std::{fs, io, thread};
use std::fs::{remove_dir_all, File};
use std::path::Path;

macro_rules! get_value {
    ($x:expr) => {
        match $x {
            Expr::String(x) => x,
            Expr::Float(x) => x,
            Expr::Integer(x) => x,
            Expr::Bool(x) => x,
            Expr::Array(x) => x,
            _ => panic!("{}", error_msg!(format!("Cannot get value of {:?}", $x))),
        }
    };
}

macro_rules! get_printable_type {
    ($x:expr) => {
        match $x {
            Expr::String(_) => "String",
            Expr::Float(_) => "Float",
            Expr::Integer(_) => "Integer",
            Expr::Bool(_) => "Boolean",
            Expr::Array(_) => "Array",
            Expr::Null => "Null",
            _ => panic!("{}", error_msg!(format!("Cannot get type of {:?}", $x))),
        }
    };
}

macro_rules! math_to_type {
    ($x:expr) => {
        if $x.fract() != 0.0 {
            Expr::Float($x)
        } else {
            Expr::Integer($x as i64)
        }
    };
}


fn get_printable_form(x: Expr) -> String {
    match x {
        Expr::String(str) => str,
        Expr::Float(float) => float.to_string(),
        Expr::Integer(int) => int.to_string(),
        Expr::Bool(boolean) => boolean.to_string(),
        Expr::Array(x) => {
            let arr = *x;
            arr.iter()
                .map(|item| get_printable_form(item.clone()) + ",")
                .collect::<String>()
                .trim_end_matches(",")
                .parse()
                .unwrap()
        },
        Expr::Null => "Null".to_string(),
        _ => panic!("{}", error_msg!(format!("Cannot print {} type", get_printable_type!(x)))),
    }
}


fn basic_functions(x: String, args: Vec<Expr>) -> (Expr, bool) {
    if x == "print" {
        assert_args_number!("print", args.len(), 1);
        println!("{}", get_printable_form(args[0].clone()));
        (Expr::Null, true)
    } else if x == "abs" {
        assert_args_number!("abs", args.len(), 1);
        match &args[0] {
            Expr::Float(val) => return (Expr::Float(val.abs()), true),
            Expr::Integer(val) => return (Expr::Integer(val.abs()), true),
            _ => error(
                &format!("Cannot get absolute value of {:?} type", &args[0]),
                "Change type",
            ),
        }
        (Expr::Null, true)
    } else if x == "round" {
        assert_args_number!("round", args.len(), 1);
        match &args[0] {
            Expr::Float(val) => return (Expr::Integer(val.round() as i64), true),
            Expr::Integer(val) => return (Expr::Integer(*val), true),
            _ => error(
                &format!("Cannot round {} type", get_printable_type!(&args[0])),
                "Change type",
            ),
        }
        (Expr::Null, true)
    } else if x == "len" {
        assert_args_number!("len", args.len(), 1);
        match &args[0] {
            Expr::String(val) => {
                return (Expr::Integer(val.len() as i64), true);
            }
            Expr::Array(val) => {
                return (Expr::Integer(val.len() as i64), true);
            }
            _ => error(
                &format!(
                    "Cannot get length of type {}",
                    get_printable_type!(&args[0])
                ),
                "Change type",
            ),
        }
        (Expr::Null, true)
    } else if x == "input" {
        assert_args_number!("input", args.len(), 0, 1);
        if args.len() == 1 {
            if let Expr::String(prompt) = &args[0] {
                print!("{}", prompt);
            } else {
                error(
                    &format!("Cannot print {} type", get_printable_type!(&args[0])),
                    "Change type",
                );
            }
        }
        io::stdout().flush().unwrap();
        return (
            Expr::String(
                BufReader::new(io::stdin())
                    .lines()
                    .next()
                    .expect(error_msg!("Failed to read input"))
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
            ),
            true,
        );
    } else if x == "type" {
        assert_args_number!("type", args.len(), 1);
        return (
            Expr::String(get_printable_type!(&args[0]).to_string()),
            true,
        );
    } else if x == "hash" {
        assert_args_number!("hash", args.len(), 1);
        (
            Expr::String(
                blake3::hash(
                    bincode::serialize(&args[0])
                        .expect(error_msg!(format!(
                            "Failed to compute hash of object {:?}",
                            &args[0]
                        )))
                        .as_ref(),
                )
                .to_string(),
            ),
            true,
        )
    } else if x == "sqrt" {
        assert_args_number!("sqrt", args.len(), 1);
        if let Expr::Integer(int) = args[0] {
            return (Expr::Float((int as f64).sqrt()), true);
        } else if let Expr::Float(float) = args[0] {
            return (Expr::Float(float.sqrt()), true);
        } else {
            error(
                format!("Cannot calculate the square root of {:?}", args[0]).as_str(),
                "",
            );
            (Expr::Null, false)
        }
    } else if x == "the_answer" {
        println!("42, the answer to the Ultimate Question of Life, the Universe, and Everything.");
        (Expr::Integer(42), true)
    }
    else {
        (Expr::Null, false)
    }
}

fn process_stack(
    mut stack: Vec<Expr>,
    variables: Vec<Variable>,
    functions: Vec<(String, Vec<String>, Vec<Vec<Expr>>)>,
) -> Expr {
    let mut output: Expr = Expr::Null;
    let mut current_operator: BasicOperator = BasicOperator::Null;
    // println!("{:?}", stack);
    for x in &mut stack {
        if let Expr::VariableIdentifier(ref var) = x {
            let variable = variables
                .iter()
                .filter(|variable| variable.name == *var)
                .next()
                .expect(error_msg!(format!("Variable '{}' doesn't exist", var)));
            *x = variable.value.clone();
        } else if let Expr::FunctionCall(ref func_name, ref func_args) = x {
            // replace function call by its result (return value)
            let args: Vec<Expr> = func_args
                .iter()
                .map(|arg| process_stack(arg.clone(), variables.clone(), functions.clone()))
                .collect();
            let matched = basic_functions(func_name.clone(), args.clone());
            if matched.1 {
                *x = matched.0;
                continue;
            } else if func_name == "executeline" {
                assert_args_number!("executeline", args.len(), 1);
                if let Expr::String(line) = &args[0] {
                    *x = process_stack(
                        parse_code(line)[0].clone(),
                        variables.clone(),
                        functions.clone(),
                    );
                    continue;
                } else {
                    error(&format!("Cannot execute line {:?}", &args[0]), "")
                }
            }
            let target_function: (String, Vec<String>, Vec<Vec<Expr>>) = functions
                .clone()
                .into_iter()
                .filter(|func| func.0 == *func_name)
                .next()
                .expect(&format!("Unknown function '{}'", func_name));
            assert_args_number!(&func_name, args.len(), target_function.1.len());
            let target_args: Vec<Variable> = target_function
                .1
                .iter()
                .enumerate()
                .map(|(i, arg)| Variable {
                    name: arg.parse().unwrap(),
                    value: args[i].clone(),
                })
                .collect();
            let result = process_function(
                target_function.2,
                target_args,
                target_function.1,
                target_function.0.as_str(),
                functions.clone(),
            );
            *x = result.0;
        } else if let Expr::Priority(calc) = x {
            *x = process_stack(*calc.clone(), variables.clone(), functions.clone());
        } else if let Expr::ArrayParsed(y) = x {
            let mut new_array: Vec<Expr> = vec![];
            for element in y.iter() {
                new_array.push(process_stack(
                    element.clone(),
                    variables.clone(),
                    functions.clone(),
                ));
            }
            *x = Expr::Array(Box::from(new_array));
        } else if let Expr::ArraySuite(y) = x {
            let arrays: Vec<Expr> = *y.clone();
            let target_array: Expr = arrays[0].clone();
            if let Expr::ArrayParsed(target_arr) = target_array {
                // TARGET ARRAY IS FULLY KNOWN
                let mut array = vec![];
                for element in target_arr.iter() {
                    array.push(process_stack(
                        element.clone(),
                        variables.clone(),
                        functions.clone(),
                    ));
                }
                let mut output = Expr::Null;
                for target_index in arrays.iter().skip(1) {
                    if let Expr::ArrayParsed(target_index_arr) = target_index {
                        let mut index_array = vec![];
                        for element in target_index_arr.iter() {
                            index_array.push(process_stack(
                                element.clone(),
                                variables.clone(),
                                functions.clone(),
                            ));
                        }

                        if index_array.len() == 1 {
                            if let Expr::Integer(intg) = index_array[0] {
                                if output == Expr::Null {
                                    output = array[intg as usize].clone()
                                } else {
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
                *x = output;
            } else if let Expr::String(str) = target_array {
                let mut output = Expr::Null;
                for target_index in arrays.iter().skip(1) {
                    if let Expr::ArrayParsed(target_index_arr) = target_index {
                        let mut index_array = vec![];
                        for element in target_index_arr.iter() {
                            index_array.push(process_stack(
                                element.clone(),
                                variables.clone(),
                                functions.clone(),
                            ));
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
                *x = output;
            } else {
                error(
                    &format!("Cannot index {} type", get_printable_type!(target_array)),
                    "",
                )
            }
        }
    }

    for element in stack {
        if output == Expr::Null {
            output = element
        } else {
            // println!("ELEM {:?}", element);
            match element {
                Expr::Operation(op) => {
                    current_operator = op;
                }
                Expr::OR(x) => {
                    let parsed_exp = process_stack(*x, variables.clone(), functions.clone());
                    if let Expr::Bool(inbool) = output {
                        if let Expr::Bool(sidebool) = parsed_exp {
                            output = Expr::Bool(inbool || sidebool)
                        } else {
                            error(format!("{:?} is not a Boolean", parsed_exp).as_str(), "");
                        }
                    } else {
                        error(format!("{:?} is not a Boolean", output).as_str(), "");
                    }
                }
                Expr::AND(x) => {
                    let parsed_exp = process_stack(*x, variables.clone(), functions.clone());
                    if let Expr::Bool(inbool) = output {
                        if let Expr::Bool(sidebool) = parsed_exp {
                            output = Expr::Bool(inbool && sidebool)
                        } else {
                            error(format!("{:?} is not a Boolean", parsed_exp).as_str(), "");
                        }
                    } else {
                        error(format!("{:?} is not a Boolean", output).as_str(), "");
                    }
                }
                Expr::String(x) => {
                    if let Expr::String(value) = output {
                        match current_operator {
                            BasicOperator::Add => {
                                output = Expr::String(value + &x);
                            }
                            _ => todo!("[ERROR] STR => STR"),
                        }
                    } else if let Expr::Integer(value) = output {
                        match current_operator {
                            BasicOperator::Multiply => {
                                output = Expr::String(x.repeat(value as usize))
                            }
                            _ => todo!("[ERORR] INT => STR"),
                        }
                    } else {
                        todo!("[ERROR] {:?} => STR", output);
                    }
                }
                Expr::Float(x) => {
                    if let Expr::Float(value) = output {
                        match current_operator {
                            BasicOperator::Add => {
                                output = math_to_type!(value + x);
                            }
                            BasicOperator::Sub => {
                                output = math_to_type!(value - x);
                            }
                            BasicOperator::Divide => {
                                output = math_to_type!(value / x);
                            }
                            BasicOperator::Multiply => {
                                output = math_to_type!(value * x);
                            }
                            BasicOperator::Power => {
                                output = math_to_type!(value.powf(x));
                            }
                            BasicOperator::Modulo => {
                                output = math_to_type!(value % x);
                            }
                            BasicOperator::EQUAL => output = Expr::Bool(value == x),
                            BasicOperator::Inferior => {
                                output = Expr::Bool(value < x);
                            }
                            BasicOperator::InferiorEqual => output = Expr::Bool(value <= x),
                            BasicOperator::Superior => {
                                output = Expr::Bool(value > x);
                            }
                            BasicOperator::SuperiorEqual => {
                                output = Expr::Bool(value >= x);
                            }
                            _ => todo!("[ERROR] FLOAT => FLOAT"),
                        }
                    } else if let Expr::Integer(value) = output {
                        match current_operator {
                            BasicOperator::Add => {
                                output = Expr::Float(value as f64 + x);
                            }
                            BasicOperator::Sub => {
                                output = Expr::Float(value as f64 - x);
                            }
                            BasicOperator::Divide => {
                                output = math_to_type!(value as f64 / x);
                            }
                            BasicOperator::Multiply => {
                                output = math_to_type!(value as f64 * x);
                            }
                            BasicOperator::Power => {
                                output = math_to_type!((value as f64).powf(x));
                            }
                            _ => todo!("[ERROR] INT => FLOAT"),
                        }
                    }
                }
                Expr::Integer(x) => {
                    if let Expr::Integer(value) = output {
                        match current_operator {
                            BasicOperator::Add => {
                                output = Expr::Integer(value + x);
                            }
                            BasicOperator::Sub => {
                                output = Expr::Integer(value - x);
                            }
                            BasicOperator::Divide => {
                                output = math_to_type!(value as f64 / x as f64);
                            }
                            BasicOperator::Multiply => {
                                output = Expr::Integer(value * x);
                            }
                            BasicOperator::Power => {
                                output = Expr::Integer(value.pow(x as u32));
                            }
                            BasicOperator::Modulo => {
                                output = Expr::Integer(value % x);
                            }
                            BasicOperator::EQUAL => output = Expr::Bool(value == x),
                            BasicOperator::Inferior => {
                                output = Expr::Bool(value < x);
                            }
                            BasicOperator::InferiorEqual => output = Expr::Bool(value <= x),
                            BasicOperator::Superior => {
                                output = Expr::Bool(value > x);
                            }
                            BasicOperator::SuperiorEqual => {
                                output = Expr::Bool(value >= x);
                            }
                            _ => todo!("[ERROR] INT => INT"),
                        }
                    } else if let Expr::Float(value) = output {
                        match current_operator {
                            BasicOperator::Add => {
                                output = Expr::Float(value + x as f64);
                            }
                            BasicOperator::Sub => {
                                output = Expr::Float(value - x as f64);
                            }
                            BasicOperator::Divide => {
                                output = Expr::Float(value / x as f64);
                            }
                            BasicOperator::Multiply => {
                                output = Expr::Float(value * x as f64);
                            }
                            BasicOperator::Power => {
                                output = Expr::Float(value.powf(x as f64));
                            }
                            _ => todo!("[ERROR] FLOAT => INT"),
                        }
                    }
                }
                Expr::Null => {
                    if let Expr::Null = output {
                        match current_operator {
                            BasicOperator::EQUAL => {
                                output = Expr::Bool(true)
                            },
                            _ => todo!()

                        }

                    }
                }
                Expr::Property(x) => {
                    // TODO
                    if matches!(output, Expr::String(_)) {
                        match x.as_str() {
                            "" => {}
                            _ => {}
                        }
                    }
                }
                Expr::PropertyFunction(z) => {
                    if let Expr::FunctionCall(x, y) = *z {
                        let args: Vec<Expr> = y
                            .iter()
                            .map(|arg| {
                                process_stack(arg.clone(), variables.clone(), functions.clone())
                            })
                            .collect();

                        // STR
                        if let Expr::String(str) = output.clone() {
                            match x.as_str() {
                                // TRANSFORM
                                "uppercase" => {
                                    assert_args_number!("uppercase", args.len(), 0);
                                    output = Expr::String(str.to_uppercase());
                                }
                                "lowercase" => {
                                    assert_args_number!("lowercase", args.len(), 0);
                                    output = Expr::String(str.to_lowercase());
                                }
                                "capitalize" => {
                                    assert_args_number!("capitalize", args.len(), 0);
                                    output = Expr::String(str.to_title_case());
                                }
                                "replace" => {
                                    assert_args_number!("replace", args.len(), 2);
                                    if let Expr::String(toreplace) = &args[0] {
                                        if let Expr::String(replaced) = &args[1] {
                                            output = Expr::String(str.replace(toreplace, replaced))
                                        } else {
                                            error(
                                                format!("{:?} is not a String", &args[1]).as_str(),
                                                format!("Convert {:?} to a String", &args[1])
                                                    .as_str(),
                                            );
                                        }
                                    } else {
                                        error(
                                            format!("{:?} is not a String", &args[0]).as_str(),
                                            format!("Convert {:?} to a String", &args[0]).as_str(),
                                        );
                                    }
                                }
                                "toInt" => {
                                    assert_args_number!("toInt", args.len(), 0);
                                    if str.parse::<i64>().is_ok() {
                                        output = Expr::Integer(str.parse::<i64>().unwrap())
                                    } else {
                                        error(
                                            &format!(
                                                "String '{}' cannot be converted to an Integer",
                                                str
                                            ),
                                            "",
                                        );
                                    }
                                }
                                "toFloat" => {
                                    assert_args_number!("toFloat", args.len(), 0);
                                    if str.parse::<f64>().is_ok() {
                                        output = Expr::Float(str.parse::<f64>().unwrap())
                                    } else {
                                        error(
                                            &format!(
                                                "String '{}' cannot be converted to an Integer",
                                                str
                                            ),
                                            "",
                                        );
                                    }
                                }
                                "toBool" => {
                                    assert_args_number!("toBool", args.len(), 0);
                                    if str.to_lowercase() == "true" {
                                        output = Expr::Bool(true)
                                    } else if str.to_lowercase() == "false" {
                                        output = Expr::Bool(false)
                                    } else {
                                        error(
                                            &format!(
                                                "String '{}' cannot be converted to a Boolean",
                                                str
                                            ),
                                            "",
                                        );
                                    }
                                }
                                "index" => {
                                    assert_args_number!("index", args.len(), 1);
                                    if let Expr::String(toindex) = &args[0] {
                                        let indx = str.find(toindex).unwrap();
                                        output = Expr::Integer(indx as i64);
                                    } else {
                                        error(
                                            format!("{:?} is not a String", &args[0]).as_str(),
                                            format!("Convert {:?} to a String", &args[0]).as_str(),
                                        );
                                    }
                                }
                                "trim" => {
                                    assert_args_number!("trim", args.len(), 0);
                                    output = Expr::String(str.trim().to_string());
                                }
                                "ltrim" => {
                                    assert_args_number!("ltrim", args.len(), 0);
                                    output = Expr::String(str.trim_start().to_string());
                                }
                                "rtrim" => {
                                    assert_args_number!("rtrim", args.len(), 0);
                                    output = Expr::String(str.trim_end().to_string());
                                }
                                _ => {}
                            }
                        } else if let Expr::Float(num) = output {
                            match x.as_str() {
                                "toInt" => {
                                    assert_args_number!("toInt", args.len(), 0);
                                    output = Expr::Integer(num as i64)
                                }
                                "toStr" => {
                                    assert_args_number!("toStr", args.len(), 0);
                                    output = Expr::String(num.to_string())
                                }
                                _ => {}
                            }
                        } else if let Expr::Integer(num) = output {
                            match x.as_str() {
                                "toFloat" => {
                                    assert_args_number!("toFloat", args.len(), 0);
                                    output = Expr::Float(num as f64)
                                }
                                "toStr" => {
                                    assert_args_number!("toStr", args.len(), 0);
                                    output = Expr::String(num.to_string())
                                }
                                _ => {}
                            }
                        } else if let Expr::Array(ref arr) = output {
                            match x.as_str() {
                                "len" => {
                                    assert_args_number!("len", args.len(), 0);
                                    output = Expr::Integer(arr.len() as i64)
                                }
                                "add" => {
                                    assert_args_number!("add", args.len(), 1);
                                    let mut new_vec = arr.clone();
                                    new_vec.push(args[0].clone());
                                    output = Expr::Array(new_vec);
                                }
                                "remove" => {
                                    assert_args_number!("add", args.len(), 1);
                                    let mut new_vec = arr.clone();
                                    let index = new_vec.iter().position(|x| *x == args[0]).unwrap();
                                    new_vec.remove(index);
                                    output = Expr::Array(new_vec);
                                }
                                "clear" => {
                                    assert_args_number!("clear", args.len(), 0);
                                    output = Expr::Array(Box::from(vec![]));
                                }
                                "reverse" => {
                                    assert_args_number!("clear", args.len(), 0);
                                    let mut new_vec = arr.clone();
                                    new_vec.reverse();
                                    output = Expr::Array(Box::from(new_vec))
                                }
                                "sort" => {
                                    assert_args_number!("sort", args.len(), 0);
                                    let mut new_vec: Vec<Expr> = *arr.clone();
                                    new_vec.sort_by(|a, b| match a {
                                        Expr::Integer(x) => match b {
                                            Expr::Integer(y) => x.cmp(y),
                                            Expr::Float(y) => x.cmp(&(*y as i64)),
                                            _ => {
                                                error(
                                                    format!("Cannot compare Integer with {:?}", b)
                                                        .as_str(),
                                                    "",
                                                );
                                                std::cmp::Ordering::Equal
                                            }
                                        },
                                        Expr::Float(x) => match b {
                                            Expr::Integer(y) => (*x as i64).cmp(y),
                                            Expr::Float(y) => x.partial_cmp(y).unwrap(),
                                            _ => {
                                                error(
                                                    format!("Cannot compare Integer with {:?}", b)
                                                        .as_str(),
                                                    "",
                                                );
                                                std::cmp::Ordering::Equal
                                            }
                                        },
                                        _ => {
                                            error(format!("Cannot sort {:?}", a).as_str(), "");
                                            std::cmp::Ordering::Equal
                                        }
                                    });
                                    output = Expr::Array(Box::from(new_vec));
                                }
                                "index" => {
                                    assert_args_number!("index", args.len(), 1);
                                    output = Expr::Integer(
                                        arr.clone().iter().position(|elem| *elem == args[0]).expect(
                                            error_msg!(format!(
                                                "{:?} was not found in the list",
                                                args[0]
                                            )),
                                        ) as i64,
                                    )
                                }
                                "extend" => {
                                    assert_args_number!("extend", args.len(), 1);
                                    let mut new_vec: Vec<Expr> = *arr.clone();
                                    if let Expr::Array(x) = args[0].clone() {
                                        new_vec.extend(*x);
                                        output = Expr::Array(Box::from(new_vec));
                                    } else {
                                        error(format!("{:?} is not a list", args[0]).as_str(), "");
                                    }
                                }
                                "insert" => {
                                    assert_args_number!("insert", args.len(), 2);
                                    let mut new_vec: Vec<Expr> = *arr.clone();
                                    if let Expr::Integer(x) = args[0] {
                                        new_vec.insert(x as usize, args[1].clone());
                                        output = Expr::Array(Box::from(new_vec))
                                    } else {
                                        error(
                                            format!("{:?} is not a valid index", args[0]).as_str(),
                                            "",
                                        );
                                    }
                                }
                                "pop" => {
                                    assert_args_number!("pop", args.len(), 1);
                                    let mut new_vec: Vec<Expr> = *arr.clone();
                                    if let Expr::Integer(x) = args[0] {
                                        new_vec.remove(x as usize);
                                        output = Expr::Array(Box::from(new_vec))
                                    } else {
                                        error(
                                            format!("{:?} is not a valid index", args[0]).as_str(),
                                            "",
                                        );
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => todo!(),
            }
        }
    }
    output
}

fn process_function(
    lines: Vec<Vec<Expr>>,
    included_variables: Vec<Variable>,
    expected_variables: Vec<String>,
    name: &str,
    functions: Vec<(String, Vec<String>, Vec<Vec<Expr>>)>,
) -> (Expr, Vec<Variable>) {
    if included_variables.len() != expected_variables.len() {
        error(
            &format!(
                "Function '{}' expected {} arguments, but received {}",
                name,
                expected_variables.len(),
                included_variables.len()
            ),
            "Remove the excess arguments",
        )
    }
    let mut variables: Vec<Variable> = included_variables.clone();

    let mut return_variables: Vec<Variable> = vec![];

    for instructions in lines {
        for instruction in instructions {
            match instruction {
                Expr::VariableDeclaration(x, y) => {
                    // if variables.iter().filter(|var| var.name == x).collect::<Vec<&Variable>>().len() != 0 {
                    //     let position = variables.clone().iter().position(|var| var.name == x).unwrap();
                    //     variables[position].value = process_stack(*y.clone(), variables.clone(), functions.clone());
                    // }
                    variables.push(Variable {
                        name: x,
                        value: process_stack(*y, variables.clone(), functions.clone()),
                    })
                }
                Expr::VariableRedeclaration(x, y) => {
                    let position = variables
                        .clone()
                        .iter()
                        .position(|var| var.name == x)
                        .expect(error_msg!(format!("Variable '{}' does not exist", x)));
                    let processed = process_stack(*y, variables.clone(), functions.clone());
                    variables[position].value = processed.clone();

                    if included_variables
                        .iter()
                        .filter(|var| var.name == x)
                        .collect::<Vec<_>>()
                        .len()
                        > 0
                    {
                        return_variables.push(Variable {
                            name: x,
                            value: processed,
                        });
                    }
                }
                Expr::FunctionCall(x, y) => {
                    // println!("{:?}", y);
                    let args: Vec<Expr> = y
                        .iter()
                        .map(|arg| process_stack(arg.clone(), variables.clone(), functions.clone()))
                        .collect();

                    let matched = basic_functions(x.clone(), args.clone());
                    if x == "executeline" && !matched.1 {
                        assert_args_number!("executeline", args.len(), 1);
                        if let Expr::String(line) = &args[0] {
                            process_stack(
                                parse_code(line)[0].clone(),
                                variables.clone(),
                                functions.clone(),
                            );
                            continue;
                        } else {
                            error(&format!("Cannot execute line {:?}", &args[0]), "")
                        }
                    } else if !matched.1 {
                        let target_function: (String, Vec<String>, Vec<Vec<Expr>>) = functions
                            .clone()
                            .into_iter()
                            .filter(|func| func.0 == x)
                            .next()
                            .expect(error_msg!(&format!("Unknown function '{}'", x)));
                        assert_args_number!(&x, args.len(), target_function.1.len());
                        let target_args: Vec<Variable> = target_function
                            .1
                            .iter()
                            .enumerate()
                            .map(|(i, arg)| Variable {
                                name: arg.parse().unwrap(),
                                value: args[i].clone(),
                            })
                            .collect();
                        process_function(
                            target_function.2,
                            target_args,
                            target_function.1,
                            &target_function.0,
                            functions.clone(),
                        );
                        // println!("{:?}", target_args)
                    }
                }
                Expr::FunctionReturn(x) => {
                    return (
                        process_stack(*x, variables.clone(), functions.clone()),
                        return_variables,
                    );
                }
                Expr::Condition(x, y, z, w) => {
                    let condition = process_stack(*x, variables.clone(), functions.clone());
                    if condition == Expr::Bool(true) {
                        let out = process_function(
                            *y,
                            variables.clone(),
                            variables
                                .iter()
                                .map(|variable| variable.name.clone())
                                .collect(),
                            name,
                            functions.clone(),
                        );
                        if Expr::Null != out.0 {
                            return out;
                        }
                    } else if *w != vec![] {
                        let condition = process_stack(*w, variables.clone(), functions.clone());
                        if condition == Expr::Bool(true) {
                            let out = process_function(
                                *z,
                                variables.clone(),
                                variables
                                    .iter()
                                    .map(|variable| variable.name.clone())
                                    .collect(),
                                name,
                                functions.clone(),
                            );
                            if Expr::Null != out.0 {
                                return out;
                            }
                        }
                    } else {
                        let out = process_function(
                            *z,
                            variables.clone(),
                            variables
                                .iter()
                                .map(|variable| variable.name.clone())
                                .collect(),
                            name,
                            functions.clone(),
                        );
                        if Expr::Null != out.0 {
                            return out;
                        }
                    }
                }
                Expr::While(x, y) => {
                    // let condition = process_stack(*x, variables.clone(), functions.clone());
                    while process_stack(*x.clone(), variables.clone(), functions.clone())
                        == Expr::Bool(true)
                    {
                        let out = process_function(
                            *y.clone(),
                            variables.clone(),
                            variables
                                .iter()
                                .map(|variable| variable.name.clone())
                                .collect(),
                            name,
                            functions.clone(),
                        );
                        if Expr::Null != out.0 {
                            return out;
                        }
                        if out.1 != vec![] {
                            for replace_var in out.1 {
                                let indx = variables
                                    .iter()
                                    .position(|var| var.name == replace_var.name)
                                    .unwrap();
                                variables[indx] = replace_var;
                            }
                        }
                    }
                }
                _ => todo!(),
            }
            // println!("{:?}", instruction)
        }
        // println!("{:?}", instructions)
    }
    (Expr::Null, return_variables)
    // println!("{:?}", variables)
}

fn main() {
    let arg = std::env::args()
        .nth(1)
        .expect(error_msg!("No file was given"));

    if arg == "clear-cache" && Path::new(".compute").exists() {
        remove_dir_all(Path::new(".compute")).expect(error_msg!("Failed to delete the cache folder (.compute)"));
        return;
    }

    let content = fs::read_to_string(arg).unwrap();

    let functions: Vec<(String, Vec<String>, Vec<Vec<Expr>>)> = parse_functions(content.trim());
    println!("{:?}", functions);

    let main_instructions = functions
        .clone()
        .into_iter()
        .filter(|function| function.0 == "main")
        .collect::<Vec<(String, Vec<String>, Vec<Vec<Expr>>)>>()
        .first()
        .unwrap()
        .clone()
        .2;
    // process_function(main_instructions, vec![], vec![], "main", functions);

    thread::Builder::new()
        // 16MB stack size
        .stack_size(16 * 1024 * 1024)
        .spawn(|| {
            process_function(main_instructions, vec![], vec![], "main", functions);
        })
        .unwrap()
        .join().unwrap();
}
