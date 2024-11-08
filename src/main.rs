#[path = "parser/parser.rs"]
mod parser;
#[path = "parser/parser_functions.rs"]
mod parser_functions;
mod util;
mod namespaces;
#[path = "types/integer.rs"]
mod integer;
#[path = "types/float.rs"]
mod float;
#[path = "types/array.rs"]
mod array;
#[path = "types/string.rs"]
mod string;
#[path = "types/file.rs"]
mod file;

use std::env::args;
use crate::parser::{parse_code, BasicOperator, Expr, Variable};
use crate::parser_functions::parse_functions;
use crate::util::error;
use inflector::Inflector;
use std::io::{BufRead, BufReader, Write};
use std::{fs, io, thread};
use std::fs::{remove_dir_all};
use std::path::Path;
use crate::float::float_ops;
use crate::integer::integer_ops;
use crate::namespaces::namespace_functions;



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
    for x in &mut stack {
        if let Expr::VariableIdentifier(ref var) = x {
            // replace variable by its value
            let variable = variables
                .iter()
                .filter(|variable| variable.name == *var)
                .next()
                .expect(error_msg!(format!("Variable '{}' doesn't exist", var)));
            *x = variable.value.clone();
        } else if let Expr::NamespaceFunctionCall(namespace, y, z) = x {
            // execute "namespace functions"
            let args: Vec<Expr> = z
                .iter()
                .map(|arg| process_stack(arg.clone(), variables.clone(), functions.clone()))
                .collect();

            let namespace_funcs = namespace_functions(namespace.clone(), y.clone(), args.clone());
            if namespace_funcs.1 {
                *x = namespace_funcs.0;
                continue;
            } else {
                error(&format!("Unknown function {}", namespace.join(".")+"."+y ),"");
            }

        }
        else if let Expr::FunctionCall(ref func_name, ref func_args) = x {
            // replace function call by its result (return value)
            let args: Vec<Expr> = func_args
                .iter()
                .map(|arg| process_stack(arg.clone(), variables.clone(), functions.clone()))
                .collect();
            let matched = basic_functions(func_name.clone(), args.clone());
            // check if function is a built-in function, else search it among user-defined functions
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
            // execute content inside parentheses before all the other content in the second loop
            *x = process_stack(*calc.clone(), variables.clone(), functions.clone());
        } else if let Expr::ArrayParsed(y) = x {
            // compute final value of arrays
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
            // matches multiple arrays following one another => implies array indexing
            let arrays: Vec<Expr> = *y.clone();
            let target_array: Expr = process_stack(vec![arrays[0].clone()], variables.clone(), functions.clone());
            // 1 - matches if the contents of the array have yet to be fully evaluated
            if let Expr::ArrayParsed(target_arr) = target_array {
                // compute the "final" value of the first/target array
                let mut array = vec![];
                for element in target_arr.iter() {
                    array.push(process_stack(
                        element.clone(),
                        variables.clone(),
                        functions.clone(),
                    ));
                }
                let mut output = Expr::Null;
                // iterate over every array following the first one => they are indexes
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
                                    println!("{:?}OUTPUT", output);
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
            } else if let Expr::Array(target_arr) = target_array {
                // 2 - matches if contents of target array have already been fully evaluated and the array only contains raw/basic values
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
                                    output = target_arr[intg as usize].clone()
                                } else {
                                    println!("{:?}OUTPUT", output);
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
            }
            else if let Expr::String(str) = target_array {
                // 3 - matches if "array" is a string => returns a letter
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
                    if let Expr::String(value) = &output {
                        match current_operator {
                            BasicOperator::Add => {
                                output = Expr::String(value.to_owned() + &x);
                            }
                            _ => error(&format!("Cannot perform operation '{:?}' between String and String", current_operator),""),
                        }
                    } else if let Expr::Integer(value) = &output {
                        match current_operator {
                            BasicOperator::Multiply => {
                                output = Expr::String(x.repeat(*value as usize))
                            }
                            _ => error(&format!("Cannot perform operation '{:?}' between Integer and String", current_operator),""),
                        }
                    } else {
                        error(&format!("Cannot perform operation '{:?}' between {:?} and String", current_operator, get_printable_type!(output)), "")
                    }
                }
                Expr::Float(x) => {
                    output = float_ops(x, output, current_operator);
                }
                Expr::Integer(x) => {
                    output = integer_ops(x, output, current_operator);
                }
                Expr::Null => {
                    if let Expr::Null = output {
                        match current_operator {
                            BasicOperator::EQUAL => {
                                output = Expr::Bool(true)
                            },
                            _ => error(&format!("Cannot perform operation '{:?}' between Null and Null", current_operator),"")
                        }
                    }
                }
                Expr::Property(x) => {
                    // TODO
                    todo!("Properties aren't implented yet!")
                    // if matches!(output, Expr::String(_)) {
                    //     match x.as_str() {
                    //         "" => {}
                    //         _ => {}
                    //     }
                    // }
                }
                Expr::PropertyFunction(z) => {
                    if let Expr::FunctionCall(x, y) = *z {
                        let args: Vec<Expr> = y
                            .iter()
                            .map(|arg| {
                                process_stack(arg.clone(), variables.clone(), functions.clone())
                            })
                            .collect();

                        if let Expr::String(str) = output.clone() {
                            string_props!(str, args, x, output);
                        } else if let Expr::Float(num) = output {
                            float_props!(num, args, x, output);
                        } else if let Expr::Integer(num) = output {
                            integer_props!(num, args, x, output);
                        } else if let Expr::Array(ref arr) = output {
                            array_props!(arr, args, x, output);
                        } else if let Expr::File(filepath) = &output {
                            file_props!(filepath, args, x, output);
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
                Expr::NamespaceFunctionCall(z, x, y) => {
                    let args: Vec<Expr> = y
                        .iter()
                        .map(|arg| process_stack(arg.clone(), variables.clone(), functions.clone()))
                        .collect();
                    if !namespace_functions(z.clone(), x.clone(), args.clone()).1 {
                        error(&format!("Unknown function '{}'", z.join(".")+"."+&x), "");
                    };
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

    let functions: Vec<(String, Vec<String>, Vec<Vec<Expr>>)> = parse_functions(content.trim(), true);
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
