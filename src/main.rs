mod parser;
mod parser_functions;
mod util;

use crate::parser::{BasicOperator, Expr, Variable};
use crate::parser_functions::parse_functions;
use crate::util::{assert_args_number, error};
use inflector::Inflector;
use std::fs;
use std::ops::Index;

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
            let target_function: (String, Vec<String>, Vec<Vec<Expr>>) = functions
                .clone()
                .into_iter()
                .filter(|func| func.0 == *func_name)
                .next()
                .expect(&format!("Unknown function '{}'", func_name));
            assert_args_number(&func_name, args.len(), target_function.1.len());
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
        }
    }

    for element in stack {
        if output == Expr::Null {
            output = element
        } else {
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
                            },
                            _ => todo!("[ERORR] INT => STR")
                        }
                    }
                    else {
                        todo!("[ERROR] {:?} => STR", output);
                    }
                }
                Expr::Float(x) => {
                    if let Expr::Integer(value) = output {
                        match current_operator {
                            BasicOperator::Add => {
                                output = Expr::Float(value as f64 + x);
                            }
                            BasicOperator::Sub => {
                                output = Expr::Float(value as f64 - x);
                            }
                            BasicOperator::Divide => {
                                output = Expr::Float(value as f64 / x);
                            }
                            BasicOperator::Multiply => {
                                output = Expr::Float(value as f64 * x);
                            }
                            BasicOperator::Power => {
                                output = Expr::Float((value as f64).powf(x));
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
                                output = Expr::Float(value as f64 / x as f64);
                            }
                            BasicOperator::Multiply => {
                                output = Expr::Integer(value * x);
                            }
                            BasicOperator::Power => {
                                output = Expr::Integer(value.pow(x as u32));
                            },
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
                    }
                }
                Expr::Property(x) => {
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
                                    assert_args_number("uppercase", args.len(), 0);
                                    output = Expr::String(str.to_uppercase());
                                }
                                "lowercase" => {
                                    assert_args_number("lowercase", args.len(), 0);
                                    output = Expr::String(str.to_lowercase());
                                }
                                "capitalize" => {
                                    assert_args_number("capitalize", args.len(), 0);
                                    output = Expr::String(str.to_title_case());
                                }
                                "replace" => {
                                    assert_args_number("replace", args.len(), 2);
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
                                    assert_args_number("toInt", args.len(), 0);
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
                                    assert_args_number("toFloat", args.len(), 0);
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
                                    assert_args_number("toBool", args.len(), 0);
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
                },
                Expr::VariableRedeclaration(x, y) => {
                    let position = variables.clone().iter().position(|var| var.name == x).expect(error_msg!(format!("Variable '{}' does not exist", x)));
                    let processed = process_stack(*y, variables.clone(), functions.clone());
                    variables[position].value = processed.clone();

                    if (included_variables.iter().filter(|var| var.name == x).collect::<Vec<_>>().len() > 0) {
                        return_variables.push(Variable { name: x, value: processed });
                    }
                },
                Expr::FunctionCall(x, y) => {
                    // println!("{:?}", y);
                    let args: Vec<Expr> = y
                        .iter()
                        .map(|arg| process_stack(arg.clone(), variables.clone(), functions.clone()))
                        .collect();
                    if x == "print" {
                        assert_args_number("print", args.len(), 1);
                        match &args[0] {
                            Expr::Float(val) => {
                                println!("{}", val)
                            }
                            Expr::Integer(val) => {
                                println!("{}", val)
                            }
                            Expr::String(val) => {
                                println!("{}", val);
                            }
                            Expr::Bool(val) => {
                                println!("{}", val)
                            }
                            _ => error(&format!("Cannot print {:?} type", &args[0]), "Change type"),
                        }
                    } else {
                        let target_function: (String, Vec<String>, Vec<Vec<Expr>>) = functions
                            .clone()
                            .into_iter()
                            .filter(|func| func.0 == x)
                            .next()
                            .expect(error_msg!(&format!("Unknown function '{}'", x)));
                        assert_args_number(&x, args.len(), target_function.1.len());
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
                    return (process_stack(*x, variables.clone(), functions.clone()), return_variables);
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
                },
                Expr::While(x, y) => {
                    // let condition = process_stack(*x, variables.clone(), functions.clone());
                    while process_stack(*x.clone(), variables.clone(), functions.clone()) == Expr::Bool(true) {
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
                                let indx = variables.iter().position(|var| var.name == replace_var.name).unwrap();
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
    let filename = "example.compute";

    let content = fs::read_to_string(filename).unwrap();

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
    process_function(main_instructions, vec![], vec![], "main", functions);
}
