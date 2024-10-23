mod util;
mod parser_functions;
mod parser;

use std::fs;
use crate::parser::{BasicOperator, Expr, Variable};
use crate::parser_functions::parse_functions;
use crate::util::error;

fn process_stack(stack: Vec<Expr>) -> Expr {
    let mut output: Expr = Expr::Null;
    let mut current_operator: BasicOperator = BasicOperator::Null;
    for element in stack {
        if output == Expr::Null {
            output = element
        } else {
            match element {
                Expr::Operation(op) => {
                    current_operator = op;
                },
                Expr::Float(x) => {
                    if matches!(output, Expr::Integer(value)) {
                        match current_operator {
                            BasicOperator::Add => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Float(value as f32 + x);
                                }
                            },
                            BasicOperator::Sub => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Float(value as f32 - x);
                                }
                            },
                            BasicOperator::Divide => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Float(value as f32 / x);
                                }
                            },
                            BasicOperator::Multiply => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Float(value as f32 * x);
                                }
                            },
                            BasicOperator::Power => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Float((value as f32).powf(x));
                                }
                            },
                            _ => todo!("")
                        }
                    }
                },
                Expr::Integer(x) => {
                    if matches!(output, Expr::Integer(_)) {
                        match current_operator {
                            BasicOperator::Add => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Integer(value + x);
                                }
                            },
                            BasicOperator::Sub => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Integer(value - x);
                                }
                            },
                            BasicOperator::Divide => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Float(value as f32 / x as f32);
                                }
                            },
                            BasicOperator::Multiply => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Integer(value * x);
                                }
                            },
                            BasicOperator::Power => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Integer(value.pow(x as u32));
                                }
                            }
                            _ => todo!("")
                        }
                    }
                }
                _ => {}
            }
        }
    }
    output

}

fn process_function(lines: Vec<Vec<Expr>>, included_variables: Vec<Variable>, expected_variables: Vec<&str>, name: &str) {
    if (included_variables.len() != expected_variables.len()) {
        error(&format!("Function '{}' expected {} arguments, but received {}", name, expected_variables.len(), included_variables.len()),"Remove the excess arguments")
    }
    let mut variables: Vec<Variable> = vec![];
    for instructions in lines {
        for instruction in instructions {
            match instruction {
                Expr::VariableDeclaration(x, y) => {
                    println!("{:?}", y);
                    variables.push(Variable{name: x, value: process_stack(*y)})
                }
                _ => {}
            }
            // println!("{:?}", instruction)

        }
        // println!("{:?}", instructions)
    }
    println!("{:?}", variables)
}

fn main() {
    let filename = "example.compute";

    let content = fs::read_to_string(filename).unwrap();
    
    let functions: Vec<(&str, Vec<&str>, Vec<Vec<Expr>>)> = parse_functions(&content, filename.parse().unwrap());
    // println!("{:?}", functions);

    let main_instructions = functions.into_iter().filter(|function| function.0 == "main").collect::<Vec<(&str, Vec<&str>, Vec<Vec<Expr>>)>>().first().unwrap().clone().2;
    process_function(main_instructions, vec![], vec![], "main");

    // println!("{:?}", process_stack(vec![Expr::Integer(32), Expr::Operation(BasicOperator::Add), Expr::Float(5.6)]))
}
