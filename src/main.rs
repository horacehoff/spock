mod util;
mod parser_functions;
mod parser;

use std::fs;
use crate::parser::{BasicOperator, Expr, Variable};
use crate::parser_functions::parse_functions;
use crate::util::error;

fn process_stack(mut stack: Vec<Expr>, variables: Vec<Variable>) -> Expr {
    let mut output: Expr = Expr::Null;
    let mut current_operator: BasicOperator = BasicOperator::Null;
    // println!("{:?}", stack);
    for item in stack.clone() {
        if let Expr::Variable(ref var) = item {
            for x in &mut stack {
                if *x == item {
                    let variable = variables.iter().filter(|variable| variable.name == *var).next().unwrap();
                    *x = variable.value.clone();
                }
            }
        }
    }
    for element in stack {
        if output == Expr::Null {
            output = element
        } else {
            match element {
                Expr::Operation(op) => {
                    current_operator = op;
                },
                Expr::String(x) => {
                    if matches!(output, Expr::String(ref value)) {
                        match current_operator {
                            BasicOperator::Add => {
                                if let Expr::String(value) = output {
                                    output = Expr::String(value + &x);
                                }
                            },
                            _ => todo!()
                        }
                    } else {
                        error("oh","");
                    }
                }
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
                            _ => todo!()
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
                            },
                            BasicOperator::EQUAL => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Bool(value == x);
                                }
                            },
                            BasicOperator::Inferior => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Bool(value < x);
                                }
                            },
                            BasicOperator::Superior => {
                                if let Expr::Integer(value) = output {
                                    output = Expr::Bool(value > x);
                                }
                            }
                            _ => todo!("")
                        }
                    }
                }
                _ => todo!()
            }
        }
    }
    output

}

fn process_function(lines: Vec<Vec<Expr>>, included_variables: Vec<Variable>, expected_variables: Vec<&str>, name: &str) {
    if (included_variables.len() != expected_variables.len()) {
        error(&format!("Function '{}' expected {} arguments, but received {}", name, expected_variables.len(), included_variables.len()),"Remove the excess arguments")
    }
    let mut variables: Vec<Variable> = included_variables;
    for instructions in lines {
        for instruction in instructions {
            println!("{:?}", instruction);
            match instruction {
                Expr::VariableDeclaration(x, y) => {
                    // println!("{:?}", y);
                    variables.push(Variable{name: x, value: process_stack(*y, variables.clone())})
                },
                Expr::Function(x, y) => {
                    let value = process_stack(*y, variables.clone());
                    if x == "print" {
                        match value {
                            Expr::Float(val) => {
                                println!("{}", val)
                            }
                            Expr::Integer(val) => {
                                println!("{}", val)
                            },
                            Expr::String(val) => {
                                println!("{}", val);
                            },
                            Expr::Bool(val) => {
                                println!("{}", val)
                            },
                            _ => todo!("[PRINT FUNCTION] Data type")
                        }
                    }
                }
                _ => todo!()
            }
            // println!("{:?}", instruction)

        }
        // println!("{:?}", instructions)
    }
    // println!("{:?}", variables)
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
