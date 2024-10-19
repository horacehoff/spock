use pest::iterators::Pair;
use crate::Rule;

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Float(f32),
    String(String),
    Identifier(String),
    BinaryOp(BasicOperator, Box<Expr>),
    Priority(Box<Vec<Expr>>),
    Operation(BasicOperator)
}

#[derive(Debug)]
pub enum BasicOperator {
    AND,
    Sub,
    Modulo,
    Add,
    Multiply,
    Inferior,
    EQUAL,
    Power,
    OR,
    Superior,
    Divide
}

#[derive(Debug)]
pub struct VariableDeclaration {
    variable: String,
    value: Expr,
}

pub fn build_ast(pair: Pair<Rule>, indent: usize) -> Vec<Expr> {
    let rule = format!("{:?}", pair.as_rule());
    let span = pair.as_span();
    let text = span.as_str();
    let mut output: Vec<Expr> = vec![];
    let mut recursive = true;

    match pair.as_rule() {
        Rule::integer => {
            output.push(Expr::Integer(pair.as_str().parse::<i64>().unwrap()))
        },
        Rule::float => {
            output.push(Expr::Float(pair.as_str().parse::<f32>().unwrap()))
        },
        Rule::string => {
            output.push(Expr::String(pair.as_str().trim_end_matches("\"").trim_start_matches("\"").parse().unwrap()))
        },
        Rule::priority => {
            recursive = false;
            let mut priority_calc: Vec<Expr> = vec![];
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut build_ast(priority_pair, 0));
            }
            output.push(Expr::Priority(Box::from(priority_calc)))
        }
        Rule::ops => {
            match pair.as_str() {
                "+" => {
                    output.push(Expr::Operation(BasicOperator::Add))
                },
                "-" => {
                    output.push(Expr::Operation(BasicOperator::Sub))
                },
                "/" => {
                    output.push(Expr::Operation(BasicOperator::Divide))
                },
                "*" => {
                    output.push(Expr::Operation(BasicOperator::Multiply))
                }
                _ => {}
            }
        }
        _ => {}
    }
    // Print the current node with indentation
    println!(
        "{}{}: \"{}\"",
        "  ".repeat(indent),  // Indentation based on depth
        rule,
        text
    );

    if recursive {
        // Recursively process the children
        for inner_pair in pair.into_inner() {
            output.append(&mut build_ast(inner_pair, indent + 1));  // Increase indent for child nodes
        }
    }

    output
}