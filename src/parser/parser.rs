use crate::parser::Expr::ArraySuite;
use crate::util::error;
use crate::{error_msg, log};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "parser/parser_grammar.pest"]
pub struct ComputeParser;

#[derive(Debug, Serialize, Clone, Deserialize, PartialEq)]
pub enum Expr {
    Null,
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Vec<Expr>),
    ArrayParsed(Vec<Vec<Expr>>),
    ArraySuite(Vec<Expr>),
    OR(Vec<Expr>),
    AND(Vec<Expr>),
    Property(String),
    PropertyFunction(String, Vec<Vec<Expr>>),
    VariableIdentifier(String),
    FunctionCall(String, Vec<Vec<Expr>>),
    NamespaceFunctionCall(Vec<String>, String, Vec<Vec<Expr>>),
    FunctionReturn(Vec<Expr>),
    Priority(Vec<Expr>),
    Operation(BasicOperator),
    VariableDeclaration(String, Vec<Expr>),
    VariableRedeclaration(String, Vec<Expr>),
    Condition(
        //Condition
        Vec<Expr>,
        // Code to execute if true
        Vec<Vec<Expr>>,
        // For each else if/else block, (condition, code)
        Vec<(Vec<Expr>, Vec<Vec<Expr>>)>
    ),
    // Condition
    While(
        Vec<Expr>,
        // Code to execute while true
        Vec<Vec<Expr>>,
    ),
    Loop(
        // Loop identifier
        String,
        // Array/string to iterate
        Vec<Expr>,
        // Code inside the loop to execute
        Vec<Vec<Expr>>
    ),

    // Objects
    File(String),
}

#[derive(Debug, Serialize, Clone, Deserialize, PartialEq, Copy)]
pub enum BasicOperator {
    Null,
    Add,
    Sub,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    Power,
    AND,
    Modulo,
    Inferior,
    InferiorEqual,
    OR,
    Superior,
    SuperiorEqual,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Variable {
    pub(crate) name: String,
    pub(crate) value: Expr,
}

pub fn parse_expression(pair: Pair<Rule>) -> Vec<Expr> {
    let mut output: Vec<Expr> = vec![];
    let mut recursive = true;

    // println!("{:?}", pair);

    match pair.as_rule() {
        Rule::integer => output.push(Expr::Integer(pair.as_str().parse::<i64>().unwrap())),
        Rule::float => output.push(Expr::Float(pair.as_str().parse::<f64>().unwrap())),
        Rule::string => output.push(Expr::String(
            pair.as_str()
                .trim_end_matches("\"")
                .trim_start_matches("\"")
                .parse()
                .unwrap(),
        )),
        Rule::bool => output.push(Expr::Bool({
            if pair.as_str() == "true" {
                true
            } else {
                false
            }
        })),
        Rule::array_suite => {
            recursive = false;
            let mut suite: Vec<Expr> = vec![];
            for extra in pair.clone().into_inner() {
                suite.push(parse_expression(extra)[0].clone());
                // println!("MEM{:?}", suite);
            }

            output.push(ArraySuite(suite))
        }
        Rule::array => {
            let mut array: Vec<Vec<Expr>> = vec![];
            for array_member in pair.clone().into_inner() {
                array.push(parse_expression(array_member))
            }
            recursive = false;
            output.push(Expr::ArrayParsed(array))
        }
        Rule::property => {
            recursive = false;
            output.push(Expr::Property(
                pair.as_str().trim_start_matches(".").parse().unwrap(),
            ))
        }
        Rule::property_function => {
            recursive = false;
            let mut priority_calc: Vec<Vec<Expr>> = vec![];
            for priority_pair in pair
                .clone()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .skip(1)
            {
                for arg_pair in priority_pair.into_inner() {
                    priority_calc.push(parse_expression(arg_pair));
                }
            }
            output.push(Expr::PropertyFunction(
                pair.clone()
                    .into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .trim_start_matches(".")
                    .parse()
                    .unwrap(),
                priority_calc,
            ));
        }
        Rule::func_call => {
            recursive = false;
            let mut priority_calc: Vec<Vec<Expr>> = vec![];
            for priority_pair in pair.clone().into_inner().into_iter().skip(1) {
                for arg_pair in priority_pair.into_inner() {
                    priority_calc.push(parse_expression(arg_pair));
                }
            }
            output.push(Expr::FunctionCall(
                pair.clone()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
                priority_calc,
            ));
        }
        Rule::func_call_namespace => {
            recursive = false;
            let func_call = parse_expression(pair.clone().into_inner().last().unwrap())
                .first()
                .unwrap()
                .clone();
            let mut namespaces = vec![];
            for namespace in pair.clone().into_inner().rev().skip(1).rev() {
                namespaces.push(namespace.as_str().to_string());
            }
            log!("{:?}", namespaces);
            if let Expr::FunctionCall(x, y) = func_call {
                output.push(Expr::NamespaceFunctionCall(
                    namespaces,
                    x.clone(),
                    y.clone(),
                ));
            } else {
                error(
                    format!("{:?} is not a valid function", func_call).as_str(),
                    "",
                );
            }
        }
        Rule::identifier => {
            if pair.as_str() == "Null" {
                output.push(Expr::Null)
            } else {
                output.push(Expr::VariableIdentifier(
                    pair.as_str()
                        .trim_end_matches("\"")
                        .trim_start_matches("\"")
                        .parse()
                        .unwrap(),
                ))
            }
        }
        Rule::priority => {
            recursive = false;
            let mut priority_calc: Vec<Expr> = vec![];
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Expr::Priority(priority_calc))
        }
        Rule::ops => match pair.as_str() {
            "+" => output.push(Expr::Operation(BasicOperator::Add)),
            "-" => output.push(Expr::Operation(BasicOperator::Sub)),
            "/" => output.push(Expr::Operation(BasicOperator::Divide)),
            "*" => output.push(Expr::Operation(BasicOperator::Multiply)),
            "==" => output.push(Expr::Operation(BasicOperator::Equal)),
            "!=" => output.push(Expr::Operation(BasicOperator::NotEqual)),
            "^" => output.push(Expr::Operation(BasicOperator::Power)),
            "&&" => output.push(Expr::Operation(BasicOperator::AND)),
            "%" => output.push(Expr::Operation(BasicOperator::Modulo)),
            "<" => output.push(Expr::Operation(BasicOperator::Inferior)),
            "<=" => output.push(Expr::Operation(BasicOperator::InferiorEqual)),
            "||" => output.push(Expr::Operation(BasicOperator::OR)),
            ">" => output.push(Expr::Operation(BasicOperator::Superior)),
            ">=" => output.push(Expr::Operation(BasicOperator::SuperiorEqual)),
            _ => todo!(),
        },
        Rule::variableDeclaration => {
            recursive = false;
            let mut priority_calc: Vec<Expr> = vec![];
            for priority_pair in pair.clone().into_inner().into_iter().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Expr::VariableDeclaration(
                pair.clone()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .trim()
                    .parse()
                    .unwrap(),
                priority_calc,
            ));
        }
        Rule::variableRedeclaration => {
            recursive = false;
            let mut priority_calc: Vec<Expr> = vec![];
            for priority_pair in pair.clone().into_inner().into_iter().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Expr::VariableRedeclaration(
                pair.clone()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .trim()
                    .parse()
                    .unwrap(),
                priority_calc,
            ));
        }
        Rule::and_operation => {
            recursive = false;
            let mut priority_calc: Vec<Expr> = vec![];
            for priority_pair in pair.clone().into_inner().into_iter() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Expr::AND(priority_calc));
        }
        Rule::or_operation => {
            recursive = false;
            let mut priority_calc: Vec<Expr> = vec![];
            for priority_pair in pair.clone().into_inner().into_iter() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Expr::OR(priority_calc));
        }
        _ => {}
    }

    if recursive {
        // Recursively process the children
        for inner_pair in pair.into_inner() {
            output.append(&mut parse_expression(inner_pair)); // Increase indent for child nodes
        }
    }
    output
}

fn _visualize_parse_tree(pair: Pair<Rule>, indent: usize) {
    let rule = format!("{:?}", pair.as_rule());
    let span = pair.as_span();
    let text = span.as_str();
    println!("{}{}: \"{}\"", "  ".repeat(indent), rule, text);

    // Recursively process the children
    for inner_pair in pair.into_inner() {
        _visualize_parse_tree(inner_pair, indent + 1);
    }
}

pub fn parse_code(content: &str) -> Vec<Vec<Expr>> {
    let mut instructions: Vec<Vec<Expr>> = vec![];
    for pair in ComputeParser::parse(Rule::code, content)
        .expect(error_msg!("Failed to parse", "Check semicolons and syntax"))
    {
        // _visualize_parse_tree(pair.clone(), 0);
        for inside in pair.into_inner() {
            let mut line_instructions: Vec<Expr> = vec![];
            match inside.as_rule() {
                Rule::expression => {
                    for pair in
                        ComputeParser::parse(Rule::expression, inside.as_str().trim()).unwrap()
                    {
                        line_instructions.append(&mut parse_expression(pair))
                    }
                }
                Rule::if_statement => {
                    let condition: Vec<Expr> = parse_expression(inside.clone().into_inner().into_iter().next().unwrap());
                    let first_code: Vec<Vec<Expr>> = parse_code(inside.clone().into_inner().into_iter().skip(1).next().unwrap().as_str());
                    let mut else_groups: Vec<(Vec<Expr>, Vec<Vec<Expr>>)> = vec![];
                    for else_block in inside.into_inner().into_iter().skip(2) {
                        if else_block.clone().into_inner().into_iter().next().unwrap().as_rule() == Rule::condition {
                            // ELSE IF
                            else_groups.push((parse_expression(else_block.clone().into_inner().into_iter().next().unwrap()),parse_code(else_block.into_inner().into_iter().skip(1).next().unwrap().as_str())));
                        } else {
                            // ELSE
                            else_groups.push((vec![], parse_code(else_block.into_inner().into_iter().next().unwrap().as_str())));
                        }
                    }
                    line_instructions.push(Expr::Condition(condition, first_code, else_groups))
                }
                Rule::return_term => line_instructions
                    .push(Expr::FunctionReturn(parse_expression(inside))),
                Rule::while_statement => {
                    let mut condition: Vec<Expr> = vec![];
                    for pair in ComputeParser::parse(
                        Rule::expression,
                        inside
                            .clone()
                            .into_inner()
                            .next()
                            .unwrap()
                            .into_inner()
                            .as_str()
                            .trim(),
                    )
                    .unwrap()
                    {
                        condition.append(&mut parse_expression(pair))
                    }
                    line_instructions.push(Expr::While(
                        condition,
                        parse_code(
                            inside
                                .into_inner()
                                .into_iter()
                                .skip(1)
                                .next()
                                .unwrap()
                                .as_str(),
                        ),
                    ));
                }
                Rule::loop_statement => {
                    let mut inner = inside.into_inner();
                    let loop_var = inner.next().unwrap().as_str().to_string();
                    let target_array = parse_expression(inner.next().unwrap());
                    let loop_code = parse_code(inner.next().unwrap().as_str());
                    line_instructions.push(Expr::Loop(
                        loop_var,
                        target_array,
                        loop_code,
                    ))
                }
                _ => {}
            }
            instructions.push(line_instructions);
        }
    }
    instructions
}
