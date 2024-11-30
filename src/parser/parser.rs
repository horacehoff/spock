use crate::parser::Types::ArraySuite;
use crate::util::error;
use crate::{error_msg, log};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use serde::{Deserialize, Serialize};
use smol_str::{SmolStr, ToSmolStr};

pub type Stack = Vec<Types>;
pub type StackLines = Vec<Vec<Types>>;

#[derive(Parser)]
#[grammar = "parser/parser_grammar.pest"]
pub struct ComputeParser;

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Types {
    Null,
    Integer(i64),
    Float(f64),
    String(SmolStr),
    Bool(bool),
    Array(Stack),
    ArrayParsed(StackLines),
    ArraySuite(Stack),
    Or(Stack),
    And(Stack),
    Property(SmolStr),
    PropertyFunction(SmolStr, StackLines),
    VariableIdentifier(SmolStr),
    FunctionCall(SmolStr, StackLines),
    NamespaceFunctionCall(Vec<SmolStr>, SmolStr, StackLines),
    FunctionReturn(Stack),
    Priority(Stack),
    Operation(BasicOperator),
    VariableDeclaration(SmolStr, Stack),
    VariableRedeclaration(SmolStr, Stack),
    Condition(
        //Condition
        Stack,
        // Code to execute if true
        StackLines,
        // For each else if/else block, (condition, code)
        Vec<(Stack, StackLines)>,
    ),
    // Condition
    While(
        Stack,
        // Code to execute while true
        StackLines,
    ),
    Loop(
        // Loop identifier
        SmolStr,
        // Array/string to iterate
        Stack,
        // Code inside the loop to execute
        Stack,
    ),
    Wrap(Stack),

    // Objects
    File(SmolStr),
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
    And,
    Modulo,
    Inferior,
    InferiorEqual,
    Or,
    Superior,
    SuperiorEqual,
}

pub fn parse_expression(pair: Pair<Rule>) -> Stack {
    let mut output: Stack = vec![];
    let mut recursive = true;

    // println!("{:?}", pair);

    match pair.as_rule() {
        Rule::integer => output.push(Types::Integer(pair.as_str().parse::<i64>().unwrap())),
        Rule::float => output.push(Types::Float(pair.as_str().parse::<f64>().unwrap())),
        Rule::string => output.push(Types::String(
            pair.as_str()
                .trim_end_matches('\"')
                .trim_start_matches('\"')
                .parse()
                .unwrap(),
        )),
        Rule::bool => output.push(Types::Bool(pair.as_str() == "true")),
        Rule::array_suite => {
            recursive = false;
            let mut suite: Stack = vec![];
            for extra in pair.clone().into_inner() {
                suite.push(parse_expression(extra)[0].clone());
                // println!("MEM{:?}", suite);
            }

            output.push(ArraySuite(suite));
        }
        Rule::array => {
            let mut array: StackLines = vec![];
            for array_member in pair.clone().into_inner() {
                array.push(parse_expression(array_member));
            }
            recursive = false;
            output.push(Types::ArrayParsed(array));
        }
        Rule::property => {
            recursive = false;
            output.push(Types::Property(
                pair.as_str().trim_start_matches('.').parse().unwrap(),
            ));
        }
        Rule::property_function => {
            recursive = false;
            let mut priority_calc: StackLines = vec![];
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
            output.push(Types::PropertyFunction(
                pair.clone()
                    .into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .trim_start_matches('.')
                    .parse()
                    .unwrap(),
                priority_calc,
            ));
        }
        Rule::func_call => {
            recursive = false;
            let mut priority_calc: StackLines = vec![];
            for priority_pair in pair.clone().into_inner().skip(1) {
                for arg_pair in priority_pair.into_inner() {
                    priority_calc.push(parse_expression(arg_pair));
                }
            }
            output.push(Types::FunctionCall(
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
                namespaces.push(namespace.as_str().to_smolstr());
            }
            log!("{:?}", namespaces);
            if let Types::FunctionCall(x, y) = func_call {
                output.push(Types::NamespaceFunctionCall(
                    namespaces,
                    x.clone(),
                    y.clone(),
                ));
            } else {
                error(
                    format!("{func_call:?} is not a valid function").as_str(),
                    "",
                );
            }
        }
        Rule::identifier => {
            if pair.as_str() == "Null" {
                output.push(Types::Null);
            } else {
                output.push(Types::VariableIdentifier(
                    pair.as_str()
                        .trim_end_matches('\"')
                        .trim_start_matches('\"')
                        .parse()
                        .unwrap(),
                ));
            }
        }
        Rule::priority => {
            recursive = false;
            let mut priority_calc: Stack = vec![];
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::Priority(priority_calc));
        }
        Rule::ops => match pair.as_str() {
            "+" => output.push(Types::Operation(BasicOperator::Add)),
            "-" => output.push(Types::Operation(BasicOperator::Sub)),
            "/" => output.push(Types::Operation(BasicOperator::Divide)),
            "*" => output.push(Types::Operation(BasicOperator::Multiply)),
            "==" => output.push(Types::Operation(BasicOperator::Equal)),
            "!=" => output.push(Types::Operation(BasicOperator::NotEqual)),
            "^" => output.push(Types::Operation(BasicOperator::Power)),
            "&&" => output.push(Types::Operation(BasicOperator::And)),
            "%" => output.push(Types::Operation(BasicOperator::Modulo)),
            "<" => output.push(Types::Operation(BasicOperator::Inferior)),
            "<=" => output.push(Types::Operation(BasicOperator::InferiorEqual)),
            "||" => output.push(Types::Operation(BasicOperator::Or)),
            ">" => output.push(Types::Operation(BasicOperator::Superior)),
            ">=" => output.push(Types::Operation(BasicOperator::SuperiorEqual)),
            _ => todo!(),
        },
        Rule::variableDeclaration => {
            recursive = false;
            let mut priority_calc: Stack = vec![];
            for priority_pair in pair.clone().into_inner().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::VariableDeclaration(
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
            let mut priority_calc: Stack = vec![];
            for priority_pair in pair.clone().into_inner().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::VariableRedeclaration(
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
            let mut priority_calc: Stack = vec![];
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::And(priority_calc));
        }
        Rule::or_operation => {
            recursive = false;
            let mut priority_calc: Stack = vec![];
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::Or(priority_calc));
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

// fn _visualize_parse_tree(pair: Pair<Rule>, indent: usize) {
//     let rule = format!("{:?}", pair.as_rule());
//     let span = pair.as_span();
//     let text = span.as_str();
//     println!("{}{}: \"{}\"", "  ".repeat(indent), rule, text);
//
//     // Recursively process the children
//     for inner_pair in pair.into_inner() {
//         _visualize_parse_tree(inner_pair, indent + 1);
//     }
// }

pub fn parse_code(content: &str) -> StackLines {
    let mut instructions: StackLines = vec![];
    for pair in ComputeParser::parse(Rule::code, content)
        .expect(error_msg!("Failed to parse", "Check semicolons and syntax"))
    {
        // _visualize_parse_tree(pair.clone(), 0);
        for inside in pair.into_inner() {
            let mut line_instructions: Stack = vec![];
            match inside.as_rule() {
                Rule::expression => {
                    for pair in
                        ComputeParser::parse(Rule::expression, inside.as_str().trim()).unwrap()
                    {
                        line_instructions.append(&mut parse_expression(pair));
                    }
                }
                Rule::if_statement => {
                    let condition: Stack =
                        parse_expression(inside.clone().into_inner().next().unwrap());
                    let first_code: StackLines =
                        parse_code(inside.clone().into_inner().nth(1).unwrap().as_str());
                    let mut else_groups: Vec<(Stack, StackLines)> = vec![];
                    for else_block in inside.into_inner().skip(2) {
                        if else_block.clone().into_inner().next().unwrap().as_rule()
                            == Rule::condition
                        {
                            // ELSE IF
                            else_groups.push((
                                parse_expression(else_block.clone().into_inner().next().unwrap()),
                                parse_code(else_block.into_inner().nth(1).unwrap().as_str()),
                            ));
                        } else {
                            // ELSE
                            else_groups.push((
                                vec![],
                                parse_code(else_block.into_inner().next().unwrap().as_str()),
                            ));
                        }
                    }
                    line_instructions.push(Types::Condition(condition, first_code, else_groups));
                }
                Rule::return_term => {
                    line_instructions.push(Types::FunctionReturn(parse_expression(inside)));
                }
                Rule::while_statement => {
                    let mut condition: Stack = vec![];
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
                        condition.append(&mut parse_expression(pair));
                    }
                    line_instructions.push(Types::While(
                        condition,
                        parse_code(inside.into_inner().nth(1).unwrap().as_str()),
                    ));
                }
                Rule::loop_statement => {
                    let mut inner = inside.into_inner();
                    let loop_var = inner.next().unwrap().as_str().into();
                    let target_array = parse_expression(inner.next().unwrap());
                    let loop_code: Stack = parse_code(inner.next().unwrap().as_str())
                        .iter()
                        .map(|x| {
                            if x.len() == 1 {
                                return x.first().unwrap().clone();
                            } else {
                                return Types::Wrap(x.clone());
                            }
                        })
                        .collect();
                    line_instructions.push(Types::Loop(loop_var, target_array, loop_code));
                }
                _ => {}
            }
            instructions.push(line_instructions);
        }
    }
    instructions
}
