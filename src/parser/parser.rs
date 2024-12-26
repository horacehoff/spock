use crate::log;
use crate::parser::Rule::func_call;
use crate::util::error;
use internment::Intern;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "parser/parser_grammar.pest"]
pub struct ComputeParser;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConditionBlock {
    pub condition: Box<[Types]>,
    // Code to execute if true
    pub code: Box<[Types]>,
    // For each else if/else block, (condition, code)
    pub else_blocks: Box<[(Box<[Types]>, Box<[Types]>)]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LoopBlock {
    // Loop identifier
    pub id: String,
    // Array/string to iterate
    pub arr: Box<[Types]>,
    // Code inside the loop to execute
    pub code: Box<[Types]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhileBlock {
    pub condition: Box<[Types]>,
    // Code to execute while true
    pub code: Box<[Types]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PropertyFunctionBlock {
    pub func1_name: String,
    pub func1_args: Box<[Types]>,
    pub func2_name: String,
    pub func2_args: Box<[Types]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NamespaceFunctionCallBlock {
    pub namespace: Box<[String]>,
    pub name: String,
    pub args: Box<[Types]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableDeclarationBlock {
    pub name: String,
    pub value: Box<[Types]>,
    pub is_declared: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionPropertyCallBlock {
    pub name: String,
    pub args: Box<[Types]>,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    STORE(Intern<u32>),
    STOP,
    // LOAD(Intern<u32>),
    // CLEAR(Intern<u32>),
    VarStore(Intern<String>, Intern<u32>),
    VarReplace(Intern<String>, Intern<u32>),
    FuncCall(Intern<String>, Intern<Vec<u32>>),
    FuncReturn(Intern<u32>),

    // CONDITION REGISTER ID -- JUMP SIZE IF FALSE
    IF(Intern<u32>, Intern<u32>),

    // JUMP X INSTRUCTIONS (CAN BE NEGATIVE)
    JUMP(Intern<i32>),

    Integer(i64),
    Float(f64),
    String(Intern<String>),
    Bool(bool),
    Operation(BasicOperator),

    VariableIdentifier(Intern<String>),
    Null,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Types {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    // ARRAY - IS_PARSED - IS_SUITE
    Array(Vec<Types>, bool, bool),
    Or(Box<[Types]>),
    And(Box<[Types]>),
    Property(Box<FunctionPropertyCallBlock>),
    PropertyFunction(Box<PropertyFunctionBlock>),
    VariableIdentifier(String),
    FunctionCall(Box<FunctionPropertyCallBlock>),
    // FunctionPatternMatching(SmolStr, Box<[Types]>),
    NamespaceFunctionCall(Box<NamespaceFunctionCallBlock>),
    FunctionReturn(Box<[Types]>),
    Priority(Box<[Types]>),
    Operation(BasicOperator),
    // NAME - VALUE - IS_REDECLARE
    VariableDeclaration(Box<VariableDeclarationBlock>),
    Null,
    Condition(Box<ConditionBlock>),
    // Condition
    While(Box<WhileBlock>),
    Loop(Box<LoopBlock>),
    Wrap(Box<[Types]>),
    Separator,
    Break,

    // Objects
    File(String),
}

#[repr(u8)]
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

pub fn wrap_to_flat(inp: Vec<Types>) -> Vec<Types> {
    let mut new_vec: Vec<Types> = vec![];
    for x in inp {
        if let Types::Wrap(content) = x {
            new_vec.extend(content);
            new_vec.push(Types::Separator);
        } else {
            new_vec.push(x);
            new_vec.push(Types::Separator);
        }
    }
    if !new_vec.is_empty() {
        if new_vec.first().unwrap().eq(&Types::Separator) {
            new_vec.remove(0);
        }
        if !new_vec.is_empty() && new_vec.last().unwrap().eq(&Types::Separator) {
            new_vec.pop();
        }
    }
    new_vec
}

pub fn parse_expression(pair: Pair<Rule>) -> Vec<Types> {
    let mut output: Vec<Types> = Vec::new();
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
            let mut suite: Vec<Types> = Vec::new();
            for extra in pair.clone().into_inner() {
                suite.push(parse_expression(extra)[0].clone());
            }

            output.push(Types::Array(suite, false, true));
        }
        Rule::array => {
            let mut array: Vec<Vec<Types>> = Vec::new();
            for array_member in pair.clone().into_inner() {
                array.push(parse_expression(array_member));
            }
            recursive = false;
            output.push(Types::Array(
                array
                    .iter()
                    .map(|x| {
                        if x.len() == 1 {
                            return x.first().unwrap().clone();
                        }
                        Types::Wrap(Box::from(x.clone()))
                    })
                    .collect(),
                true,
                false,
            ));
        }
        Rule::property => {
            recursive = false;
            let mut priority_calc: Vec<Vec<Types>> = Vec::new();
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
            output.push(Types::Property(Box::from(FunctionPropertyCallBlock {
                name: pair
                    .clone()
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
                args: Box::from(wrap_to_flat(
                    priority_calc
                        .iter()
                        .map(|x| {
                            if x.len() == 1 {
                                return x.first().unwrap().clone();
                            }
                            Types::Wrap(Box::from(x.clone()))
                        })
                        .collect(),
                )),
            })));
        }
        Rule::property_function => {
            if let Types::FunctionCall(ref block1) =
                parse_expression(pair.clone().into_inner().next().unwrap())
                    .first()
                    .unwrap()
            {
                if let Types::FunctionCall(ref block2) =
                    parse_expression(pair.clone().into_inner().nth(1).unwrap())
                        .first()
                        .unwrap()
                {
                    output.push(Types::PropertyFunction(Box::from(PropertyFunctionBlock {
                        func1_name: block1.name.clone(),
                        func1_args: block1.args.clone(),
                        func2_name: block2.name.clone(),
                        func2_args: block2.args.clone(),
                    })))
                }
            }
            recursive = false;
        }
        Rule::func_call => {
            recursive = false;
            let mut priority_calc: Vec<Vec<Types>> = Vec::new();
            for priority_pair in pair.clone().into_inner().skip(1) {
                for arg_pair in priority_pair.into_inner() {
                    priority_calc.push(parse_expression(arg_pair));
                }
            }
            output.push(Types::FunctionCall(Box::from(FunctionPropertyCallBlock {
                name: pair
                    .clone()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
                args: Box::from(wrap_to_flat(
                    priority_calc
                        .iter()
                        .map(|x| {
                            if x.len() == 1 {
                                return x.first().unwrap().clone();
                            }
                            Types::Wrap(Box::from(x.clone()))
                        })
                        .collect(),
                )),
            })));
        }
        Rule::func_call_namespace => {
            recursive = false;
            let other_func_call = parse_expression(pair.clone().into_inner().last().unwrap())
                .first()
                .unwrap()
                .clone();
            let mut namespaces = Vec::new();
            for namespace in pair.clone().into_inner().rev().skip(1).rev() {
                namespaces.push(namespace.as_str().parse().unwrap());
            }
            log!("{:?}", namespaces);
            if let Types::FunctionCall(ref block) = other_func_call {
                output.push(Types::NamespaceFunctionCall(Box::from(
                    NamespaceFunctionCallBlock {
                        namespace: Box::from(namespaces),
                        name: block.name.clone(),
                        args: block.args.clone(),
                    },
                )));
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
            let mut priority_calc: Vec<Types> = Vec::new();
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::Priority(Box::from(priority_calc)));
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
            let mut priority_calc: Vec<Types> = Vec::new();
            for priority_pair in pair.clone().into_inner().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::VariableDeclaration(Box::from(
                VariableDeclarationBlock {
                    name: pair
                        .clone()
                        .into_inner()
                        .next()
                        .unwrap()
                        .as_str()
                        .trim()
                        .parse()
                        .unwrap(),
                    value: Box::from(priority_calc),
                    is_declared: false,
                },
            )));
        }
        Rule::variableRedeclaration => {
            recursive = false;
            let mut priority_calc: Vec<Types> = Vec::new();
            for priority_pair in pair.clone().into_inner().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::VariableDeclaration(Box::from(
                VariableDeclarationBlock {
                    name: pair
                        .clone()
                        .into_inner()
                        .next()
                        .unwrap()
                        .as_str()
                        .trim()
                        .parse()
                        .unwrap(),
                    value: Box::from(priority_calc),
                    is_declared: true,
                },
            )));
        }
        Rule::and_operation => {
            recursive = false;
            let mut priority_calc: Vec<Types> = Vec::new();
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::And(Box::from(priority_calc)));
        }
        Rule::or_operation => {
            recursive = false;
            let mut priority_calc: Vec<Types> = Vec::new();
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(Types::Or(Box::from(priority_calc)));
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

pub fn parse_code(content: &str) -> Vec<Vec<Types>> {
    let mut instructions: Vec<Vec<Types>> = Vec::new();
    for pair in ComputeParser::parse(Rule::code, content).unwrap_or_else(|_| {
        error("Failed to parse", "Check semicolons and syntax");
        std::process::exit(1)
    }) {
        // _visualize_parse_tree(pair.clone(), 0);
        for inside in pair.into_inner() {
            let mut line_instructions: Vec<Types> = Vec::new();
            match inside.as_rule() {
                Rule::expression => {
                    for pair in
                        ComputeParser::parse(Rule::expression, inside.as_str().trim()).unwrap()
                    {
                        line_instructions.append(&mut parse_expression(pair));
                    }
                }
                Rule::if_statement => {
                    let condition: Vec<Types> =
                        parse_expression(inside.clone().into_inner().next().unwrap());
                    let first_code: Vec<Types> =
                        parse_code(inside.clone().into_inner().nth(1).unwrap().as_str())
                            .iter()
                            .map(|x| {
                                if x.len() == 1 {
                                    return x.first().unwrap().clone();
                                }
                                Types::Wrap(Box::from(x.clone()))
                            })
                            .collect();
                    let mut else_groups: Vec<(Box<[Types]>, Box<[Types]>)> = Vec::new();
                    for else_block in inside.into_inner().skip(2) {
                        if else_block.clone().into_inner().next().unwrap().as_rule()
                            == Rule::condition
                        {
                            // ELSE IF
                            else_groups.push((
                                Box::from(parse_expression(
                                    else_block.clone().into_inner().next().unwrap(),
                                )),
                                parse_code(else_block.into_inner().nth(1).unwrap().as_str())
                                    .iter()
                                    .map(|x| {
                                        if x.len() == 1 {
                                            x.first().unwrap().clone()
                                        } else {
                                            Types::Wrap(Box::from(x.clone()))
                                        }
                                    })
                                    .collect(),
                            ));
                        } else {
                            // ELSE
                            else_groups.push((
                                Vec::new().into(),
                                parse_code(else_block.into_inner().next().unwrap().as_str())
                                    .iter()
                                    .map(|x| {
                                        if x.len() == 1 {
                                            x.first().unwrap().clone()
                                        } else {
                                            Types::Wrap(Box::from(x.clone()))
                                        }
                                    })
                                    .collect(),
                            ));
                        }
                    }
                    line_instructions.push(Types::Condition(Box::from(ConditionBlock {
                        condition: Box::from(condition),
                        code: Box::from(first_code),
                        else_blocks: Box::from(else_groups),
                    })));
                }
                Rule::return_term => {
                    line_instructions
                        .push(Types::FunctionReturn(Box::from(parse_expression(inside))));
                }
                Rule::break_term => {
                    line_instructions.push(Types::Break);
                }
                Rule::while_statement => {
                    let mut condition: Vec<Types> = Vec::new();
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
                    line_instructions.push(Types::While(Box::from(WhileBlock {
                        condition: Box::from(condition),
                        code: parse_code(inside.into_inner().nth(1).unwrap().as_str())
                            .iter()
                            .map(|x| {
                                if x.len() == 1 {
                                    return x.first().unwrap().clone();
                                }
                                Types::Wrap(Box::from(x.clone()))
                            })
                            .collect(),
                    })));
                }
                Rule::loop_statement => {
                    let mut inner = inside.into_inner();
                    let loop_var = inner.next().unwrap().as_str().into();
                    let target_array = parse_expression(inner.next().unwrap());
                    let loop_code: Vec<Types> = parse_code(inner.next().unwrap().as_str())
                        .iter()
                        .map(|x| {
                            if x.len() == 1 {
                                return x.first().unwrap().clone();
                            }
                            Types::Wrap(Box::from(x.clone()))
                        })
                        .collect();
                    line_instructions.push(Types::Loop(Box::from(LoopBlock {
                        id: loop_var,
                        arr: Box::from(target_array),
                        code: Box::from(loop_code),
                    })));
                }
                _ => {}
            }
            instructions.push(line_instructions);
        }
    }
    instructions
}
