use crate::log;
use crate::parser::Rule::func_call;
use crate::util::error;
use internment::Intern;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use serde::{Deserialize, Serialize};

pub type Functions = Vec<(
    Intern<String>,
    Vec<Intern<String>>,
    Vec<Instr>,
    Vec<(u16, String)>,
)>;
pub type FunctionsSlice = [(
    Intern<String>,
    Vec<Intern<String>>,
    Vec<Instr>,
    Vec<(u16, String)>,
)];

#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
// #[repr(u)]
pub enum Instr {
    StopStore,
    Null,
    Store,
    StoreArg,
    Operation(Operator),
    Bool(bool),
    FuncReturn,
    // JUMP X INSTRUCTIONS -- IS_NEGATIVE
    Jump(u16, bool),
    // JUMP SIZE IF CONDITION IS FALSE
    If(u16),
    Integer(i32),
    Float(f32),
    VarStore(u16),
    VarUpdate(u16),
    FuncCall(u16),
    VariableIdentifier(Intern<String>),
    String(u16),
}

#[derive(Parser)]
#[grammar = "parser/compute_grammar.pest"]
pub struct ComputeParser;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConditionBlock {
    pub condition: Box<[ParserInstr]>,
    // Code to execute if true
    pub code: Box<[ParserInstr]>,
    // For each else if/else block, (condition, code)
    pub else_blocks: Box<[(Box<[ParserInstr]>, Box<[ParserInstr]>)]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LoopBlock {
    // Loop identifier
    pub id: String,
    // Array/string to iterate
    pub arr: Box<[ParserInstr]>,
    // Code inside the loop to execute
    pub code: Box<[ParserInstr]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhileBlock {
    pub condition: Box<[ParserInstr]>,
    // Code to execute while true
    pub code: Box<[ParserInstr]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PropertyFunctionBlock {
    pub func1_name: String,
    pub func1_args: Box<[ParserInstr]>,
    pub func2_name: String,
    pub func2_args: Box<[ParserInstr]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NamespaceFunctionCallBlock {
    pub namespace: Box<[String]>,
    pub name: String,
    pub args: Box<[ParserInstr]>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableDeclarationBlock {
    pub name: String,
    pub value: Box<[ParserInstr]>,
    pub is_declared: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionPropertyCallBlock {
    pub name: String,
    pub args: Box<[ParserInstr]>,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParserInstr {
    Integer(i32),
    Float(f32),
    String(String),
    Bool(bool),
    // ARRAY - IS_PARSED - IS_SUITE
    Array(Vec<ParserInstr>, bool, bool),
    Or(Box<[ParserInstr]>),
    And(Box<[ParserInstr]>),
    Property(Box<FunctionPropertyCallBlock>),
    PropertyFunction(Box<PropertyFunctionBlock>),
    VariableIdentifier(String),
    FunctionCall(Box<FunctionPropertyCallBlock>),
    // FunctionPatternMatching(SmolStr, Box<[Types]>),
    NamespaceFunctionCall(Box<NamespaceFunctionCallBlock>),
    FunctionReturn(Box<[ParserInstr]>),
    Priority(Box<[ParserInstr]>),
    Operation(Operator),
    // NAME - VALUE - IS_REDECLARE
    VariableDeclaration(Box<VariableDeclarationBlock>),
    Null,
    Condition(Box<ConditionBlock>),
    // Condition
    While(Box<WhileBlock>),
    Loop(Box<LoopBlock>),
    Wrap(Box<[ParserInstr]>),
    Separator,
    Break,

    // Objects
    File(Intern<String>),
}

#[repr(u8)]
#[derive(Debug, Serialize, Clone, Deserialize, PartialEq, Copy)]
pub enum Operator {
    Null,
    Add,
    Sub,
    Divide,
    Multiply,
    Power,
    Modulo,
    Equal,
    NotEqual,
    And,
    Inferior,
    InferiorEqual,
    Or,
    Superior,
    SuperiorEqual,
}

pub fn wrap_to_flat(inp: Vec<ParserInstr>) -> Vec<ParserInstr> {
    let mut new_vec: Vec<ParserInstr> = vec![];
    for x in inp {
        if let ParserInstr::Wrap(content) = x {
            new_vec.extend(content);
            new_vec.push(ParserInstr::Separator);
        } else {
            new_vec.push(x);
            new_vec.push(ParserInstr::Separator);
        }
    }
    if !new_vec.is_empty() {
        if new_vec.first().unwrap().eq(&ParserInstr::Separator) {
            new_vec.remove(0);
        }
        if !new_vec.is_empty() && new_vec.last().unwrap().eq(&ParserInstr::Separator) {
            new_vec.pop();
        }
    }
    new_vec
}

pub fn parse_expression(pair: Pair<Rule>) -> Vec<ParserInstr> {
    let mut output: Vec<ParserInstr> = Vec::new();
    let mut recursive = true;

    // println!("{:?}", pair);

    // let inner = pair.clone().into_inner();
    match pair.as_rule() {
        Rule::integer => output.push(ParserInstr::Integer(pair.as_str().parse::<i32>().unwrap())),
        Rule::float => output.push(ParserInstr::Float(pair.as_str().parse::<f32>().unwrap())),
        Rule::string => output.push(ParserInstr::String(
            pair.as_str()
                .trim_end_matches('\"')
                .trim_start_matches('\"')
                .parse()
                .unwrap(),
        )),
        Rule::bool => output.push(ParserInstr::Bool(pair.as_str() == "true")),
        Rule::array_suite => {
            recursive = false;
            let mut suite: Vec<ParserInstr> = Vec::new();
            for extra in pair.clone().into_inner() {
                suite.push(parse_expression(extra)[0].clone());
            }

            output.push(ParserInstr::Array(suite, false, true));
        }
        Rule::array => {
            let mut array: Vec<Vec<ParserInstr>> = Vec::new();
            for array_member in pair.clone().into_inner() {
                array.push(parse_expression(array_member));
            }
            recursive = false;
            output.push(ParserInstr::Array(
                array
                    .iter()
                    .map(|x| {
                        if x.len() == 1 {
                            return x.first().unwrap().clone();
                        }
                        ParserInstr::Wrap(Box::from(x.clone()))
                    })
                    .collect(),
                true,
                false,
            ));
        }
        Rule::property => {
            recursive = false;
            let mut priority_calc: Vec<Vec<ParserInstr>> = Vec::new();
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
            output.push(ParserInstr::Property(Box::from(
                FunctionPropertyCallBlock {
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
                                ParserInstr::Wrap(Box::from(x.clone()))
                            })
                            .collect(),
                    )),
                },
            )));
        }
        Rule::property_function => {
            if let ParserInstr::FunctionCall(ref block1) =
                parse_expression(pair.clone().into_inner().next().unwrap())
                    .first()
                    .unwrap()
            {
                if let ParserInstr::FunctionCall(ref block2) =
                    parse_expression(pair.clone().into_inner().nth(1).unwrap())
                        .first()
                        .unwrap()
                {
                    output.push(ParserInstr::PropertyFunction(Box::from(
                        PropertyFunctionBlock {
                            func1_name: block1.name.clone(),
                            func1_args: block1.args.clone(),
                            func2_name: block2.name.clone(),
                            func2_args: block2.args.clone(),
                        },
                    )))
                }
            }
            recursive = false;
        }
        Rule::func_call => {
            recursive = false;
            let mut priority_calc: Vec<Vec<ParserInstr>> = Vec::new();
            for priority_pair in pair.clone().into_inner().skip(1) {
                for arg_pair in priority_pair.into_inner() {
                    priority_calc.push(parse_expression(arg_pair));
                }
            }
            output.push(ParserInstr::FunctionCall(Box::from(
                FunctionPropertyCallBlock {
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
                                ParserInstr::Wrap(Box::from(x.clone()))
                            })
                            .collect(),
                    )),
                },
            )));
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
            if let ParserInstr::FunctionCall(ref block) = other_func_call {
                output.push(ParserInstr::NamespaceFunctionCall(Box::from(
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
                output.push(ParserInstr::Null);
            } else {
                output.push(ParserInstr::VariableIdentifier(
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
            let mut priority_calc: Vec<ParserInstr> = Vec::new();
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(ParserInstr::Priority(Box::from(priority_calc)));
        }
        Rule::ops => match pair.as_str() {
            "+" => output.push(ParserInstr::Operation(Operator::Add)),
            "-" => output.push(ParserInstr::Operation(Operator::Sub)),
            "/" => output.push(ParserInstr::Operation(Operator::Divide)),
            "*" => output.push(ParserInstr::Operation(Operator::Multiply)),
            "==" => output.push(ParserInstr::Operation(Operator::Equal)),
            "!=" => output.push(ParserInstr::Operation(Operator::NotEqual)),
            "^" => output.push(ParserInstr::Operation(Operator::Power)),
            "&&" => output.push(ParserInstr::Operation(Operator::And)),
            "%" => output.push(ParserInstr::Operation(Operator::Modulo)),
            "<" => output.push(ParserInstr::Operation(Operator::Inferior)),
            "<=" => output.push(ParserInstr::Operation(Operator::InferiorEqual)),
            "||" => output.push(ParserInstr::Operation(Operator::Or)),
            ">" => output.push(ParserInstr::Operation(Operator::Superior)),
            ">=" => output.push(ParserInstr::Operation(Operator::SuperiorEqual)),
            _ => todo!(),
        },
        Rule::variableDeclaration => {
            recursive = false;
            let mut priority_calc: Vec<ParserInstr> = Vec::new();
            for priority_pair in pair.clone().into_inner().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(ParserInstr::VariableDeclaration(Box::from(
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
            let mut priority_calc: Vec<ParserInstr> = Vec::new();
            for priority_pair in pair.clone().into_inner().skip(1) {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(ParserInstr::VariableDeclaration(Box::from(
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
            let mut priority_calc: Vec<ParserInstr> = Vec::new();
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(ParserInstr::And(Box::from(priority_calc)));
        }
        Rule::or_operation => {
            recursive = false;
            let mut priority_calc: Vec<ParserInstr> = Vec::new();
            for priority_pair in pair.clone().into_inner() {
                priority_calc.append(&mut parse_expression(priority_pair));
            }
            output.push(ParserInstr::Or(Box::from(priority_calc)));
        }
        _ => {}
    }

    if recursive {
        // Recursively process the children
        for inner_pair in pair.clone().into_inner() {
            output.append(&mut parse_expression(inner_pair)); // Increase indent for child nodes
        }
    }
    output
}

pub fn parse_code(content: &str) -> Vec<Vec<ParserInstr>> {
    let mut instructions: Vec<Vec<ParserInstr>> = Vec::new();
    for pair in ComputeParser::parse(Rule::code, content).unwrap_or_else(|_| {
        error("Failed to parse", "Check semicolons and syntax");
        std::process::exit(1)
    }) {
        // _visualize_parse_tree(pair.clone(), 0);
        for inside in pair.into_inner() {
            let mut line_instructions: Vec<ParserInstr> = Vec::new();
            match inside.as_rule() {
                Rule::expression => {
                    for pair in
                        ComputeParser::parse(Rule::expression, inside.as_str().trim()).unwrap()
                    {
                        line_instructions.append(&mut parse_expression(pair));
                    }
                }
                Rule::if_statement => {
                    let condition: Vec<ParserInstr> =
                        parse_expression(inside.clone().into_inner().next().unwrap());
                    let first_code: Vec<ParserInstr> =
                        parse_code(inside.clone().into_inner().nth(1).unwrap().as_str())
                            .iter()
                            .map(|x| {
                                if x.len() == 1 {
                                    return x.first().unwrap().clone();
                                }
                                ParserInstr::Wrap(Box::from(x.clone()))
                            })
                            .collect();
                    let mut else_groups: Vec<(Box<[ParserInstr]>, Box<[ParserInstr]>)> = Vec::new();
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
                                            ParserInstr::Wrap(Box::from(x.clone()))
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
                                            ParserInstr::Wrap(Box::from(x.clone()))
                                        }
                                    })
                                    .collect(),
                            ));
                        }
                    }
                    line_instructions.push(ParserInstr::Condition(Box::from(ConditionBlock {
                        condition: Box::from(condition),
                        code: Box::from(first_code),
                        else_blocks: Box::from(else_groups),
                    })));
                }
                Rule::return_term => {
                    line_instructions.push(ParserInstr::FunctionReturn(Box::from(
                        parse_expression(inside),
                    )));
                }
                Rule::break_term => {
                    line_instructions.push(ParserInstr::Break);
                }
                Rule::while_statement => {
                    let mut condition: Vec<ParserInstr> = Vec::new();
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
                    line_instructions.push(ParserInstr::While(Box::from(WhileBlock {
                        condition: Box::from(condition),
                        code: parse_code(inside.into_inner().nth(1).unwrap().as_str())
                            .iter()
                            .map(|x| {
                                if x.len() == 1 {
                                    return x.first().unwrap().clone();
                                }
                                ParserInstr::Wrap(Box::from(x.clone()))
                            })
                            .collect(),
                    })));
                }
                Rule::loop_statement => {
                    let mut inner = inside.into_inner();
                    let loop_var = inner.next().unwrap().as_str().into();
                    let target_array = parse_expression(inner.next().unwrap());
                    let loop_code: Vec<ParserInstr> = parse_code(inner.next().unwrap().as_str())
                        .iter()
                        .map(|x| {
                            if x.len() == 1 {
                                return x.first().unwrap().clone();
                            }
                            ParserInstr::Wrap(Box::from(x.clone()))
                        })
                        .collect();
                    line_instructions.push(ParserInstr::Loop(Box::from(LoopBlock {
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
