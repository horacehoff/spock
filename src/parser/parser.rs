use crate::instr_set::Instr;
use crate::parser::Rule::func_call;
use crate::{error, log, parser_math_to_type};
use internment::Intern;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use serde::{Deserialize, Serialize};

pub type Functions = Vec<(
    Intern<String>,      // name
    Vec<Intern<String>>, // args
    Vec<Instr>,          // code
    Vec<Intern<String>>, // locals
    Vec<Intern<String>>, // vars
)>;
pub type FunctionsSlice = [(
    Intern<String>,      // name
    Vec<Intern<String>>, // args
    Vec<Instr>,          // code
    Vec<Intern<String>>, // locals
    Vec<Intern<String>>, // vars
)];

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
    // Priority(Box<[ParserInstr]>),
    LPAREN,
    RPAREN,
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

fn parse_operation(operation_input: Pair<Rule>) -> Vec<ParserInstr> {
    let mut return_vector: Vec<ParserInstr> = Vec::new();
    for x in operation_input.into_inner() {
        if x.as_rule() == Rule::expression || x.as_rule() == Rule::operation {
            return_vector.extend(parse_operation(x));
        } else {
            return_vector.extend(parse_expression(x));
        }
    }
    return_vector
}

fn get_precedence(operator: ParserInstr) -> u8 {
    if let ParserInstr::Operation(op) = operator {
        match op {
            Operator::Null => {
                panic!();
            }
            Operator::Add => 2,
            Operator::Sub => 2,
            Operator::Divide => 3,
            Operator::Multiply => 3,
            Operator::Power => 4,
            Operator::Modulo => 3,
            Operator::Equal => 7,
            Operator::NotEqual => 7,
            Operator::And => 11,
            Operator::Inferior => 6,
            Operator::InferiorEqual => 6,
            Operator::Or => 12,
            Operator::Superior => 6,
            Operator::SuperiorEqual => 6,
        }
    } else {
        panic!("Expected operator (get_precedence)")
    }
}

fn is_left_associative(operator: ParserInstr) -> bool {
    if let ParserInstr::Operation(op) = operator {
        match op {
            Operator::Null => {
                panic!();
            }
            Operator::Power => false,
            _ => true,
        }
    } else {
        panic!("Expected operator (is_left_associative)")
    }
}

// shunting yard algorithm implementation
pub fn op_to_rpn(operation_input: Vec<ParserInstr>) -> Vec<ParserInstr> {
    let mut return_vector: Vec<ParserInstr> = Vec::new();
    let mut op_stack: Vec<ParserInstr> = Vec::new();
    for x in operation_input {
        // num, function,...
        if !matches!(
            x,
            ParserInstr::Operation(_) | ParserInstr::LPAREN | ParserInstr::RPAREN
        ) {
            return_vector.push(x);
        } else if matches!(x, ParserInstr::Operation(_))
            && x != ParserInstr::LPAREN
            && x != ParserInstr::RPAREN
        {
            // operator
            while !op_stack.is_empty()
                && op_stack.last().unwrap() != &ParserInstr::LPAREN
                && (get_precedence(op_stack.last().unwrap().clone()) > get_precedence(x.clone())
                    || (get_precedence(op_stack.last().unwrap().clone())
                        == get_precedence(x.clone())
                        && is_left_associative(x.clone())))
            {
                return_vector.push(op_stack.pop().unwrap());
            }
            op_stack.push(x);
        } else if x == ParserInstr::LPAREN {
            op_stack.push(x);
        } else if x == ParserInstr::RPAREN {
            while op_stack.last().unwrap() != &ParserInstr::LPAREN {
                assert!(!op_stack.is_empty(), "MISMATCHED PARENTHESES");
                return_vector.push(op_stack.pop().unwrap());
            }
            assert_eq!(op_stack.last().unwrap(), &ParserInstr::LPAREN, "WHAT??");
            op_stack.pop();
        }
    }
    while !op_stack.is_empty() {
        assert_ne!(
            op_stack.last().unwrap(),
            &ParserInstr::LPAREN,
            "MISMATCHED PARENTHESES"
        );
        return_vector.push(op_stack.pop().unwrap());
    }

    return_vector
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
        Rule::LPAREN => {
            output.push(ParserInstr::LPAREN);
        }
        Rule::RPAREN => {
            output.push(ParserInstr::RPAREN);
        }
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
                error!(format_args!("{func_call:?} is not a valid function"));
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
        // Rule::priority => {
        //     recursive = false;
        //     let mut priority_calc: Vec<ParserInstr> = Vec::new();
        //     for priority_pair in pair.clone().into_inner() {
        //         priority_calc.append(&mut parse_expression(priority_pair));
        //     }
        //     if priority_calc.len() == 1 {
        //         output.extend(priority_calc);
        //     } else {
        //         output.push(ParserInstr::LPAREN);
        //         output.extend(priority_calc);
        //         output.push(ParserInstr::RPAREN);
        //     }
        // }
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
        Rule::operation => {
            recursive = false;
            let parsed = parse_operation(pair.clone());
            let extra_parsed = op_to_rpn(parsed);
            output.extend(extra_parsed);
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
        // Pre-process operations between constants to speed things up
        Rule::consts_operation => {
            recursive = false;
            let operation: Vec<ParserInstr> = pair
                .clone()
                .into_inner()
                .flat_map(|x| parse_expression(x))
                .collect();
            let mut operator: Operator = Operator::Null;
            let mut result: ParserInstr = ParserInstr::Null;
            for elem in operation {
                if result == ParserInstr::Null {
                    result = elem;
                    continue;
                }
                match elem {
                    ParserInstr::Operation(op) => {
                        operator = op;
                    }
                    ParserInstr::Integer(int) => {
                        if let ParserInstr::Float(parent) = result {
                            match operator {
                                Operator::Add => result = parser_math_to_type!(parent + int as f32),
                                Operator::Sub => result = parser_math_to_type!(parent - int as f32),
                                Operator::Divide => {
                                    result = parser_math_to_type!(parent / int as f32)
                                }
                                Operator::Multiply => {
                                    result = parser_math_to_type!(parent * int as f32)
                                }
                                Operator::Power => result = parser_math_to_type!(parent.powi(int)),
                                Operator::Modulo => {
                                    result = parser_math_to_type!(parent % int as f32)
                                }
                                _ => todo!(),
                            }
                        } else if let ParserInstr::Integer(parent) = result {
                            match operator {
                                Operator::Add => result = ParserInstr::Integer(parent + int),
                                Operator::Sub => result = ParserInstr::Integer(parent - int),
                                Operator::Divide => {
                                    result = parser_math_to_type!(parent as f32 / int as f32)
                                }
                                Operator::Multiply => result = ParserInstr::Integer(parent * int),
                                Operator::Power => {
                                    result = ParserInstr::Integer(parent.pow(int as u32))
                                }
                                Operator::Modulo => result = ParserInstr::Integer(parent % int),
                                _ => todo!(),
                            }
                        }
                    }
                    ParserInstr::Float(float) => {
                        if let ParserInstr::Float(parent) = result {
                            match operator {
                                Operator::Add => result = ParserInstr::Float(parent + float),
                                Operator::Sub => result = ParserInstr::Float(parent - float),
                                Operator::Divide => result = parser_math_to_type!(parent / float),
                                Operator::Multiply => result = parser_math_to_type!(parent * float),
                                Operator::Power => {
                                    result = parser_math_to_type!(parent.powf(float))
                                }
                                Operator::Modulo => result = parser_math_to_type!(parent % float),
                                _ => todo!(),
                            }
                        } else if let ParserInstr::Integer(parent) = result {
                            match operator {
                                Operator::Add => {
                                    result = parser_math_to_type!(parent as f32 + float)
                                }
                                Operator::Sub => {
                                    result = parser_math_to_type!(parent as f32 - float)
                                }
                                Operator::Divide => {
                                    result = parser_math_to_type!(parent as f32 / float)
                                }
                                Operator::Multiply => {
                                    result = parser_math_to_type!(parent as f32 * float)
                                }
                                Operator::Power => {
                                    result = parser_math_to_type!((parent as f32).powf(float))
                                }
                                Operator::Modulo => {
                                    result = parser_math_to_type!(parent as f32 % float)
                                }
                                _ => todo!(),
                            }
                        }
                    }
                    _ => {
                        error!(format_args!("Unkown constant value {elem:?}"));
                    }
                }
            }
            // println!("RESULT IS {result:?}");
            output.push(result);
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
        error!("Failed to parse", "Check semicolons and syntax");
    }) {
        for inside in pair.clone().into_inner() {
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
                Rule::variableDeclaration => {
                    let mut priority_calc: Vec<ParserInstr> = Vec::new();
                    for priority_pair in inside.clone().into_inner() {
                        priority_calc.append(&mut parse_expression(priority_pair));
                    }
                    let parsed_name = priority_calc.remove(0);
                    if let ParserInstr::VariableIdentifier(name) = parsed_name {
                        line_instructions.push(ParserInstr::VariableDeclaration(Box::from(
                            VariableDeclarationBlock {
                                name: name,
                                value: Box::from(priority_calc),
                                is_declared: false,
                            },
                        )));
                    }
                }
                Rule::variableRedeclaration => {
                    let mut priority_calc: Vec<ParserInstr> = Vec::new();
                    for priority_pair in inside.clone().into_inner() {
                        priority_calc.append(&mut parse_expression(priority_pair));
                    }
                    let parsed_name = priority_calc.remove(0);
                    if let ParserInstr::VariableIdentifier(name) = parsed_name {
                        line_instructions.push(ParserInstr::VariableDeclaration(Box::from(
                            VariableDeclarationBlock {
                                name: name,
                                value: Box::from(priority_calc),
                                is_declared: true,
                            },
                        )));
                    }
                }
                _ => {}
            }
            instructions.push(line_instructions);
        }
    }
    instructions
}
