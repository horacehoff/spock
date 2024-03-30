use std::str::FromStr;
use std::sync::{Mutex, MutexGuard};

use lazy_static::lazy_static;
use once_cell::sync::Lazy;
use regex::Regex;
use fancy_regex::Regex as FancyRegex;


use crate::operations::{f32_add, f32_divide, f32_multiply, f32_str_add, f32_subtract, str_add, str_f32_add};
use crate::{operations, types, util};
use crate::types::{Node, Types};
use crate::util::error;

pub type GlobalVarType = Mutex<Vec<(String, Types)>>;
lazy_static! {
    pub static ref VARIABLES: GlobalVarType = Mutex::new(Vec::new());
}

pub fn add_variable(name: String, value: Types) {
    let mut global_variable = VARIABLES.lock().unwrap();
    global_variable.push((name, value));
}

pub fn get_variables() -> MutexGuard<'static, Vec<(String, Types)>> {
    VARIABLES.lock().unwrap()
}



fn process(content: String, index: i32) -> Types {
    static SPLIT_REGEX: Lazy<FancyRegex> = Lazy::new(|| FancyRegex::new(r#"(?![^(]*\))(?=(?:[^"]|"[^"]*")*$)[+\-*\/]"#).unwrap());
    let mut tokens = util::split_keep(&SPLIT_REGEX, &content);

    let mut structs: Vec<Node> = Vec::new();
    for token in &tokens {
        if token == "+" {
            structs.push(Node::Operation(types::Operations::Add));
        } else if token == "-" {
            structs.push(Node::Operation(types::Operations::Subtract));
        } else if token == "*" {
            structs.push(Node::Operation(types::Operations::Multiply));
        } else if token == "/" {
            structs.push(Node::Operation(types::Operations::Divide));
        } else {
            // structs.push(Node::Data(types::convert_str_to_native_type(token, index)));
            structs.push(Node::Data(operations::process_object_props(token, index)));
        }
    }

    // iterate over each group of operations ( data + operation + ... + data)
    let mut result = Types::Number(0.0);
    let mut current_operation = types::Operations::Add;

    let mut i = 0;
    for node in structs {
        if i == 0 {
            if let Node::Data(data) = node {
                result = data;
            }
        } else {
            if let Node::Operation(data) = node {
                current_operation = data;
                //println!("{:?}", current_operation);
            } else {
                if current_operation == types::Operations::Add {
                    //println!("{:?} - {:?}", result, node);
                    match (&result, &node) {
                        // f32 -- f32
                        (Types::Number(value1), Node::Data(Types::Number(value2))) => {
                            result = Types::Number(f32_add(value1, value2));
                        }

                        // str -- str
                        (Types::String(value1), Node::Data(Types::String(value2))) => {
                            result = Types::String(str_add(value1, value2));
                        }

                        // str -- f32
                        (Types::Number(value1), Node::Data(Types::String(value2))) => {
                            result = Types::String(f32_str_add(value1, value2));
                        }
                        (Types::String(value1), Node::Data(Types::Number(value2))) => {
                            result = Types::String(str_f32_add(value1, value2));
                        }
                        _ => {
                            error(index, format!("Could not perform an operation between the following types: {:?} - {:?}", &result, node).as_str());
                        }
                    }
                } else if current_operation == types::Operations::Subtract {
                    match (&result, &node) {
                        // f32 -- f32
                        (Types::Number(value1), Node::Data(Types::Number(value2))) => {
                            result = Types::Number(f32_subtract(value1, value2));
                        }
                        _ => {
                            error(index, format!("Could not perform an operation between the following types: {:?} - {:?}", &result, node).as_str());
                        }
                    }
                } else if current_operation == types::Operations::Divide {
                    match (&result, &node) {
                        // f32 -- f32
                        (Types::Number(value1), Node::Data(Types::Number(value2))) => {
                            result = Types::Number(f32_divide(value1, value2));
                        }
                        _ => {
                            error(index, format!("Could not perform an operation between the following types: {:?} - {:?}", &result, node).as_str());
                        }
                    }
                } else if current_operation == types::Operations::Multiply {
                    match (&result, &node) {
                        // f32 -- f32
                        (Types::Number(value1), Node::Data(Types::Number(value2))) => {
                            result = Types::Number(f32_multiply(value1, value2));
                        }
                        _ => {
                            error(index, format!("Could not perform an operation between the following types: {:?} - {:?}", &result, node).as_str());
                        }
                    }
                }
            }
        }
        i += 1;
    }

    //println!("{:?}", result);
    result
}

pub fn process_line(line: String, index: i32) {
    let tokens = line.split(" ").collect::<Vec<&str>>();
    if tokens[0] == "set" {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^set (.*) -> (.*)$").unwrap());
        if let Some(captures) = RE.captures(&line) {
            let group1 = captures.get(1).map_or("", |m| m.as_str());
            let group2 = captures.get(2).map_or("", |m| m.as_str());

            add_variable(
                String::from_str(group1).unwrap(),
                process(String::from(group2), index),
            );
        } else {
            error(index, "Variable declaration failed");
        }
    } else if tokens[0] == "print" {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^print -> (.*)$").unwrap());
        if let Some(captures) = RE.captures(&line) {
            let print_target = captures.get(1).map_or("", |m| m.as_str());
            let processed = process(String::from(print_target), index);
            match processed {
                Types::Number(value) => println!("{}", value),
                Types::String(value) => println!("{}", value),
                Types::Boolean(value) => println!("{}", value),
            }
        } else {
            error(index, "An error occurred during 'print'");
        }
    }
}


pub fn get_function_args(functname: &str, functcall: &str, index: i32) -> Vec<Types> {
    let REPLACE_REGEX = FancyRegex::new(&format!(r"{}\((.*?)\)", functname)).unwrap();
    process_args(REPLACE_REGEX.captures(functcall).unwrap().expect("").get(1).unwrap().as_str(), index)
}

pub fn process_args(functcall: &str, index: i32) -> Vec<Types> {
    let mut results :Vec<Types> = Vec::new();
    static ARGS_REGEX: Lazy<FancyRegex> = Lazy::new(|| FancyRegex::new(r#"(?:[^,"]+|"[^"]*")+"#).unwrap());
    for i in ARGS_REGEX.captures_iter(functcall) {
        results.push(process(String::from(i.unwrap().get(0).unwrap().as_str().trim()), index));
    }
    results
}