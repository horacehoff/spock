use std::sync::{Mutex, MutexGuard};
use once_cell::sync::Lazy;
use regex::Regex;
use std::str::FromStr;
use crate::types::{Node, Types};
use crate::types;
use crate::util;
use lazy_static::lazy_static;


type GlobalVarType = Mutex<Vec<(String, Types)>>;
lazy_static! {
    static ref VARIABLES: GlobalVarType = Mutex::new(Vec::new());
}

fn add_variable(name: String, value: Types) {
    let mut global_variable = VARIABLES.lock().unwrap();
    global_variable.push((name, value));
}

fn get_variables() -> MutexGuard<'static, Vec<(String, Types)>> {
    VARIABLES.lock().unwrap()
}







fn process(content: String, index: i32) {
    let mut split_tokens: Vec<String> = Vec::new();
    let delimiters = ['+', '/','*','-'];
    for part in content.split_inclusive(delimiters) {
        split_tokens.push(part.parse().unwrap());
    }
    let mut tokens: Vec<String> = Vec::new();
    for token in &split_tokens {
        if (delimiters.contains(&token.chars().last().unwrap())) {
            let mut token_stripped = token.chars();
            token_stripped.next_back();
            tokens.push(token_stripped.clone().collect::<String>());
            let token_last = token.chars().last().unwrap().to_string();
            tokens.push(token_last);
            //println!("{:?}", token_last);
        } else {
            tokens.push(token.to_owned());
        }
    }
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
            structs.push(Node::Data(types::convert_str_to_native_type(token, index)));
        }
    }
    // iterate over each group of operations ( data + operation + ... + data)
    let mut i = 0;


    println!("{:?}", structs);
}








pub fn process_line(line: String, index: i32) {
    let tokens = line.split(" ").collect::<Vec<&str>>();
    if tokens[0] == "set" {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^set (.*) -> (.*)$").unwrap());
        if let Some(captures) = RE.captures(&line) {
            let group1 = captures.get(1).map_or("", |m| m.as_str());
            let group2 = captures.get(2).map_or("", |m| m.as_str());

            /*println!("Group 1: {}", group1);
            println!("Group 2: {}", group2);*/

            add_variable(String::from_str(group1).unwrap(), types::convert_str_to_native_type(group2, index));
        } else {
            util::error(index, "Variable declaration failed");
        }
    } else if tokens[0] == "print" {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^print -> (.*)$").unwrap());
        if let Some(captures) = RE.captures(&line) {
            let print_target = captures.get(1).map_or("", |m| m.as_str());

/*            println!("TO PRINT ==> {}", print_target);*/
            process(String::from(print_target), index);
        } else {
            util::error(index, "An error occured during 'print'");
        }
    }
    /*println!("{:?}", tokens);
    println!("Global variable value: {:?}", get_variables());*/
}
