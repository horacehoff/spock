use crate::process::get_variables;
use crate::util;
use once_cell::sync::Lazy;
use regex::Regex;

#[derive(Debug, Clone)]
pub enum Types {
    Number(f32),
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operations {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub enum Node {
    Data(Types),
    Operation(Operations),
}

pub fn convert_str_to_native_type(mut input: &str, index: i32) -> Types {
    static STRING_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r#"^"(.*?)"$"#).unwrap());
    static NUMBER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[+-]?([0-9]*[.]?[0-9]+)$").unwrap());
    input = input.trim();
    if let Some(_captures) = STRING_RE.captures(input) {
        Types::String(
            STRING_RE
                .captures(input)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse()
                .unwrap(),
        )
    } else if input == "true" {
        Types::Boolean(true)
    } else if input == "false" {
        Types::Boolean(false)
    } else if let Some(_captures) = NUMBER_RE.captures(input) {
        let parsed: f32 = NUMBER_RE
            .captures(input)
            .unwrap()
            .get(0)
            .unwrap()
            .as_str()
            .parse()
            .unwrap();
        Types::Number(parsed)
    } else {
        let mut has_found = false;
        let mut value = Types::Boolean(false);
        for var in get_variables().iter() {
            if input == var.0 {
                has_found = true;
                value = var.clone().1;
            }
        }
        if !has_found {
            util::error(index, "Native type conversion failed");
            Types::Boolean(false)
        } else {
            value
        }
    }
}
