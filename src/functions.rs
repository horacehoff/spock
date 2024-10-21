use std::fs::File;
use std::io::{Read, Write};
use fancy_regex::Regex;
use crate::parser::parse_code;
use crate::parser::Expr;
use crate::util::error;

pub fn parse_functions(content: &str, filename: String) -> Vec<(&str, &str, Vec<Vec<Expr>>)> {
    let mut functions: Vec<(&str, &str, Vec<Vec<Expr>>)> = vec![];
    
    // Parse functions
    let function_regex = Regex::new(r"(?ms)^func\s+(\w+)\s*\((.*?)\)\s*\{(.*?)}(?=((\s*func|\z)))").unwrap();
    let function_results: Vec<_> = function_regex.captures_iter(content).collect();


    for func_match in function_results.iter() {
        let function = func_match.as_ref().unwrap();
        let parsed = parse_code(function.get(3).unwrap().as_str().trim());
        functions.push((function.get(1).unwrap().as_str(), function.get(2).unwrap().as_str(), parsed));
    }

    // Cache functions
    let data = bincode::serialize(&functions).unwrap();
    File::create(format!(".compute.{}", blake3::hash(content.as_bytes()).to_string())).unwrap().write_all(&data).unwrap();

    if functions.clone().into_iter().filter(|function| function.0 == "main").collect::<Vec<(&str, &str, Vec<Vec<Expr>>)>>().len() == 0 {
        error("No main function", "Add 'func main() {}' to your file");
    }
    
    functions
}