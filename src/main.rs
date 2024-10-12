use std::fs;
use regex::Regex;

fn main() {
    let mut functions: Vec<(&str, &str, &str)> = vec![];


    let mut content = fs::read_to_string("foo.txt").unwrap();


    // PRE_PROCESSING


    // Remove comments
    content = Regex::new(r"//.*").unwrap().replace_all(&content, "").parse().unwrap();
    println!("{:?}", content);



    // Register functions
    let function_regex = Regex::new(r"func (.*)\((.*)\)\s*\{((?s:.)*)}").unwrap();
    let function_results: Vec<_> = function_regex.find_iter(&content).collect();
    for func_match in function_results.iter() {
        let function = function_regex.captures(func_match.as_str()).unwrap();
        functions.push((function.get(1).unwrap().as_str(), function.get(2).unwrap().as_str(), function.get(3).unwrap().as_str()));
    }



    // Register main function
    let main_regex = Regex::new(r"main\s*\{((?s:.)*)}").unwrap();
    let main_block_match = main_regex.captures(&content).unwrap();
    let main_block = main_block_match.get(1).unwrap().as_str();
    println!("{:?}", functions);



}
