use std::process::exit;
use colored::Colorize;
use regex::Regex;
use fancy_regex::Regex as FancyRegex;






pub fn error(index: i32, error: &str) {
    println!("{}", format!("\nTURBO -- ERROR [LINE {}]", index).red().bold());
    println!("{}", error.blue());
    exit(1);
}

pub fn split_keep<'a>(r: &FancyRegex, text: &str) -> Vec<String> {
    let mut result= Vec::new();
    let mut last = 0;
    for mat in r.find_iter(text) {
        let matchs = mat.unwrap();
        let index =  matchs.start();
        let matched = matchs.as_str();
        if last != index {
            result.push(&text[last..index]);
        }
        result.push(matched);
        last = matchs.end();
    }
    if last < text.len() {
        result.push(&text[last..]);
    }
    result.iter().map(|&s| s.to_owned()).collect()
}