mod util;
mod functions;
use std::fs;
use pest::Parser;
use pest_derive::Parser;
use crate::functions::parse_functions;



#[derive(Parser)]
#[grammar_inline = r#"
integer = @{ "-" ? ~ ASCII_DIGIT+ }

float = @{ "-" ? ~ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT* }

string = @{ "'" ~ ( "''" | (!"'" ~ ANY) )* ~ "'" }

ops = {
    "&&" | "-"  | "%" | "+"  | "*"  | "<"  | "==" | "^" | "||" | ">"
}

terms = { term+ }

term = _{ float | integer | string | "(" ~ expression ~ ")" }

operation = { ops ~ expression }

variableDeclaration = {"let" ~ ASCII_ALPHA+ ~ "=" ~ expression}

expression = {
    terms | operation | variableDeclaration
}

line = {expression+}

"#] // relative to src
struct LineParser;

fn main() {
    let content = fs::read_to_string("foo.txt").unwrap();

    let functions: Vec<(&str, &str, Vec<&str>)> = parse_functions(&content);

    // RUN MAIN FUNCTION
    let main_lines = &functions.iter().filter(|function| function.0 == "main").collect::<Vec<_>>()[0].2;
    println!("{:?}", main_lines);
    for line in main_lines {
        let parsed = LineParser::parse(Rule::line, line).unwrap();

        println!("{:?}", parsed)
    }
}
