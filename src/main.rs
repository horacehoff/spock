mod util;
mod functions;
use std::fs;
use pest::Parser;
use pest_derive::Parser;
use crate::functions::parse_functions;



#[derive(Parser)]
#[grammar_inline = r#"
field = { (ASCII_DIGIT | "." | "-")+ }
"#] // relative to src
struct LineParser;

fn main() {
    let content = fs::read_to_string("foo.txt").unwrap();

    let functions: Vec<(&str, &str, Vec<&str>)> = parse_functions(&content);

    println!("{:?}", functions);


    // RUN MAIN FUNCTION
    let main_lines = &functions.iter().filter(|function| function.0 == "main").collect::<Vec<_>>()[0].2;
    for line in main_lines {
        let parsed = LineParser::parse(Rule::field, line);

        println!("{:?}", parsed)
    }
}
