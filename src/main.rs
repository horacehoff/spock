mod util;
mod functions;
mod ast;

use std::fs;
use pest::Parser;
use pest_derive::Parser;
use crate::functions::parse_functions;



#[derive(Parser)]
#[grammar_inline = r#"
integer = @{ "-" ? ~ ASCII_DIGIT+ }

float = @{ "-" ? ~ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT* }

string = @{ "\"" ~ ( "\"\"" | (!"\"" ~ ANY) )* ~ "\"" }

identifier = @{ASCII_ALPHA+}

ops = {
    "&&" | "-"  | "%" | "+"  | "*"  | "<"  | "==" | "^" | "||" | ">" | "/"
}

terms = _{ term+ }

priority = {"(" ~ expression ~ ")"}

term = _{ float | integer | string | priority | identifier }

operation = { ops ~ expression }


variableIdentifier = { ASCII_ALPHA ~ ASCII_ALPHANUMERIC* }
variableDeclaration = {"let" ~ variableIdentifier ~ "=" ~ expression}



expression_unit = _{
    variableDeclaration | operation | terms
}

expression = {expression_unit+}


WHITESPACE = _{ " " | "\t" | "\n" }
"#]
struct LineParser;


fn main() {
    let content = fs::read_to_string("foo.txt").unwrap();

    let functions: Vec<(&str, &str, Vec<&str>)> = parse_functions(&content);

    // RUN MAIN FUNCTION
    let main_lines = &functions.iter().filter(|function| function.0 == "main").collect::<Vec<_>>()[0].2;
    println!("{:?}", main_lines);
    for line in main_lines {
        for pair in LineParser::parse(Rule::expression, line).unwrap() {
            println!("{:?}", ast::build_ast(pair, 0));
        }
    }
}
