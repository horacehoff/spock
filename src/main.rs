mod util;
mod functions;
mod ast;

use std::fs;
use std::io::Write;
use pest::Parser;
use pest_derive::Parser;
use crate::ast::Expr;
use crate::functions::parse_functions;



#[derive(Parser)]
#[grammar_inline = r#"
integer = @{ "-" ? ~ ASCII_DIGIT+ }

float = @{ "-" ? ~ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT* }

string = @{ "\"" ~ ( "\"\"" | (!"\"" ~ ANY) )* ~ "\"" }

identifier = @{(ASCII_ALPHA | ".")+}

ops = {
    "&&" | "-"  | "%" | "+"  | "*"  | "<"  | "==" | "^" | "||" | ">" | "/"
}

terms = _{ term+ }

priority = {"(" ~ expression ~ ")"}

func_call = {identifier ~ "(" ~ expression ~ ")"}

term = _{ float | integer | string | func_call | priority | identifier }

operation = { ops ~ expression }






variableIdentifier = { ASCII_ALPHA ~ ASCII_ALPHANUMERIC* }
variableDeclaration = {"let" ~ variableIdentifier ~ "=" ~ expression}



expression_unit = _{
    variableDeclaration | operation | terms
}

expression = {expression_unit+}
//
// line = _{expression ~ ";"}
//
// condition = {"if" ~ expression ~ "{" ~ line+ ~ "}"}



WHITESPACE = _{ " " | "\t" | "\n" }
"#]
struct LineParser;



fn build_functions_ast<'a>(functions: Vec<(&'a str, &'a str, Vec<&str>)>) -> Vec<(&'a str, &'a str, Vec<Expr>)> {
    let mut output_functions: Vec<(&str, &str, Vec<Expr>)> = vec![];
    for function in functions {
        let mut instructions: Vec<Expr> = vec![];
        for line in function.2 {
            for pair in LineParser::parse(Rule::expression, line.trim()).unwrap() {
                // println!("{:?}", ast::build_ast(pair, 0));
                instructions.append(&mut ast::build_ast(pair, 0))
            }
        }
        output_functions.push((function.0, function.1, instructions))
    }


    // Output to cache file
    let mut functions_file = fs::File::create(".functions").unwrap();
    functions_file.write_all(&bincode::serialize(&output_functions).unwrap()).unwrap();

    output_functions
}


fn main() {
    let content = fs::read_to_string("foo.txt").unwrap();

    let functions: Vec<(&str, &str, Vec<Expr>)> = build_functions_ast(parse_functions(&content));
    // let other = build_functions_ast(functions);
    println!("{:?}", functions)
    // RUN MAIN FUNCTION
    // let main_lines = &functions.iter().filter(|function| function.0 == "main").collect::<Vec<_>>()[0].2;

    // for function in functions {
    //     let mut instructions: Vec<Expr> = vec![];
    //     for line in function.2 {
    //         for pair in LineParser::parse(Rule::expression, line.trim()).unwrap() {
    //             // println!("{:?}", ast::build_ast(pair, 0));
    //             instructions.append(&mut ast::build_ast(pair, 0))
    //         }
    //     }
    // }

}
