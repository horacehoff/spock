mod util;
mod functions;
mod ast;

use std::fs;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use sha2::digest::typenum::op;
use crate::ast::{BasicOperator, Expr, VariableDeclaration};
use crate::ast::Expr::Float;
use crate::functions::parse_functions;



#[derive(Parser)]
#[grammar_inline = r#"
integer = @{ "-" ? ~ ASCII_DIGIT+ }

float = @{ "-" ? ~ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT* }

string = @{ "'" ~ ( "''" | (!"'" ~ ANY) )* ~ "'" }

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



fn visualize_parse_tree(pair: Pair<Rule>, indent: usize) {
    let rule = format!("{:?}", pair.as_rule());
    let span = pair.as_span();
    let text = span.as_str();

    // Print the current node with indentation
    println!(
        "{}{}: \"{}\"",
        "  ".repeat(indent),  // Indentation based on depth
        rule,
        text
    );

    // Recursively process the children
    for inner_pair in pair.into_inner() {
        visualize_parse_tree(inner_pair, indent + 1);  // Increase indent for child nodes
    }
}


fn build_ast(pair: Pair<Rule>) -> () {
    println!("{:?}", pair.as_rule());
    let mut output: Vec<()> = vec![];
    match pair.as_rule() {
        Rule::expression => {
            for expression in pair.into_inner() {
                println!("TEST {:?}\n", expression)
            }
            // build_ast(pair.into_inner());
        },
        _ => todo!()
    }
}

fn main() {
    let content = fs::read_to_string("foo.txt").unwrap();

    let functions: Vec<(&str, &str, Vec<&str>)> = parse_functions(&content);

    // RUN MAIN FUNCTION
    let main_lines = &functions.iter().filter(|function| function.0 == "main").collect::<Vec<_>>()[0].2;
    println!("{:?}", main_lines);
    for line in main_lines {
        let parsed = LineParser::parse(Rule::expression, line).unwrap().next().unwrap();
        // for pair in parsed {
        //     // visualize_parse_tree(pair, 0);
        //
        //
        // }
        // println!("NEXT{:?}", parsed);
        build_ast(parsed)

    }
}
