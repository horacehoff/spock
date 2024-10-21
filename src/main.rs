mod util;
mod functions;
mod parser;

use std::fs;
use crate::parser::Expr;
use crate::functions::parse_functions;


fn main() {
    let filename = "example.compute";
    
    
    let content = fs::read_to_string(filename).unwrap();
    let functions: Vec<(&str, &str, Vec<Vec<Expr>>)> = parse_functions(&content, filename.parse().unwrap());
    println!("{:?}", functions)

}
