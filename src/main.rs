mod util;
mod functions;
use std::fs;
use std::io::Write;
use crate::functions::parse_functions;

fn main() {
    let content = fs::read_to_string("foo.txt").unwrap();

    let functions: Vec<(&str, &str, Vec<&str>)> = parse_functions(&content);

    println!("{:?}", functions);


    // RUN MAIN FUNCTION
}
