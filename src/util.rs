use std::process::exit;
use colored::Colorize;

pub fn error(index: i32, error: &str) {
    println!("{}", "\nTURBO -- ERROR".red());
    println!("LINE {} -> {}", index, error.blue());
    exit(1);
}
