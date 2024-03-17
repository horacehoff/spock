use std::process::exit;
use colored::Colorize;





pub fn error(index: i32, error: &str) {
    println!("{}", format!("\nTURBO -- ERROR [LINE {}]", index).red().bold());
    println!("{}", error.blue());
    exit(1);
}