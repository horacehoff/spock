use colored::Colorize;

pub fn error(message: &str, tip: &str) {
    eprintln!("--------------\n{}\n{}\n{}\n{}\n--------------","COMPUTE ERROR:".red(), message, "POSSIBLE SOLUTION:".blue(), tip);
    std::process::exit(1);
}