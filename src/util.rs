use colored::Colorize;

pub fn error(message: &str) {
    eprintln!("--------------\n{}\n{}\n--------------","COMPUTE ERROR:".red(), message);
}