use std::process::exit;
use crate::parser::Expr;

pub fn namespace_functions(x: Vec<String>, y: String, args: Vec<Expr>) -> (Expr, bool) {
    if x[0] == "compute" {
        if y == "exit" {
            exit(0);
        } else {
            (Expr::Null, false)
        }
    } else {
        (Expr::Null, false)
    }
}