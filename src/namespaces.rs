use std::fs;
use std::process::exit;
use crate::{assert_args_number, error_msg, get_printable_form};
use crate::parser::Expr;
use crate::error;

pub fn namespace_functions(x: Vec<String>, y: String, args: Vec<Expr>) -> (Expr, bool) {
    if x[0] == "compute" {
        if y == "exit" {
            assert_args_number!(y, args.len(), 1);
            if let Expr::Integer(excode) = args[0] {
                exit(excode as i32);
            } else {
                error(&format!("Invalid exit code: {:?}", get_printable_form(args[0].clone())),"");
                (Expr::Null, true)
            }
        } else {
            (Expr::Null, false)
        }
    } else if x[0] == "io" {
        if y == "read" {
            assert_args_number!(y, args.len(), 1);
            if let Expr::String(filename) = args[0].clone() {
                let filecontent = fs::read_to_string(&filename).expect(error_msg!(format!("Failed to read {}", filename)));
                (Expr::String(filecontent), true)
            } else {
                error(&format!("Invalid file name: {:?}", get_printable_form(args[0].clone())),"");
                (Expr::Null, true)
            }
        } else {
            (Expr::Null, false)
        }
    }
    else {
        (Expr::Null, false)
    }
}