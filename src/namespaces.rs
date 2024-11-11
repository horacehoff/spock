use crate::error;
use crate::parser::Expr;
use crate::{assert_args_number, error_msg, get_printable_form};
use std::fs::OpenOptions;
use std::io::Write;
use std::process::exit;

pub fn namespace_functions(x: &Vec<String>, y: &str, args: &Vec<Expr>) -> (Expr, bool) {
    if x[0] == "compute" {
        if y == "exit" {
            assert_args_number!(y, args.len(), 1);
            if let Expr::Integer(excode) = args[0] {
                exit(excode as i32);
            } else {
                error(
                    &format!(
                        "Invalid exit code: {:?}",
                        get_printable_form(&args[0])
                    ),
                    "",
                );
                (Expr::Null, true)
            }
        } else {
            (Expr::Null, false)
        }
    } else if x[0] == "io" {
        if y == "open" {
            assert_args_number!(y, args.len(), 1);
            if let Expr::String(filename) = &args[0] {
                OpenOptions::new()
                    .write(true)
                    .create(true)
                    .open(&filename)
                    .expect(error_msg!("Failed to check/create file"));
                (Expr::File(filename.to_string()), true)
            } else {
                error(
                    &format!(
                        "Invalid file name: {:?}",
                        get_printable_form(&args[0])
                    ),
                    "",
                );
                (Expr::Null, true)
            }
        } else {
            (Expr::Null, false)
        }
    } else {
        (Expr::Null, false)
    }
}
