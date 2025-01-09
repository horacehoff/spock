use crate::error;
use crate::parser::ParserInstr;
use crate::{assert_args_number, get_printable_form};
use internment::Intern;
use std::fs::OpenOptions;
use std::process::exit;
// use smartstring::alias::String;

pub fn namespace_functions(x: &[String], y: &str, args: &[ParserInstr]) -> (ParserInstr, bool) {
    if x[0] == "compute" {
        if y == "exit" {
            assert_args_number!(y, args.len(), 1);
            if let ParserInstr::Integer(excode) = args[0] {
                exit(excode as i32);
            } else {
                error(
                    &format!("Invalid exit code: {:?}", get_printable_form(&args[0])),
                    "",
                );
                (ParserInstr::Null, true)
            }
        } else {
            (ParserInstr::Null, false)
        }
    } else if x[0] == "io" {
        if y == "open" {
            assert_args_number!(y, args.len(), 1);
            if let ParserInstr::String(filename) = &args[0] {
                OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(false)
                    .open(filename)
                    .unwrap_or_else(|_| {
                        error("Failed to check/create file", "");
                        std::process::exit(1)
                    });
                (ParserInstr::File(Intern::from(filename.to_string())), true)
            } else {
                error(
                    &format!("Invalid file name: {:?}", get_printable_form(&args[0])),
                    "",
                );
                (ParserInstr::Null, true)
            }
        } else {
            (ParserInstr::Null, false)
        }
    } else {
        (ParserInstr::Null, false)
    }
}
