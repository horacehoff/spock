use crate::error;
use crate::parser::Types;
use crate::{assert_args_number, get_printable_form};
use smol_str::{SmolStr, ToSmolStr};
use std::fs::OpenOptions;
use std::process::exit;

pub fn namespace_functions(x: &[SmolStr], y: &str, args: &[Types]) -> (Types, bool) {
    if x[0] == "compute" {
        if y == "exit" {
            assert_args_number!(y, args.len(), 1);
            if let Types::Integer(excode) = args[0] {
                exit(excode as i32);
            } else {
                error(
                    &format!("Invalid exit code: {:?}", get_printable_form(&args[0])),
                    "",
                );
                (Types::Null, true)
            }
        } else {
            (Types::Null, false)
        }
    } else if x[0] == "io" {
        if y == "open" {
            assert_args_number!(y, args.len(), 1);
            if let Types::String(filename) = &args[0] {
                OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(false)
                    .open(filename)
                    .unwrap_or_else(|_| {
                        error("Failed to check/create file", "");
                        std::process::exit(1)
                    });
                (Types::File(filename.to_smolstr()), true)
            } else {
                error(
                    &format!("Invalid file name: {:?}", get_printable_form(&args[0])),
                    "",
                );
                (Types::Null, true)
            }
        } else {
            (Types::Null, false)
        }
    } else {
        (Types::Null, false)
    }
}
