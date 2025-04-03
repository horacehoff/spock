use inline_colorization::*;
use colored::Colorize;
use internment::Intern;
use crate::{error_b, Data};

fn uppercase(tgt: u16, dest: u16, arg: u16, consts: &mut [Data]) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    }
}

fn lowercase(tgt: u16, dest: u16, arg: u16, consts: &mut [Data]) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    }
}

fn len(tgt: u16, dest: u16, arg: u16, consts: &mut [Data]) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::Number(str.chars().count() as f64)
    }
}

fn contains(tgt: u16, dest: u16, arg: u16, consts: &mut [Data]) {
    if let Data::String(str) = consts[tgt as usize] {
        if let Data::String(arg) = consts[arg as usize] {
            consts[dest as usize] = Data::Bool(str.contains(arg.as_str()))
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }
    }
}

pub const FUNCS: [fn(u16, u16, u16, &mut [Data]); 4] = [uppercase, lowercase, len, contains];