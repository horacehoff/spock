use crate::{Data, error_b};
use colored::Colorize;
use inline_colorization::*;
use internment::Intern;

fn uppercase(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    }
}

fn lowercase(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    }
}

fn len(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::Number(str.chars().count() as f64)
    }
}

fn contains(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
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

fn trim(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    }
}

fn trim_sequence(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if let Data::String(arg) = &consts[arg as usize] {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }
    }
}

pub const FUNCS: [fn(u16, u16, &mut [Data], &mut Vec<u16>); 6] =
    [uppercase, lowercase, len, contains, trim, trim_sequence];
