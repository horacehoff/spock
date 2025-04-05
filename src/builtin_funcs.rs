use crate::{Data, error_b};
use colored::Colorize;
use inline_colorization::*;
use internment::Intern;

fn uppercase(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn lowercase(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn len(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::Number(str.chars().count() as f64)
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
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
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn trim(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn trim_sequence(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if let Data::String(arg) = consts[arg as usize] {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn index(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if let Data::String(arg) = consts[arg as usize] {
            consts[dest as usize] = Data::Number(str.find(arg.as_str()).unwrap() as f64);
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn is_num(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::Bool(str.parse::<f64>().is_ok())
    }
}

fn trim_left(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.trim_start().to_string()))
    }
}

fn trim_right(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        consts[dest as usize] = Data::String(Intern::from(str.trim_end().to_string()))
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn trim_sequence_left(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if let Data::String(arg) = consts[arg as usize] {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_start_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{:?} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn trim_sequence_right(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if let Data::String(arg) = consts[arg as usize] {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_end_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{:?} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn rindex(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if let Data::String(arg) = consts[arg as usize] {
            consts[dest as usize] = Data::Number(str.rfind(arg.as_str()).unwrap() as f64);
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }
    }
}

fn repeat(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if let Data::Number(arg) = consts[arg as usize] {
            consts[dest as usize] = Data::String(Intern::from(str.repeat(arg as usize)))
        } else {
            error_b!(format_args!(
                "{:?} is not a Number",
                consts[arg as usize].to_string().red()
            ));
        }
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

pub const FUNCS: [fn(u16, u16, &mut [Data], &mut Vec<u16>); 14] = [
    uppercase,
    lowercase,
    len,
    contains,
    trim,
    trim_sequence,
    index,
    is_num,
    trim_left,
    trim_right,
    trim_sequence_left,
    trim_sequence_right,
    rindex,
    repeat,
];
