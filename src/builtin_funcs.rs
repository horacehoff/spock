use crate::display::format_data;
use crate::util::get_type;
use crate::{Data, error_b};
use colored::Colorize;
use fnv::FnvHashMap;
use inline_colorization::*;
use internment::Intern;
use likely_stable::if_likely;

fn uppercase(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn lowercase(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! {let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn len(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    let tgt = consts[tgt as usize];
    if let Data::String(str) = tgt {
        consts[dest as usize] = Data::Number(str.chars().count() as f64)
    } else if let Data::Array(arr) = tgt {
        consts[dest as usize] = Data::Number(arrays[&arr].len() as f64)
    } else {
        error_b!(format_args!(
            "Cannot get length of type {color_red}{}{color_reset}",
            get_type(tgt)
        ));
    }
}

fn contains(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    let target = consts[tgt as usize];
    if let Data::String(str) = target {
        let arg = args.swap_remove(0);
        if_likely! { let Data::String(arg) = consts[arg as usize] => {
            consts[dest as usize] = Data::Bool(str.contains(arg.as_str()))
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }}
    } else if let Data::Array(x) = target {
        let arg = consts[args.swap_remove(0) as usize];
        consts[dest as usize] = Data::Bool(arrays[&x].contains(&arg))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn trim(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn trim_sequence(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }}
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn index(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    let target = consts[tgt as usize];
    if let Data::String(str) = target {
        let arg = consts[args.swap_remove(0) as usize];
        if_likely! { let Data::String(arg) = arg => {
            consts[dest as usize] = Data::Number(str.find(arg.as_str()).unwrap_or_else(|| {
                error_b!(format_args!("Cannot get index of {:?} in \"{}\"", arg.red(), str.blue()));
            }) as f64);
        } else {
            error_b!(format_args!(
                "{} is not a String",
                arg.to_string().red()
            ));
        }}
    } else if let Data::Array(x) = target {
        let arg = consts[args.swap_remove(0) as usize];
        consts[dest as usize] = Data::Number(arrays[&x].iter().position(|x| x == &arg).unwrap_or_else(|| {
            error_b!(format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{:?}{color_reset}", arg, format_data(target, arrays)));
        }) as f64);
    } else {
        error_b!(format_args!(
            "Cannot index type {color_red}{}{color_reset}",
            get_type(target)
        ));
    }
}

fn is_num(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Bool(str.parse::<f64>().is_ok())
    }}
}

fn trim_left(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_start().to_string()))
    }}
}

fn trim_right(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_end().to_string()))
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn trim_sequence_left(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_start_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{:?} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }}
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn trim_sequence_right(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_end_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{:?} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }}
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn rindex(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            consts[dest as usize] = Data::Number(str.rfind(arg.as_str()).unwrap() as f64);
        } else {
            error_b!(format_args!(
                "{} is not a String",
                consts[arg as usize].to_string().red()
            ));
        }}
    }}
}

fn repeat(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if let Data::String(str) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if_likely! { let Data::Number(arg) = consts[arg as usize] => {
            consts[dest as usize] = Data::String(Intern::from(str.repeat(arg as usize)))
        } else {
            error_b!(format_args!(
                "{:?} is not a Number",
                consts[arg as usize].to_string().red()
            ));
        }}
    } else if let Data::Array(x) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if_likely! { let Data::Number(arg) = consts[arg as usize] => {
            let id = arrays.len() as u16;
            arrays.insert(id, arrays[&x].repeat(arg as usize));
            consts[dest as usize] = Data::Array(id);
        } else {
            error_b!(format_args!(
                "{:?} is not a Number",
                consts[arg as usize].to_string().red()
            ));
        }}
    } else {
        error_b!(format_args!(
            "{:?} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn push(
    tgt: u16,
    _: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if let Data::Array(id) = consts[tgt as usize] {
        arrays
            .get_mut(&id)
            .unwrap()
            .push(consts[args.remove(0) as usize]);
    }
}

pub const FUNCS: [fn(u16, u16, &mut [Data], &mut Vec<u16>, &mut FnvHashMap<u16, Vec<Data>>); 15] = [
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
    push,
];
