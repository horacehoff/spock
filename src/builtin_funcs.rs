use crate::display::format_data;
use crate::{Data, error_b};
use colored::Colorize;
use inline_colorization::*;
use internment::Intern;
use likely_stable::if_likely;

fn uppercase(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>, _: &mut Vec<Data>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn lowercase(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>, _: &mut Vec<Data>) {
    if_likely! {let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn len(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>, _: &mut Vec<Data>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(str.chars().count() as f64)
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn contains(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>, arrays: &mut Vec<Data>) {
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
    } else if let Data::Array(x, y) = target {
        let arg = consts[args.swap_remove(0) as usize];
        consts[dest as usize] = Data::Bool(arrays[x as usize..y as usize].contains(&arg));
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }
}

fn trim(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>, _: &mut Vec<Data>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    } else {
        error_b!(format_args!(
            "{} is not a String",
            consts[tgt as usize].to_string().red()
        ));
    }}
}

fn trim_sequence(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>, _: &mut Vec<Data>) {
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

fn index(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>, arrays: &mut Vec<Data>) {
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
    } else if let Data::Array(x, y) = target {
        let arg = consts[args.swap_remove(0) as usize];
        consts[dest as usize] = Data::Number(arrays[x as usize..y as usize].iter().position(|x| x == &arg).unwrap_or_else(|| {
            error_b!(format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{:?}{color_reset}", arg, format_data(target, arrays)));
        }) as f64);
    } else {
        error_b!(format_args!("{} is not a String", target.to_string().red()));
    }
}

fn is_num(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>, _: &mut Vec<Data>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Bool(str.parse::<f64>().is_ok())
    }}
}

fn trim_left(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>, _: &mut Vec<Data>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_start().to_string()))
    }}
}

fn trim_right(tgt: u16, dest: u16, consts: &mut [Data], _: &mut Vec<u16>, _: &mut Vec<Data>) {
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
    _: &mut Vec<Data>,
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
    _: &mut Vec<Data>,
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

fn rindex(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>, _: &mut Vec<Data>) {
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

fn repeat(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>, arrays: &mut Vec<Data>) {
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
    } else if let Data::Array(x, y) = consts[tgt as usize] {
        let arg = args.swap_remove(0);
        if_likely! { let Data::Number(arg) = consts[arg as usize] => {
            let arr = arrays[x as usize..y as usize].repeat(arg as usize);
            let index_i = arrays.len() as u16;
            arrays.extend(arr);
            consts[dest as usize] = Data::Array(index_i,(arrays.len()) as u16)
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

fn push(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>, arrays: &mut Vec<Data>) {
    if let Data::Array(start, end) = consts[tgt as usize] {
        let mut arr: Vec<Data> = arrays[start as usize..end as usize].to_vec();
        print!("GOT {arr:?}");
        arr.push(consts[args.remove(0) as usize]);
        let a = arrays.len();
        arrays.extend(arr);
        let b = arrays.len() - 1;
        consts[dest as usize] = Data::Array(a as u16, b as u16);
        print!("ARR {arrays:?}");
        print!("CONSTS {consts:?}")
    }
}

pub const FUNCS: [fn(u16, u16, &mut [Data], &mut Vec<u16>, &mut Vec<Data>); 15] = [
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
