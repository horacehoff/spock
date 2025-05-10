use crate::display::format_data;
use crate::util::get_type;
use crate::{Data, Num, error_b, is_float};
use fnv::FnvHashMap;
use inline_colorization::*;
use internment::Intern;
use likely_stable::if_likely;
use std::fs::OpenOptions;
use std::io::Write;

fn uppercase(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    } else {
        error_b!(format_args!(
            "{color_red}{}{color_reset} is not a String",
            format_data(consts[tgt as usize], arrs)
        ));
    }}
}

fn lowercase(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! {let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    } else {
        error_b!(format_args!(
            "{color_red}{}{color_reset} is not a String",
            format_data(consts[tgt as usize], arrs)
        ));
    }}
}

fn contains(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = args.swap_remove(0);
            if_likely! { let Data::String(arg) = consts[arg as usize] => {
                consts[dest as usize] = Data::Bool(str.contains(arg.as_str()))
            } else {
                error_b!(format_args!(
                    "{color_red}{}{color_reset} is not a String",
                    format_data(consts[arg as usize], arrays)
                ));
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Bool(arrays[&x].contains(&arg))
        }
        invalid => {
            error_b!(format_args!(
                "{color_red}{}{color_reset} is not a String",
                format_data(invalid, arrays)
            ));
        }
    }
}

fn trim(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    } else {
        error_b!(format_args!(
            "{color_red}{}{color_reset} is not a String",
            format_data(consts[tgt as usize], arrs)
        ));
    }}
}

fn trim_sequence(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{color_red}{}{color_reset} is not a String",
                format_data(consts[arg as usize], arrs)
            ));
        }}
    } else {
        error_b!(format_args!(
            "{color_red}{}{color_reset} is not a String",
            format_data(consts[tgt as usize], arrs)
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
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = consts[args.swap_remove(0) as usize];
            if_likely! { let Data::String(arg) = arg => {
                consts[dest as usize] = Data::Number(str.find(arg.as_str()).unwrap_or_else(|| {
                    error_b!(format_args!("Cannot get index of {color_red}{:?}{color_reset} in \"{color_blue}{}{color_reset}\"", arg, str));
                }) as Num);
            } else {
                error_b!(format_args!(
                    "{color_red}{}{color_reset} is not a String",
                    format_data(arg, arrays)
                ));
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Number(arrays[&x].iter().position(|x| x == &arg).unwrap_or_else(|| {
                error_b!(format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{:?}{color_reset}", arg, format_data(Data::Array(x), arrays)));
            }) as Num);
        }
        invalid => {
            error_b!(format_args!(
                "Cannot index type {color_red}{}{color_reset}",
                get_type(invalid)
            ));
        }
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
            "{color_red}{:?}{color_reset} is not a String",
            consts[tgt as usize]
        ));
    }}
}

fn trim_sequence_left(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_start_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{color_red}{}{color_reset} is not a String",
                format_data(consts[arg as usize], arrs)
            ));
        }}
    } else {
        error_b!(format_args!(
            "{color_red}{}{color_reset} is not a String",
            format_data(consts[tgt as usize], arrs)
        ));
    }}
}

fn trim_sequence_right(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_end_matches(&chars[..]).to_string()));
        } else {
            error_b!(format_args!(
                "{color_red}{}{color_reset} is not a String",
                format_data(consts[arg as usize], arrs)
            ));
        }}
    } else {
        error_b!(format_args!(
            "{color_red}{:?}{color_blue} is not a String",
            format_data(consts[tgt as usize], arrs)
        ));
    }}
}

fn rindex(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = consts[args.swap_remove(0) as usize];
            if_likely! { let Data::String(arg) = arg => {
                consts[dest as usize] = Data::Number(str.rfind(arg.as_str()).unwrap_or_else(|| {
                    error_b!(format_args!("Cannot get index of {color_red}{:?}{color_reset} in \"{color_blue}{}{color_reset}\"", arg, str));
                }) as Num);
            } else {
                error_b!(format_args!(
                    "{color_red}{}{color_reset} is not a String",
                    format_data(arg, arrays)
                ));
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Number(arrays[&x].iter().rposition(|x| x == &arg).unwrap_or_else(|| {
                error_b!(format_args!("Cannot get index of {color_red}{}{color_reset} in {color_blue}{}{color_reset}", format_data(arg, arrays), format_data(Data::Array(x), arrays)));
            }) as Num);
        }
        invalid => {
            error_b!(format_args!(
                "Cannot index type {color_red}{}{color_reset}",
                get_type(invalid)
            ));
        }
    }
}

fn repeat(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = args.swap_remove(0);
            if_likely! { let Data::Number(arg) = consts[arg as usize] => {
                consts[dest as usize] = Data::String(Intern::from(str.repeat(arg as usize)))
            } else {
                error_b!(format_args!(
                    "{color_red}{}{color_reset} is not a Number",
                    format_data(consts[arg as usize], arrays)
                ));
            }}
        }
        Data::Array(x) => {
            let arg = args.swap_remove(0);
            if_likely! { let Data::Number(arg) = consts[arg as usize] => {
                let id = arrays.len() as u16;
                arrays.insert(id, arrays[&x].repeat(arg as usize));
                consts[dest as usize] = Data::Array(id);
            } else {
                error_b!(format_args!(
                "{color_red}{}{color_reset} is not a Number",
                    format_data(consts[arg as usize], arrays)
                ));
            }}
        }
        invalid => {
            error_b!(format_args!(
                "{color_red}{}{color_reset} is not a String",
                format_data(invalid, arrays)
            ));
        }
    }
}

fn round(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.round(),num));
    } else {
        error_b!(format_args!("Cannot round {color_red}{}{color_reset}", format_data(consts[tgt as usize], arrays)));
    }}
}

fn abs(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.abs(),num))
    } else {
        error_b!(format_args!("Cannot get absolute value of {color_red}{}{color_reset}", format_data(consts[tgt as usize], arrays)));
    }}
}

fn read(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! {let Data::File(path) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(std::fs::read_to_string(path.as_str()).unwrap_or_else(|_| {
            error_b!(format_args!("Cannot read file {color_red}{path}{color_reset}"));
        })))
    } else {
        error_b!(format_args!("Cannot get absolute value of {color_red}{}{color_reset}", format_data(consts[tgt as usize], arrays)));
    }}
}

fn write(
    tgt: u16,
    _: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    if_likely! {let Data::File(path) = consts[tgt as usize] => {
        if_likely!{let Data::String(contents) = consts[args.swap_remove(0) as usize] => {
            if_likely!{let Data::Bool(truncate) = consts[args.swap_remove(0) as usize] => {
                OpenOptions::new()
                    .write(true)
                    .truncate(truncate)
                    .open(path.as_str()).unwrap_or_else(|_| {
                        error_b!(format_args!("Cannot open file {color_red}{path}{color_reset}"));
                    }).write_all(contents.as_bytes()).unwrap_or_else(|_| {
                        error_b!(format_args!("Cannot write {color_red}{path}{color_reset} to file {color_blue}{path}{color_reset}"));
                });
            }}
        }}
    } else {
        error_b!(format_args!("Invalid file: {color_red}{}{color_reset}", format_data(consts[tgt as usize], arrays)));
    }}
}

fn reverse(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
) {
    match consts[tgt as usize] {
        Data::Array(id) => {
            // let array_id = arrays.len();
            // let mut array = arrays[&id].to_vec();
            // array.reverse();
            // arrays.insert(array_id as u16, array);
            //
            // reverses the array and returns it, maybe a bad idea?
            //
            arrays.get_mut(&id).unwrap().reverse();
            consts[dest as usize] = Data::Array(id)
        }
        Data::String(str) => {
            consts[dest as usize] =
                Data::String(Intern::from(str.chars().rev().collect::<String>()))
        }
        invalid => {
            error_b!(format_args!(
                "Cannot reverse type {color_red}{}{color_reset}",
                get_type(invalid)
            ));
        }
    }
}

pub const FUNCS: [fn(u16, u16, &mut [Data], &mut Vec<u16>, &mut FnvHashMap<u16, Vec<Data>>); 18] = [
    uppercase,
    lowercase,
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
    round,
    abs,
    read,
    write,
    reverse,
];
