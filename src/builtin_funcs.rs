use crate::display::format_data;
use crate::{Data, Instr, Num, is_float, parser_error};
use ariadne::*;
use inline_colorization::*;
use internment::Intern;
use likely_stable::if_likely;
use slab::Slab;
use std::fs::OpenOptions;
use std::hint::unreachable_unchecked;
use std::io::Write;

macro_rules! builtin_error {
    ($general_error: expr, $msg:expr, $instr_src:expr,$call:expr,$filename:expr,$src:expr) => {
        let (_, start, end) = $instr_src.iter().find(|(x, _, _)| x == &$call).unwrap();
        parser_error!($filename, $src, *start, *end, $general_error, $msg);
    };
}

fn uppercase(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    }}
}

fn lowercase(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! {let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    }}
}

fn contains(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = args.swap_remove(0);
            if_likely! { let Data::String(arg) = consts[arg as usize] => {
                consts[dest as usize] = Data::Bool(str.contains(arg.as_str()))
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Bool(arrays[x].contains(&arg))
        }
        _ => unreachable!(),
    }
}

fn trim(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    }}
}

fn trim_sequence(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
        }}
    }}
}

fn index(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    call: Instr,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = consts[args.swap_remove(0) as usize];
            if_likely! { let Data::String(arg) = arg => {
                consts[dest as usize] = Data::Number(str.find(arg.as_str()).unwrap_or_else(|| {
                    builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}\"{}\"{color_reset}", arg, str),instr_src,call,filename,src);
                }) as Num);
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Number(arrays[x].iter().position(|x| x == &arg).unwrap_or_else(|| {
                builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{}{color_reset}", arg, format_data(Data::Array(x), arrays,true)),instr_src,call,filename,src);
            }) as Num);
        }
        _ => unreachable!(),
    }
}

fn is_num(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
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
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_start().to_string()));
    }}
}

fn trim_right(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_end().to_string()))
    }}
}

fn trim_sequence_left(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_start_matches(&chars[..]).to_string()));
        }}
    }}
}

fn trim_sequence_right(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = consts[args.swap_remove(0) as usize];
        if_likely!{ let Data::String(arg) = arg => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_end_matches(&chars[..]).to_string()));
        }}
    }}
}

fn rindex(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    call: Instr,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = consts[args.swap_remove(0) as usize];
            if_likely! { let Data::String(arg) = arg => {
                consts[dest as usize] = Data::Number(str.rfind(arg.as_str()).unwrap_or_else(|| {
                    builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}\"{}\"{color_reset}", arg, str),instr_src,call,filename,src);
                }) as Num);
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Number(arrays[x].iter().rposition(|x| x == &arg).unwrap_or_else(|| {
                builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{}{color_reset}", arg, format_data(Data::Array(x), arrays,true)),instr_src,call,filename,src);
            }) as Num);
        }
        _ => unreachable!(),
    }
}

fn repeat(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = args.swap_remove(0);
            if_likely! { let Data::Number(arg) = consts[arg as usize] => {
                consts[dest as usize] = Data::String(Intern::from(str.repeat(arg as usize)))
            }}
        }
        Data::Array(x) => {
            let arg = args.swap_remove(0);
            if_likely! { let Data::Number(arg) = consts[arg as usize] => {
                consts[dest as usize] = Data::Array(arrays.insert(arrays[x].repeat(arg as usize)));
            }}
        }
        _ => unreachable!(),
    }
}

fn round(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.round(),num));
    }}
}

fn abs(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.abs(),num))
    }}
}

fn read(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    call: Instr,
) {
    if_likely! {let Data::File(path) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(std::fs::read_to_string(path.as_str()).unwrap_or_else(|_| {
            builtin_error!("File does not exist or cannot be read",format_args!("Cannot read file {color_red}{path}{color_reset}"),instr_src,call,filename,src);
        })))
    }}
}

fn write(
    tgt: u16,
    _: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    call: Instr,
) {
    if_likely! {let Data::File(path) = consts[tgt as usize] => {
        if_likely!{let Data::String(contents) = consts[args.swap_remove(0) as usize] => {
            if_likely!{let Data::Bool(truncate) = consts[args.swap_remove(0) as usize] => {
                OpenOptions::new()
                    .write(true)
                    .truncate(truncate)
                    .open(path.as_str()).unwrap_or_else(|_| {
                        builtin_error!("File does not exist or cannot be opened",format_args!("Cannot open file {color_red}{path}{color_reset}"),instr_src,call,filename,src);
                    }).write_all(contents.as_bytes()).unwrap_or_else(|_| {
                        builtin_error!("File does not exist or cannot be written to",format_args!("Cannot write {color_red}{path}{color_reset} to file {color_blue}{path}{color_reset}"),instr_src,call,filename,src);
                });
            }}
        }}
    }}
}

fn reverse(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    _: &[(Instr, usize, usize)],
    _: &str,
    _: &str,
    _: Instr,
) {
    match consts[tgt as usize] {
        Data::Array(id) => {
            arrays.get_mut(id).unwrap().reverse();
            consts[dest as usize] = Data::Array(id)
        }
        Data::String(str) => {
            consts[dest as usize] =
                Data::String(Intern::from(str.chars().rev().collect::<String>()))
        }
        _ => unreachable!(),
    }
}

pub const FUNCS: [fn(
    u16,
    u16,
    &mut [Data],
    &mut Vec<u16>,
    &mut Slab<Vec<Data>>,
    &[(Instr, usize, usize)],
    &str,
    &str,
    Instr,
); 18] = [
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
