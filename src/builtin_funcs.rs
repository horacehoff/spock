use crate::display::format_data;
use crate::util::format_type;
use crate::{Data, Instr, Num, error_b, is_float, parser_error};
use ariadne::*;
use inline_colorization::*;
use internment::Intern;
use likely_stable::if_likely;
use slab::Slab;
use std::fs::OpenOptions;
use std::io::Write;

macro_rules! builtin_error {
    ($instr_src: expr, $self_i:expr, $filename: expr, $src:expr, $general_error: expr, $msg:expr) => {
        let (_, start, end) = $instr_src.iter().find(|(x, _, _)| x == &$self_i).unwrap();
        parser_error!($filename, $src, *start, *end, $general_error, $msg);
    };
}

macro_rules! type_error {
    ($instr_src: expr, $self_i:expr, $filename: expr, $src:expr, $expected: expr, $received: expr) => {
        builtin_error!(
            $instr_src,
            $self_i,
            $filename,
            $src,
            "Invalid type",
            format_args!(
                "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                $expected,
                format_type($received),
            )
        );
    };
}

fn uppercase(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrs: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
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
    arrs: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
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
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = args.swap_remove(0);
            if_likely! { let Data::String(arg) = consts[arg as usize] => {
                consts[dest as usize] = Data::Bool(str.contains(arg.as_str()))
            } else {
                builtin_error!(instr_src, self_i, filename, src, "Invalid argument type", format_args!(
                "Expected string as argument, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                format_type(consts[arg as usize]),
            ));
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Bool(arrays[x].contains(&arg))
        }
        invalid => {
            type_error!(instr_src, self_i, filename, src, "string or array", invalid);
        }
    }
}

fn trim(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrs: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    } else {
        type_error!(instr_src, self_i, filename, src, "string", consts[tgt as usize]);
    }}
}

fn trim_sequence(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrs: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
        } else {
            builtin_error!(instr_src, self_i, filename, src, "Invalid argument type", format_args!(
                "Expected string as argument, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                format_type(consts[arg as usize]),
            ));
        }}
    } else {
        type_error!(instr_src, self_i, filename, src, "string", consts[tgt as usize]);
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
    self_i: Instr,
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
            consts[dest as usize] = Data::Number(arrays[x].iter().position(|x| x == &arg).unwrap_or_else(|| {
                error_b!(format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{:?}{color_reset}", arg, format_data(Data::Array(x), arrays)));
            }) as Num);
        }
        invalid => {
            type_error!(instr_src, self_i, filename, src, "string or array", invalid);
        }
    }
}

fn is_num(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Bool(str.parse::<f64>().is_ok())
    } else {
       type_error!(instr_src, self_i, filename, src, "string", consts[tgt as usize]);
    }}
}

fn trim_left(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_start().to_string()));
    } else {
       type_error!(instr_src, self_i, filename, src, "string", consts[tgt as usize]);
    }}
}

fn trim_right(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_end().to_string()))
    } else {
       type_error!(instr_src, self_i, filename, src, "string", consts[tgt as usize]);
    }}
}

fn trim_sequence_left(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrs: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_start_matches(&chars[..]).to_string()));
        } else {
            builtin_error!(instr_src, self_i, filename, src, "Invalid argument type", format_args!(
                "Expected string as argument, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                format_type(consts[arg as usize]),
            ));
        }}
    } else {
        type_error!(instr_src, self_i, filename, src, "string", consts[tgt as usize]);
    }}
}

fn trim_sequence_right(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrs: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = consts[args.swap_remove(0) as usize];
        if_likely!{ let Data::String(arg) = arg => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_end_matches(&chars[..]).to_string()));
        } else {
            builtin_error!(instr_src, self_i, filename, src, "Invalid argument type", format_args!(
                "Expected string as argument, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                format_type(arg),
            ));
        }}
    } else {
        type_error!(instr_src, self_i, filename, src, "string", consts[tgt as usize]);
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
    self_i: Instr,
) {
    match consts[tgt as usize] {
        Data::String(str) => {
            let arg = consts[args.swap_remove(0) as usize];
            if_likely! { let Data::String(arg) = arg => {
                consts[dest as usize] = Data::Number(str.rfind(arg.as_str()).unwrap_or_else(|| {
                    error_b!(format_args!("Cannot get index of {color_red}{:?}{color_reset} in \"{color_blue}{}{color_reset}\"", arg, str));
                }) as Num);
            } else {
                builtin_error!(instr_src, self_i, filename, src, "Invalid type", format_args!(
                    "Expected string, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                    format_type(arg),
                ));
            }}
        }
        Data::Array(x) => {
            let arg = consts[args.swap_remove(0) as usize];
            consts[dest as usize] = Data::Number(arrays[x].iter().rposition(|x| x == &arg).unwrap_or_else(|| {
                error_b!(format_args!("Cannot get index of {color_red}{}{color_reset} in {color_blue}{}{color_reset}", format_data(arg, arrays), format_data(Data::Array(x), arrays)));
            }) as Num);
        }
        invalid => {
            type_error!(instr_src, self_i, filename, src, "string or array", invalid);
        }
    }
}

fn repeat(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
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
                let id = arrays.insert(arrays[x].repeat(arg as usize));
                consts[dest as usize] = Data::Array(id);
            } else {
                error_b!(format_args!(
                "{color_red}{}{color_reset} is not a Number",
                    format_data(consts[arg as usize], arrays)
                ));
            }}
        }
        invalid => {
            type_error!(instr_src, self_i, filename, src, "string or array", invalid);
        }
    }
}

fn round(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.round(),num));
    } else {
        type_error!(instr_src, self_i, filename, src, "number", consts[tgt as usize]);
    }}
}

fn abs(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.abs(),num))
    } else {
        type_error!(instr_src, self_i, filename, src, "number", consts[tgt as usize]);
    }}
}

fn read(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
) {
    if_likely! {let Data::File(path) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(std::fs::read_to_string(path.as_str()).unwrap_or_else(|_| {
            error_b!(format_args!("Cannot read file {color_red}{path}{color_reset}"));
        })))
    } else {
        type_error!(instr_src, self_i, filename, src, "file", consts[tgt as usize]);
    }}
}

fn write(
    tgt: u16,
    _: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
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
        type_error!(instr_src, self_i, filename, src, "file", consts[tgt as usize]);
    }}
}

fn reverse(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut Slab<Vec<Data>>,
    instr_src: &[(Instr, usize, usize)],
    src: &str,
    filename: &str,
    self_i: Instr,
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
            arrays.get_mut(id).unwrap().reverse();
            consts[dest as usize] = Data::Array(id)
        }
        Data::String(str) => {
            consts[dest as usize] =
                Data::String(Intern::from(str.chars().rev().collect::<String>()))
        }
        invalid => {
            type_error!(instr_src, self_i, filename, src, "string or array", invalid);
        }
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
