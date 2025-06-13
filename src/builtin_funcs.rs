use crate::ArrayStorage;
use crate::display::format_data;
use crate::{Data, Instr, Num, is_float, parser_error};
use ariadne::*;
use inline_colorization::*;
use internment::Intern;
use likely_stable::if_likely;
use std::fs::OpenOptions;
use std::io::Write;

macro_rules! builtin_error {
    ($general_error: expr, $msg:expr, $instr_src:expr,$call:expr,$filename:expr,$src:expr) => {
        let (_, start, end) = $instr_src.iter().find(|(x, _, _)| x == &$call).unwrap();
        parser_error!($filename, $src, *start, *end, $general_error, $msg);
    };
}

pub fn uppercase(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
    }}
}

pub fn lowercase(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! {let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
    }}
}

pub fn contains(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut ArrayStorage,
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

pub fn trim(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
    }}
}

pub fn trim_sequence(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
        }}
    }}
}

pub fn index(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut ArrayStorage,
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

pub fn is_num(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Bool(str.parse::<f64>().is_ok())
    }}
}

pub fn trim_left(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_start().to_string()));
    }}
}

pub fn trim_right(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        consts[dest as usize] = Data::String(Intern::from(str.trim_end().to_string()))
    }}
}

pub fn trim_sequence_left(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = args.swap_remove(0);
        if_likely!{ let Data::String(arg) = consts[arg as usize] => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_start_matches(&chars[..]).to_string()));
        }}
    }}
}

pub fn trim_sequence_right(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
    if_likely! { let Data::String(str) = consts[tgt as usize] => {
        let arg = consts[args.swap_remove(0) as usize];
        if_likely!{ let Data::String(arg) = arg => {
            let chars: Vec<char> = arg.chars().collect();
            consts[dest as usize] =
                Data::String(Intern::from(str.trim_end_matches(&chars[..]).to_string()));
        }}
    }}
}

pub fn rindex(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut ArrayStorage,
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

pub fn repeat(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    arrays: &mut ArrayStorage,
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

pub fn round(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.round(),num));
    }}
}

pub fn abs(tgt: u16, dest: u16, consts: &mut [Data]) {
    if_likely! {let Data::Number(num) = consts[tgt as usize] => {
        consts[dest as usize] = Data::Number(is_float!(num.abs(),num))
    }}
}

pub fn read(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    _: &mut ArrayStorage,
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

pub fn write(
    tgt: u16,
    _: u16,
    consts: &mut [Data],
    args: &mut Vec<u16>,
    _: &mut ArrayStorage,
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

pub fn reverse(
    tgt: u16,
    dest: u16,
    consts: &mut [Data],
    _: &mut Vec<u16>,
    arrays: &mut ArrayStorage,
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
// pub fn uppercase(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::String(Intern::from(str.to_uppercase()))
//     }}
// }

// pub fn lowercase(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! {let Data::String(str) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::String(Intern::from(str.to_lowercase()))
//     }}
// }

// pub fn contains(
//     tgt: u16,
//     dest: u16,
//     consts: &mut [Data],
//     args: &mut Vec<u16>,
//     arrays: &mut ArrayStorage,
// ) {
//     match consts[tgt as usize] {
//         Data::String(str) => {
//             let arg = args.swap_remove(0);
//             if_likely! { let Data::String(arg) = consts[arg as usize] => {
//                 consts[dest as usize] = Data::Bool(str.contains(arg.as_str()))
//             }}
//         }
//         Data::Array(x) => {
//             let arg = consts[args.swap_remove(0) as usize];
//             consts[dest as usize] = Data::Bool(arrays[x].contains(&arg))
//         }
//         _ => unreachable!(),
//     }
// }

// pub fn trim(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::String(Intern::from(str.trim().to_string()))
//     }}
// }

// pub fn trim_sequence(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         let arg = args.swap_remove(0);
//         if_likely!{ let Data::String(arg) = consts[arg as usize] => {
//             let chars: Vec<char> = arg.chars().collect();
//             consts[dest as usize] =
//                 Data::String(Intern::from(str.trim_matches(&chars[..]).to_string()));
//         }}
//     }}
// }

// pub fn index(
//     tgt: u16,
//     dest: u16,
//     consts: &mut [Data],
//     args: &mut Vec<u16>,
//     arrays: &mut ArrayStorage,
//     instr_src: &[(Instr, usize, usize)],
//     src: &str,
//     filename: &str,
//     call: Instr,
// ) {
//     match consts[tgt as usize] {
//         Data::String(str) => {
//             let arg = consts[args.swap_remove(0) as usize];
//             if_likely! { let Data::String(arg) = arg => {
//                 consts[dest as usize] = Data::Number(str.find(arg.as_str()).unwrap_or_else(|| {
//                     builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}\"{}\"{color_reset}", arg, str),instr_src,call,filename,src);
//                 }) as Num);
//             }}
//         }
//         Data::Array(x) => {
//             let arg = consts[args.swap_remove(0) as usize];
//             consts[dest as usize] = Data::Number(arrays[x].iter().position(|x| x == &arg).unwrap_or_else(|| {
//                 builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{}{color_reset}", arg, format_data(Data::Array(x), arrays,true)),instr_src,call,filename,src);
//             }) as Num);
//         }
//         _ => unreachable!(),
//     }
// }

// pub fn is_num(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::Bool(str.parse::<f64>().is_ok())
//     }}
// }

// pub fn trim_left(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::String(Intern::from(str.trim_start().to_string()));
//     }}
// }

// pub fn trim_right(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::String(Intern::from(str.trim_end().to_string()))
//     }}
// }

// pub fn trim_sequence_left(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         let arg = args.swap_remove(0);
//         if_likely!{ let Data::String(arg) = consts[arg as usize] => {
//             let chars: Vec<char> = arg.chars().collect();
//             consts[dest as usize] =
//                 Data::String(Intern::from(str.trim_start_matches(&chars[..]).to_string()));
//         }}
//     }}
// }

// pub fn trim_sequence_right(tgt: u16, dest: u16, consts: &mut [Data], args: &mut Vec<u16>) {
//     if_likely! { let Data::String(str) = consts[tgt as usize] => {
//         let arg = consts[args.swap_remove(0) as usize];
//         if_likely!{ let Data::String(arg) = arg => {
//             let chars: Vec<char> = arg.chars().collect();
//             consts[dest as usize] =
//                 Data::String(Intern::from(str.trim_end_matches(&chars[..]).to_string()));
//         }}
//     }}
// }

// pub fn rindex(
//     tgt: u16,
//     dest: u16,
//     consts: &mut [Data],
//     args: &mut Vec<u16>,
//     arrays: &mut ArrayStorage,
//     instr_src: &[(Instr, usize, usize)],
//     src: &str,
//     filename: &str,
//     call: Instr,
// ) {
//     match consts[tgt as usize] {
//         Data::String(str) => {
//             let arg = consts[args.swap_remove(0) as usize];
//             if_likely! { let Data::String(arg) = arg => {
//                 consts[dest as usize] = Data::Number(str.rfind(arg.as_str()).unwrap_or_else(|| {
//                     builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}\"{}\"{color_reset}", arg, str),instr_src,call,filename,src);
//                 }) as Num);
//             }}
//         }
//         Data::Array(x) => {
//             let arg = consts[args.swap_remove(0) as usize];
//             consts[dest as usize] = Data::Number(arrays[x].iter().rposition(|x| x == &arg).unwrap_or_else(|| {
//                 builtin_error!("Item not found",format_args!("Cannot get index of {color_red}{:?}{color_reset} in {color_blue}{}{color_reset}", arg, format_data(Data::Array(x), arrays,true)),instr_src,call,filename,src);
//             }) as Num);
//         }
//         _ => unreachable!(),
//     }
// }

// pub fn repeat(
//     tgt: u16,
//     dest: u16,
//     consts: &mut [Data],
//     args: &mut Vec<u16>,
//     arrays: &mut ArrayStorage,
// ) {
//     match consts[tgt as usize] {
//         Data::String(str) => {
//             let arg = args.swap_remove(0);
//             if_likely! { let Data::Number(arg) = consts[arg as usize] => {
//                 consts[dest as usize] = Data::String(Intern::from(str.repeat(arg as usize)))
//             }}
//         }
//         Data::Array(x) => {
//             let arg = args.swap_remove(0);
//             if_likely! { let Data::Number(arg) = consts[arg as usize] => {
//                 consts[dest as usize] = Data::Array(arrays.insert(arrays[x].repeat(arg as usize)));
//             }}
//         }
//         _ => unreachable!(),
//     }
// }

// pub fn round(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! {let Data::Number(num) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::Number(is_float!(num.round(),num));
//     }}
// }

// pub fn abs(tgt: u16, dest: u16, consts: &mut [Data]) {
//     if_likely! {let Data::Number(num) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::Number(is_float!(num.abs(),num))
//     }}
// }

// pub fn read(
//     tgt: u16,
//     dest: u16,
//     consts: &mut [Data],
//     _: &mut Vec<u16>,
//     _: &mut ArrayStorage,
//     instr_src: &[(Instr, usize, usize)],
//     src: &str,
//     filename: &str,
//     call: Instr,
// ) {
//     if_likely! {let Data::File(path) = consts[tgt as usize] => {
//         consts[dest as usize] = Data::String(Intern::from(std::fs::read_to_string(path.as_str()).unwrap_or_else(|_| {
//             builtin_error!("File does not exist or cannot be read",format_args!("Cannot read file {color_red}{path}{color_reset}"),instr_src,call,filename,src);
//         })))
//     }}
// }

// pub fn write(
//     tgt: u16,
//     _: u16,
//     consts: &mut [Data],
//     args: &mut Vec<u16>,
//     _: &mut ArrayStorage,
//     instr_src: &[(Instr, usize, usize)],
//     src: &str,
//     filename: &str,
//     call: Instr,
// ) {
//     if_likely! {let Data::File(path) = consts[tgt as usize] => {
//         if_likely!{let Data::String(contents) = consts[args.swap_remove(0) as usize] => {
//             if_likely!{let Data::Bool(truncate) = consts[args.swap_remove(0) as usize] => {
//                 OpenOptions::new()
//                     .write(true)
//                     .truncate(truncate)
//                     .open(path.as_str()).unwrap_or_else(|_| {
//                         builtin_error!("File does not exist or cannot be opened",format_args!("Cannot open file {color_red}{path}{color_reset}"),instr_src,call,filename,src);
//                     }).write_all(contents.as_bytes()).unwrap_or_else(|_| {
//                         builtin_error!("File does not exist or cannot be written to",format_args!("Cannot write {color_red}{path}{color_reset} to file {color_blue}{path}{color_reset}"),instr_src,call,filename,src);
//                 });
//             }}
//         }}
//     }}
// }

// pub fn reverse(
//     tgt: u16,
//     dest: u16,
//     consts: &mut [Data],
//     _: &mut Vec<u16>,
//     arrays: &mut ArrayStorage,
// ) {
//     match consts[tgt as usize] {
//         Data::Array(id) => {
//             arrays.get_mut(id).unwrap().reverse();
//             consts[dest as usize] = Data::Array(id)
//         }
//         Data::String(str) => {
//             consts[dest as usize] =
//                 Data::String(Intern::from(str.chars().rev().collect::<String>()))
//         }
//         _ => unreachable!(),
//     }
// }

// pub const FUNCS: [fn(
//     u16,
//     u16,
//     &mut [Data],
//     &mut Vec<u16>,
//     &mut ArrayStorage,
//     &[(Instr, usize, usize)],
//     &str,
//     &str,
//     Instr,
// ); 18] = [
//     uppercase,
//     lowercase,
//     contains,
//     trim,
//     trim_sequence,
//     index,
//     is_num,
//     trim_left,
//     trim_right,
//     trim_sequence_left,
//     trim_sequence_right,
//     rindex,
//     repeat,
//     round,
//     abs,
//     read,
//     write,
//     reverse,
// ];

#[macro_export]
macro_rules! builtin_funcs_add {
    ($id: expr,$tgt: expr,
    $dest: expr,
    $consts: expr,
    $args: expr,
    $arrays: expr,
    $instr_src: expr,
    $src: expr,
    $filename: expr,
    $call: expr) => {
        match $id {
            0 => uppercase($tgt, $dest, $consts),
            1 => lowercase($tgt, $dest, $consts),
            2 => contains($tgt, $dest, $consts, $args, $arrays),
            3 => trim($tgt, $dest, $consts),
            4 => trim_sequence($tgt, $dest, $consts, $args),
            5 => index(
                $tgt, $dest, $consts, $args, $arrays, $instr_src, $src, $filename, $call,
            ),
            6 => is_num($tgt, $dest, $consts),
            7 => trim_left($tgt, $dest, $consts),
            8 => trim_right($tgt, $dest, $consts),
            9 => trim_sequence_left($tgt, $dest, $consts, $args),
            10 => trim_sequence_right($tgt, $dest, $consts, $args),
            11 => rindex(
                $tgt, $dest, $consts, $args, $arrays, $instr_src, $src, $filename, $call,
            ),
            12 => repeat($tgt, $dest, $consts, $args, $arrays),
            13 => round($tgt, $dest, $consts),
            14 => abs($tgt, $dest, $consts),
            15 => read(
                $tgt, $dest, $consts, $args, $arrays, $instr_src, $src, $filename, $call,
            ),
            16 => write(
                $tgt, $dest, $consts, $args, $arrays, $instr_src, $src, $filename, $call,
            ),
            17 => reverse($tgt, $dest, $consts, $args, $arrays),
            _ => unreachable!(),
        }
    };
}
