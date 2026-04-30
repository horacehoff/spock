use crate::type_system::DataType;
use smol_str::SmolStr;
use smol_str::ToSmolStr;
use std::hint::cold_path;

#[inline(always)]
pub const fn likely(b: bool) -> bool {
    if !b {
        cold_path();
    }
    b
}

#[inline(always)]
pub const fn unlikely(b: bool) -> bool {
    if b {
        cold_path();
    }
    b
}

#[macro_export]
macro_rules! debug {
    ($($x:tt)*) => {
        #[cfg(debug_assertions)]
        println!("[DEBUG] {}", format_args!($($x)*))
    }
}

pub fn str_to_type(s: &str) -> DataType {
    if s == "int" {
        DataType::Int
    } else if s == "float" {
        DataType::Float
    } else if s == "bool" {
        DataType::Bool
    } else if s == "string" {
        DataType::String
    } else {
        unreachable!()
    }
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Float => write!(f, "Float"),
            DataType::Int => write!(f, "Integer"),
            DataType::Bool => write!(f, "Boolean"),
            DataType::String => write!(f, "String"),
            DataType::Array(array_type) => match array_type {
                Some(t) => write!(f, "Array<{}>", t),
                None => write!(f, "Array<?>"),
            },
            DataType::Null => write!(f, "Null"),
            DataType::File => write!(f, "File"),
            DataType::Poly(types) => write!(
                f,
                "{}",
                types
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("|")
            ),
            DataType::Fn(t) => {
                write!(
                    f,
                    "({}) -> {}",
                    t[..1]
                        .iter()
                        .map(|x| x.to_smolstr())
                        .collect::<Vec<_>>()
                        .join(", "),
                    {
                        let x = t.last().unwrap();
                        if x == &DataType::Null {
                            SmolStr::new_static("")
                        } else {
                            x.to_smolstr()
                        }
                    }
                )
            }
        }
    }
}

/// check_args(args, expected_args_len, fn_name, src, markers)
#[macro_export]
macro_rules! check_args {
    ($args:expr, $expected_args_len:expr, $fn_name:expr, $src:expr,$markers:expr) => {
        if $args.len() != $expected_args_len {
            throw_parser_error(
                $src,
                $markers,
                ErrType::IncorrectFuncArgCount(
                    $fn_name,
                    $expected_args_len as u16,
                    $args.len() as u16,
                ),
            );
        }
    };
}

/// check_args_range(args, min_args_len, max_args_len, fn_name, src, markers)
#[macro_export]
macro_rules! check_args_range {
    ($args:expr, $min_args_len:expr,$max_args_len:expr, $fn_name:expr, $src:expr,$markers:expr) => {
        if $args.len() < $min_args_len || $args.len() > $max_args_len {
            throw_parser_error(
                $src,
                $markers,
                ErrType::IncorrectFuncArgCountVariable(
                    $fn_name.into(),
                    $min_args_len as u16,
                    $max_args_len as u16,
                    $args.len() as u16,
                ),
            );
        }
    };
}

pub const SPOCK_LOGO: &str = r#"
 @@@@@@  @@@@@@@   @@@@@@@@    @@@@@@  @@@  @@@
@@@      @@@  @@  @@@    @@@  @@@      @@@#@@
 @@@@@@  @@@@@@@ @@@      @@  @@       @@@@@
     @@  @@@      @@@    @@@  @@@      @@@ @@@
@@@@@@@  @@@       @@@@@@@@    @@@@@@  @@@  @@@

by Horace Hoff."#;
