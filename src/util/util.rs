use crate::color_bright_blue;
use crate::color_reset;
use crate::style_bold;
use crate::style_reset;
use crate::{errors::error, type_system::DataType};
use smol_str::SmolStr;
use smol_str::ToSmolStr;

#[cold]
#[inline(never)]
pub fn cold() {}

#[inline(always)]
pub fn likely(b: bool) -> bool {
    if !b {
        cold();
    }
    b
}

#[inline(always)]
pub fn unlikely(b: bool) -> bool {
    if b {
        cold();
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

pub fn str_to_type(s: &SmolStr) -> DataType {
    if s == "int" {
        DataType::Int
    } else if s == "float" {
        DataType::Float
    } else if s == "bool" {
        DataType::Bool
    } else if s == "string" {
        DataType::String
    } else {
        error(
            format_args!(
                "Unknown type {color_bright_blue}{style_bold}{s}{color_reset}{style_reset}"
            )
            .as_str()
            .unwrap(),
        );
    }
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Float => write!(f, "Float"),
            DataType::Int => write!(f, "Integer"),
            DataType::Bool => write!(f, "Boolean"),
            DataType::String => write!(f, "String"),
            DataType::Array(array_type) => write!(f, "Array<{}>", array_type),
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
                            SmolStr::new_inline("")
                        } else {
                            x.to_smolstr()
                        }
                    }
                )
            }
        }
    }
}

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

pub const SPOCK_LOGO: &str = concat!(
    r#"
              ++++  #### ++++  ###+ ++++  ###-
             ++++  ####  ++++  ####  ++++  ####
            ++++  ####   ++++  ####   ++++  ####
           ++++  #####  +++++  #####  +++++  ####
         +++++   ####   +++++  #####   ++++   #####
        +++++   #####   +++++  #####   +++++   #####
       +++++   #####   ++++++  ######   +++++   #####

       @@@@@@  @@@@@@@   @@@@@@@@    @@@@@@  @@@  @@@
      @@@      @@@  @@  @@@    @@@  @@@      @@@#@@
       @@@@@@  @@@@@@@ @@@      @@  @@       @@@@@
           @@  @@@      @@@    @@@  @@@      @@@ @@@
      @@@@@@@  @@@       @@@@@@@@    @@@@@@  @@@  @@@"#,
    "  v.",
    env!("CARGO_PKG_VERSION"),
    "\n\n      by Horace Hoff."
);
