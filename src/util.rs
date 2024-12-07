use crate::parser::Types;
use smol_str::{SmolStr, ToSmolStr as _};

pub fn error(message: &str, tip: &str) {
    if tip.is_empty() {
        eprintln!(
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\n{message}\n--------------"
        );
    } else {
        eprintln!(
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\n{message}\n\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m\n{tip}\n--------------"
        );
    }
    std::process::exit(1);
}

#[macro_export]
macro_rules! error_msg {
    ($x:expr) => {
        format!(
            "--------------\n{}\n{}\n--------------",
            "\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", $x
        )
        .as_str()
    };
    ($x:expr, $y:expr) => {
        format!(
            "--------------\n{}\n{}\n{}\n{}\n--------------",
            "\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m",
            $x,
            "\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m",
            $y
        )
        .as_str()
    };
}

// https://stackoverflow.com/a/63305257
#[macro_export]
macro_rules! log {
    ($($rest:tt)*) => {
        #[cfg(debug_assertions)]
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($rest)*));
    }
}

#[macro_export]
macro_rules! log_release {
    ($($rest:tt)*) => {
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($rest)*));
    }
}

#[macro_export]
macro_rules! assert_args_number {
    ($func_name:expr, $received_args_len:expr, $expected_args_len:expr) => {
        // if $received_args_len != $expected_args_len {
        //     error(
        //         &format!(
        //             "Function '{}' expected {} argument(s) but received {}",
        //             $func_name, $expected_args_len, $received_args_len
        //         ),
        //         "",
        //     );
        // }
        assert!($received_args_len == $expected_args_len,
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\nFunction '{}' expected {} argument(s) but received {}\n--------------",
            $func_name, $expected_args_len, $received_args_len
        )
    };
    ($func_name:expr, $received_args_len:expr, $min_args_len:expr, $max_args_len:expr) => {
        // if $received_args_len < $min_args_len || $received_args_len > $max_args_len {
        //     error(
        //         &format!(
        //             "Function '{}' expected between {} and {} arguments but received {}",
        //             $func_name, $min_args_len, $max_args_len, $received_args_len
        //         ),
        //         "Remove the excess arguments",
        //     );
        // }
        assert!(
            $received_args_len >= $min_args_len && $received_args_len <= $max_args_len,
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\nFunction '{}' expected between {} and {} argument(s) but received {}\n--------------",
            $func_name, $min_args_len, $max_args_len, $received_args_len
        )
    };
}

macro_rules! get_value {
    ($x:expr) => {
        match $x {
            Types::String(x) => x,
            Types::Float(x) => x,
            Types::Integer(x) => x,
            Types::Bool(x) => x,
            Types::Array(x) => x,
            _ => error(
                format!("{}", error_msg!(format!("Cannot get value of {:?}", $x))),
                "",
            ),
        }
    };
}

#[macro_export]
macro_rules! get_printable_type {
    ($x:expr) => {
        match $x {
            Types::String(_) => "String",
            Types::Float(_) => "Float",
            Types::Integer(_) => "Integer",
            Types::Bool(_) => "Boolean",
            Types::Array(_) => "Array",
            Types::Null => "Null",
            _ => {
                println!("{:?}", $x);
                error(
                    &format!("{}", error_msg!(format!("Cannot get value of {:?}", $x))),
                    "",
                );
                ""
            }
        }
    };
}

#[macro_export]
macro_rules! math_to_type {
    ($x:expr) => {
        if $x == $x.trunc() {
            Types::Integer($x as i64)
        } else {
            Types::Float($x)
        }
    };
}

#[macro_export]
macro_rules! if_let {
    // When likely is specified
    (likely, $pattern:pat, $value:expr, $body:block) => {
        if likely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            }
        }
    };

    (likely, $pattern:pat, $value:expr, $body:block, else $elseblock:block) => {
        if likely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            } else {
                $elseblock
            }
        } else {
            $elseblock
        }
    };

    // When unlikely is specified
    (unlikely, $pattern:pat, $value:expr, $body:block) => {
        if unlikely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            }
        }
    };

    (unlikely, $pattern:pat, $value:expr, $body:block, else $elseblock:block) => {
        if unlikely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            } else {
                $elseblock
            }
        } else {
            $elseblock
        }
    };

    // Default (no prediction hint)
    ($pattern:pat, $value:pat, $body:block) => {
        if let $pattern = $value {
            $body
        }
    };

    ($pattern:pat, $value:expr, $body:block, else $elseblock:block) => {
        if let $pattern = $value {
            $body
        } else {
            $elseblock
        }
    };
}

pub fn get_printable_form(x: &Types) -> SmolStr {
    match x {
        Types::String(str) => str.to_owned(),
        Types::Float(float) => float.to_smolstr(),
        Types::Integer(int) => int.to_smolstr(),
        Types::Bool(boolean) => boolean.to_smolstr(),
        Types::Array(x) => {
            let arr = x;
            ("[".to_owned()
                + arr
                    .iter()
                    .map(|item| get_printable_form(item).to_string() + ",")
                    .collect::<String>()
                    .trim_end_matches(',')
                    .parse::<String>()
                    .unwrap()
                    .as_str()
                + "]")
                .to_smolstr()
        }
        Types::Null => "Null".to_smolstr(),
        _ => {
            error(
                &format!("Cannot display {} type", get_printable_type!(x)),
                "",
            );
            "".to_smolstr()
        }
    }
}
