use crate::parser::Expr;

pub fn error(message: &str, tip: &str) {
    if tip == "" {
        eprintln!(
            "--------------\n{}\n{}\n--------------",
            "\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", message
        );
    } else {
        eprintln!(
            "--------------\n{}\n{}\n{}\n{}\n--------------",
            "\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m",
            message,
            "\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m",
            tip
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
        std::println!($($rest)*)
    }
}

#[macro_export]
macro_rules! assert_args_number {
    ($func_name:expr, $received_args_len:expr, $expected_args_len:expr) => {
        if $received_args_len != $expected_args_len {
            error(
                &format!(
                    "Function '{}' expected {} argument(s) but received {}",
                    $func_name, $expected_args_len, $received_args_len
                ),
                "Remove the excess arguments",
            );
        }
    };
    ($func_name:expr, $received_args_len:expr, $min_args_len:expr, $max_args_len:expr) => {
        if $received_args_len < $min_args_len || $received_args_len > $max_args_len {
            error(
                &format!(
                    "Function '{}' expected between {} and {} arguments but received {}",
                    $func_name, $min_args_len, $max_args_len, $received_args_len
                ),
                "Remove the excess arguments",
            );
        }
    };
}

macro_rules! get_value {
    ($x:expr) => {
        match $x {
            Expr::String(x) => x,
            Expr::Float(x) => x,
            Expr::Integer(x) => x,
            Expr::Bool(x) => x,
            Expr::Array(x) => x,
            _ => panic!("{}", error_msg!(format!("Cannot get value of {:?}", $x))),
        }
    };
}

#[macro_export]
macro_rules! get_printable_type {
    ($x:expr) => {
        match $x {
            Expr::String(_) => "String",
            Expr::Float(_) => "Float",
            Expr::Integer(_) => "Integer",
            Expr::Bool(_) => "Boolean",
            Expr::Array(_) => "Array",
            Expr::Null => "Null",
            _ => panic!("{}", error_msg!(format!("Cannot get type of {:?}", $x))),
        }
    };
}

#[macro_export]
macro_rules! math_to_type {
    ($x:expr) => {
        if $x.fract() != 0.0 {
            Expr::Float($x)
        } else {
            Expr::Integer($x as i64)
        }
    };
}

pub fn get_printable_form(x: &Expr) -> String {
    match x {
        Expr::String(str) => str.to_owned(),
        Expr::Float(float) => float.to_string(),
        Expr::Integer(int) => int.to_string(),
        Expr::Bool(boolean) => boolean.to_string(),
        Expr::Array(x) => {
            let arr = x;
            "[".to_string()
                + arr
                    .iter()
                    .map(|item| get_printable_form(item) + ",")
                    .collect::<String>()
                    .trim_end_matches(",")
                    .parse::<String>()
                    .unwrap()
                    .as_str()
                + "]"
        }
        Expr::Null => "Null".to_string(),
        _ => {
            error(
                &format!("Cannot display {} type", get_printable_type!(x)),
                "",
            );
            "".to_string()
        }
    }
}
