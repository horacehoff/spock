use crate::parser::{BasicOperator, Expr};

pub fn error(message: &str, tip: &str) {
    if tip == "" {
        eprintln!("--------------\n{}\n{}\n--------------","\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", message);
    } else {
        eprintln!("--------------\n{}\n{}\n{}\n{}\n--------------","\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", message, "\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m", tip);
    }
    std::process::exit(1);
}

#[macro_export] macro_rules! error_msg {
    ($x:expr) => {
        format!("--------------\n{}\n{}\n--------------","\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", $x).as_str()
    };
}
pub fn assert_args_number(func_name: &str, received_args_len: usize, expected_args_len: usize) {
   if received_args_len != expected_args_len {
        error(&format!("Function '{}' expected {} argument(s) but received {}", func_name, expected_args_len, received_args_len), "Remove the excess arguments");
   }
}