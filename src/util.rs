use crate::parser::{BasicOperator, Expr};

pub fn error(message: &str, tip: &str) {
    eprintln!("--------------\n{}\n{}\n{}\n{}\n--------------","\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", message, "\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m", tip);
    std::process::exit(1);
}
pub fn assert_args_number(func_name: &str, received_args_len: usize, expected_args_len: usize) {
   if received_args_len != expected_args_len {
        error(&format!("Function '{}' expected {} argument(s) but received {}", func_name, expected_args_len, received_args_len), "Remove the excess arguments");
   }
}

pub fn check_and_or(and_or_operator: BasicOperator, current_value: Expr, new_value: Expr) -> Expr {
    println!("CHECKING");
    println!("CURRENT VAL {:?}", current_value);
    println!("NEW VAL {:?}", new_value);
    if let Expr::Bool(bool1) = current_value {
        println!("1 IS BOOL");
        if let Expr::Bool(bool2) = new_value {
            println!("2 IS BOOL");
            if and_or_operator == BasicOperator::AND {
                println!("TRUEAND");
                Expr::Bool(bool1 && bool2)
            } else if and_or_operator == BasicOperator::OR {
                println!("TRUEOR");
                Expr::Bool(bool1 || bool2)
            } else {
                new_value
            }
        } else {
            new_value
        }
    } else {
        new_value
    }
}