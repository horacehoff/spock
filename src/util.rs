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
