use crate::Data;
use crate::parser::Expr;
use crate::type_inference::DataType;

#[macro_export]
macro_rules! error {
    ($x: expr, $y: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK ERROR:{color_reset}\n{color_bright_red}CONTEXT:{color_reset}\n{style_bold}{}{style_reset}\n{color_bright_red}ERROR:{color_reset}\n{}\n--------------", $x, $y
        );
        std::process::exit(1);
    };
    ($x: expr, $y: expr, $z: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK ERROR:{color_reset}\n{color_bright_red}CONTEXT:{color_reset}\n{style_bold}{}{style_reset}\n{color_bright_red}ERROR:{color_reset}\n{}\n{color_bright_red}POSSIBLE FIX:{color_reset}\n{}\n--------------", $x, $y, $z
        );
        std::process::exit(1);
    }
}

#[macro_export]
macro_rules! error_b {
    ($x: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK ERROR:{color_reset}\n{}\n--------------",
            $x
        );
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! parsing_error {
    ($x: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK PARSING ERROR:{color_reset}\n{}\n--------------",
            $x
        );
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! print {
    ($($x:tt)*) => {
        #[cfg(debug_assertions)]
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($x)*))
    }
}

#[macro_export]
macro_rules! format_lines {
    ($line: expr) => {
        if format!("{}", $line).chars().last().unwrap() != '}' {
            format!("{};\n", $line)
        } else {
            format!("{}\n", $line)
        }
    };
}

pub fn format_type(x: Data) -> String {
    match x {
        Data::Number(_) => String::from("Number"),
        Data::Bool(_) => String::from("Bool"),
        Data::String(_) => String::from("String"),
        Data::Array(_) => String::from("Array"),
        Data::Null => String::from("NULL"),
        Data::File(_) => String::from("File"),
    }
}

pub fn format_datatype(x: DataType) -> String {
    match x {
        DataType::Number => String::from("Number"),
        DataType::Bool => String::from("Bool"),
        DataType::String => String::from("String"),
        DataType::Array(_) => String::from("Array"),
        DataType::Null => String::from("NULL"),
        DataType::File => String::from("File"),
    }
}

pub fn format_type_expr(x: &Expr) -> String {
    match x {
        Expr::Num(_) => String::from("Number"),
        Expr::Bool(_) => String::from("Bool"),
        Expr::String(_) => String::from("String"),
        Expr::Array(_, _, _) => String::from("Array"),
        _ => unreachable!(),
    }
}

#[macro_export]
macro_rules! check_args {
    ($args:expr, $expected_args_len:expr, $fn_name:expr, $filename:expr,$src:expr,$start:expr,$end:expr) => {
        if $args.len() > $expected_args_len {
            parser_error!(
                $filename,
                $src,
                $start,
                $end,
                "Incorrect arguments for function",
                format_args!(
                    "Function {color_bright_blue}{style_bold}{}{color_reset}{style_reset}{} expects {} argument{}",
                    $fn_name,
                    if $expected_args_len != 0 { " only" } else { "" },
                    $expected_args_len,
                    if $expected_args_len == 1 { "" } else { "s" }
                ),
                format_args!(
                    "Replace with {color_green}{}({}){color_reset}",
                    $fn_name,
                    $args[0..$expected_args_len]
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(",")
                )
            );
        } else if $args.len() < $expected_args_len {
            parser_error!(
                $filename,
                $src,
                $start,
                $end,
                "Incorrect arguments for function",
                format_args!(
                    "Function {color_bright_blue}{style_bold}{}{color_reset}{style_reset} expects {} argument{}",
                    $fn_name,
                    $expected_args_len,
                    if $expected_args_len == 1 { "" } else { "s" }
                ),
                format_args!(
                    "Add {} additional argument{}",
                    $expected_args_len - $args.len(),
                    if $expected_args_len - $args.len() > 1 {
                        "s"
                    } else {
                        ""
                    }
                )
            );
        }
    };
}

#[macro_export]
macro_rules! check_args_range {
    ($args:expr, $min_args_len:expr,$max_args_len:expr, $fn_name:expr, $filename:expr,$src:expr,$start:expr,$end:expr) => {
        if $args.len() < $min_args_len {
            parser_error!(
                $filename,
                $src,
                $start,
                $end,
                "Incorrect arguments for function",
                format_args!(
                    "Function {color_bright_blue}{style_bold}{}{color_reset}{style_reset} expects at least {} argument{}",
                    $fn_name,
                    $min_args_len,
                    if $min_args_len > 1 { "s" } else { "" }
                ),
                format_args!(
                    "Add {} additional argument{}",
                    $min_args_len - $args.len(),
                    if $min_args_len - $args.len() > 1 {
                        "s"
                    } else {
                        ""
                    }
                )
            );
        } else if $args.len() > $max_args_len {
            parser_error!(
                $filename,
                $src,
                $start,
                $end,
                "Incorrect arguments for function",
                format_args!(
                    "Function {color_bright_blue}{style_bold}{}{color_reset}{style_reset} expects at most {} argument{}",
                    $fn_name,
                    $max_args_len,
                    if $max_args_len > 1 { "s" } else { "" }
                ),
                format_args!(
                    "Remove {} argument{}",
                    $args.len() - $max_args_len,
                    if $args.len() - $max_args_len > 1 {
                        "s"
                    } else {
                        ""
                    }
                )
            );
        }
    };
}

pub const SPOCK_LOGO: &str = r#"
              ++++  #### ++++  ###+ ++++  ###-
             ++++  ####  ++++  ####  ++++  ####
            ++++  ####   ++++  ####   ++++  ####
           ++++  #####  +++++  #####  +++++  ####
         +++++   ####   +++++  #####   ++++   #####
        +++++   #####   +++++  #####   +++++   #####
       +++++   #####   ++++++  ######   +++++   #####

       @@@@@@  @@@@@@@   @@@@@@@@    @@@@@@  @@@  @@@
      *@@      @@@  @@+ @@@    @@@  @@@      @@@#@@
       @@@@@@  @@@@@@@ @@@      @@  @@       @@@@@
           @@  @@@      @@@    @@@  @@@      @@@ @@@
      @@@@@@@  @@@       @@@@@@@@    @@@@@@  @@@  @@@

      by Horace Hoff.
"#;
