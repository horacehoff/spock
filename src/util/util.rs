use crate::type_inference::DataType;

#[macro_export]
macro_rules! error {
    ($x: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK RUNTIME ERROR:{color_reset}\n{}\n--------------",
            $x
        );
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! debug {
    ($($x:tt)*) => {
        #[cfg(debug_assertions)]
        println!("[DEBUG] {}", format!($($x)*))
    }
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Number => write!(f, "Number"),
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
        }
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
                        .map(|x| format_expr(x))
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
      @@@      @@@  @@  @@@    @@@  @@@      @@@#@@
       @@@@@@  @@@@@@@ @@@      @@  @@       @@@@@
           @@  @@@      @@@    @@@  @@@      @@@ @@@
      @@@@@@@  @@@       @@@@@@@@    @@@@@@  @@@  @@@

      by Horace Hoff.
"#;
