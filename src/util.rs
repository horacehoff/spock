use crate::{Data, Instr};

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

pub fn get_type(x: Data) -> String {
    match x {
        Data::Number(_) => String::from("Number"),
        Data::Bool(_) => String::from("Bool"),
        Data::String(_) => String::from("String"),
        Data::Array(_, _) => String::from("Array"),
        Data::Null => String::from("NULL"),
    }
}

pub fn print_instructions(instructions: &[Instr]) {
    for (i, instr) in instructions.iter().enumerate() {
        println!("{} {}", i + 1, match instr {
            Instr::Print(x) => format!("PRINT {x}"),
            Instr::Jmp(x, y) => format!("JMP {x} {y}"),
            Instr::Cmp(x, y) => format!("CMP {x} {y}"),
            Instr::Mov(x, y) => format!("MOV {x} {y}"),
            Instr::Add(x, y, z) => format!("ADD {x} {y} {z}"),
            Instr::Mul(x, y, z) => format!("MUL {x} {y} {z}"),
            Instr::Sub(x, y, z) => format!("SUB {x} {y} {z}"),
            Instr::Div(x, y, z) => format!("DIV {x} {y} {z}"),
            Instr::Mod(x, y, z) => format!("MOD {x} {y} {z}"),
            Instr::Pow(x, y, z) => format!("POW {x} {y} {z}"),
            Instr::Eq(x, y, z) => format!("EQ {x} {y} {z}"),
            Instr::NotEq(x, y, z) => format!("NOT_EQ {x} {y} {z}"),
            Instr::Sup(x, y, z) => format!("SUP {x} {y} {z}"),
            Instr::SupEq(x, y, z) => format!("SUP_EQ {x} {y} {z}"),
            Instr::Inf(x, y, z) => format!("INF {x} {y} {z}"),
            Instr::InfEq(x, y, z) => format!("INF_EQ {x} {y} {z}"),
            Instr::BoolAnd(x, y, z) => format!("AND {x} {y} {z}"),
            Instr::BoolOr(x, y, z) => format!("OR {x} {y} {z}"),
            Instr::Neg(x, y) => format!("NEG {x} {y}"),
            Instr::Abs(x, y) => format!("ABS {x} {y}"),
            Instr::Num(x, y) => format!("NUM {x} {y}"),
            Instr::Str(x, y) => format!("STR {x} {y}"),
            Instr::Bool(x, y) => format!("BOOL {x} {y}"),
            Instr::Input(x, y) => format!("INPUT {x} {y}"),
            Instr::ApplyFunc(x, y, z) => format!("APPLY_FUNCTION {x} {y} {z}"),
            Instr::StoreFuncArg(x) => format!("STORE_ARG {x}"),
            x => format!("UNKNOWN {x:?}"),
        });
    }
}

#[macro_export]
macro_rules! check_args {
    ($args:expr, $expected_args_len:expr, $fn_name:expr, $ctx: expr) => {
        if $args.len() > $expected_args_len {
            error!(
                $ctx,
                format_args!(
                    "Function '{}'{} expects {} argument{}",
                    $fn_name,
                    if $expected_args_len != 0 { " only" } else { "" },
                    $expected_args_len,
                    if $expected_args_len > 1 || $expected_args_len == 0 {
                        "s"
                    } else {
                        ""
                    }
                ),
                format_args!(
                    "Replace with '{}({})'",
                    $fn_name,
                    $args[0..$expected_args_len]
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(",")
                )
            );
        } else if $args.len() < $expected_args_len {
            error!(
                $ctx,
                format_args!(
                    "Function '{}' expects {} argument{}",
                    $fn_name,
                    $expected_args_len,
                    if $expected_args_len > 1 || $expected_args_len == 0 {
                        "s"
                    } else {
                        ""
                    }
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
    ($args:expr, $min_args_len:expr,$max_args_len:expr, $fn_name:expr, $ctx: expr) => {
        if $args.len() < $min_args_len {
            error!(
                $ctx,
                format_args!(
                    "Function '{}' expects at least {} argument{}",
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
            error!(
                $ctx,
                format_args!(
                    "Function '{}' expects at most {} argument{}",
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
