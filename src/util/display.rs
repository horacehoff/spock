use crate::ArrayStorage;
use crate::parser::Expr;
use crate::{Data, Instr};
use ariadne::*;
use concat_string::concat_string;
use inline_colorization::*;
use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;

pub fn format_data(x: Data, arrays: &ArrayStorage, show_str: bool) -> String {
    match x {
        Data::Number(num) => num.to_string(),
        Data::Bool(bool) => bool.to_string(),
        Data::String(str) => {
            if show_str {
                str.to_string()
            } else {
                format!("\"{str}\"")
            }
        }
        Data::Array(a) => concat_string!(
            "[",
            arrays[a]
                .iter()
                .map(|x| format_data(*x, arrays, true))
                .collect::<Vec<_>>()
                .join(","),
            "]"
        ),
        Data::Null => String::from("NULL"),
        Data::File(path) => format!("FILE({path:?})"),
    }
}

pub fn format_expr(x: &Expr) -> String {
    match x {
        Expr::Num(num) => num.to_string(),
        Expr::Bool(bool) => bool.to_string(),
        Expr::String(str) => {
            format!("\"{str}\"")
        }
        Expr::Array(a, _, _) => concat_string!(
            "[",
            a.iter()
                .map(|x| format_expr(x))
                .collect::<Vec<_>>()
                .join(","),
            "]"
        ),
        Expr::Var(x, _, _) => x.to_string(),
        _ => unreachable!(),
    }
}

pub fn format_err(x: Data, arrays: &ArrayStorage) -> String {
    match x {
        Data::Number(num) => num.to_string(),
        Data::Bool(bool) => bool.to_string(),
        Data::String(str) => format!("\"{str}\""),
        Data::Array(a) => concat_string!(
            "[",
            arrays[a]
                .iter()
                .map(|x| format_err(*x, arrays))
                .collect::<Vec<_>>()
                .join(","),
            "]"
        ),
        Data::Null => String::from("NULL"),
        Data::File(path) => format!("FILE({path:?})"),
    }
}

fn token_recognition(token: String) -> String {
    match token.as_str() {
        "r#\"[a-zA-Z_]+\"#" => String::from("identifier"),
        "r#\"([0-9]*[.])?[0-9]+\"#" => String::from("number"),
        "\"true\"" => String::from("boolean"),
        other => {
            if other.contains("|[^") {
                String::from("string")
            } else {
                other.trim_matches('\"').to_string()
            }
        }
    }
}

#[macro_export]
macro_rules! parser_error {
    ($filename: expr,
    $source: expr,
    $start: expr,
    $end: expr,
    $error_general: expr,
    $msg:expr) => {
        eprintln!("{color_red}SPOCK ERROR{color_reset}");
        Report::build(ReportKind::Error, ($filename, $start..$end))
            .with_message($error_general)
            .with_label(
                Label::new(($filename, $start..$end))
                    .with_message($msg)
                    .with_color(Color::Red),
            )
            .finish()
            .print(($filename, Source::from($source)))
            .unwrap();
        std::process::exit(1);
    };
    ($filename: expr,
    $source: expr,
    $start: expr,
    $end: expr,
    $error_general: expr,
    $msg:expr,
    $note: expr
    ) => {
        eprintln!("{color_red}SPOCK ERROR{color_reset}");
        Report::build(ReportKind::Error, ($filename, $start..$end))
            .with_message($error_general)
            .with_label(
                Label::new(($filename, $start..$end))
                    .with_message($msg)
                    .with_color(Color::Red),
            )
            .with_note($note)
            .finish()
            .print(($filename, Source::from($source)))
            .unwrap();
        std::process::exit(1);
    };
}

#[cold]
pub fn lalrpop_error<'a, L, T, E>(x: ParseError<usize, T, &str>, file: &str, filename: &str)
where
    Token<'a>: From<T>,
{
    eprintln!("{color_red}SPOCK ERROR{color_reset}");
    match x {
        ParseError::InvalidToken { location } => {
            Report::build(ReportKind::Error, (filename, location..location + 1))
                .with_message("Invalid token")
                .with_label(
                    Label::new((filename, location..location + 1))
                        .with_message(format_args!("This token is invalid"))
                        .with_color(Color::Red),
                )
                .finish()
                .print((filename, Source::from(file)))
                .unwrap();
        }
        ParseError::UnrecognizedEof { location, expected } => {
            Report::build(ReportKind::Error, (filename, location..location + 1))
                .with_message("Unrecognized EOF")
                .with_label(
                    Label::new((filename, location..location + 1))
                        .with_message(format_args!(
                            "Expected one or more {color_bright_blue}{style_bold}}}{style_reset}{color_reset}"
                        ))
                        .with_color(Color::Red),
                )
                .finish()
                .print((filename, Source::from(file)))
                .unwrap();
        }
        ParseError::UnrecognizedToken { token, expected } => {
            let begin = token.0;
            let end = token.2;

            let expected_tokens = expected
                .into_iter()
                .filter_map(|x| {
                    if x == "\"false\"" {
                        None
                    } else {
                        Some(format!(
                            "{color_bright_blue}{style_bold}{}{style_reset}{color_reset}",
                            token_recognition(x)
                        ))
                    }
                })
                .collect::<Vec<String>>()
                .join(" OR ");

            Report::build(ReportKind::Error, (filename, begin..end))
                .with_message("Unrecognized token")
                .with_label(
                    Label::new((filename, begin..end))
                        .with_message(format_args!("Expected {}", expected_tokens))
                        .with_color(Color::Red),
                )
                .finish()
                .print((filename, Source::from(file)))
                .unwrap();
        }
        ParseError::ExtraToken { .. } => {
            unreachable!("ExtraTokenError")
        }
        ParseError::User { .. } => {
            unreachable!("UserError")
        }
    }
    std::process::exit(1);
}

pub fn print_debug(instructions: &[Instr], consts: &[Data], arrays: &ArrayStorage) {
    println!("{color_yellow}---- DEBUG ----{color_reset}");
    if !arrays.is_empty() {
        println!("{color_green}---  ARRAYS  ---{color_reset}");
        for (i, data) in arrays {
            println!(" {i} {data:?}")
        }
    }
    println!("{color_green}-- CONSTANTS --{color_reset}");
    for (i, data) in consts.iter().enumerate() {
        println!(" {i} {data:?}")
    }
    println!("{color_red}-- INSTRUCTIONS --{color_reset}");
    let mut flows: Vec<(usize, usize)> = Vec::new();
    for (i, instr) in instructions.iter().enumerate() {
        match instr {
            Instr::Jmp(jump_size)
            | Instr::Cmp(_, jump_size)
            | Instr::InfCmp(_, _, jump_size)
            | Instr::InfEqCmp(_, _, jump_size)
            | Instr::SupCmp(_, _, jump_size)
            | Instr::SupEqCmp(_, _, jump_size)
            | Instr::EqCmp(_, _, jump_size)
            | Instr::ArrayEqCmp(_, _, jump_size)
            | Instr::NotEqCmp(_, _, jump_size)
            | Instr::ArrayNotEqCmp(_, _, jump_size) => flows.push((i, i + *jump_size as usize)),
            Instr::JmpSave(jump_size, is_neg, _) => flows.push((i, *jump_size as usize)),
            Instr::JmpNeg(jump_size) => flows.push((i, i - *jump_size as usize)),
            _ => continue,
        }
    }
    let instr_str = instructions
        .iter()
        .enumerate()
        .map(|(i, instr)| format!(" {i} {instr:?} "))
        .collect::<Vec<String>>();
    let max_len = instr_str.iter().max_by_key(|x| x.len()).unwrap().len();
    let margins = instr_str
        .iter()
        .map(|str| " ".repeat(max_len - str.len()))
        .collect::<Vec<String>>();
    for (i, instr) in instr_str.iter().enumerate() {
        let mut indicators: String = String::new();
        for x in &flows {
            if i == x.0 {
                if x.1 > x.0 {
                    indicators.push_str("  -┐");
                } else {
                    indicators.push_str("  -┘");
                }
            } else if i == x.1 {
                if x.1 > x.0 {
                    indicators.push_str(" ←-┘");
                } else {
                    indicators.push_str(" ←-┐");
                }
            } else if (x.0..x.1).contains(&i) || (x.1..x.0).contains(&i) {
                indicators.push_str("   |");
            } else {
                indicators.push_str("    ");
            }
        }
        println!("{instr}{}{}", margins[i], indicators);
    }
    println!("{color_yellow}------------------{color_reset}");
}
