use crate::parser::Expr;
use crate::{format_lines, Data, Instr};
use ariadne::*;
use concat_string::concat_string;
use inline_colorization::*;
use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError;
use slab::Slab;
use std::fmt::Formatter;

pub fn format_data(x: Data, arrays: &Slab<Vec<Data>>) -> String {
    match x {
        Data::Number(num) => num.to_string(),
        Data::Bool(bool) => bool.to_string(),
        Data::String(str) => str.to_string(),
        Data::Array(a) => concat_string!(
            "[",
            arrays[a]
                .iter()
                .map(|x| format_data(*x, arrays))
                .collect::<Vec<_>>()
                .join(","),
            "]"
        ),
        Data::Null => String::from("NULL"),
        Data::File(path) => format!("FILE({path:?})"),
    }
}

pub fn format_err(x: Data, arrays: &Slab<Vec<Data>>) -> String {
    match x {
        Data::Number(num) => num.to_string(),
        Data::Bool(bool) => bool.to_string(),
        Data::String(str) => format!("\"{str}\""),
        Data::Array(a) => concat_string!(
            "[",
            arrays[a]
                .iter()
                .map(|x| format_data(*x, arrays))
                .collect::<Vec<_>>()
                .join(","),
            "]"
        ),
        Data::Null => String::from("NULL"),
        Data::File(path) => format!("FILE({path:?})"),
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Num(x) => write!(f, "{x}"),
            Expr::Bool(x) => write!(f, "{x}"),
            Expr::String(x) => write!(f, "\"{x}\""),
            Expr::Var(x, _, _) => write!(f, "{}", x),
            Expr::Condition(x, y, _, _) => {
                write!(
                    f,
                    "if {x} {{\n{}}}",
                    y.iter()
                        .map(|x| format_lines!(x))
                        .collect::<Vec<String>>()
                        .join(""),
                )
            }
            Expr::FunctionDecl(x, y, _, _) => {
                write!(
                    f,
                    "fn {}({}) {{\n{}}}",
                    x.first().unwrap(),
                    x.iter().skip(1).cloned().collect::<Vec<String>>().join(","),
                    y.iter()
                        .map(|x| format_lines!(x))
                        .collect::<Vec<String>>()
                        .join("")
                )
            }
            Expr::WhileBlock(x, y) => {
                write!(
                    f,
                    "while {x} {{\n{}}}",
                    y.iter()
                        .map(|x| format_lines!(x))
                        .collect::<Vec<String>>()
                        .join("")
                )
            }
            Expr::VarAssign(x, y, start, end) => {
                write!(f, "{x} = {y}")
            }
            Expr::VarDeclare(x, y) => {
                write!(f, "let {x} = {y}")
            }
            Expr::FunctionCall(y, z, _, _, _) => {
                write!(
                    f,
                    "{}{}({})",
                    if z.is_empty() || z.len() - 1 == 0 {
                        String::new()
                    } else {
                        format!("{}::", z[1..].join("::"))
                    },
                    z.last().unwrap(),
                    y.iter()
                        .map(|w| format!("{w}"))
                        .collect::<Vec<String>>()
                        .join(",")
                )
            }
            // Expr::ObjFunctionCall(x, y) => {
            //     write!(
            //         f,
            //         "{x}{}",
            //         y.iter()
            //             .map(|x| format!(
            //                 ".{}({})",
            //                 x.0,
            //                 x.1.iter()
            //                     .map(|w| format!("{w}"))
            //                     .collect::<Vec<String>>()
            //                     .join(",")
            //             ))
            //             .collect::<Vec<String>>()
            //             .join("")
            //     )
            // }
            Expr::ReturnVal(x) => {
                write!(
                    f,
                    "return{}",
                    if x.is_none() {
                        String::new()
                    } else {
                        format!(" {}", x.clone().unwrap())
                    }
                )
            }
            _ => write!(f, "{self:?}"),
        }
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
        ParseError::InvalidToken { .. } => {
            unreachable!("InvalidTokenError")
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

pub fn print_instructions(instructions: &[Instr]) {
    for (i, instr) in instructions.iter().enumerate() {
        println!("{} {:?}", i, instr);
    }
}

pub fn format_expr_type(x: Expr) -> String {
    match x {
        Expr::Num(x) => format_args!("Num({x})").to_string(),
        Expr::String(x) => format_args!("String(\"{x}\")").to_string(),
        Expr::Bool(x) => format_args!("Bool({x})").to_string(),
        _ => unreachable!(),
    }
}
