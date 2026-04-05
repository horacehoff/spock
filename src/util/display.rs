use std::hint::unreachable_unchecked;

use crate::ArrayStorage;
use crate::parser::Expr;
use crate::type_system::DataType;
use crate::{Data, Instr};
use ariadne::*;
use concat_string::concat_string;
use inline_colorization::*;
use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;

#[inline(always)]
pub fn format_data(x: Data, arrays: Option<&ArrayStorage>, show_str: bool) -> String {
    if x.is_float() {
        x.as_float().to_string()
    } else if x.is_int() {
        x.as_int().to_string()
    } else if x.is_bool() {
        x.as_bool().to_string()
    } else if x.is_str() {
        let s = x.as_str();
        if show_str { s } else { format!("\"{s}\"") }
    } else if x.is_array() {
        if let Some(arrays) = arrays {
            concat_string!(
                "[",
                arrays[x.as_array() as usize]
                    .iter()
                    .map(|x| format_data(*x, Some(arrays), true))
                    .collect::<Vec<_>>()
                    .join(","),
                "]"
            )
        } else {
            format!("ARRAY[{}]", x.as_array())
        }
    } else if x.is_file() {
        format!("FILE({:?})", x.as_file())
    } else if x.is_null() {
        String::from("NULL")
    } else {
        unsafe { unreachable_unchecked() }
    }
}

impl std::fmt::Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_data(*self, None, false))
    }
}

pub fn get_type_name(x: Data) -> String {
    String::from(if x.is_array() {
        "Array"
    } else if x.is_bool() {
        "Boolean"
    } else if x.is_str() {
        "String"
    } else if x.is_file() {
        "File"
    } else if x.is_float() {
        "Float"
    } else if x.is_int() {
        "Integer"
    } else if x.is_null() {
        "Null"
    } else {
        unreachable!()
    })
}

pub fn format_expr(x: &Expr) -> String {
    match x {
        Expr::Float(num) => num.to_string(),
        Expr::Bool(bool) => bool.to_string(),
        Expr::String(str) => {
            format!("\"{str}\"")
        }
        Expr::Array(a, _, _) => concat_string!(
            "[",
            a.iter().map(format_expr).collect::<Vec<_>>().join(","),
            "]"
        ),
        Expr::Var(x, _, _) => x.to_string(),
        _ => unreachable!(),
    }
}

fn token_recognition(token: &str) -> String {
    match token {
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

#[inline(never)]
#[cold]
pub fn op_error(
    src: (&str, &str),
    l: DataType,
    r: DataType,
    op: &str,
    start: usize,
    end: usize,
) -> ! {
    parser_error(
        src,
        start,
        end,
        "Invalid operation",
        &format!(
            "Cannot perform operation {color_bright_blue}{style_bold}{} {color_red}{}{color_bright_blue} {}{color_reset}{style_reset}",
            l, op, r
        ),
        "",
    )
}

#[cold]
#[inline(never)]
pub fn parser_error(
    src: (&str, &str),
    start: usize,
    end: usize,
    general_error: &str,
    msg: &str,
    note: &str,
) -> ! {
    eprintln!("{color_red}SPOCK ERROR{color_reset}");
    if !note.is_empty() {
        Report::build(ReportKind::Error, (src.0, start..end))
            .with_message(general_error)
            .with_label(
                Label::new((src.0, start..end))
                    .with_message(msg)
                    .with_color(Color::Red),
            )
            .with_note(note)
            .finish()
            .print((src.0, Source::from(src.1)))
            .unwrap();
    } else {
        Report::build(ReportKind::Error, (src.0, start..end))
            .with_message(general_error)
            .with_label(
                Label::new((src.0, start..end))
                    .with_message(msg)
                    .with_color(Color::Red),
            )
            .finish()
            .print((src.0, Source::from(src.1)))
            .unwrap();
    }
    std::process::exit(1);
}

#[cold]
#[inline(never)]
pub fn lalrpop_error<'a, L, T, E>(x: ParseError<usize, T, &str>, file: &str, filename: &str) -> !
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
        ParseError::UnrecognizedEof {
            location,
            expected: _,
        } => {
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
                            token_recognition(&x)
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

pub fn print_debug(instructions: &[Instr], registers: &[Data], arrays: &ArrayStorage) {
    println!("{color_yellow}---- DEBUG ----{color_reset}");
    if !arrays.is_empty() {
        println!("{color_green}---  ARRAYS  ---{color_reset}");
        for (i, data) in arrays.iter().enumerate() {
            println!(" {i} {data:?}")
        }
    }
    println!("{color_green}-- REGISTERS --{color_reset}");
    for (i, data) in registers.iter().enumerate() {
        println!(" [{i}] {}({data})", get_type_name(*data))
    }
    println!("{color_red}-- INSTRUCTIONS --{color_reset}");
    let mut flows: Vec<(usize, usize)> = Vec::new();
    for (i, instr) in instructions.iter().enumerate() {
        match instr {
            Instr::Jmp(jump_size)
            | Instr::IsFalseJmp(_, jump_size)
            | Instr::SupEqFloatJmp(_, _, jump_size)
            | Instr::SupEqIntJmp(_, _, jump_size)
            | Instr::SupFloatJmp(_, _, jump_size)
            | Instr::SupIntJmp(_, _, jump_size)
            | Instr::InfEqFloatJmp(_, _, jump_size)
            | Instr::InfEqIntJmp(_, _, jump_size)
            | Instr::InfFloatJmp(_, _, jump_size)
            | Instr::InfIntJmp(_, _, jump_size)
            | Instr::NotEqJmp(_, _, jump_size)
            | Instr::ArrayNotEqJmp(_, _, jump_size)
            | Instr::EqJmp(_, _, jump_size)
            | Instr::ArrayEqJmp(_, _, jump_size) => flows.push((i, i + *jump_size as usize)),
            Instr::CallLibFunc(jump_size, _, _) => flows.push((i, *jump_size as usize)),
            Instr::JmpBack(jump_size) => flows.push((i, i - *jump_size as usize)),
            Instr::CallFunc(n, _) => flows.push((i, *n as usize)),
            _ => continue,
        }
    }
    let instr_str = instructions
        .iter()
        .enumerate()
        .map(|(i, instr)| format!(" {i}: {instr:?} "))
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
                    indicators.push_str("  ─┐");
                } else {
                    indicators.push_str("  ─┘");
                }
            } else if i == x.1 {
                if x.1 > x.0 {
                    indicators.push_str(" <─┘");
                } else {
                    indicators.push_str(" <─┐");
                }
            } else if (x.0..x.1).contains(&i) || (x.1..x.0).contains(&i) {
                if i == instr_str.len() - 1 {
                    indicators.push_str("   X");
                } else {
                    indicators.push_str("   │");
                }
            } else {
                indicators.push_str("    ");
            }
        }
        println!("{instr}{}{}", margins[i], indicators);
    }
    println!("{color_yellow}------------------{color_reset}");
}

pub fn _format_registers_inline(registers: &[Data]) -> String {
    let mut output = String::new();
    registers.iter().enumerate().for_each(|(i, x)| {
        output.push_str(&format!("[{i}] {x:?} "));
    });
    output
}
