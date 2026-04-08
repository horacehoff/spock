use crate::Instr;
use crate::display::token_recognition;
use ariadne::{Color, Label, Report, ReportKind, Source};
use inline_colorization::*;
use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;
use std::fmt::Arguments;

#[cold]
#[inline(never)]
pub fn runtime_error(
    instr_src: &[(Instr, usize, usize)],
    src: (&str, &str),
    instr: &Instr,
    error: &str,
    message: Arguments,
) -> ! {
    let (_, start, end) = instr_src.iter().find(|(x, _, _)| x == instr).unwrap();
    parser_error(src, *start, *end, error, message, None);
}

#[cold]
#[inline(never)]
pub fn error(message: &str) -> ! {
    eprintln!(
        "--------------\n{color_red}SPOCK RUNTIME ERROR:{color_reset}\n{}\n--------------",
        message
    );
    std::process::exit(1);
}

#[cold]
#[inline(always)]
pub fn dev_error(file: &str, function: &str, additional_data: Arguments) -> ! {
    unreachable!(
        "\n--------------\n{color_red}SPOCK COMPILATION ERROR:{color_reset}\nFROM FILE: {}\nFROM FUNCTION: {}\nADDITIONAL DATA: {}\n--------------",
        file, function, additional_data
    );
}

#[macro_export]
macro_rules! op_error {
    ($src: expr, $l: expr, $r: expr, $op: expr, $start:expr, $end:expr) => {
        parser_error(
            $src,
            $start,
            $end,
            "Invalid operation",
            format_args!(
                "Cannot perform operation {color_bright_blue}{style_bold}{} {color_red}{}{color_bright_blue} {}{color_reset}{style_reset}",
                $l, $op, $r
            ),
            None,
        )
    };
}

#[cold]
#[inline(never)]
pub fn parser_error(
    src: (&str, &str),
    start: usize,
    end: usize,
    general_error: &str,
    msg: Arguments,
    note: Option<Arguments>,
) -> ! {
    eprintln!("{color_red}SPOCK ERROR{color_reset}");
    if let Some(note) = note {
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
