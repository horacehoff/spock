use crate::display::token_recognition;
use crate::{Instr, type_system::DataType};
use ariadne::{Color, Label, Report, ReportKind, Source};
use inline_colorization::*;
use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;
use lazy_format::lazy_format;
use smol_str::{SmolStr, ToSmolStr};
use std::fmt::Arguments;

#[cold]
#[inline(never)]
pub fn runtime_error(
    instr_src: &[(Instr, (usize, usize))],
    src: (&str, &str),
    instr: &Instr,
    error: &str,
    message: Arguments,
) -> ! {
    let (_, (start, end)) = instr_src.iter().find(|(x, _)| x == instr).unwrap();
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

/// Error types, largely borrowed from Rust
pub enum ErrType<'a> {
    // IO ERRORS
    AlreadyExists,
    Deadlock,
    FileTooLarge,
    Interrupted,
    InvalidData,
    InvalidFilename,
    /// When a file was expected...
    IsADirectory,
    /// When a directory was expected...
    NotADirectory,
    NotFound,
    PermissionDenied,
    OutOfMemory,
    ReadOnlyFilesystem,
    StorageFull,
    TimedOut,
    Custom(SmolStr),

    // PARSER ERRORS
    UnknownVariable(&'a str),
    UnknownFunction(&'a str),
    UnknownNamespace(&'a str),
    /// When an array holds two or more different types
    ArrayWithDiffType,
    NotIndexable(DataType),
    InvalidIndexType(DataType),
    /// CannotPushTypeToArray(elem_type, array_type)
    CannotPushTypeToArray(DataType, DataType),
    CannotInferType(&'a str),
    /// IncorrectFuncArgCount(fn_name, expected, received)
    IncorrectFuncArgCount(&'a str, u16, u16),
    IncorrectFuncArgCountVariable(&'a str, u16, u16, u16),
    /// InvalidType(expected_type, received_type)
    InvalidType(DataType, DataType),
    OpError(DataType, DataType, &'a str),
}

impl<'a> From<ErrType<'a>> for SmolStr {
    fn from(value: ErrType) -> Self {
        match value {
            ErrType::Custom(m) => m,
            ErrType::AlreadyExists => "The entity (directory, file, ...) already exists".into(),
            ErrType::Deadlock => "This operation would result in a deadlock".into(),
            ErrType::FileTooLarge => "The file is too large".into(),
            ErrType::Interrupted => "This operation was interrupted".into(),
            ErrType::InvalidData => "Malformed or invalid data were encountered".into(),
            ErrType::InvalidFilename => "The filename is invalid or too long".into(),
            ErrType::IsADirectory => {
                "This operation encountered a directory, when a non-directory was expected".into()
            }
            ErrType::NotADirectory => {
                "This operation encountered a non-directory, when a directory was expected".into()
            }
            ErrType::NotFound => "The entity (directory, file, ...) was not found".into(),
            ErrType::PermissionDenied => {
                "This operation lacked the necessary privileges to complete".into()
            }
            ErrType::OutOfMemory => {
                "This operation could not be completed, because it failed to allocate enough memory"
                    .into()
            }
            ErrType::ReadOnlyFilesystem => {
                "The filesystem or storage medium is read-only, but a write operation was attempted"
                    .into()
            }
            ErrType::StorageFull => "Storage is full".into(),
            ErrType::TimedOut => "This operation timed out".into(),
            ErrType::UnknownFunction(f) => format_args!(
                "Cannot find function {color_bright_blue}{style_bold}{f}{color_reset}{style_reset}"
            )
            .to_smolstr(),
            ErrType::UnknownVariable(v) => format_args!(
                "Cannot find variable {color_bright_blue}{style_bold}{v}{color_reset}{style_reset}"
            )
            .to_smolstr(),
            ErrType::UnknownNamespace(n) => format_args!(
                "Unknown namespace {color_bright_blue}{style_bold}{n}{color_reset}{style_reset}"
            )
            .to_smolstr(),
            ErrType::ArrayWithDiffType => "Arrays can only hold a single type".into(),
            ErrType::NotIndexable(t) => format_args!(
                "The type {color_bright_blue}{style_bold}{t}{color_reset}{style_reset} cannot be indexed"
            )
            .to_smolstr(),
            ErrType::InvalidIndexType(t) => format_args!(
                "The type {color_bright_blue}{style_bold}{t}{color_reset}{style_reset} is not a valid index"
            )
            .to_smolstr(),
            ErrType::CannotPushTypeToArray(elem_t, array_t) => format_args!("Cannot insert {color_bright_blue}{style_bold}{elem_t}{color_reset}{style_reset} in {array_t}").to_smolstr(),
            ErrType::CannotInferType(t) => format_args!(
                "Cannot infer the type of {color_bright_blue}{style_bold}{t}{color_reset}{style_reset}"
            )
            .to_smolstr(),
            ErrType::IncorrectFuncArgCount(fn_name, expected, received) => format_args!("Function {color_bright_blue}{style_bold}{fn_name}{color_reset}{style_reset} expects {expected} argument{} but received {received}", if expected != 1 {"s"} else {""} ).to_smolstr(),
            ErrType::IncorrectFuncArgCountVariable(fn_name, expected_min, expected_max, received) => format_args!("Function {color_bright_blue}{style_bold}{fn_name}{color_reset}{style_reset} expects between {expected_min} and {expected_max} arguments but received {received}").to_smolstr(),
            ErrType::InvalidType(expected, received) => format_args!("Expected type {expected}, found {color_bright_blue}{style_bold}{received}{color_reset}{style_reset}").to_smolstr(),
            ErrType::OpError(l, r, op) => format_args!(
                "Cannot perform operation {color_bright_blue}{style_bold}{l} {color_red}{op}{color_bright_blue} {r}{color_reset}{style_reset}").to_smolstr(),
        }
    }
}

impl<'a> From<std::io::ErrorKind> for ErrType<'a> {
    fn from(value: std::io::ErrorKind) -> Self {
        match value {
            std::io::ErrorKind::AlreadyExists => ErrType::AlreadyExists,
            std::io::ErrorKind::Deadlock => ErrType::Deadlock,
            std::io::ErrorKind::FileTooLarge => ErrType::FileTooLarge,
            std::io::ErrorKind::Interrupted => ErrType::Interrupted,
            std::io::ErrorKind::InvalidData => ErrType::InvalidData,
            std::io::ErrorKind::InvalidFilename => ErrType::InvalidFilename,
            std::io::ErrorKind::IsADirectory => ErrType::IsADirectory,
            std::io::ErrorKind::NotADirectory => ErrType::NotADirectory,
            std::io::ErrorKind::NotFound => ErrType::NotFound,
            std::io::ErrorKind::PermissionDenied => ErrType::PermissionDenied,
            std::io::ErrorKind::OutOfMemory => ErrType::OutOfMemory,
            std::io::ErrorKind::ReadOnlyFilesystem => ErrType::ReadOnlyFilesystem,
            std::io::ErrorKind::StorageFull => ErrType::StorageFull,
            std::io::ErrorKind::TimedOut => ErrType::TimedOut,
            other => ErrType::Custom(other.to_smolstr()),
        }
    }
}

#[cold]
#[inline(never)]
pub fn throw_error(
    instr_src: &[(Instr, (usize, usize))],
    src: (&str, &str),
    instr: &Instr,
    t: ErrType,
) -> ! {
    let (_, (start, end)) = instr_src.iter().find(|(x, _)| x == instr).unwrap();
    let err_message: SmolStr = t.into();
    eprintln!("{color_red}SPOCK ERROR{color_reset}");
    Report::build(ReportKind::Error, (src.0, *start..*end))
        .with_label(
            Label::new((src.0, *start..*end))
                .with_message(err_message)
                .with_color(Color::Red),
        )
        .finish()
        .eprint((src.0, Source::from(src.1)))
        .unwrap();
    std::process::exit(1);
}

#[cold]
#[inline(never)]
pub fn throw_parser_error(src: (&str, &str), (start, end): &(usize, usize), t: ErrType) -> ! {
    let err_message: SmolStr = t.into();
    eprintln!("{color_red}SPOCK ERROR{color_reset}");
    Report::build(ReportKind::Error, (src.0, *start..*end))
        .with_label(
            Label::new((src.0, *start..*end))
                .with_message(err_message)
                .with_color(Color::Red),
        )
        .finish()
        .eprint((src.0, Source::from(src.1)))
        .unwrap();
    std::process::exit(1);
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
