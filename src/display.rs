use crate::parser::Expr;
use crate::{Data, Instr, Opcode, format_lines};
use concat_string::concat_string;
use fnv::FnvHashMap;
use inline_colorization::*;
use lalrpop_util::lexer::Token;
use lalrpop_util::{ErrorRecovery, ParseError};
use std::fmt::Formatter;

pub fn format_data(x: Data, arrays: &FnvHashMap<u16, Vec<Data>>) -> String {
    match x {
        Data::Number(num) => num.to_string(),
        Data::Bool(bool) => bool.to_string(),
        Data::String(str) => str.to_string(),
        Data::Array(a) => concat_string!(
            "[",
            arrays[&a]
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
            Expr::Var(x) => write!(f, "{}", x),
            Expr::Opcode(x) => write!(f, "{}", x),
            Expr::Op(x) => {
                write!(f, "{:?}", x,)
            }
            Expr::Condition(x, y) => {
                write!(
                    f,
                    "if {x} {{\n{}}}",
                    y.iter()
                        .map(|x| format_lines!(x))
                        .collect::<Vec<String>>()
                        .join(""),
                )
            }
            Expr::FunctionDecl(x, y) => {
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
            Expr::VarAssign(x, y) => {
                write!(f, "{x} = {y}")
            }
            Expr::VarDeclare(x, y) => {
                write!(f, "let {x} = {y}")
            }
            Expr::FunctionCall(y, z) => {
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
            Expr::ObjFunctionCall(x, y) => {
                write!(
                    f,
                    "{x}{}",
                    y.iter()
                        .map(|x| format!(
                            ".{}({})",
                            x.0,
                            x.1.iter()
                                .map(|w| format!("{w}"))
                                .collect::<Vec<String>>()
                                .join(",")
                        ))
                        .collect::<Vec<String>>()
                        .join("")
                )
            }
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

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Opcode::Mul => "*",
                Opcode::Div => "/",
                Opcode::Add => "+",
                Opcode::Sub => "-",
                Opcode::Mod => "%",
                Opcode::Pow => "^",
                Opcode::Eq => "==",
                Opcode::NotEq => "!=",
                Opcode::Sup => ">",
                Opcode::SupEq => ">=",
                Opcode::Inf => "<",
                Opcode::InfEq => "<=",
                Opcode::BoolAnd => "&&",
                Opcode::BoolOr => "||",
                Opcode::Neg => "-",
            }
        )
    }
}

pub fn format_parser_error<'a, L, T, E>(x: ParseError<L, T, E>, ctx: &str) -> String
where
    Token<'a>: From<T>,
{
    match x {
        ParseError::InvalidToken { .. } => {
            unreachable!("InvalidTokenError")
        }
        ParseError::UnrecognizedEof { .. } => {
            unreachable!("UnrecognizedEofError")
        }
        ParseError::UnrecognizedToken { token, expected } => {
            format!(
                "PARSING: {ctx}\nExpected token {color_blue}{}{color_reset} but got '{color_red}{}{color_reset}'",
                expected
                    .iter()
                    .map(|x| x.trim_matches('"'))
                    .collect::<Vec<&str>>()
                    .join(" / "),
                {
                    let tok: Token = token.1.into();
                    tok.1
                }
            )
        }
        ParseError::ExtraToken { .. } => {
            unreachable!("ExtraTokenError")
        }
        ParseError::User { .. } => {
            unreachable!("UserError")
        }
    }
}

pub fn format_parser_error_recovery<'a, L, T, E>(x: ErrorRecovery<L, T, E>, ctx: &str) -> String
where
    Token<'a>: From<T>,
{
    match x.error {
        ParseError::InvalidToken { .. } => {
            unreachable!("InvalidTokenError")
        }
        ParseError::UnrecognizedEof { .. } => {
            unreachable!("UnrecognizedEofError")
        }
        ParseError::UnrecognizedToken { token, expected } => {
            format!(
                "PARSING: {ctx}\nExpected token {color_blue}{}{color_reset} but got '{color_red}{}{color_reset}'",
                expected
                    .iter()
                    .map(|x| x.trim_matches('"'))
                    .collect::<Vec<&str>>()
                    .join(" / "),
                {
                    let tok: Token = token.1.into();
                    tok.1
                }
            )
        }
        ParseError::ExtraToken { .. } => {
            unreachable!("ExtraTokenError")
        }
        ParseError::User { .. } => {
            unreachable!("UserError")
        }
    }
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
