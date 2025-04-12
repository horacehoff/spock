use crate::parser::Expr;
use crate::{Data, Instr, Opcode, format_lines};
use colored::Colorize;
use concat_string::concat_string;
use fnv::FnvHashMap;
use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;
use std::fmt::Formatter;

impl std::fmt::Display for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Number(num) => write!(f, "{num}"),
            Data::Bool(bool) => write!(f, "{bool}"),
            Data::String(str) => write!(f, "{str}"),
            Data::Null => write!(f, "NULL"),
            Data::Array(start) => write!(f, "ARRAY{start}"),
        }
    }
}

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
            Expr::Op(x, y) => {
                write!(
                    f,
                    "{} {}",
                    x,
                    y.iter()
                        .map(|w| format!("{} {}", w.0, w.1))
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Expr::Condition(x, y, z, w) => {
                write!(
                    f,
                    "if {x} {{\n{}}} {} {}",
                    y.iter()
                        .map(|x| format_lines!(x))
                        .collect::<Vec<String>>()
                        .join(""),
                    if z.is_empty() {
                        String::new()
                    } else {
                        z.iter()
                            .map(|w| {
                                if let Expr::ElseIfBlock(x, y) = w {
                                    format!(
                                        "else if {x} {{\n{}}}",
                                        y.iter()
                                            .map(|x| format_lines!(x))
                                            .collect::<Vec<String>>()
                                            .join("")
                                    )
                                } else {
                                    String::new()
                                }
                            })
                            .collect::<Vec<String>>()
                            .join("")
                    },
                    if let Some(else_block) = w {
                        format!(
                            "else {{\n{}}}",
                            else_block
                                .iter()
                                .map(|x| format_lines!(x))
                                .collect::<Vec<String>>()
                                .join("")
                        )
                    } else {
                        String::new()
                    }
                )
            }
            Expr::FunctionDecl(x, y, z) => {
                write!(
                    f,
                    "fn {x}({}) {{\n{}}}",
                    y.to_vec().join(","),
                    z.iter()
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
            Expr::FunctionCall(x, y) => {
                write!(
                    f,
                    "{x}({})",
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
            _ => write!(f, "{self:?}"),
        }
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
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
        })
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
                "PARSING: {ctx}\nExpected token {} but got '{}'",
                expected
                    .iter()
                    .map(|x| x.trim_matches('"'))
                    .collect::<Vec<&str>>()
                    .join(" / ")
                    .blue(),
                {
                    let tok: Token = token.1.into();
                    tok.1
                }
                .blue()
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
            Instr::Num(x, y) => format!("NUM {x} {y}"),
            Instr::Str(x, y) => format!("STR {x} {y}"),
            Instr::Bool(x, y) => format!("BOOL {x} {y}"),
            Instr::Input(x, y) => format!("INPUT {x} {y}"),
            Instr::ApplyFunc(x, y, z) => format!("APPLY_FUNCTION {x} {y} {z}"),
            Instr::StoreFuncArg(x) => format!("STORE_ARG {x}"),
            Instr::InfCmp(x, y, z) => format!("INF_CMP {x} {y} {z}"),
            Instr::InfEqCmp(x, y, z) => format!("INF_EQ_CMP {x} {y} {z}"),
            Instr::SupCmp(x, y, z) => format!("SUP_CMP {x} {y} {z}"),
            Instr::SupEqCmp(x, y, z) => format!("SUP_EQ_CMP {x} {y} {z}"),
            Instr::EqCmp(x, y, z) => format!("EQ_CMP {x} {y} {z}"),
            Instr::NotEqCmp(x, y, z) => format!("NOT_EQ_CMP {x} {y} {z}"),
            Instr::ArrayMov(x, y, z) => format!("ARRAY_MOV {x} {y} {z}"),
            Instr::ArrayMod(x, y, z) => format!("ARRAY_MOD {x} {y} {z}"),
            Instr::GetIndex(x, y, z) => format!("GET_INDEX {x} {y} {z}"),
            Instr::Range(x, y, z) => format!("RANGE {x} {y} {z}"),
            Instr::Type(x, y) => format!("TYPE {x} {y}"),
        });
    }
}
