use std::fmt::Formatter;
use crate::{format_lines, Data, Expr, Opcode};

impl std::fmt::Display for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Number(num) => write!(f, "{num}"),
            Data::Bool(bool) => write!(f, "{bool}"),
            Data::String(str) => write!(f, "{str}"),
            Data::Null => write!(f, "NULL"),
        }
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