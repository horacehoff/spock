use colored::Colorize;
use concat_string::concat_string;
use inline_colorization::*;
use internment::Intern;
use lalrpop_util::lexer::Token;
use lalrpop_util::{lalrpop_mod, ParseError};
use likely_stable::if_likely;
use std::cmp::PartialEq;
use std::f64::consts::E;
use std::fmt::Formatter;
use std::time::Instant;
use builtin_funcs::FUNCS;
use crate::parser::{grammar, parser_to_instr_set};

mod util;
mod builtin_funcs;
mod display;
mod parser;

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Data {
    Number(f64),
    Bool(bool),
    String(Intern<String>),
    Null,
}


#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Instr {
    Print(u16),

    // LOGIC
    // size -- is_neg
    Jmp(u16, bool),
    // condition id -- size
    Cmp(u16, u16),
    // CopyArg(u16, u16),
    Mov(u16, u16),
    GoTo(u16),

    // OPS
    Add(u16, u16, u16),
    Mul(u16, u16, u16),
    Sub(u16, u16, u16),
    Div(u16, u16, u16),
    Mod(u16, u16, u16),
    Pow(u16, u16, u16),
    Eq(u16, u16, u16),
    NotEq(u16, u16, u16),
    Sup(u16, u16, u16),
    SupEq(u16, u16, u16),
    Inf(u16, u16, u16),
    InfEq(u16, u16, u16),
    BoolAnd(u16, u16, u16),
    BoolOr(u16, u16, u16),
    Neg(u16, u16),

    // Funcs
    Abs(u16, u16),
    Num(u16, u16),
    Str(u16, u16),
    Bool(u16, u16),

    ApplyFunc(u8, u16, u16, u16),
}

fn execute(instructions: &[Instr], consts: &mut [Data]) {
    let len = instructions.len();
    let mut i: usize = 0;
    while i < len {
        match instructions[i] {
            Instr::Jmp(size, is_neg) => {
                if is_neg {
                    i -= size as usize;
                } else {
                    i += size as usize;
                }
                continue;
            }
            Instr::Cmp(cond_id, size) => {
                if let Data::Bool(false) = consts[cond_id as usize] {
                    i += size as usize;
                    continue;
                }
            }
            Instr::GoTo(index) => {
                i = index as usize;
            }
            Instr::Add(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Number(parent + child);
                    }
                    (Data::String(parent), Data::String(child)) => {
                        let result = concat_string!(*parent, *child);
                        consts[dest as usize] = Data::String(Intern::from(result));
                    }
                    _ => {
                        error_b!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} + {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Mul(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent * child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} * {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::Div(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent / child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} / {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::Sub(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent - child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} - {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::Mod(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent % child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} % {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::Pow(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Number(parent.powf(child));
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} ^ {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::Eq(o1, o2, dest) => {
                let val = consts[o1 as usize] == consts[o2 as usize];
                consts[dest as usize] = Data::Bool(val);
            }
            Instr::NotEq(o1, o2, dest) => {
                let val = consts[o1 as usize] != consts[o2 as usize];
                consts[dest as usize] = Data::Bool(val);
            }
            Instr::Sup(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent > child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} > {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::SupEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent >= child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} >= {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::Inf(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent < child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} < {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::InfEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent <= child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} <= {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::BoolAnd(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent && child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} && {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::BoolOr(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                if_likely! {let (Data::Bool(parent), Data::Bool(child)) = (first_elem, second_elem) => {
                    consts[dest as usize] = Data::Bool(parent || child);
                } else {
                    error_b!(format_args!(
                        "UNSUPPORTED OPERATION: {:?} || {:?}",
                        first_elem, second_elem
                    ));
                }}
            }
            Instr::Mov(tgt, dest) => {
                consts[dest as usize] = consts[tgt as usize];
            }
            Instr::Neg(tgt, dest) => {
                let tgt = consts[tgt as usize];
                if_likely! {let Data::Number(x) = tgt => {
                    consts[dest as usize] = Data::Number(-x);
                } else {
                    error_b!(format_args!("UNSUPPORTED OPERATION: -{tgt:?}"));
                }}
            }
            Instr::Print(target) => {
                let elem = consts[target as usize];
                println!("{elem}");
            }
            Instr::Abs(tgt, dest) => {
                if let Data::Number(x) = consts[tgt as usize] {
                    consts[dest as usize] = Data::Number(x.abs());
                }
            }
            Instr::Num(tgt, dest) => {
                let base = consts[tgt as usize];
                match base {
                    Data::String(str) => {
                        consts[dest as usize] =
                            Data::Number(str.parse::<f64>().unwrap_or_else(|_| {
                                error_b!(format_args!("CANNOT CONVERT '{str}' TO NUMBER"));
                            }))
                    }
                    Data::Number(_) => consts[dest as usize] = base,
                    other => {
                        error_b!(format_args!("CANNOT CONVERT {other} TO NUMBER"));
                    }
                }
            }
            Instr::Str(tgt, dest) => {
                consts[dest as usize] =
                    Data::String(Intern::from(format!("{}", consts[tgt as usize])));
            }
            Instr::Bool(tgt, dest) => {
                let base = consts[tgt as usize];
                if let Data::String(str) = base {
                    consts[dest as usize] = Data::Bool(str.parse::<bool>().unwrap_or_else(|_| {
                        error_b!(format_args!("CANNOT CONVERT {str} TO BOOL"));
                    }));
                } else {
                    error_b!(format_args!("CANNOT CONVERT {base} TO BOOL"));
                }
            }
            Instr::ApplyFunc(fctn_id, tgt, dest, arg) => {
                FUNCS[fctn_id as usize](tgt, dest, arg, consts);
            }
        }
        i += 1;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(f64),
    Bool(bool),
    Op(Box<Expr>, Box<[(Opcode, Box<Expr>)]>),
    Opcode(Opcode),
    Priority(Box<Expr>),
    String(String),
    Var(String),
    // Group(Box<[Expr]>),
    VarDeclare(String, Box<Expr>),
    VarAssign(String, Box<Expr>),
    // condition - code -- else_if_blocks(condition array) - else_block
    Condition(Box<Expr>, Box<[Expr]>, Box<[Expr]>, Option<Box<[Expr]>>),
    ElseIfBlock(Box<Expr>, Box<[Expr]>),
    WhileBlock(Box<Expr>, Box<[Expr]>),
    FunctionCall(String, Box<[Expr]>),
    ObjFunctionCall(Box<Expr>, Box<[(String, Box<[Expr]>)]>),
    LPAREN,
    RPAREN,

    FunctionDecl(String, Box<[String]>, Box<[Expr]>),

    ReturnVal(Box<Option<Expr>>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    Pow,
    Eq,
    NotEq,
    Sup,
    SupEq,
    Inf,
    InfEq,
    BoolAnd,
    BoolOr,
    Neg,
}




fn print_instructions(instructions: &[Instr]) {
    for (i, instr) in instructions.iter().enumerate() {
        println!("{} {}", i + 1, match instr {
            Instr::Print(x) => format!("PRINT {x}"),
            Instr::Jmp(x, y) => format!("JMP {x} {y}"),
            Instr::Cmp(x, y) => format!("CMP {x} {y}"),
            Instr::GoTo(x) => format!("GOTO {x}"),
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
            Instr::ApplyFunc(x, y, z, _) => format!("APPLY_FUNCTION {x} {y} {z}"),
        });
    }
}

fn parse(contents: &str) -> (Vec<Instr>, Vec<Data>) {
    let mut functions: Vec<Expr> = grammar::FileParser::new().parse(&contents).unwrap();
    print!("funcs {functions:?}");
    let main_function: Vec<Expr> = {
        if let Some(fctn) = functions.iter().position(|a| {
            if let Expr::FunctionDecl(name, _, _) = a {
                name.trim_end_matches('(') == "main"
            } else {
                false
            }
        }) {
            if let Expr::FunctionDecl(_, _, code) = functions.swap_remove(fctn) {
                code.to_vec()
            } else {
                error!(contents, "No main function");
            }
        } else {
            error!(contents, "No main function");
        }
    };

    let mut functions: Vec<(String, Box<[String]>, Box<[Expr]>)> = functions
        .iter()
        .map(|w| {
            if let Expr::FunctionDecl(x, y, z) = w {
                (x.trim_end_matches('(').to_string(), y.clone(), z.clone())
            } else {
                error!(contents, "Function expected");
            }
        })
        .collect();

    print!("{functions:?}");

    let mut variables: Vec<(String, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let instructions = parser_to_instr_set(
        main_function,
        &mut variables,
        &mut consts,
        &mut functions,
        None,
    );
    print!("CONSTS are {consts:?}");
    #[cfg(debug_assertions)]
    {
        print_instructions(&instructions);
    }
    (instructions, consts)
}

fn format_parser_error<'a, L, T, E>(x: ParseError<L, T, E>, ctx: &str) -> String
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

// Live long and prosper
fn main() {

    let mut n:f64 = 1.0;
    let mut u = 1.0/2.0*E - 1.0/2.0;
    while n < 21.0 {
        u = 1.0 / 2.0 * E - (n+1.0)/2.0*u;
        n += 2.0;
    }
    println!("{u}");  
    dbg!(size_of::<Instr>());
    let mut contents = std::fs::read_to_string("test.spock").unwrap();
    contents = contents
        .lines()
        .filter_map(|mut line| {
            if line.starts_with("//") {
                return None;
            } else if let Some(idx) = line.find("//") {
                let mut in_str = false;
                for c in line.chars().take(idx + 2) {
                    if c == '"' {
                        in_str = !in_str;
                    } else if !in_str && c == '/' {
                        line = &line[..idx];
                    }
                }
            }
            Some(line)
        })
        .collect::<Vec<&str>>()
        .join("\r\n");
    print!("{contents:?}");

    let (instructions, mut consts) = parse(&contents);

    let now = Instant::now();
    execute(&instructions, &mut consts);
    println!("EXEC TIME {:.2?}", now.elapsed());
}
