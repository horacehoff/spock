use colored::Colorize;
use concat_string::concat_string;
use inline_colorization::*;
use internment::Intern;
use lalrpop_util::lalrpop_mod;
use likely_stable::if_likely;
use std::cmp::PartialEq;
use std::fmt::Formatter;
use std::hint::unreachable_unchecked;
use std::time::Instant;
use cached::proc_macro::io_cached;

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

macro_rules! error_b {
    ($x: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK ERROR:{color_reset}\n{}\n--------------",
            $x
        );
        std::process::exit(1);
    };
}

macro_rules! print {
    ($($x:tt)*) => {
        #[cfg(debug_assertions)]
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($x)*))
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Data {
    Number(f64),
    Bool(bool),
    String(Intern<String>),
    Null,
}

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
    LPAREN,
    RPAREN,

    FunctionDecl(String, Box<[String]>, Box<[Expr]>),

    ReturnVal(Box<Expr>),
}

macro_rules! format_lines {
    ($line: expr) => {
        if format!("{}", $line).chars().last().unwrap() != '}' {
            format!("{};\n", $line)
        } else {
            format!("{}\n", $line)
        }
    };
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
                                .map(|w| if let Expr::ElseIfBlock(x, y) = w {
                                    format!(
                                        "else if {x} {{\n{}}}",
                                        y.iter()
                                            .map(|x| format_lines!(x))
                                            .collect::<Vec<String>>()
                                            .join("")
                                    )
                                } else {
                                    String::new()
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
            Opcode::Null => unsafe { unreachable_unchecked() },
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    Null,
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

lalrpop_mod!(pub grammar);

fn get_precedence(operator: &Expr) -> u8 {
    if let Expr::Opcode(op) = operator {
        match op {
            Opcode::BoolOr => 1,
            Opcode::BoolAnd => 2,
            Opcode::Eq | Opcode::NotEq => 3,
            Opcode::Inf | Opcode::InfEq | Opcode::Sup | Opcode::SupEq => 4,
            Opcode::Add | Opcode::Sub | Opcode::Neg => 5,
            Opcode::Mul | Opcode::Div | Opcode::Mod => 6,
            Opcode::Pow => 7,
            Opcode::Null => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

fn is_left_associative(operator: &Expr) -> bool {
    if let Expr::Opcode(op) = operator {
        !matches!(op, Opcode::Pow)
    } else {
        unreachable!()
    }
}

pub fn op_to_rpn(operation_input: Vec<Expr>) -> Vec<Expr> {
    let mut return_vector: Vec<Expr> = Vec::new();
    let mut op_stack: Vec<Expr> = Vec::new();
    for x in operation_input {
        // num, function,...
        if !matches!(x, Expr::Opcode(_) | Expr::LPAREN | Expr::RPAREN) {
            return_vector.push(x);
        } else if matches!(x, Expr::Opcode(_)) && x != Expr::LPAREN && x != Expr::RPAREN {
            // operator
            while !op_stack.is_empty()
                && op_stack.last().unwrap() != &Expr::LPAREN
                && (get_precedence(op_stack.last().unwrap()) > get_precedence(&x)
                    || (get_precedence(op_stack.last().unwrap()) == get_precedence(&x)
                        && is_left_associative(&x)))
            {
                return_vector.push(op_stack.pop().unwrap());
            }
            op_stack.push(x);
        } else if x == Expr::LPAREN {
            op_stack.push(x);
        } else if x == Expr::RPAREN {
            while op_stack.last().unwrap() != &Expr::LPAREN {
                assert!(!op_stack.is_empty(), "MISMATCHED PARENTHESES");
                return_vector.push(op_stack.pop().unwrap());
            }
            op_stack.pop();
        }
    }
    while !op_stack.is_empty() {
        assert_ne!(
            op_stack.last().unwrap(),
            &Expr::LPAREN,
            "MISMATCHED PARENTHESES"
        );
        return_vector.push(op_stack.pop().unwrap());
    }

    return_vector
}

fn get_tgt_id(x: Instr) -> u16 {
    match x {
        Instr::Mov(_, y)
        | Instr::Add(_, _, y)
        | Instr::Mul(_, _, y)
        | Instr::Sub(_, _, y)
        | Instr::Div(_, _, y)
        | Instr::Mod(_, _, y)
        | Instr::Pow(_, _, y)
        | Instr::Eq(_, _, y)
        | Instr::NotEq(_, _, y)
        | Instr::Sup(_, _, y)
        | Instr::SupEq(_, _, y)
        | Instr::Inf(_, _, y)
        | Instr::InfEq(_, _, y)
        | Instr::BoolAnd(_, _, y)
        | Instr::BoolOr(_, _, y)
        | Instr::Neg(_, y)
        | Instr::Abs(_, y)
        | Instr::Num(_, y)
        | Instr::Bool(_, y)
        | Instr::Str(_, y) => y,
        _ => unreachable!(),
    }
}

fn move_to_id(x: &mut [Instr], tgt_id: u16) {
    if x.is_empty() {
        return;
    }
    print!("MOVING TO ID => {x:?}");
    match x.last_mut().unwrap() {
        Instr::Add(_, _, z)
        | Instr::Mul(_, _, z)
        | Instr::Sub(_, _, z)
        | Instr::Div(_, _, z)
        | Instr::Mod(_, _, z)
        | Instr::Pow(_, _, z)
        | Instr::Eq(_, _, z)
        | Instr::NotEq(_, _, z)
        | Instr::Sup(_, _, z)
        | Instr::SupEq(_, _, z)
        | Instr::Inf(_, _, z)
        | Instr::InfEq(_, _, z)
        | Instr::BoolAnd(_, _, z)
        | Instr::BoolOr(_, _, z)
        | Instr::Mov(_, z)
        | Instr::Neg(_, z)
        | Instr::Abs(_, z)
        | Instr::Bool(_, z)
        | Instr::Num(_, z)
        | Instr::Str(_, z) => *z = tgt_id,
        _ => unreachable!(),
    }
}

macro_rules! handle_ops {
    ($final_stack: expr, $x: expr, $y: expr, $z: expr, $op: expr, $consts: expr) => {
        match $op {
            Opcode::Mul => $final_stack.push(Instr::Mul($x, $y, $z)),
            Opcode::Div => $final_stack.push(Instr::Div($x, $y, $z)),
            Opcode::Add => $final_stack.push(Instr::Add($x, $y, $z)),
            Opcode::Sub => $final_stack.push(Instr::Sub($x, $y, $z)),
            Opcode::Mod => $final_stack.push(Instr::Mod($x, $y, $z)),
            Opcode::Pow => $final_stack.push(Instr::Pow($x, $y, $z)),
            Opcode::Eq => $final_stack.push(Instr::Eq($x, $y, $z)),
            Opcode::NotEq => $final_stack.push(Instr::NotEq($x, $y, $z)),
            Opcode::Sup => $final_stack.push(Instr::Sup($x, $y, $z)),
            Opcode::SupEq => $final_stack.push(Instr::SupEq($x, $y, $z)),
            Opcode::Inf => $final_stack.push(Instr::Inf($x, $y, $z)),
            Opcode::InfEq => $final_stack.push(Instr::InfEq($x, $y, $z)),
            Opcode::BoolAnd => $final_stack.push(Instr::BoolAnd($x, $y, $z)),
            Opcode::BoolOr => $final_stack.push(Instr::BoolOr($x, $y, $z)),
            Opcode::Neg => $final_stack.push(Instr::Neg($y, $z)),
            Opcode::Null => unreachable!(),
        }
    };
}

fn get_id(
    x: Expr,
    variables: &mut Vec<(String, u16)>,
    consts: &mut Vec<Data>,
    instr: &mut Vec<Instr>,
    line: &String,
    functions: &mut Vec<(String, Box<[String]>, Box<[Expr]>)>,
) -> u16 {
    print!("GETTING ID OF {x:?}");
    match x {
        Expr::Num(num) => {
            consts.push(Data::Number(num));
            (consts.len() - 1) as u16
        }
        Expr::String(str) => {
            consts.push(Data::String(Intern::from(str)));
            (consts.len() - 1) as u16
        }
        Expr::Bool(bool) => {
            consts.push(Data::Bool(bool));
            (consts.len() - 1) as u16
        }
        Expr::Var(name) => {
            if let Some((_, id)) = variables.iter().find(|(var, _)| name == *var) {
                *id
            } else {
                error!(
                    line,
                    format_args!("Unknown variable {}", name.red()),
                    format_args!("Add 'let {name} = 0;'")
                );
            }
        }
        _ => {
            print!("PARSING {x}");
            let mut out = parser_to_instr_set(vec![x], variables, consts, functions, None);
            instr.append(&mut out);
            get_tgt_id(*instr.last().unwrap())
        }
    }
}

fn expr_to_data(input: Expr) -> Data {
    match input {
        Expr::Num(num) => Data::Number(num),
        Expr::Bool(bool) => Data::Bool(bool),
        Expr::String(str) => Data::String(Intern::from(str)),
        _ => Data::Null,
    }
}

macro_rules! check_args {
    ($args:expr, $expected_args_len:expr, $fn_name:expr, $ctx: expr) => {
        if $args.len() > $expected_args_len {
            error!(
                $ctx,
                format_args!(
                    "Function '{}' only expects {} argument{}",
                    $fn_name,
                    $expected_args_len,
                    if $expected_args_len > 1 { "s" } else { "" }
                ),
                format_args!(
                    "Replace with '{}({})'",
                    $fn_name,
                    $args[0..$expected_args_len]
                        .iter()
                        .map(|x| format!("{x}"))
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
                    if $expected_args_len > 1 { "s" } else { "" }
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



fn parser_to_instr_set(
    input: Vec<Expr>,
    variables: &mut Vec<(String, u16)>,
    consts: &mut Vec<Data>,
    functions: &mut Vec<(String, Box<[String]>, Box<[Expr]>)>,
    is_processing_function: Option<&(String, u16, Vec<(String, u16)>, Option<u16>)>,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::new();
    for x in input {
        let ctx = x.to_string();
        match x {
            Expr::Num(num) => consts.push(Data::Number(num)),
            Expr::Bool(bool) => consts.push(Data::Bool(bool)),
            Expr::String(str) => consts.push(Data::String(Intern::from(str))),
            Expr::Var(name) => {
                consts.push(Data::Null);
                if let Some((_, var_id)) = variables.iter().find(|(x, _)| &name == x) {
                    output.push(Instr::Mov(*var_id, (consts.len() - 1) as u16));
                } else {
                    error!(
                        ctx,
                        format_args!("Unknown variable {}", name.red()),
                        format_args!("Add 'let {name} = 0;'")
                    );
                }
            }
            Expr::Condition(x, y, z, w) => {
                let mut condition_blocks: Vec<(Vec<Instr>, Vec<Instr>)> = Vec::new();
                let val = *x;
                if matches!(val, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", val));
                }
                let condition = parser_to_instr_set(
                    vec![val],
                    variables,
                    consts,
                    functions,
                    is_processing_function,
                );
                let mut priv_vars = variables.clone();
                let cond_code = parser_to_instr_set(
                    y.into_vec(),
                    &mut priv_vars,
                    consts,
                    functions,
                    is_processing_function,
                );

                condition_blocks.push((condition, cond_code));

                for condition in z {
                    if let Expr::ElseIfBlock(condition, code) = condition {
                        let conserved = *condition;
                        if matches!(conserved, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                            error!(ctx, format_args!("{} is not a bool", conserved));
                        }
                        let condition = parser_to_instr_set(
                            vec![conserved],
                            variables,
                            consts,
                            functions,
                            is_processing_function,
                        );
                        let mut priv_vars = variables.clone();
                        let cond_code = parser_to_instr_set(
                            code.into_vec(),
                            &mut priv_vars,
                            consts,
                            functions,
                            is_processing_function,
                        );
                        condition_blocks.push((condition, cond_code));
                    }
                }
                if let Some(code) = w {
                    let mut priv_vars = variables.clone();
                    let cond_code = parser_to_instr_set(
                        code.into_vec(),
                        &mut priv_vars,
                        consts,
                        functions,
                        is_processing_function,
                    );
                    condition_blocks.push((Vec::new(), cond_code));
                }

                let jumps: Vec<u16> = (0..condition_blocks.len())
                    .map(|i| {
                        condition_blocks
                            .iter()
                            .skip(i + 1)
                            .map(|x| {
                                if x.0.is_empty() {
                                    (x.1.len() + 1) as u16
                                } else {
                                    (x.0.len() + x.1.len() + 2) as u16
                                }
                            })
                            .sum::<u16>()
                    })
                    .collect();

                for (i, (x, y)) in condition_blocks.into_iter().enumerate() {
                    if x.is_empty() {
                        output.extend(y);
                        break;
                    }
                    output.extend(x);
                    let condition_id = get_tgt_id(*output.last().unwrap());
                    let jump_size = jumps[i];
                    if jump_size == 0 {
                        output.push(Instr::Cmp(condition_id, (y.len() + 1) as u16));
                        output.extend(y);
                    } else {
                        output.push(Instr::Cmp(condition_id, (y.len() + 2) as u16));
                        output.extend(y);
                        output.push(Instr::Jmp(jump_size, false));
                    }
                }

                print!("{consts:?}");
            }
            Expr::WhileBlock(x, y) => {
                if matches!(*x, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", *x));
                }
                let condition = parser_to_instr_set(
                    vec![*x],
                    variables,
                    consts,
                    functions,
                    is_processing_function,
                );
                output.extend(condition);
                let condition_id = get_tgt_id(*output.last().unwrap());
                let mut priv_vars = variables.clone();
                let cond_code = parser_to_instr_set(
                    y.into_vec(),
                    &mut priv_vars,
                    consts,
                    functions,
                    is_processing_function,
                );
                let len = (cond_code.len() + 2) as u16;
                output.push(Instr::Cmp(condition_id, len));
                output.extend(cond_code);
                output.push(Instr::Jmp(len, true));
            }
            Expr::VarDeclare(x, y) => {
                let len = consts.len() as u16;
                let mut val = parser_to_instr_set(
                    vec![*y],
                    variables,
                    consts,
                    functions,
                    is_processing_function,
                );
                move_to_id(&mut val, len);
                variables.push((x, len));
                output.extend(val);
            }
            Expr::VarAssign(x, y) => {
                let id = variables
                    .iter()
                    .find(|(w, _)| w == &x)
                    .unwrap_or_else(|| {
                        error!(
                            ctx,
                            format_args!("Unknown variable {x}"),
                            format_args!("Add 'let {x} = 0;'")
                        );
                    })
                    .1;

                let mut value = parser_to_instr_set(
                    vec![*y],
                    variables,
                    consts,
                    functions,
                    is_processing_function,
                );
                move_to_id(&mut value, id);
                output.extend(value);
            }
            Expr::FunctionCall(x, args) => match x.as_str() {
                "print" => {
                    for arg in args {
                        let id = get_id(arg, variables, consts, &mut output, &ctx, functions);
                        output.push(Instr::Print(id));
                    }
                }
                "abs" => {
                    check_args!(args, 1, "abs", ctx);
                    let id = get_id(
                        args[0].clone(),
                        variables,
                        consts,
                        &mut output,
                        &ctx,
                        functions,
                    );
                    output.push(Instr::Abs(id, id));
                }
                "num" => {
                    check_args!(args, 1, "num", ctx);
                    let id = get_id(
                        args[0].clone(),
                        variables,
                        consts,
                        &mut output,
                        &ctx,
                        functions,
                    );
                    output.push(Instr::Num(id, id));
                }
                "str" => {
                    check_args!(args, 1, "str", ctx);
                    let id = get_id(
                        args[0].clone(),
                        variables,
                        consts,
                        &mut output,
                        &ctx,
                        functions,
                    );
                    output.push(Instr::Str(id, id));
                }
                "bool" => {
                    check_args!(args, 1, "bool", ctx);
                    let id = get_id(
                        args[0].clone(),
                        variables,
                        consts,
                        &mut output,
                        &ctx,
                        functions,
                    );
                    output.push(Instr::Bool(id, id));
                }
                function => {
                    let (fn_code, exp_args): (Vec<Expr>, Box<[String]>) = {
                        if let Some((_, exp_args, code)) =
                            functions.iter().find(|(a, _, _)| a == function)
                        {
                            (code.to_vec(), exp_args.clone())
                        } else {
                            error!(ctx, format_args!("Unknown function {}", function.red()));
                        }
                    };
                    check_args!(args, exp_args.len(), function, ctx);

                    if let Some((name, loc, func_args, return_id)) = is_processing_function {
                        if name == function {
                            // recursive function, go back to function def and move on
                            // "return" doesn't work with recursive functions for now
                            for (i, _) in exp_args.iter().enumerate() {
                                let arg = args.get(i).unwrap();
                                let val = expr_to_data(arg.clone());
                                if val != Data::Null {
                                    consts[func_args[i].1 as usize] = val;
                                } else {
                                    print!("{arg}");
                                    let mut value = parser_to_instr_set(
                                        vec![arg.clone()],
                                        variables,
                                        consts,
                                        functions,
                                        is_processing_function,
                                    );
                                    move_to_id(&mut value, func_args[i].1);
                                    print!("VAL{value:?}");
                                    output.extend(value);
                                }
                            }

                            output.push(Instr::Jmp((output.len() as u16) - loc, true));
                            continue;
                        }
                    }

                    let mut fn_variables: Vec<(String, u16)> = Vec::new();
                    let mut instructions: Vec<Instr> = Vec::new();

                    for (i, x) in exp_args.iter().enumerate() {
                        let len = consts.len() as u16;
                        let mut value = parser_to_instr_set(
                            vec![args[i].clone()],
                            variables,
                            consts,
                            functions,
                            is_processing_function,
                        );
                        move_to_id(&mut value, len);
                        output.extend(value);
                        fn_variables.push((x.to_string(), len));
                    }
                    let vars = fn_variables.clone();
                    consts.push(Data::Null);
                    instructions.extend(parser_to_instr_set(
                        fn_code,
                        &mut fn_variables,
                        consts,
                        functions,
                        Some(&(
                            function.to_string(),
                            output.len() as u16,
                            vars,
                            Some((consts.len() - 1) as u16),
                        )),
                    ));
                    output.extend(instructions);
                }
            },
            Expr::ReturnVal(val) => {
                if let Some(x) = is_processing_function {
                    if let Some(ret_id) = x.3 {
                        let mut val = parser_to_instr_set(
                            vec![*val],
                            variables,
                            consts,
                            functions,
                            is_processing_function,
                        );
                        if val.is_empty() {
                            output.push(Instr::Mov((consts.len() - 1) as u16, ret_id));
                        } else {
                            move_to_id(&mut val, ret_id);
                            output.extend(val);
                        }
                    }
                }
                // error_b!("WTF");
            }
            Expr::FunctionDecl(x, y, z) => {
                if functions.iter().any(|(name, _, _)| name == &x) {
                    error!(ctx, format_args!("Function {} is already defined", x.red()));
                }
                functions.push((x, y, z));
            }
            Expr::Op(left, right) => {
                fn remove_priority(
                    x: Expr,
                    variables: &mut Vec<(String, u16)>,
                    consts: &mut Vec<Data>,
                ) -> Vec<Expr> {
                    match x {
                        Expr::Op(_, _) => process_op(x, variables, consts),
                        Expr::Priority(x) => {
                            let mut output: Vec<Expr> = Vec::new();
                            output.push(Expr::LPAREN);
                            output.extend(remove_priority(*x, variables, consts));
                            output.push(Expr::RPAREN);
                            output
                        }
                        _ => vec![x],
                    }
                }
                fn process_op(
                    op: Expr,
                    variables: &mut Vec<(String, u16)>,
                    consts: &mut Vec<Data>,
                ) -> Vec<Expr> {
                    let mut operation: Vec<Expr> = Vec::new();
                    if let Expr::Op(left, right) = op {
                        operation.extend(remove_priority(*left, variables, consts));
                        for x in right {
                            let val = *x.1;
                            operation.extend(remove_priority(Expr::Opcode(x.0), variables, consts));
                            if matches!(val, Expr::Op(_, _)) {
                                operation.extend(process_op(val, variables, consts));
                            } else {
                                operation.extend(remove_priority(val, variables, consts));
                            }
                        }
                    }
                    operation
                }


                print!("{left:?} {right:?}");
                let temp_op: Vec<Expr> = process_op(Expr::Op(left, right), variables, consts);
                print!("TEMPOP {temp_op:?}");
                let op = op_to_rpn(temp_op);
                print!("OP {op:?}");

                let mut item_stack: Vec<Expr> = Vec::new();
                let mut final_stack: Vec<Instr> = Vec::new();
                for x in op {
                    if let Expr::Opcode(op) = x {
                        if final_stack.is_empty() {
                            let last = item_stack.pop().unwrap();
                            print!("1.OLD IS {last}");
                            let first = item_stack.pop().unwrap();
                            print!("1.NEW IS {first}");

                            let first_v =
                                get_id(first, variables, consts, &mut output, &ctx, functions);
                            let second_v =
                                get_id(last, variables, consts, &mut output, &ctx, functions);
                            consts.push(Data::Null);
                            let x = first_v;
                            let y = second_v;
                            let z = consts.len() - 1;

                            print!("1.{x} {y} {z}");

                            handle_ops!(final_stack, x, y, z as u16, op, consts);
                        } else {
                            print!("2.OLD IS {:?}", final_stack.last().unwrap());
                            let old_id = get_tgt_id(*final_stack.last().unwrap());
                            let new = item_stack.pop().unwrap();
                            print!("2.NEW IS {new}");
                            let new_v =
                                get_id(new, variables, consts, &mut output, &ctx, functions);
                            consts.push(Data::Null);
                            let x = new_v;
                            let y = old_id;
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op, consts);
                            print!("2.{x} {y} {z}");
                        }
                    } else {
                        item_stack.push(x);
                    }
                }
                print!("CONSTS {consts:?}");
                print!("VARS {variables:?}");
                print!("FINAL STACK {final_stack:?}");
                output.extend(final_stack);
            }
            Expr::Priority(x) => {
                output.extend(parser_to_instr_set(
                    vec![*x],
                    variables,
                    consts,
                    functions,
                    is_processing_function,
                ));
            }
            other => {
                error!(ctx, format_args!("Not implemented {other:?}"));
            }
        }
    }

    output
}

fn print_instructions(instructions: &[Instr]) {
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
        });
    }
}


fn parse(contents: String) -> (Vec<Instr>, Vec<Data>) {
    let mut functions: Vec<Expr> = grammar::FileParser::new().parse(&contents).unwrap();
    let main_function: Vec<Expr> = {
        if let Some(fctn) = functions.iter().position(|a| {
            if let Expr::FunctionDecl(name, _, _) = a {
                name == "main"
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
                (x.to_string(), y.clone(), z.clone())
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

// Live long and prosper
fn main() {
    let contents = std::fs::read_to_string("test.spock").unwrap();
    let (instructions, mut consts) = parse(contents);


    let now = Instant::now();
    execute(&instructions, &mut consts);
    println!("EXEC TIME {:.2?}", now.elapsed());
}
