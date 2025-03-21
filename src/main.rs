use colored::Colorize;
use concat_string::concat_string;
use inline_colorization::*;
use internment::Intern;
use lalrpop_util::lalrpop_mod;
use std::cmp::PartialEq;
use std::fmt::Formatter;
use std::hint::unreachable_unchecked;
use std::slice;
use std::time::Instant;
use likely_stable::if_likely;

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

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Instr {
    Null,
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Number(parent), Data::Number(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Bool(parent), Data::Bool(child)) = (first_elem, second_elem) => {
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

                if_likely!{let (Data::Bool(parent), Data::Bool(child)) = (first_elem, second_elem) => {
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
                if_likely!{let Data::Number(x) = tgt => {
                    consts[dest as usize] = Data::Number(-x);
                } else {
                    error_b!(format_args!("UNSUPPORTED OPERATION: -{tgt:?}"));
                }}
            }
            Instr::Print(target) => {
                let elem = consts[target as usize];
                println!("PRINTING => {elem:?}");
            }
            Instr::Abs(tgt, dest) => {
                if let Data::Number(x) = consts[tgt as usize] {
                    consts[dest as usize] = Data::Number(x.abs());
                }
            }
            Instr::Null => unsafe { unreachable_unchecked() },
        }
        i += 1;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Null,
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


    FunctionDecl(String, Box<[String]>, Box<[Expr]>)
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
                    "if {x} {{\n{}}}",
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
        | Instr::Abs(_, y) => y,
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
        | Instr::Abs(_, z) => *z = tgt_id,
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
    line: &Expr,
    functions: &mut Vec<(String,Box<[String]>,Box<[Expr]>)>
) -> u16 {
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
            let out = parser_to_instr_set(vec![x], variables, consts,functions,&None);
            instr.append(&mut out.clone());
            get_tgt_id(*out.last().unwrap())
        }
    }
}

fn returns_bool(instruction: Instr) -> bool {
    matches!(
        instruction,
        Instr::Eq(_, _, _)
            | Instr::NotEq(_, _, _)
            | Instr::Sup(_, _, _)
            | Instr::SupEq(_, _, _)
            | Instr::Inf(_, _, _)
            | Instr::InfEq(_, _, _)
            | Instr::BoolOr(_, _, _)
            | Instr::BoolAnd(_, _, _)
    )
}

fn offset_id(instr: &mut [Instr], amount: u16) {
    for x in instr.iter_mut() {
        match x {
            Instr::Print(id) |  Instr::Jmp(id, _) => *id += amount,
            Instr::Cmp(a, b) | Instr::Mov(a, b) | Instr::Neg(a, b) | Instr::Abs(a, b) => {
                *a += amount;
                *b += amount;
            }
            Instr::Add(a, b, c)
            | Instr::Mul(a, b, c)
            | Instr::Sub(a, b, c)
            | Instr::Div(a, b, c)
            | Instr::Mod(a, b, c)
            | Instr::Pow(a, b, c)
            | Instr::Eq(a, b, c)
            | Instr::NotEq(a, b, c)
            | Instr::Sup(a, b, c)
            | Instr::SupEq(a, b, c)
            | Instr::Inf(a, b, c)
            | Instr::InfEq(a, b, c)
            | Instr::BoolAnd(a, b, c)
            | Instr::BoolOr(a, b, c)  => {
                *a += amount;
                *b += amount;
                *c += amount;
            }
            Instr::Null => unreachable!()
        }
    }
}

// fn stay_true(x: bool, var: bool) -> bool {
//     if var {
//         true
//     } else {
//         x
//     }
// }
//
// fn check_recursion(instructions: &[Expr], fn_name: &str) -> bool {
//     let mut is_recursive = false;
//     for x in instructions {
//         match x {
//             Expr::Op(x, y) => {
//                 is_recursive = stay_true(check_recursion(slice::from_ref(x), fn_name), is_recursive);
//                 for w in y {
//                     is_recursive = stay_true(check_recursion(slice::from_ref(&w.1), fn_name), is_recursive);
//                 }
//             }
//             Expr::Priority(x) => is_recursive = stay_true(check_recursion(slice::from_ref(x), fn_name), is_recursive),
//             Expr::VarDeclare(_, x) | Expr::VarAssign(_, x) => is_recursive = stay_true(check_recursion(slice::from_ref(x), fn_name), is_recursive),
//             Expr::Condition(x, y, z, w) => {
//                 is_recursive = stay_true(check_recursion(slice::from_ref(x), fn_name), is_recursive);
//                 is_recursive = stay_true(check_recursion(y.iter().as_slice(), fn_name), is_recursive);
//                 is_recursive = stay_true(check_recursion(z.iter().as_slice(), fn_name), is_recursive);
//                 if let Some(opt) = w {
//                     is_recursive = stay_true(check_recursion(opt.iter().as_slice(), fn_name), is_recursive);
//                 }
//             }
//             Expr::ElseIfBlock(x, y)
//             | Expr::WhileBlock(x, y) => {
//                 is_recursive = stay_true(check_recursion(slice::from_ref(x), fn_name), is_recursive);
//                 is_recursive = stay_true(check_recursion(y.iter().as_slice(), fn_name), is_recursive);
//             }
//             Expr::FunctionCall(x, _) => {
//                 if x == fn_name {
//                     is_recursive = true;
//                 }
//             }
//             _ => continue
//         }
//     }
//     is_recursive
// }

fn parser_to_instr_set(
    input: Vec<Expr>,
    variables: &mut Vec<(String, u16)>,
    consts: &mut Vec<Data>,
    functions: &mut Vec<(String, Box<[String]>, Box<[Expr]>)>,
    is_processing_function: &Option<(String, u16)>,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::new();
    for x in input {
        let ctx = x.clone();
        match x {
            Expr::Num(num) => consts.push(Data::Number(num)),
            Expr::Bool(bool) => consts.push(Data::Bool(bool)),
            Expr::String(str) => consts.push(Data::String(Intern::from(str))),
            Expr::Condition(x, y, z, w) => {
                if matches!(*x, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", *x));
                }
                let condition = parser_to_instr_set(vec![*x.clone()], variables, consts,functions,&None);
                if !returns_bool(*condition.last().unwrap()) {
                    error!(ctx, format_args!("{} is not a bool", *x));
                }
                output.extend(condition);
                let condition_id = get_tgt_id(*output.last().unwrap());
                let mut priv_vars = variables.clone();
                let cond_code = parser_to_instr_set(y.into_vec(), &mut priv_vars, consts,functions,&None);
                let len = cond_code.len() + 1;
                output.push(Instr::Cmp(condition_id, len as u16));
                output.extend(cond_code);

                print!("{consts:?}");
                print!("CONDITION IS {condition_id:?}");
                // TODO
            }
            Expr::WhileBlock(x, y) => {
                if matches!(*x, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", *x));
                }
                let condition = parser_to_instr_set(vec![*x.clone()], variables, consts,functions,&None);
                if !returns_bool(*condition.last().unwrap()) {
                    error!(ctx, format_args!("{} is not a bool", *x));
                }
                output.extend(condition);
                let condition_id = get_tgt_id(*output.last().unwrap());
                let mut priv_vars = variables.clone();
                let cond_code = parser_to_instr_set(y.into_vec(), &mut priv_vars, consts,functions,&None);
                let len = (cond_code.len() + 2) as u16;
                output.push(Instr::Cmp(condition_id, len));
                output.extend(cond_code);
                output.push(Instr::Jmp(len, true));
            }
            Expr::VarDeclare(x, y) => {
                let val = *y;
                if let Expr::Num(data) = val {
                    consts.push(Data::Number(data));
                    variables.push((x, (consts.len() - 1) as u16));
                } else if let Expr::String(data) = val {
                    consts.push(Data::String(Intern::from(data)));
                    variables.push((x, (consts.len() - 1) as u16));
                } else if let Expr::Bool(data) = val {
                    consts.push(Data::Bool(data));
                    variables.push((x, (consts.len() - 1) as u16));
                } else if let Expr::Var(name) = val {
                    consts.push(Data::Null);
                    variables.push((x, (consts.len() - 1) as u16));
                    if let Some((_, var_id)) = variables.iter().find(|(x, _)| &name == x) {
                        output.push(Instr::Mov(*var_id, (consts.len() - 1) as u16));
                    } else {
                        error!(
                            ctx,
                            format_args!("Unknown variable {}", name.red()),
                            format_args!("Add 'let {name} = 0;'")
                        );
                    }
                } else {
                    consts.push(Data::Null);
                    let len = (consts.len() - 1) as u16;
                    variables.push((x, len));
                    let mut val = parser_to_instr_set(vec![val], variables, consts,functions,&None);
                    move_to_id(&mut val, len);
                    output.extend(val);
                }
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
                let val = *y;
                if let Expr::Num(data) = val {
                    consts.push(Data::Number(data));
                    output.push(Instr::Mov((consts.len() - 1) as u16, id));
                } else if let Expr::String(data) = val {
                    consts.push(Data::String(Intern::from(data)));
                    output.push(Instr::Mov((consts.len() - 1) as u16, id));
                } else if let Expr::Bool(data) = val {
                    consts.push(Data::Bool(data));
                    output.push(Instr::Mov((consts.len() - 1) as u16, id));
                } else if let Expr::Var(name) = val {
                    if let Some((_, var_id)) = variables.iter().find(|(x, _)| &name == x) {
                        output.push(Instr::Mov(*var_id, id));
                    } else {
                        error!(
                            ctx,
                            format_args!("Unknown variable {}", name.red()),
                            format_args!("Add 'let {name} = 0;'")
                        );
                    }
                } else {
                    let mut value = parser_to_instr_set(vec![val], variables, consts, functions,&None);
                    move_to_id(&mut value, id);
                    output.extend(value);
                }
            }
            Expr::FunctionCall(x, args) => match x.as_str() {
                "print" => {
                    for arg in args {
                        let id = get_id(arg, variables, consts, &mut output, &ctx,functions);
                        output.push(Instr::Print(id));
                    }
                }
                "abs" => {
                    if args.len() > 1 {
                        error!(
                            ctx,
                            "Function 'abs' only expects one argument",
                            format_args!("Replace with 'abs({})'", args[0])
                        );
                    }
                    let id = get_id(args[0].clone(), variables, consts, &mut output, &ctx, functions);
                    output.push(Instr::Abs(id, id));
                }
                function => {
                    let (fn_code, exp_args):(Vec<Expr>,Box<[String]>) = {
                        if let Some((_, exp_args, code)) = functions.iter().find(|(a,_,_)| a == function) {
                            (code.to_vec(), exp_args.clone())
                        } else {
                            error!(ctx, format_args!("Unknown function {}", function.red()));
                        }
                    };

                    if let Some((name, loc)) = is_processing_function {
                        if name == function {
                            // recursive function, go back to function def and move on
                            output.push(Instr::Jmp((output.len() as u16) - loc, true));
                            continue;
                        }
                    }


                    let mut fn_variables: Vec<(String, u16)> = Vec::new();
                    let mut instructions:Vec<Instr> = Vec::new();

                    for (i,x) in exp_args.iter().enumerate() {
                        let match_value = args.get(i).unwrap();
                        if let Expr::Num(num) = match_value {
                            consts.push(Data::Number(*num));
                            fn_variables.push((x.to_string(), (consts.len() - 1) as u16));
                        } else if let Expr::String(val) = match_value {
                            consts.push(Data::String(Intern::from(val.to_string())));
                            fn_variables.push((x.to_string(), (consts.len() - 1) as u16));
                        } else if let Expr::Bool(val) = match_value {
                            consts.push(Data::Bool(*val));
                            fn_variables.push((x.to_string(), (consts.len() - 1) as u16));
                        }  else if let Expr::Var(name) = match_value {
                            if let Some((_, var_id)) = variables.iter().find(|(w, _)| name == w) {
                                consts.push(Data::Null);
                                instructions.push(Instr::Mov(*var_id, (consts.len() - 1) as u16));
                                fn_variables.push((x.to_string(), (consts.len() - 1) as u16));
                            } else {
                                error!(
                                    ctx,
                                    format_args!("Unknown variable {}", name.red()),
                                    format_args!("Add 'let {name} = 0;'")
                                );
                            }
                        }
                        else {
                            let mut value = parser_to_instr_set(vec![match_value.clone()], variables, consts, functions,&None);
                            consts.push(Data::Null);
                            move_to_id(&mut value, (consts.len() - 1) as u16);
                            output.extend(value);
                            fn_variables.push((x.to_string(), (consts.len() - 1) as u16));
                        }
                    }

                    instructions.extend(parser_to_instr_set(fn_code, &mut fn_variables, consts, functions,&Some((function.to_string(), output.len() as u16))));
                    output.extend(instructions);

                }
            },
            Expr::FunctionDecl(x,y,z) => {
                functions.push((x,y,z));
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
                            let mut output: Vec<Expr> = vec![];
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
                    let mut operation: Vec<Expr> = vec![];
                    if let Expr::Op(left, mut right) = op {
                        operation.extend(remove_priority(*left, variables, consts));
                        for x in &mut right {
                            let val = *x.1.clone();
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
                            let first = item_stack.pop().unwrap();

                            let first_v = get_id(first, variables, consts, &mut output, &ctx,functions);
                            let second_v = get_id(last, variables, consts, &mut output, &ctx,functions);
                            consts.push(Data::Null);
                            let x = first_v;
                            let y = second_v;
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op, consts);
                        } else {
                            let old_id = get_tgt_id(*final_stack.last().unwrap());
                            let new = item_stack.pop().unwrap();

                            let new_v = get_id(new, variables, consts, &mut output, &ctx,functions);
                            consts.push(Data::Null);
                            let x = old_id;
                            let y = new_v;
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op, consts);
                        }
                    } else {
                        item_stack.push(x);
                    }
                }
                output.extend(final_stack);
            }
            other => {
                error!(ctx, format_args!("Not implemented {other:?}"));
            }
        }
    }

    output
}





// Live long and prosper
fn main() {
    dbg!(size_of::<Instr>());
    // dbg!(size_of::<Data>());
    // dbg!(size_of::<Expr>());

    let now = Instant::now();

    let contents = std::fs::read_to_string("test.spock").unwrap();
    let mut functions:Vec<Expr> = grammar::FileParser::new().parse(&contents).unwrap();
    let main_function:Vec<Expr> = {
        if let Some(fctn) = functions.iter().position(|a| {
            if let Expr::FunctionDecl(name, _, _) = a {
                name == "main"
            } else {
                false
            }
        }) {
            if let Expr::FunctionDecl(_,_,code) = functions.swap_remove(fctn) {
                code.to_vec()
            } else {
                error!(contents, "No main function");
            }
        } else {
            error!(contents, "No main function");
        }
    };

    let mut functions:Vec<(String,Box<[String]>,Box<[Expr]>)> = functions.iter().map(|w| {
        if let Expr::FunctionDecl(x,y,z) = w {
            (x.to_string(),y.clone(),z.clone())
        } else {
            error!(contents, "Function expected");
        }
    }).collect();

    print!("{functions:?}");

    let mut variables: Vec<(String, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let instructions = parser_to_instr_set(main_function, &mut variables, &mut consts, &mut functions,&None);
    print!("INSTR OUT {instructions:?}");
    print!("CONSTS ARE {consts:?}");
    print!("VARS ARE {variables:?}");
    println!("Parsed in {:.2?}", now.elapsed());

    let now = Instant::now();
    execute(&instructions, &mut consts);
    print!("CONSTS are {consts:?}");
    println!("EXEC TIME {:.2?}", now.elapsed());
}
