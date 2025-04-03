use concat_string::concat_string;
use inline_colorization::*;
use internment::Intern;
use likely_stable::if_likely;
use std::cmp::PartialEq;
use std::f64::consts::E;
use std::time::Instant;
use builtin_funcs::FUNCS;
use crate::parser::{grammar, parser_to_instr_set, Expr};
use crate::util::print_instructions;

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
