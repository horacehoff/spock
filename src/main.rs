use colored::Colorize;
use concat_string::concat_string;
use internment::Intern;
use lalrpop_util::lalrpop_mod;
use std::cmp::PartialEq;
use std::fs;
use std::time::Instant;

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
}

macro_rules! error {
    ($x: expr) => {
        eprintln!(
            "--------------\n\u{001b}[31mSPOCK ERROR:\u{001b}[0m\n{}\n--------------", $x
        );
        std::process::exit(1);
    };
    ($x: expr, $y: expr) => {
        eprintln!(
            "--------------\n\u{001b}[31mSPOCK ERROR:\u{001b}[0m\n{}\n\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m\n{}\n--------------", $x, $y
        );
        std::process::exit(1);
    }
}

fn execute(instructions: &[Instr], consts: &mut [Data]) {
    let len = instructions.len();
    let mut i: usize = 0;
    while i < len {
        match instructions[i] {
            Instr::Jmp(size, is_neg) => {
                if is_neg {
                    i -= size as usize;
                    continue;
                }
                i += size as usize;
                continue;
            }
            Instr::Cmp(cond_id, size) => {
                if consts[cond_id as usize] == Data::Bool(false) {
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
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} + {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Mul(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Number(parent * child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} * {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Div(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Number(parent / child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} / {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Sub(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Number(parent - child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} - {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Mod(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Number(parent % child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} % {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Pow(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Number(parent.powf(child));
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} ^ {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Eq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];
                consts[dest as usize] = Data::Bool(first_elem == second_elem);
            }
            Instr::NotEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];
                consts[dest as usize] = Data::Bool(first_elem != second_elem);
            }
            Instr::Sup(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Bool(parent > child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} > {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::SupEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Bool(parent >= child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} >= {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Inf(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];
                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Bool(parent < child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} < {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::InfEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        consts[dest as usize] = Data::Bool(parent <= child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} <= {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::BoolAnd(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Bool(parent), Data::Bool(child)) => {
                        consts[dest as usize] = Data::Bool(parent && child);
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} && {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::BoolOr(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Bool(parent), Data::Bool(child)) => {
                        consts[dest as usize] = Data::Bool(parent || child)
                    }
                    _ => {
                        error!(format_args!(
                            "UNSUPPORTED OPERATION: {:?} || {:?}",
                            first_elem, second_elem
                        ));
                    }
                }
            }
            Instr::Mov(tgt, dest) => {
                consts[dest as usize] = consts[tgt as usize];
            }
            Instr::Print(target) => {
                let elem = consts[target as usize];
                println!("PRINTING => {elem:?}");
            }
            Instr::Null => {
                error!("NULL INSTRUCTION");
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
}

lalrpop_mod!(pub grammar);

fn get_precedence(operator: Expr) -> u8 {
    if let Expr::Opcode(op) = operator {
        match op {
            Opcode::BoolOr => 1,
            Opcode::BoolAnd => 2,
            Opcode::Eq => 3,
            Opcode::NotEq => 3,
            Opcode::Inf => 4,
            Opcode::InfEq => 4,
            Opcode::Sup => 4,
            Opcode::SupEq => 4,
            Opcode::Add => 5,
            Opcode::Sub => 5,
            Opcode::Mul => 6,
            Opcode::Div => 6,
            Opcode::Mod => 6,
            Opcode::Pow => 7,
            Opcode::Null => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

fn is_left_associative(operator: Expr) -> bool {
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
                && (get_precedence(op_stack.last().unwrap().clone()) > get_precedence(x.clone())
                    || (get_precedence(op_stack.last().unwrap().clone())
                        == get_precedence(x.clone())
                        && is_left_associative(x.clone())))
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
        Instr::Mov(_, y) => y,
        Instr::Add(_, _, y) => y,
        Instr::Mul(_, _, y) => y,
        Instr::Sub(_, _, y) => y,
        Instr::Div(_, _, y) => y,
        Instr::Mod(_, _, y) => y,
        Instr::Pow(_, _, y) => y,
        Instr::Eq(_, _, y) => y,
        Instr::NotEq(_, _, y) => y,
        Instr::Sup(_, _, y) => y,
        Instr::SupEq(_, _, y) => y,
        Instr::Inf(_, _, y) => y,
        Instr::InfEq(_, _, y) => y,
        Instr::BoolAnd(_, _, y) => y,
        Instr::BoolOr(_, _, y) => y,
        _ => unreachable!(),
    }
}

fn move_to_id(x: &mut [Instr], tgt_id: u16) {
    if x.is_empty() {
        return;
    }
    match x.last_mut().unwrap() {
        Instr::Mov(_, z) => *z = tgt_id,
        Instr::Add(_, _, z) => *z = tgt_id,
        Instr::Mul(_, _, z) => *z = tgt_id,
        Instr::Sub(_, _, z) => *z = tgt_id,
        Instr::Div(_, _, z) => *z = tgt_id,
        Instr::Mod(_, _, z) => *z = tgt_id,
        Instr::Pow(_, _, z) => *z = tgt_id,
        Instr::Eq(_, _, z) => *z = tgt_id,
        Instr::NotEq(_, _, z) => *z = tgt_id,
        Instr::Sup(_, _, z) => *z = tgt_id,
        Instr::SupEq(_, _, z) => *z = tgt_id,
        Instr::Inf(_, _, z) => *z = tgt_id,
        Instr::InfEq(_, _, z) => *z = tgt_id,
        Instr::BoolAnd(_, _, z) => *z = tgt_id,
        Instr::BoolOr(_, _, z) => *z = tgt_id,
        _ => unreachable!(),
    }
}

macro_rules! handle_ops {
    ($final_stack: expr, $x: expr, $y: expr, $z: expr, $op: expr) => {
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
            Opcode::Null => unreachable!(),
        }
    };
}

fn get_id(x: Expr, variables: &mut [(String, u16)], consts: &mut Vec<Data>) -> (u16, bool) {
    match x {
        Expr::Num(num) => {
            consts.push(Data::Number(num));
            ((consts.len() - 1) as u16, false)
        }
        Expr::String(str) => {
            consts.push(Data::String(Intern::from(str)));
            ((consts.len() - 1) as u16, false)
        }
        Expr::Bool(bool) => {
            consts.push(Data::Bool(bool));
            ((consts.len() - 1) as u16, false)
        }
        Expr::Var(name) => {
            if let Some((_, id)) = variables.iter().find(|(x, _)| &name == x) {
                (*id, true)
            } else {
                error!(format_args!("Unknown variable {}", name.red()));
            }
        }
        _ => todo!(""),
    }
}

macro_rules! print {
    ($($x:tt)*) => {
        #[cfg(debug_assertions)]
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($x)*))
    }
}

fn parser_to_instr_set(
    input: Vec<Expr>,
    variables: &mut Vec<(String, u16)>,
    consts: &mut Vec<Data>,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::new();
    for x in input {
        match x {
            Expr::Num(num) => consts.push(Data::Number(num)),
            Expr::Bool(bool) => consts.push(Data::Bool(bool)),
            Expr::String(str) => consts.push(Data::String(Intern::from(str))),
            Expr::Condition(x, y, z, w) => {
                let condition = parser_to_instr_set(vec![*x], variables, consts);
                output.extend(condition);
                let condition_id = get_tgt_id(*output.last().unwrap());
                let mut priv_vars = variables.clone();
                let cond_code = parser_to_instr_set(y.into_vec(), &mut priv_vars, consts);
                let len = cond_code.len() + 1;
                output.push(Instr::Cmp(condition_id, len as u16));
                output.extend(cond_code);

                print!("CONDITION IS {condition_id:?}");
                // TODO
            }
            Expr::WhileBlock(x, y) => {
                let condition = parser_to_instr_set(vec![*x], variables, consts);
                output.extend(condition);
                let condition_id = get_tgt_id(*output.last().unwrap());
                let mut priv_vars = variables.clone();
                let cond_code = parser_to_instr_set(y.into_vec(), &mut priv_vars, consts);
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
                        error!(format_args!("Unknown variable {}", name.red()));
                    }
                } else {
                    consts.push(Data::Null);
                    let len = (consts.len() - 1) as u16;
                    variables.push((x, len));
                    let mut val = parser_to_instr_set(vec![val], variables, consts);
                    move_to_id(&mut val, len);
                    output.extend(val);
                }
            }
            Expr::VarAssign(x, y) => {
                let id = variables
                    .iter()
                    .find(|(w, _)| w == &x)
                    .unwrap_or_else(|| panic!("Unknown variable {x}"))
                    .1;
                let val = *y;
                if let Expr::Num(data) = val {
                    consts.push(Data::Number(data));
                    output.push(Instr::Mov((consts.len() - 1) as u16, id))
                } else if let Expr::String(data) = val {
                    consts.push(Data::String(Intern::from(data)));
                    output.push(Instr::Mov((consts.len() - 1) as u16, id))
                } else if let Expr::Bool(data) = val {
                    consts.push(Data::Bool(data));
                    output.push(Instr::Mov((consts.len() - 1) as u16, id))
                } else if let Expr::Var(name) = val {
                    if let Some((_, var_id)) = variables.iter().find(|(x, _)| &name == x) {
                        output.push(Instr::Mov(*var_id, id));
                    } else {
                        error!(format_args!("Unknown variable {}", name.red()));
                    }
                } else {
                    let mut value = parser_to_instr_set(vec![val], variables, consts);
                    move_to_id(&mut value, id);
                    output.extend(value);
                }
            }
            Expr::FunctionCall(x, args) => match x.as_str() {
                "print" => {
                    for arg in args {
                        let (id, _) = get_id(arg, variables, consts);
                        output.push(Instr::Print(id));
                    }
                }
                unknown => {
                    error!(format_args!("Unknown function {}", unknown.red()));
                }
            },
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
                        for x in right.iter_mut() {
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
                        if !final_stack.is_empty() {
                            let old_id = get_tgt_id(*final_stack.last().unwrap());
                            let new = item_stack.pop().unwrap();

                            let (new_v, _) = get_id(new, variables, consts);
                            consts.push(Data::Null);
                            let x = old_id;
                            let y = new_v;
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op)
                        } else {
                            let last = item_stack.pop().unwrap();
                            let first = item_stack.pop().unwrap();

                            let (first_v, _) = get_id(first, variables, consts);
                            let (second_v, _) = get_id(last, variables, consts);
                            let x = first_v;
                            let y = second_v;
                            consts.push(Data::Null);
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op);
                        }
                    } else {
                        item_stack.push(x);
                    }
                }
                output.extend(final_stack);
            }
            _ => {
                error!("Not implemented");
            }
        }
    }

    output
}

// Live long and prosper
fn main() {
    // dbg!(size_of::<Instr>());
    // dbg!(size_of::<Data>());
    // dbg!(size_of::<Expr>());

    let now = Instant::now();

    print!("READ FILE");
    let contents = fs::read_to_string("test.spock").unwrap();
    print!("PARSE");
    let parsed = grammar::CodeParser::new().parse(&contents).unwrap();
    print!("{parsed:?}");

    let mut variables: Vec<(String, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let instructions = parser_to_instr_set(parsed.into_vec(), &mut variables, &mut consts);
    println!("INSTR OUT {instructions:?}");
    println!("CONSTS ARE {consts:?}");
    print!("VARS ARE {variables:?}");
    println!("Parsed in {:.2?}", now.elapsed());

    let now = Instant::now();
    // let instructions: Vec<Instr> = vec![
    //     Instr::Inf(0, 3, 6),
    //     Instr::Cmp(6, 7),
    //     Instr::Add(0, 5, 0),
    //     Instr::Mul(1, 2, 1),
    //     Instr::Sup(1, 4, 7),
    //     Instr::Cmp(7, 1),
    //     Instr::Mod(1, 4, 1),
    //     Instr::Jmp(7, true),
    //     Instr::Print(1),
    // ];
    // let mut consts: Vec<Data> = vec![
    //     // count
    //     Data::Number(0.0),
    //     // result
    //     Data::Number(1.0),
    //     // nums
    //     Data::Number(2.0),       // -> 2
    //     Data::Number(1000000.0), // -> 3
    //     Data::Number(1000000.0), // -> 4
    //     Data::Number(1.0),       // -> 5
    //     Data::Bool(false),       // -> 6
    //     Data::Bool(false),       // -> 7
    // ];
    execute(&instructions, &mut consts);
    print!("CONSTS are {consts:?}");
    println!("EXEC TIME {:.2?}", now.elapsed());
}
