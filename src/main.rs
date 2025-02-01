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
    // Data(u16),

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

fn execute(instructions: &[Instr], consts: &mut [Data]) {
    let len = instructions.len();
    let mut i: usize = 0;
    while i < len {
        // println!("{consts:?}");
        match instructions[i] {
            Instr::Jmp(size, is_neg) => {
                if is_neg {
                    i -= size as usize;
                    continue;
                } else {
                    i += size as usize;
                    continue;
                }
            }
            Instr::Mov(tgt, dest) => {
                consts[dest as usize] = consts[tgt as usize];
            }
            Instr::Cmp(cond_id, size) => {
                let condition = consts[cond_id as usize];
                if let Data::Bool(false) = condition {
                    i += size as usize;
                    continue;
                }
            }
            Instr::Add(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent + child;
                        consts[dest as usize] = Data::Number(result);
                    }
                    (Data::String(parent), Data::String(child)) => {
                        let result = concat_string!(*parent, *child);
                        consts[dest as usize] = Data::String(Intern::from(result));
                    }
                    x => todo!("{x:?}"),
                }
            }
            Instr::Mul(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent * child;
                        consts[dest as usize] = Data::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Div(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent / child;
                        consts[dest as usize] = Data::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Sub(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent - child;
                        consts[dest as usize] = Data::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Mod(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent % child;
                        consts[dest as usize] = Data::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Eq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent == child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    (Data::Bool(parent), Data::Bool(child)) => {
                        let result = parent == child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    (Data::String(parent), Data::String(child)) => {
                        let result = parent == child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::NotEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent != child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    (Data::Bool(parent), Data::Bool(child)) => {
                        let result = parent != child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    (Data::String(parent), Data::String(child)) => {
                        let result = parent != child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Sup(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent > child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::SupEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent >= child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Inf(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];
                // println!("IS {first_elem:?} INF TO {second_elem:?}???");
                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent < child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::InfEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Number(parent), Data::Number(child)) => {
                        let result = parent <= child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::BoolAnd(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Bool(parent), Data::Bool(child)) => {
                        let result = parent && child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::BoolOr(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Data::Bool(parent), Data::Bool(child)) => {
                        let result = parent || child;
                        consts[dest as usize] = Data::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Print(target) => {
                let elem = consts[target as usize];
                // println!("{consts:?}");
                println!("PRINTING => {elem:?}");
            }
            _ => todo!(""),
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
            Opcode::Null => {
                panic!();
            }
            Opcode::Add => 2,
            Opcode::Sub => 2,
            Opcode::Div => 3,
            Opcode::Mul => 3,
            Opcode::Pow => 4,
            Opcode::Mod => 3,
            Opcode::Eq => 1,
            Opcode::NotEq => 1,
            Opcode::BoolAnd => 1,
            Opcode::Inf => 1,
            Opcode::InfEq => 1,
            Opcode::BoolOr => 1,
            Opcode::Sup => 1,
            Opcode::SupEq => 1,
        }
    } else {
        todo!("")
    }
}

fn is_left_associative(operator: Expr) -> bool {
    if let Expr::Opcode(op) = operator {
        match op {
            Opcode::Null => {
                panic!();
            }
            Opcode::Pow => false,
            _ => true,
        }
    } else {
        todo!("")
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
            assert_eq!(op_stack.last().unwrap(), &Expr::LPAREN, "WHAT??");
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
        Instr::Null => todo!(""),
        Instr::Print(_) => todo!(""),
        Instr::Jmp(_, _) => todo!(""),
        Instr::Cmp(_, _) => todo!(""),
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
    }
}

fn move_to_id(x: &mut Vec<Instr>, tgt_id: u16) {
    println!("X IS {x:?}");
    if x.is_empty() {
        return;
    }
    match x.last_mut().unwrap() {
        Instr::Null => todo!(""),
        Instr::Print(_) => todo!(""),
        Instr::Jmp(_, _) => todo!(""),
        Instr::Cmp(_, _) => todo!(""),
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
    }
}

macro_rules! handle_ops {
    ($final_stack: expr, $x: expr, $y: expr, $z: expr, $op: expr) => {
        match $op {
            Opcode::Null => {}
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
        }
    };
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
            Expr::Var(name) => {
                if let Some((_, id)) = variables.iter().find(|(x, _)| x == &name) {
                    consts.push(consts[*id as usize])
                }
            }
            Expr::Condition(x, y, z, w) => {
                let condition = parser_to_instr_set(vec![*x], variables, consts);
                output.extend(condition);
                let condition_id = get_tgt_id(*output.last().unwrap());
                let mut priv_vars = variables.clone();
                let cond_code = parser_to_instr_set(y.into_vec(), &mut priv_vars, consts);
                let len = cond_code.len() + 1;
                output.push(Instr::Cmp(condition_id, len as u16));
                output.extend(cond_code);

                println!("CONDITION IS {condition_id:?}");
                // TODO
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
                if let Some((name, id)) = variables.clone().iter().find(|(w, _)| w == &x) {
                    let val = *y;
                    if let Expr::Num(data) = val {
                        consts.push(Data::Number(data));
                        output.push(Instr::Mov((consts.len() - 1) as u16, *id))
                    } else if let Expr::String(data) = val {
                        consts.push(Data::String(Intern::from(data)));
                        output.push(Instr::Mov((consts.len() - 1) as u16, *id))
                    } else if let Expr::Bool(data) = val {
                        consts.push(Data::Bool(data));
                        output.push(Instr::Mov((consts.len() - 1) as u16, *id))
                    } else {
                        let mut value = parser_to_instr_set(vec![val], variables, consts);
                        move_to_id(&mut value, *id);
                        output.extend(value);
                    }
                } else {
                    todo!("Var {x} doesn't exist")
                }
            }
            Expr::FunctionCall(x, y) => {
                let args: Vec<Expr> = y.into_vec();
                match x.as_str() {
                    "print" => {
                        let arg = args[0].clone();
                        if let Expr::Var(id) = arg {
                            let var = variables.iter().find(|(name, _)| name == &id).unwrap().1;
                            output.push(Instr::Print(var));
                        }
                    }
                    unknown => todo!("{unknown}"),
                }
            }
            Expr::Op(left, right) => {
                fn remove_priority(
                    x: Expr,
                    variables: &mut Vec<(String, u16)>,
                    consts: &mut Vec<Data>,
                ) -> Vec<Expr> {
                    match x {
                        Expr::Num(_) => return vec![x],
                        Expr::Bool(_) => return vec![x],
                        Expr::Op(_, _) => return process_op(x, variables, consts),
                        Expr::Opcode(_) => return vec![x],
                        Expr::Priority(x) => {
                            let mut output: Vec<Expr> = vec![];
                            output.push(Expr::LPAREN);
                            output.extend(remove_priority(*x, variables, consts));
                            output.push(Expr::RPAREN);
                            return output;
                        }
                        Expr::String(_) => return vec![x],
                        Expr::Var(_) => return vec![x],
                        Expr::VarDeclare(_, _) => return vec![x],
                        Expr::VarAssign(_, _) => return vec![x],
                        Expr::Condition(_, _, _, _) => return vec![x],
                        Expr::ElseIfBlock(_, _) => return vec![x],
                        Expr::WhileBlock(_, _) => return vec![x],
                        Expr::FunctionCall(_, _) => return vec![x],
                        Expr::LPAREN => return vec![x],
                        Expr::RPAREN => return vec![x],
                        // _ => todo!("{x:?}")
                    };
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
                let op = op_to_rpn(temp_op);
                println!("OP {op:?}");

                let mut item_stack: Vec<Expr> = Vec::new();
                let mut final_stack: Vec<Instr> = Vec::new();
                for x in op {
                    if let Expr::Opcode(op) = x {
                        if !final_stack.is_empty() {
                            let old_id = get_tgt_id(*final_stack.last().unwrap());
                            let new = item_stack.pop().unwrap();
                            output.extend(parser_to_instr_set(vec![new], variables, consts));

                            let len = consts.len();
                            let x = old_id;
                            let y = (len - 1) as u16;
                            let z = y;
                            handle_ops!(final_stack, x, y, z, op)
                        } else {
                            let last = item_stack.pop().unwrap();
                            let first = item_stack.pop().unwrap();

                            output.extend(parser_to_instr_set(vec![first], variables, consts));
                            output.extend(parser_to_instr_set(vec![last], variables, consts));

                            let len = consts.len();

                            let x = (len - 2) as u16;
                            let y = (len - 1) as u16;
                            let z = y;
                            handle_ops!(final_stack, x, y, z, op)
                        }
                    } else {
                        item_stack.push(x);
                    }
                }
                output.extend(final_stack);
            }
            _ => todo!("{x:?}"),
        }
    }

    output
}

fn main() {
    dbg!(size_of::<Instr>());
    dbg!(size_of::<Data>());
    dbg!(size_of::<Expr>());

    let now = Instant::now();

    let contents = fs::read_to_string("test.compute").unwrap();

    let parsed = grammar::CodeParser::new().parse(&contents).unwrap();
    println!("{parsed:?}");

    let mut variables: Vec<(String, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let instructions = parser_to_instr_set(parsed.into_vec(), &mut variables, &mut consts);
    println!("INSTR OUT {instructions:?}");
    println!("CONSTS ARE {consts:?}");
    println!("VARS ARE {variables:?}");
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
    println!("CONSTS are {consts:?}");
    println!("EXEC TIME {:.2?}", now.elapsed())
}
