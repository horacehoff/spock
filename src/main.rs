use concat_string::concat_string;
use internment::Intern;
use lalrpop_util::lalrpop_mod;
use std::fs;
use std::time::Instant;

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Data {
    Number(f64),
    Bool(bool),
    String(Intern<String>),
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
                    _ => todo!(""),
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
                println!("{elem:?}");
            }
            _ => todo!(""),
        }
        i += 1;
    }
}

#[derive(Debug)]
pub enum Expr {
    Num(f64),
    Bool(bool),
    Op(Box<Expr>, Box<[(Opcode, Box<Expr>)]>),
    Opcode(Opcode),
    Priority(Box<Expr>),
    String(String),
    Var(String),
    Group(Box<[Expr]>),
    VarDeclare(String, Box<Expr>),
    VarAssign(String, Box<Expr>),
    // condition - code -- else_if_blocks(condition array) - else_block
    Condition(Box<Expr>, Box<[Expr]>, Box<[Expr]>, Option<Box<[Expr]>>),
    ElseIfBlock(Box<Expr>, Box<[Expr]>),
    WhileBlock(Box<Expr>, Box<[Expr]>),
    FunctionCall(String, Box<[Expr]>),
}

#[derive(Debug)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
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

fn parser_to_instr_set(
    input: Vec<Expr>,
    variables: &mut Vec<(String, u16)>,
    consts: &mut Vec<Data>,
) -> (Vec<Instr>, bool) {
    // very bad
    let mut output: Vec<Instr> = Vec::new();
    let mut assigned_directly: bool = false;
    for x in input {
        match x {
            Expr::VarDeclare(name, value) => {
                let val = parser_to_instr_set(vec![*value], variables, consts).0;
                output.extend(val);
                variables.push((name.to_string(), (consts.len() - 1) as u16));
            }
            Expr::VarAssign(ref name, value) => {
                let var = variables.iter().find(|(x, _)| x == name).unwrap().clone();
                let (val, assigned) = parser_to_instr_set(vec![*value], variables, consts);
                output.extend(val);
                if !assigned {
                    output.push(Instr::Mov((consts.len() - 1) as u16, var.1));
                }
            }
            Expr::Num(num) => consts.push(Data::Number(num)),
            Expr::Bool(bool) => consts.push(Data::Bool(bool)),
            Expr::String(str) => consts.push(Data::String(Intern::from(str))),
            Expr::Op(left, right) => {
                assigned_directly = true;
            }
            _ => todo!("{x:?}"),
        }
    }

    (output, assigned_directly)
}

fn main() {
    dbg!(size_of::<Instr>());
    dbg!(size_of::<Data>());
    dbg!(size_of::<Expr>());

    let now = Instant::now();

    let contents = fs::read_to_string("test.compute").unwrap();

    let parsed = grammar::CodeParser::new().parse(&contents).unwrap();
    println!("{parsed:?}");
    println!("Parsed in {:.2?}", now.elapsed());

    let mut variables: Vec<(String, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let instructions = parser_to_instr_set(parsed.into_vec(), &mut variables, &mut consts).0;
    println!("INSTR OUT {instructions:?}");
    println!("CONSTS ARE {consts:?}");

    let now = Instant::now();
    let instructions: Vec<Instr> = vec![
        Instr::Inf(0, 3, 6),
        Instr::Cmp(6, 7),
        Instr::Add(0, 5, 0),
        Instr::Mul(1, 2, 1),
        Instr::Sup(1, 4, 7),
        Instr::Cmp(7, 1),
        Instr::Mod(1, 4, 1),
        Instr::Jmp(7, true),
        Instr::Print(1),
    ];
    let mut consts: Vec<Data> = vec![
        // count
        Data::Number(0.0),
        // result
        Data::Number(1.0),
        // nums
        Data::Number(2.0),       // -> 2
        Data::Number(1000000.0), // -> 3
        Data::Number(1000000.0), // -> 4
        Data::Number(1.0),       // -> 5
        Data::Bool(false),       // -> 6
        Data::Bool(false),       // -> 7
    ];
    // execute(&instructions, &mut consts);
    println!("EXEC TIME {:.2?}", now.elapsed())
}
