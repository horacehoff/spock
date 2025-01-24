use concat_string::concat_string;
use internment::Intern;

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
    Data(u16),

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
    Eq(u16, u16, u16),
    NotEq(u16, u16, u16),
    Sup(u16, u16, u16),
    SupEq(u16, u16, u16),
    Inf(u16, u16, u16),
    InfEq(u16, u16, u16),
}

fn main() {
    dbg!(std::mem::size_of::<Instr>());
    let instructions: Vec<Instr> = vec![
        Instr::Mov(1, 3),
        Instr::Print(3),
        // Instr::Print(2),
    ];
    let mut consts: Vec<Data> = vec![
        Data::Number(10.0),
        Data::Number(20.0),
        Data::String(Intern::from_ref("hello")),
        Data::String(Intern::from_ref("world")),
    ];

    let len = instructions.len();
    let mut i: usize = 0;
    while i < len {
        let instruction = instructions[i];
        match instruction {
            Instr::Jmp(size, is_neg) => {
                if is_neg {
                    i -= size as usize;
                } else {
                    i += size as usize;
                }
            }
            Instr::Mov(tgt, dest) => {
                consts[dest as usize] = consts[tgt as usize];
            }
            Instr::Cmp(cond_id, size) => {
                let condition = consts[cond_id as usize];
                if let Data::Bool(false) = condition {
                    i += size as usize;
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
            Instr::Print(target) => {
                let elem = consts[target as usize];
                println!("PRINT => {elem:?}");
            }
            _ => todo!(""),
        }
        i += 1;
    }
}
