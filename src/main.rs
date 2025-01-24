#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Const {
    Number(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Instr {
    Null,
    Print(u16),
    Data(u16),

    // LOGIC
    Jmp(u16),
    // condition id -- size
    Cmp(u16, u16),

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
    let instructions: Vec<Instr> = vec![Instr::Inf(1, 0, 1), Instr::Cmp(1, 1), Instr::Print(1)];
    let mut consts: Vec<Const> = vec![Const::Number(10.0), Const::Number(20.0)];

    let len = instructions.len();
    let mut i: usize = 0;
    while i < len {
        let instruction = instructions[i];
        match instruction {
            Instr::Jmp(size) => {
                i += size as usize;
            }
            Instr::Cmp(cond_id, size) => {
                let condition = consts[cond_id as usize];
                if let Const::Bool(false) = condition {
                    i += size as usize;
                }
            }
            Instr::Add(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent + child;
                        consts[dest as usize] = Const::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Mul(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent * child;
                        consts[dest as usize] = Const::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Div(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent / child;
                        consts[dest as usize] = Const::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Sub(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent - child;
                        consts[dest as usize] = Const::Number(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Eq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent == child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    (Const::Bool(parent), Const::Bool(child)) => {
                        let result = parent == child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::NotEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent != child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    (Const::Bool(parent), Const::Bool(child)) => {
                        let result = parent != child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Sup(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent > child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::SupEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent >= child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Inf(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent < child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::InfEq(o1, o2, dest) => {
                let first_elem = consts[o1 as usize];
                let second_elem = consts[o2 as usize];

                match (first_elem, second_elem) {
                    (Const::Number(parent), Const::Number(child)) => {
                        let result = parent <= child;
                        consts[dest as usize] = Const::Bool(result);
                    }
                    _ => todo!(""),
                }
            }
            Instr::Print(target) => {
                let elem = consts[target as usize];
                println!("PRINT => {elem:?}")
            }
            _ => todo!(""),
        }
        i += 1;
    }
}
