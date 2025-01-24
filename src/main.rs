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
    Print(u32),
    Data(u32),

    // OPS
    Add(u32, u32, u32),
    Mul(u32, u32, u32),
    Sub(u32, u32, u32),
    Div(u32, u32, u32),
    Eq(u32, u32, u32),
    NotEq(u32, u32, u32),
    Sup(u32, u32, u32),
    SupEq(u32, u32, u32),
    Inf(u32, u32, u32),
    InfEq(u32, u32, u32),
}

fn main() {
    let instructions: Vec<Instr> = vec![Instr::Inf(0, 1, 1), Instr::Print(1)];
    let mut consts: Vec<Const> = vec![Const::Number(10.0), Const::Number(20.0)];

    for instruction in instructions {
        match instruction {
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
    }
}
