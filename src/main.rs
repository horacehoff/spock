#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Const {
    Number(f64),
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Instr {
    Null,
    Add(u32, u32, u32),
    Mul(u32, u32, u32),
    Sub(u32, u32, u32),
    Div(u32, u32, u32),
    Print(u32),
    Data(u32),
}

fn main() {
    let instructions: Vec<Instr> = vec![Instr::Add(0, 1, 1), Instr::Mul(1, 0, 1), Instr::Print(1)];
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
