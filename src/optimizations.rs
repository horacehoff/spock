use crate::parser::Expr;
use crate::{Data, Instr, Num, is_float};
use fnv::FnvHashMap;
use internment::Intern;

pub fn while_loop_summation(
    output: &mut Vec<Instr>,
    consts: &mut Vec<Data>,
    v: &mut [(Intern<String>, u16)],
    x: &Expr,
    y: &[Expr],
) -> bool {
    // if let Expr::Op(items) = x {
    //     if items.len() == 3 {
    //         let op = items.get(1).unwrap();
    //         let dest = items.get(2).unwrap();

    //         if let Expr::Num(fac) = *dest {
    //             if op == &Expr::Opcode(Opcode::Inf) {
    //                 if let Expr::Var(var_name) = items.first().unwrap() {
    //                     if let Expr::VarAssign(name, x) = y.first().unwrap() {
    //                         if *name == *var_name {
    //                             if let Expr::Op(second_items) = &**x {
    //                                 if items.len() == 3 {
    //                                     let op = second_items.get(1).unwrap();
    //                                     let dest_reps = second_items.get(2).unwrap();
    //                                     if op == &Expr::Opcode(Opcode::Add) {
    //                                         if second_items.first().unwrap() == &Expr::Var(*name) {
    //                                             let var_id =
    //                                                 v.iter().find(|(w, _)| *w == *name).unwrap().1;
    //                                             if let Expr::Num(reps) = *dest_reps {
    //                                                 consts.push(Data::Number(fac));
    //                                                 consts.push(Data::Null);
    //                                                 output.push(Instr::Sub(
    //                                                     (consts.len() - 2) as u16,
    //                                                     var_id,
    //                                                     (consts.len() - 1) as u16,
    //                                                 ));
    //                                                 consts.push(Data::Number(reps));
    //                                                 consts.push(Data::Null);
    //                                                 output.push(Instr::Div(
    //                                                     (consts.len() - 3) as u16,
    //                                                     (consts.len() - 2) as u16,
    //                                                     (consts.len() - 1) as u16,
    //                                                 ));
    //                                                 output.push(Instr::Floor(
    //                                                     (consts.len() - 1) as u16,
    //                                                     var_id,
    //                                                 ));
    //                                                 return true;
    //                                             }
    //                                         }
    //                                     }
    //                                 }
    //                             }
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }
    false
}

//
pub fn for_loop_summation(
    output: &mut Vec<Instr>,
    consts: &mut Vec<Data>,
    v: &mut [(Intern<String>, u16)],
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
    array: u16,
    code: &[Expr],
) -> bool {
    // if code.len() == 1 {
    //     if let Expr::VarAssign(name, x) = code.first().unwrap() {
    //         if let Expr::Op(items) = &**x {
    //             if items.len() == 3 {
    //                 if &Expr::Var(*name) == items.first().unwrap() {
    //                     let var_id = v.iter().find(|(w, _)| w == name).unwrap().1;
    //                     if let Expr::Num(reps) = items.last().unwrap() {
    //                         let op = if let Expr::Opcode(x) = items[1] {
    //                             x
    //                         } else {
    //                             unreachable!()
    //                         };

    //                         if let Data::Array(id) = consts[array as usize] {
    //                             let len = arrs[&id].len();
    //                             if op == Opcode::Add {
    //                                 consts.push(Data::Number(reps * len as Num));
    //                             } else if op == Opcode::Mul {
    //                                 consts.push(Data::Number(is_float!(
    //                                     reps.powi(len as i32),
    //                                     reps.pow(len as u32)
    //                                 )));
    //                             }
    //                             output.push(Instr::Mov((consts.len() - 1) as u16, var_id));
    //                         } else {
    //                             consts.push(Data::Null);
    //                             output.push(Instr::ApplyFunc(2, array, (consts.len() - 1) as u16));
    //                             consts.push(Data::Number(*reps));
    //                             consts.push(Data::Null);
    //                             if op == Opcode::Add {
    //                                 output.push(Instr::Mul(
    //                                     (consts.len() - 3) as u16,
    //                                     (consts.len() - 2) as u16,
    //                                     (consts.len() - 1) as u16,
    //                                 ));
    //                             } else if op == Opcode::Mul {
    //                                 output.push(Instr::Pow(
    //                                     (consts.len() - 2) as u16,
    //                                     (consts.len() - 3) as u16,
    //                                     (consts.len() - 1) as u16,
    //                                 ));
    //                             }
    //                             output.push(Instr::Mov((consts.len() - 1) as u16, var_id));
    //                         }
    //                         return true;
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }
    false
}
