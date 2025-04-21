// pub fn while_loop_summation(
//     output: &mut Vec<Instr>,
//     consts: &mut Vec<Data>,
//     v: &mut [(String, u16)],
//     x: Expr,
//     y: Box<[Expr]>,
// ) -> bool {
//     if let Expr::Op(base, tgt) = x {
//         if tgt.len() == 1 {
//             let (op, dest) = tgt.first().unwrap();
//             if let Expr::Num(fac) = **dest {
//                 if op == &Opcode::Inf {
//                     if let Expr::Var(var_name) = *base {
//                         if let Expr::VarAssign(name, x) = y.first().unwrap() {
//                             if name == &var_name {
//                                 if let Expr::Op(base, tgt) = &**x {
//                                     if tgt.len() == 1 {
//                                         let (op, dest_reps) = tgt.first().unwrap();
//                                         if op == &Opcode::Add {
//                                             if **base == Expr::Var(name.to_string()) {
//                                                 let var_id =
//                                                     v.iter().find(|(w, _)| w == name).unwrap().1;
//                                                 if let Expr::Num(reps) = **dest_reps {
//                                                     consts.push(Data::Number(fac));
//                                                     consts.push(Data::Null);
//                                                     output.push(Instr::Sub(
//                                                         (consts.len() - 2) as u16,
//                                                         var_id,
//                                                         (consts.len() - 1) as u16,
//                                                     ));
//                                                     consts.push(Data::Number(reps));
//                                                     consts.push(Data::Null);
//                                                     output.push(Instr::Div(
//                                                         (consts.len() - 3) as u16,
//                                                         (consts.len() - 2) as u16,
//                                                         (consts.len() - 1) as u16,
//                                                     ));
//                                                     output.push(Instr::Floor(
//                                                         (consts.len() - 1) as u16,
//                                                         var_id,
//                                                     ));
//                                                     return true;
//                                                 }
//                                             }
//                                         }
//                                     }
//                                 }
//                             } else {
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//     }
//     false
// }
//
// pub fn for_loop_summation(
//     output: &mut Vec<Instr>,
//     consts: &mut Vec<Data>,
//     v: &mut [(String, u16)],
//     arrs: &mut FnvHashMap<u16, Vec<Data>>,
//     array: u16,
//     code: Box<[Expr]>,
// ) -> bool {
//     if code.len() == 1 {
//         if let Expr::VarAssign(name, x) = code.first().unwrap() {
//             if let Expr::Op(base, tgt) = &**x {
//                 if tgt.len() == 1 {
//                     let (op, dest) = tgt.first().unwrap();
//                     if **base == Expr::Var(name.to_string()) {
//                         let var_id = v.iter().find(|(w, _)| w == name).unwrap().1;
//                         if let Expr::Num(reps) = **dest {
//                             if let Data::Array(id) = consts[array as usize] {
//                                 let len = arrs[&id].len();
//                                 if op == &Opcode::Add {
//                                     consts.push(Data::Number(reps * len as f64));
//                                 } else if op == &Opcode::Mul {
//                                     consts.push(Data::Number(reps.powi(len as i32)));
//                                 }
//                                 output.push(Instr::Mov((consts.len() - 1) as u16, var_id));
//                             } else {
//                                 consts.push(Data::Null);
//                                 output.push(Instr::ApplyFunc(2, array, (consts.len() - 1) as u16));
//                                 consts.push(Data::Number(reps));
//                                 consts.push(Data::Null);
//                                 if op == &Opcode::Add {
//                                     output.push(Instr::Mul(
//                                         (consts.len() - 3) as u16,
//                                         (consts.len() - 2) as u16,
//                                         (consts.len() - 1) as u16,
//                                     ));
//                                 } else if op == &Opcode::Mul {
//                                     output.push(Instr::Pow(
//                                         (consts.len() - 2) as u16,
//                                         (consts.len() - 3) as u16,
//                                         (consts.len() - 1) as u16,
//                                     ));
//                                 }
//                                 output.push(Instr::Mov((consts.len() - 1) as u16, var_id));
//                             }
//                             return true;
//                         }
//                     }
//                 }
//             }
//         }
//     }
//     false
// }
