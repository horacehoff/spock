use crate::parser::Expr;
use crate::{Data, Instr};
use internment::Intern;

pub fn while_loop_summation(
    output: &mut Vec<Instr>,
    consts: &mut Vec<Data>,
    v: &mut [(Intern<String>, u16)],
    condition: &Expr,
    code: &[Expr],
) -> bool {
    if code.len() == 1 {
        if let Expr::VarAssign(x, increment, _, _) = &code[0] {
            if let Expr::Inf(a, b, _, _) = condition {
                if let Expr::Var(tgt_var, _, _) = **a {
                    if tgt_var == *x {
                        if let Expr::Num(num_limit) = **b {
                        } else if let Expr::Var(var_limit, _, _) = **b {
                        }
                    }
                }
            }
        }
    }

    false
}

//
pub fn for_loop_summation(
    output: &mut Vec<Instr>,
    consts: &mut Vec<Data>,
    v: &mut [(Intern<String>, u16)],
    array: u16,
    code: &[Expr],
) -> bool {
    if code.len() == 1 {
        if let Expr::VarAssign(name, value, _, _) = code.first().unwrap() {
            if let Expr::Add(l, reps, _, _) = &**value {
                if let Expr::Var(v_name, _, _) = **l {
                    if &v_name == name {
                        let var_id = v.iter().find(|(w, _)| w == name).unwrap().1;
                        consts.push(Data::Null);
                        output.push(Instr::Len(array, (consts.len() - 1) as u16));
                        consts.push(Data::Number(if let Expr::Num(x) = **reps {
                            x
                        } else {
                            unreachable!()
                        }));
                        consts.push(Data::Null);
                        output.push(Instr::Mul(
                            (consts.len() - 3) as u16,
                            (consts.len() - 2) as u16,
                            (consts.len() - 1) as u16,
                        ));
                        output.push(Instr::Add((consts.len() - 1) as u16, var_id, var_id));
                        return true;
                    }
                }
            } else if let Expr::Mul(l, reps, _, _) = &**value {
                if let Expr::Var(v_name, _, _) = **l {
                    if &v_name == name {
                        let var_id = v.iter().find(|(w, _)| w == name).unwrap().1;
                        consts.push(Data::Null);
                        output.push(Instr::Len(array, (consts.len() - 1) as u16));
                        consts.push(Data::Number(if let Expr::Num(x) = **reps {
                            x
                        } else {
                            unreachable!()
                        }));
                        consts.push(Data::Null);
                        output.push(Instr::Pow(
                            (consts.len() - 2) as u16,
                            (consts.len() - 3) as u16,
                            (consts.len() - 1) as u16,
                        ));
                        output.push(Instr::Mul((consts.len() - 1) as u16, var_id, var_id));
                        return true;
                    }
                }
            }
        }
    }
    false
}
