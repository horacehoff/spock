use crate::parser::{Expr, Function, FunctionState, get_id};
use crate::type_inference::DataType;
use crate::{Data, Instr};
use internment::Intern;
use slab::Slab;

pub fn while_loop_summation(
    output: &mut Vec<Instr>,
    consts: &mut Vec<Data>,
    v: &mut Vec<(Intern<String>, u16)>,
    var_types: &mut Vec<(Intern<String>, DataType)>,
    fns: &mut Vec<Function>,
    arrs: &mut Slab<Vec<Data>>,
    fn_state: Option<&FunctionState>,
    id: u16,
    // (filename, contents)
    src: (&str, &str),
    instr_src: &mut Vec<(Instr, usize, usize)>,
    condition: &Expr,
    code: &[Expr],
) -> bool {
    if code.len() == 1 {
        if let Expr::VarAssign(x, increment, _, _) = &code[0] {
            if let Expr::Inf(a, b, _, _) = condition {
                if let Expr::Var(tgt_var, _, _) = **a {
                    if tgt_var == *x {
                        if let Expr::Add(var, increment, _, _) = *increment.clone() {
                            if let Expr::Var(add_var, _, _) = *var {
                                // most simple option
                                if add_var == *x {
                                    if *increment == Expr::Num(1.0) {
                                        let limit_id = get_id(
                                            b, v, var_types, consts, output, fns, arrs, fn_state,
                                            id, src, instr_src,
                                        );
                                        let var_id = get_id(
                                            a, v, var_types, consts, output, fns, arrs, fn_state,
                                            id, src, instr_src,
                                        );
                                        output.push(Instr::Mov(limit_id, var_id));
                                        return true;
                                    } else if let Expr::Num(increment_num) = *increment {
                                        // TODO
                                    }
                                }
                            }
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
                            return false;
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
                            return false;
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
