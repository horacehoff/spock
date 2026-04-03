use crate::parser::{Expr, get_id};
use crate::parser_data::ParserData;
use crate::parser_data::Variable;
use crate::{Data, Instr, LibFunc};

pub fn while_loop_summation(
    output: &mut Vec<Instr>,
    v: &mut Vec<Variable>,
    p: &ParserData,
    condition: &Expr,
    code: &[Expr],
) -> bool {
    if code.len() == 1
        && let Expr::VarAssign(x, increment, _, _) = &code[0]
    {
        if let Expr::Inf(a, b, _, _) = condition {
            if let Expr::Var(tgt_var, _, _) = **a {
                if tgt_var == *x {
                    if let Expr::Add(var, increment, _, _) = *increment.clone() {
                        if let Expr::Var(add_var, _, _) = *var {
                            // most simple option
                            if add_var == *x {
                                if *increment == Expr::Float(1.0) || *increment == Expr::Int(1) {
                                    let limit_id = get_id(b, v, p, output, None, false);
                                    let var_id = get_id(a, v, p, output, None, false);
                                    output.push(Instr::Mov(limit_id, var_id));
                                    return true;
                                } else if let Expr::Float(increment_num) = *increment {
                                    // TODO
                                    // let limit_id = get_id(
                                    //     b, v, var_types, registers, output, fns, arrs, fn_state,
                                    //     id, src, instr_src,
                                    // );
                                    // registers.push(Data::Number(increment_num));
                                    // registers.push(Data::NULL);
                                    // output.push(Instr::Mod(
                                    //     limit_id,
                                    //     (registers.len() - 2) as u16,
                                    //     (registers.len() - 1) as u16,
                                    // ));
                                    // registers.push(Data::NULL);
                                    // output.push(Instr::Add(
                                    //     limit_id,
                                    //     (registers.len() - 2) as u16,
                                    //     (registers.len() - 1) as u16,
                                    // ));
                                    // output.push(Instr::Print((registers.len() - 1) as u16));
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
    registers: &mut Vec<Data>,
    v: &mut [Variable],
    array: u16,
    code: &[Expr],
) -> bool {
    if code.len() == 1
        && let Expr::VarAssign(name, value, _, _) = code.first().unwrap()
    {
        if let Expr::Add(l, reps, _, _) = &**value {
            if let Expr::Var(v_name, _, _) = **l
                && &v_name == name
            {
                let var_id = v.iter().find(|x| x.name == *name).unwrap().register_id;
                registers.push(Data::NULL);
                output.push(Instr::CallLibFunc(
                    LibFunc::Len,
                    array,
                    (registers.len() - 1) as u16,
                ));
                registers.push(
                    if let Expr::Int(x) = **reps {
                        x
                    } else {
                        return false;
                    }
                    .into(),
                );
                registers.push(Data::NULL);
                output.push(Instr::MulFloat(
                    (registers.len() - 3) as u16,
                    (registers.len() - 2) as u16,
                    (registers.len() - 1) as u16,
                ));
                output.push(Instr::AddFloat(
                    (registers.len() - 1) as u16,
                    var_id,
                    var_id,
                ));
                return true;
            }
        } else if let Expr::Mul(l, reps, _, _) = &**value
            && let Expr::Var(v_name, _, _) = **l
            && &v_name == name
        {
            let var_id = v.iter().find(|x| x.name == *name).unwrap().register_id;
            registers.push(Data::NULL);
            output.push(Instr::CallLibFunc(
                LibFunc::Len,
                array,
                (registers.len() - 1) as u16,
            ));
            registers.push(
                if let Expr::Int(x) = **reps {
                    x
                } else {
                    return false;
                }
                .into(),
            );
            registers.push(Data::NULL);
            output.push(Instr::PowFloat(
                (registers.len() - 2) as u16,
                (registers.len() - 3) as u16,
                (registers.len() - 1) as u16,
            ));
            output.push(Instr::MulFloat(
                (registers.len() - 1) as u16,
                var_id,
                var_id,
            ));
            return true;
        }
    }
    false
}
