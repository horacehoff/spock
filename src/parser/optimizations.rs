use crate::parser::{Expr, ParserData, get_id};
use crate::{Data, Instr};
use internment::Intern;

pub fn while_loop_summation(
    output: &mut Vec<Instr>,
    v: &mut Vec<(Intern<String>, u16)>,
    (
        var_types,
        registers,
        fns,
        arrays,
        block_id,
        src,
        instr_src,
        is_parsing_recursive,
        fn_registers,
        parsing_fn_id,
    ): ParserData,
    condition: &Expr,
    code: &[Expr],
) -> bool {
    macro_rules! parser_data {
        () => {
            (
                var_types,
                registers,
                fns,
                arrays,
                block_id,
                src,
                instr_src,
                is_parsing_recursive,
                fn_registers,
                parsing_fn_id,
            )
        };
    }

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
                                        let limit_id = get_id(b, v, parser_data!(), output);
                                        let var_id = get_id(a, v, parser_data!(), output);
                                        output.push(Instr::Mov(limit_id, var_id));
                                        return true;
                                    } else if let Expr::Num(increment_num) = *increment {
                                        // TODO
                                        // let limit_id = get_id(
                                        //     b, v, var_types, registers, output, fns, arrs, fn_state,
                                        //     id, src, instr_src,
                                        // );
                                        // registers.push(Data::Number(increment_num));
                                        // registers.push(Data::Null);
                                        // output.push(Instr::Mod(
                                        //     limit_id,
                                        //     (registers.len() - 2) as u16,
                                        //     (registers.len() - 1) as u16,
                                        // ));
                                        // registers.push(Data::Null);
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
    }

    false
}

//
pub fn for_loop_summation(
    output: &mut Vec<Instr>,
    registers: &mut Vec<Data>,
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
                        registers.push(Data::Null);
                        output.push(Instr::Len(array, (registers.len() - 1) as u16));
                        registers.push(Data::Number(if let Expr::Num(x) = **reps {
                            x
                        } else {
                            return false;
                        }));
                        registers.push(Data::Null);
                        output.push(Instr::Mul(
                            (registers.len() - 3) as u16,
                            (registers.len() - 2) as u16,
                            (registers.len() - 1) as u16,
                        ));
                        output.push(Instr::Add((registers.len() - 1) as u16, var_id, var_id));
                        return true;
                    }
                }
            } else if let Expr::Mul(l, reps, _, _) = &**value {
                if let Expr::Var(v_name, _, _) = **l {
                    if &v_name == name {
                        let var_id = v.iter().find(|(w, _)| w == name).unwrap().1;
                        registers.push(Data::Null);
                        output.push(Instr::Len(array, (registers.len() - 1) as u16));
                        registers.push(Data::Number(if let Expr::Num(x) = **reps {
                            x
                        } else {
                            return false;
                        }));
                        registers.push(Data::Null);
                        output.push(Instr::Pow(
                            (registers.len() - 2) as u16,
                            (registers.len() - 3) as u16,
                            (registers.len() - 1) as u16,
                        ));
                        output.push(Instr::Mul((registers.len() - 1) as u16, var_id, var_id));
                        return true;
                    }
                }
            }
        }
    }
    false
}
