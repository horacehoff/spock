use crate::display::print_instructions;
// use crate::optimizations::{for_loop_summation, while_loop_summation};
use crate::{Data, Instr, Opcode, error, num};
use crate::{check_args, check_args_range, print};
use fnv::FnvHashMap;
use inline_colorization::*;
use internment::Intern;
use lalrpop_util::lalrpop_mod;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(f64),
    Bool(bool),
    Op(Box<[Expr]>),
    Opcode(Opcode),
    Priority(Box<Expr>),
    String(String),
    Var(String),
    Array(Box<[Expr]>),
    VarDeclare(String, Box<Expr>),
    VarAssign(String, Box<Expr>),
    // condition - code -- else_if_blocks(condition array) - else_block
    Condition(Box<Expr>, Box<[Expr]>, Box<[Expr]>, Option<Box<[Expr]>>),
    ElseIfBlock(Box<Expr>, Box<[Expr]>),
    WhileBlock(Box<Expr>, Box<[Expr]>),
    // name - args - optional namespace
    FunctionCall(String, Box<[Expr]>, Box<[String]>),
    ObjFunctionCall(Box<Expr>, Box<[(String, Box<[Expr]>)]>),
    LPAREN,
    RPAREN,

    FunctionDecl(String, Box<[String]>, Box<[Expr]>),

    ReturnVal(Box<Option<Expr>>),

    GetIndex(Box<Expr>, Box<[Expr]>),
    ArrayModify(Box<Expr>, Box<[Expr]>, Box<Expr>),

    ForLoop(String, Box<Expr>, Box<[Expr]>),
    Import(String),
}

lalrpop_mod!(pub grammar);

fn get_precedence(operator: &Expr) -> u8 {
    if let Expr::Opcode(op) = operator {
        match op {
            Opcode::BoolOr => 1,
            Opcode::BoolAnd => 2,
            Opcode::Eq | Opcode::NotEq => 3,
            Opcode::Inf | Opcode::InfEq | Opcode::Sup | Opcode::SupEq => 4,
            Opcode::Add | Opcode::Sub | Opcode::Neg => 5,
            Opcode::Mul | Opcode::Div | Opcode::Mod => 6,
            Opcode::Pow => 7,
        }
    } else {
        unreachable!()
    }
}

fn is_left_associative(operator: &Expr) -> bool {
    if let Expr::Opcode(op) = operator {
        !matches!(op, Opcode::Pow)
    } else {
        unreachable!()
    }
}

pub fn op_to_rpn(operation_input: Vec<Expr>) -> Vec<Expr> {
    let mut return_vector: Vec<Expr> = Vec::new();
    let mut op_stack: Vec<Expr> = Vec::new();
    for x in operation_input {
        // num, function,...
        if !matches!(x, Expr::Opcode(_) | Expr::LPAREN | Expr::RPAREN) {
            return_vector.push(x);
        } else if matches!(x, Expr::Opcode(_)) && x != Expr::LPAREN && x != Expr::RPAREN {
            // operator
            while !op_stack.is_empty()
                && op_stack.last().unwrap() != &Expr::LPAREN
                && (get_precedence(op_stack.last().unwrap()) > get_precedence(&x)
                    || (get_precedence(op_stack.last().unwrap()) == get_precedence(&x)
                        && is_left_associative(&x)))
            {
                return_vector.push(op_stack.pop().unwrap());
            }
            op_stack.push(x);
        } else if x == Expr::LPAREN {
            op_stack.push(x);
        } else if x == Expr::RPAREN {
            while op_stack.last().unwrap() != &Expr::LPAREN {
                assert!(!op_stack.is_empty(), "MISMATCHED PARENTHESES");
                return_vector.push(op_stack.pop().unwrap());
            }
            op_stack.pop();
        }
    }
    while !op_stack.is_empty() {
        assert_ne!(
            op_stack.last().unwrap(),
            &Expr::LPAREN,
            "MISMATCHED PARENTHESES"
        );
        return_vector.push(op_stack.pop().unwrap());
    }

    return_vector
}

fn get_tgt_id(x: Instr) -> u16 {
    match x {
        Instr::Mov(_, y)
        | Instr::Add(_, _, y)
        | Instr::Mul(_, _, y)
        | Instr::Sub(_, _, y)
        | Instr::Div(_, _, y)
        | Instr::Mod(_, _, y)
        | Instr::Pow(_, _, y)
        | Instr::Eq(_, _, y)
        | Instr::NotEq(_, _, y)
        | Instr::Sup(_, _, y)
        | Instr::SupEq(_, _, y)
        | Instr::Inf(_, _, y)
        | Instr::InfEq(_, _, y)
        | Instr::BoolAnd(_, _, y)
        | Instr::BoolOr(_, _, y)
        | Instr::Neg(_, y)
        | Instr::Num(_, y)
        | Instr::Bool(_, y)
        | Instr::ApplyFunc(_, _, y)
        | Instr::Input(_, y)
        | Instr::GetIndex(_, _, y)
        | Instr::Range(_, _, y)
        | Instr::Type(_, y)
        | Instr::IoOpen(_, y, _)
        | Instr::Floor(_, y)
        | Instr::Ret(_, y)
        | Instr::Call(_, y)
        | Instr::TheAnswer(y)
        | Instr::Str(_, y) => y,
        _ => unreachable!("Was unable to match {:?}", x),
    }
}

fn move_to_id(x: &mut [Instr], tgt_id: u16) {
    if x.is_empty() {
        return;
    }
    if matches!(
        x.last().unwrap(),
        Instr::ArrayMov(_, _, _) | Instr::IoDelete(_)
    ) {
        return;
    }
    print!("MOVING TO ID {tgt_id} => {x:?}");
    match x
        .get_mut(
            x.iter()
                .rposition(|w| {
                    matches!(
                        w,
                        Instr::Add(_, _, _)
                            | Instr::Mul(_, _, _)
                            | Instr::Sub(_, _, _)
                            | Instr::Div(_, _, _)
                            | Instr::Mod(_, _, _)
                            | Instr::Pow(_, _, _)
                            | Instr::Eq(_, _, _)
                            | Instr::NotEq(_, _, _)
                            | Instr::Sup(_, _, _)
                            | Instr::SupEq(_, _, _)
                            | Instr::Inf(_, _, _)
                            | Instr::InfEq(_, _, _)
                            | Instr::BoolAnd(_, _, _)
                            | Instr::BoolOr(_, _, _)
                            | Instr::Mov(_, _)
                            | Instr::Neg(_, _)
                            | Instr::Bool(_, _)
                            | Instr::Num(_, _)
                            | Instr::Str(_, _)
                            | Instr::Type(_, _)
                            | Instr::Range(_, _, _)
                            | Instr::IoOpen(_, _, _)
                            | Instr::GetIndex(_, _, _)
                            | Instr::ApplyFunc(_, _, _)
                            | Instr::Floor(_, _)
                            | Instr::Input(_, _)
                            | Instr::Call(_, _)
                            | Instr::Ret(_, _)
                            | Instr::TheAnswer(_)
                    )
                })
                .unwrap_or(x.len() - 1),
        )
        .unwrap()
    {
        Instr::Add(_, _, z)
        | Instr::Mul(_, _, z)
        | Instr::Sub(_, _, z)
        | Instr::Div(_, _, z)
        | Instr::Mod(_, _, z)
        | Instr::Pow(_, _, z)
        | Instr::Eq(_, _, z)
        | Instr::NotEq(_, _, z)
        | Instr::Sup(_, _, z)
        | Instr::SupEq(_, _, z)
        | Instr::Inf(_, _, z)
        | Instr::InfEq(_, _, z)
        | Instr::BoolAnd(_, _, z)
        | Instr::BoolOr(_, _, z)
        | Instr::Mov(_, z)
        | Instr::Neg(_, z)
        | Instr::Type(_, z)
        | Instr::Bool(_, z)
        | Instr::Num(_, z)
        | Instr::ApplyFunc(_, _, z)
        | Instr::Input(_, z)
        | Instr::GetIndex(_, _, z)
        | Instr::Range(_, _, z)
        | Instr::IoOpen(_, z, _)
        | Instr::Floor(_, z)
        | Instr::Ret(_, z)
        | Instr::Call(_, z)
        | Instr::TheAnswer(z)
        | Instr::Str(_, z) => *z = tgt_id,
        _ => unreachable!(),
    }
}

fn get_tgt_id_vec(x: &mut [Instr]) -> u16 {
    if x.is_empty() {
        unreachable!();
    }
    if matches!(
        x.last().unwrap(),
        Instr::ArrayMov(_, _, _) | Instr::IoDelete(_)
    ) {
        unreachable!();
    }
    // print!("MOVING TO ID {tgt_id} => {x:?}");
    match x
        .get_mut(
            x.iter()
                .rposition(|w| {
                    matches!(
                        w,
                        Instr::Add(_, _, _)
                            | Instr::Mul(_, _, _)
                            | Instr::Sub(_, _, _)
                            | Instr::Div(_, _, _)
                            | Instr::Mod(_, _, _)
                            | Instr::Pow(_, _, _)
                            | Instr::Eq(_, _, _)
                            | Instr::NotEq(_, _, _)
                            | Instr::Sup(_, _, _)
                            | Instr::SupEq(_, _, _)
                            | Instr::Inf(_, _, _)
                            | Instr::InfEq(_, _, _)
                            | Instr::BoolAnd(_, _, _)
                            | Instr::BoolOr(_, _, _)
                            | Instr::Mov(_, _)
                            | Instr::Neg(_, _)
                            | Instr::Bool(_, _)
                            | Instr::Num(_, _)
                            | Instr::Str(_, _)
                            | Instr::Type(_, _)
                            | Instr::Range(_, _, _)
                            | Instr::IoOpen(_, _, _)
                            | Instr::GetIndex(_, _, _)
                            | Instr::ApplyFunc(_, _, _)
                            | Instr::Floor(_, _)
                            | Instr::Input(_, _)
                            | Instr::Call(_, _)
                            | Instr::Ret(_, _)
                    )
                })
                .unwrap_or(x.len() - 1),
        )
        .unwrap()
    {
        Instr::Add(_, _, z)
        | Instr::Mul(_, _, z)
        | Instr::Sub(_, _, z)
        | Instr::Div(_, _, z)
        | Instr::Mod(_, _, z)
        | Instr::Pow(_, _, z)
        | Instr::Eq(_, _, z)
        | Instr::NotEq(_, _, z)
        | Instr::Sup(_, _, z)
        | Instr::SupEq(_, _, z)
        | Instr::Inf(_, _, z)
        | Instr::InfEq(_, _, z)
        | Instr::BoolAnd(_, _, z)
        | Instr::BoolOr(_, _, z)
        | Instr::Mov(_, z)
        | Instr::Neg(_, z)
        | Instr::Type(_, z)
        | Instr::Bool(_, z)
        | Instr::Num(_, z)
        | Instr::ApplyFunc(_, _, z)
        | Instr::Input(_, z)
        | Instr::GetIndex(_, _, z)
        | Instr::Range(_, _, z)
        | Instr::IoOpen(_, z, _)
        | Instr::Floor(_, z)
        | Instr::Ret(_, z)
        | Instr::Call(_, z)
        | Instr::Str(_, z) => *z,
        _ => unreachable!(),
    }
}

macro_rules! handle_ops {
    ($final_stack: expr, $x: expr, $y: expr, $z: expr, $op: expr, $consts: expr) => {
        match $op {
            Opcode::Mul => $final_stack.push(Instr::Mul($x, $y, $z)),
            Opcode::Div => $final_stack.push(Instr::Div($x, $y, $z)),
            Opcode::Add => $final_stack.push(Instr::Add($x, $y, $z)),
            Opcode::Sub => $final_stack.push(Instr::Sub($x, $y, $z)),
            Opcode::Mod => $final_stack.push(Instr::Mod($x, $y, $z)),
            Opcode::Pow => $final_stack.push(Instr::Pow($x, $y, $z)),
            Opcode::Eq => $final_stack.push(Instr::Eq($x, $y, $z)),
            Opcode::NotEq => $final_stack.push(Instr::NotEq($x, $y, $z)),
            Opcode::Sup => $final_stack.push(Instr::Sup($x, $y, $z)),
            Opcode::SupEq => $final_stack.push(Instr::SupEq($x, $y, $z)),
            Opcode::Inf => $final_stack.push(Instr::Inf($x, $y, $z)),
            Opcode::InfEq => $final_stack.push(Instr::InfEq($x, $y, $z)),
            Opcode::BoolAnd => $final_stack.push(Instr::BoolAnd($x, $y, $z)),
            Opcode::BoolOr => $final_stack.push(Instr::BoolOr($x, $y, $z)),
            Opcode::Neg => $final_stack.push(Instr::Neg($y, $z)),
        }
    };
}

fn get_id(
    x: Expr,
    variables: &mut Vec<(String, u16)>,
    consts: &mut Vec<Data>,
    instr: &mut Vec<Instr>,
    line: &String,
    functions: &mut Vec<Function>,
    arrays: &mut FnvHashMap<u16, Vec<Data>>,
    fn_state: Option<&FunctionState>,
) -> u16 {
    print!("GETTING ID OF {x:?}");
    match x {
        Expr::Num(num) => {
            consts.push(Data::Number(num as num));
            (consts.len() - 1) as u16
        }
        Expr::String(str) => {
            consts.push(Data::String(Intern::from(str)));
            (consts.len() - 1) as u16
        }
        Expr::Bool(bool) => {
            consts.push(Data::Bool(bool));
            (consts.len() - 1) as u16
        }
        Expr::Var(name) => {
            print!("getting id of var {name:?}");
            print!("{variables:?}");
            if let Some((_, id)) = variables.iter().find(|(var, _)| name == *var) {
                print!("returning id {id:?}");
                *id
            } else {
                error!(
                    line,
                    format_args!("Unknown variable {color_red}{}{color_reset}", name),
                    format_args!("Add 'let {name} = 0;'")
                );
            }
        }
        _ => {
            print!("PARSING FOR ID {:?}", x);
            // println!("vars are {variables:?}");
            // println!("consts are {consts:?}");
            // println!("funcs are {functions:?}");
            // println!("fn_state is {fn_state:?}");
            instr.extend(parser_to_instr_set(
                vec![x],
                variables,
                consts,
                functions,
                fn_state,
                arrays,
            ));
            if instr.is_empty() {
                (consts.len() - 1) as u16
            } else {
                // println!("GET_ID CALLING");
                get_tgt_id_vec(instr)
            }
        }
    }
}

fn expr_to_data(input: &Expr) -> Data {
    match input {
        Expr::Num(num) => Data::Number(*num as num),
        Expr::Bool(bool) => Data::Bool(*bool),
        Expr::String(str) => Data::String(Intern::from(str.to_string())),
        _ => Data::Null,
    }
}

fn can_move(x: Instr) -> bool {
    !matches!(x, Instr::ArrayMov(_, _, _))
}

fn add_cmp(condition_id: u16, len: &mut u16, output: &mut Vec<Instr>, jmp_backwards: bool) {
    match *output.last().unwrap() {
        Instr::Inf(o1, o2, o3) => {
            if o3 == condition_id {
                output.remove(output.len() - 1);
                output.push(Instr::InfCmp(o1, o2, *len));
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::InfEq(o1, o2, o3) => {
            if o3 == condition_id {
                output.remove(output.len() - 1);
                output.push(Instr::InfEqCmp(o1, o2, *len));
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::Sup(o1, o2, o3) => {
            if o3 == condition_id {
                output.remove(output.len() - 1);
                output.push(Instr::SupCmp(o1, o2, *len));
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::SupEq(o1, o2, o3) => {
            if o3 == condition_id {
                output.remove(output.len() - 1);
                output.push(Instr::SupEqCmp(o1, o2, *len));
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::Eq(o1, o2, o3) => {
            if o3 == condition_id {
                output.remove(output.len() - 1);
                output.push(Instr::EqCmp(o1, o2, *len));
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::NotEq(o1, o2, o3) => {
            if o3 == condition_id {
                output.remove(output.len() - 1);
                output.push(Instr::NotEqCmp(o1, o2, *len));
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }

        _ => {
            output.push(Instr::Cmp(condition_id, *len));
        }
    }
}

macro_rules! add_args {
    ($args: expr, $variables: expr, $consts: expr, $output: expr, $ctx: expr, $functions: expr, $arrays: expr, $fn_state: expr) => {
        for arg in $args {
            let arg_id = get_id(
                arg,
                $variables,
                $consts,
                &mut $output,
                &$ctx,
                $functions,
                $arrays,
                $fn_state,
            );
            $output.push(Instr::StoreFuncArg(arg_id));
        }
    };
}

macro_rules! ctx {
    ($x: expr) => {
        x.to_string()
    };
}

type Function = (String, Box<[String]>, Box<[Expr]>);
// last = expecting return ?
type FunctionState = (String, u16, Vec<(String, u16)>, Option<u16>, bool);

fn parser_to_instr_set(
    input: Vec<Expr>,
    // variables
    v: &mut Vec<(String, u16)>,
    // constants
    consts: &mut Vec<Data>,
    // functions
    fns: &mut Vec<Function>,
    fn_state: Option<&FunctionState>,
    // arrays
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::new();
    for x in input {
        print!("PARSING {x}");
        let ctx = x.to_string();
        match x {
            Expr::Num(num) => consts.push(Data::Number(num as num)),
            Expr::Bool(bool) => consts.push(Data::Bool(bool)),
            Expr::String(str) => consts.push(Data::String(Intern::from(str))),
            Expr::Array(elems) => {
                let id = arrs.len() as u16;
                arrs.insert(id, Vec::new());
                for elem in elems {
                    let x = parser_to_instr_set(vec![elem], v, consts, fns, fn_state, arrs);
                    if !x.is_empty() {
                        let c_id = get_tgt_id(*x.last().unwrap());
                        output.extend(x);
                        arrs.get_mut(&id).unwrap().push(Data::Null);
                        output.push(Instr::ArrayMov(c_id, id, (arrs[&id].len() - 1) as u16));
                    } else {
                        arrs.get_mut(&id).unwrap().push(consts.pop().unwrap());
                    }
                }
                consts.push(Data::Array(id));
                print!("ARRAYS {arrs:?}");
            }
            Expr::GetIndex(target, index) => {
                let x = parser_to_instr_set(vec![*target], v, consts, fns, fn_state, arrs);
                output.extend(x);
                let mut id = (consts.len() - 1) as u16;
                for elem in index {
                    let x = parser_to_instr_set(vec![elem], v, consts, fns, fn_state, arrs);
                    output.extend(x);
                    let f_id = (consts.len() - 1) as u16;

                    consts.push(Data::Null);
                    output.push(Instr::GetIndex(id, f_id, (consts.len() - 1) as u16));
                    id = (consts.len() - 1) as u16;
                }
            }
            Expr::Var(name) => {
                consts.push(Data::Null);
                if let Some((_, var_id)) = v.iter().rev().find(|(x, _)| &name == x) {
                    output.push(Instr::Mov(*var_id, (consts.len() - 1) as u16));
                } else {
                    error!(
                        ctx,
                        format_args!("Unknown variable {color_red}{}{color_reset}", name),
                        format_args!("Add 'let {name} = 0;'")
                    );
                }
            }
            Expr::Condition(x, y, z, w) => {
                let mut condition_blocks: Vec<(Vec<Instr>, Vec<Instr>)> = Vec::new();
                let val = *x;
                if matches!(val, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", val));
                }
                let condition = parser_to_instr_set(vec![val], v, consts, fns, fn_state, arrs);
                let mut priv_vars = v.clone();
                let cond_code =
                    parser_to_instr_set(y.into_vec(), &mut priv_vars, consts, fns, fn_state, arrs);

                condition_blocks.push((condition, cond_code));

                for condition in z {
                    if let Expr::ElseIfBlock(condition, code) = condition {
                        let conserved = *condition;
                        if matches!(conserved, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                            error!(ctx, format_args!("{} is not a bool", conserved));
                        }
                        let condition =
                            parser_to_instr_set(vec![conserved], v, consts, fns, fn_state, arrs);
                        let mut priv_vars = v.clone();
                        let cond_code = parser_to_instr_set(
                            code.into_vec(),
                            &mut priv_vars,
                            consts,
                            fns,
                            fn_state,
                            arrs,
                        );
                        condition_blocks.push((condition, cond_code));
                    }
                }
                if let Some(code) = w {
                    let mut priv_vars = v.clone();
                    let cond_code = parser_to_instr_set(
                        code.into_vec(),
                        &mut priv_vars,
                        consts,
                        fns,
                        fn_state,
                        arrs,
                    );
                    condition_blocks.push((Vec::new(), cond_code));
                }

                let jumps: Vec<u16> = (0..condition_blocks.len())
                    .map(|i| {
                        condition_blocks
                            .iter()
                            .skip(i + 1)
                            .map(|x| {
                                if x.0.is_empty() {
                                    (x.1.len() + 1) as u16
                                } else {
                                    (x.0.len() + x.1.len() + 2) as u16
                                }
                            })
                            .sum::<u16>()
                    })
                    .collect();

                for (i, (x, y)) in condition_blocks.into_iter().enumerate() {
                    if x.is_empty() {
                        output.extend(y);
                        break;
                    }
                    output.extend(x);
                    let condition_id = get_tgt_id(*output.last().unwrap());
                    let jump_size = jumps[i];
                    if jump_size == 0 {
                        add_cmp(
                            condition_id,
                            &mut ((y.len() + 1) as u16),
                            &mut output,
                            false,
                        );
                        output.extend(y);
                    } else {
                        add_cmp(
                            condition_id,
                            &mut ((y.len() + 2) as u16),
                            &mut output,
                            false,
                        );
                        output.extend(y);
                        output.push(Instr::Jmp(jump_size, false));
                    }
                }

                print!("{consts:?}");
            }
            Expr::WhileBlock(x, y) => {
                if matches!(*x, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", *x));
                }
                // if while_loop_summation(&mut output, consts, v, *x.clone(), y.clone()) {
                //     continue;
                // }
                let condition = parser_to_instr_set(vec![*x], v, consts, fns, fn_state, arrs);
                output.extend(condition);
                let condition_id = get_tgt_id(*output.last().unwrap());
                let mut temp_vars = v.clone();
                // let consts_before = consts.len() - 1;
                // let temp_vars_before = temp_vars.len() - 1;
                let cond_code =
                    parser_to_instr_set(y.into_vec(), &mut temp_vars, consts, fns, fn_state, arrs);
                // let consts_after = consts.len();
                // let temp_vars_after = temp_vars.len();
                // let modified_temp_vars = (temp_vars_before..temp_vars_after)
                //     .map(|x| temp_vars[x].clone())
                //     .collect::<Vec<_>>();

                let mut len = (cond_code.len() + 2) as u16;
                add_cmp(condition_id, &mut len, &mut output, true);
                output.extend(cond_code);

                output.push(Instr::Jmp(len, true));

                // clean up
                // if consts_before + 1 != consts_after {
                //     consts.push(Data::Number(0.0));
                // }
                // (consts_before..consts_after).for_each(|x| {
                //     if modified_temp_vars.iter().any(|w| w.1 == x as u16) {
                //         output.push(Instr::Mov((consts.len() - 1) as u16, x as u16));
                //     }
                // });
            }
            Expr::ForLoop(var_name, array, code) => {
                let array = get_id(*array, v, consts, &mut output, &ctx, fns, arrs, fn_state);
                // if for_loop_summation(&mut output, consts, v, arrs, array, code.clone()) {
                //     continue;
                // }
                consts.push(Data::Null);
                let array_len_id = (consts.len() - 1) as u16;
                output.push(Instr::ApplyFunc(2, array, array_len_id));

                consts.push(Data::Number(0.0));
                let index_id = (consts.len() - 1) as u16;
                consts.push(Data::Null);
                let condition_id = (consts.len() - 1) as u16;
                output.push(Instr::Inf(index_id, array_len_id, condition_id));

                consts.push(Data::Null);
                let current_element_id = (consts.len() - 1) as u16;
                v.push((var_name, current_element_id));
                let current_element_variable_id = v.len() - 1;
                let mut temp_vars = v.clone();
                // let consts_before = consts.len() - 1;
                let temp_vars_before = temp_vars.len() - 1;
                let cond_code = parser_to_instr_set(
                    code.into_vec(),
                    &mut temp_vars,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                );
                v.remove(current_element_variable_id);
                temp_vars.remove(current_element_variable_id);
                // let consts_after = consts.len();
                let temp_vars_after = temp_vars.len();
                // let modified_temp_vars = (temp_vars_before..temp_vars_after)
                //     .map(|x| temp_vars[x].clone())
                //     .collect::<Vec<_>>();
                let mut len = (cond_code.len() + 4) as u16;
                add_cmp(condition_id, &mut len, &mut output, true);

                output.push(Instr::GetIndex(array, index_id, current_element_id));
                output.extend(cond_code);
                consts.push(Data::Number(1.0));
                output.push(Instr::Add(index_id, (consts.len() - 1) as u16, index_id));
                output.push(Instr::Jmp(len, true));
                consts.push(Data::Number(0.0));

                // clean up
                output.push(Instr::Mov((consts.len() - 1) as u16, index_id));
                // (consts_before..consts_after).for_each(|x| {
                //     if modified_temp_vars.iter().any(|w| w.1 == x as u16) {
                //         output.push(Instr::Mov((consts.len() - 1) as u16, x as u16));
                //     }
                // });
            }
            Expr::VarDeclare(x, y) => {
                let mut val = parser_to_instr_set(vec![*y], v, consts, fns, fn_state, arrs);
                print!("VAL IS {val:?}");
                if val.is_empty() {
                    print!("VAR {x:?} IS EMPTY");
                    v.push((x, (consts.len() - 1) as u16));
                } else {
                    if can_move(*val.last().unwrap()) {
                        consts.push(Data::Null);
                    }
                    move_to_id(&mut val, (consts.len() - 1) as u16);
                    v.push((x, (consts.len() - 1) as u16));
                    output.extend(val);
                }
            }
            Expr::ArrayModify(x, z, w) => {
                let mut id = get_id(*x, v, consts, &mut output, &ctx, fns, arrs, fn_state);

                for elem in z.iter().rev().skip(1).rev() {
                    print!("ELM {elem:?}");
                    let x = parser_to_instr_set(vec![elem.clone()], v, consts, fns, fn_state, arrs);
                    output.extend(x);
                    let f_id = (consts.len() - 1) as u16;

                    consts.push(Data::Null);
                    output.push(Instr::GetIndex(id, f_id, (consts.len() - 1) as u16));
                    id = (consts.len() - 1) as u16;
                }

                let final_id = get_id(
                    z.last().unwrap().clone(),
                    v,
                    consts,
                    &mut output,
                    &ctx,
                    fns,
                    arrs,
                    fn_state,
                );

                print!("ID IS {id:?}");
                print!("CONSTS IS {consts:?}");
                print!("LAST Z IS {}", z.last().unwrap());

                let elem_id = get_id(*w, v, consts, &mut output, &ctx, fns, arrs, fn_state);

                let to_push = Instr::ArrayMod(id, elem_id, final_id);
                output.push(to_push);
            }
            Expr::VarAssign(x, y) => {
                let id = v
                    .iter()
                    .find(|(w, _)| w == &x)
                    .unwrap_or_else(|| {
                        error!(
                            ctx,
                            format_args!("Unknown variable {x}"),
                            format_args!("Add 'let {x} = 0;'")
                        );
                    })
                    .1;

                let mut value = parser_to_instr_set(vec![*y], v, consts, fns, fn_state, arrs);
                move_to_id(&mut value, id);
                if value.is_empty() {
                    output.push(Instr::Mov((consts.len() - 1) as u16, id));
                }
                output.extend(value);
            }
            Expr::FunctionCall(x, args, namespace) => {
                if namespace.is_empty() {
                    match x.as_str() {
                        "print" => {
                            for arg in args {
                                let id =
                                    get_id(arg, v, consts, &mut output, &ctx, fns, arrs, fn_state);
                                output.push(Instr::Print(id));
                            }
                        }
                        "type" => {
                            check_args!(args, 1, "type", ctx);
                            let id = get_id(
                                args[0].clone(),
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Type(id, (consts.len() - 1) as u16));
                        }
                        "num" => {
                            check_args!(args, 1, "num", ctx);
                            let id = get_id(
                                args[0].clone(),
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Num(id, (consts.len() - 1) as u16));
                        }
                        "str" => {
                            check_args!(args, 1, "str", ctx);
                            let id = get_id(
                                args[0].clone(),
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Str(id, (consts.len() - 1) as u16));
                        }
                        "bool" => {
                            check_args!(args, 1, "bool", ctx);
                            let id = get_id(
                                args[0].clone(),
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Bool(id, (consts.len() - 1) as u16));
                        }
                        "input" => {
                            check_args_range!(args, 0, 1, "input", ctx);
                            if !args.is_empty() {
                                let id = get_id(
                                    args[0].clone(),
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                );
                                consts.push(Data::Null);
                                output.push(Instr::Input(id, (consts.len() - 1) as u16));
                            } else {
                                consts.push(Data::String(Intern::from(String::new())));
                                let id = (consts.len() - 1) as u16;
                                consts.push(Data::Null);
                                output.push(Instr::Input(id, (consts.len() - 1) as u16));
                            }
                        }
                        "range" => {
                            check_args_range!(args, 1, 2, "range", ctx);
                            if args.len() == 1 {
                                let id_x = get_id(
                                    args[0].clone(),
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                );
                                consts.push(Data::Number(0.0));
                                consts.push(Data::Null);
                                output.push(Instr::Range(
                                    (consts.len() - 2) as u16,
                                    id_x,
                                    (consts.len() - 1) as u16,
                                ));
                            } else {
                                let id_x = get_id(
                                    args[0].clone(),
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                );
                                let id_y = get_id(
                                    args[1].clone(),
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                );
                                consts.push(Data::Null);
                                output.push(Instr::Range(id_x, id_y, (consts.len() - 1) as u16));
                            }
                        }
                        "floor" => {
                            check_args!(args, 1, "floor", ctx);
                            let id = get_id(
                                args[0].clone(),
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Num(id, (consts.len() - 1) as u16));
                        }
                        "the_answer" => {
                            check_args!(args, 0, "the_answer", ctx);
                            consts.push(Data::Null);
                            output.push(Instr::TheAnswer((consts.len() - 1) as u16));
                        }
                        function => {
                            let found =
                                fns.iter()
                                    .find(|(a, _, _)| a == function)
                                    .unwrap_or_else(|| {
                                        error!(
                                            ctx,
                                            format_args!(
                                                "Unknown function {color_red}{}{color_reset}",
                                                function
                                            )
                                        );
                                    });

                            let args_len = found.1.len();
                            check_args!(args, args_len, function, ctx);

                            if let Some((name, loc, func_args, return_id, in_return)) = fn_state {
                                if name == function {
                                    let mut saves: Vec<(u16, u16)> = Vec::new();
                                    // recursive function, go back to function def and move on
                                    for i in 0..args_len {
                                        let arg = args.get(i).unwrap();
                                        let val = expr_to_data(arg);
                                        consts.push(Data::Null);
                                        output.push(Instr::Mov(
                                            func_args[i].1,
                                            (consts.len() - 1) as u16,
                                        ));
                                        saves.push((func_args[i].1, (consts.len() - 1) as u16));
                                        if val != Data::Null {
                                            consts[func_args[i].1 as usize] = val;
                                        } else {
                                            print!("{arg}");
                                            let mut value = parser_to_instr_set(
                                                vec![arg.clone()],
                                                v,
                                                consts,
                                                fns,
                                                fn_state,
                                                arrs,
                                            );
                                            move_to_id(&mut value, func_args[i].1);
                                            print!("VAL{value:?}");
                                            output.extend(value);
                                        }
                                    }

                                    fn get_all_tgt_id(x: &[Instr]) -> Vec<u16> {
                                        let mut total: Vec<u16> = Vec::new();
                                        for x in x {
                                            if !matches!(x, Instr::Ret(_, _)) {
                                                total.push(get_tgt_id(*x))
                                            }
                                        }
                                        total
                                    }

                                    // doesn't work for now
                                    // let len = output.len();
                                    // let instr_to_backup:Vec<u16> = (0u16..consts.len() as u16).collect();
                                    // let instr_to_backup = get_all_tgt_id(&output[0..len]);
                                    // println!("CONSTS TO BE BAC/KED UP ARE {instr_to_backup:?}");
                                    // let mut instr_backups: Vec<(u16, u16)> = Vec::new();
                                    // let start = consts.len();
                                    // let mut offset = 0;
                                    // for x in instr_to_backup {
                                    //     consts.push(Data::Null);
                                    //     offset = (consts.len() - 1) as u16 - x;
                                    //     // output.push(Instr::Mov(x, (consts.len() - 1) as u16));
                                    //     // instr_backups.push((x, (consts.len() - 1) as u16))
                                    // }
                                    // let end = consts.len();
                                    // output.push(Instr::MovRange(start as u16, end as u16, offset));
                                    // output.push(Instr::TEMP_START_MOV);
                                    consts.push(Data::Null);
                                    let final_tgt_id = (consts.len() - 1) as u16;
                                    output.push(Instr::Call(*loc, final_tgt_id));
                                    let mut i = 0;
                                    // let len = instr_backups.len();
                                    // for (x, y) in instr_backups {
                                    //     output.push(Instr::RestoreCallArg(y, x, i==(len-1)));
                                    //     i += 1;
                                    // }
                                    // output.push(Instr::TEMP_STOP_MOV);
                                    for (x, y) in saves {
                                        output.push(Instr::RestoreCallArg(y, x));
                                    }

                                    continue;
                                }
                            }

                            let mut fn_variables: Vec<(String, u16)> = Vec::new();

                            for (i, x) in found.1.iter().enumerate() {
                                let len = consts.len() as u16;
                                let mut value = parser_to_instr_set(
                                    vec![args[i].clone()],
                                    v,
                                    consts,
                                    &mut fns.clone(),
                                    fn_state,
                                    arrs,
                                );
                                move_to_id(&mut value, len);
                                output.extend(value);
                                fn_variables.push((x.to_string(), len));
                            }
                            let vars = fn_variables.clone();
                            consts.push(Data::Null);
                            let start = consts.len();
                            output.extend(parser_to_instr_set(
                                found.2.to_vec(),
                                &mut fn_variables,
                                consts,
                                fns,
                                Some(&(
                                    function.to_string(),
                                    output.len() as u16,
                                    vars,
                                    Some((consts.len() - 1) as u16),
                                    false,
                                )),
                                arrs,
                            ));
                            let end = consts.len();
                        }
                    }
                } else if *namespace == ["io"] {
                    match x.as_str() {
                        "open" => {
                            check_args_range!(args, 1, 2, "open", ctx);
                            consts.push(Data::Null);
                            let arg_id = get_id(
                                args[0].clone(),
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );

                            let second_arg = if args.len() == 1 {
                                consts.push(Data::Bool(false));
                                (consts.len() - 1) as u16
                            } else {
                                get_id(
                                    args[1].clone(),
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                )
                            };

                            output.push(Instr::IoOpen(
                                arg_id,
                                (consts.len() - 1) as u16,
                                second_arg,
                            ));
                        }
                        "delete" => {
                            check_args!(args, 1, "delete", ctx);
                            let arg_id = get_id(
                                args[0].clone(),
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );
                            output.push(Instr::IoDelete(arg_id));
                        }
                        other => {
                            error!(
                                ctx,
                                format_args!(
                                    "Unknown function {color_red}{}{color_reset} in namespace {color_red}{}{color_reset}",
                                    other,
                                    namespace.join("::")
                                )
                            );
                        }
                    }
                } else {
                    error!(
                        ctx,
                        format_args!(
                            "Unknown namespace {color_red}{}{color_reset}",
                            namespace.join("::")
                        )
                    );
                }
            }
            Expr::ReturnVal(val) => {
                if let Some(x) = fn_state {
                    if let Some(return_value) = *val {
                        if let Some(ret_id) = x.3 {
                            let val = get_id(
                                return_value,
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                            );
                            output.push(Instr::Ret(val, ret_id));
                        }
                    }
                } else {
                    // exit the program
                    output.push(Instr::Jmp(65535, false));
                }
            }
            Expr::ObjFunctionCall(obj, funcs) => {
                let mut id = get_id(*obj, v, consts, &mut output, &ctx, fns, arrs, fn_state);
                for func in funcs {
                    let args = func.1;
                    match func.0.as_str() {
                        "uppercase" => {
                            check_args!(args, 0, "uppercase", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(0, id, f_id));
                            id = f_id;
                        }
                        "lowercase" => {
                            check_args!(args, 0, "lowercase", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(1, id, f_id));
                            id = f_id;
                        }
                        "len" => {
                            check_args!(args, 0, "len", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(2, id, f_id));
                            id = f_id;
                        }
                        "contains" => {
                            check_args!(args, 1, "contains", ctx);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(3, id, f_id));
                            id = f_id;
                        }
                        "trim" => {
                            check_args!(args, 0, "trim", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(4, id, f_id));
                            id = f_id;
                        }
                        "trim_sequence" => {
                            check_args!(args, 1, "trim_sequence", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);
                            output.push(Instr::ApplyFunc(5, id, f_id));
                            id = f_id;
                        }
                        "index" => {
                            check_args!(args, 1, "index", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);
                            output.push(Instr::ApplyFunc(6, id, f_id));
                            id = f_id;
                        }
                        "is_num" => {
                            check_args!(args, 0, "is_num", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(7, id, f_id));
                            id = f_id;
                        }
                        "trim_left" => {
                            check_args!(args, 0, "trim_left", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(8, id, f_id));
                            id = f_id;
                        }
                        "trim_right" => {
                            check_args!(args, 0, "trim_right", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(9, id, f_id));
                            id = f_id;
                        }
                        "trim_sequence_left" => {
                            check_args!(args, 1, "trim_sequence_left", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);
                            output.push(Instr::ApplyFunc(10, id, f_id));
                            id = f_id;
                        }
                        "trim_sequence_right" => {
                            check_args!(args, 1, "trim_sequence_right", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);
                            output.push(Instr::ApplyFunc(11, id, f_id));
                            id = f_id;
                        }
                        "rindex" => {
                            check_args!(args, 1, "rindex", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);
                            output.push(Instr::ApplyFunc(12, id, f_id));
                            id = f_id;
                        }
                        "repeat" => {
                            check_args!(args, 1, "repeat", ctx);
                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(13, id, f_id));
                            id = f_id;
                        }
                        "push" => {
                            check_args!(args, 1, "push", ctx);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(14, id, f_id));
                            id = f_id;
                        }
                        "sqrt" => {
                            check_args!(args, 0, "sqrt", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(15, id, f_id));
                            id = f_id;
                        }
                        "round" => {
                            check_args!(args, 0, "round", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(16, id, f_id));
                            id = f_id;
                        }
                        "abs" => {
                            check_args!(args, 0, "abs", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(17, id, f_id));
                            id = f_id;
                        }
                        // io::read
                        "read" => {
                            check_args!(args, 0, "read", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(18, id, f_id));
                            id = f_id;
                        }
                        // io::write
                        "write" => {
                            check_args_range!(args, 1, 2, "write", ctx);

                            let len = args.len();
                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state);
                            if len == 1 {
                                consts.push(Data::Bool(false));
                                output.push(Instr::StoreFuncArg((consts.len() - 1) as u16));
                            }

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(19, id, f_id));
                            id = f_id;
                        }
                        other => {
                            error!(
                                ctx,
                                format_args!("Unknown function {color_red}{}{color_reset}", other)
                            );
                        }
                    }
                }
            }
            Expr::FunctionDecl(x, y, z) => {
                if fns.iter().any(|(name, _, _)| name == &x) {
                    error!(
                        ctx,
                        format_args!("Function {color_red}{}{color_reset} is already defined", x)
                    );
                }
                fns.push((x, y, z));
            }
            Expr::Op(items) => {
                fn remove_priority(
                    x: Expr,
                    variables: &mut Vec<(String, u16)>,
                    consts: &mut Vec<Data>,
                ) -> Vec<Expr> {
                    match x {
                        Expr::Op(w) => {
                            let mut output = Vec::new();
                            for x in w {
                                output.extend(remove_priority(x, variables, consts));
                            }
                            output
                        }
                        Expr::Priority(x) => {
                            let mut output: Vec<Expr> = Vec::with_capacity(3);
                            output.push(Expr::LPAREN);
                            output.extend(remove_priority(*x, variables, consts));
                            output.push(Expr::RPAREN);
                            output
                        }
                        _ => vec![x],
                    }
                }

                let mut temp_op = Vec::new();
                for x in items {
                    temp_op.extend(remove_priority(x, v, consts));
                }
                let op = op_to_rpn(temp_op);

                let mut item_stack: Vec<Expr> = Vec::with_capacity(4);
                let mut final_stack: Vec<Instr> =
                    Vec::with_capacity(op.iter().filter(|x| matches!(x, Expr::Opcode(_))).count());
                for x in op {
                    if let Expr::Opcode(op) = x {
                        if final_stack.is_empty() {
                            let last = item_stack.pop().unwrap();
                            let first = item_stack.pop().unwrap();

                            let first_v =
                                get_id(first, v, consts, &mut output, &ctx, fns, arrs, fn_state);
                            let second_v =
                                get_id(last, v, consts, &mut output, &ctx, fns, arrs, fn_state);
                            consts.push(Data::Null);
                            let x = first_v;
                            let y = second_v;
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op, consts);
                        } else {
                            let old_id = get_tgt_id(*final_stack.last().unwrap());
                            let new = item_stack.pop().unwrap();
                            let new_v =
                                get_id(new, v, consts, &mut output, &ctx, fns, arrs, fn_state);
                            consts.push(Data::Null);
                            let x = new_v;
                            let y = old_id;
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op, consts);
                        }
                    } else {
                        item_stack.push(x);
                    }
                }
                output.extend(final_stack);
            }
            Expr::Priority(x) => {
                output.extend(parser_to_instr_set(
                    vec![*x],
                    v,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                ));
            }
            other => {
                unreachable!("Not implemented {:?}", other);
            }
        }
    }
    output
}

pub fn parse(contents: &str) -> (Vec<Instr>, Vec<Data>, FnvHashMap<u16, Vec<Data>>) {
    let mut functions: Vec<Expr> = grammar::FileParser::new().parse(contents).unwrap();
    print!("funcs {functions:?}");
    let main_function: Vec<Expr> = {
        if let Some(fctn) = functions.iter().position(|a| {
            if let Expr::FunctionDecl(name, _, _) = a {
                name.trim_end_matches('(') == "main"
            } else {
                false
            }
        }) {
            if let Expr::FunctionDecl(_, _, code) = functions.swap_remove(fctn) {
                code.to_vec()
            } else {
                error!(contents, "No main function");
            }
        } else {
            error!(contents, "No main function");
        }
    };

    let mut functions: Vec<(String, Box<[String]>, Box<[Expr]>)> = functions
        .iter()
        .map(|w| {
            if let Expr::FunctionDecl(x, y, z) = w {
                (x.trim_end_matches('(').to_string(), y.clone(), z.clone())
            } else {
                error!(contents, "Function expected");
            }
        })
        .collect();

    print!("{functions:?}");

    let mut variables: Vec<(String, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let mut arrays: FnvHashMap<u16, Vec<Data>> = FnvHashMap::default();
    let instructions = parser_to_instr_set(
        main_function,
        &mut variables,
        &mut consts,
        &mut functions,
        None,
        &mut arrays,
    );
    print!("CONSTS are {consts:?}");
    print!("{consts:?}");
    print!("{arrays:?}");
    #[cfg(debug_assertions)]
    {
        print_instructions(&instructions);
    }
    (instructions, consts, arrays)
}
