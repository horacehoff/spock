use crate::display::print_instructions;
use crate::optimizations::{for_loop_summation, while_loop_summation};
use crate::{Data, Instr, Num, Opcode, error, error_b};
use crate::{check_args, check_args_range, print};
use fnv::FnvHashMap;
use inline_colorization::*;
use internment::Intern;
use lalrpop_util::lalrpop_mod;
use std::slice;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum Expr {
    Num(Num),
    Bool(bool),
    Op(Box<[Expr]>),
    Opcode(Opcode),
    Priority(Box<Expr>),
    String(String),
    Var(Intern<String>),
    Array(Box<[Expr]>),
    VarDeclare(Intern<String>, Box<Expr>),
    VarAssign(Intern<String>, Box<Expr>),
    // condition - code (contains else_if_blocks and potentially else_block)
    Condition(Box<Expr>, Box<[Expr]>),
    ElseIfBlock(Box<Expr>, Box<[Expr]>),
    ElseBlock(Box<[Expr]>),

    WhileBlock(Box<Expr>, Box<[Expr]>),
    // args - (optional namespace + name)
    FunctionCall(Box<[Expr]>, Box<[String]>),
    ObjFunctionCall(Box<Expr>, Box<[(String, Box<[Expr]>)]>),
    LPAREN,
    RPAREN,

    // name+args -- code
    FunctionDecl(Box<[String]>, Box<[Expr]>),

    ReturnVal(Box<Option<Expr>>),

    GetIndex(Box<Expr>, Box<[Expr]>),
    ArrayModify(Box<Expr>, Box<[Expr]>, Box<Expr>),

    // id name -- array as first + code
    // ForLoop(String, Box<Expr>, Box<[Expr]>),
    ForLoop(Intern<String>, Box<[Expr]>),
    Import(String),
    // Closure(Box<[String]>, Box<[Expr]>),
    Break,
    Continue,

    EvalBlock(Box<[Expr]>),
    LoopBlock(Box<[Expr]>),
}

lalrpop_mod!(pub grammar);

fn move_to_id(x: &mut [Instr], tgt_id: u16) {
    if x.is_empty()
        || matches!(
            x.last().unwrap(),
            Instr::ArrayMov(_, _, _) | Instr::IoDelete(_)
        )
    {
        return;
    }
    match x
        .get_mut(
            x.iter()
                .rposition(|w| get_tgt_id(*w).is_some())
                .unwrap_or(x.len() - 1),
        )
        .unwrap()
    {
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
        | Instr::Len(_, y)
        | Instr::Sqrt(_, y)
        | Instr::Split(_, _, y)
        | Instr::Str(_, y) => *y = tgt_id,
        _ => unreachable!(),
    }
}

fn get_tgt_id(x: Instr) -> Option<u16> {
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
        | Instr::Len(_, y)
        | Instr::Sqrt(_, y)
        | Instr::Split(_, _, y)
        | Instr::Str(_, y) => Some(y),
        _ => None,
    }
}

fn get_tgt_id_vec(x: &[Instr]) -> u16 {
    debug_assert!(
        !(x.is_empty()
            || matches!(
                x.last().unwrap(),
                Instr::ArrayMov(_, _, _) | Instr::IoDelete(_)
            ))
    );
    for y in x.iter().rev() {
        if let Some(id) = get_tgt_id(*y) {
            return id;
        }
    }
    unreachable!();
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
    x: &Expr,
    v: &mut Vec<(Intern<String>, u16)>,
    consts: &mut Vec<Data>,
    output: &mut Vec<Instr>,
    ctx: &str,
    fns: &mut Vec<Function>,
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
    fn_state: Option<&FunctionState>,
    id: u16,
) -> u16 {
    match x {
        Expr::Num(num) => {
            consts.push(Data::Number(*num as Num));
            (consts.len() - 1) as u16
        }
        Expr::String(str) => {
            consts.push(Data::String(Intern::from(str.to_string())));
            (consts.len() - 1) as u16
        }
        Expr::Bool(bool) => {
            consts.push(Data::Bool(*bool));
            (consts.len() - 1) as u16
        }
        Expr::Var(name) => {
            if let Some((_, id)) = v.iter().find(|(var, _)| *name == *var) {
                *id
            } else {
                error!(
                    ctx,
                    format_args!("Unknown variable {color_red}{}{color_reset}", name),
                    format_args!("Add 'let {name} = 0;'")
                );
            }
        }
        Expr::Array(elems) => {
            let id = arrs.len() as u16;
            arrs.insert(id, Vec::new());
            for elem in elems {
                let x =
                    parser_to_instr_set(slice::from_ref(elem), v, consts, fns, fn_state, arrs, id);
                if !x.is_empty() {
                    let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                    arrs.get_mut(&id).unwrap().push(Data::Null);

                    output.extend(x);
                    output.push(Instr::ArrayMov(c_id, id, (arrs[&id].len() - 1) as u16));
                } else {
                    arrs.get_mut(&id).unwrap().push(consts.pop().unwrap());
                }
            }
            consts.push(Data::Array(id));
            (consts.len() - 1) as u16
        }
        Expr::Condition(main_condition, code) => {
            let return_id = consts.len() as u16;
            consts.push(Data::Null);

            // check if condition is not var/str/num (it's supposed to be a bool)
            if matches!(**main_condition, Expr::String(_) | Expr::Num(_)) {
                error!(ctx, format_args!("{} is not a bool", main_condition));
            }

            // get first code limit (after which there are only else(if) blocks)
            let main_code_limit = code
                .iter()
                .position(|x| matches!(x, Expr::ElseIfBlock(_, _) | Expr::ElseBlock(_)))
                .unwrap_or(code.len());

            let condition_blocks_count = code.len() - main_code_limit;
            let mut cmp_markers: Vec<usize> = Vec::with_capacity(condition_blocks_count);
            let mut jmp_markers: Vec<usize> = Vec::with_capacity(condition_blocks_count);
            let mut condition_markers: Vec<usize> = Vec::with_capacity(condition_blocks_count);

            // parse the main condition
            let condition_id = get_id(
                main_condition,
                v,
                consts,
                output,
                ctx,
                fns,
                arrs,
                fn_state,
                id,
            );
            add_cmp(condition_id, &mut 0, output, false);
            cmp_markers.push(output.len() - 1);
            let mut priv_vars = v.clone();
            // parse the main code block
            let cond_code = parser_to_instr_set(
                &code[0..main_code_limit],
                &mut priv_vars,
                consts,
                fns,
                fn_state,
                arrs,
                id,
            );
            let is_empty = cond_code.is_empty();
            output.extend(cond_code);
            output.push(Instr::Mov(
                if is_empty {
                    (consts.len() - 1) as u16
                } else {
                    get_tgt_id_vec(output)
                },
                return_id,
            ));
            if main_code_limit != code.len() {
                output.push(Instr::Jmp(0, false));
                jmp_markers.push(output.len() - 1);
            }

            for elem in &code[main_code_limit..] {
                if let Expr::ElseIfBlock(condition, code) = elem {
                    if matches!(**condition, Expr::String(_) | Expr::Num(_)) {
                        error!(ctx, format_args!("{} is not a bool", main_condition));
                    }
                    condition_markers.push(output.len());
                    let condition_id =
                        get_id(condition, v, consts, output, ctx, fns, arrs, fn_state, id);
                    add_cmp(condition_id, &mut 0, output, false);
                    cmp_markers.push(output.len() - 1);
                    let mut priv_vars = v.clone();
                    let cond_code =
                        parser_to_instr_set(code, &mut priv_vars, consts, fns, fn_state, arrs, id);
                    let is_empty = cond_code.is_empty();
                    output.extend(cond_code);
                    output.push(Instr::Mov(
                        if is_empty {
                            (consts.len() - 1) as u16
                        } else {
                            get_tgt_id_vec(output)
                        },
                        return_id,
                    ));
                    output.push(Instr::Jmp(0, false));
                    jmp_markers.push(output.len() - 1);
                } else if let Expr::ElseBlock(code) = elem {
                    condition_markers.push(output.len());
                    let mut priv_vars = v.clone();
                    let cond_code =
                        parser_to_instr_set(code, &mut priv_vars, consts, fns, fn_state, arrs, id);
                    let is_empty = cond_code.is_empty();
                    output.extend(cond_code);
                    output.push(Instr::Mov(
                        if is_empty {
                            (consts.len() - 1) as u16
                        } else {
                            get_tgt_id_vec(output)
                        },
                        return_id,
                    ));
                }
            }

            for y in jmp_markers {
                let diff = output.len() - y;
                output[y] = Instr::Jmp(diff as u16, false);
            }
            for (i, y) in cmp_markers.iter().enumerate() {
                let diff = if i >= condition_markers.len() {
                    output.len() - 1 - y
                } else {
                    condition_markers[i] - y
                };
                if let Some(
                    Instr::Cmp(_, jump_size)
                    | Instr::InfCmp(_, _, jump_size)
                    | Instr::InfEqCmp(_, _, jump_size)
                    | Instr::SupCmp(_, _, jump_size)
                    | Instr::SupEqCmp(_, _, jump_size)
                    | Instr::EqCmp(_, _, jump_size)
                    | Instr::NotEqCmp(_, _, jump_size),
                ) = output.get_mut(*y)
                {
                    *jump_size = diff as u16;
                }
            }
            return_id
        }
        Expr::Priority(contents) => {
            get_id(contents, v, consts, output, ctx, fns, arrs, fn_state, id)
        }
        other => {
            let output_code =
                parser_to_instr_set(slice::from_ref(other), v, consts, fns, fn_state, arrs, id);
            if !output_code.is_empty() {
                output.extend(output_code);
                get_tgt_id_vec(output)
            } else {
                (consts.len() - 1) as u16
            }
        }
    }
}

#[inline(always)]
fn expr_to_data(input: &Expr) -> Data {
    match input {
        Expr::Num(num) => Data::Number(*num as Num),
        Expr::Bool(bool) => Data::Bool(*bool),
        Expr::String(str) => Data::String(Intern::from(str.to_string())),
        _ => Data::Null,
    }
}

#[inline(always)]
fn can_move(x: &Instr) -> bool {
    !matches!(x, Instr::ArrayMov(_, _, _))
}

fn add_cmp(condition_id: u16, len: &mut u16, output: &mut Vec<Instr>, jmp_backwards: bool) {
    if output.is_empty() {
        return output.push(Instr::Cmp(condition_id, *len));
    }
    match *output.last().unwrap() {
        Instr::Inf(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::InfCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::InfEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::InfEqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::Sup(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::SupCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::SupEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::SupEqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::Eq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::EqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!("Should not be reached")
            }
        }
        Instr::NotEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::NotEqCmp(o1, o2, *len);
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
    ($args: expr, $variables: expr, $consts: expr, $output: expr, $ctx: expr, $functions: expr, $arrays: expr, $fn_state: expr,$id: expr) => {
        for arg in $args {
            let arg_id = get_id(
                &arg,
                $variables,
                $consts,
                &mut $output,
                &$ctx,
                $functions,
                $arrays,
                $fn_state,
                $id,
            );
            $output.push(Instr::StoreFuncArg(arg_id));
        }
    };
}

fn parse_loop_flow_control(
    loop_code: &mut [Instr],
    loop_id: u16,
    code_length: u16,
    for_loop: bool,
) {
    loop_code.iter_mut().enumerate().for_each(|x| {
        if let Instr::Break(break_id) = x.1 {
            if *break_id == loop_id {
                if for_loop {
                    *x.1 = Instr::Jmp(code_length - x.0 as u16 - 1, false);
                } else {
                    *x.1 = Instr::Jmp(code_length - x.0 as u16, false);
                }
            }
        } else if let Instr::Continue(continue_id) = x.1 {
            if *continue_id == loop_id {
                if for_loop {
                    *x.1 = Instr::Jmp(code_length - x.0 as u16 - 3, false);
                } else {
                    *x.1 = Instr::Jmp(code_length - x.0 as u16 - 1, false);
                }
            }
        }
    });
}

fn parse_indef_loop_flow_control(loop_code: &mut [Instr], loop_id: u16, code_length: u16) {
    loop_code.iter_mut().enumerate().for_each(|x| {
        if let Instr::Break(break_id) = x.1 {
            if *break_id == loop_id {
                *x.1 = Instr::Jmp(code_length - x.0 as u16, false);
            }
        } else if let Instr::Continue(continue_id) = x.1 {
            if *continue_id == loop_id {
                *x.1 = Instr::Jmp(code_length - x.0 as u16 - 3, false);
            }
        }
    });
}

type Function = (String, Box<[String]>, Box<[Expr]>);
type FunctionState = (String, u16, Vec<(Intern<String>, u16)>, Option<u16>);

#[inline(always)]
fn parser_to_instr_set(
    input: &[Expr],
    // variables
    v: &mut Vec<(Intern<String>, u16)>,
    // constants
    consts: &mut Vec<Data>,
    // functions
    fns: &mut Vec<Function>,
    fn_state: Option<&FunctionState>,
    // arrays
    arrs: &mut FnvHashMap<u16, Vec<Data>>,
    id: u16,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::with_capacity(input.len());
    for x in input {
        print!("PARSING {x}");
        // ctx => for errors
        let ctx = x.to_string();
        match x {
            // if number / bool / str, just push it to the constants, and the caller will grab the last index
            Expr::Num(num) => consts.push(Data::Number(*num as Num)),
            Expr::Bool(bool) => consts.push(Data::Bool(*bool)),
            Expr::String(str) => consts.push(Data::String(Intern::from(str.to_string()))),
            Expr::Var(name) => {
                if let Some((_, id)) = v.iter().find(|(var, _)| *name == *var) {
                    consts.push(Data::Null);
                    output.push(Instr::Mov(*id, (consts.len() - 1) as u16));
                } else {
                    error!(
                        ctx,
                        format_args!("Unknown variable {color_red}{name}{color_reset}"),
                        format_args!("Add 'let {name} = 0;'")
                    );
                }
            }
            Expr::Array(elems) => {
                // create new blank array with latest id
                let id = arrs.len() as u16;
                arrs.insert(id, Vec::new());
                for elem in elems {
                    // process each array element
                    let x = parser_to_instr_set(
                        slice::from_ref(elem),
                        v,
                        consts,
                        fns,
                        fn_state,
                        arrs,
                        id,
                    );
                    // if there are no instructions, then that means the element has been pushed to the constants, so pop it and push it directly to the array
                    if x.is_empty() {
                        arrs.get_mut(&id).unwrap().push(consts.pop().unwrap());
                    } else {
                        // if there are instructions, then push everything, add a null to the array, and then add an instruction to move the element to the array at runtime with ArrayMov
                        let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                        output.extend(x);
                        arrs.get_mut(&id).unwrap().push(Data::Null);
                        output.push(Instr::ArrayMov(c_id, id, (arrs[&id].len() - 1) as u16));
                    }
                }
                consts.push(Data::Array(id));
                print!("ARRAYS {arrs:?}");
            }
            // array[index]
            Expr::GetIndex(target, index) => {
                // process the array/string that is being indexed
                let mut id = get_id(
                    target,
                    v,
                    consts,
                    &mut output,
                    &ctx,
                    fns,
                    arrs,
                    fn_state,
                    id,
                );
                // for each index operation, process the index, adjust the id variable for the next index operation, push null to constants to use GetIndex to index at runtime
                for elem in index {
                    let f_id = get_id(elem, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);
                    consts.push(Data::Null);
                    output.push(Instr::GetIndex(id, f_id, (consts.len() - 1) as u16));
                    id = (consts.len() - 1) as u16;
                }
            }
            Expr::Condition(main_condition, code) => {
                // check if condition is not var/str/num (it's supposed to be a bool)
                if matches!(**main_condition, Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", main_condition));
                }

                // get first code limit (after which there are only else(if) blocks)
                let main_code_limit = code
                    .iter()
                    .position(|x| matches!(x, Expr::ElseIfBlock(_, _) | Expr::ElseBlock(_)))
                    .unwrap_or(code.len());

                let condition_blocks_count = code.len() - main_code_limit;
                let mut cmp_markers: Vec<usize> = Vec::with_capacity(condition_blocks_count);
                let mut jmp_markers: Vec<usize> = Vec::with_capacity(condition_blocks_count);
                let mut condition_markers: Vec<usize> = Vec::with_capacity(condition_blocks_count);

                // parse the main condition
                let condition_id = get_id(
                    main_condition,
                    v,
                    consts,
                    &mut output,
                    &ctx,
                    fns,
                    arrs,
                    fn_state,
                    id,
                );
                add_cmp(condition_id, &mut 0, &mut output, false);
                cmp_markers.push(output.len() - 1);
                let mut priv_vars = v.clone();
                // parse the main code block
                let cond_code = parser_to_instr_set(
                    &code[0..main_code_limit],
                    &mut priv_vars,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                    id,
                );
                output.extend(cond_code);
                if main_code_limit != code.len() {
                    output.push(Instr::Jmp(0, false));
                    jmp_markers.push(output.len() - 1);
                }

                for elem in &code[main_code_limit..] {
                    if let Expr::ElseIfBlock(condition, code) = elem {
                        if matches!(**condition, Expr::String(_) | Expr::Num(_)) {
                            error!(ctx, format_args!("{} is not a bool", main_condition));
                        }
                        condition_markers.push(output.len());
                        let condition_id = get_id(
                            condition,
                            v,
                            consts,
                            &mut output,
                            &ctx,
                            fns,
                            arrs,
                            fn_state,
                            id,
                        );
                        add_cmp(condition_id, &mut 0, &mut output, false);
                        cmp_markers.push(output.len() - 1);
                        let mut priv_vars = v.clone();
                        let cond_code = parser_to_instr_set(
                            code,
                            &mut priv_vars,
                            consts,
                            fns,
                            fn_state,
                            arrs,
                            id,
                        );
                        output.extend(cond_code);
                        output.push(Instr::Jmp(0, false));
                        jmp_markers.push(output.len() - 1);
                    } else if let Expr::ElseBlock(code) = elem {
                        condition_markers.push(output.len());
                        let mut priv_vars = v.clone();
                        let cond_code = parser_to_instr_set(
                            code,
                            &mut priv_vars,
                            consts,
                            fns,
                            fn_state,
                            arrs,
                            id,
                        );
                        output.extend(cond_code);
                    }
                }

                for y in jmp_markers {
                    let diff = output.len() - y;
                    output[y] = Instr::Jmp(diff as u16, false);
                }
                for (i, y) in cmp_markers.iter().enumerate() {
                    let diff = if i >= condition_markers.len() {
                        output.len() - 1 - y
                    } else {
                        condition_markers[i] - y
                    };
                    if let Some(
                        Instr::Cmp(_, jump_size)
                        | Instr::InfCmp(_, _, jump_size)
                        | Instr::InfEqCmp(_, _, jump_size)
                        | Instr::SupCmp(_, _, jump_size)
                        | Instr::SupEqCmp(_, _, jump_size)
                        | Instr::EqCmp(_, _, jump_size)
                        | Instr::NotEqCmp(_, _, jump_size),
                    ) = output.get_mut(*y)
                    {
                        *jump_size = diff as u16;
                    }
                }
            }
            Expr::WhileBlock(x, y) => {
                if matches!(&**x, Expr::Var(_) | Expr::String(_) | Expr::Num(_)) {
                    error!(ctx, format_args!("{} is not a bool", *x));
                }
                // try to optimize it (if it's a summation loop)
                if while_loop_summation(&mut output, consts, v, x, y) {
                    continue;
                }

                // parse the condition, get its id
                let condition_id = get_id(x, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);

                // parse the code block, clone the vars to avoid overriding anything
                let mut temp_vars = v.clone();

                let loop_id = id + 1;
                let mut cond_code =
                    parser_to_instr_set(y, &mut temp_vars, consts, fns, fn_state, arrs, loop_id);

                // get length of the code, then add Cmp/OpCmp (decided by add_cmp), and add the condition logic
                let mut len = (cond_code.len() + 2) as u16;
                add_cmp(condition_id, &mut len, &mut output, true);
                parse_loop_flow_control(&mut cond_code, loop_id, len, false);
                output.extend(cond_code);
                output.push(Instr::Jmp(len, true));
            }
            Expr::ForLoop(var_name, array_code) => {
                // parse the array, get its id (the target array is the first Expr in array_code)
                let array = array_code.first().unwrap();
                let code = &array_code[1..];
                let array = get_id(array, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);

                // try to optimize it
                if for_loop_summation(&mut output, consts, v, arrs, array, code) {
                    continue;
                }

                // add an instruction to get array length (func id 2 = len)
                consts.push(Data::Null);
                let array_len_id = (consts.len() - 1) as u16;
                output.push(Instr::Len(array, array_len_id));

                // set up the id of the index variable (0..len)
                consts.push(Data::Number(0.0 as Num));
                let index_id = (consts.len() - 1) as u16;

                // do the 'i < len' condition, set up the condition's id (true/false)
                consts.push(Data::Null);
                let condition_id = (consts.len() - 1) as u16;
                output.push(Instr::Inf(index_id, array_len_id, condition_id));

                // set up the variable for the current element (for current_element_id in ... {}) => current_element_id = array[index]
                consts.push(Data::Null);
                let current_element_id = (consts.len() - 1) as u16;

                // parse everything, add the current element variable to temp_vars so that the loop code can interact with it
                let mut temp_vars = v.clone();
                temp_vars.push((*var_name, current_element_id));

                let current_element_variable_id = temp_vars.len() - 1;

                let loop_id = id + 1;
                let mut cond_code =
                    parser_to_instr_set(code, &mut temp_vars, consts, fns, fn_state, arrs, loop_id);
                // discard the current element variable, no longer needed by the parser
                temp_vars.remove(current_element_variable_id);

                // add the condition ('i < len') jumping logic
                let mut len = (cond_code.len() + 4) as u16;
                add_cmp(condition_id, &mut len, &mut output, true);

                // instruction to make current_element actually hold the array index's value
                output.push(Instr::GetIndex(array, index_id, current_element_id));
                parse_loop_flow_control(&mut cond_code, loop_id, len, true);
                // then add the condition code
                output.extend(cond_code);
                // add 1 to the index (i+=1) so that the next loop iteration will have the next element in the array
                consts.push(Data::Number(1.0 as Num));
                output.push(Instr::Add(index_id, (consts.len() - 1) as u16, index_id));

                // jump back to the loop if still inside of it
                output.push(Instr::Jmp(len, true));

                // clean up, reset the index variable
                consts.push(Data::Number(0.0 as Num));
                output.push(Instr::Mov((consts.len() - 1) as u16, index_id));
            }
            Expr::LoopBlock(code) => {
                let loop_id = id + 1;
                let mut vars = v.clone();
                let mut compiled =
                    parser_to_instr_set(code, &mut vars, consts, fns, fn_state, arrs, loop_id);
                let code_length = compiled.len() as u16 + 1;
                parse_indef_loop_flow_control(&mut compiled, loop_id, code_length);
                output.extend(compiled);
                output.push(Instr::Jmp(code_length, true));
            }
            Expr::VarDeclare(x, y) => {
                let output_len = output.len();
                let obj_id = get_id(y, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);
                if output.len() != output_len {
                    if can_move(output.last().unwrap()) {
                        consts.push(Data::Null);
                    }
                    move_to_id(&mut output, (consts.len() - 1) as u16);
                    v.push((*x, (consts.len() - 1) as u16));
                } else {
                    v.push((*x, obj_id));
                }
            }
            Expr::VarAssign(x, y) => {
                let id = v
                    .iter()
                    .find(|(w, _)| w == x)
                    .unwrap_or_else(|| {
                        error!(
                            ctx,
                            format_args!("Unknown variable {color_red}{x}{color_reset}"),
                            format_args!("Add 'let {x} = 0;'")
                        );
                    })
                    .1;
                let output_len = output.len();
                let obj_id = get_id(y, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);
                if output.len() != output_len {
                    move_to_id(&mut output, id);
                } else {
                    output.push(Instr::Mov(obj_id, id));
                }
            }
            // x[y]... = z;
            Expr::ArrayModify(x, z, w) => {
                // get the id of the target array
                let mut id = get_id(x, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);

                for elem in z.iter().rev().skip(1).rev() {
                    let f_id = get_id(elem, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);

                    consts.push(Data::Null);
                    output.push(Instr::GetIndex(id, f_id, (consts.len() - 1) as u16));
                    id = (consts.len() - 1) as u16;
                }

                // get the
                let final_id = get_id(
                    z.last().unwrap(),
                    v,
                    consts,
                    &mut output,
                    &ctx,
                    fns,
                    arrs,
                    fn_state,
                    id,
                );

                let elem_id = get_id(w, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);

                output.push(Instr::ArrayMod(id, elem_id, final_id));
            }
            Expr::FunctionCall(args, namespace) => {
                let len = namespace.len() - 1;
                let name = namespace[len].as_str();
                let namespace = &namespace[0..len];
                if namespace.is_empty() {
                    match name {
                        "print" => {
                            for arg in args {
                                let id = get_id(
                                    arg,
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                );
                                output.push(Instr::Print(id));
                            }
                        }
                        "type" => {
                            check_args!(args, 1, "type", ctx);
                            let id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Type(id, (consts.len() - 1) as u16));
                        }
                        "Num" => {
                            check_args!(args, 1, "Num", ctx);
                            let id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Num(id, (consts.len() - 1) as u16));
                        }
                        "str" => {
                            check_args!(args, 1, "str", ctx);
                            let id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Str(id, (consts.len() - 1) as u16));
                        }
                        "bool" => {
                            check_args!(args, 1, "bool", ctx);
                            let id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Bool(id, (consts.len() - 1) as u16));
                        }
                        "input" => {
                            check_args_range!(args, 0, 1, "input", ctx);
                            if !args.is_empty() {
                                let id = get_id(
                                    &args[0],
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
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
                                    &args[0],
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                );
                                consts.push(Data::Number(0.0 as Num));
                                consts.push(Data::Null);
                                output.push(Instr::Range(
                                    (consts.len() - 2) as u16,
                                    id_x,
                                    (consts.len() - 1) as u16,
                                ));
                            } else {
                                let id_x = get_id(
                                    &args[0],
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                );
                                let id_y = get_id(
                                    &args[1],
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                );
                                consts.push(Data::Null);
                                output.push(Instr::Range(id_x, id_y, (consts.len() - 1) as u16));
                            }
                        }
                        "floor" => {
                            check_args!(args, 1, "floor", ctx);
                            let id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
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
                            let user_function = fns
                                .iter()
                                .find(|(a, _, _)| *a == function)
                                .unwrap_or_else(|| {
                                    error!(
                                        ctx,
                                        format_args!(
                                            "Unknown function {color_red}{}{color_reset}",
                                            function
                                        )
                                    );
                                });

                            let args_len = user_function.1.len();
                            check_args!(args, args_len, function, ctx);

                            if let Some((name, loc, func_args, _)) = fn_state {
                                if name == function {
                                    let mut saves: Vec<(u16, u16)> = Vec::new();
                                    // recursive function, go back to function def and move on
                                    for i in 0..args_len {
                                        let arg = &args[i];
                                        let val = expr_to_data(arg);
                                        // backup args to avoid them getting modified by the function
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
                                                slice::from_ref(arg),
                                                v,
                                                consts,
                                                fns,
                                                fn_state,
                                                arrs,
                                                id,
                                            );
                                            move_to_id(&mut value, func_args[i].1);
                                            print!("VAL{value:?}");
                                            output.extend(value);
                                        }
                                    }

                                    consts.push(Data::Null);
                                    let final_tgt_id = (consts.len() - 1) as u16;
                                    output.push(Instr::Call(*loc, final_tgt_id));

                                    // restore the args, use MovAnon so as not to be modified by mov_to_id()
                                    for (x, y) in saves {
                                        output.push(Instr::MovAnon(y, x));
                                    }

                                    continue;
                                }
                            }

                            let mut fn_variables: Vec<(Intern<String>, u16)> = Vec::new();

                            for (i, x) in user_function.1.iter().enumerate() {
                                let len = consts.len() as u16;
                                let mut value = parser_to_instr_set(
                                    slice::from_ref(&args[i]),
                                    v,
                                    consts,
                                    &mut fns.clone(),
                                    fn_state,
                                    arrs,
                                    id,
                                );
                                move_to_id(&mut value, len);
                                output.extend(value);
                                fn_variables.push((Intern::from_ref(x), len));
                            }
                            let vars = fn_variables.clone();
                            consts.push(Data::Null);

                            // process and inline the function
                            let user_function_code = user_function.2.to_vec();
                            let to_extend = parser_to_instr_set(
                                &user_function_code,
                                &mut fn_variables,
                                consts,
                                fns,
                                Some(&(
                                    function.to_string(),
                                    output.len() as u16,
                                    vars,
                                    Some((consts.len() - 1) as u16),
                                )),
                                arrs,
                                id,
                            );

                            output.extend(to_extend);
                        }
                    }
                } else if *namespace == ["io"] {
                    match name {
                        "open" => {
                            check_args_range!(args, 1, 2, "open", ctx);
                            consts.push(Data::Null);
                            let arg_id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );

                            let second_arg = if args.len() == 1 {
                                consts.push(Data::Bool(false));
                                (consts.len() - 1) as u16
                            } else {
                                get_id(
                                    &args[1],
                                    v,
                                    consts,
                                    &mut output,
                                    &ctx,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
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
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            output.push(Instr::IoDelete(arg_id));
                        }
                        other => {
                            error!(
                                ctx,
                                format_args!(
                                    "Unknown function {color_red}{}{color_reset} in namespace {color_red}{}{color_reset}",
                                    other,
                                    namespace
                                        .iter()
                                        .map(|x| (*x).to_string())
                                        .collect::<Vec<String>>()
                                        .join("::")
                                )
                            );
                        }
                    }
                } else {
                    error!(
                        ctx,
                        format_args!(
                            "Unknown namespace {color_red}{}{color_reset}",
                            namespace
                                .iter()
                                .map(|x| (*x).to_string())
                                .collect::<Vec<String>>()
                                .join("::")
                        )
                    );
                }
            }
            Expr::ReturnVal(val) => {
                if let Some(x) = fn_state {
                    if let Some(return_value) = &**val {
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
                                id,
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
                let mut id = get_id(obj, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);
                for func in funcs {
                    let args = &func.1;
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
                            output.push(Instr::Len(id, f_id));
                            id = f_id;
                        }
                        "contains" => {
                            check_args!(args, 1, "contains", ctx);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(2, id, f_id));
                            id = f_id;
                        }
                        "trim" => {
                            check_args!(args, 0, "trim", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(3, id, f_id));
                            id = f_id;
                        }
                        "trim_sequence" => {
                            check_args!(args, 1, "trim_sequence", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);
                            output.push(Instr::ApplyFunc(4, id, f_id));
                            id = f_id;
                        }
                        "index" => {
                            check_args!(args, 1, "index", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);
                            output.push(Instr::ApplyFunc(5, id, f_id));
                            id = f_id;
                        }
                        "is_num" => {
                            check_args!(args, 0, "is_num", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(6, id, f_id));
                            id = f_id;
                        }
                        "trim_left" => {
                            check_args!(args, 0, "trim_left", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(7, id, f_id));
                            id = f_id;
                        }
                        "trim_right" => {
                            check_args!(args, 0, "trim_right", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);
                            output.push(Instr::ApplyFunc(8, id, f_id));
                            id = f_id;
                        }
                        "trim_sequence_left" => {
                            check_args!(args, 1, "trim_sequence_left", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);
                            output.push(Instr::ApplyFunc(9, id, f_id));
                            id = f_id;
                        }
                        "trim_sequence_right" => {
                            check_args!(args, 1, "trim_sequence_right", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);
                            output.push(Instr::ApplyFunc(10, id, f_id));
                            id = f_id;
                        }
                        "rindex" => {
                            check_args!(args, 1, "rindex", ctx);
                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);
                            output.push(Instr::ApplyFunc(11, id, f_id));
                            id = f_id;
                        }
                        "repeat" => {
                            check_args!(args, 1, "repeat", ctx);
                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(12, id, f_id));
                            id = f_id;
                        }
                        "push" => {
                            check_args!(args, 1, "push", ctx);
                            let arg_id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );

                            output.push(Instr::Push(id, arg_id));
                        }
                        "sqrt" => {
                            check_args!(args, 0, "sqrt", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::Sqrt(id, f_id));
                            id = f_id;
                        }
                        "round" => {
                            check_args!(args, 0, "round", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(13, id, f_id));
                            id = f_id;
                        }
                        "abs" => {
                            check_args!(args, 0, "abs", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(14, id, f_id));
                            id = f_id;
                        }
                        // io::read
                        "read" => {
                            check_args!(args, 0, "read", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(15, id, f_id));
                            id = f_id;
                        }
                        // io::write
                        "write" => {
                            check_args_range!(args, 1, 2, "write", ctx);

                            let len = args.len();
                            add_args!(args, v, consts, output, ctx, fns, arrs, fn_state, id);
                            if len == 1 {
                                consts.push(Data::Bool(false));
                                output.push(Instr::StoreFuncArg((consts.len() - 1) as u16));
                            }

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(16, id, f_id));
                            id = f_id;
                        }
                        "reverse" => {
                            check_args!(args, 0, "reverse", ctx);

                            let f_id = consts.len() as u16;
                            consts.push(Data::Null);

                            output.push(Instr::ApplyFunc(17, id, f_id));
                            id = f_id;
                        }
                        "split" => {
                            check_args!(args, 1, "split", ctx);
                            let arg_id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Split(id, arg_id, (consts.len() - 1) as u16));
                        }
                        "remove" => {
                            check_args!(args, 1, "remove", ctx);
                            let arg_id = get_id(
                                &args[0],
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            output.push(Instr::Remove(id, arg_id));
                        }
                        other => {
                            error!(
                                ctx,
                                format_args!("Unknown function {color_red}{other}{color_reset}")
                            );
                        }
                    }
                }
            }
            Expr::FunctionDecl(x, y) => {
                if fns.iter().any(|(name, _, _)| **name == *x.first().unwrap()) {
                    error!(
                        ctx,
                        format_args!(
                            "Function {color_red}{}{color_reset} is already defined",
                            x.first().unwrap()
                        )
                    );
                }
                fns.push((
                    x.first().unwrap().to_string(),
                    x.iter().skip(1).map(ToString::to_string).collect(),
                    y.clone(),
                ));
            }
            Expr::Op(items) => {
                let final_stack_capacity = items
                    .iter()
                    .filter(|x| matches!(x, Expr::Opcode(_)))
                    .count();
                let mut item_stack: Vec<&Expr> =
                    Vec::with_capacity(items.len() - final_stack_capacity);
                let mut final_stack: Vec<Instr> = Vec::with_capacity(final_stack_capacity);
                for x in items {
                    if let Expr::Opcode(op) = x {
                        if final_stack.is_empty() {
                            let last = item_stack.pop().unwrap();
                            let first = item_stack.pop().unwrap();

                            let first_v = get_id(
                                first,
                                v,
                                consts,
                                &mut output,
                                &ctx,
                                fns,
                                arrs,
                                fn_state,
                                id,
                            );
                            let second_v =
                                get_id(last, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);
                            consts.push(Data::Null);
                            let x = first_v;
                            let y = second_v;
                            let z = consts.len() - 1;
                            handle_ops!(final_stack, x, y, z as u16, op, consts);
                        } else {
                            let old_id = get_tgt_id(*final_stack.last().unwrap()).unwrap();
                            let new = item_stack.pop().unwrap();
                            let new_v =
                                get_id(new, v, consts, &mut output, &ctx, fns, arrs, fn_state, id);
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
                    slice::from_ref(x),
                    v,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                    id,
                ));
            }
            Expr::Break => output.push(Instr::Break(id)),
            Expr::Continue => output.push(Instr::Continue(id)),
            Expr::EvalBlock(code) => {
                let mut vars = v.clone();
                output.extend(parser_to_instr_set(
                    code, &mut vars, consts, fns, fn_state, arrs, id,
                ))
            }
            other => {
                unreachable!("Not implemented {:?}", other);
            }
        }
    }
    output
}

pub fn parse(contents: &str) -> (Vec<Instr>, Vec<Data>, FnvHashMap<u16, Vec<Data>>) {
    let functions: Vec<Expr> = grammar::FileParser::new().parse(contents).unwrap();
    let mut functions: Vec<Function> = functions
        .iter()
        .map(|w| {
            if let Expr::FunctionDecl(x, y) = w {
                (
                    x.first().unwrap().trim_end_matches('(').to_string(),
                    x.iter()
                        .skip(1)
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .into_boxed_slice(),
                    y.clone(),
                )
            } else {
                error!(contents, "Function expected");
            }
        })
        .collect();

    print!("{functions:?}");

    let mut variables: Vec<(Intern<String>, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let mut arrays: FnvHashMap<u16, Vec<Data>> = FnvHashMap::default();
    let instructions = parser_to_instr_set(
        functions
            .iter()
            .find(|x| x.0 == "main")
            .unwrap_or_else(|| {
                error_b!("Could not find main function");
            })
            .2
            .to_vec()
            .as_slice(),
        &mut variables,
        &mut consts,
        &mut functions,
        None,
        &mut arrays,
        0,
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
