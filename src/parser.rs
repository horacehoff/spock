use crate::display::{lalrpop_error, print_instructions};
use crate::grammar::Token;
use crate::optimizations::{for_loop_summation, while_loop_summation};
use crate::type_inference::{DataType, infer_type};
use crate::util::{format_datatype, format_type_expr};
use crate::{Data, Instr, Num, error};
use crate::{check_args, check_args_range, parser_error, type_inference};
use ariadne::*;
use inline_colorization::*;
use internment::Intern;
use lalrpop_util::lalrpop_mod;
use slab::Slab;
use std::slice;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum Expr {
    Num(Num),
    Bool(bool),
    String(String),
    Var(Intern<String>, usize, usize),
    Array(Box<[Expr]>, usize, usize),
    VarDeclare(Intern<String>, Box<Expr>),
    VarAssign(Intern<String>, Box<Expr>, usize, usize),
    // condition - code (contains else_if_blocks and potentially else_block)
    Condition(Box<Expr>, Box<[Expr]>, usize, usize),
    ElseIfBlock(Box<Expr>, Box<[Expr]>),
    ElseBlock(Box<[Expr]>),

    WhileBlock(Box<Expr>, Box<[Expr]>),
    // args -- (optional namespace + name) -- fn_start -- fn_end -- (arg_start,arg_end)
    FunctionCall(
        Box<[Expr]>,
        Box<[String]>,
        usize,
        usize,
        Box<[(usize, usize)]>,
    ),
    ObjFunctionCall(
        Box<Expr>,
        Box<[Expr]>,
        Box<[String]>,
        usize,
        usize,
        Box<[(usize, usize)]>,
    ),

    // name+args -- code
    FunctionDecl(Box<[String]>, Box<[Expr]>, usize, usize),

    ReturnVal(Box<Option<Expr>>),

    GetIndex(Box<Expr>, Box<[Expr]>, usize, usize),
    ArrayModify(
        Box<Expr>,
        Box<[Expr]>,
        Box<Expr>,
        usize,
        usize,
        usize,
        usize,
    ),

    // id name -- array as first + code
    ForLoop(Intern<String>, Box<[Expr]>),
    Import(String),
    // Closure(Box<[String]>, Box<[Expr]>),
    Break,
    Continue,

    EvalBlock(Box<[Expr]>),
    LoopBlock(Box<[Expr]>),

    Mul(Box<Expr>, Box<Expr>, usize, usize),
    Div(Box<Expr>, Box<Expr>, usize, usize),
    Add(Box<Expr>, Box<Expr>, usize, usize),
    Sub(Box<Expr>, Box<Expr>, usize, usize),
    Mod(Box<Expr>, Box<Expr>, usize, usize),
    Pow(Box<Expr>, Box<Expr>, usize, usize),
    Eq(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    Sup(Box<Expr>, Box<Expr>, usize, usize),
    SupEq(Box<Expr>, Box<Expr>, usize, usize),
    Inf(Box<Expr>, Box<Expr>, usize, usize),
    InfEq(Box<Expr>, Box<Expr>, usize, usize),
    BoolAnd(Box<Expr>, Box<Expr>, usize, usize),
    BoolOr(Box<Expr>, Box<Expr>, usize, usize),
    Neg(Box<Expr>, usize, usize),
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
        | Instr::ArrayAdd(_, _, y)
        | Instr::StrAdd(_, _, y)
        | Instr::Mul(_, _, y)
        | Instr::Sub(_, _, y)
        | Instr::Div(_, _, y)
        | Instr::Mod(_, _, y)
        | Instr::Pow(_, _, y)
        | Instr::Eq(_, _, y)
        | Instr::ArrayEq(_, _, y)
        | Instr::NotEq(_, _, y)
        | Instr::ArrayNotEq(_, _, y)
        | Instr::Sup(_, _, y)
        | Instr::SupEq(_, _, y)
        | Instr::Inf(_, _, y)
        | Instr::InfEq(_, _, y)
        | Instr::BoolAnd(_, _, y)
        | Instr::BoolOr(_, _, y)
        | Instr::Neg(_, y)
        | Instr::Num(_, y)
        | Instr::Bool(_, y)
        | Instr::CallFunc(_, _, y)
        | Instr::Input(_, y)
        | Instr::ArrayGet(_, _, y)
        | Instr::ArrayStrGet(_, _, y)
        | Instr::Range(_, _, y)
        | Instr::Type(_, y)
        | Instr::IoOpen(_, y, _)
        | Instr::Floor(_, y)
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
        | Instr::ArrayAdd(_, _, y)
        | Instr::StrAdd(_, _, y)
        | Instr::Mul(_, _, y)
        | Instr::Sub(_, _, y)
        | Instr::Div(_, _, y)
        | Instr::Mod(_, _, y)
        | Instr::Pow(_, _, y)
        | Instr::Eq(_, _, y)
        | Instr::ArrayEq(_, _, y)
        | Instr::NotEq(_, _, y)
        | Instr::ArrayNotEq(_, _, y)
        | Instr::Sup(_, _, y)
        | Instr::SupEq(_, _, y)
        | Instr::Inf(_, _, y)
        | Instr::InfEq(_, _, y)
        | Instr::BoolAnd(_, _, y)
        | Instr::BoolOr(_, _, y)
        | Instr::Neg(_, y)
        | Instr::Num(_, y)
        | Instr::Bool(_, y)
        | Instr::CallFunc(_, _, y)
        | Instr::Input(_, y)
        | Instr::ArrayGet(_, _, y)
        | Instr::ArrayStrGet(_, _, y)
        | Instr::Range(_, _, y)
        | Instr::Type(_, y)
        | Instr::IoOpen(_, y, _)
        | Instr::Floor(_, y)
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

fn get_id(
    x: &Expr,
    v: &mut Vec<(Intern<String>, u16)>,
    var_types: &mut Vec<(Intern<String>, DataType)>,
    consts: &mut Vec<Data>,
    output: &mut Vec<Instr>,
    fns: &mut Vec<Function>,
    arrs: &mut Slab<Vec<Data>>,
    fn_state: Option<&FunctionState>,
    id: u16,
    // (filename, contents)
    src: (&str, &str),
    instr_src: &mut Vec<(Instr, usize, usize)>,
) -> u16 {
    let op_error = |l: &Expr, r: &Expr, op: &str, start: &usize, end: &usize| {
        parser_error!(
            src.0,
            src.1,
            *start,
            *end,
            "Invalid operation",
            format_args!(
                "Cannot perform operation {color_bright_blue}{style_bold}{} {color_red}{op}{color_bright_blue} {}{color_reset}{style_reset}",
                format_datatype(infer_type(l, var_types, fns)),
                format_datatype(infer_type(r, var_types, fns))
            )
        );
    };
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
        Expr::Var(name, start, end) => {
            if let Some((_, id)) = v.iter().find(|(var, _)| *name == *var) {
                *id
            } else {
                parser_error!(
                    src.0,
                    src.1,
                    *start,
                    *end,
                    "Unknown variable",
                    format_args!(
                        "Variable {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} has not been declared yet"
                    ),
                    format_args!("Declare it with {color_green}let {name} = 0;{color_reset}")
                );
            }
        }
        Expr::Array(elems, start, end) => {
            let first_type = infer_type(&elems[0], var_types, fns);
            if !elems
                .iter()
                .all(|x| infer_type(x, var_types, fns) == first_type)
            {
                parser_error!(
                    src.0,
                    src.1,
                    *start,
                    *end,
                    "Array",
                    format_args!("Arrays can only hold one type of value")
                );
            }
            let array_id = arrs.insert(Vec::new());
            for elem in elems {
                let x = parser_to_instr_set(
                    slice::from_ref(elem),
                    v,
                    var_types,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                    id,
                    src,
                    instr_src,
                );
                if !x.is_empty() {
                    let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                    arrs.get_mut(array_id).unwrap().push(Data::Null);

                    output.extend(x);
                    output.push(Instr::ArrayMov(c_id, id, (arrs[array_id].len() - 1) as u16));
                } else {
                    arrs.get_mut(array_id).unwrap().push(consts.pop().unwrap());
                }
            }
            consts.push(Data::Array(array_id));
            (consts.len() - 1) as u16
        }
        Expr::Mul(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, "*", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Mul(id_l, id_r, id));
            // instr_src.push((Instr::Mul(id_l, id_r, id), *start, *end));
            id
        }
        Expr::Div(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, "/", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Div(id_l, id_r, id));
            // instr_src.push((Instr::Div(id_l, id_r, id), *start, *end));
            id
        }
        Expr::Add(l, r, start, end) => {
            let type_l = infer_type(l, var_types, fns);
            let type_r = infer_type(r, var_types, fns);
            if type_l != type_r
                || !matches!(
                    type_l,
                    DataType::String | DataType::Array(_) | DataType::Number
                )
            {
                op_error(l, r, "+", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            if matches!(type_l, DataType::Array(_)) {
                output.push(Instr::ArrayAdd(id_l, id_r, id));
                // instr_src.push((Instr::ArrayAdd(id_l, id_r, id), *start, *end));
            } else if type_l == DataType::String {
                output.push(Instr::StrAdd(id_l, id_r, id));
                // instr_src.push((Instr::StrAdd(id_l, id_r, id), *start, *end));
            } else if type_l == DataType::Number {
                output.push(Instr::Add(id_l, id_r, id));
                // instr_src.push((Instr::Add(id_l, id_r, id), *start, *end));
            }
            id
        }
        Expr::Sub(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, "-", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Sub(id_l, id_r, id));
            // instr_src.push((Instr::Sub(id_l, id_r, id), *start, *end));
            id
        }
        Expr::Mod(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, "%", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Mod(id_l, id_r, id));
            // instr_src.push((Instr::Mod(id_l, id_r, id), *start, *end));
            id
        }
        Expr::Pow(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, "^", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Pow(id_l, id_r, id));
            // instr_src.push((Instr::Pow(id_l, id_r, id), *start, *end));
            id
        }
        Expr::Eq(l, r) => {
            // println!("EQ LEFT IS {:?}", type_inference::infer_type(l, var_types,fns));
            // println!("EQ RIGHT IS {:?}", type_inference::infer_type(r, var_types,fns));
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            if matches!(infer_type(l, var_types, fns), DataType::Array(_))
                && matches!(infer_type(r, var_types, fns), DataType::Array(_))
            {
                output.push(Instr::ArrayEq(id_l, id_r, id));
            } else {
                output.push(Instr::Eq(id_l, id_r, id));
            }
            id
        }
        Expr::NotEq(l, r) => {
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::NotEq(id_l, id_r, id));
            id
        }
        Expr::Sup(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, ">", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Sup(id_l, id_r, id));
            // instr_src.push((Instr::Sup(id_l, id_r, id), *start, *end));
            id
        }
        Expr::SupEq(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, ">=", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::SupEq(id_l, id_r, id));
            // instr_src.push((Instr::SupEq(id_l, id_r, id), *start, *end));
            id
        }
        Expr::Inf(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, "<", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Inf(id_l, id_r, id));
            // instr_src.push((Instr::Inf(id_l, id_r, id), *start, *end));
            id
        }
        Expr::InfEq(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number
                || infer_type(r, var_types, fns) != DataType::Number
            {
                op_error(l, r, "<=", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::InfEq(id_l, id_r, id));
            // instr_src.push((Instr::InfEq(id_l, id_r, id), *start, *end));
            id
        }
        Expr::BoolAnd(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Bool
                || infer_type(r, var_types, fns) != DataType::Bool
            {
                op_error(l, r, "&&", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::BoolAnd(id_l, id_r, id));
            // instr_src.push((Instr::BoolAnd(id_l, id_r, id), *start, *end));
            id
        }
        Expr::BoolOr(l, r, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Bool
                || infer_type(r, var_types, fns) != DataType::Bool
            {
                op_error(l, r, "||", start, end);
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id_r = get_id(
                r, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::BoolOr(id_l, id_r, id));
            // instr_src.push((Instr::BoolOr(id_l, id_r, id), *start, *end));
            id
        }
        Expr::Neg(l, start, end) => {
            if infer_type(l, var_types, fns) != DataType::Number {
                parser_error!(
                    src.0,
                    src.1,
                    *start,
                    *end,
                    "Invalid operation",
                    format_args!(
                        "Cannot negate {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        format_type_expr(l),
                    )
                );
            }
            let id_l = get_id(
                l, v, var_types, consts, output, fns, arrs, fn_state, id, src, instr_src,
            );
            let id = consts.len() as u16;
            consts.push(Data::Null);
            output.push(Instr::Neg(id_l, id));
            // instr_src.push((Instr::Neg(id_l, id), *start, *end));
            id
        }

        Expr::Condition(main_condition, code, start, end) => {
            let return_id = consts.len() as u16;
            consts.push(Data::Null);

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
                var_types,
                consts,
                output,
                fns,
                arrs,
                fn_state,
                id,
                src,
                instr_src,
            );
            add_cmp(condition_id, &mut 0, output, false);
            cmp_markers.push(output.len() - 1);
            let mut priv_vars = v.clone();
            // parse the main code block
            let cond_code = parser_to_instr_set(
                &code[0..main_code_limit],
                &mut priv_vars,
                var_types,
                consts,
                fns,
                fn_state,
                arrs,
                id,
                src,
                instr_src,
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

            let mut else_exists = false;
            for elem in &code[main_code_limit..] {
                if let Expr::ElseIfBlock(condition, code) = elem {
                    condition_markers.push(output.len());
                    let condition_id = get_id(
                        condition, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                        instr_src,
                    );
                    add_cmp(condition_id, &mut 0, output, false);
                    cmp_markers.push(output.len() - 1);
                    let mut priv_vars = v.clone();
                    let cond_code = parser_to_instr_set(
                        code,
                        &mut priv_vars,
                        var_types,
                        consts,
                        fns,
                        fn_state,
                        arrs,
                        id,
                        src,
                        instr_src,
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
                    output.push(Instr::Jmp(0, false));
                    jmp_markers.push(output.len() - 1);
                } else if let Expr::ElseBlock(code) = elem {
                    else_exists = true;
                    condition_markers.push(output.len());
                    let mut priv_vars = v.clone();
                    let cond_code = parser_to_instr_set(
                        code,
                        &mut priv_vars,
                        var_types,
                        consts,
                        fns,
                        fn_state,
                        arrs,
                        id,
                        src,
                        instr_src,
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
                }
            }
            if !else_exists {
                parser_error!(
                    src.0,
                    src.1,
                    *start,
                    *end,
                    "Invalid condition",
                    format_args!("Inline conditions need an else statement")
                );
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
                    | Instr::ArrayEqCmp(_, _, jump_size)
                    | Instr::NotEqCmp(_, _, jump_size)
                    | Instr::ArrayNotEqCmp(_, _, jump_size),
                ) = output.get_mut(*y)
                {
                    *jump_size = diff as u16;
                }
            }
            return_id
        }
        other => {
            let output_code = parser_to_instr_set(
                slice::from_ref(other),
                v,
                var_types,
                consts,
                fns,
                fn_state,
                arrs,
                id,
                src,
                instr_src,
            );
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
                unreachable!()
            }
        }
        Instr::InfEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::InfEqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!()
            }
        }
        Instr::Sup(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::SupCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!()
            }
        }
        Instr::SupEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::SupEqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!()
            }
        }
        Instr::Eq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::EqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!()
            }
        }
        Instr::ArrayEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::ArrayEqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!()
            }
        }
        Instr::NotEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::NotEqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!()
            }
        }
        Instr::ArrayNotEq(o1, o2, o3) => {
            if o3 == condition_id {
                *output.last_mut().unwrap() = Instr::ArrayNotEqCmp(o1, o2, *len);
                if jmp_backwards {
                    *len -= 1;
                }
            } else {
                unreachable!()
            }
        }

        _ => {
            output.push(Instr::Cmp(condition_id, *len));
        }
    }
}

macro_rules! add_args {
    ($args: expr, $variables: expr,$var_types:expr, $consts: expr, $output: expr, $functions: expr, $arrays: expr, $fn_state: expr,$id: expr,$src:expr,$instr_src:expr) => {
        for arg in $args {
            let arg_id = get_id(
                &arg,
                $variables,
                $var_types,
                $consts,
                &mut $output,
                $functions,
                $arrays,
                $fn_state,
                $id,
                $src,
                $instr_src,
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

pub type Function = (
    String,
    Box<[String]>,
    Box<[Expr]>,
    Option<u16>,
    Option<Vec<u16>>,
);
type FunctionState = (String, u16, Vec<(Intern<String>, u16)>, Option<u16>);

#[inline(always)]
fn parser_to_instr_set(
    input: &[Expr],
    // variables
    v: &mut Vec<(Intern<String>, u16)>,
    var_types: &mut Vec<(Intern<String>, DataType)>,
    // constants
    consts: &mut Vec<Data>,
    // functions
    fns: &mut Vec<Function>,
    fn_state: Option<&FunctionState>,
    // arrays
    arrs: &mut Slab<Vec<Data>>,
    id: u16,
    // (filename, contents)
    src: (&str, &str),
    instr_src: &mut Vec<(Instr, usize, usize)>,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::with_capacity(input.len());
    for x in input {
        match x {
            // if number / bool / str, just push it to the constants, and the caller will grab the last index
            Expr::Num(num) => consts.push(Data::Number(*num as Num)),
            Expr::Bool(bool) => consts.push(Data::Bool(*bool)),
            Expr::String(str) => consts.push(Data::String(Intern::from(str.to_string()))),
            Expr::Var(name, start, end) => {
                if let Some((_, id)) = v.iter().find(|(var, _)| *name == *var) {
                    consts.push(Data::Null);
                    output.push(Instr::Mov(*id, (consts.len() - 1) as u16));
                } else {
                    parser_error!(
                        src.0,
                        src.1,
                        *start,
                        *end,
                        "Unknown variable",
                        format_args!(
                            "Variable {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} has not been declared yet"
                        ),
                        format_args!("Declare it with {color_green}let {name} = 0;{color_reset}")
                    );
                }
            }
            Expr::Array(elems, start, end) => {
                let first_type = infer_type(&elems[0], var_types, fns);
                if !elems
                    .iter()
                    .all(|x| infer_type(x, var_types, fns) == first_type)
                {
                    parser_error!(
                        src.0,
                        src.1,
                        *start,
                        *end,
                        "Array",
                        format_args!("Arrays can only hold one type of value")
                    );
                }
                // create new blank array with latest id
                let array_id = arrs.insert(Vec::new());
                for elem in elems {
                    // process each array element
                    let x = parser_to_instr_set(
                        slice::from_ref(elem),
                        v,
                        var_types,
                        consts,
                        fns,
                        fn_state,
                        arrs,
                        id,
                        src,
                        instr_src,
                    );
                    // if there are no instructions, then that means the element has been pushed to the constants, so pop it and push it directly to the array
                    if x.is_empty() {
                        arrs.get_mut(array_id).unwrap().push(consts.pop().unwrap());
                    } else {
                        // if there are instructions, then push everything, add a null to the array, and then add an instruction to move the element to the array at runtime with ArrayMov
                        let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                        output.extend(x);
                        arrs.get_mut(array_id).unwrap().push(Data::Null);
                        output.push(Instr::ArrayMov(c_id, id, (arrs[array_id].len() - 1) as u16));
                    }
                }
                consts.push(Data::Array(array_id));
            }
            // array[index]
            Expr::GetIndex(target, index, start, end) => {
                let mut infered = infer_type(target, var_types, fns);
                // process the array/string that is being indexed
                let mut id = get_id(
                    target,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );
                // for each index operation, process the index, adjust the id variable for the next index operation, push null to constants to use GetIndex to index at runtime
                for elem in index {
                    if !matches!(infered, DataType::String | DataType::Array(_)) {
                        parser_error!(
                            src.0,
                            src.1,
                            *start,
                            *end,
                            "Invalid type",
                            format_args!(
                                "Cannot index {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                format_datatype(infered)
                            )
                        );
                    }

                    let index_infered = infer_type(elem, var_types, fns);
                    if index_infered != DataType::Number {
                        parser_error!(
                            src.0,
                            src.1,
                            *start,
                            *end,
                            "Invalid type",
                            format_args!(
                                "{color_bright_blue}{style_bold}{}{color_reset}{style_reset} is not a valid index",
                                format_datatype(index_infered)
                            )
                        );
                    }
                    let f_id = get_id(
                        elem,
                        v,
                        var_types,
                        consts,
                        &mut output,
                        fns,
                        arrs,
                        fn_state,
                        id,
                        src,
                        instr_src,
                    );
                    consts.push(Data::Null);
                    if infered == DataType::String {
                        instr_src.push((
                            Instr::ArrayStrGet(id, f_id, (consts.len() - 1) as u16),
                            *start,
                            *end,
                        ));
                        output.push(Instr::ArrayStrGet(id, f_id, (consts.len() - 1) as u16));
                    } else {
                        instr_src.push((
                            Instr::ArrayStrGet(id, f_id, (consts.len() - 1) as u16),
                            *start,
                            *end,
                        ));
                        output.push(Instr::ArrayGet(id, f_id, (consts.len() - 1) as u16));
                    }
                    id = (consts.len() - 1) as u16;
                    if let DataType::Array(array_type) = infered {
                        infered = *array_type;
                    }
                }
            }
            // x[y]... = z;
            Expr::ArrayModify(x, z, w, index_start, index_end, elem_start, elem_end) => {
                let infered = infer_type(x, var_types, fns);
                if !matches!(infered, DataType::String | DataType::Array(_)) {
                    parser_error!(
                        src.0,
                        src.1,
                        *index_start,
                        *index_end,
                        "Invalid type",
                        format_args!(
                            "Cannot index {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            format_datatype(infered)
                        )
                    );
                }
                // get the id of the target array
                let mut id = get_id(
                    x,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );

                for elem in z.iter().rev().skip(1).rev() {
                    let infered = infer_type(elem, var_types, fns);
                    if infered != DataType::Number {
                        parser_error!(
                            src.0,
                            src.1,
                            *index_start,
                            *index_end,
                            "Invalid type",
                            format_args!(
                                "{color_bright_blue}{style_bold}{}{color_reset}{style_reset} is not a valid index",
                                format_datatype(infered)
                            )
                        );
                    }
                    let f_id = get_id(
                        elem,
                        v,
                        var_types,
                        consts,
                        &mut output,
                        fns,
                        arrs,
                        fn_state,
                        id,
                        src,
                        instr_src,
                    );

                    consts.push(Data::Null);
                    output.push(Instr::ArrayGet(id, f_id, (consts.len() - 1) as u16));
                    id = (consts.len() - 1) as u16;
                }

                // get the
                let final_id = get_id(
                    z.last().unwrap(),
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );

                let elem_type = infer_type(w, var_types, fns);
                let elem_id = get_id(
                    w,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );

                if let DataType::Array(array_type) = &infered {
                    if **array_type != elem_type {
                        parser_error!(
                            src.0,
                            src.1,
                            *elem_start,
                            *elem_end,
                            "Invalid type",
                            format_args!(
                                "Cannot insert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} in {}",
                                format_datatype(elem_type),
                                format_datatype(infered)
                            )
                        );
                    }
                } else if infered == DataType::String && elem_type != DataType::String {
                    parser_error!(
                        src.0,
                        src.1,
                        *elem_start,
                        *elem_end,
                        "Invalid type",
                        format_args!(
                            "Cannot insert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} in String",
                            format_datatype(elem_type)
                        )
                    );
                }

                if infered == DataType::String {
                    instr_src.push((
                        Instr::StrMod(id, elem_id, final_id),
                        *index_start,
                        *index_end,
                    ));
                    output.push(Instr::StrMod(id, elem_id, final_id));
                } else {
                    instr_src.push((
                        Instr::ArrayMod(id, elem_id, final_id),
                        *index_start,
                        *index_end,
                    ));
                    output.push(Instr::ArrayMod(id, elem_id, final_id));
                }
            }
            Expr::Condition(main_condition, code, _, _) => {
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
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );
                add_cmp(condition_id, &mut 0, &mut output, false);
                cmp_markers.push(output.len() - 1);
                let mut priv_vars = v.clone();
                // parse the main code block
                let cond_code = parser_to_instr_set(
                    &code[0..main_code_limit],
                    &mut priv_vars,
                    var_types,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                    id,
                    src,
                    instr_src,
                );
                output.extend(cond_code);
                if main_code_limit != code.len() {
                    output.push(Instr::Jmp(0, false));
                    jmp_markers.push(output.len() - 1);
                }

                for elem in &code[main_code_limit..] {
                    if let Expr::ElseIfBlock(condition, code) = elem {
                        condition_markers.push(output.len());
                        let condition_id = get_id(
                            condition,
                            v,
                            var_types,
                            consts,
                            &mut output,
                            fns,
                            arrs,
                            fn_state,
                            id,
                            src,
                            instr_src,
                        );
                        add_cmp(condition_id, &mut 0, &mut output, false);
                        cmp_markers.push(output.len() - 1);
                        let mut priv_vars = v.clone();
                        let cond_code = parser_to_instr_set(
                            code,
                            &mut priv_vars,
                            var_types,
                            consts,
                            fns,
                            fn_state,
                            arrs,
                            id,
                            src,
                            instr_src,
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
                            var_types,
                            consts,
                            fns,
                            fn_state,
                            arrs,
                            id,
                            src,
                            instr_src,
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
                        *jump_size = diff as u16 + 1;
                    }
                }
            }
            Expr::WhileBlock(condition, code) => {
                // try to optimize it (if it's a summation loop)
                if while_loop_summation(&mut output, consts, v, condition, code) {
                    continue;
                }

                // parse the condition, get its id
                let condition_id = get_id(
                    condition,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );

                // parse the code block, clone the vars to avoid overriding anything
                let mut temp_vars = v.clone();

                let loop_id = id + 1;
                let mut cond_code = parser_to_instr_set(
                    code,
                    &mut temp_vars,
                    var_types,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                    loop_id,
                    src,
                    instr_src,
                );

                // get length of the code, then add Cmp/OpCmp (decided by add_cmp), and add the condition logic
                let mut len = (cond_code.len() + 2) as u16;
                add_cmp(condition_id, &mut len, &mut output, true);
                parse_loop_flow_control(&mut cond_code, loop_id, len, false);
                output.extend(cond_code);
                output.push(Instr::Jmp(len, true));
            }
            Expr::ForLoop(var_name, array_code) => {
                let real_var = var_name.as_str() != "_";

                // parse the array, get its id (the target array is the first Expr in array_code)
                let array = array_code.first().unwrap();
                let code = &array_code[1..];
                let array_type = infer_type(array, var_types, fns);
                let array = get_id(
                    array,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );

                // try to optimize it
                if for_loop_summation(&mut output, consts, v, array, code) {
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
                let current_element_id = consts.len() as u16;
                if real_var {
                    consts.push(Data::Null);
                }

                // parse everything, add the current element variable to temp_vars so that the loop code can interact with it
                let mut temp_vars = v.clone();
                temp_vars.push((*var_name, current_element_id));
                var_types.push((
                    *var_name,
                    if array_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(a_type) = &array_type {
                        *a_type.clone()
                    } else {
                        todo!("For loop invalid type")
                    },
                ));

                let current_element_variable_id = temp_vars.len() - 1;

                let loop_id = id + 1;
                let mut cond_code = parser_to_instr_set(
                    code,
                    &mut temp_vars,
                    var_types,
                    consts,
                    fns,
                    fn_state,
                    arrs,
                    loop_id,
                    src,
                    instr_src,
                );
                // discard the current element variable, no longer needed by the parser
                temp_vars.remove(current_element_variable_id);

                // add the condition ('i < len') jumping logic
                let mut len = (cond_code.len() + 3) as u16 + if real_var { 1 } else { 0 };
                add_cmp(condition_id, &mut len, &mut output, true);

                // instruction to make current_element actually hold the array index's value
                if real_var {
                    if array_type == DataType::String {
                        output.push(Instr::ArrayStrGet(array, index_id, current_element_id));
                    } else {
                        output.push(Instr::ArrayGet(array, index_id, current_element_id));
                    }
                }
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
                let mut compiled = parser_to_instr_set(
                    code, &mut vars, var_types, consts, fns, fn_state, arrs, loop_id, src,
                    instr_src,
                );
                let code_length = compiled.len() as u16;
                parse_indef_loop_flow_control(&mut compiled, loop_id, code_length + 1);
                output.extend(compiled);
                output.push(Instr::Jmp(code_length, true));
            }
            Expr::VarDeclare(x, y) => {
                let var_type = type_inference::infer_type(y, var_types, fns);
                let output_len = output.len();
                let obj_id = get_id(
                    y,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );
                if output.len() != output_len {
                    if can_move(output.last().unwrap()) {
                        consts.push(Data::Null);
                    }
                    move_to_id(&mut output, (consts.len() - 1) as u16);
                    v.push((*x, (consts.len() - 1) as u16));
                } else {
                    v.push((*x, obj_id));
                }
                var_types.push((*x, var_type));
            }
            Expr::VarAssign(name, y, start, end) => {
                let var_type = type_inference::infer_type(y, var_types, fns);
                let id = v
                    .iter()
                    .find(|(w, _)| w == name)
                    .unwrap_or_else(|| {
                        parser_error!(
                            src.0,
                            src.1,
                            *start,
                            *end,
                            "Unknown variable",
                            format_args!(
                                "Variable {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} has not been declared yet"
                            ),
                            format_args!("Declare it with {color_green}let {name} = 0;{color_reset}")
                        );
                    })
                    .1;
                let output_len = output.len();
                let obj_id = get_id(
                    y,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );
                if output.len() != output_len {
                    move_to_id(&mut output, id);
                } else {
                    output.push(Instr::Mov(obj_id, id));
                }

                var_types
                    .iter_mut()
                    .find(|(v_name, _)| v_name == name)
                    .unwrap()
                    .1 = var_type;
                println!("NEW VAR TYPES ARE {var_types:?}");
            }

            Expr::FunctionCall(args, namespace, start, end, args_indexes) => {
                let check_type = |arg: usize, expected: &[DataType]| {
                    let infered = infer_type(&args[arg], var_types, fns);

                    if !{
                        if let DataType::Poly(polytype) = &infered {
                            polytype.iter().all(|x| expected.contains(x))
                        } else {
                            expected.contains(&infered)
                        }
                    } {
                        parser_error!(
                            src.0,
                            src.1,
                            args_indexes[arg].0,
                            args_indexes[arg].1,
                            "Invalid type",
                            format_args!(
                                "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                expected
                                    .into_iter()
                                    .map(|x| format_datatype(x.clone()).to_lowercase())
                                    .collect::<Vec<String>>()
                                    .join(" or "),
                                format_datatype(infered)
                            )
                        );
                    }
                };
                let len = namespace.len() - 1;
                let full_name = namespace.join("::");
                let name = namespace[len].as_str();
                let namespace = &namespace[0..len];
                if namespace.is_empty() {
                    match name {
                        "print" => {
                            for arg in args {
                                let id = get_id(
                                    arg,
                                    v,
                                    var_types,
                                    consts,
                                    &mut output,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                    src,
                                    instr_src,
                                );
                                output.push(Instr::Print(id));
                            }
                        }
                        "type" => {
                            check_args!(args, 1, "type", src.0, src.1, *start, *end);
                            let id = get_id(
                                &args[0],
                                v,
                                var_types,
                                consts,
                                &mut output,
                                fns,
                                arrs,
                                fn_state,
                                id,
                                src,
                                instr_src,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Type(id, (consts.len() - 1) as u16));
                        }
                        "num" => {
                            check_args!(args, 1, "num", src.0, src.1, *start, *end);
                            check_type(0, &[DataType::String, DataType::Number]);
                            let id = get_id(
                                &args[0],
                                v,
                                var_types,
                                consts,
                                &mut output,
                                fns,
                                arrs,
                                fn_state,
                                id,
                                src,
                                instr_src,
                            );
                            consts.push(Data::Null);
                            instr_src.push((
                                Instr::Num(id, (consts.len() - 1) as u16),
                                *start,
                                *end,
                            ));
                            output.push(Instr::Num(id, (consts.len() - 1) as u16));
                        }
                        "str" => {
                            check_args!(args, 1, "str", src.0, src.1, *start, *end);
                            let id = get_id(
                                &args[0],
                                v,
                                var_types,
                                consts,
                                &mut output,
                                fns,
                                arrs,
                                fn_state,
                                id,
                                src,
                                instr_src,
                            );
                            consts.push(Data::Null);
                            output.push(Instr::Str(id, (consts.len() - 1) as u16));
                        }
                        "bool" => {
                            check_args!(args, 1, "bool", src.0, src.1, *start, *end);
                            check_type(0, &[DataType::String, DataType::Bool]);
                            let id = get_id(
                                &args[0],
                                v,
                                var_types,
                                consts,
                                &mut output,
                                fns,
                                arrs,
                                fn_state,
                                id,
                                src,
                                instr_src,
                            );
                            consts.push(Data::Null);
                            instr_src.push((
                                Instr::Bool(id, (consts.len() - 1) as u16),
                                *start,
                                *end,
                            ));
                            output.push(Instr::Bool(id, (consts.len() - 1) as u16));
                        }
                        "input" => {
                            check_args_range!(args, 0, 1, "input", src.0, src.1, *start, *end);
                            check_type(0, &[DataType::String]);
                            let id = if args.is_empty() {
                                consts.push(Data::String(Intern::from(String::new())));
                                (consts.len() - 1) as u16
                            } else {
                                get_id(
                                    &args[0],
                                    v,
                                    var_types,
                                    consts,
                                    &mut output,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                    src,
                                    instr_src,
                                )
                            };
                            consts.push(Data::Null);
                            instr_src.push((
                                Instr::Input(id, (consts.len() - 1) as u16),
                                *start,
                                *end,
                            ));
                            output.push(Instr::Input(id, (consts.len() - 1) as u16));
                        }
                        "range" => {
                            check_args_range!(args, 1, 2, "range", src.0, src.1, *start, *end);
                            if args.len() == 1 {
                                check_type(0, &[DataType::Number]);
                                let id_x = get_id(
                                    &args[0],
                                    v,
                                    var_types,
                                    consts,
                                    &mut output,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                    src,
                                    instr_src,
                                );
                                consts.push(Data::Number(0.0 as Num));
                                consts.push(Data::Null);
                                output.push(Instr::Range(
                                    (consts.len() - 2) as u16,
                                    id_x,
                                    (consts.len() - 1) as u16,
                                ));
                            } else {
                                check_type(0, &[DataType::Number]);
                                check_type(1, &[DataType::Number]);
                                let id_x = get_id(
                                    &args[0],
                                    v,
                                    var_types,
                                    consts,
                                    &mut output,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                    src,
                                    instr_src,
                                );
                                let id_y = get_id(
                                    &args[1],
                                    v,
                                    var_types,
                                    consts,
                                    &mut output,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                    src,
                                    instr_src,
                                );
                                consts.push(Data::Null);
                                output.push(Instr::Range(id_x, id_y, (consts.len() - 1) as u16));
                            }
                        }
                        "floor" => {
                            check_args!(args, 1, "floor", src.0, src.1, *start, *end);
                            check_type(0, &[DataType::Number]);
                            let id = get_id(
                                &args[0],
                                v,
                                var_types,
                                consts,
                                &mut output,
                                fns,
                                arrs,
                                fn_state,
                                id,
                                src,
                                instr_src,
                            );
                            consts.push(Data::Null);
                            instr_src.push((
                                Instr::Num(id, (consts.len() - 1) as u16),
                                *start,
                                *end,
                            ));
                            output.push(Instr::Num(id, (consts.len() - 1) as u16));
                        }
                        "the_answer" => {
                            check_args!(args, 0, "the_answer", src.0, src.1, *start, *end);
                            consts.push(Data::Null);
                            output.push(Instr::TheAnswer((consts.len() - 1) as u16));
                        }
                        fn_name => {
                            let mut fns_clone = fns.clone();
                            let function_id = fns
                                .iter_mut()
                                .position(|(a, _, _, _, _)| *a == fn_name)
                                .unwrap_or_else(|| {
                                    parser_error!(
                                        src.0,
                                        src.1,
                                        *start,
                                        *end,
                                        "Unknown function",
                                        format_args!(
                                            "Function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} does not exist or has not been declared yet"
                                        )
                                    );
                                });
                            let (_, fn_args, fn_code, fn_loc, fn_args_loc) =
                                &fns[function_id].clone();
                            let args_len = fn_args.len();
                            check_args!(args, args_len, fn_name, src.0, src.1, *start, *end);

                            if fn_loc.is_none() {
                                let mut vars: Vec<(Intern<String>, u16)> = Vec::new();
                                for (i, x) in fn_args.iter().enumerate() {
                                    consts.push(Data::Null);
                                    vars.push((Intern::from(x.clone()), (consts.len() - 1) as u16));
                                    let infered = infer_type(&args[i], var_types, fns);
                                    var_types.push((Intern::from(x.clone()), infered));
                                }
                                fns.get_mut(function_id).unwrap().4 =
                                    Some(vars.iter().map(|(_, x)| *x).collect::<Vec<u16>>());
                                output.push(Instr::Jmp(0, false));
                                let jump_idx = output.len() - 1;
                                let fn_start = output.len();
                                fns.get_mut(function_id).unwrap().3 = Some(fn_start as u16);
                                let mut parsed = parser_to_instr_set(
                                    fn_code, &mut vars, var_types, consts, &mut *fns, fn_state,
                                    arrs, id, src, instr_src,
                                );
                                let len = parsed.len();
                                println!("PARSED IS {parsed:?}");
                                parsed.iter_mut().for_each(|x| {
                                    println!("ENCOUNTERED {x:?}");
                                    if let Instr::JmpSave(size, neg) = x {
                                        *size += (output.len() + len - 3) as u16;
                                    }
                                });
                                output.extend(parsed);
                                output.push(Instr::JmpLoad(false));
                                *output.get_mut(jump_idx).unwrap() =
                                    Instr::Jmp((output.len() - fn_start + 1) as u16, false)
                            }

                            if let Some(fn_args) = fns[function_id].4.clone() {
                                for (x, tgt_id) in fn_args.iter().enumerate() {
                                    let start_len = output.len();
                                    let arg_id = get_id(
                                        &args[x],
                                        v,
                                        var_types,
                                        consts,
                                        &mut output,
                                        fns,
                                        arrs,
                                        fn_state,
                                        id,
                                        src,
                                        instr_src,
                                    );

                                    if output.len() != start_len {
                                        move_to_id(&mut output, *tgt_id);
                                    } else {
                                        output.push(Instr::Mov(arg_id, *tgt_id))
                                    }
                                }
                            }
                            let loc = if let Some(fn_loc) = &fns[function_id].3 {
                                *fn_loc
                            } else {
                                unreachable!()
                            };

                            println!("LOC IS {loc:?}");
                            println!("OUTP LEN IS {}", output.len());
                            println!("OUTPUT IS {output:?}");
                            output.push(Instr::JmpSave((output.len() as u16) - loc, true));
                        }
                    }
                } else if *namespace == ["io"] {
                    match name {
                        "open" => {
                            check_args_range!(args, 1, 2, "open", src.0, src.1, *start, *end);
                            consts.push(Data::Null);
                            let arg_id = get_id(
                                &args[0],
                                v,
                                var_types,
                                consts,
                                &mut output,
                                fns,
                                arrs,
                                fn_state,
                                id,
                                src,
                                instr_src,
                            );

                            let second_arg = if args.len() == 1 {
                                consts.push(Data::Bool(false));
                                (consts.len() - 1) as u16
                            } else {
                                get_id(
                                    &args[1],
                                    v,
                                    var_types,
                                    consts,
                                    &mut output,
                                    fns,
                                    arrs,
                                    fn_state,
                                    id,
                                    src,
                                    instr_src,
                                )
                            };

                            instr_src.push((
                                Instr::IoOpen(arg_id, (consts.len() - 1) as u16, second_arg),
                                *start,
                                *end,
                            ));
                            output.push(Instr::IoOpen(
                                arg_id,
                                (consts.len() - 1) as u16,
                                second_arg,
                            ));
                        }
                        "delete" => {
                            check_args!(args, 1, "delete", src.0, src.1, *start, *end);
                            let arg_id = get_id(
                                &args[0],
                                v,
                                var_types,
                                consts,
                                &mut output,
                                fns,
                                arrs,
                                fn_state,
                                id,
                                src,
                                instr_src,
                            );
                            instr_src.push((Instr::IoDelete(arg_id), *start, *end));
                            output.push(Instr::IoDelete(arg_id));
                        }
                        _ => {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Unknown function in namespace",
                                format_args!(
                                    "Namespace {color_bright_blue}{style_bold}{}{color_reset}{style_reset} does not contain function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset}",
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
                    parser_error!(
                        src.0,
                        src.1,
                        *start,
                        *end,
                        "Unknown namespace",
                        format_args!(
                            "Namespace {color_bright_blue}{style_bold}{}{color_reset}{style_reset} does not exist",
                            namespace
                                .iter()
                                .map(|x| (*x).to_string())
                                .collect::<Vec<String>>()
                                .join("::")
                        )
                    );
                }
            }
            Expr::ObjFunctionCall(obj, args, namespace, start, end, args_indexes) => {
                let check_arg_type = |arg: usize, expected: &[DataType]| {
                    let infered = infer_type(&args[arg], var_types, fns);

                    if !{
                        if let DataType::Poly(polytype) = &infered {
                            polytype.iter().all(|x| expected.contains(x))
                        } else {
                            expected.contains(&infered)
                        }
                    } {
                        parser_error!(
                            src.0,
                            src.1,
                            args_indexes[arg].0,
                            args_indexes[arg].1,
                            "Invalid type",
                            format_args!(
                                "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                expected
                                    .into_iter()
                                    .map(|x| format_datatype(x.clone()).to_lowercase())
                                    .collect::<Vec<String>>()
                                    .join(" or "),
                                format_datatype(infered)
                            )
                        );
                    }
                };

                macro_rules! check_obj_type {
                    ($expected: expr) => {
                        let infered = infer_type(obj, var_types, fns);

                        if !{
                            if let DataType::Poly(polytype) = &infered {
                                polytype.iter().all(|x| $expected.contains(x))
                            } else {
                                $expected.contains(&infered)
                            }
                        } {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    $expected
                                        .into_iter()
                                        .map(|x| format_datatype(x.clone()).to_lowercase())
                                        .collect::<Vec<String>>()
                                        .join(" or "),
                                    format_datatype(infered)
                                )
                            );
                        }
                    };
                }

                macro_rules! check_obj_type_array {
                    ($expected: expr) => {
                        let infered = infer_type(obj, var_types, fns);

                        if !{
                            if let DataType::Poly(polytype) = &infered {
                                polytype.iter().all(|x| $expected.contains(x))
                            } else {
                                $expected.contains(&infered)
                            }
                        } {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    $expected
                                        .into_iter()
                                        .map(|x| format_datatype(x.clone()).to_lowercase())
                                        .collect::<Vec<String>>()
                                        .join(" or "),
                                    format_datatype(infered)
                                )
                            );
                        }
                    };
                }

                let id = get_id(
                    obj,
                    v,
                    var_types,
                    consts,
                    &mut output,
                    fns,
                    arrs,
                    fn_state,
                    id,
                    src,
                    instr_src,
                );
                let len = namespace.len() - 1;
                let name = namespace[len].as_str();
                // not in use for now
                let namespace = &namespace[0..len];
                match name {
                    "uppercase" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 0, "uppercase", src.0, src.1, *start, *end);
                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        output.push(Instr::CallFunc(0, id, f_id));
                        // instr_src.push((Instr::CallFunc(0, id, f_id), *start, *end));
                    }
                    "lowercase" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 0, "lowercase", src.0, src.1, *start, *end);
                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        output.push(Instr::CallFunc(1, id, f_id));
                        instr_src.push((Instr::CallFunc(1, id, f_id), *start, *end))
                    }
                    "len" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::String | DataType::Array(_)) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected String or Array, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }
                        check_args!(args, 0, "len", src.0, src.1, *start, *end);
                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        instr_src.push((Instr::Len(id, f_id), *start, *end));
                        output.push(Instr::Len(id, f_id));
                    }
                    "contains" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::String | DataType::Array(_)) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected String or Array, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        check_args!(args, 1, "contains", src.0, src.1, *start, *end);

                        let arg_infered = infer_type(&args[0], var_types, fns);
                        if infered == DataType::String && arg_infered != DataType::String {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(arg_infered)
                                )
                            );
                        }

                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        output.push(Instr::CallFunc(2, id, f_id));
                        instr_src.push((Instr::CallFunc(2, id, f_id), *start, *end))
                    }
                    "trim" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 0, "trim", src.0, src.1, *start, *end);

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        output.push(Instr::CallFunc(3, id, f_id));
                        instr_src.push((Instr::CallFunc(3, id, f_id), *start, *end))
                    }
                    "trim_sequence" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 1, "trim_sequence", src.0, src.1, *start, *end);

                        let infered = infer_type(&args[0], var_types, fns);
                        if infered != DataType::String {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );
                        output.push(Instr::CallFunc(4, id, f_id));
                        instr_src.push((Instr::CallFunc(4, id, f_id), *start, *end))
                    }
                    "index" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::String | DataType::Array(_)) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected String or Array, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        check_args!(args, 1, "index", src.0, src.1, *start, *end);

                        let arg_infered = infer_type(&args[0], var_types, fns);
                        if let DataType::Array(array_type) = &infered {
                            if **array_type != arg_infered {
                                parser_error!(
                                    src.0,
                                    src.1,
                                    args_indexes[0].0,
                                    args_indexes[0].1,
                                    "Invalid type",
                                    format_args!(
                                        "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                        format_datatype(*array_type.clone()),
                                        format_datatype(infered),
                                        format_datatype(arg_infered)
                                    )
                                );
                            }
                        } else if arg_infered != infered {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(arg_infered)
                                )
                            );
                        }

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );
                        output.push(Instr::CallFunc(5, id, f_id));
                        instr_src.push((Instr::CallFunc(5, id, f_id), *start, *end))
                    }
                    "is_num" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 0, "is_num", src.0, src.1, *start, *end);
                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        output.push(Instr::CallFunc(6, id, f_id));
                        instr_src.push((Instr::CallFunc(6, id, f_id), *start, *end))
                    }
                    "trim_left" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 0, "trim_left", src.0, src.1, *start, *end);
                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        output.push(Instr::CallFunc(7, id, f_id));
                        instr_src.push((Instr::CallFunc(7, id, f_id), *start, *end))
                    }
                    "trim_right" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 0, "trim_right", src.0, src.1, *start, *end);
                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);
                        output.push(Instr::CallFunc(8, id, f_id));
                        instr_src.push((Instr::CallFunc(8, id, f_id), *start, *end))
                    }
                    "trim_sequence_left" => {
                        check_obj_type!(&[DataType::String]);
                        check_args!(args, 1, "trim_sequence_left", src.0, src.1, *start, *end);

                        let infered = infer_type(&args[0], var_types, fns);
                        if infered != DataType::String {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );
                        output.push(Instr::CallFunc(9, id, f_id));
                        instr_src.push((Instr::CallFunc(9, id, f_id), *start, *end))
                    }
                    "trim_sequence_right" => {
                        check_args!(args, 1, "trim_sequence_right", src.0, src.1, *start, *end);

                        let infered = infer_type(&args[0], var_types, fns);
                        if infered != DataType::String {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );
                        output.push(Instr::CallFunc(10, id, f_id));
                        instr_src.push((Instr::CallFunc(10, id, f_id), *start, *end))
                    }
                    "rindex" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::String | DataType::Array(_)) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected String or Array, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        check_args!(args, 1, "rindex", src.0, src.1, *start, *end);

                        let arg_infered = infer_type(&args[0], var_types, fns);
                        if let DataType::Array(array_type) = &infered {
                            if **array_type != arg_infered {
                                parser_error!(
                                    src.0,
                                    src.1,
                                    args_indexes[0].0,
                                    args_indexes[0].1,
                                    "Invalid type",
                                    format_args!(
                                        "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                        format_datatype(*array_type.clone()),
                                        format_datatype(infered),
                                        format_datatype(arg_infered)
                                    )
                                );
                            }
                        } else if arg_infered != infered {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(arg_infered)
                                )
                            );
                        }

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );
                        output.push(Instr::CallFunc(11, id, f_id));
                        instr_src.push((Instr::CallFunc(11, id, f_id), *start, *end))
                    }
                    "repeat" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::String | DataType::Array(_)) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected String or Array, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        check_args!(args, 1, "repeat", src.0, src.1, *start, *end);

                        let arg_infered = infer_type(&args[0], var_types, fns);
                        if arg_infered != DataType::Number {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected Number, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(arg_infered)
                                )
                            );
                        }

                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        output.push(Instr::CallFunc(12, id, f_id));
                        instr_src.push((Instr::CallFunc(12, id, f_id), *start, *end))
                    }
                    "push" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::Array(_)) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected Array, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        check_args!(args, 1, "push", src.0, src.1, *start, *end);

                        let arg_infered = infer_type(&args[0], var_types, fns);
                        if let DataType::Array(array_type) = &infered {
                            if **array_type != arg_infered {
                                parser_error!(
                                    src.0,
                                    src.1,
                                    args_indexes[0].0,
                                    args_indexes[0].1,
                                    "Invalid type",
                                    format_args!(
                                        "Expected {} (because array has type {}), found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                        format_datatype(*array_type.clone()),
                                        format_datatype(infered),
                                        format_datatype(arg_infered)
                                    )
                                );
                            }
                        }

                        let arg_id = get_id(
                            &args[0],
                            v,
                            var_types,
                            consts,
                            &mut output,
                            fns,
                            arrs,
                            fn_state,
                            id,
                            src,
                            instr_src,
                        );

                        instr_src.push((Instr::Push(id, arg_id), *start, *end));
                        output.push(Instr::Push(id, arg_id));
                    }
                    "sqrt" => {
                        check_obj_type!(&[DataType::Number]);
                        check_args!(args, 0, "sqrt", src.0, src.1, *start, *end);

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        instr_src.push((Instr::Sqrt(id, f_id), *start, *end));
                        output.push(Instr::Sqrt(id, f_id));
                    }
                    "round" => {
                        check_obj_type!(&[DataType::Number]);
                        check_args!(args, 0, "round", src.0, src.1, *start, *end);

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        output.push(Instr::CallFunc(13, id, f_id));
                        instr_src.push((Instr::CallFunc(13, id, f_id), *start, *end))
                    }
                    "abs" => {
                        check_obj_type!(&[DataType::Number]);
                        check_args!(args, 0, "abs", src.0, src.1, *start, *end);

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        output.push(Instr::CallFunc(14, id, f_id));
                        instr_src.push((Instr::CallFunc(14, id, f_id), *start, *end))
                    }
                    // io::read
                    "read" => {
                        check_obj_type!(&[DataType::File]);
                        check_args!(args, 0, "read", src.0, src.1, *start, *end);

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        output.push(Instr::CallFunc(15, id, f_id));
                        instr_src.push((Instr::CallFunc(15, id, f_id), *start, *end))
                    }
                    // io::write
                    "write" => {
                        check_obj_type!(&[DataType::File]);
                        check_args_range!(args, 1, 2, "write", src.0, src.1, *start, *end);

                        let len = args.len();
                        add_args!(
                            args, v, var_types, consts, output, fns, arrs, fn_state, id, src,
                            instr_src
                        );
                        if len == 1 {
                            consts.push(Data::Bool(false));
                            output.push(Instr::StoreFuncArg((consts.len() - 1) as u16));
                        }

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        output.push(Instr::CallFunc(16, id, f_id));
                        instr_src.push((Instr::CallFunc(16, id, f_id), *start, *end))
                    }
                    "reverse" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::Array(_) | DataType::String) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected Array or String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        check_args!(args, 0, "reverse", src.0, src.1, *start, *end);

                        let f_id = consts.len() as u16;
                        consts.push(Data::Null);

                        output.push(Instr::CallFunc(17, id, f_id));
                        instr_src.push((Instr::CallFunc(17, id, f_id), *start, *end))
                    }
                    "split" => {
                        let infered = infer_type(obj, var_types, fns);
                        if !matches!(infered, DataType::Array(_) | DataType::String) {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Invalid type",
                                format_args!(
                                    "Expected Array or String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        check_args!(args, 1, "split", src.0, src.1, *start, *end);

                        let arg_infered = infer_type(&args[0], var_types, fns);
                        if let DataType::Array(array_type) = infered {
                            if *array_type != arg_infered {
                                parser_error!(
                                    src.0,
                                    src.1,
                                    args_indexes[0].0,
                                    args_indexes[0].1,
                                    "Invalid type",
                                    format_args!(
                                        "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                        format_datatype(*array_type),
                                        format_datatype(arg_infered)
                                    )
                                );
                            }
                        } else if infered != arg_infered {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected String, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(arg_infered)
                                )
                            );
                        }

                        let arg_id = get_id(
                            &args[0],
                            v,
                            var_types,
                            consts,
                            &mut output,
                            fns,
                            arrs,
                            fn_state,
                            id,
                            src,
                            instr_src,
                        );
                        consts.push(Data::Null);
                        instr_src.push((
                            Instr::Split(id, arg_id, (consts.len() - 1) as u16),
                            *start,
                            *end,
                        ));
                        output.push(Instr::Split(id, arg_id, (consts.len() - 1) as u16));
                    }
                    "remove" => {
                        check_args!(args, 1, "remove", src.0, src.1, *start, *end);

                        let infered = infer_type(&args[0], var_types, fns);
                        if infered != DataType::Number {
                            parser_error!(
                                src.0,
                                src.1,
                                args_indexes[0].0,
                                args_indexes[0].1,
                                "Invalid type",
                                format_args!(
                                    "Expected Number, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                    format_datatype(infered)
                                )
                            );
                        }

                        let arg_id = get_id(
                            &args[0],
                            v,
                            var_types,
                            consts,
                            &mut output,
                            fns,
                            arrs,
                            fn_state,
                            id,
                            src,
                            instr_src,
                        );
                        instr_src.push((Instr::Remove(id, arg_id), *start, *end));
                        output.push(Instr::Remove(id, arg_id));
                    }
                    _ => {
                        parser_error!(
                            src.0,
                            src.1,
                            *start,
                            *end,
                            "Unknown function",
                            format_args!(
                                "Function {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} does not exist"
                            )
                        );
                    }
                }
            }
            Expr::FunctionDecl(x, y, start, end) => {
                if fns
                    .iter()
                    .any(|(name, _, _, _, _)| **name == *x.first().unwrap())
                {
                    parser_error!(
                        src.0,
                        src.1,
                        *start,
                        *end,
                        "Function defined twice",
                        format_args!(
                            "Function {color_bright_blue}{style_bold}{}{color_reset}{style_reset} is already defined",
                            x[0]
                        )
                    );
                }
                fns.push((
                    x.first().unwrap().to_string(),
                    x.into_iter().skip(1).map(ToString::to_string).collect(),
                    y.clone(),
                    None,
                    None,
                ));
            }
            Expr::ReturnVal(val) => {}
            Expr::Break => output.push(Instr::Break(id)),
            Expr::Continue => output.push(Instr::Continue(id)),
            Expr::EvalBlock(code) => {
                let mut vars = v.clone();
                output.extend(parser_to_instr_set(
                    code, &mut vars, var_types, consts, fns, fn_state, arrs, id, src, instr_src,
                ))
            }
            other => {
                unreachable!("Not implemented {:?}", other);
            }
        }
    }
    output
}

pub fn parse(
    contents: &str,
    filename: &str,
) -> (
    Vec<Instr>,
    Vec<Data>,
    Slab<Vec<Data>>,
    Vec<(Instr, usize, usize)>,
) {
    let now = std::time::Instant::now();
    let functions: Vec<Expr> = grammar::FileParser::new()
        .parse(contents)
        .unwrap_or_else(|x| {
            lalrpop_error::<usize, Token<'_>, &str>(x, contents, filename);
            std::process::exit(1);
        });
    println!("LALRPOP TIME {:.2?}", now.elapsed());
    // println!("FUNCS {functions:?}");
    let mut functions: Vec<Function> = functions
        .into_iter()
        .map(|w| {
            if let Expr::FunctionDecl(x, y, _, _) = w {
                (x[0].to_string(), x[1..].into(), y, None, None)
            } else {
                unreachable!()
            }
        })
        .collect();

    // print!("{functions:?}");

    let mut variables: Vec<(Intern<String>, u16)> = Vec::new();
    let mut consts: Vec<Data> = Vec::new();
    let mut arrays: Slab<Vec<Data>> = Slab::with_capacity(20);
    let mut instr_src = Vec::new();
    let mut var_types: Vec<(Intern<String>, DataType)> = Vec::new();
    let instructions = parser_to_instr_set(
        functions
            .iter()
            .find(|x| x.0 == "main")
            .unwrap_or_else(|| {
                error!("Could not find main function");
            })
            .2
            .to_vec()
            .as_slice(),
        &mut variables,
        &mut var_types,
        &mut consts,
        &mut functions,
        None,
        &mut arrays,
        0,
        (filename, contents),
        &mut instr_src,
    );
    // print!("CONSTS are {consts:?}");
    // print!("{consts:?}");
    // print!("{arrays:?}");
    #[cfg(debug_assertions)]
    {
        print_instructions(&instructions);
    }
    (instructions, consts, arrays, instr_src)
}
