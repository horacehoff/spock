use crate::ArrayStorage;
use crate::debug;
use crate::display::op_error;
use crate::display::parser_error;
use crate::display::{lalrpop_error, print_debug};
use crate::functions::handle_functions;
use crate::grammar::Token;
use crate::method_calls::handle_method_calls;
use crate::optimizations::{for_loop_summation, while_loop_summation};
use crate::type_inference;
use crate::type_inference::contains_recursive_call;
use crate::type_inference::{DataType, infer_type};
use crate::types::is_indexable;
use crate::util::compilation_error;
use crate::{Data, Instr, error};
use inline_colorization::*;
use internment::Intern;
use lalrpop_util::lalrpop_mod;
use slab::Slab;
use std::slice;

lalrpop_mod!(pub grammar);

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub enum Expr {
    Float(f64),
    Int(i32),
    Bool(bool),
    String(String),
    /// Var(name, start, end)
    Var(Intern<String>, usize, usize),
    /// Array(contents, start, end)
    Array(Box<[Expr]>, usize, usize),
    /// VarDeclare(name, value),
    VarDeclare(Intern<String>, Box<Expr>),
    /// VarDeclare(name, value, start, end)
    VarAssign(Intern<String>, Box<Expr>, usize, usize),
    /// Condition(condition, code (contains else_if_blocks and potentially else_block), start, end)
    Condition(Box<Expr>, Box<[Expr]>, usize, usize),
    ElseIfBlock(Box<Expr>, Box<[Expr]>),
    ElseBlock(Box<[Expr]>),

    WhileBlock(Box<Expr>, Box<[Expr]>),
    /// FunctionCall(args, (optional namespace + name), start, end, (arg_start,arg_end))
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
    /// FunctionDecl(name+args, code, start, end)
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

    /// ForLoop(loop_var_name, loop_array+code)
    ForLoop(Intern<String>, Box<[Expr]>),
    /// IntForLoop(loop_var_name, first_elem, final_elem, code)
    IntForLoop(
        Intern<String>,
        Box<Expr>,
        Box<Expr>,
        Box<[Expr]>,
        usize,
        usize,
        usize,
        usize,
    ),
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

#[cold]
#[inline(never)]
pub fn symbol_of_expr(expr: &Expr) -> &str {
    match expr {
        Expr::Mul(_, _, _, _) => "*",
        Expr::Div(_, _, _, _) => "/",
        Expr::Add(_, _, _, _) => "+",
        Expr::Sub(_, _, _, _) => "-",
        Expr::Mod(_, _, _, _) => "%",
        Expr::Pow(_, _, _, _) => "^",
        Expr::Eq(_, _) => "==",
        Expr::NotEq(_, _) => "!=",
        Expr::Sup(_, _, _, _) => ">",
        Expr::SupEq(_, _, _, _) => ">=",
        Expr::Inf(_, _, _, _) => "<",
        Expr::InfEq(_, _, _, _) => "<=",
        Expr::BoolAnd(_, _, _, _) => "&&",
        Expr::BoolOr(_, _, _, _) => "||",
        Expr::Neg(_, _, _) => "-",
        other => compilation_error(
            "parser.rs",
            "symbol_of_expr",
            format_args!("Tried to get symbol of {other:?}"),
        ),
    }
}

pub fn move_to_id(x: &mut [Instr], tgt_id: u16) {
    if x.is_empty()
        || matches!(
            x.last().unwrap(),
            Instr::ArrayMov(_, _, _) | Instr::IoDelete(_)
        )
    {
        return;
    }
    let matching_elem_index = x
        .iter()
        .rposition(|w| get_tgt_id(*w).is_some())
        .unwrap_or(x.len() - 1);
    let matching_elem = x.get_mut(matching_elem_index).unwrap();
    match matching_elem{
        Instr::Mov(_, y)
        | Instr::CallFunc(_, y)
        | Instr::AddFloat(_, _, y)
        | Instr::AddInt(_, _, y)
        | Instr::AddArray(_, _, y)
        | Instr::AddStr(_, _, y)
        | Instr::MulFloat(_, _, y)
        | Instr::MulInt(_, _, y)
        | Instr::SubFloat(_, _, y)
        | Instr::SubInt(_, _, y)
        | Instr::DivFloat(_, _, y)
        | Instr::DivInt(_, _, y)
        | Instr::ModFloat(_, _, y)
        | Instr::ModInt(_, _, y)
        | Instr::PowFloat(_, _, y)
        | Instr::PowInt(_, _, y)
        | Instr::Eq(_, _, y)
        | Instr::ArrayEq(_, _, y)
        | Instr::NotEq(_, _, y)
        | Instr::ArrayNotEq(_, _, y)
        | Instr::SupFloat(_, _, y)
        | Instr::SupInt(_, _, y)
        | Instr::SupEqFloat(_, _, y)
        | Instr::SupEqInt(_, _, y)
        | Instr::InfFloat(_, _, y)
        | Instr::InfInt(_, _, y)
        | Instr::InfEqFloat(_, _, y)
        | Instr::InfEqInt(_, _, y)
        | Instr::BoolAnd(_, _, y)
        | Instr::BoolOr(_, _, y)
        | Instr::NegFloat(_, y)
        | Instr::NegInt(_, y)
        | Instr::Float(_, y)
        | Instr::Bool(_, y)
        | Instr::CallLibFunc(_, _, y)
        | Instr::Input(_, y)
        | Instr::ArrayGet(_, _, y)
        | Instr::ArrayStrGet(_, _, y)
        | Instr::Range(_, _, y)
        // | Instr::Type(_, y)
        | Instr::IoOpen(_, y, _)
        | Instr::Floor(_, y)
        | Instr::TheAnswer(y)
        | Instr::Len(_, y)
        | Instr::SqrtFloat(_, y)
        | Instr::Split(_, _, y)
        | Instr::SaveFrame(_, y, _)
        | Instr::Str(_, y) => *y = tgt_id,
        Instr::CallFuncRecursive(_, y) => {
            *y = tgt_id;
            for i in 1..x.len() - 1 {
                if let Some(Instr::SaveFrame(_, y, _)) = x.get_mut(matching_elem_index - i) {
                    *y = tgt_id;
                    break;
                }
            }
        }
        other => compilation_error("parser.rs","move_to_id", format_args!("Tried to move {other:?} to tgt_id={tgt_id}")),
    }
}

fn get_tgt_id(x: Instr) -> Option<u16> {
    match x {
        Instr::Mov(_, y)
        | Instr::CallFunc(_, y)
        | Instr::CallFuncRecursive(_, y)
        | Instr::SaveFrame(_, y, _)
        | Instr::AddFloat(_, _, y)
        | Instr::AddInt(_, _, y)
        | Instr::AddArray(_, _, y)
        | Instr::AddStr(_, _, y)
        | Instr::MulFloat(_, _, y)
        | Instr::MulInt(_, _, y)
        | Instr::SubFloat(_, _, y)
        | Instr::SubInt(_, _, y)
        | Instr::DivFloat(_, _, y)
        | Instr::DivInt(_, _, y)
        | Instr::ModFloat(_, _, y)
        | Instr::ModInt(_, _, y)
        | Instr::PowFloat(_, _, y)
        | Instr::PowInt(_, _, y)
        | Instr::Eq(_, _, y)
        | Instr::ArrayEq(_, _, y)
        | Instr::NotEq(_, _, y)
        | Instr::ArrayNotEq(_, _, y)
        | Instr::SupFloat(_, _, y)
        | Instr::SupInt(_, _, y)
        | Instr::SupEqFloat(_, _, y)
        | Instr::SupEqInt(_, _, y)
        | Instr::InfFloat(_, _, y)
        | Instr::InfInt(_, _, y)
        | Instr::InfEqFloat(_, _, y)
        | Instr::InfEqInt(_, _, y)
        | Instr::BoolAnd(_, _, y)
        | Instr::BoolOr(_, _, y)
        | Instr::NegFloat(_, y)
        | Instr::NegInt(_, y)
        | Instr::Float(_, y)
        | Instr::Bool(_, y)
        | Instr::CallLibFunc(_, _, y)
        | Instr::Input(_, y)
        | Instr::ArrayGet(_, _, y)
        | Instr::ArrayStrGet(_, _, y)
        | Instr::Range(_, _, y)
        | Instr::IoOpen(_, y, _)
        | Instr::Floor(_, y)
        | Instr::TheAnswer(y)
        | Instr::Len(_, y)
        | Instr::SqrtFloat(_, y)
        | Instr::Split(_, _, y)
        | Instr::Str(_, y) => Some(y),
        _ => None,
    }
}

pub fn get_tgt_ids(x: &[Instr]) -> Vec<u16> {
    let mut ids: Vec<u16> = x.iter().filter_map(|i| get_tgt_id(*i)).collect();
    ids.sort_unstable();
    ids.dedup();
    ids
}

fn get_tgt_id_vec(x: &[Instr]) -> Option<u16> {
    debug_assert!(
        !(x.is_empty()
            || matches!(
                x.last().unwrap(),
                Instr::ArrayMov(_, _, _) | Instr::IoDelete(_)
            ))
    );
    for y in x.iter().rev() {
        if let Some(id) = get_tgt_id(*y) {
            return Some(id);
        }
    }
    None
}

pub fn get_id(
    input: &Expr,
    v: &mut Vec<(Intern<String>, u16, DataType)>,
    (
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
    output: &mut Vec<Instr>,
) -> u16 {
    macro_rules! parser_data {
        () => {
            (
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

    macro_rules! uniform_op {
        ($instr: ident,$symbol:expr, $l: expr, $r: expr, $start: expr, $end: expr, $type:expr) => {{
            let (t_l, t_r) = (infer_type($l, v, fns, src), infer_type($r, v, fns, src));
            if t_l != $type || t_r != $type {
                op_error(src, t_l, t_r, $symbol, *$start, *$end);
            }
            let id_l = get_id($l, v, parser_data!(), output);
            let id_r = get_id($r, v, parser_data!(), output);
            let id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(Instr::$instr(id_l, id_r, id));
            id
        }};
        ($instr: ident, $instr2:ident,$symbol:expr, $l: expr, $r: expr, $start: expr, $end: expr, $type1:expr, $type2:expr) => {{
            let (t_l, t_r) = (infer_type($l, v, fns, src), infer_type($r, v, fns, src));
            if !((t_l == $type1 && t_r == $type1) || (t_l == $type2 && t_r == $type2)) {
                op_error(src, t_l, t_r, $symbol, *$start, *$end);
            }
            let id_l = get_id($l, v, parser_data!(), output);
            let id_r = get_id($r, v, parser_data!(), output);
            let id = registers.len() as u16;
            registers.push(Data::NULL);
            output.push(if t_l == $type1 {
                Instr::$instr(id_l, id_r, id)
            } else {
                Instr::$instr2(id_l, id_r, id)
            });
            id
        }};
    }
    match input {
        Expr::Float(num) => {
            registers.push((*num).into());
            (registers.len() - 1) as u16
        }
        Expr::String(str) => {
            registers.push(str.as_str().into());
            (registers.len() - 1) as u16
        }
        Expr::Bool(bool) => {
            registers.push((*bool).into());
            (registers.len() - 1) as u16
        }
        Expr::Var(name, start, end) => {
            if let Some((_, id, _)) = v.iter().find(|(var, _, _)| *name == *var) {
                *id
            } else {
                parser_error(
                    src,
                    *start,
                    *end,
                    "Unknown variable",
                    &format!(
                        "Variable {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} has not been declared yet"
                    ),
                    &format!("Declare it with {color_green}let {name} = 0;{color_reset}"),
                )
            }
        }
        Expr::Array(elems, start, end) => {
            let first_type = infer_type(&elems[0], v, fns, src);
            if !elems
                .iter()
                .all(|x| infer_type(x, v, fns, src) == first_type)
            {
                parser_error(
                    src,
                    *start,
                    *end,
                    "Array",
                    "Arrays can only hold one type of value",
                    "",
                );
            }
            let array_id = arrays.insert(Vec::new());
            for elem in elems {
                let x = parser_to_instr_set(slice::from_ref(elem), v, parser_data!());
                if !x.is_empty() {
                    let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                    arrays.get_mut(array_id).unwrap().push(Data::NULL);

                    output.extend(x);
                    output.push(Instr::ArrayMov(
                        c_id,
                        block_id,
                        (arrays[array_id].len() - 1) as u16,
                    ));
                } else {
                    arrays
                        .get_mut(array_id)
                        .unwrap()
                        .push(registers.pop().unwrap());
                }
            }
            registers.push(Data::array(array_id as u32));
            (registers.len() - 1) as u16
        }
        Expr::Mul(l, r, start, end) => {
            uniform_op!(
                MulFloat,
                MulInt,
                "*",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Div(l, r, start, end) => {
            uniform_op!(
                DivFloat,
                MulInt,
                "/",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Add(l, r, start, end) => {
            let t_l = infer_type(l, v, fns, src);
            let t_r = infer_type(r, v, fns, src);
            if t_l != t_r
                || !matches!(
                    t_l,
                    DataType::String | DataType::Array(_) | DataType::Float | DataType::Int
                )
            {
                op_error(src, t_l, t_r, "+", *start, *end);
            }
            let id_l = get_id(l, v, parser_data!(), output);
            let id_r = get_id(r, v, parser_data!(), output);
            let id = registers.len() as u16;
            registers.push(Data::NULL);
            if matches!(t_l, DataType::Array(_)) {
                output.push(Instr::AddArray(id_l, id_r, id));
            } else if t_l == DataType::String {
                output.push(Instr::AddStr(id_l, id_r, id));
            } else if t_l == DataType::Float {
                output.push(Instr::AddFloat(id_l, id_r, id));
            } else if t_l == DataType::Int {
                output.push(Instr::AddInt(id_l, id_r, id));
            }
            id
        }
        Expr::Sub(l, r, start, end) => {
            uniform_op!(
                SubFloat,
                SubInt,
                "-",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Mod(l, r, start, end) => {
            uniform_op!(
                ModFloat,
                ModInt,
                "%",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Pow(l, r, start, end) => {
            uniform_op!(
                PowFloat,
                PowInt,
                "^",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Eq(l, r) => {
            let id_l = get_id(l, v, parser_data!(), output);
            let id_r = get_id(r, v, parser_data!(), output);
            let id = registers.len() as u16;
            registers.push(Data::NULL);
            if matches!(infer_type(l, v, fns, src), DataType::Array(_))
                && matches!(infer_type(r, v, fns, src), DataType::Array(_))
            {
                output.push(Instr::ArrayEq(id_l, id_r, id));
            } else {
                output.push(Instr::Eq(id_l, id_r, id));
            }
            id
        }
        Expr::NotEq(l, r) => {
            let id_l = get_id(l, v, parser_data!(), output);
            let id_r = get_id(r, v, parser_data!(), output);
            let id = registers.len() as u16;
            registers.push(Data::NULL);
            if matches!(infer_type(l, v, fns, src), DataType::Array(_))
                && matches!(infer_type(r, v, fns, src), DataType::Array(_))
            {
                output.push(Instr::ArrayNotEq(id_l, id_r, id));
            } else {
                output.push(Instr::NotEq(id_l, id_r, id));
            }
            id
        }
        Expr::Sup(l, r, start, end) => {
            uniform_op!(
                SupFloat,
                SupInt,
                ">",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::SupEq(l, r, start, end) => {
            uniform_op!(
                SupEqFloat,
                SupEqInt,
                ">=",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Inf(l, r, start, end) => {
            uniform_op!(
                InfFloat,
                InfInt,
                "<",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::InfEq(l, r, start, end) => {
            uniform_op!(
                InfEqFloat,
                InfEqInt,
                "<=",
                l,
                r,
                start,
                end,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::BoolAnd(l, r, start, end) => {
            uniform_op!(BoolAnd, "&&", l, r, start, end, DataType::Bool)
        }
        Expr::BoolOr(l, r, start, end) => {
            uniform_op!(BoolOr, "||", l, r, start, end, DataType::Bool)
        }
        Expr::Neg(l, start, end) => {
            let infered = infer_type(l, v, fns, src);
            let id_l = get_id(l, v, parser_data!(), output);
            let id = registers.len() as u16;
            registers.push(Data::NULL);
            if infered == DataType::Float {
                output.push(Instr::NegFloat(id_l, id))
            } else if infered == DataType::Int {
                output.push(Instr::NegInt(id_l, id))
            } else {
                parser_error(
                    src,
                    *start,
                    *end,
                    "Invalid operation",
                    &format!(
                        "Cannot negate {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                        infered,
                    ),
                    "",
                );
            }
            id
        }

        Expr::Condition(main_condition, code, start, end) => {
            let return_id = registers.len() as u16;
            registers.push(Data::NULL);

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
            let condition_id = get_id(main_condition, v, parser_data!(), output);
            add_cmp(condition_id, &mut 0, output, false);
            cmp_markers.push(output.len() - 1);

            let mut priv_vars = v.clone();
            // parse the main code block
            let cond_code =
                parser_to_instr_set(&code[0..main_code_limit], &mut priv_vars, parser_data!());
            let is_empty = cond_code.is_empty();
            output.extend(cond_code);
            output.push(Instr::Mov(
                if is_empty {
                    (registers.len() - 1) as u16
                } else {
                    get_tgt_id_vec(output).unwrap()
                },
                return_id,
            ));
            if main_code_limit != code.len() {
                output.push(Instr::Jmp(0));
                jmp_markers.push(output.len() - 1);
            }

            let mut else_exists = false;
            for elem in &code[main_code_limit..] {
                if let Expr::ElseIfBlock(condition, code) = elem {
                    condition_markers.push(output.len());
                    let condition_id = get_id(condition, v, parser_data!(), output);
                    add_cmp(condition_id, &mut 0, output, false);
                    cmp_markers.push(output.len() - 1);
                    let mut priv_vars = v.clone();
                    let cond_code = parser_to_instr_set(code, &mut priv_vars, parser_data!());
                    let is_empty = cond_code.is_empty();
                    output.extend(cond_code);
                    output.push(Instr::Mov(
                        if is_empty {
                            (registers.len() - 1) as u16
                        } else {
                            get_tgt_id_vec(output).unwrap()
                        },
                        return_id,
                    ));
                    output.push(Instr::Jmp(0));
                    jmp_markers.push(output.len() - 1);
                } else if let Expr::ElseBlock(code) = elem {
                    else_exists = true;
                    condition_markers.push(output.len());
                    let mut priv_vars = v.clone();
                    let cond_code = parser_to_instr_set(code, &mut priv_vars, parser_data!());
                    let is_empty = cond_code.is_empty();
                    output.extend(cond_code);
                    output.push(Instr::Mov(
                        if is_empty {
                            (registers.len() - 1) as u16
                        } else {
                            get_tgt_id_vec(output).unwrap()
                        },
                        return_id,
                    ));
                }
            }
            if !else_exists {
                parser_error(
                    src,
                    *start,
                    *end,
                    "Invalid condition",
                    "Inline conditions need an else statement",
                    "",
                );
            }

            for y in jmp_markers {
                let diff = output.len() - y;
                output[y] = Instr::Jmp(diff as u16);
            }
            for (i, y) in cmp_markers.iter().enumerate() {
                let diff = if i >= condition_markers.len() {
                    output.len() - 1 - y
                } else {
                    condition_markers[i] - y
                };
                if let Some(
                    Instr::Cmp(_, jump_size)
                    | Instr::InfFloatCmp(_, _, jump_size)
                    | Instr::InfIntCmp(_, _, jump_size)
                    | Instr::InfEqFloatCmp(_, _, jump_size)
                    | Instr::InfEqIntCmp(_, _, jump_size)
                    | Instr::SupFloatCmp(_, _, jump_size)
                    | Instr::SupIntCmp(_, _, jump_size)
                    | Instr::SupEqFloatCmp(_, _, jump_size)
                    | Instr::SupEqIntCmp(_, _, jump_size)
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
        Expr::FunctionCall(args, namespace, start, end, args_indexes) => handle_functions(
            output,
            v,
            parser_data!(),
            args,
            namespace,
            *start,
            *end,
            args_indexes,
        )
        .unwrap_or_else(|| (registers.len() - 1) as u16),
        other => {
            let output_code = parser_to_instr_set(slice::from_ref(other), v, parser_data!());
            if !output_code.is_empty() {
                output.extend(output_code);
                get_tgt_id_vec(output).unwrap_or((registers.len() - 1) as u16)
            } else {
                (registers.len() - 1) as u16
            }
        }
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
    *output.last_mut().unwrap() = match *output.last().unwrap() {
        Instr::InfFloat(o1, o2, o3) if o3 == condition_id => Instr::InfFloatCmp(o1, o2, *len),
        Instr::InfInt(o1, o2, o3) if o3 == condition_id => Instr::InfIntCmp(o1, o2, *len),
        Instr::InfEqFloat(o1, o2, o3) if o3 == condition_id => Instr::InfEqFloatCmp(o1, o2, *len),
        Instr::InfEqInt(o1, o2, o3) if o3 == condition_id => Instr::InfEqIntCmp(o1, o2, *len),
        Instr::SupFloat(o1, o2, o3) if o3 == condition_id => Instr::SupFloatCmp(o1, o2, *len),
        Instr::SupInt(o1, o2, o3) if o3 == condition_id => Instr::SupIntCmp(o1, o2, *len),
        Instr::SupEqFloat(o1, o2, o3) if o3 == condition_id => Instr::SupEqFloatCmp(o1, o2, *len),
        Instr::SupEqInt(o1, o2, o3) if o3 == condition_id => Instr::SupEqIntCmp(o1, o2, *len),
        Instr::Eq(o1, o2, o3) if o3 == condition_id => Instr::EqCmp(o1, o2, *len),
        Instr::ArrayEq(o1, o2, o3) if o3 == condition_id => Instr::ArrayEqCmp(o1, o2, *len),
        Instr::NotEq(o1, o2, o3) if o3 == condition_id => Instr::NotEqCmp(o1, o2, *len),
        Instr::ArrayNotEq(o1, o2, o3) if o3 == condition_id => Instr::ArrayNotEqCmp(o1, o2, *len),
        _ => {
            output.push(Instr::Cmp(condition_id, *len));
            return;
        }
    };
    if jmp_backwards {
        *len -= 1;
    }
}

fn parse_loop_flow_control(
    loop_code: &mut [Instr],
    loop_id: u16,
    code_length: u16,
    for_loop: bool,
) {
    loop_code.iter_mut().enumerate().for_each(|x| {
        if let Instr::EqCmp(break_id, 0, 0) = x.1
            && *break_id == loop_id
        {
            if for_loop {
                *x.1 = Instr::Jmp(code_length - x.0 as u16 - 1);
            } else {
                *x.1 = Instr::Jmp(code_length - x.0 as u16);
            }
        } else if let Instr::NotEqCmp(continue_id, 0, 0) = x.1
            && *continue_id == loop_id
        {
            if for_loop {
                *x.1 = Instr::Jmp(code_length - x.0 as u16 - 3);
            } else {
                *x.1 = Instr::Jmp(code_length - x.0 as u16 - 1);
            }
        }
    });
}

fn parse_indef_loop_flow_control(loop_code: &mut [Instr], loop_id: u16, code_length: u16) {
    loop_code.iter_mut().enumerate().for_each(|(i, x)| {
        if let Instr::EqCmp(break_id, 0, 0) = x
            && *break_id == loop_id
        {
            *x = Instr::Jmp(code_length - i as u16);
        } else if let Instr::NotEqCmp(continue_id, 0, 0) = x
            && *continue_id == loop_id
        {
            *x = Instr::Jmp(code_length - i as u16 - 3);
        }
    });
}

pub type Function = (
    // name
    String,
    // args
    Box<[String]>,
    // code
    Box<[Expr]>,
    // fn loc, fn args loc, argument types
    Vec<(u16, Vec<u16>, Vec<DataType>)>,
    // is_recursive
    bool,
    // id
    u16,
);

pub type ParserData<'a> = (
    // registers
    &'a mut Vec<Data>,
    // functions
    &'a mut Vec<Function>,
    // arrays
    &'a mut ArrayStorage,
    // block_id
    u16,
    // src => (filename, contents)
    (&'a str, &'a str),
    // instr_src
    &'a mut Vec<(Instr, usize, usize)>,
    // is_parsing_recursive
    bool,
    // fn_registers
    &'a mut Vec<Vec<u16>>,
    // current function id,
    Option<u16>,
);

#[inline(always)]
pub fn parser_to_instr_set(
    input: &[Expr],
    // variables
    v: &mut Vec<(Intern<String>, u16, DataType)>,
    (
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
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::with_capacity(input.len());

    macro_rules! parser_data {
        () => {
            (
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

    for x in input {
        match x {
            // if number / bool / str, just push it to the registers, and the caller will grab the last index
            Expr::Float(num) => registers.push((*num).into()),
            Expr::Int(num) => registers.push((*num).into()),
            Expr::Bool(bool) => registers.push((*bool).into()),
            Expr::String(str) => registers.push(str.as_str().into()),
            Expr::Var(name, start, end) => {
                if let Some((_, id, _)) = v.iter().find(|(var, _, _)| *name == *var) {
                    registers.push(Data::NULL);
                    output.push(Instr::Mov(*id, (registers.len() - 1) as u16));
                } else {
                    parser_error(
                        src,
                        *start,
                        *end,
                        "Unknown variable",
                        &format!(
                            "Variable {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} has not been declared yet"
                        ),
                        &format!("Declare it with {color_green}let {name} = 0;{color_reset}"),
                    );
                }
            }
            Expr::Array(elems, start, end) => {
                let first_type = infer_type(&elems[0], v, fns, src);
                if !elems
                    .iter()
                    .all(|x| infer_type(x, v, fns, src) == first_type)
                {
                    parser_error(
                        src,
                        *start,
                        *end,
                        "Array",
                        "Arrays can only hold one type of value",
                        "",
                    );
                }
                // create new blank array with latest id
                let array_id = arrays.insert(Vec::new());
                for elem in elems {
                    // process each array element
                    let x = parser_to_instr_set(slice::from_ref(elem), v, parser_data!());
                    // if there are no instructions, then that means the element has been pushed to the registers, so pop it and push it directly to the array
                    if x.is_empty() {
                        arrays
                            .get_mut(array_id)
                            .unwrap()
                            .push(registers.pop().unwrap());
                    } else {
                        // if there are instructions, then push everything, add a null to the array, and then add an instruction to move the element to the array at runtime with ArrayMov
                        let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                        output.extend(x);
                        arrays.get_mut(array_id).unwrap().push(Data::NULL);
                        output.push(Instr::ArrayMov(
                            c_id,
                            block_id,
                            (arrays[array_id].len() - 1) as u16,
                        ));
                    }
                }
                registers.push(Data::array(array_id as u32));
            }
            // array[index]
            Expr::GetIndex(target, index, start, end) => {
                let mut infered = infer_type(target, v, fns, src);
                // process the array/string that is being indexed
                let mut id = get_id(target, v, parser_data!(), &mut output);
                // for each index operation, process the index, adjust the id variable for the next index operation, push null to registers to use GetIndex to index at runtime
                for elem in index {
                    if !is_indexable(&infered) {
                        parser_error(
                            src,
                            *start,
                            *end,
                            "Invalid type",
                            &format!(
                                "Cannot index {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                                infered,
                            ),
                            "",
                        );
                    }

                    let index_infered = infer_type(elem, v, fns, src);
                    if index_infered != DataType::Int {
                        parser_error(
                            src,
                            *start,
                            *end,
                            "Invalid type",
                            &format!(
                                "{color_bright_blue}{style_bold}{}{color_reset}{style_reset} is not a valid index",
                                index_infered
                            ),
                            "",
                        );
                    }
                    let f_id = get_id(elem, v, parser_data!(), &mut output);
                    registers.push(Data::NULL);
                    if infered == DataType::String {
                        instr_src.push((
                            Instr::ArrayStrGet(id, f_id, (registers.len() - 1) as u16),
                            *start,
                            *end,
                        ));
                        output.push(Instr::ArrayStrGet(id, f_id, (registers.len() - 1) as u16));
                    } else {
                        instr_src.push((
                            Instr::ArrayGet(id, f_id, (registers.len() - 1) as u16),
                            *start,
                            *end,
                        ));
                        output.push(Instr::ArrayGet(id, f_id, (registers.len() - 1) as u16));
                    }
                    id = (registers.len() - 1) as u16;
                    if let DataType::Array(array_type) = infered {
                        infered = *array_type;
                    }
                }
            }
            // x[y]... = z;
            Expr::ArrayModify(x, z, w, index_start, index_end, elem_start, elem_end) => {
                let infered = infer_type(x, v, fns, src);
                if !is_indexable(&infered) {
                    parser_error(
                        src,
                        *index_start,
                        *index_end,
                        "Invalid type",
                        &format!(
                            "Cannot index {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            infered
                        ),
                        "",
                    );
                }
                // get the id of the target array
                let mut id = get_id(x, v, parser_data!(), &mut output);

                for elem in z.iter().rev().skip(1).rev() {
                    let infered = infer_type(elem, v, fns, src);
                    if infered != DataType::Int {
                        parser_error(
                            src,
                            *index_start,
                            *index_end,
                            "Invalid type",
                            &format!(
                                "{color_bright_blue}{style_bold}{}{color_reset}{style_reset} is not a valid index",
                                infered,
                            ),
                            "",
                        );
                    }
                    let f_id = get_id(elem, v, parser_data!(), &mut output);

                    registers.push(Data::NULL);
                    output.push(Instr::ArrayGet(id, f_id, (registers.len() - 1) as u16));
                    id = (registers.len() - 1) as u16;
                }

                // get the
                let final_id = get_id(z.last().unwrap(), v, parser_data!(), &mut output);

                let elem_type = infer_type(w, v, fns, src);
                let elem_id = get_id(w, v, parser_data!(), &mut output);

                if let DataType::Array(array_type) = &infered {
                    if **array_type != elem_type {
                        parser_error(
                            src,
                            *elem_start,
                            *elem_end,
                            "Invalid type",
                            &format!(
                                "Cannot insert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} in {}",
                                elem_type, infered,
                            ),
                            "",
                        );
                    }
                } else if infered == DataType::String && elem_type != DataType::String {
                    parser_error(
                        src,
                        *elem_start,
                        *elem_end,
                        "Invalid type",
                        &format!(
                            "Cannot insert {color_bright_blue}{style_bold}{}{color_reset}{style_reset} in String",
                            elem_type,
                        ),
                        "",
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
                let condition_id = get_id(main_condition, v, parser_data!(), &mut output);
                add_cmp(condition_id, &mut 0, &mut output, false);
                cmp_markers.push(output.len() - 1);
                let mut priv_vars = v.clone();
                // parse the main code block
                let cond_code =
                    parser_to_instr_set(&code[0..main_code_limit], &mut priv_vars, parser_data!());
                output.extend(cond_code);
                if main_code_limit != code.len() {
                    output.push(Instr::Jmp(0));
                    jmp_markers.push(output.len() - 1);
                }

                for elem in &code[main_code_limit..] {
                    if let Expr::ElseIfBlock(condition, code) = elem {
                        condition_markers.push(output.len());
                        let condition_id = get_id(condition, v, parser_data!(), &mut output);
                        add_cmp(condition_id, &mut 0, &mut output, false);
                        cmp_markers.push(output.len() - 1);
                        let mut priv_vars = v.clone();
                        let cond_code = parser_to_instr_set(code, &mut priv_vars, parser_data!());
                        debug!("COND CODE IS {cond_code:?}");
                        output.extend(cond_code);
                        output.push(Instr::Jmp(0));
                        jmp_markers.push(output.len() - 1);
                    } else if let Expr::ElseBlock(code) = elem {
                        condition_markers.push(output.len());
                        let mut priv_vars = v.clone();
                        let cond_code = parser_to_instr_set(code, &mut priv_vars, parser_data!());
                        debug!("COND CODE IS {cond_code:?}");
                        output.extend(cond_code);
                    }
                }

                for y in jmp_markers {
                    let diff = output.len() - y;
                    output[y] = Instr::Jmp(diff as u16);
                }
                for (i, y) in cmp_markers.iter().enumerate() {
                    let diff = if i >= condition_markers.len() {
                        output.len() - 1 - y
                    } else {
                        condition_markers[i] - y
                    };
                    if let Some(
                        Instr::Cmp(_, jump_size)
                        | Instr::InfFloatCmp(_, _, jump_size)
                        | Instr::InfIntCmp(_, _, jump_size)
                        | Instr::InfEqFloatCmp(_, _, jump_size)
                        | Instr::InfEqIntCmp(_, _, jump_size)
                        | Instr::SupFloatCmp(_, _, jump_size)
                        | Instr::SupIntCmp(_, _, jump_size)
                        | Instr::SupEqFloatCmp(_, _, jump_size)
                        | Instr::SupEqIntCmp(_, _, jump_size)
                        | Instr::EqCmp(_, _, jump_size)
                        | Instr::NotEqCmp(_, _, jump_size),
                    ) = output.get_mut(*y)
                    {
                        *jump_size = diff as u16;
                        // *jump_size = diff as u16 + 1;
                    }
                }
            }
            Expr::WhileBlock(condition, code) => {
                // try to optimize it (if it's a summation loop)
                if while_loop_summation(&mut output, v, parser_data!(), condition, code) {
                    continue;
                }

                // parse the condition, get its id
                let condition_id = get_id(condition, v, parser_data!(), &mut output);

                // parse the code block, clone the vars to avoid overriding anything
                let mut temp_vars = v.clone();

                let loop_id = block_id + 1;
                let mut cond_code = parser_to_instr_set(code, &mut temp_vars, parser_data!());

                // get length of the code, then add Cmp/OpCmp (decided by add_cmp), and add the condition logic
                let mut len = (cond_code.len() + 2) as u16;
                add_cmp(condition_id, &mut len, &mut output, true);
                parse_loop_flow_control(&mut cond_code, loop_id, len, false);
                output.extend(cond_code);
                output.push(Instr::JmpBack(len));
            }
            Expr::ForLoop(var_name, array_code) => {
                let real_var = var_name.as_str() != "_";

                // parse the array, get its id (the target array is the first Expr in array_code)
                let array = array_code.first().unwrap();
                let code = &array_code[1..];
                let array_type = infer_type(array, v, fns, src);
                let array = get_id(array, v, parser_data!(), &mut output);

                // try to optimize it
                if for_loop_summation(&mut output, registers, v, array, code) {
                    continue;
                }

                // add an instruction to get array length (func id 2 = len)
                registers.push(Data::NULL);
                let array_len_id = (registers.len() - 1) as u16;
                output.push(Instr::Len(array, array_len_id));

                // set up the id of the index variable (0..len)
                registers.push(0.into());
                let index_id = (registers.len() - 1) as u16;

                // do the 'i < len' condition, set up the condition's id (true/false)
                registers.push(Data::NULL);
                let condition_id = (registers.len() - 1) as u16;
                output.push(Instr::InfInt(index_id, array_len_id, condition_id));

                // set up the variable for the current element (for current_element_id in ... {}) => current_element_id = array[index]
                let current_element_id = registers.len() as u16;
                if real_var {
                    registers.push(Data::NULL);
                }

                // parse everything, add the current element variable to temp_vars so that the loop code can interact with it
                v.push((
                    *var_name,
                    current_element_id,
                    match &array_type {
                        DataType::String => DataType::String,
                        DataType::Array(a_type) => *a_type.clone(),
                        _ => todo!("For loop invalid type"),
                    },
                ));
                let (v_idx, t_idx) = (v.len() - 1, v.len() - 1);

                let loop_id = block_id + 1;
                let mut cond_code = parser_to_instr_set(code, v, parser_data!());
                // Clean up variables
                v.remove(v_idx);
                v.remove(t_idx);

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
                registers.push(1.into());
                output.push(Instr::AddInt(
                    index_id,
                    (registers.len() - 1) as u16,
                    index_id,
                ));

                // jump back to the loop if still inside of it
                output.push(Instr::JmpBack(len));

                // clean up, reset the index variable
                registers.push(0.into());
                output.push(Instr::Mov((registers.len() - 1) as u16, index_id));
            }
            Expr::IntForLoop(var_name, start_elem, end_elem, code, start1, end1, start2, end2) => {
                // Check start elem type
                let t1 = infer_type(start_elem, v, fns, src);
                if t1 != DataType::Int {
                    parser_error(
                        src,
                        *start1,
                        *end1,
                        "Invalid type",
                        &format!(
                            "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            DataType::Int,
                            t1
                        ),
                        "",
                    )
                }
                // Check end elem type
                let t2 = infer_type(end_elem, v, fns, src);
                if t2 != DataType::Int {
                    parser_error(
                        src,
                        *start2,
                        *end2,
                        "Invalid type",
                        &format!(
                            "Expected {}, found {color_bright_blue}{style_bold}{}{color_reset}{style_reset}",
                            DataType::Int,
                            t2
                        ),
                        "",
                    )
                }
                let elem_id = get_id(start_elem, v, parser_data!(), &mut output);
                // Create temporary variable and store its index, so as to delete it later
                let mut v_temp = v.clone();
                v_temp.push((*var_name, elem_id, DataType::Int));
                let (v_idx, t_idx) = (v.len() - 1, v.len() - 1);

                // Compile the code inside the loop
                let compiled_loop_code = parser_to_instr_set(code, v, parser_data!());
                let compiled_loop_code_len = compiled_loop_code.len() as u16;

                // Loop logic
                let end_elem_id = get_id(end_elem, v, parser_data!(), &mut output);
                // Add an InfCmp first, to check if i < end_elem
                output.push(Instr::InfIntCmp(
                    elem_id,
                    end_elem_id,
                    compiled_loop_code_len + 3,
                ));
                output.extend(compiled_loop_code);
                registers.push(1.into());
                // Do i += 1
                output.push(Instr::AddInt(
                    elem_id,
                    (registers.len() - 1) as u16,
                    elem_id,
                ));
                // Jump back to the start of the function
                output.push(Instr::JmpBack(2 + compiled_loop_code_len))
            }
            Expr::LoopBlock(code) => {
                let loop_id = block_id + 1;
                let mut vars = v.clone();
                let mut compiled = parser_to_instr_set(code, &mut vars, parser_data!());
                let code_length = compiled.len() as u16;
                parse_indef_loop_flow_control(&mut compiled, loop_id, code_length + 1);
                output.extend(compiled);
                output.push(Instr::JmpBack(code_length));
            }
            Expr::VarDeclare(x, y) => {
                let var_type = type_inference::infer_type(y, v, fns, src);
                let output_len = output.len();
                let obj_id = get_id(y, v, parser_data!(), &mut output);

                let var_id = if output.len() != output_len {
                    if can_move(output.last().unwrap()) {
                        registers.push(Data::NULL);
                    }
                    move_to_id(&mut output, (registers.len() - 1) as u16);
                    (registers.len() - 1) as u16
                } else {
                    obj_id
                };
                v.push((*x, var_id, var_type));
            }
            Expr::VarAssign(name, y, start, end) => {
                let var_type = type_inference::infer_type(y, v, fns, src);
                let id = v
                    .iter()
                    .find(|(w, _, _)| w == name)
                    .unwrap_or_else(|| {
                        parser_error(
                            src,
                            *start,
                            *end,
                            "Unknown variable",
                            &format!(
                                "Variable {color_bright_blue}{style_bold}{name}{color_reset}{style_reset} has not been declared yet"
                            ),
                            &format!("Declare it with {color_green}let {name} = 0;{color_reset}")
                        )
                    })
                    .1;
                let output_len = output.len();
                let obj_id = get_id(y, v, parser_data!(), &mut output);
                if output.len() != output_len {
                    move_to_id(&mut output, id);
                } else {
                    output.push(Instr::Mov(obj_id, id));
                }

                v.iter_mut()
                    .find(|(v_name, _, _)| v_name == name)
                    .unwrap()
                    .2 = var_type;
                debug!("NEW VAR TYPES ARE {v:?}");
            }

            Expr::FunctionCall(args, namespace, start, end, args_indexes) => {
                handle_functions(
                    &mut output,
                    v,
                    parser_data!(),
                    args,
                    namespace,
                    *start,
                    *end,
                    args_indexes,
                );
            }
            Expr::ObjFunctionCall(obj, args, namespace, start, end, args_indexes) => {
                handle_method_calls(
                    &mut output,
                    v,
                    parser_data!(),
                    obj,
                    args,
                    namespace,
                    *start,
                    *end,
                    args_indexes,
                );
            }
            Expr::FunctionDecl(x, y, start, end) => {
                if fns
                    .iter()
                    .any(|(name, _, _, _, _, _)| **name == *x.first().unwrap())
                {
                    parser_error(
                        src,
                        *start,
                        *end,
                        "Function defined twice",
                        &format!(
                            "Function {color_bright_blue}{style_bold}{}{color_reset}{style_reset} is already defined",
                            x[0]
                        ),
                        "",
                    );
                }
                fns.push((
                    x.first().unwrap().to_string(),
                    x.into_iter().skip(1).map(ToString::to_string).collect(),
                    y.clone(),
                    Vec::new(),
                    contains_recursive_call(y, x.first().unwrap()),
                    fn_registers.len() as u16,
                ));
                fn_registers.push(Vec::new());
            }
            Expr::ReturnVal(val) => {
                if let Some(x) = &**val {
                    let id = get_id(x, v, parser_data!(), &mut output);
                    if is_parsing_recursive {
                        output.push(Instr::RecursiveReturn(id, parsing_fn_id.unwrap()));
                    } else {
                        output.push(Instr::Return(id));
                    }
                }
            }
            // Break(block_id) = EqCmp(block_id, 0, 0)
            Expr::Break => output.push(Instr::EqCmp(block_id, 0, 0)),
            // Break(block_id) = NotEqCmp(block_id, 0, 0)
            Expr::Continue => output.push(Instr::NotEqCmp(block_id, 0, 0)),
            Expr::EvalBlock(code) => {
                let mut vars = v.clone();
                output.extend(parser_to_instr_set(code, &mut vars, parser_data!()))
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
    ArrayStorage,
    Vec<(Instr, usize, usize)>,
    Vec<Vec<u16>>,
) {
    let now = std::time::Instant::now();
    let functions: Vec<Expr> = grammar::FileParser::new()
        .parse(contents)
        .unwrap_or_else(|x| lalrpop_error::<usize, Token<'_>, &str>(x, contents, filename));
    println!("LALRPOP TIME {:.2?}", now.elapsed());
    // println!("FUNCS {functions:?}");

    let mut variables: Vec<(Intern<String>, u16, DataType)> = Vec::new();
    let mut registers: Vec<Data> = Vec::new();
    let mut arrays: ArrayStorage = Slab::with_capacity(20);
    let mut instr_src = Vec::new();
    let mut fn_registers: Vec<Vec<u16>> = Vec::new();
    let mut functions: Vec<Function> = functions
        .into_iter()
        .map(|w| {
            if let Expr::FunctionDecl(x, y, _, _) = w {
                fn_registers.push(Vec::new());
                (
                    x[0].to_string(),
                    x[1..].into(),
                    y.clone(),
                    Vec::new(),
                    contains_recursive_call(&y, &x[0]),
                    (fn_registers.len() - 1) as u16,
                )
            } else {
                unreachable!()
            }
        })
        .collect();

    let instructions = parser_to_instr_set(
        functions
            .iter()
            .find(|x| x.0 == "main")
            .unwrap_or_else(|| {
                error(String::from("Could not find main function"));
            })
            .2
            .to_vec()
            .as_slice(),
        &mut variables,
        (
            &mut registers,
            &mut functions,
            &mut arrays,
            0,
            (filename, contents),
            &mut instr_src,
            false,
            &mut fn_registers,
            None,
        ),
    );
    for x in fn_registers.iter_mut() {
        x.sort();
        x.dedup();
    }
    #[cfg(debug_assertions)]
    {
        print_debug(&instructions, &registers, &arrays);
    }
    (instructions, registers, arrays, instr_src, fn_registers)
}
