use crate::ArrayPool;
use crate::LibFunc;
use crate::data::NULL;
use crate::debug;
use crate::errors::ErrType;
use crate::errors::dev_error;
use crate::errors::lalrpop_error;
use crate::errors::throw_parser_error;
use crate::functions::handle_functions;
use crate::grammar::Token;
use crate::method_calls::handle_method_calls;
use crate::parser_data::*;
use crate::type_system::check_if_returns_void;
use crate::type_system::contains_recursive_call;
use crate::type_system::datatype_to_c_type;
use crate::type_system::is_array_with_incompatible_type;
use crate::type_system::is_indexable;
use crate::type_system::{DataType, infer_type};
use crate::{Data, Instr};
use inline_colorization::*;
use lalrpop_util::lalrpop_mod;
use smol_str::SmolStr;
use smol_str::ToSmolStr;
use std::collections::HashSet;
use std::slice;

lalrpop_mod!(pub grammar);

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub enum Expr {
    Float(f64),
    Int(i32),
    Bool(bool),
    String(SmolStr),
    /// Var(name, start, end)
    Var(SmolStr, (usize, usize)),
    /// Array(contents, start, end)
    Array(Box<[Expr]>, (usize, usize)),
    /// VarDeclare(name, value),
    VarDeclare(SmolStr, Box<Expr>),
    /// VarDeclare(name, value, start, end)
    VarAssign(SmolStr, Box<Expr>, (usize, usize)),
    /// Condition(condition, code (contains else_if_blocks and potentially else_block), start, end)
    Condition(Box<Expr>, Box<[Expr]>, (usize, usize)),
    ElseIfBlock(Box<Expr>, Box<[Expr]>),
    ElseBlock(Box<[Expr]>),

    WhileBlock(Box<Expr>, Box<[Expr]>),
    /// FunctionCall(args, (optional namespace + name), start, end, (arg_start,arg_end))
    FunctionCall(
        Box<[Expr]>,
        Box<[SmolStr]>,
        (usize, usize),
        Box<[(usize, usize)]>,
    ),
    ObjFunctionCall(
        Box<Expr>,
        Box<[Expr]>,
        Box<[SmolStr]>,
        // obj_start
        (
            usize,
            // obj_end
            usize,
        ),
        // fn_start
        (
            usize,
            // fn_end
            usize,
        ),
        Box<[(usize, usize)]>,
    ),
    /// FunctionDecl(name+args, code, start, end)
    FunctionDecl(Box<[SmolStr]>, Box<[Expr]>, (usize, usize)),

    ReturnVal(Box<Option<Expr>>),

    GetIndex(Box<Expr>, Box<[Expr]>, (usize, usize)),
    ArrayModify(
        Box<Expr>,
        Box<[Expr]>,
        Box<Expr>,
        (usize, usize),
        (usize, usize),
    ),

    /// ForLoop(loop_var_name, loop_array+code)
    ForLoop(SmolStr, Box<[Expr]>),
    /// IntForLoop(loop_var_name, first_elem, final_elem, code)
    IntForLoop(
        SmolStr,
        Box<Expr>,
        Box<Expr>,
        Box<[Expr]>,
        (usize, usize),
        (usize, usize),
    ),
    /// Import(lib_path, [(fn_name, fn_args, fn_return_type)])
    ImportDynLib(SmolStr, Box<[(SmolStr, Box<[DataType]>, DataType)]>),

    ImportFile(SmolStr),

    Break,
    Continue,

    EvalBlock(Box<[Expr]>),
    LoopBlock(Box<[Expr]>),

    Mul(Box<Expr>, Box<Expr>, (usize, usize)),
    Div(Box<Expr>, Box<Expr>, (usize, usize)),
    Add(Box<Expr>, Box<Expr>, (usize, usize)),
    Sub(Box<Expr>, Box<Expr>, (usize, usize)),
    Mod(Box<Expr>, Box<Expr>, (usize, usize)),
    Pow(Box<Expr>, Box<Expr>, (usize, usize)),
    Eq(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    Sup(Box<Expr>, Box<Expr>, (usize, usize)),
    SupEq(Box<Expr>, Box<Expr>, (usize, usize)),
    Inf(Box<Expr>, Box<Expr>, (usize, usize)),
    InfEq(Box<Expr>, Box<Expr>, (usize, usize)),
    BoolAnd(Box<Expr>, Box<Expr>, (usize, usize)),
    BoolOr(Box<Expr>, Box<Expr>, (usize, usize)),
    Neg(Box<Expr>, (usize, usize)),
}

#[cold]
#[inline(never)]
pub fn symbol_of_expr(expr: &Expr) -> &str {
    match expr {
        Expr::Mul(_, _, _) => "*",
        Expr::Div(_, _, _) => "/",
        Expr::Add(_, _, _) => "+",
        Expr::Sub(_, _, _) => "-",
        Expr::Mod(_, _, _) => "%",
        Expr::Pow(_, _, _) => "^",
        Expr::Eq(_, _) => "==",
        Expr::NotEq(_, _) => "!=",
        Expr::Sup(_, _, _) => ">",
        Expr::SupEq(_, _, _) => ">=",
        Expr::Inf(_, _, _) => "<",
        Expr::InfEq(_, _, _) => "<=",
        Expr::BoolAnd(_, _, _) => "&&",
        Expr::BoolOr(_, _, _) => "||",
        Expr::Neg(_, _) => "-",
        other => dev_error(
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
            Instr::ArrayMov(_, _, _) // | Instr::IoDelete(_)
        )
    {
        return;
    }
    let matching_elem_index = x
        .iter()
        .rposition(|w| get_tgt_id(*w).is_some())
        .unwrap_or(x.len() - 1);
    let matching_elem = x.get_mut(matching_elem_index).unwrap();
    match matching_elem {
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
        | Instr::CallLibFunc(_, _, y)
        | Instr::GetIndexArray(_, _, y)
        | Instr::GetIndexString(_, _, y)
        // | Instr::IoOpen(_, y, _)
        | Instr::SaveFrame(_, y, _)
        | Instr::CallDynamicLibFunc(_, y) => *y = tgt_id,
        Instr::CallFuncRecursive(_, y_func) => {
            *y_func = tgt_id;
            for i in 1..x.len() - 1 {
                if let Some(Instr::SaveFrame(_, y_frame, _)) = x.get_mut(matching_elem_index - i) {
                    *y_frame = tgt_id;
                    break;
                }
            }
        }
        other => dev_error(
            "parser.rs",
            "move_to_id",
            format_args!("Tried to move {other:?} to tgt_id={tgt_id}"),
        ),
    }
}

/// Returns the ID of the register that will be modified by the given instruction
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
        | Instr::CallLibFunc(_, _, y)
        | Instr::GetIndexArray(_, _, y)
        | Instr::GetIndexString(_, _, y)
        // | Instr::IoOpen(_, y, _)
        | Instr::SetElementString(y, _, _)
        | Instr::CallDynamicLibFunc(_, y) => Some(y),
        // ↓ INSTRUCTIONS THAT DON'T MODIFY ANY REGISTER ↓
        Instr::Print(_)
        | Instr::Jmp(_)
        | Instr::JmpBack(_)
        | Instr::IsFalseJmp(_, _)
        | Instr::NotEqJmp(_, _, _)
        | Instr::ArrayNotEqJmp(_, _, _)
        | Instr::EqJmp(_, _, _)
        | Instr::ArrayEqJmp(_, _, _)
        | Instr::SupFloatJmp(_, _, _)
        | Instr::SupIntJmp(_, _, _)
        | Instr::SupEqFloatJmp(_, _, _)
        | Instr::SupEqIntJmp(_, _, _)
        | Instr::InfEqFloatJmp(_, _, _)
        | Instr::InfEqIntJmp(_, _, _)
        | Instr::InfFloatJmp(_, _, _)
        | Instr::InfIntJmp(_, _, _)
        // | Instr::IoDelete(_)
        | Instr::StoreFuncArg(_)
        | Instr::SetElementArray(_, _, _)
        | Instr::ArrayMov(_, _, _)
        | Instr::Push(_, _)
        | Instr::Return(_) // Modifies a register, but this function doesn't know which one
        | Instr::RecursiveReturn(_) // Modifies a register, but this function doesn't know which one
        | Instr::VoidReturn
        | Instr::Remove(_, _) => None,
    }
}

/// Returns a list containing the IDs of all the registers which are modified by the given instructions
pub fn get_tgt_ids(x: &[Instr]) -> Vec<u16> {
    let mut ids: Vec<u16> = x.iter().filter_map(|i| get_tgt_id(*i)).collect();
    ids.sort_unstable();
    ids.dedup();
    ids
}

fn get_last_tgt_id(x: &[Instr]) -> Option<u16> {
    debug_assert!(
        !(x.is_empty()
            || matches!(
                x.last().unwrap(),
                Instr::ArrayMov(_, _, _) // | Instr::IoDelete(_)
            ))
    );
    for y in x.iter().rev() {
        if let Some(id) = get_tgt_id(*y) {
            return Some(id);
        }
    }
    None
}

pub fn alloc_register(registers: &mut Vec<Data>, free_registers: &mut Vec<u16>) -> u16 {
    if let Some(id) = free_registers.pop() {
        id
    } else {
        registers.push(NULL);
        (registers.len() - 1) as u16
    }
}

pub fn free_register(
    id: u16,
    free_registers: &mut Vec<u16>,
    v: &[Variable],
    const_registers: &[u16],
) {
    if !v.iter().any(|var| var.register_id == id)
        && !const_registers.iter().any(|reg| reg == &id)
        && !free_registers.contains(&id)
    {
        free_registers.push(id);
    }
}

pub fn is_reg_free(v: &[Variable], id: u16, name: &SmolStr) -> bool {
    !v.iter()
        .any(|var| &var.name != name && var.register_id == id)
}

pub fn get_id(
    input: &Expr,
    v: &mut Vec<Variable>,
    p: &ParserData,
    output: &mut Vec<Instr>,
    tgt_id: Option<u16>,
    expects_op_cmp: bool,
    var_assignment: bool,
    offset: u16,
) -> u16 {
    let (
        registers,
        fns,
        Pools {
            array_pool,
            string_pool,
        },
        _,
        _,
        block_id,
        src,
        _,
        _,
        dyn_libs,
        _,
        _,
        const_registers,
        free_registers,
    ) = p.destructure();

    macro_rules! uniform_op {
        ($instr: ident,$symbol:expr, $l: expr, $r: expr, $markers: expr, $type:expr) => {{
            let (t_l, t_r) = (
                infer_type($l, v, fns, src, dyn_libs),
                infer_type($r, v, fns, src, dyn_libs),
            );
            if t_l != $type || t_r != $type {
                throw_parser_error(src, $markers, ErrType::OpError(t_l, t_r, $symbol))
                // op_error!(src, t_l, t_r, $symbol, *$start, *$end);
            }
            let id_l = get_id($l, v, p, output, None, false, false, offset);
            let id_r = get_id($r, v, p, output, None, false, false, offset);
            free_register(id_l, free_registers, v, const_registers);
            free_register(id_r, free_registers, v, const_registers);
            let id = if let Some(tgt_register_id) = tgt_id {
                tgt_register_id
            } else if expects_op_cmp {
                0
            } else {
                alloc_register(registers, free_registers)
            };
            output.push(Instr::$instr(id_l, id_r, id));
            id
        }};
        ($instr: ident, $instr2:ident,$symbol:expr, $l: expr, $r: expr, $markers: expr, $type1:expr, $type2:expr) => {{
            let (t_l, t_r) = (
                infer_type($l, v, fns, src, dyn_libs),
                infer_type($r, v, fns, src, dyn_libs),
            );
            if !((t_l == $type1 && t_r == $type1) || (t_l == $type2 && t_r == $type2)) {
                throw_parser_error(src, $markers, ErrType::OpError(t_l, t_r, $symbol))
                // op_error!(src, t_l, t_r, $symbol, *$start, *$end);
            }
            let id_l = get_id($l, v, p, output, None, false, false, offset);
            let id_r = get_id($r, v, p, output, None, false, false, offset);
            free_register(id_l, free_registers, v, const_registers);
            free_register(id_r, free_registers, v, const_registers);
            let id = if let Some(tgt_register_id) = tgt_id {
                tgt_register_id
            } else if expects_op_cmp {
                0
            } else {
                alloc_register(registers, free_registers)
            };
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
            if var_assignment {
                registers.push((*num).into());
                return (registers.len() - 1) as u16;
            }
            if let Some(id) = const_registers
                .iter()
                .find(|x| registers[**x as usize] == (*num).into())
            {
                *id
            } else {
                registers.push((*num).into());
                const_registers.push(registers.len() as u16);
                (registers.len() - 1) as u16
            }
        }
        Expr::Int(num) => {
            if var_assignment {
                registers.push((*num).into());
                return (registers.len() - 1) as u16;
            }
            if let Some(id) = const_registers
                .iter()
                .find(|x| registers[**x as usize] == (*num).into())
            {
                *id
            } else {
                const_registers.push(registers.len() as u16);
                registers.push((*num).into());
                (registers.len() - 1) as u16
            }
        }
        Expr::String(str) => {
            if var_assignment {
                registers.push(Data::p_str(str, string_pool));
                return (registers.len() - 1) as u16;
            }
            if let Some(id) = const_registers.iter().find(|x| {
                registers[**x as usize].is_str()
                    && registers[**x as usize].as_str(string_pool) == str
            }) {
                *id
            } else {
                const_registers.push(registers.len() as u16);
                registers.push(Data::p_str(str, string_pool));
                (registers.len() - 1) as u16
            }
        }
        Expr::Bool(bool) => {
            if var_assignment {
                registers.push((*bool).into());
                return (registers.len() - 1) as u16;
            }
            if let Some(id) = const_registers
                .iter()
                .find(|x| registers[**x as usize] == (*bool).into())
            {
                *id
            } else {
                const_registers.push(registers.len() as u16);
                registers.push((*bool).into());
                (registers.len() - 1) as u16
            }
        }
        Expr::Var(name, markers) => {
            if let Some(Variable {
                name: _,
                register_id,
                infered_type: _,
            }) = v.iter().rfind(|v_temp| *name == v_temp.name)
            {
                *register_id
            } else {
                throw_parser_error(src, markers, ErrType::UnknownVariable(name))
            }
        }
        Expr::Array(elems, markers) => {
            let first_type = infer_type(&elems[0], v, fns, src, dyn_libs);
            if !elems
                .iter()
                .all(|x| infer_type(x, v, fns, src, dyn_libs) == first_type)
            {
                throw_parser_error(src, markers, ErrType::ArrayWithDiffType);
            }
            let array_id = {
                array_pool.push(Vec::new());
                array_pool.len() - 1
            };
            for elem in elems {
                let x = compile_expr(slice::from_ref(elem), v, p, 0);
                if !x.is_empty() {
                    let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                    array_pool.get_mut(array_id).unwrap().push(NULL);

                    output.extend(x);
                    output.push(Instr::ArrayMov(
                        c_id,
                        block_id,
                        (array_pool[array_id].len() - 1) as u16,
                    ));
                } else {
                    array_pool
                        .get_mut(array_id)
                        .unwrap()
                        .push(registers.pop().unwrap());
                }
            }
            registers.push(Data::array(array_id as u32));
            (registers.len() - 1) as u16
        }
        Expr::Mul(l, r, markers) => {
            uniform_op!(
                MulFloat,
                MulInt,
                "*",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Div(l, r, markers) => {
            uniform_op!(
                DivFloat,
                DivInt,
                "/",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Add(l, r, markers) => {
            let t_l = infer_type(l, v, fns, src, dyn_libs);
            let t_r = infer_type(r, v, fns, src, dyn_libs);
            if t_l != t_r
                || !matches!(
                    t_l,
                    DataType::String | DataType::Array(_) | DataType::Float | DataType::Int
                )
            {
                throw_parser_error(src, markers, ErrType::OpError(t_l, t_r, "+"));
                // op_error!(src, t_l, t_r, "+", markers);
            }
            let id_l = get_id(l, v, p, output, None, false, false, offset);
            let id_r = get_id(r, v, p, output, None, false, false, offset);
            free_register(id_l, free_registers, v, const_registers);
            free_register(id_r, free_registers, v, const_registers);
            let id = if let Some(tgt_register_id) = tgt_id {
                tgt_register_id
            } else if expects_op_cmp {
                0
            } else {
                alloc_register(registers, free_registers)
            };
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
        Expr::Sub(l, r, markers) => {
            uniform_op!(
                SubFloat,
                SubInt,
                "-",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Mod(l, r, markers) => {
            uniform_op!(
                ModFloat,
                ModInt,
                "%",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Pow(l, r, markers) => {
            uniform_op!(
                PowFloat,
                PowInt,
                "^",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Eq(l, r) => {
            let is_array = matches!(infer_type(l, v, fns, src, dyn_libs), DataType::Array(_))
                && matches!(infer_type(r, v, fns, src, dyn_libs), DataType::Array(_));
            let id_l = get_id(l, v, p, output, None, false, false, offset);
            let id_r = get_id(r, v, p, output, None, false, false, offset);
            free_register(id_l, free_registers, v, const_registers);
            free_register(id_r, free_registers, v, const_registers);
            let id = if let Some(tgt_register_id) = tgt_id {
                tgt_register_id
            } else if expects_op_cmp {
                0
            } else {
                alloc_register(registers, free_registers)
            };
            output.push(if is_array {
                Instr::ArrayEq(id_l, id_r, id)
            } else {
                Instr::Eq(id_l, id_r, id)
            });
            id
        }
        Expr::NotEq(l, r) => {
            let id_l = get_id(l, v, p, output, None, false, false, offset);
            let id_r = get_id(r, v, p, output, None, false, false, offset);
            free_register(id_l, free_registers, v, const_registers);
            free_register(id_r, free_registers, v, const_registers);
            let id = if let Some(tgt_register_id) = tgt_id {
                tgt_register_id
            } else if expects_op_cmp {
                0
            } else {
                alloc_register(registers, free_registers)
            };
            if matches!(infer_type(l, v, fns, src, dyn_libs), DataType::Array(_))
                && matches!(infer_type(r, v, fns, src, dyn_libs), DataType::Array(_))
            {
                output.push(Instr::ArrayNotEq(id_l, id_r, id));
            } else {
                output.push(Instr::NotEq(id_l, id_r, id));
            }
            id
        }
        Expr::Sup(l, r, markers) => {
            uniform_op!(
                SupFloat,
                SupInt,
                ">",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::SupEq(l, r, markers) => {
            uniform_op!(
                SupEqFloat,
                SupEqInt,
                ">=",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::Inf(l, r, markers) => {
            uniform_op!(
                InfFloat,
                InfInt,
                "<",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::InfEq(l, r, markers) => {
            uniform_op!(
                InfEqFloat,
                InfEqInt,
                "<=",
                l,
                r,
                markers,
                DataType::Float,
                DataType::Int
            )
        }
        Expr::BoolAnd(l, r, markers) => {
            uniform_op!(BoolAnd, "&&", l, r, markers, DataType::Bool)
        }
        Expr::BoolOr(l, r, markers) => {
            uniform_op!(BoolOr, "||", l, r, markers, DataType::Bool)
        }
        Expr::Neg(l, markers) => {
            let operand_type = infer_type(l, v, fns, src, dyn_libs);
            let id_l = get_id(l, v, p, output, None, false, false, offset);
            free_register(id_l, free_registers, v, const_registers);
            let id = if let Some(tgt_register_id) = tgt_id {
                tgt_register_id
            } else if expects_op_cmp {
                0
            } else {
                alloc_register(registers, free_registers)
            };
            if operand_type == DataType::Float {
                output.push(Instr::NegFloat(id_l, id))
            } else if operand_type == DataType::Int {
                output.push(Instr::NegInt(id_l, id))
            } else {
                throw_parser_error(src, markers, ErrType::InvalidOp(operand_type, "-"));
            }
            id
        }

        Expr::Condition(main_condition, code, markers) => {
            // let return_id = registers.len() as u16;
            // registers.push(NULL);
            let return_id = alloc_register(registers, free_registers);

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
            let condition_id = get_id(main_condition, v, p, output, None, true, false, offset);
            add_cmp(condition_id, &mut 0, output, false);
            cmp_markers.push(output.len() - 1);

            let v_len = v.len();
            // parse the main code block
            let cond_code = compile_expr(&code[0..main_code_limit], v, p, output.len() as u16);
            v.truncate(v_len);
            let is_empty = cond_code.is_empty();
            output.extend(cond_code);
            output.push(Instr::Mov(
                if is_empty {
                    (registers.len() - 1) as u16
                } else {
                    get_last_tgt_id(output).unwrap()
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
                    let condition_id = get_id(condition, v, p, output, None, true, false, offset);
                    add_cmp(condition_id, &mut 0, output, false);
                    free_register(condition_id, free_registers, v, const_registers);
                    cmp_markers.push(output.len() - 1);
                    let v_len = v.len();
                    let cond_code = compile_expr(code, v, p, output.len() as u16);
                    v.truncate(v_len);
                    let is_empty = cond_code.is_empty();
                    output.extend(cond_code);
                    output.push(Instr::Mov(
                        if is_empty {
                            (registers.len() - 1) as u16
                        } else {
                            get_last_tgt_id(output).unwrap()
                        },
                        return_id,
                    ));
                    output.push(Instr::Jmp(0));
                    jmp_markers.push(output.len() - 1);
                } else if let Expr::ElseBlock(code) = elem {
                    else_exists = true;
                    condition_markers.push(output.len());
                    let v_len = v.len();
                    let cond_code = compile_expr(code, v, p, output.len() as u16);
                    v.truncate(v_len);
                    let is_empty = cond_code.is_empty();
                    output.extend(cond_code);
                    output.push(Instr::Mov(
                        if is_empty {
                            (registers.len() - 1) as u16
                        } else {
                            get_last_tgt_id(output).unwrap()
                        },
                        return_id,
                    ));
                }
            }
            if !else_exists {
                throw_parser_error(src, markers, ErrType::InvalidConditionalExpression);
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
                    Instr::IsFalseJmp(_, jump_size)
                    | Instr::SupEqFloatJmp(_, _, jump_size)
                    | Instr::SupEqIntJmp(_, _, jump_size)
                    | Instr::SupFloatJmp(_, _, jump_size)
                    | Instr::SupIntJmp(_, _, jump_size)
                    | Instr::InfEqFloatJmp(_, _, jump_size)
                    | Instr::InfEqIntJmp(_, _, jump_size)
                    | Instr::InfFloatJmp(_, _, jump_size)
                    | Instr::InfIntJmp(_, _, jump_size)
                    | Instr::NotEqJmp(_, _, jump_size)
                    | Instr::ArrayNotEqJmp(_, _, jump_size)
                    | Instr::EqJmp(_, _, jump_size)
                    | Instr::ArrayEqJmp(_, _, jump_size),
                ) = output.get_mut(*y)
                {
                    *jump_size = diff as u16;
                }
            }
            free_register(condition_id, free_registers, v, const_registers);
            return_id
        }
        Expr::FunctionCall(args, namespace, markers, args_indexes) => handle_functions(
            output,
            v,
            p,
            args,
            namespace,
            markers,
            args_indexes,
            offset + output.len() as u16,
        )
        .unwrap_or_else(|| get_last_tgt_id(output).unwrap_or_else(|| (registers.len() - 1) as u16)),
        other => {
            // dbg!(&other);
            let output_code = compile_expr(slice::from_ref(other), v, p, 0);
            if !output_code.is_empty() {
                output.extend(output_code);
                get_last_tgt_id(output).unwrap_or((registers.len() - 1) as u16)
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
        return output.push(Instr::IsFalseJmp(condition_id, *len));
    }
    *output.last_mut().unwrap() = match *output.last().unwrap() {
        Instr::InfFloat(o1, o2, o3) if o3 == condition_id => Instr::SupEqFloatJmp(o1, o2, *len),
        Instr::InfInt(o1, o2, o3) if o3 == condition_id => Instr::SupEqIntJmp(o1, o2, *len),
        Instr::InfEqFloat(o1, o2, o3) if o3 == condition_id => Instr::SupFloatJmp(o1, o2, *len),
        Instr::InfEqInt(o1, o2, o3) if o3 == condition_id => Instr::SupIntJmp(o1, o2, *len),
        Instr::SupFloat(o1, o2, o3) if o3 == condition_id => Instr::InfEqFloatJmp(o1, o2, *len),
        Instr::SupInt(o1, o2, o3) if o3 == condition_id => Instr::InfEqIntJmp(o1, o2, *len),
        Instr::SupEqFloat(o1, o2, o3) if o3 == condition_id => Instr::InfFloatJmp(o1, o2, *len),
        Instr::SupEqInt(o1, o2, o3) if o3 == condition_id => Instr::InfIntJmp(o1, o2, *len),
        Instr::Eq(o1, o2, o3) if o3 == condition_id => Instr::NotEqJmp(o1, o2, *len),
        Instr::ArrayEq(o1, o2, o3) if o3 == condition_id => Instr::ArrayNotEqJmp(o1, o2, *len),
        Instr::NotEq(o1, o2, o3) if o3 == condition_id => Instr::EqJmp(o1, o2, *len),
        Instr::ArrayNotEq(o1, o2, o3) if o3 == condition_id => Instr::ArrayEqJmp(o1, o2, *len),
        _ => {
            output.push(Instr::IsFalseJmp(condition_id, *len));
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
        if let Instr::NotEqJmp(break_id, 0, 0) = x.1
            && *break_id == loop_id
        {
            if for_loop {
                *x.1 = Instr::Jmp(code_length - x.0 as u16 - 1);
            } else {
                *x.1 = Instr::Jmp(code_length - x.0 as u16);
            }
        } else if let Instr::EqJmp(continue_id, 0, 0) = x.1
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
        if let Instr::NotEqJmp(break_id, 0, 0) = x
            && *break_id == loop_id
        {
            *x = Instr::Jmp(code_length - i as u16);
        } else if let Instr::EqJmp(continue_id, 0, 0) = x
            && *continue_id == loop_id
        {
            *x = Instr::Jmp(code_length - i as u16 - 3);
        }
    });
}

pub fn for_each_read_reg(instr: Instr, mut f: impl FnMut(u16)) {
    match instr {
        Instr::AddFloat(a, b, _)
        | Instr::AddInt(a, b, _)
        | Instr::AddArray(a, b, _)
        | Instr::AddStr(a, b, _)
        | Instr::MulFloat(a, b, _)
        | Instr::MulInt(a, b, _)
        | Instr::SubFloat(a, b, _)
        | Instr::SubInt(a, b, _)
        | Instr::DivFloat(a, b, _)
        | Instr::DivInt(a, b, _)
        | Instr::ModFloat(a, b, _)
        | Instr::ModInt(a, b, _)
        | Instr::PowFloat(a, b, _)
        | Instr::PowInt(a, b, _)
        | Instr::Eq(a, b, _)
        | Instr::NotEq(a, b, _)
        | Instr::ArrayEq(a, b, _)
        | Instr::ArrayNotEq(a, b, _)
        | Instr::SupFloat(a, b, _)
        | Instr::SupInt(a, b, _)
        | Instr::SupEqFloat(a, b, _)
        | Instr::SupEqInt(a, b, _)
        | Instr::InfFloat(a, b, _)
        | Instr::InfInt(a, b, _)
        | Instr::InfEqFloat(a, b, _)
        | Instr::InfEqInt(a, b, _)
        | Instr::BoolAnd(a, b, _)
        | Instr::BoolOr(a, b, _)
        | Instr::GetIndexArray(a, b, _)
        | Instr::GetIndexString(a, b, _)
        | Instr::NotEqJmp(a, b, _)
        | Instr::EqJmp(a, b, _)
        | Instr::ArrayNotEqJmp(a, b, _)
        | Instr::ArrayEqJmp(a, b, _)
        | Instr::SupFloatJmp(a, b, _)
        | Instr::SupIntJmp(a, b, _)
        | Instr::SupEqFloatJmp(a, b, _)
        | Instr::SupEqIntJmp(a, b, _)
        | Instr::InfFloatJmp(a, b, _)
        | Instr::InfIntJmp(a, b, _)
        | Instr::InfEqFloatJmp(a, b, _)
        | Instr::InfEqIntJmp(a, b, _)
        | Instr::Push(a, b)
        | Instr::Remove(a, b) => {
            f(a);
            f(b);
        }

        Instr::SetElementArray(a, b, c) | Instr::SetElementString(a, b, c) => {
            f(a);
            f(b);
            f(c);
        }

        Instr::Mov(a, _)
        | Instr::NegFloat(a, _)
        | Instr::NegInt(a, _)
        | Instr::CallLibFunc(_, a, _)
        | Instr::Print(a)
        | Instr::StoreFuncArg(a)
        | Instr::Return(a)
        | Instr::RecursiveReturn(a)
        | Instr::IsFalseJmp(a, _) => f(a),

        Instr::ArrayMov(a, _, _) => f(a),

        Instr::Jmp(_)
        | Instr::JmpBack(_)
        | Instr::VoidReturn
        | Instr::CallFunc(_, _)
        | Instr::CallFuncRecursive(_, _)
        | Instr::SaveFrame(_, _, _)
        | Instr::CallDynamicLibFunc(_, _) => {}
    }
}

#[inline(always)]
pub fn compile_expr(
    input: &[Expr],
    v: &mut Vec<Variable>,
    p: &ParserData,
    offset: u16,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::with_capacity(input.len());

    let (
        registers,
        fns,
        Pools {
            array_pool,
            string_pool,
        },
        instr_src,
        fn_registers,
        block_id,
        src,
        is_parsing_recursive,
        parsing_fn_id,
        dyn_libs,
        _,
        _,
        const_registers,
        free_registers,
    ) = p.destructure();

    for x in input {
        match x {
            // if number / bool / str, just push it to the registers, and the caller will grab the last index
            Expr::Float(num) => registers.push((*num).into()),
            Expr::Int(num) => registers.push((*num).into()),
            Expr::Bool(bool) => registers.push((*bool).into()),
            Expr::String(str) => registers.push(Data::p_str(str, string_pool)),
            Expr::Var(name, markers) => {
                if let Some(Variable {
                    name: _,
                    register_id,
                    infered_type: _,
                }) = v.iter().find(|v_temp| *name == v_temp.name)
                {
                    output.push(Instr::Mov(
                        *register_id,
                        alloc_register(registers, free_registers),
                    ));
                } else {
                    throw_parser_error(src, markers, ErrType::UnknownVariable(name))
                }
            }
            Expr::Array(elems, markers) => {
                let first_type = infer_type(&elems[0], v, fns, src, dyn_libs);
                if !elems
                    .iter()
                    .all(|x| infer_type(x, v, fns, src, dyn_libs) == first_type)
                {
                    throw_parser_error(src, markers, ErrType::ArrayWithDiffType);
                }
                // create new blank array with latest id
                let array_id = {
                    array_pool.push(Vec::new());
                    array_pool.len() - 1
                };
                for elem in elems {
                    // process each array element
                    let x = compile_expr(slice::from_ref(elem), v, p, 0);
                    // if there are no instructions, then that means the element has been pushed to the registers, so pop it and push it directly to the array
                    if x.is_empty() {
                        array_pool
                            .get_mut(array_id)
                            .unwrap()
                            .push(registers.pop().unwrap());
                    } else {
                        // if there are instructions, then push everything, add a null to the array, and then add an instruction to move the element to the array at runtime with ArrayMov
                        let c_id = get_tgt_id(*x.last().unwrap()).unwrap();
                        output.extend(x);
                        array_pool.get_mut(array_id).unwrap().push(NULL);
                        output.push(Instr::ArrayMov(
                            c_id,
                            block_id,
                            (array_pool[array_id].len() - 1) as u16,
                        ));
                    }
                }
                registers.push(Data::array(array_id as u32));
            }
            // array[index]
            Expr::GetIndex(array, index, markers) => {
                let mut infered = infer_type(array, v, fns, src, dyn_libs);
                // process the array/string that is being indexed
                let mut id = get_id(array, v, p, &mut output, None, false, false, offset);
                // for each indexing operation, process the index, adjust the id variable for the next index operation, push null to registers to use GetIndex to index at runtime
                for elem in index {
                    if !is_indexable(&infered) {
                        throw_parser_error(src, markers, ErrType::NotIndexable(infered));
                    }

                    let index_infered = infer_type(elem, v, fns, src, dyn_libs);
                    if index_infered != DataType::Int {
                        throw_parser_error(src, markers, ErrType::InvalidIndexType(index_infered));
                    }
                    let f_id = get_id(elem, v, p, &mut output, None, false, false, offset);
                    free_register(f_id, free_registers, v, const_registers);
                    let dest_reg_id = alloc_register(registers, free_registers);

                    let to_push = if infered == DataType::String {
                        Instr::GetIndexString(id, f_id, dest_reg_id)
                    } else {
                        Instr::GetIndexArray(id, f_id, dest_reg_id)
                    };
                    instr_src.push((to_push, *markers));
                    output.push(to_push);

                    id = (registers.len() - 1) as u16;
                    if let DataType::Array(array_type) = infered {
                        infered = *array_type;
                    }
                }
                free_register(id, free_registers, v, const_registers);
            }
            // x[y]... = z;
            Expr::ArrayModify(array, z, w, index_markers, elem_markers) => {
                let mut array_type = infer_type(array, v, fns, src, dyn_libs);
                if !is_indexable(&array_type) {
                    throw_parser_error(src, index_markers, ErrType::NotIndexable(array_type));
                }
                // Get the id of the source array
                let mut id = get_id(array, v, p, &mut output, None, false, false, offset);

                for elem in z.iter().rev().skip(1).rev() {
                    // Check if the index is an integer
                    let t = infer_type(elem, v, fns, src, dyn_libs);
                    if t != DataType::Int {
                        throw_parser_error(src, index_markers, ErrType::InvalidIndexType(t));
                    }
                    let f_id = get_id(elem, v, p, &mut output, None, false, false, offset);

                    let dest_reg_id = alloc_register(registers, free_registers);

                    output.push(Instr::GetIndexArray(id, f_id, dest_reg_id));

                    id = dest_reg_id;
                    free_register(f_id, free_registers, v, const_registers);
                    if let DataType::Array(inner) = array_type {
                        array_type = *inner;
                    }
                }

                // get the
                let final_id = get_id(
                    z.last().unwrap(),
                    v,
                    p,
                    &mut output,
                    None,
                    false,
                    false,
                    offset,
                );

                let elem_type = infer_type(w, v, fns, src, dyn_libs);
                let elem_id = get_id(w, v, p, &mut output, None, false, false, offset);
                free_register(elem_id, free_registers, v, const_registers);
                if is_array_with_incompatible_type(&array_type, &elem_type)
                    || (array_type == DataType::String && elem_type != DataType::String)
                {
                    throw_parser_error(
                        src,
                        elem_markers,
                        ErrType::CannotPushTypeToArray(elem_type, array_type),
                    );
                }

                let to_push = if array_type == DataType::String {
                    Instr::SetElementString(id, elem_id, final_id)
                } else {
                    Instr::SetElementArray(id, elem_id, final_id)
                };
                instr_src.push((to_push, *index_markers));
                output.push(to_push);
                free_register(id, free_registers, v, const_registers);
            }
            Expr::Condition(main_condition, code, _) => {
                // get first code limit (after which there are only else(if) blocks)
                let main_code_limit = code
                    .iter()
                    .position(|x| matches!(x, Expr::ElseIfBlock(_, _) | Expr::ElseBlock(_)))
                    .unwrap_or(code.len());

                let condition_blocks_count = code.len() - main_code_limit;
                let mut conditional_jmp_instr_idx: Vec<usize> =
                    Vec::with_capacity(condition_blocks_count);
                let mut jmp_instr_idx: Vec<usize> = Vec::with_capacity(condition_blocks_count);
                let mut condition_markers: Vec<usize> = Vec::with_capacity(condition_blocks_count);

                // parse the main condition
                let condition_id =
                    get_id(main_condition, v, p, &mut output, None, true, false, offset);
                add_cmp(condition_id, &mut 0, &mut output, false);
                free_register(condition_id, free_registers, v, const_registers);
                conditional_jmp_instr_idx.push(output.len() - 1);

                let v_len = v.len();
                // parse the main code block
                let cond_code = compile_expr(&code[0..main_code_limit], v, p, output.len() as u16);
                v.truncate(v_len);
                output.extend(cond_code);
                if main_code_limit != code.len() {
                    output.push(Instr::Jmp(0));
                    jmp_instr_idx.push(output.len() - 1);
                }

                for elem in &code[main_code_limit..] {
                    if let Expr::ElseIfBlock(condition, code) = elem {
                        condition_markers.push(output.len());
                        let condition_id =
                            get_id(condition, v, p, &mut output, None, true, false, offset);
                        free_register(condition_id, free_registers, v, const_registers);
                        add_cmp(condition_id, &mut 0, &mut output, false);
                        conditional_jmp_instr_idx.push(output.len() - 1);
                        let v_len = v.len();
                        let cond_code = compile_expr(code, v, p, output.len() as u16);
                        v.truncate(v_len);
                        debug!("COND CODE IS {cond_code:?}");
                        output.extend(cond_code);
                        output.push(Instr::Jmp(0));
                        jmp_instr_idx.push(output.len() - 1);
                    } else if let Expr::ElseBlock(code) = elem {
                        condition_markers.push(output.len());
                        let v_len = v.len();
                        let cond_code = compile_expr(code, v, p, output.len() as u16);
                        v.truncate(v_len);
                        debug!("COND CODE IS {cond_code:?}");
                        output.extend(cond_code);
                    }
                }

                for y in jmp_instr_idx {
                    let diff = output.len() - y;
                    output[y] = Instr::Jmp(diff as u16);
                }
                for (i, y) in conditional_jmp_instr_idx.iter().enumerate() {
                    let diff = if i >= condition_markers.len() {
                        output.len() - y
                    } else {
                        condition_markers[i] - y
                    };
                    if let Some(
                        Instr::IsFalseJmp(_, jump_size)
                        | Instr::SupEqFloatJmp(_, _, jump_size)
                        | Instr::SupEqIntJmp(_, _, jump_size)
                        | Instr::SupFloatJmp(_, _, jump_size)
                        | Instr::SupIntJmp(_, _, jump_size)
                        | Instr::InfEqFloatJmp(_, _, jump_size)
                        | Instr::InfEqIntJmp(_, _, jump_size)
                        | Instr::InfFloatJmp(_, _, jump_size)
                        | Instr::InfIntJmp(_, _, jump_size)
                        | Instr::NotEqJmp(_, _, jump_size)
                        | Instr::EqJmp(_, _, jump_size),
                    ) = output.get_mut(*y)
                    {
                        *jump_size = diff as u16;
                    }
                }
            }
            Expr::WhileBlock(condition, code) => {
                // try to optimize it (if it's a summation loop)
                // if while_loop_summation(&mut output, v, p, condition, code) {
                //     continue;
                // }
                // parse the condition, get its id
                let condition_id = get_id(condition, v, p, &mut output, None, true, false, offset);
                free_register(condition_id, free_registers, v, const_registers);

                // parse the code block, clone the vars to avoid overriding anything
                let v_len = v.len();
                let loop_id = block_id + 1;

                let mut cond_code = compile_expr(code, v, p, offset + output.len() as u16);
                v.truncate(v_len);

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
                let array_type = infer_type(array, v, fns, src, dyn_libs);
                let array = get_id(array, v, p, &mut output, None, false, false, offset);

                // try to optimize it
                // if for_loop_summation(&mut output, registers, v, array, code) {
                //     continue;
                // }

                // add an instruction to get array length (func id 2 = len)
                // let array_len_id = registers.len() as u16;
                // registers.push(NULL);
                let array_len_id = alloc_register(registers, free_registers);

                output.push(Instr::CallLibFunc(LibFunc::Len, array, array_len_id));

                // set up the id of the index variable (0..len)
                let index_id = registers.len() as u16;
                registers.push(0.into());

                // do the 'i < len' condition, set up the condition's id (true/false)
                // let condition_id = registers.len() as u16;
                // registers.push(NULL);
                let condition_id = alloc_register(registers, free_registers);

                output.push(Instr::InfInt(index_id, array_len_id, condition_id));

                // set up the variable for the current element (for current_element_id in ... {}) => current_element_id = array[index]
                // let current_element_id = registers.len() as u16;
                // if real_var {
                //     registers.push(NULL);
                // }
                // dbg!(free_registers);
                let current_element_id = alloc_register(registers, free_registers);

                let v_len = v.len();
                // parse everything, add the current element variable to temp_vars so that the loop code can interact with it
                v.push(Variable {
                    name: if real_var {
                        var_name.clone()
                    } else {
                        SmolStr::new_inline("")
                    },
                    register_id: current_element_id,
                    infered_type: match &array_type {
                        DataType::String => DataType::String,
                        DataType::Array(a_type) => *a_type.clone(),
                        _ => todo!("For loop invalid type"),
                    },
                });
                let loop_id = block_id + 1;
                let mut cond_code = compile_expr(code, v, p, output.len() as u16);
                // Clean up variables
                v.truncate(v_len);

                // add the condition ('i < len') jumping logic
                let mut len = (cond_code.len() + 3) as u16 + if real_var { 1 } else { 0 };
                add_cmp(condition_id, &mut len, &mut output, true);

                // instruction to make current_element actually hold the array index's value
                if real_var {
                    if array_type == DataType::String {
                        output.push(Instr::GetIndexString(array, index_id, current_element_id));
                    } else {
                        output.push(Instr::GetIndexArray(array, index_id, current_element_id));
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

                free_register(array_len_id, free_registers, v, const_registers);
                free_register(index_id, free_registers, v, const_registers);
                free_register(condition_id, free_registers, v, const_registers);
                // if real_var {
                free_register(current_element_id, free_registers, v, const_registers);
                // }
            }
            Expr::IntForLoop(var_name, start_elem, end_elem, code, markers1, markers2) => {
                // Check start elem type
                let t1 = infer_type(start_elem, v, fns, src, dyn_libs);
                if t1 != DataType::Int {
                    throw_parser_error(src, markers1, ErrType::InvalidType(DataType::Int, t1));
                }
                // Check end elem type
                let t2 = infer_type(end_elem, v, fns, src, dyn_libs);
                if t2 != DataType::Int {
                    throw_parser_error(src, markers2, ErrType::InvalidType(DataType::Int, t2));
                }
                let elem_id = get_id(start_elem, v, p, &mut output, None, false, false, offset);
                let end_elem_id = get_id(end_elem, v, p, &mut output, None, false, false, offset);

                let v_len = v.len();
                v.push(Variable {
                    name: var_name.clone(),
                    register_id: elem_id,
                    infered_type: DataType::Int,
                });
                // Compile the code inside the loop
                let compiled_loop_code = compile_expr(code, v, p, offset + output.len() as u16);
                let compiled_loop_code_len = compiled_loop_code.len() as u16;

                // Loop logic
                // Add an InfCmp first, to check if i < end_elem
                let jmp_idx = output.len();
                output.push(Instr::SupEqIntJmp(elem_id, end_elem_id, 0));

                output.extend(compiled_loop_code);
                let len1 = output.len() as u16;
                let one_cst_id =
                    get_id(&Expr::Int(1), v, p, &mut output, None, false, false, offset);
                let len2 = output.len() as u16;
                // Do i += 1
                output.push(Instr::AddInt(elem_id, one_cst_id, elem_id));
                // Jump back to the start of the function
                output.push(Instr::JmpBack(2 + (len2 - len1) + compiled_loop_code_len));

                let exit_size = (output.len() - jmp_idx) as u16;
                output[jmp_idx] = Instr::SupEqIntJmp(elem_id, end_elem_id, exit_size);
                v.truncate(v_len);

                free_register(end_elem_id, free_registers, v, const_registers);
                free_register(elem_id, free_registers, v, const_registers);
                free_register(one_cst_id, free_registers, v, const_registers);
            }
            Expr::LoopBlock(code) => {
                let loop_id = block_id + 1;
                let v_len = v.len();
                let mut compiled = compile_expr(code, v, p, output.len() as u16);
                v.truncate(v_len);
                let code_length = compiled.len() as u16;
                parse_indef_loop_flow_control(&mut compiled, loop_id, code_length + 1);
                output.extend(compiled);
                output.push(Instr::JmpBack(code_length));
            }
            Expr::VarDeclare(x, y) => {
                let var_type = infer_type(y, v, fns, src, dyn_libs);
                let output_len = output.len();

                let var_id = if output.len() != output_len {
                    if can_move(output.last().unwrap()) {
                        let id = alloc_register(registers, free_registers);
                        move_to_id(&mut output, id);
                        id
                    } else {
                        move_to_id(&mut output, (registers.len() - 1) as u16);
                        (registers.len() - 1) as u16
                    }
                } else {
                    get_id(y, v, p, &mut output, None, false, true, offset)
                    // let id = get_id(y, v, p, &mut output, None, false, false);
                    // if const_registers.contains(&id) {
                    //     registers.push(registers[id as usize]);
                    //     (registers.len() - 1) as u16
                    // } else {
                    //     id
                    // }
                };
                v.push(Variable {
                    name: x.clone(),
                    register_id: var_id,
                    infered_type: var_type,
                });
            }
            Expr::VarAssign(name, y, markers) => {
                let var_type = infer_type(y, v, fns, src, dyn_libs);
                let id = v
                    .iter()
                    .rfind(|x| x.name == *name)
                    .unwrap_or_else(|| {
                        throw_parser_error(src, markers, ErrType::UnknownVariable(name));
                    })
                    .register_id;

                // if const_registers.contains(&id) {
                //     let new_register = alloc_register(registers, free_registers);
                //     registers[new_register as usize] = registers[id as usize];
                //     v.iter_mut().find(|x| x.name == *name).unwrap().register_id = new_register;
                // }

                let output_len = output.len();
                let obj_id = get_id(y, v, p, &mut output, Some(id), false, false, offset);
                if output.len() != output_len {
                    move_to_id(&mut output, id);
                } else {
                    output.push(Instr::Mov(obj_id, id));
                }
                if is_reg_free(v, obj_id, name) {
                    free_register(obj_id, free_registers, v, const_registers);
                }
                v.iter_mut().find(|x| x.name == *name).unwrap().infered_type = var_type;
                debug!("NEW VAR TYPES ARE {v:?}");
            }

            Expr::FunctionCall(args, namespace, markers, args_indexes) => {
                let output_id = handle_functions(
                    &mut output,
                    v,
                    p,
                    args,
                    namespace,
                    markers,
                    args_indexes,
                    offset,
                );
                if let Some(id) = output_id {
                    free_register(id, free_registers, v, const_registers);
                }
            }
            Expr::ObjFunctionCall(obj, args, namespace, obj_markers, fn_markers, args_indexes) => {
                handle_method_calls(
                    &mut output,
                    v,
                    p,
                    obj,
                    args,
                    namespace,
                    obj_markers,
                    fn_markers,
                    args_indexes,
                    offset,
                );
            }
            Expr::FunctionDecl(x, y, markers) => {
                if fns.iter().any(|func| &func.name == x.first().unwrap()) {
                    throw_parser_error(src, markers, ErrType::FunctionAlreadyExists(&x[0]));
                    // parser_error(
                    //     src,
                    //     *start,
                    //     *end,
                    //     "Function defined twice",
                    //     format_args!(
                    //         "Function {color_bright_blue}{style_bold}{}{color_reset}{style_reset} is already defined",
                    //         x[0]
                    //     ),
                    //     None,
                    // );
                }
                fns.push(Function {
                    name: x.first().unwrap().clone(),
                    args: x.into_iter().skip(1).cloned().collect(),
                    code: y.clone(),
                    impls: Vec::new(),
                    is_recursive: contains_recursive_call(y, x.first().unwrap()),
                    id: fn_registers.len() as u16,
                    returns_void: check_if_returns_void(y),
                });
                fn_registers.push(Vec::new());
            }
            Expr::ReturnVal(val) => {
                if let Some(x) = &**val {
                    let id = get_id(x, v, p, &mut output, None, false, false, offset);
                    if is_parsing_recursive {
                        output.push(Instr::RecursiveReturn(id));
                    } else {
                        output.push(Instr::Return(id));
                    }
                }
            }
            // Break(block_id) = EqCmp(block_id, 0, 0)
            Expr::Break => output.push(Instr::NotEqJmp(block_id, 0, 0)),
            // Break(block_id) = NotEqCmp(block_id, 0, 0)
            Expr::Continue => output.push(Instr::EqJmp(block_id, 0, 0)),
            Expr::EvalBlock(code) => {
                let v_len = v.len();
                output.extend(compile_expr(code, v, p, output.len() as u16));
                v.truncate(v_len);
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
    Pools,
    Vec<(Instr, (usize, usize))>,
    Vec<Vec<u16>>,
    Vec<DynamicLibFn>,
    usize,
    usize,
) {
    let now = std::time::Instant::now();
    let code: Vec<Expr> = grammar::FileParser::new()
        .parse((filename, contents), contents)
        .unwrap_or_else(|x| lalrpop_error::<usize, Token<'_>, &str>(x, contents, filename));
    println!("LALRPOP TIME {:.2?}", now.elapsed());
    // println!("FUNCS {functions:?}");

    let mut variables: Vec<Variable> = Vec::new();
    let mut registers: Vec<Data> = Vec::new();
    let mut pools: Pools = Pools {
        array_pool: Vec::with_capacity(20),
        string_pool: Vec::with_capacity(20),
    };
    let mut instr_src = Vec::new();
    let mut fn_registers: Vec<Vec<u16>> = Vec::new();
    let mut functions: Vec<Function> = Vec::new();
    let mut dyn_libs: Vec<Dynamiclib> = Vec::new();
    let mut id = 0;
    let mut dyn_lib_fns: Vec<DynamicLibFn> = Vec::new();
    let mut allocated_arg_count = 0;
    let mut allocated_call_depth = 0;
    let mut const_registers = Vec::new();
    let mut free_registers = Vec::new();

    for w in code {
        match w {
            Expr::FunctionDecl(x, y, _) => {
                fn_registers.push(Vec::new());
                functions.push(Function {
                    name: x[0].to_smolstr(),
                    args: x[1..].into(),
                    code: y.clone(),
                    impls: Vec::new(),
                    is_recursive: contains_recursive_call(&y, &x[0]),
                    id: (fn_registers.len() - 1) as u16,
                    returns_void: check_if_returns_void(&y),
                });
            }
            Expr::ImportDynLib(path, fn_signatures) => {
                let fns = fn_signatures
                    .iter()
                    .map(|(fn_name, fn_args, fn_return_type)| {
                        let return_val = FnSignature {
                            name: fn_name.clone(),
                            args: fn_args.clone(),
                            return_type: fn_return_type.clone(),
                            id,
                        };

                        // Convert arguments and return to libffi types
                        let arg_types: Vec<_> = fn_args.iter().map(datatype_to_c_type).collect();
                        let return_type = datatype_to_c_type(fn_return_type);
                        // Build the CIF (call interface object)
                        let cif = libffi::middle::Cif::new(arg_types.clone(), return_type.clone());
                        // Get the function's pointer (lib is stored to avoid freeing the memory)
                        let lib = unsafe { libloading::Library::new(path.as_str()).unwrap() };
                        let ptr = unsafe {
                            libffi::middle::CodePtr(
                                lib.get::<*const ()>(fn_name.as_str())
                                    .unwrap()
                                    .try_as_raw_ptr()
                                    .unwrap(),
                            )
                        };
                        dyn_lib_fns.push(DynamicLibFn {
                            arg_types: Box::from(arg_types),
                            return_type,
                            lib,
                            ptr,
                            cif,
                        });
                        id += 1;
                        return_val
                    })
                    .collect();
                dyn_libs.push(Dynamiclib {
                    name: std::path::PathBuf::from(path.as_str())
                        .file_prefix()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_smolstr(),

                    fns,
                });
            }
            _ => {}
        }
    }

    let instructions = compile_expr(
        &functions
            .iter()
            .find(|func| func.name == "main")
            .unwrap_or_else(|| {
                eprintln!(
                    "--------------\n{color_red}SPOCK RUNTIME ERROR:{color_reset}\nCannot find {color_bright_blue}{style_bold}main{style_reset}{color_reset} function\n--------------",
                );
                std::process::exit(1);
            })
            .code
            .clone(),
        &mut variables,
        &ParserData {
            registers: &mut registers,
            fns: &mut functions,
            pools: &mut pools,
            block_id: 0,
            src: (filename, contents),
            instr_src: &mut instr_src,
            is_parsing_recursive: false,
            fn_registers: &mut fn_registers,
            parsing_fn_id: None,
            dyn_libs: &mut dyn_libs,
            allocated_arg_count: &mut allocated_arg_count,
            allocated_call_depth: &mut allocated_call_depth,
            const_registers: &mut const_registers,
            free_registers: &mut free_registers,
        },
        0,
    );
    for x in fn_registers.iter_mut() {
        x.sort();
        x.dedup();
    }
    #[cfg(debug_assertions)]
    {
        crate::display::print_debug(&instructions, &registers, &pools);
    }
    (
        instructions,
        registers,
        pools,
        instr_src,
        fn_registers,
        dyn_lib_fns,
        allocated_arg_count,
        allocated_call_depth,
    )
}
