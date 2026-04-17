use crate::errors::ErrType;
use crate::errors::dev_error;
use crate::errors::throw_parser_error;
use crate::parser::Expr;
use crate::parser::symbol_of_expr;
use crate::parser_data::Dynamiclib;
use crate::parser_data::FnSignature;
use crate::parser_data::Function;
use crate::parser_data::Variable;
use libffi::middle::Type;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum DataType {
    Array(Box<DataType>),
    Float,
    Int,
    Bool,
    String,
    File,
    Null,
    Poly(Box<[DataType]>),
    /// Fn (\[arg_types ... return_type\]) => return_type is always specified
    Fn(Box<[DataType]>),
}

pub fn is_indexable(x: &DataType) -> bool {
    matches!(x, DataType::String | DataType::Array(_))
}

fn contains_recursive_call_expr(expr: &Expr, fn_name: &str) -> bool {
    contains_recursive_call(std::slice::from_ref(expr), fn_name)
}

// Check if given function body contains a call to that same function
pub fn contains_recursive_call(content: &[Expr], fn_name: &str) -> bool {
    for content in content {
        match content {
            Expr::FunctionCall(_, namespace, _, _) => {
                if namespace.last().unwrap().as_str() == fn_name {
                    return true;
                }
            }
            Expr::Condition(x, y, _) => {
                if contains_recursive_call_expr(x, fn_name) || contains_recursive_call(y, fn_name) {
                    return true;
                }
            }

            Expr::ElseIfBlock(x, y) => {
                if contains_recursive_call_expr(x, fn_name) || contains_recursive_call(y, fn_name) {
                    return true;
                }
            }
            Expr::ElseBlock(x) => {
                if contains_recursive_call(x, fn_name) {
                    return true;
                }
            }
            Expr::ObjFunctionCall(x, y, _, _, _, _) => {
                if contains_recursive_call_expr(x, fn_name) || contains_recursive_call(y, fn_name) {
                    return true;
                }
            }
            Expr::ReturnVal(code) => {
                if let Some(code) = code.as_ref()
                    && contains_recursive_call_expr(code, fn_name)
                {
                    return true;
                }
            }
            // name+args -- code
            Expr::FunctionDecl(_, x, _) => {
                if contains_recursive_call(x, fn_name) {
                    return true;
                }
            }
            Expr::GetIndex(x, y, _) => {
                if contains_recursive_call_expr(x, fn_name) || contains_recursive_call(y, fn_name) {
                    return true;
                }
            }
            Expr::Mul(x, y, _)
            | Expr::Div(x, y, _)
            | Expr::Add(x, y, _)
            | Expr::Sub(x, y, _)
            | Expr::Mod(x, y, _)
            | Expr::Pow(x, y, _)
            | Expr::Eq(x, y)
            | Expr::NotEq(x, y)
            | Expr::Sup(x, y, _)
            | Expr::SupEq(x, y, _)
            | Expr::Inf(x, y, _)
            | Expr::InfEq(x, y, _)
            | Expr::BoolAnd(x, y, _)
            | Expr::BoolOr(x, y, _) => {
                if contains_recursive_call_expr(x, fn_name)
                    || contains_recursive_call_expr(y, fn_name)
                {
                    return true;
                }
            }
            Expr::Neg(x, _) => {
                if contains_recursive_call_expr(x, fn_name) {
                    return true;
                }
            }
            _ => continue,
        }
    }
    false
}

pub fn check_if_returns_void(content: &[Expr]) -> bool {
    for content in content {
        match content {
            Expr::ElseIfBlock(_, code)
            | Expr::ElseBlock(code)
            | Expr::Condition(_, code, _)
            | Expr::WhileBlock(_, code)
            | Expr::ForLoop(_, code)
            | Expr::EvalBlock(code)
            | Expr::LoopBlock(code)
            | Expr::IntForLoop(_, _, _, code, _, _) => {
                if !check_if_returns_void(code) {
                    return false;
                }
            }
            Expr::ReturnVal(return_val) => {
                if return_val.is_some() {
                    return false;
                }
            }
            _ => continue,
        }
    }
    true
}

pub fn track_returns(
    content: &[Expr],
    v: &mut Vec<Variable>,
    fns: &[Function],
    src: (&str, &str),
    fn_name: &str,
    track_condition: bool,
    dyn_libs: &[Dynamiclib],
) -> Vec<DataType> {
    let mut return_types: Vec<DataType> = Vec::new();
    for content in content {
        match content {
            Expr::Condition(_, code, _) => {
                if track_condition {
                    return_types.extend(track_returns(
                        code,
                        v,
                        fns,
                        src,
                        fn_name,
                        track_condition,
                        dyn_libs,
                    ));
                }
            }
            Expr::ElseIfBlock(_, code) | Expr::ElseBlock(code) => {
                let to_return =
                    track_returns(code, v, fns, src, fn_name, track_condition, dyn_libs);
                if !to_return.is_empty() || contains_recursive_call(code, fn_name) {
                    return_types.extend(to_return)
                } else {
                    return_types.push(DataType::Null);
                }
            }
            Expr::WhileBlock(_, code)
            | Expr::ForLoop(_, code)
            | Expr::EvalBlock(code)
            | Expr::LoopBlock(code)
            | Expr::IntForLoop(_, _, _, code, _, _) => return_types.extend(track_returns(
                code,
                v,
                fns,
                src,
                fn_name,
                track_condition,
                dyn_libs,
            )),
            Expr::ReturnVal(return_val) => {
                if let Some(val) = return_val.as_ref() {
                    let contains_call = contains_recursive_call_expr(val, fn_name);
                    if !contains_call {
                        let infered = infer_type(val, v, fns, src, dyn_libs);
                        if !return_types.contains(&infered) {
                            return_types.push(infered);
                        }
                    }
                } else if !return_types.contains(&DataType::Null) {
                    return_types.push(DataType::Null);
                }
            }
            _ => continue,
        }
    }
    return_types
}

pub fn infer_type(
    e: &Expr,
    v: &mut Vec<Variable>,
    fns: &[Function],
    src: (&str, &str),
    // p: &ParserData,
    dyn_libs: &[Dynamiclib],
) -> DataType {
    // let (_, _, _, _, _, _, _, _, _, dyn_libs, _, _, _, free_registers) = p.destructure();
    match e {
        Expr::Var(name, markers) => v
            .iter()
            .rfind(|x| &x.name == name)
            .unwrap_or_else(|| {
                throw_parser_error(src, markers, ErrType::CannotInferType(name));
            })
            .infered_type
            .clone(),
        Expr::Float(_) => DataType::Float,
        Expr::Int(_) => DataType::Int,
        Expr::String(_) => DataType::String,
        Expr::Bool(_) => DataType::Bool,
        Expr::Array(x, _) => DataType::Array(Box::from(infer_type(&x[0], v, fns, src, dyn_libs))),
        Expr::Add(x, y, markers) => {
            match (
                infer_type(x, v, fns, src, dyn_libs),
                infer_type(y, v, fns, src, dyn_libs),
            ) {
                (DataType::Float, DataType::Float) => DataType::Float,
                (DataType::Int, DataType::Int) => DataType::Int,
                (DataType::String, DataType::String) => DataType::String,
                (DataType::Array(type1), DataType::Array(_)) => DataType::Array(type1),
                (l, r) => throw_parser_error(src, markers, ErrType::OpError(l, r, "+")),
            }
        }
        Expr::Mul(x, y, markers)
        | Expr::Div(x, y, markers)
        | Expr::Sub(x, y, markers)
        | Expr::Mod(x, y, markers)
        | Expr::Pow(x, y, markers) => {
            match (
                infer_type(x, v, fns, src, dyn_libs),
                infer_type(y, v, fns, src, dyn_libs),
            ) {
                (DataType::Float, DataType::Float) => DataType::Float,
                (DataType::Int, DataType::Int) => DataType::Int,
                (l, r) => {
                    throw_parser_error(src, markers, ErrType::OpError(l, r, symbol_of_expr(e)))
                }
            }
        }
        Expr::Eq(_, _) => DataType::Bool,
        Expr::NotEq(_, _) => DataType::Bool,
        Expr::Sup(x, y, markers)
        | Expr::SupEq(x, y, markers)
        | Expr::Inf(x, y, markers)
        | Expr::InfEq(x, y, markers) => {
            match (
                infer_type(x, v, fns, src, dyn_libs),
                infer_type(y, v, fns, src, dyn_libs),
            ) {
                (DataType::Float, DataType::Float) => DataType::Bool,
                (DataType::Int, DataType::Int) => DataType::Bool,
                (l, r) => {
                    throw_parser_error(src, markers, ErrType::OpError(l, r, symbol_of_expr(e)))
                }
            }
        }
        Expr::BoolAnd(x, y, markers) | Expr::BoolOr(x, y, markers) => {
            match (
                infer_type(x, v, fns, src, dyn_libs),
                infer_type(y, v, fns, src, dyn_libs),
            ) {
                (DataType::Bool, DataType::Bool) => DataType::Bool,
                (l, r) => throw_parser_error(src, markers, ErrType::OpError(l, r, "||")),
            }
        }
        Expr::Neg(x, _) => match infer_type(x, v, fns, src, dyn_libs) {
            DataType::Float => DataType::Float,
            DataType::Int => DataType::Int,
            _ => todo!("TODO NEG ERR"),
        },
        Expr::GetIndex(array, _, _) => match infer_type(array, v, fns, src, dyn_libs) {
            DataType::Array(array_type) => *array_type,
            DataType::String => DataType::String,
            _ => todo!(),
        },
        Expr::FunctionCall(args, namespace, markers, _) => {
            if namespace.len() == 1 && &namespace[0] == "io" {
                match namespace.last().unwrap().as_str() {
                    "open" => return DataType::File,
                    "delete" => return DataType::Null,
                    _ => unreachable!(),
                }
            }
            match namespace.last().unwrap().as_str() {
                "print" => DataType::Null,
                "type" => DataType::String,
                "float" => DataType::Float,
                "int" => DataType::Int,
                "str" => DataType::String,
                "bool" => DataType::Bool,
                "input" => DataType::String,
                "range" => DataType::Array(Box::from(DataType::Int)),
                "the_answer" => DataType::Int,
                // File System
                "read" => DataType::String,
                "exists" => DataType::Bool,
                "write" => DataType::Null,
                "append" => DataType::Null,
                "delete" => DataType::Null,
                "delete_dir" => DataType::Null,
                function_name => {
                    if let Some(lib) = dyn_libs.iter().find(|l| l.name == namespace[0]) {
                        if let Some(FnSignature {
                            name: _,
                            args: _,
                            return_type: fn_return_type,
                            id: _,
                        }) = lib.fns.iter().find(|x| x.name == function_name)
                        {
                            return fn_return_type.clone();
                        }
                    }

                    let Function {
                        name: _,
                        args: fn_args,
                        code: fn_code,
                        impls: _,
                        is_recursive: _,
                        id: _,
                        returns_void: _,
                    } = fns
                        .iter()
                        .find(|func| func.name == function_name)
                        .unwrap_or_else(|| {
                            throw_parser_error(
                                src,
                                markers,
                                ErrType::UnknownFunction(function_name),
                            );
                        });

                    let mut arg_types: Vec<usize> = Vec::with_capacity(args.len());
                    args.iter().enumerate().for_each(|(i, x)| {
                        let infered_type = infer_type(x, v, fns, src, dyn_libs);
                        arg_types.push(v.len());
                        // 0 => placeholder id, it's never used
                        v.push(Variable {
                            name: fn_args[i].clone(),
                            register_id: 0,
                            infered_type,
                        });
                    });

                    // ----- MORE COMPLEX SOLUTION (DOES NOT ALLOW NULL OPS) -----
                    // let mut fn_type = [
                    //     track_returns(fn_code, var_types, fns, src, function, false),
                    //     track_returns(fn_code, var_types, fns, src, function, true),
                    // ]
                    // .concat();
                    // fn_type.dedup();
                    // -----

                    let fn_type =
                        track_returns(fn_code, v, fns, src, function_name, true, dyn_libs);
                    let to_return = if !fn_type.is_empty() {
                        // If function returns anything, check if it returns the same thing each time
                        check_poly(DataType::Poly(Box::from(fn_type)))
                    } else {
                        // If function doesn't return anything, return nothing
                        DataType::Null
                    };

                    arg_types.iter().for_each(|i| {
                        v.remove(*i);
                    });

                    to_return
                }
            }
        }
        Expr::ObjFunctionCall(obj, _, namespace, _, _, _) => {
            match namespace.last().unwrap().as_str() {
                "uppercase" => DataType::String,
                "lowercase" => DataType::String,
                "starts_with" => DataType::Bool,
                "ends_with" => DataType::Bool,
                "replace" => DataType::String,
                "len" => DataType::Int,
                "contains" => DataType::Bool,
                "trim" => DataType::String,
                "trim_sequence" => DataType::String,
                "find" => DataType::Int,
                "is_float" => DataType::Bool,
                "is_int" => DataType::Bool,
                "trim_left" => DataType::String,
                "trim_right" => DataType::String,
                "trim_sequence_left" => DataType::String,
                "trim_sequence_right" => DataType::String,
                "repeat" => {
                    let obj_type = infer_type(obj, v, fns, src, dyn_libs);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        unreachable!()
                    }
                }
                "push" => DataType::Null,
                "sqrt" => DataType::Float,
                "round" => DataType::Float,
                "floor" => DataType::Float,
                "abs" => {
                    let obj_type = infer_type(obj, v, fns, src, dyn_libs);
                    if obj_type == DataType::Float {
                        DataType::Float
                    } else if obj_type == DataType::Int {
                        DataType::Int
                    } else {
                        unreachable!()
                    }
                }
                "reverse" => {
                    let obj_type = infer_type(obj, v, fns, src, dyn_libs);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        todo!()
                    }
                }
                "split" => DataType::Array(Box::from(DataType::String)),
                "partition" => {
                    let obj_type = infer_type(obj, v, fns, src, dyn_libs);
                    if let DataType::Array(array_type) = obj_type {
                        DataType::Array(Box::from(DataType::Array(array_type)))
                    } else {
                        unreachable!()
                    }
                }
                "join" => DataType::String,
                "remove" => DataType::Null,

                _ => todo!(),
            }
        }
        Expr::Condition(_, code, _) => {
            let mut types: Vec<DataType> = Vec::with_capacity(code.len());
            types.push(infer_type(&code[0], v, fns, src, dyn_libs));
            for t in &code[0..] {
                if let Expr::ElseIfBlock(_, code) = t {
                    let infered = infer_type(&code[0], v, fns, src, dyn_libs);
                    if !types.contains(&infered) {
                        types.push(infered);
                    }
                } else if let Expr::ElseBlock(code) = t {
                    let infered = infer_type(&code[0], v, fns, src, dyn_libs);
                    if !types.contains(&infered) {
                        types.push(infered);
                    }
                }
            }
            check_poly(DataType::Poly(Box::from(types)))
        }
        unknown_type => todo!("TYPE: {unknown_type:?}"),
    }
}

pub fn check_poly(data: DataType) -> DataType {
    if let DataType::Poly(ref elems) = data {
        if !elems.is_empty() {
            let first_type = &elems[0];
            if elems.iter().all(|x| x == first_type) {
                first_type.clone()
            } else {
                data
            }
        } else {
            dev_error(
                "type_inference.rs",
                "check_poly",
                format_args!("DataType::Poly is empty"),
            )
        }
    } else {
        dev_error(
            "type_inference.rs",
            "check_poly",
            format_args!("Received data : {data} and not data : DataType::Poly"),
        )
    }
}

pub fn is_array_with_incompatible_type(t: &DataType, array_elem_type: &DataType) -> bool {
    if let DataType::Array(array_type) = t
        && array_type.as_ref() != array_elem_type
    {
        true
    } else {
        false
    }
}

// WIP
pub fn datatype_to_c_type(x: &DataType) -> Type {
    match x {
        DataType::Int => libffi::middle::Type::i32(),
        DataType::Float => libffi::middle::Type::f64(),
        DataType::Null => libffi::middle::Type::void(),
        _ => todo!(),
    }
}
