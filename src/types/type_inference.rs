use crate::display::op_error;
use crate::display::parser_error;
use crate::parser::Expr;
use crate::parser::Function;
use crate::parser::Variable;
use crate::parser::symbol_of_expr;
use inline_colorization::*;
use internment::Intern;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
#[repr(C)]
pub enum DataType {
    Array(Box<DataType>),
    Float,
    Int,
    Bool,
    String,
    File,
    Null,
    Poly(Box<[DataType]>),
}

fn contains_recursive_call_expr(expr: &Expr, fn_name: &str) -> bool {
    contains_recursive_call(std::slice::from_ref(expr), fn_name)
}

// Check if given function body contains a call to that same function
pub fn contains_recursive_call(content: &[Expr], fn_name: &str) -> bool {
    for content in content {
        match content {
            Expr::FunctionCall(_, namespace, _, _, _) => {
                if namespace.last().unwrap().as_str() == fn_name {
                    return true;
                }
            }
            Expr::Condition(x, y, _, _) => {
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
            Expr::FunctionDecl(_, x, _, _) => {
                if contains_recursive_call(x, fn_name) {
                    return true;
                }
            }
            Expr::GetIndex(x, y, _, _) => {
                if contains_recursive_call_expr(x, fn_name) || contains_recursive_call(y, fn_name) {
                    return true;
                }
            }
            Expr::Mul(x, y, _, _)
            | Expr::Div(x, y, _, _)
            | Expr::Add(x, y, _, _)
            | Expr::Sub(x, y, _, _)
            | Expr::Mod(x, y, _, _)
            | Expr::Pow(x, y, _, _)
            | Expr::Eq(x, y)
            | Expr::NotEq(x, y)
            | Expr::Sup(x, y, _, _)
            | Expr::SupEq(x, y, _, _)
            | Expr::Inf(x, y, _, _)
            | Expr::InfEq(x, y, _, _)
            | Expr::BoolAnd(x, y, _, _)
            | Expr::BoolOr(x, y, _, _) => {
                if contains_recursive_call_expr(x, fn_name)
                    || contains_recursive_call_expr(y, fn_name)
                {
                    return true;
                }
            }
            Expr::Neg(x, _, _) => {
                if contains_recursive_call_expr(x, fn_name) {
                    return true;
                }
            }
            _ => continue,
        }
    }
    false
}

fn track_returns(
    content: &[Expr],
    v: &mut Vec<Variable>,
    fns: &[Function],
    src: (&str, &str),
    fn_name: &str,
    track_condition: bool,
) -> Vec<DataType> {
    let mut return_types: Vec<DataType> = Vec::new();
    for content in content {
        match content {
            Expr::Condition(_, code, _, _) => {
                if track_condition {
                    return_types.extend(track_returns(code, v, fns, src, fn_name, track_condition));
                }
            }
            Expr::ElseIfBlock(_, code) | Expr::ElseBlock(code) => {
                let to_return = track_returns(code, v, fns, src, fn_name, track_condition);
                if !to_return.is_empty() || contains_recursive_call(code, fn_name) {
                    return_types.extend(to_return)
                } else {
                    return_types.push(DataType::Null);
                }
            }
            Expr::WhileBlock(_, code) => {
                return_types.extend(track_returns(code, v, fns, src, fn_name, track_condition))
            }
            Expr::ReturnVal(return_val) => {
                if let Some(val) = return_val.as_ref() {
                    let contains_call = contains_recursive_call_expr(val, fn_name);
                    if !contains_call {
                        let infered = infer_type(val, v, fns, src);
                        if !return_types.contains(&infered) {
                            return_types.push(infered);
                        }
                    }
                } else if !return_types.contains(&DataType::Null) {
                    return_types.push(DataType::Null);
                }
            }
            Expr::ForLoop(_, code) => {
                return_types.extend(track_returns(code, v, fns, src, fn_name, track_condition))
            }
            Expr::EvalBlock(code) => {
                return_types.extend(track_returns(code, v, fns, src, fn_name, track_condition))
            }
            Expr::LoopBlock(code) => {
                return_types.extend(track_returns(code, v, fns, src, fn_name, track_condition))
            }
            _ => continue,
        }
    }
    return_types
}

pub fn infer_type(
    x: &Expr,
    v: &mut Vec<Variable>,
    fns: &[Function],
    src: (&str, &str),
) -> DataType {
    match x {
        Expr::Var(name, start, end) => v
            .iter()
            .rfind(|x| &x.name == name)
            .unwrap_or_else(|| {
                parser_error(
                    src,
                    *start,
                    *end,
                    "Variable type",
                    "Unable to get variable's type",
                    "",
                );
            })
            .infered_type
            .clone(),
        Expr::Float(_) => DataType::Float,
        Expr::Int(_) => DataType::Int,
        Expr::String(_) => DataType::String,
        Expr::Bool(_) => DataType::Bool,
        Expr::Array(x, _, _) => DataType::Array(Box::from(infer_type(&x[0], v, fns, src))),
        Expr::Add(x, y, start, end) => {
            match (infer_type(x, v, fns, src), infer_type(y, v, fns, src)) {
                (DataType::Float, DataType::Float) => DataType::Float,
                (DataType::Int, DataType::Int) => DataType::Int,
                (DataType::String, DataType::String) => DataType::String,
                (DataType::Array(type1), DataType::Array(_)) => DataType::Array(type1),
                (a, b) => {
                    op_error(src, a, b, "+", *start, *end);
                }
            }
        }
        Expr::Mul(x, y, start, end)
        | Expr::Div(x, y, start, end)
        | Expr::Sub(x, y, start, end)
        | Expr::Mod(x, y, start, end)
        | Expr::Pow(x, y, start, end) => {
            match (infer_type(x, v, fns, src), infer_type(y, v, fns, src)) {
                (DataType::Float, DataType::Float) => DataType::Float,
                (DataType::Int, DataType::Int) => DataType::Int,
                (a, b) => {
                    op_error(src, a, b, symbol_of_expr(x), *start, *end);
                }
            }
        }
        Expr::Eq(_, _) => DataType::Bool,
        Expr::NotEq(_, _) => DataType::Bool,
        Expr::Sup(x, y, start, end)
        | Expr::SupEq(x, y, start, end)
        | Expr::Inf(x, y, start, end)
        | Expr::InfEq(x, y, start, end) => {
            match (infer_type(x, v, fns, src), infer_type(y, v, fns, src)) {
                (DataType::Float, DataType::Float) => DataType::Bool,
                (DataType::Int, DataType::Int) => DataType::Bool,
                (a, b) => {
                    op_error(src, a, b, symbol_of_expr(x), *start, *end);
                }
            }
        }
        Expr::BoolAnd(x, y, start, end) | Expr::BoolOr(x, y, start, end) => {
            match (infer_type(x, v, fns, src), infer_type(y, v, fns, src)) {
                (DataType::Bool, DataType::Bool) => DataType::Bool,
                (a, b) => {
                    op_error(src, a, b, "||", *start, *end);
                }
            }
        }
        Expr::Neg(x, _, _) => match infer_type(x, v, fns, src) {
            DataType::Float => DataType::Float,
            DataType::Int => DataType::Int,
            _ => todo!("TODO NEG ERR"),
        },
        Expr::GetIndex(array, _, _, _) => match infer_type(array, v, fns, src) {
            DataType::Array(array_type) => *array_type,
            DataType::String => DataType::String,
            _ => todo!(),
        },
        Expr::FunctionCall(args, namespace, start, end, _) => {
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
                "floor" => DataType::Float,
                "the_answer" => DataType::Int,
                function => {
                    let (_, fn_args, fn_code, _, _, _) =
                        fns.iter().find(|(a, _, _, _, _, _)| *a == function).unwrap_or_else(|| {
                            parser_error(
                                src,
                                *start,
                                *end,
                                "Unknown function",
                                &format!(
                                    "Function {color_bright_blue}{style_bold}{function}{color_reset}{style_reset} does not exist or has not been declared yet"
                                ),""
                            );
                        });

                    let mut arg_types: Vec<usize> = Vec::with_capacity(args.len());
                    args.iter().enumerate().for_each(|(i, x)| {
                        let infered_type = infer_type(x, v, fns, src);
                        arg_types.push(v.len());
                        // 0 => placeholder id, it's never used
                        v.push(Variable {
                            name: Intern::from_ref(&fn_args[i]),
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

                    // fn dedup(v: &mut Vec<DataType>) {
                    //     let mut set = HashSet::new();
                    //     v.retain(|x| set.insert(x.clone()));
                    // }
                    // dedup(&mut fn_type);
                    // -----

                    let fn_type = track_returns(fn_code, v, fns, src, function, true);
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
                "len" => DataType::Int,
                "contains" => DataType::Bool,
                "trim" => DataType::String,
                "trim_sequence" => DataType::String,
                "index" => DataType::Int,
                "is_num" => DataType::Bool,
                "trim_left" => DataType::String,
                "trim_right" => DataType::String,
                "trim_sequence_left" => DataType::String,
                "trim_sequence_right" => DataType::String,
                "rindex" => DataType::Int,
                "repeat" => {
                    let obj_type = infer_type(obj, v, fns, src);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        todo!()
                    }
                }
                "push" => DataType::Null,
                "sqrt" => DataType::Float,
                "round" => DataType::Float,
                "abs" => DataType::Float,
                // io::read => doesn't work
                "read" => DataType::String,
                // io::write => doesn't work
                "write" => DataType::Null,
                "reverse" => {
                    let obj_type = infer_type(obj, v, fns, src);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        todo!()
                    }
                }
                "split" => {
                    let obj_type = infer_type(obj, v, fns, src);
                    if obj_type == DataType::String {
                        DataType::Array(Box::from(DataType::String))
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(Box::from(DataType::Array(array_type)))
                    } else {
                        todo!()
                    }
                }
                "remove" => DataType::Null,
                _ => todo!(),
            }
        }
        Expr::Condition(_, code, _, _) => {
            let mut types: Vec<DataType> = Vec::with_capacity(code.len());
            types.push(infer_type(&code[0], v, fns, src));
            for t in &code[0..] {
                if let Expr::ElseIfBlock(_, code) = t {
                    let infered = infer_type(&code[0], v, fns, src);
                    if !types.contains(&infered) {
                        types.push(infered);
                    }
                } else if let Expr::ElseBlock(code) = t {
                    let infered = infer_type(&code[0], v, fns, src);
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

fn check_poly(data: DataType) -> DataType {
    if let DataType::Poly(ref elems) = data {
        if !elems.is_empty() {
            let first_type = &elems[0];
            if elems.iter().all(|x| x == first_type) {
                first_type.clone()
            } else {
                data
            }
        } else {
            panic!();
        }
    } else {
        unreachable!()
    }
}
