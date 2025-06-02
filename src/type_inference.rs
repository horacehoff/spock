use crate::parser::Expr;
use crate::parser::Function;
use internment::Intern;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum DataType {
    Array(Box<DataType>),
    Number,
    Bool,
    String,
    File,
    Null,
    Poly(Box<[DataType]>),
}

fn track_returns(
    content: &[Expr],
    var_types: &[(Intern<String>, DataType)],
    fns: &[Function],
) -> Vec<DataType> {
    let mut return_types: Vec<DataType> = Vec::new();
    for content in content {
        match content {
            Expr::Condition(_, code, _, _) => {
                return_types.extend(track_returns(code, var_types, fns))
            }
            Expr::ElseIfBlock(_, code) => return_types.extend(track_returns(code, var_types, fns)),
            Expr::ElseBlock(code) => return_types.extend(track_returns(code, var_types, fns)),
            Expr::WhileBlock(_, code) => return_types.extend(track_returns(code, var_types, fns)),
            Expr::ReturnVal(return_val) => {
                if let Some(val) = *return_val.to_owned() {
                    let infered = infer_type(&val, var_types, fns);
                    if !return_types.contains(&infered) {
                        return_types.push(infered);
                    }
                } else {
                    if !return_types.contains(&DataType::Null) {
                        return_types.push(DataType::Null);
                    }
                }
            }
            Expr::ForLoop(_, code) => return_types.extend(track_returns(code, var_types, fns)),
            Expr::EvalBlock(code) => return_types.extend(track_returns(code, var_types, fns)),
            Expr::LoopBlock(code) => return_types.extend(track_returns(code, var_types, fns)),
            _ => continue,
        }
    }
    return_types
}

pub fn infer_type(
    x: &Expr,
    var_types: &[(Intern<String>, DataType)],
    fns: &[Function],
) -> DataType {
    match x {
        Expr::Var(name, _, _) => var_types.iter().find(|(n, _)| n == name).unwrap().1.clone(),
        Expr::Num(_) => DataType::Number,
        Expr::String(_) => DataType::String,
        Expr::Bool(_) => DataType::Bool,
        Expr::Array(x, _, _) => DataType::Array(Box::from(infer_type(&x[0], var_types, fns))),
        Expr::Add(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Number,
                (DataType::String, DataType::String) => DataType::String,
                (DataType::Array(type1), DataType::Array(_)) => DataType::Array(type1),
                _ => todo!("TODO ADD ERR"),
            }
        }
        Expr::Mul(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO MUL ERR"),
            }
        }
        Expr::Div(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO DIV ERR"),
            }
        }
        Expr::Sub(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO SUB ERR"),
            }
        }
        Expr::Mod(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO MOD ERR"),
            }
        }
        Expr::Pow(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO POW ERR"),
            }
        }
        Expr::Eq(_, _) => DataType::Bool,
        Expr::NotEq(_, _) => DataType::Bool,
        Expr::Sup(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO SUP ERR"),
            }
        }
        Expr::SupEq(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO SUPEQ ERR"),
            }
        }
        Expr::Inf(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO INF ERR"),
            }
        }
        Expr::InfEq(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO INFEQ ERR"),
            }
        }
        Expr::BoolAnd(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Bool, DataType::Bool) => DataType::Bool,
                _ => todo!("TODO BOOLAND ERR"),
            }
        }
        Expr::BoolOr(x, y, _, _) => {
            match (infer_type(x, var_types, fns), infer_type(y, var_types, fns)) {
                (DataType::Bool, DataType::Bool) => DataType::Bool,
                _ => todo!("TODO BOOLOR ERR"),
            }
        }
        Expr::Neg(x, _, _) => match infer_type(x, var_types, fns) {
            DataType::Number => DataType::Number,
            _ => todo!("TODO NEG ERR"),
        },
        Expr::GetIndex(array, _, _, _) => match infer_type(array, var_types, fns) {
            DataType::Array(array_type) => *array_type,
            DataType::String => DataType::String,
            _ => todo!(),
        },
        Expr::FunctionCall(args, namespace, _, _, _) => match namespace.last().unwrap().as_str() {
            "print" => DataType::Null,
            "type" => DataType::String,
            "num" => DataType::Number,
            "str" => DataType::String,
            "bool" => DataType::Bool,
            "input" => DataType::String,
            "range" => DataType::Array(Box::from(DataType::Number)),
            "floor" => DataType::Number,
            "the_answer" => DataType::Number,
            function => {
                let (_, _, fn_code, _, _) =
                    fns.iter().find(|(a, _, _, _, _)| *a == function).unwrap();

                check_poly(DataType::Poly(Box::from(track_returns(
                    fn_code, var_types, fns,
                ))))
            }
        },
        Expr::ObjFunctionCall(obj, args, namespace, start, end, _) => {
            match namespace.last().unwrap().as_str() {
                "uppercase" => DataType::String,
                "lowercase" => DataType::String,
                "len" => DataType::Number,
                "contains" => DataType::Bool,
                "trim" => DataType::String,
                "trim_sequence" => DataType::String,
                "index" => DataType::Number,
                "is_num" => DataType::Bool,
                "trim_left" => DataType::String,
                "trim_right" => DataType::String,
                "trim_sequence_left" => DataType::String,
                "trim_sequence_right" => DataType::String,
                "rindex" => DataType::Number,
                "repeat" => {
                    let obj_type = infer_type(obj, var_types, fns);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        todo!()
                    }
                }
                "push" => DataType::Null,
                "sqrt" => DataType::Number,
                "round" => DataType::Number,
                "abs" => DataType::Number,
                // io::read
                "read" => DataType::String,
                // io::write
                "write" => DataType::Null,
                "reverse" => {
                    let obj_type = infer_type(obj, var_types, fns);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        todo!()
                    }
                }
                "split" => {
                    let obj_type = infer_type(obj, var_types, fns);
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
            types.push(infer_type(&code[0], var_types, fns));
            for t in &code[0..] {
                if let Expr::ElseIfBlock(_, code) = t {
                    let infered = infer_type(&code[0], var_types, fns);
                    if !types.contains(&infered) {
                        types.push(infered);
                    }
                } else if let Expr::ElseBlock(code) = t {
                    let infered = infer_type(&code[0], var_types, fns);
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
        let first_type = &elems[0];
        if elems.iter().all(|x| x == first_type) {
            first_type.clone()
        } else {
            data
        }
    } else {
        unreachable!()
    }
}
