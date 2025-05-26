use crate::parser::Expr;
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
}

pub fn infer_type(x: &Expr, var_types: &[(Intern<String>, DataType)]) -> DataType {
    match x {
        Expr::Var(name, _, _) => var_types.iter().find(|(n, _)| n == name).unwrap().1.clone(),
        Expr::Num(_) => DataType::Number,
        Expr::String(_) => DataType::String,
        Expr::Bool(_) => DataType::Bool,
        Expr::Array(x, _, _) => DataType::Array(Box::from(infer_type(&x[0], var_types))),
        Expr::Add(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Number,
            (DataType::String, DataType::String) => DataType::Number,
            (DataType::Array(_), DataType::Array(_)) => DataType::Number,
            _ => todo!("TODO ADD ERR"),
        },
        Expr::Mul(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Number,
            _ => todo!("TODO MUL ERR"),
        },
        Expr::Div(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Number,
            _ => todo!("TODO DIV ERR"),
        },
        Expr::Sub(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Number,
            _ => todo!("TODO SUB ERR"),
        },
        Expr::Mod(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Number,
            _ => todo!("TODO MOD ERR"),
        },
        Expr::Pow(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Number,
            _ => todo!("TODO POW ERR"),
        },
        Expr::Eq(_, _) => DataType::Bool,
        Expr::NotEq(_, _) => DataType::Bool,
        Expr::Sup(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Bool,
            _ => todo!("TODO SUP ERR"),
        },
        Expr::SupEq(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Bool,
            _ => todo!("TODO SUPEQ ERR"),
        },
        Expr::Inf(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Bool,
            _ => todo!("TODO INF ERR"),
        },
        Expr::InfEq(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Number, DataType::Number) => DataType::Bool,
            _ => todo!("TODO INFEQ ERR"),
        },
        Expr::BoolAnd(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Bool, DataType::Bool) => DataType::Bool,
            _ => todo!("TODO BOOLAND ERR"),
        },
        Expr::BoolOr(x, y, _, _) => match (infer_type(x, var_types), infer_type(y, var_types)) {
            (DataType::Bool, DataType::Bool) => DataType::Bool,
            _ => todo!("TODO BOOLOR ERR"),
        },
        Expr::Neg(x, _, _) => match infer_type(x, var_types) {
            DataType::Number => DataType::Number,
            _ => todo!("TODO NEG ERR"),
        },
        Expr::GetIndex(array, _, _, _) => match infer_type(array, var_types) {
            DataType::Array(array_type) => *array_type,
            DataType::String => DataType::String,
            _ => todo!(),
        },
        Expr::FunctionCall(args, namespace, _, _) => match namespace.last().unwrap().as_str() {
            "print" => DataType::Null,
            "type" => infer_type(&args[0], var_types),
            "num" => DataType::Number,
            "str" => DataType::String,
            "bool" => DataType::Bool,
            "input" => DataType::String,
            "range" => DataType::Array(Box::from(DataType::Number)),
            "floor" => DataType::Number,
            "the_answer" => DataType::Number,
            _ => todo!(),
        },
        Expr::ObjFunctionCall(obj, args, namespace, start, end) => {
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
                    let obj_type = infer_type(obj, var_types);
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
                    let obj_type = infer_type(obj, var_types);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        todo!()
                    }
                }
                "split" => {
                    let obj_type = infer_type(obj, var_types);
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
        unknown_type => todo!("TYPE: {unknown_type}"),
    }
}
