use crate::parser::Expr;
use crate::parser::Function;
use crate::parser_error;
use ariadne::*;
use inline_colorization::*;
use internment::Intern;
// use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
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

fn contains_recursive_call(content: &[Expr], fn_name: &str) -> bool {
    let mut contains_call = false;
    for content in content {
        match content {
            Expr::FunctionCall(_, namespace, _, _, _) => {
                if namespace.last().unwrap().as_str() == fn_name {
                    contains_call = true;
                }
            }
            Expr::Condition(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(y, fn_name)
                {
                    contains_call = true
                }
            }

            Expr::ElseIfBlock(x, y) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(y, fn_name)
                {
                    contains_call = true
                }
            }
            Expr::ElseBlock(x) => {
                if contains_recursive_call(x, fn_name) {
                    contains_call = true
                }
            }
            Expr::ObjFunctionCall(x, y, _, _, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(y, fn_name)
                {
                    contains_call = true
                }
            }

            // name+args -- code
            Expr::FunctionDecl(_, x, _, _) => {
                if contains_recursive_call(x, fn_name) {
                    contains_call = true
                }
            }
            Expr::GetIndex(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(y, fn_name)
                {
                    contains_call = true
                }
            }

            Expr::Mul(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Div(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Add(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Sub(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Mod(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Pow(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Eq(x, y) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::NotEq(x, y) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Sup(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::SupEq(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Inf(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::InfEq(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::BoolAnd(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::BoolOr(x, y, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name)
                    || contains_recursive_call(&[*y.clone()], fn_name)
                {
                    contains_call = true
                }
            }
            Expr::Neg(x, _, _) => {
                if contains_recursive_call(&[*x.clone()], fn_name) {
                    contains_call = true
                }
            }
            _ => continue,
        }
    }
    contains_call
}

fn track_returns(
    content: &[Expr],
    var_types: &mut Vec<(Intern<String>, DataType)>,
    fns: &[Function],
    src: (&str, &str),
    fn_name: &str,
    track_condition: bool,
) -> Vec<DataType> {
    let mut return_types: Vec<DataType> = Vec::new();
    // let mut complete_return_condition = true;
    for content in content {
        match content {
            Expr::Condition(_, code, _, _) => {
                if track_condition {
                    return_types.extend(track_returns(
                        code,
                        var_types,
                        fns,
                        src,
                        fn_name,
                        track_condition,
                    ))
                }
            }
            Expr::ElseIfBlock(_, code) => return_types.extend(track_returns(
                code,
                var_types,
                fns,
                src,
                fn_name,
                track_condition,
            )),
            Expr::ElseBlock(code) => return_types.extend(track_returns(
                code,
                var_types,
                fns,
                src,
                fn_name,
                track_condition,
            )),
            Expr::WhileBlock(_, code) => return_types.extend(track_returns(
                code,
                var_types,
                fns,
                src,
                fn_name,
                track_condition,
            )),
            Expr::ReturnVal(return_val) => {
                if let Some(val) = *return_val.to_owned() {
                    let contains_call = contains_recursive_call(&[val.clone()], fn_name);
                    if !contains_call {
                        let infered = infer_type(&val, var_types, fns, src);
                        if !return_types.contains(&infered) {
                            return_types.push(infered);
                        }
                    }
                } else {
                    if !return_types.contains(&DataType::Null) {
                        return_types.push(DataType::Null);
                    }
                }
            }
            Expr::ForLoop(_, code) => return_types.extend(track_returns(
                code,
                var_types,
                fns,
                src,
                fn_name,
                track_condition,
            )),
            Expr::EvalBlock(code) => return_types.extend(track_returns(
                code,
                var_types,
                fns,
                src,
                fn_name,
                track_condition,
            )),
            Expr::LoopBlock(code) => return_types.extend(track_returns(
                code,
                var_types,
                fns,
                src,
                fn_name,
                track_condition,
            )),
            _ => continue,
        }
    }
    return_types
}

pub fn infer_type(
    x: &Expr,
    var_types: &mut Vec<(Intern<String>, DataType)>,
    fns: &[Function],
    src: (&str, &str),
) -> DataType {
    match x {
        Expr::Var(name, start, end) => var_types
            .iter()
            .rfind(|(n, _)| n == name)
            // .unwrap()
            .unwrap_or_else(|| {
                parser_error!(
                    src.0,
                    src.1,
                    *start,
                    *end,
                    "Variable type",
                    "Unable to get variable's type"
                );
            })
            .1
            .clone(),
        Expr::Num(_) => DataType::Number,
        Expr::String(_) => DataType::String,
        Expr::Bool(_) => DataType::Bool,
        Expr::Array(x, _, _) => DataType::Array(Box::from(infer_type(&x[0], var_types, fns, src))),
        Expr::Add(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Number,
                (DataType::String, DataType::String) => DataType::String,
                (DataType::Array(type1), DataType::Array(_)) => DataType::Array(type1),
                _ => todo!("TODO ADD ERR"),
            }
        }
        Expr::Mul(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO MUL ERR"),
            }
        }
        Expr::Div(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO DIV ERR"),
            }
        }
        Expr::Sub(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO SUB ERR"),
            }
        }
        Expr::Mod(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO MOD ERR"),
            }
        }
        Expr::Pow(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Number,
                _ => todo!("TODO POW ERR"),
            }
        }
        Expr::Eq(_, _) => DataType::Bool,
        Expr::NotEq(_, _) => DataType::Bool,
        Expr::Sup(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO SUP ERR"),
            }
        }
        Expr::SupEq(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO SUPEQ ERR"),
            }
        }
        Expr::Inf(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO INF ERR"),
            }
        }
        Expr::InfEq(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Number, DataType::Number) => DataType::Bool,
                _ => todo!("TODO INFEQ ERR"),
            }
        }
        Expr::BoolAnd(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Bool, DataType::Bool) => DataType::Bool,
                _ => todo!("TODO BOOLAND ERR"),
            }
        }
        Expr::BoolOr(x, y, _, _) => {
            match (
                infer_type(x, var_types, fns, src),
                infer_type(y, var_types, fns, src),
            ) {
                (DataType::Bool, DataType::Bool) => DataType::Bool,
                _ => todo!("TODO BOOLOR ERR"),
            }
        }
        Expr::Neg(x, _, _) => match infer_type(x, var_types, fns, src) {
            DataType::Number => DataType::Number,
            _ => todo!("TODO NEG ERR"),
        },
        Expr::GetIndex(array, _, _, _) => match infer_type(array, var_types, fns, src) {
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
                "num" => DataType::Number,
                "str" => DataType::String,
                "bool" => DataType::Bool,
                "input" => DataType::String,
                "range" => DataType::Array(Box::from(DataType::Number)),
                "floor" => DataType::Number,
                "the_answer" => DataType::Number,
                function => {
                    let (_, fn_args, fn_code, _) =
                        fns.iter().find(|(a, _, _, _)| *a == function).unwrap_or_else(|| {
                            parser_error!(
                                src.0,
                                src.1,
                                *start,
                                *end,
                                "Unknown function",
                                format_args!(
                                    "Function {color_bright_blue}{style_bold}{function}{color_reset}{style_reset} does not exist or has not been declared yet"
                                )
                            );
                        });

                    let mut arg_types: Vec<usize> = Vec::with_capacity(args.len());
                    args.iter().enumerate().for_each(|(i, x)| {
                        let infered = infer_type(x, var_types, fns, src);
                        arg_types.push(var_types.len());
                        var_types.push((Intern::from_ref(&fn_args[i]), infered))
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

                    let fn_type = track_returns(fn_code, var_types, fns, src, function, true);
                    let to_return = check_poly(DataType::Poly(Box::from(fn_type)));

                    arg_types.iter().for_each(|i| {
                        var_types.remove(*i);
                    });

                    to_return
                }
            }
        }
        Expr::ObjFunctionCall(obj, _, namespace, _, _, _) => {
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
                    let obj_type = infer_type(obj, var_types, fns, src);
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
                    let obj_type = infer_type(obj, var_types, fns, src);
                    if obj_type == DataType::String {
                        DataType::String
                    } else if let DataType::Array(array_type) = obj_type {
                        DataType::Array(array_type)
                    } else {
                        todo!()
                    }
                }
                "split" => {
                    let obj_type = infer_type(obj, var_types, fns, src);
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
            types.push(infer_type(&code[0], var_types, fns, src));
            for t in &code[0..] {
                if let Expr::ElseIfBlock(_, code) = t {
                    let infered = infer_type(&code[0], var_types, fns, src);
                    if !types.contains(&infered) {
                        types.push(infered);
                    }
                } else if let Expr::ElseBlock(code) = t {
                    let infered = infer_type(&code[0], var_types, fns, src);
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
