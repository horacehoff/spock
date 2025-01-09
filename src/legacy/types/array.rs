use crate::error_msg;
use crate::get_printable_type;
use crate::parser::{Operator, ParserInstr};
use crate::util::error;

pub fn array_ops(
    x: &[ParserInstr],
    output: ParserInstr,
    current_operator: Operator,
) -> ParserInstr {
    if let ParserInstr::Integer(value) = output {
        if current_operator == Operator::Multiply {
            let mut new_vec: Vec<ParserInstr> = Vec::with_capacity(value as usize * x.len());
            for _ in 0..value {
                new_vec.append(&mut x.to_owned());
            }
            ParserInstr::Array(new_vec, false, false)
        } else {
            error(
                &format!(
                    "Cannot perform operation '{current_operator:?}' between Array and Integer"
                ),
                "",
            );
            ParserInstr::Null
        }
    } else {
        error(
            &format!(
                "Cannot perform operation '{:?}' between {:?} and Integer",
                current_operator,
                get_printable_type!(output)
            ),
            "",
        );
        ParserInstr::Null
    }
}

#[macro_export]
macro_rules! array_props {
    ($arr: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "len" => {
                assert_args_number!($x, $args.len(), 0);
                $output = ParserInstr::Integer($arr.len() as i64)
            }
            "add" => {
                assert_args_number!("add", $args.len(), 1);
                let mut new_vec = $arr.clone();
                new_vec.push($args[0].clone());
                $output = ParserInstr::Array(new_vec, false, false);
            }
            "remove" => {
                assert_args_number!("add", $args.len(), 1);
                let mut new_vec = $arr.clone();
                let index = new_vec.iter().position(|x| return *x == $args[0]).unwrap();
                new_vec.remove(index);
                $output = ParserInstr::Array(new_vec, false, false);
            }
            "clear" => {
                assert_args_number!("clear", $args.len(), 0);
                $output = ParserInstr::Array(Vec::new(), false, false);
            }
            "reverse" => {
                assert_args_number!("clear", $args.len(), 0);
                let mut new_vec = $arr.clone();
                new_vec.reverse();
                $output = ParserInstr::Array(new_vec, false, false)
            }
            "sort" => {
                assert_args_number!("sort", $args.len(), 0);
                let mut new_vec: Vec<ParserInstr> = $arr.clone();
                new_vec.sort_by(|a, b| match a {
                    ParserInstr::Integer(x) => match b {
                        ParserInstr::Integer(y) => return x.cmp(y),
                        ParserInstr::Float(y) => return x.cmp(&(*y as i64)),
                        _ => {
                            error(format!("Cannot compare Integer with {:?}", b).as_str(), "");
                            return core::cmp::Ordering::Equal;
                        }
                    },
                    ParserInstr::Float(x) => match b {
                        ParserInstr::Integer(y) => return (*x as i64).cmp(y),
                        ParserInstr::Float(y) => return x.partial_cmp(y).unwrap(),
                        _ => {
                            error(format!("Cannot compare Integer with {:?}", b).as_str(), "");
                            return core::cmp::Ordering::Equal;
                        }
                    },
                    _ => {
                        error(format!("Cannot sort {:?}", a).as_str(), "");
                        return core::cmp::Ordering::Equal;
                    }
                });
                $output = ParserInstr::Array(new_vec, false, false);
            }
            "index" => {
                assert_args_number!("index", $args.len(), 1);
                $output = ParserInstr::Integer(
                    $arr.clone()
                        .iter()
                        .position(|elem| return *elem == $args[0])
                        .expect(error_msg!(format!(
                            "{:?} was not found in the list",
                            $args[0]
                        ))) as i64,
                )
            }
            "extend" => {
                assert_args_number!("extend", $args.len(), 1);
                let mut new_vec: Vec<ParserInstr> = $arr.clone();
                if let ParserInstr::Array(x, _, false) = $args[0].clone() {
                    new_vec.extend(x);
                    $output = ParserInstr::Array(new_vec, false, false);
                } else {
                    error(format!("{:?} is not a list", $args[0]).as_str(), "");
                }
            }
            "insert" => {
                assert_args_number!("insert", $args.len(), 2);
                let mut new_vec: Vec<ParserInstr> = $arr.clone();
                if let ParserInstr::Integer(x) = $args[0] {
                    new_vec.insert(x as usize, $args[1].clone());
                    $output = ParserInstr::Array(new_vec, false, false)
                } else {
                    error(format!("{:?} is not a valid index", $args[0]).as_str(), "");
                }
            }
            "pop" => {
                assert_args_number!("pop", $args.len(), 1);
                let mut new_vec: Vec<ParserInstr> = $arr.clone();
                if let ParserInstr::Integer(x) = $args[0] {
                    new_vec.remove(x as usize);
                    $output = ParserInstr::Array(new_vec, false, false)
                } else {
                    error(format!("{:?} is not a valid index", $args[0]).as_str(), "");
                }
            }
            "filter" => {
                assert_args_number!($x, $args.len(), 1);
            }
            _ => error(&format!("Unknown function '{}' for object Array", $x), ""),
        }
    };
}
