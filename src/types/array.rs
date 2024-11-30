use crate::error_msg;
use crate::get_printable_type;
use crate::parser::{BasicOperator, Types};
use crate::util::error;

pub fn array_ops(x: Vec<Types>, output: Types, current_operator: BasicOperator) -> Types {
    if let Types::Integer(value) = output {
        match current_operator {
            BasicOperator::Multiply => {
                let mut new_vec: Vec<Types> = vec![];
                for _ in 0..value {
                    new_vec.append(&mut x.clone());
                }
                Types::Array(new_vec)
            }
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between Array and Integer",
                        current_operator
                    ),
                    "",
                );
                Types::Null
            }
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
        Types::Null
    }
}

#[macro_export]
macro_rules! array_props {
    ($arr: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "len" => {
                assert_args_number!("len", $args.len(), 0);
                $output = Types::Integer($arr.len() as i64)
            }
            "add" => {
                assert_args_number!("add", $args.len(), 1);
                let mut new_vec = $arr.clone();
                new_vec.push($args[0].clone());
                $output = Types::Array(new_vec);
            }
            "remove" => {
                assert_args_number!("add", $args.len(), 1);
                let mut new_vec = $arr.clone();
                let index = new_vec.iter().position(|x| *x == $args[0]).unwrap();
                new_vec.remove(index);
                $output = Types::Array(new_vec);
            }
            "clear" => {
                assert_args_number!("clear", $args.len(), 0);
                $output = Types::Array(vec![]);
            }
            "reverse" => {
                assert_args_number!("clear", $args.len(), 0);
                let mut new_vec = $arr.clone();
                new_vec.reverse();
                $output = Types::Array(new_vec)
            }
            "sort" => {
                assert_args_number!("sort", $args.len(), 0);
                let mut new_vec: Vec<Types> = $arr.clone();
                new_vec.sort_by(|a, b| match a {
                    Types::Integer(x) => match b {
                        Types::Integer(y) => x.cmp(y),
                        Types::Float(y) => x.cmp(&(*y as i64)),
                        _ => {
                            error(format!("Cannot compare Integer with {:?}", b).as_str(), "");
                            std::cmp::Ordering::Equal
                        }
                    },
                    Types::Float(x) => match b {
                        Types::Integer(y) => (*x as i64).cmp(y),
                        Types::Float(y) => x.partial_cmp(y).unwrap(),
                        _ => {
                            error(format!("Cannot compare Integer with {:?}", b).as_str(), "");
                            std::cmp::Ordering::Equal
                        }
                    },
                    _ => {
                        error(format!("Cannot sort {:?}", a).as_str(), "");
                        std::cmp::Ordering::Equal
                    }
                });
                $output = Types::Array(new_vec);
            }
            "index" => {
                assert_args_number!("index", $args.len(), 1);
                $output = Types::Integer(
                    $arr.clone()
                        .iter()
                        .position(|elem| *elem == $args[0])
                        .expect(error_msg!(format!(
                            "{:?} was not found in the list",
                            $args[0]
                        ))) as i64,
                )
            }
            "extend" => {
                assert_args_number!("extend", $args.len(), 1);
                let mut new_vec: Vec<Types> = $arr.clone();
                if let Types::Array(x) = $args[0].clone() {
                    new_vec.extend(x);
                    $output = Types::Array(new_vec);
                } else {
                    error(format!("{:?} is not a list", $args[0]).as_str(), "");
                }
            }
            "insert" => {
                assert_args_number!("insert", $args.len(), 2);
                let mut new_vec: Vec<Types> = $arr.clone();
                if let Types::Integer(x) = $args[0] {
                    new_vec.insert(usize::try_from(x), $args[1].clone());
                    $output = Types::Array(new_vec)
                } else {
                    error(format!("{:?} is not a valid index", $args[0]).as_str(), "");
                }
            }
            "pop" => {
                assert_args_number!("pop", $args.len(), 1);
                let mut new_vec: Vec<Types> = $arr.clone();
                if let Types::Integer(x) = $args[0] {
                    new_vec.remove(usize::try_from(x));
                    $output = Types::Array(new_vec)
                } else {
                    error(format!("{:?} is not a valid index", $args[0]).as_str(), "");
                }
            }
            _ => {}
        }
    };
}
