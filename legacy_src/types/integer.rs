use crate::parser::{Operator, Types};
use crate::util::error;
use crate::{error_msg, get_printable_type, if_let};
use branches::likely;

// #[inline(always)]
pub fn integer_ops(x: i64, output: Types, current_operator: Operator) -> Types {
    if let Types::Integer(value) = output {
        match current_operator {
            Operator::Add => Types::Integer(value + x),
            Operator::Sub => Types::Integer(value - x),
            // BasicOperator::Divide => {
            //     math_to_type!(value as f64 / x as f64)
            // }
            Operator::Multiply => Types::Integer(value * x),
            Operator::Power => Types::Integer(value.pow(x as u32)),
            Operator::Modulo => Types::Integer(value % x),
            Operator::Equal => Types::Bool(value == x),
            Operator::NotEqual => Types::Bool(value != x),
            Operator::Inferior => Types::Bool(value < x),
            Operator::InferiorEqual => Types::Bool(value <= x),
            Operator::Superior => Types::Bool(value > x),
            Operator::SuperiorEqual => Types::Bool(value >= x),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{current_operator:?}' between Integer and Integer"
                    ),
                    "",
                );
                Types::Null
            }
        }
    } else if let Types::Float(value) = output {
        match current_operator {
            Operator::Add => Types::Float(value + x as f64),
            Operator::Sub => Types::Float(value - x as f64),
            Operator::Divide => Types::Float(value / x as f64),
            Operator::Multiply => Types::Float(value * x as f64),
            Operator::Power => Types::Float(value.powf(x as f64)),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{current_operator:?}' between Float and Integer"
                    ),
                    "",
                );
                Types::Null
            }
        }
    } else if let Types::Operation(y) = output {
        if_let!(likely, Operator::Sub, y, {
            Types::Integer(-x)
        }, else {
            error(
                &format!(
                    "Cannot perform operation '{y:?}' between {:?} and Integer",
                    get_printable_type!(output)
                ),
                "",
            );
            Types::Null
        })
    } else if let Types::Array(ref y, false, false) = output {
        if_let!(likely, Operator::Multiply, current_operator, {
            let mut new_vec: Vec<Types> = Vec::with_capacity(x as usize * y.len());
            for _ in 0..x {
                new_vec.append(&mut y.clone());
            }
            Types::Array(new_vec, false, false)
        }, else {
            error(
                &format!(
                    "Cannot perform operation '{current_operator:?}' between Array and Integer",
                ),
                "",
            );
            Types::Null
        })
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
macro_rules! integer_props {
    ($num: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "toFloat" => {
                assert_args_number!("toFloat", $args.len(), 0);
                $output = Types::Float($num as f64)
            }
            "toStr" => {
                assert_args_number!("toStr", $args.len(), 0);
                $output = Types::String($num.to_string().parse().unwrap())
            }
            _ => error(&format!("Unknown function '{}' for object Integer", $x), ""),
        }
    };
}
