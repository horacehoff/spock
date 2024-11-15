use crate::parser::{BasicOperator, Expr};
use crate::util::error;
use crate::{error_msg, get_printable_type, math_to_type};

// #[inline(always)]
pub fn integer_ops(x: i64, output: Expr, current_operator: BasicOperator) -> Expr {
    if let Expr::Integer(value) = output {
        match current_operator {
            BasicOperator::Add => Expr::Integer(value + x),
            BasicOperator::Sub => Expr::Integer(value - x),
            BasicOperator::Divide => {
                math_to_type!(value as f64 / x as f64)
            }
            BasicOperator::Multiply => Expr::Integer(value * x),
            BasicOperator::Power => Expr::Integer(value.pow(x as u32)),
            BasicOperator::Modulo => Expr::Integer(value % x),
            BasicOperator::Equal => Expr::Bool(value == x),
            BasicOperator::NotEqual => Expr::Bool(value != x),
            BasicOperator::Inferior => Expr::Bool(value < x),
            BasicOperator::InferiorEqual => Expr::Bool(value <= x),
            BasicOperator::Superior => Expr::Bool(value > x),
            BasicOperator::SuperiorEqual => Expr::Bool(value >= x),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between Integer and Integer",
                        current_operator
                    ),
                    "",
                );
                Expr::Null
            }
        }
    } else if let Expr::Float(value) = output {
        match current_operator {
            BasicOperator::Add => Expr::Float(value + x as f64),
            BasicOperator::Sub => Expr::Float(value - x as f64),
            BasicOperator::Divide => Expr::Float(value / x as f64),
            BasicOperator::Multiply => Expr::Float(value * x as f64),
            BasicOperator::Power => Expr::Float(value.powf(x as f64)),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between Float and Integer",
                        current_operator
                    ),
                    "",
                );
                Expr::Null
            }
        }
    } else if let Expr::Operation(y) = output {
        if let BasicOperator::Sub = y {
            Expr::Integer(-x)
        } else {
            error(
                &format!(
                    "Cannot perform operation '{y:?}' between {:?} and Integer",
                    get_printable_type!(output)
                ),
                "",
            );
            Expr::Null
        }
    } else if let Expr::Array(ref y) = output {
        if let BasicOperator::Multiply = current_operator {
            let mut new_vec: Vec<Expr> = vec![];
            for _ in 0..x {
                new_vec.append(&mut y.clone());
            }
            Expr::Array(new_vec)
        } else {
            error(
                &format!(
                    "Cannot perform operation '{:?}' between Array and Integer",
                    current_operator
                ),
                "",
            );
            Expr::Null
        }
    }
    else {
        error(
            &format!(
                "Cannot perform operation '{:?}' between {:?} and Integer",
                current_operator,
                get_printable_type!(output)
            ),
            "",
        );
        Expr::Null
    }
}

#[macro_export]
macro_rules! integer_props {
    ($num: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "toFloat" => {
                assert_args_number!("toFloat", $args.len(), 0);
                $output = Expr::Float($num as f64)
            }
            "toStr" => {
                assert_args_number!("toStr", $args.len(), 0);
                $output = Expr::String($num.to_string())
            }
            _ => {}
        }
    };
}
