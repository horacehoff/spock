use crate::parser::{BasicOperator, Expr};
use crate::util::error;
use crate::{error_msg, get_printable_type, math_to_type};

#[inline(always)]
pub fn float_ops(x: f64, output: Expr, current_operator: BasicOperator) -> Expr {
    if let Expr::Float(value) = output {
        match current_operator {
            BasicOperator::Add => {
                math_to_type!(value + x)
            }
            BasicOperator::Sub => {
                math_to_type!(value - x)
            }
            BasicOperator::Divide => {
                math_to_type!(value / x)
            }
            BasicOperator::Multiply => {
                math_to_type!(value * x)
            }
            BasicOperator::Power => {
                math_to_type!(value.powf(x))
            }
            BasicOperator::Modulo => {
                math_to_type!(value % x)
            }
            BasicOperator::Equal => Expr::Bool(value == x),
            BasicOperator::NotEqual => Expr::Bool(value != x),
            BasicOperator::Inferior => Expr::Bool(value < x),
            BasicOperator::InferiorEqual => Expr::Bool(value <= x),
            BasicOperator::Superior => Expr::Bool(value > x),
            BasicOperator::SuperiorEqual => Expr::Bool(value >= x),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between Float and Float",
                        current_operator
                    ),
                    "",
                );
                Expr::Null
            }
        }
    } else if let Expr::Integer(value) = output {
        match current_operator {
            BasicOperator::Add => Expr::Float(value as f64 + x),
            BasicOperator::Sub => Expr::Float(value as f64 - x),
            BasicOperator::Divide => {
                math_to_type!(value as f64 / x)
            }
            BasicOperator::Multiply => {
                math_to_type!(value as f64 * x)
            }
            BasicOperator::Power => {
                math_to_type!((value as f64).powf(x))
            }
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between Integer and Float",
                        current_operator
                    ),
                    "",
                );
                Expr::Null
            }
        }
    } else if let Expr::Operation(y) = output {
        if let BasicOperator::Sub = y {
            Expr::Float(-x)
        } else {
            error(
                &format!(
                    "Cannot perform operation '{y:?}' between {:?} and Float",
                    get_printable_type!(output)
                ),
                "",
            ); Expr::Null
        }
    }
    else {
        error(
            &format!(
                "Cannot perform operation '{:?}' between {:?} and Float",
                current_operator,
                get_printable_type!(output)
            ),
            "",
        );
        Expr::Null
    }
}

#[macro_export]
macro_rules! float_props {
    ($num: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "toInt" => {
                assert_args_number!("toInt", $args.len(), 0);
                $output = Expr::Integer($num as i64)
            }
            "toStr" => {
                assert_args_number!("toStr", $args.len(), 0);
                $output = Expr::String($num.to_string())
            }
            _ => {}
        }
    };
}
