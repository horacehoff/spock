use crate::{get_printable_type, math_to_type, error_msg};
use crate::parser::{BasicOperator, Expr};
use crate::util::error;

#[inline(always)]
pub fn float_ops(x:f64, output: Expr, current_operator: BasicOperator) -> Expr {
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
            BasicOperator::EQUAL => Expr::Bool(value == x),
            BasicOperator::Inferior => {
                Expr::Bool(value < x)
            }
            BasicOperator::InferiorEqual => Expr::Bool(value <= x),
            BasicOperator::Superior => {
                Expr::Bool(value > x)
            }
            BasicOperator::SuperiorEqual => {
                Expr::Bool(value >= x)
            }
            _ => {error(&format!("Cannot perform operation '{:?}' between Float and Float", current_operator),""); Expr::Null},
        }
    } else if let Expr::Integer(value) = output {
        match current_operator {
            BasicOperator::Add => {
                Expr::Float(value as f64 + x)
            }
            BasicOperator::Sub => {
                Expr::Float(value as f64 - x)
            }
            BasicOperator::Divide => {
                math_to_type!(value as f64 / x)
            }
            BasicOperator::Multiply => {
                math_to_type!(value as f64 * x)
            }
            BasicOperator::Power => {
                math_to_type!((value as f64).powf(x))
            }
            _ => {error(&format!("Cannot perform operation '{:?}' between Integer and Float", current_operator),""); Expr::Null},
        }
    } else {
        error(&format!("Cannot perform operation '{:?}' between {:?} and Float", get_printable_type!(output), current_operator),""); Expr::Null
    }
}