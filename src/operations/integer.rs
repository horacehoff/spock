use crate::{get_printable_type, math_to_type, error_msg};
use crate::parser::{BasicOperator, Expr};
use crate::util::error;

#[inline(always)]
pub fn integer_ops(x:i64, output: Expr, current_operator: BasicOperator) -> Expr {
    if let Expr::Integer(value) = output {
        match current_operator {
            BasicOperator::Add => {
                Expr::Integer(value + x)
            }
            BasicOperator::Sub => {
                Expr::Integer(value - x)
            }
            BasicOperator::Divide => {
                math_to_type!(value as f64 / x as f64)
            }
            BasicOperator::Multiply => {
                Expr::Integer(value * x)
            }
            BasicOperator::Power => {
                Expr::Integer(value.pow(x as u32))
            }
            BasicOperator::Modulo => {
                Expr::Integer(value % x)
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
            _ => {
                error(&format!("Cannot perform operation '{:?}' between Integer and Integer", current_operator), "");
                Expr::Null
            },
        }
    } else if let Expr::Float(value) = output {
        match current_operator {
            BasicOperator::Add => {
                Expr::Float(value + x as f64)
            }
            BasicOperator::Sub => {
                Expr::Float(value - x as f64)
            }
            BasicOperator::Divide => {
                Expr::Float(value / x as f64)
            }
            BasicOperator::Multiply => {
                Expr::Float(value * x as f64)
            }
            BasicOperator::Power => {
                Expr::Float(value.powf(x as f64))
            }
            _ => {
                error(&format!("Cannot perform operation '{:?}' between Float and Integer", current_operator), "");
                Expr::Null
            },
        }
    } else {
        error(&format!("Cannot perform operation '{:?}' between {:?} and Integer", get_printable_type!(output), current_operator), "");
        Expr::Null
    }
}