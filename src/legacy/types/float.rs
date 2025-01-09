use crate::parser::{Operator, ParserInstr};
use crate::util::error;
use crate::{error_msg, get_printable_type};

// #[inline(always)]
pub fn float_ops(x: f64, output: ParserInstr, current_operator: Operator) -> ParserInstr {
    if let ParserInstr::Float(value) = output {
        match current_operator {
            // BasicOperator::Add => {
            //     math_to_type!(value + x)
            // }
            // BasicOperator::Sub => {
            //     math_to_type!(value - x)
            // }
            // BasicOperator::Divide => {
            //     math_to_type!(value / x)
            // }
            // BasicOperator::Multiply => {
            //     math_to_type!(value * x)
            // }
            // BasicOperator::Power => {
            //     math_to_type!(value.powf(x))
            // }
            // BasicOperator::Modulo => {
            //     math_to_type!(value % x)
            // }
            Operator::Equal => ParserInstr::Bool(value == x),
            Operator::NotEqual => ParserInstr::Bool(value != x),
            Operator::Inferior => ParserInstr::Bool(value < x),
            Operator::InferiorEqual => ParserInstr::Bool(value <= x),
            Operator::Superior => ParserInstr::Bool(value > x),
            Operator::SuperiorEqual => ParserInstr::Bool(value >= x),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{current_operator:?}' between Float and Float"
                    ),
                    "",
                );
                ParserInstr::Null
            }
        }
    } else if let ParserInstr::Integer(value) = output {
        match current_operator {
            Operator::Add => ParserInstr::Float(value as f64 + x),
            Operator::Sub => ParserInstr::Float(value as f64 - x),
            // BasicOperator::Divide => {
            //     math_to_type!(value as f64 / x)
            // }
            // BasicOperator::Multiply => {
            //     math_to_type!(value as f64 * x)
            // }
            // BasicOperator::Power => {
            //     math_to_type!((value as f64).powf(x))
            // }
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{current_operator:?}' between Integer and Float"
                    ),
                    "",
                );
                ParserInstr::Null
            }
        }
    } else if let ParserInstr::Operation(y) = output {
        if let Operator::Sub = y {
            ParserInstr::Float(-x)
        } else {
            error(
                &format!(
                    "Cannot perform operation '{y:?}' between {:?} and Float",
                    get_printable_type!(output)
                ),
                "",
            );
            ParserInstr::Null
        }
    } else {
        error(
            &format!(
                "Cannot perform operation '{:?}' between {:?} and Float",
                current_operator,
                get_printable_type!(output)
            ),
            "",
        );
        ParserInstr::Null
    }
}

#[macro_export]
macro_rules! float_props {
    ($num: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "toInt" => {
                assert_args_number!("toInt", $args.len(), 0);
                $output = ParserInstr::Integer($num as i64)
            }
            "toStr" => {
                assert_args_number!("toStr", $args.len(), 0);
                $output = ParserInstr::String($num.to_string().parse().unwrap())
            }
            _ => error(&format!("Unknown function '{}' for object Float", $x), ""),
        }
    };
}
