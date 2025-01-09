use crate::parser::{Operator, ParserInstr};
use crate::util::error;
use crate::{error_msg, get_printable_type, if_let};
use branches::likely;

// #[inline(always)]
pub fn integer_ops(x: i64, output: ParserInstr, current_operator: Operator) -> ParserInstr {
    if let ParserInstr::Integer(value) = output {
        match current_operator {
            Operator::Add => ParserInstr::Integer(value + x),
            Operator::Sub => ParserInstr::Integer(value - x),
            // BasicOperator::Divide => {
            //     math_to_type!(value as f64 / x as f64)
            // }
            Operator::Multiply => ParserInstr::Integer(value * x),
            Operator::Power => ParserInstr::Integer(value.pow(x as u32)),
            Operator::Modulo => ParserInstr::Integer(value % x),
            Operator::Equal => ParserInstr::Bool(value == x),
            Operator::NotEqual => ParserInstr::Bool(value != x),
            Operator::Inferior => ParserInstr::Bool(value < x),
            Operator::InferiorEqual => ParserInstr::Bool(value <= x),
            Operator::Superior => ParserInstr::Bool(value > x),
            Operator::SuperiorEqual => ParserInstr::Bool(value >= x),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{current_operator:?}' between Integer and Integer"
                    ),
                    "",
                );
                ParserInstr::Null
            }
        }
    } else if let ParserInstr::Float(value) = output {
        match current_operator {
            Operator::Add => ParserInstr::Float(value + x as f64),
            Operator::Sub => ParserInstr::Float(value - x as f64),
            Operator::Divide => ParserInstr::Float(value / x as f64),
            Operator::Multiply => ParserInstr::Float(value * x as f64),
            Operator::Power => ParserInstr::Float(value.powf(x as f64)),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{current_operator:?}' between Float and Integer"
                    ),
                    "",
                );
                ParserInstr::Null
            }
        }
    } else if let ParserInstr::Operation(y) = output {
        if_let!(likely, Operator::Sub, y, {
            ParserInstr::Integer(-x)
        }, else {
            error(
                &format!(
                    "Cannot perform operation '{y:?}' between {:?} and Integer",
                    get_printable_type!(output)
                ),
                "",
            );
            ParserInstr::Null
        })
    } else if let ParserInstr::Array(ref y, false, false) = output {
        if_let!(likely, Operator::Multiply, current_operator, {
            let mut new_vec: Vec<ParserInstr> = Vec::with_capacity(x as usize * y.len());
            for _ in 0..x {
                new_vec.append(&mut y.clone());
            }
            ParserInstr::Array(new_vec, false, false)
        }, else {
            error(
                &format!(
                    "Cannot perform operation '{current_operator:?}' between Array and Integer",
                ),
                "",
            );
            ParserInstr::Null
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
        ParserInstr::Null
    }
}

#[macro_export]
macro_rules! integer_props {
    ($num: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "toFloat" => {
                assert_args_number!("toFloat", $args.len(), 0);
                $output = ParserInstr::Float($num as f64)
            }
            "toStr" => {
                assert_args_number!("toStr", $args.len(), 0);
                $output = ParserInstr::String($num.to_string().parse().unwrap())
            }
            _ => error(&format!("Unknown function '{}' for object Integer", $x), ""),
        }
    };
}
