use crate::error_msg;
use crate::get_printable_type;
use crate::parser::{BasicOperator, Expr};
use crate::util::error;

// #[inline(always)]
pub fn string_ops(x: String, output: Expr, current_operator: BasicOperator) -> Expr {
    if let Expr::String(value) = &output {
        match current_operator {
            BasicOperator::Add => Expr::String(value.to_owned() + &x),
            BasicOperator::Equal => Expr::Bool(value.to_owned() == x),
            BasicOperator::NotEqual => Expr::Bool(value.to_owned() != x),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between String and String",
                        current_operator
                    ),
                    "",
                );
                Expr::Null
            }
        }
    } else if let Expr::Integer(value) = &output {
        match current_operator {
            BasicOperator::Multiply => Expr::String(x.repeat(*value as usize)),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between Integer and String",
                        current_operator
                    ),
                    "",
                );
                Expr::Null
            }
        }
    } else {
        error(
            &format!(
                "Cannot perform operation '{:?}' between {:?} and String",
                current_operator,
                get_printable_type!(output)
            ),
            "",
        );
        Expr::Null
    }
}

#[macro_export]
macro_rules! string_props {
    ($str: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            // TRANSFORM
            "uppercase" => {
                assert_args_number!("uppercase", $args.len(), 0);
                $output = Expr::String($str.to_uppercase());
            }
            "lowercase" => {
                assert_args_number!("lowercase", $args.len(), 0);
                $output = Expr::String($str.to_lowercase());
            }
            "capitalize" => {
                assert_args_number!("capitalize", $args.len(), 0);
                $output = Expr::String($str.to_title_case());
            }
            "replace" => {
                assert_args_number!("replace", $args.len(), 2);
                if let Expr::String(toreplace) = &$args[0] {
                    if let Expr::String(replaced) = &$args[1] {
                        $output = Expr::String($str.replace(toreplace, replaced))
                    } else {
                        error(
                            format!("{:?} is not a String", &$args[1]).as_str(),
                            format!("Convert {:?} to a String", &$args[1]).as_str(),
                        );
                    }
                } else {
                    error(
                        format!("{:?} is not a String", &$args[0]).as_str(),
                        format!("Convert {:?} to a String", &$args[0]).as_str(),
                    );
                }
            }
            "toInt" => {
                assert_args_number!("toInt", $args.len(), 0);
                if $str.parse::<i64>().is_ok() {
                    $output = Expr::Integer($str.parse::<i64>().unwrap())
                } else {
                    error(
                        &format!("String '{}' cannot be converted to an Integer", $str),
                        "",
                    );
                }
            }
            "toFloat" => {
                assert_args_number!("toFloat", $args.len(), 0);
                if $str.parse::<f64>().is_ok() {
                    $output = Expr::Float($str.parse::<f64>().unwrap())
                } else {
                    error(
                        &format!("String '{}' cannot be converted to an Integer", $str),
                        "",
                    );
                }
            }
            "toBool" => {
                assert_args_number!("toBool", $args.len(), 0);
                if $str.to_lowercase() == "true" {
                    $output = Expr::Bool(true)
                } else if $str.to_lowercase() == "false" {
                    $output = Expr::Bool(false)
                } else {
                    error(
                        &format!("String '{}' cannot be converted to a Boolean", $str),
                        "",
                    );
                }
            }
            "index" => {
                assert_args_number!("index", $args.len(), 1);
                if let Expr::String(toindex) = &$args[0] {
                    let indx = $str.find(toindex).unwrap();
                    $output = Expr::Integer(indx as i64);
                } else {
                    error(
                        format!("{:?} is not a String", &$args[0]).as_str(),
                        format!("Convert {:?} to a String", &$args[0]).as_str(),
                    );
                }
            }
            "trim" => {
                assert_args_number!("trim", $args.len(), 0);
                $output = Expr::String($str.trim().to_string());
            }
            "ltrim" => {
                assert_args_number!("ltrim", $args.len(), 0);
                $output = Expr::String($str.trim_start().to_string());
            }
            "rtrim" => {
                assert_args_number!("rtrim", $args.len(), 0);
                $output = Expr::String($str.trim_end().to_string());
            }
            _ => {}
        }
    };
}
