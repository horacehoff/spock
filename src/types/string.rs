use smol_str::SmolStr;
use crate::error_msg;
use crate::get_printable_type;
use crate::parser::{BasicOperator, Types};
use crate::util::error;

// #[inline(always)]
pub fn string_ops(x: SmolStr, output: Types, current_operator: BasicOperator) -> Types {
    if let Types::String(value) = &output {
        match current_operator {
            BasicOperator::Add => Types::String(format!("{}{}", value, x).into()),
            BasicOperator::Equal => Types::Bool(value.eq(&x)),
            BasicOperator::NotEqual => Types::Bool(value.eq(&x)),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between String and String",
                        current_operator
                    ),
                    "",
                );
                Types::Null
            }
        }
    } else if let Types::Integer(value) = &output {
        match current_operator {
            BasicOperator::Multiply => Types::String(x.repeat(*value as usize).parse().unwrap()),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{:?}' between Integer and String",
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
                "Cannot perform operation '{:?}' between {:?} and String",
                current_operator,
                get_printable_type!(output)
            ),
            "",
        );
        Types::Null
    }
}

#[macro_export]
macro_rules! string_props {
    ($str: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            // TRANSFORM
            "uppercase" => {
                assert_args_number!("uppercase", $args.len(), 0);
                $output = Types::String($str.to_uppercase_smolstr());
            }
            "lowercase" => {
                assert_args_number!("lowercase", $args.len(), 0);
                $output = Types::String($str.to_lowercase_smolstr());
            }
            "capitalize" => {
                assert_args_number!("capitalize", $args.len(), 0);
                $output = Types::String($str.to_title_case().to_smolstr());
            }
            "replace" => {
                assert_args_number!("replace", $args.len(), 2);
                if let Types::String(toreplace) = &$args[0] {
                    if let Types::String(replaced) = &$args[1] {
                        $output = Types::String($str.replace_smolstr(toreplace, replaced))
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
                    $output = Types::Integer($str.parse::<i64>().unwrap())
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
                    $output = Types::Float($str.parse::<f64>().unwrap())
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
                    $output = Types::Bool(true)
                } else if $str.to_lowercase() == "false" {
                    $output = Types::Bool(false)
                } else {
                    error(
                        &format!("String '{}' cannot be converted to a Boolean", $str),
                        "",
                    );
                }
            }
            "index" => {
                // assert_args_number!("index", $args.len(), 1);
                // if let Expr::String(toindex) = &$args[0] {
                //     let indx = $str.find(toindex).unwrap();
                //     $output = Expr::Integer(indx as i64);
                // } else {
                //     error(
                //         format!("{:?} is not a String", &$args[0]).as_str(),
                //         format!("Convert {:?} to a String", &$args[0]).as_str(),
                //     );
                // }
            }
            "trim" => {
                assert_args_number!("trim", $args.len(), 0);
                $output = Types::String($str.trim().to_smolstr());
            }
            "ltrim" => {
                assert_args_number!("ltrim", $args.len(), 0);
                $output = Types::String($str.trim_start().to_smolstr());
            }
            "rtrim" => {
                assert_args_number!("rtrim", $args.len(), 0);
                $output = Types::String($str.trim_end().to_smolstr());
            }
            _ => {}
        }
    };
}
