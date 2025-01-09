use crate::error_msg;
use crate::get_printable_type;
use crate::parser::{Operator, ParserInstr};
use crate::util::error;
// use smartstring::alias::String;

// #[inline(always)]
pub fn string_ops(x: &String, output: ParserInstr, current_operator: Operator) -> ParserInstr {
    if let ParserInstr::String(value) = output {
        match current_operator {
            Operator::Add => return ParserInstr::String(format!("{value}{x}")),
            Operator::Equal | Operator::NotEqual => return ParserInstr::Bool(value.eq(x)),
            _ => {
                error(
                    &format!(
                        "Cannot perform operation '{current_operator:?}' between String and String"
                    ),
                    "",
                );
            }
        }
        return ParserInstr::Null;
    } else if let ParserInstr::Integer(value) = output {
        if current_operator == Operator::Multiply {
            return ParserInstr::String(x.repeat(value as usize).parse().unwrap());
        }
        error(
            &format!("Cannot perform operation '{current_operator:?}' between Integer and String"),
            "",
        );
        return ParserInstr::Null;
    }
    error(
        &format!(
            "Cannot perform operation '{:?}' between {:?} and String",
            current_operator,
            get_printable_type!(output)
        ),
        "",
    );
    ParserInstr::Null
}

pub fn to_title_case(text: &str) -> String {
    text.split_whitespace()
        .map(|word| {
            let (first, rest) = word.split_at(1);
            first.to_uppercase().parse::<String>().unwrap() + rest.to_lowercase().as_str()
        })
        .collect::<Vec<String>>()
        .join(" ")
    // .parse().unwrap()
}

#[macro_export]
macro_rules! string_props {
    ($str: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            // TRANSFORM
            "uppercase" => {
                assert_args_number!("uppercase", $args.len(), 0);
                $output = ParserInstr::String($str.to_uppercase().parse().unwrap());
            }
            "lowercase" => {
                assert_args_number!("lowercase", $args.len(), 0);
                $output = ParserInstr::String($str.to_lowercase().parse().unwrap());
            }
            "capitalize" => {
                assert_args_number!("capitalize", $args.len(), 0);
                $output = ParserInstr::String(to_title_case($str));
            }
            "replace" => {
                assert_args_number!("replace", $args.len(), 2);
                // if let Types::String(toreplace) = &$args[0] {
                //     if let Types::String(replaced) = &$args[1] {
                //         $output = Types::String(&$str.replace(toreplace, replaced).parse().unwrap())
                //     } else {
                //         error(
                //             format!("{:?} is not a String", &$args[1]).as_str(),
                //             format!("Convert {:?} to a String", &$args[1]).as_str(),
                //         );
                //     }
                // } else {
                //     error(
                //         format!("{:?} is not a String", &$args[0]).as_str(),
                //         format!("Convert {:?} to a String", &$args[0]).as_str(),
                //     );
                // }
            }
            "toInt" => {
                assert_args_number!("toInt", $args.len(), 0);
                if $str.parse::<i64>().is_ok() {
                    $output = ParserInstr::Integer($str.parse::<i64>().unwrap())
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
                    $output = ParserInstr::Float($str.parse::<f64>().unwrap())
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
                    $output = ParserInstr::Bool(true)
                } else if $str.to_lowercase() == "false" {
                    $output = ParserInstr::Bool(false)
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
                $output = ParserInstr::String($str.trim().parse().unwrap());
            }
            "ltrim" => {
                assert_args_number!("ltrim", $args.len(), 0);
                $output = ParserInstr::String($str.trim_start().parse().unwrap());
            }
            "rtrim" => {
                assert_args_number!("rtrim", $args.len(), 0);
                $output = ParserInstr::String($str.trim_end().parse().unwrap());
            }
            _ => error(&format!("Unknown function '{}' for object String", $x), ""),
        }
    };
}
