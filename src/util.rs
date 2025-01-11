use crate::parser::{Instr, Operator, ParserInstr};
use internment::Intern;
// use smartstring::alias::String;
// use smartstring::alias::

pub fn error(message: &str, tip: &str) {
    if tip.is_empty() {
        eprintln!(
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\n{message}\n--------------"
        );
    } else {
        eprintln!(
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\n{message}\n\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m\n{tip}\n--------------"
        );
    }
    std::process::exit(1);
}

#[macro_export]
macro_rules! error_msg {
    ($x:expr) => {
        format!(
            "--------------\n{}\n{}\n--------------",
            "\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", $x
        )
        .as_str()
    };
    ($x:expr, $y:expr) => {
        format!(
            "--------------\n{}\n{}\n{}\n{}\n--------------",
            "\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m",
            $x,
            "\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m",
            $y
        )
        .as_str()
    };
}

// https://stackoverflow.com/a/63305257
#[macro_export]
macro_rules! log {
    ($($rest:tt)*) => {
        #[cfg(debug_assertions)]
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($rest)*))
    }
}

#[macro_export]
macro_rules! log_release {
    ($($rest:tt)*) => {
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($rest)*))
    }
}

#[macro_export]
macro_rules! assert_args_number {
    ($func_name:expr, $received_args_len:expr, $expected_args_len:expr) => {
        // if $received_args_len != $expected_args_len {
        //     error(
        //         &format!(
        //             "Function '{}' expected {} argument(s) but received {}",
        //             $func_name, $expected_args_len, $received_args_len
        //         ),
        //         "",
        //     );
        // }
        assert!($received_args_len == $expected_args_len,
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\nFunction '{}' expected {} argument(s) but received {}\n--------------",
            $func_name, $expected_args_len, $received_args_len
        )
    };
    ($func_name:expr, $received_args_len:expr, $min_args_len:expr, $max_args_len:expr) => {
        // if $received_args_len < $min_args_len || $received_args_len > $max_args_len {
        //     error(
        //         &format!(
        //             "Function '{}' expected between {} and {} arguments but received {}",
        //             $func_name, $min_args_len, $max_args_len, $received_args_len
        //         ),
        //         "Remove the excess arguments",
        //     );
        // }
        assert!(
            $received_args_len >= $min_args_len && $received_args_len <= $max_args_len,
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\nFunction '{}' expected between {} and {} argument(s) but received {}\n--------------",
            $func_name, $min_args_len, $max_args_len, $received_args_len
        )
    };
}

macro_rules! get_value {
    ($x:expr) => {
        match $x {
            ParserInstr::String(x) => x,
            ParserInstr::Float(x) => x,
            ParserInstr::Integer(x) => x,
            ParserInstr::Bool(x) => x,
            ParserInstr::Array(x) => x,
            _ => error(
                format!("{}", error_msg!(format!("Cannot get value of {:?}", $x))),
                "",
            ),
        }
    };
}

pub fn get_type<'a>(unknown: Instr) -> &'a str {
    match unknown {
        Instr::String(_) => "String",
        Instr::Integer(_) => "Integer",
        Instr::Bool(_) => "Boolean",
        Instr::Float(_) => "Float",
        _ => {
            error(error_msg!(format!("Cannot get type of {:?}", unknown)), "");
            ""
        }
    }
}

#[macro_export]
macro_rules! get_printable_type {
    ($x:expr) => {
        match $x {
            ParserInstr::String(_) => "String",
            ParserInstr::Float(_) => "Float",
            ParserInstr::Integer(_) => "Integer",
            ParserInstr::Bool(_) => "Boolean",
            ParserInstr::Array(_, _, _) => "Array",
            ParserInstr::Null => "Null",
            _ => {
                println!("{:?}", $x);
                error(
                    &format!("{}", error_msg!(format!("Cannot get type of {:?}", $x))),
                    "",
                );
                ""
            }
        }
    };
}

#[macro_export]
macro_rules! math_to_type {
    ($x:expr) => {
        if $x == $x.trunc() {
            Instr::Integer($x as i32)
        } else {
            Instr::Float($x)
        }
    };
}

#[macro_export]
macro_rules! parser_math_to_type {
    ($x:expr) => {
        if $x == $x.trunc() {
            ParserInstr::Integer($x as i32)
        } else {
            ParserInstr::Float($x)
        }
    };
}

#[macro_export]
macro_rules! if_let {
    // When likely is specified
    (likely, $pattern:pat, $value:expr, $body:block) => {
        if likely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            }
        }
    };

    (likely, $pattern:pat, $value:expr, $body:block, else $elseblock:block) => {
        if likely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            } else {
                $elseblock
            }
        } else {
            $elseblock
        }
    };

    // When unlikely is specified
    (unlikely, $pattern:pat, $value:expr, $body:block) => {
        if unlikely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            }
        }
    };

    (unlikely, $pattern:pat, $value:expr, $body:block, else $elseblock:block) => {
        if unlikely(matches!($value, $pattern)) {
            if let $pattern = $value {
                $body
            } else {
                $elseblock
            }
        } else {
            $elseblock
        }
    };

    // Default (no prediction hint)
    ($pattern:pat, $value:pat, $body:block) => {
        if let $pattern = $value {
            $body
        }
    };

    ($pattern:pat, $value:expr, $body:block, else $elseblock:block) => {
        if let $pattern = $value {
            $body
        } else {
            $elseblock
        }
    };
}

pub fn get_printable_form(x: &ParserInstr) -> String {
    match x {
        ParserInstr::String(ref str) => str.parse().unwrap(),
        ParserInstr::Float(ref float) => float.to_string().parse().unwrap(),
        ParserInstr::Integer(ref int) => int.to_string().parse().unwrap(),
        ParserInstr::Bool(ref boolean) => boolean.to_string().parse().unwrap(),
        ParserInstr::Array(ref x, _, _) => {
            let arr = x;
            ("[".to_owned()
                + arr
                    .iter()
                    .map(|item| get_printable_form(item).to_string() + ",")
                    .collect::<String>()
                    .trim_end_matches(',')
                    .parse::<String>()
                    .unwrap()
                    .as_str()
                + "]")
                .parse()
                .unwrap()
        }
        ParserInstr::Null => "Null".parse().unwrap(),
        _ => {
            error(
                &format!("Cannot display {} type", get_printable_type!(x)),
                "",
            );
            "".parse().unwrap()
        }
    }
}

pub fn print_form(x: &Instr, locals: &mut Vec<Intern<String>>) -> String {
    // println!("LOCALS ARE {locals:?}");
    match x {
        Instr::String(str) => locals[*str as usize].to_string(),
        Instr::Float(ref float) => float.to_string().parse().unwrap(),
        Instr::Integer(ref int) => int.to_string().parse().unwrap(),
        Instr::Bool(ref boolean) => boolean.to_string().parse().unwrap(),
        _ => {
            error(&format!("Cannot display {x:?}"), "");
            "".parse().unwrap()
        }
    }
}

pub fn op_to_symbol(op: Operator) -> String {
    match op {
        Operator::Null => "Null".parse().unwrap(),
        Operator::Add => "+".parse().unwrap(),
        Operator::Sub => "-".parse().unwrap(),
        Operator::Divide => "/".parse().unwrap(),
        Operator::Multiply => "*".parse().unwrap(),
        Operator::Power => "^".parse().unwrap(),
        Operator::Modulo => "%".parse().unwrap(),
        Operator::Equal => "==".parse().unwrap(),
        Operator::NotEqual => "!=".parse().unwrap(),
        Operator::And => "&&".parse().unwrap(),
        Operator::Inferior => "<".parse().unwrap(),
        Operator::InferiorEqual => "<=".parse().unwrap(),
        Operator::Or => "||".parse().unwrap(),
        Operator::Superior => ">".parse().unwrap(),
        Operator::SuperiorEqual => ">=".parse().unwrap(),
    }
}

pub fn split_vec<T: PartialEq>(input: Vec<T>, separator: T) -> Vec<Vec<T>> {
    let mut result = Vec::with_capacity(input.len() / 2); // Pre-allocate capacity based on expected number of splits.
    let mut current = Vec::new();

    for item in input {
        if item.eq(&separator) {
            if !current.is_empty() {
                result.push(current);
                current = Vec::new(); // Clear the current vector, don't reallocate.
            }
        } else {
            current.push(item);
        }
    }

    if !current.is_empty() {
        result.push(current);
    }

    result
}

pub fn split_vec_box<T: PartialEq + Clone>(input: &[T], separator: T) -> Vec<Vec<T>> {
    let mut result = Vec::with_capacity(input.len() / 2); // Pre-allocate space for the result
    let mut current = Vec::new();

    for item in input.iter() {
        if *item == separator {
            if !current.is_empty() {
                result.push(current);
                current = Vec::new(); // Clear without reallocating
            }
        } else {
            current.push(item.clone()); // Clone item to store owned value
        }
    }

    if !current.is_empty() {
        result.push(current);
    }

    result
}

pub fn print_instructions(lines: &[Instr]) {
    let mut i = 0;
    let mut depth = 0;
    for line in lines {
        i += 1;
        match line {
            Instr::StopStore => {
                depth -= 1;
                println!("{i} {} STOP", "--".repeat(depth))
            }
            Instr::Null => {
                println!("{i} {} NULL", "--".repeat(depth));
            }
            Instr::Store => {
                println!("{i} {} STORE", "--".repeat(depth));
                depth += 1;
            }
            Instr::StoreArg => {
                println!("{i} {} STORE_ARG", "--".repeat(depth));
            }
            Instr::Operation(op) => {
                println!("{i} {} OP      {}", "--".repeat(depth), op_to_symbol(*op));
            }
            Instr::FuncReturn => {
                println!("{i} {} RET", "--".repeat(depth));
            }
            Instr::Jump(x, y) => {
                println!("{i} {} JMP      {}", "--".repeat(depth), y);
            }
            Instr::If(cond) => {
                println!("{i} {} CMP      {}", "--".repeat(depth), cond);
            }
            Instr::VarStore(id) => {
                println!("{i} {} SETVAR      {}", "--".repeat(depth), id);
            }
            Instr::VarUpdate(id) => {
                println!("{i} {} SETVAR      {}", "--".repeat(depth), id);
            }
            Instr::FuncCall(id) => {
                println!("{i} {} CALL      {}", "--".repeat(depth), id);
            }
            Instr::VariableIdentifier(id) => {
                println!("{i} {} VAR      {}", "--".repeat(depth), id);
            }
            Instr::Bool(bool) => {
                println!("{i} {} BOOL({})", "--".repeat(depth), bool);
            }
            Instr::String(id) => {
                println!("{i} {} STR      {}", "--".repeat(depth), id);
            }
            Instr::Integer(int) => {
                println!("{i} {} INT({})", "--".repeat(depth), int);
            }
            Instr::Float(float) => {
                println!("{i} {} FLOAT({})", "--".repeat(depth), float);
            }
        }
    }
}
