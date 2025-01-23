use crate::instr_set::Instr;
use crate::parser::Operator;
use internment::Intern;

#[macro_export]
macro_rules! error {
    ($x: expr) => {
        eprintln!(
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\n{}\n--------------", $x
        );
        std::process::exit(1);
    };
    ($x: expr, $y: expr) => {
        eprintln!(
            "--------------\n\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m\n{}\n\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m\n{}\n--------------", $x, $y
        );
        std::process::exit(1);
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

pub fn get_type<'a>(unknown: Instr) -> &'a str {
    match unknown {
        Instr::String(_) => "String",
        Instr::Integer(_) => "Integer",
        Instr::Bool(_) => "Boolean",
        Instr::Float(_) => "Float",
        _ => {
            error!(format_args!("Cannot get type of {:?}", unknown));
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
                error!(format_args!("Cannot get type of {:?}", $x), "");
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

pub fn print_form(x: &Instr, locals: &mut [Intern<String>]) -> String {
    match x {
        Instr::String(str) => locals[*str as usize].to_string(),
        Instr::Float(ref float) => float.to_string().parse().unwrap(),
        Instr::Integer(ref int) => int.to_string().parse().unwrap(),
        Instr::Bool(ref boolean) => boolean.to_string().parse().unwrap(),
        _ => {
            error!(format_args!("Cannot display {x:?}"));
        }
    }
}

pub fn op_to_symbol(op: Operator) -> String {
    match op {
        Operator::Null => "Null",
        Operator::Add => "+",
        Operator::Sub => "-",
        Operator::Divide => "/",
        Operator::Multiply => "*",
        Operator::Power => "^",
        Operator::Modulo => "%",
        Operator::Equal => "==",
        Operator::NotEqual => "!=",
        Operator::And => "&&",
        Operator::Inferior => "<",
        Operator::InferiorEqual => "<=",
        Operator::Or => "||",
        Operator::Superior => ">",
        Operator::SuperiorEqual => ">=",
    }
    .parse()
    .unwrap()
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
    for line in lines {
        i += 1;
        match line {
            Instr::Null => {
                println!("{i} NULL");
            }
            Instr::StoreArg => {
                println!("{i} STORE_ARG");
            }
            Instr::Operation(op) => {
                println!("{i} OP      {}", op_to_symbol(*op));
            }
            Instr::FuncReturn => {
                println!("{i} RET");
            }
            Instr::Jump(x, y) => {
                println!("{i} JMP      {}", y);
            }
            Instr::If(cond) => {
                println!("{i} CMP      {}", cond);
            }
            Instr::VarSet(id) => {
                println!("{i} SETVAR      {}", id);
            }
            Instr::FuncCall(use_result, id) => {
                println!(
                    "{i} CALL      {} {}",
                    id,
                    if *use_result { "USE_RESULT" } else { "DISCARD" }
                );
            }
            Instr::VariableIdentifier(id) => {
                println!("{i} VAR      {}", id);
            }
            Instr::Bool(bool) => {
                println!("{i} BOOL({})", bool);
            }
            Instr::String(id) => {
                println!("{i} STR      {}", id);
            }
            Instr::Integer(int) => {
                println!("{i} INT({})", int);
            }
            Instr::Float(float) => {
                println!("{i} FLOAT({})", float);
            }
        }
    }
}
