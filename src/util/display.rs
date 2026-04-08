use crate::ArrayStorage;
use crate::parser::Expr;
use crate::{Data, Instr};
use inline_colorization::*;
use smol_str::{SmolStr, ToSmolStr};
use std::hint::unreachable_unchecked;

#[inline(always)]
pub fn format_data(x: &Data, arrays: Option<&ArrayStorage>, show_str: bool) -> SmolStr {
    if x.is_float() {
        x.as_float().to_smolstr()
    } else if x.is_int() {
        x.as_int().to_smolstr()
    } else if x.is_bool() {
        x.as_bool().to_smolstr()
    } else if x.is_str() {
        if show_str {
            x.as_str().to_smolstr()
        } else {
            format_args!("\"{}\"", x.as_str()).to_smolstr()
        }
    } else if x.is_array() {
        if let Some(arrays) = arrays {
            format_args!(
                "[{}]",
                arrays[x.as_array() as usize]
                    .iter()
                    .map(|x| format_data(x, Some(arrays), true))
                    .collect::<Vec<_>>()
                    .join(","),
            )
            .to_smolstr()
        } else {
            format_args!("ARRAY[{}]", x.as_array()).to_smolstr()
        }
    } else if x.is_null() {
        SmolStr::from("NULL")
    } else {
        unsafe { unreachable_unchecked() }
    }
}

impl std::fmt::Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_data(self, None, false))
    }
}

pub fn get_type_name(x: &Data) -> SmolStr {
    SmolStr::from(if x.is_array() {
        "Array"
    } else if x.is_bool() {
        "Boolean"
    } else if x.is_str() {
        "String"
    } else if x.is_float() {
        "Float"
    } else if x.is_int() {
        "Integer"
    } else if x.is_null() {
        "Null"
    } else {
        unreachable!()
    })
}

pub fn format_expr(x: &Expr) -> SmolStr {
    match x {
        Expr::Float(num) => num.to_smolstr(),
        Expr::Bool(bool) => bool.to_smolstr(),
        Expr::String(str) => format_args!("\"{str}\"").to_smolstr(),
        Expr::Array(a, _, _) => format_args!(
            "[{}]",
            a.iter().map(format_expr).collect::<Vec<_>>().join(","),
        )
        .as_str()
        .unwrap()
        .to_smolstr(),
        Expr::Var(x, _, _) => x.to_smolstr(),
        _ => unreachable!(),
    }
}

pub fn token_recognition(token: &str) -> SmolStr {
    match token {
        "r#\"[a-zA-Z_]+\"#" => SmolStr::from("identifier"),
        "r#\"([0-9]*[.])?[0-9]+\"#" => SmolStr::from("number"),
        "\"true\"" => SmolStr::from("boolean"),
        other => {
            if other.contains("|[^") {
                SmolStr::from("string")
            } else {
                other.trim_matches('\"').to_smolstr()
            }
        }
    }
}

pub fn print_debug(instructions: &[Instr], registers: &[Data], arrays: &ArrayStorage) {
    println!("{color_yellow}---- DEBUG ----{color_reset}");
    if !arrays.is_empty() {
        println!("{color_green}---  ARRAYS  ---{color_reset}");
        for (i, data) in arrays.iter().enumerate() {
            println!(" {i} {data:?}")
        }
    }
    println!("{color_green}-- REGISTERS --{color_reset}");
    for (i, data) in registers.iter().enumerate() {
        println!(" [{i}] {}({data})", get_type_name(data))
    }
    println!("{color_red}-- INSTRUCTIONS --{color_reset}");
    let mut flows: Vec<(usize, usize)> = Vec::new();
    for (i, instr) in instructions.iter().enumerate() {
        match instr {
            Instr::Jmp(jump_size)
            | Instr::IsFalseJmp(_, jump_size)
            | Instr::SupEqFloatJmp(_, _, jump_size)
            | Instr::SupEqIntJmp(_, _, jump_size)
            | Instr::SupFloatJmp(_, _, jump_size)
            | Instr::SupIntJmp(_, _, jump_size)
            | Instr::InfEqFloatJmp(_, _, jump_size)
            | Instr::InfEqIntJmp(_, _, jump_size)
            | Instr::InfFloatJmp(_, _, jump_size)
            | Instr::InfIntJmp(_, _, jump_size)
            | Instr::NotEqJmp(_, _, jump_size)
            | Instr::ArrayNotEqJmp(_, _, jump_size)
            | Instr::EqJmp(_, _, jump_size)
            | Instr::ArrayEqJmp(_, _, jump_size) => flows.push((i, i + *jump_size as usize)),
            Instr::CallLibFunc(jump_size, _, _) => flows.push((i, *jump_size as usize)),
            Instr::JmpBack(jump_size) => flows.push((i, i - *jump_size as usize)),
            Instr::CallFunc(n, _) => flows.push((i, *n as usize)),
            _ => continue,
        }
    }
    let instr_str = instructions
        .iter()
        .enumerate()
        .map(|(i, instr)| format_args!(" {i}: {instr:?} ").to_smolstr())
        .collect::<Vec<SmolStr>>();
    let max_len = instr_str.iter().max_by_key(|x| x.len()).unwrap().len();
    let margins = instr_str
        .iter()
        .map(|str| " ".repeat(max_len - str.len()).to_smolstr())
        .collect::<Vec<SmolStr>>();
    for (i, instr) in instr_str.iter().enumerate() {
        let mut indicators: String = String::new();
        for x in &flows {
            if i == x.0 {
                if x.1 > x.0 {
                    indicators.push_str("  ─┐");
                } else {
                    indicators.push_str("  ─┘");
                }
            } else if i == x.1 {
                if x.1 > x.0 {
                    indicators.push_str(" <─┘");
                } else {
                    indicators.push_str(" <─┐");
                }
            } else if (x.0..x.1).contains(&i) || (x.1..x.0).contains(&i) {
                if i == instr_str.len() - 1 {
                    indicators.push_str("   X");
                } else {
                    indicators.push_str("   │");
                }
            } else {
                indicators.push_str("    ");
            }
        }
        println!("{instr}{}{}", margins[i], indicators);
    }
    println!("{color_yellow}------------------{color_reset}");
}

pub fn _format_registers_inline(registers: &[Data]) -> String {
    let mut output = String::new();
    registers.iter().enumerate().for_each(|(i, x)| {
        output.push_str(&format!("[{i}] {x:?} "));
    });
    output
}
