use crate::parser::Expr;
use crate::parser_data::{Function, Pools};
use crate::type_system::DataType;
use crate::{Data, Instr};
use inline_colorization::*;
use smol_str::{SmolStr, ToSmolStr};
use std::hint::unreachable_unchecked;

pub fn format_data(
    x: &Data,
    array_pool: &[Vec<Data>],
    string_pool: &[String],
    show_str: bool,
) -> SmolStr {
    if x.is_float() {
        x.as_float().to_smolstr()
    } else if x.is_int() {
        x.as_int().to_smolstr()
    } else if x.is_bool() {
        x.as_bool().to_smolstr()
    } else if x.is_str() {
        if show_str {
            x.as_str(string_pool).to_smolstr()
        } else {
            format_args!("\"{}\"", x.as_str(string_pool)).to_smolstr()
        }
    } else if x.is_array() {
        format_args!(
            "[{}]",
            array_pool[x.as_array()]
                .iter()
                .map(|x| format_data(x, array_pool, string_pool, false))
                .collect::<Vec<SmolStr>>()
                .join(","),
        )
        .to_smolstr()
    } else if x.is_null() {
        SmolStr::new_static("NULL")
    } else {
        unsafe { unreachable_unchecked() }
    }
}

pub fn display_fn_signatures(f: Function) {
    for fn_impl in f.impls {
        println!(
            "{} : ({}) -> {}",
            f.name,
            fn_impl
                .arg_types
                .iter()
                .map(|x| x.to_smolstr())
                .collect::<Vec<_>>()
                .join(", "),
            {
                let return_type = fn_impl.return_type;
                if return_type != DataType::Null {
                    return_type.to_smolstr()
                } else {
                    SmolStr::new_static("()")
                }
            }
        )
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
        Expr::Array(a, _) => format_args!(
            "[{}]",
            a.iter().map(format_expr).collect::<Vec<_>>().join(","),
        )
        .as_str()
        .unwrap()
        .to_smolstr(),
        Expr::Var(x, _) => x.to_smolstr(),
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

pub fn print_debug(instructions: &[Instr], registers: &[Data], pools: &Pools) {
    println!("{color_yellow}---- DEBUG ----{color_reset}");
    if !pools.array_pool.is_empty() {
        println!("{color_green}---  ARRAYS  ---{color_reset}");
        for (i, data) in pools.array_pool.iter().enumerate() {
            println!(" {i} {data:?}")
        }
    }
    println!("{color_green}-- REGISTERS --{color_reset}");
    for (i, data) in registers.iter().enumerate() {
        println!(
            " [{i}] {}({})",
            get_type_name(data),
            format_data(data, &pools.array_pool, &pools.string_pool, true)
        )
    }
    if instructions.is_empty() {
        return;
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
            Instr::CallFunc(n, _) => flows.push((i, *n as usize)),
            Instr::CallFuncRecursive(n, _) => flows.push((i, *n as usize)),
            Instr::JmpBack(jump_size) => flows.push((i, i.saturating_sub(*jump_size as usize))),
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
