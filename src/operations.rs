use once_cell::sync::Lazy;
use regex::Regex;
use crate::types::{convert_str_to_native_type, Types};
use crate::util::error;
use fancy_regex::Regex as FancyRegex;


#[inline(always)]
pub fn f32_add(x: &f32, y: &f32) -> f32 {
    x + y
}

#[inline(always)]
pub fn f32_subtract(x: &f32, y: &f32) -> f32 {
    x - y
}

#[inline(always)]
pub fn f32_divide(x: &f32, y: &f32) -> f32 {
    x / y
}

#[inline(always)]
pub fn f32_multiply(x: &f32, y: &f32) -> f32 {
    x * y
}

#[inline(always)]
pub fn str_add(x: &str, y: &str) -> String {
    format!("{}{}", x, y)
}

#[inline(always)]
pub fn f32_str_add(x: &f32, y: &str) -> String {
    format!("{}{}", x, y)
}

#[inline(always)]
pub fn str_f32_add(x: &str, y: &f32) -> String {
    format!("{}{}", x, y)
}



pub fn str_capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

pub fn process_object_props(content: &String, index: i32) -> Types {
    // test regex ((?<=\")(?=\"))?((?<=\.)[^.\d]+(?:\.[^.\d]+))*$
    // DEFINITELY => (?<=\.)(\w|\.)+(?=\.|$)
    // NEW (letters) => (?<=\.)([a-zA-Z]|\.)+(?=\.|$)
    // NEW TEST => (?<=\.)([a-zA-Z]|\.|\(.*\))+(?=\.|$)
    static PROPERTY_REGEX: Lazy<FancyRegex> = Lazy::new(|| FancyRegex::new(r"(?<=\.)([a-zA-Z]|\.|\(.*\))+(?=\.|$)").unwrap());
    let mut rgx_split = Vec::new();

    if PROPERTY_REGEX.is_match(content).expect("") {
        let result = PROPERTY_REGEX.captures(content).expect("1").expect("1").get(0).unwrap().as_str();
        let result_dot = [".",result].join("");
        let split_suff = content.strip_suffix(&result_dot).unwrap();
        rgx_split.push(split_suff);
        rgx_split.append(&mut result.split('.').collect::<Vec<&str>>())
    } else {
        rgx_split.push(content)
    }

    let mut _i = 0;
    let mut result: Types = Types::Number(0.0);
    for token in rgx_split {
        if _i == 0 {
            result = convert_str_to_native_type(token, index);
        } else {
            match (&result, token) {
                (Types::String(value), value2) => {
                    if value2 == "lowercase" {
                        result = Types::String(value.to_lowercase())
                    } else if value2 == "uppercase" {
                        result = Types::String(value.to_uppercase())
                    } else if value2 == "capitalize" {
                        result = Types::String(str_capitalize(value))
                    } else if value2 == "strip" {
                        result = Types::String(String::from(value.trim()))
                    } else if value2 == "to_number" {
                        result = Types::Number(value.parse().unwrap())
                    } else if value2 == "is_number" {
                        result = Types::Boolean(value.parse::<f32>().is_ok())
                    } else if value2 == "is_letters" {
                        result = Types::Boolean(value.chars().all(char::is_alphabetic))
                    } else if &value2[..7] == "replace" {
                        static REPLACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"replace\((.*?)\)").unwrap());
                        static REPLACE_DATA: Lazy<Regex> = Lazy::new(|| Regex::new(r#""(.*)","(.*)""#).unwrap());
                        let capt = REPLACE_DATA.captures(REPLACE_REGEX.captures(value2).unwrap().get(1).unwrap().as_str()).unwrap();
                        result = Types::String(value.replace(capt.get(1).unwrap().as_str(), capt.get(2).unwrap().as_str()));

                    }


                    else {
                        error(index, &format!("Unknown property => {:?}", value2))
                    }
                },
                _ => {}
            }
        }
        _i += 1;
    }
    result
}
