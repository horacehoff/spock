use crate::parser::{parse_expression, ComputeParser, Rule};
use crate::parser::Expr;
use crate::util::error;
use fancy_regex::Regex;
use std::fs;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::ops::Index;
use std::path::Path;
use pest::Parser;
use crate::error_msg;

pub fn parse_functions(content: &str, check_main: bool) -> Vec<(String, Vec<String>, Vec<Vec<Expr>>)> {
    let mut functions: Vec<(&str, Vec<&str>, Vec<Vec<Expr>>)> = vec![];

    let import_regex = Regex::new(r"^import (.*);").unwrap();
    let mut imported_functions: Vec<(String, Vec<String>, Vec<Vec<Expr>>)> = vec![];
    for imp in import_regex.captures_iter(&content) {
        let name = "./".to_owned() + imp.unwrap().get(1).unwrap().as_str() +".compute";
        let file_content = fs::read_to_string(&name).expect(error_msg!(format!("Cannot find module {}", name.trim_start_matches("./"))));
        imported_functions.append(&mut parse_functions(&file_content, false));
    }

    let hash = blake3::hash(content.as_bytes()).to_string();
    if Path::new(&format!(".compute/{}", hash)).exists() {
        let file = File::open(&format!(".compute/{}", hash)).unwrap();
        let mut reader = BufReader::new(file);
        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer).unwrap();

        let mut deserialized_data: Vec<(String, Vec<String>, Vec<Vec<Expr>>)> = bincode::deserialize(&buffer)
            .expect(error_msg!("Failed to read from cache", "Delete the .compute folder"));
        deserialized_data.append(&mut imported_functions);
        return deserialized_data;
    }

    let comment_regex = Regex::new(r"(?m)(?<=\}|;|\{)\s*//.*$").unwrap();
    let mut content: String = comment_regex.replace_all(content, "").to_string().lines().map(|ln| ln.trim()).collect();

    let macro_regex = Regex::new(r"(?m)^macro (\S*)->(\S*);").unwrap();
    for macro_match in macro_regex.captures_iter(&content.clone()) {
        let re_match = macro_match.unwrap();
        content = content.replace(re_match.get(0).unwrap().as_str(),"").replace(re_match.get(1).unwrap().as_str().trim(), re_match.get(2).unwrap().as_str().trim());

    }

    println!("CONTENT{:?}", &content);
    // Parse functions
    let functions_pairs = ComputeParser::parse(Rule::function, &content).unwrap();
    for func in functions_pairs {
        let mut parsed: Vec<Vec<Expr>> = vec![];
        let mut inner_rules = func.into_inner();
        let name = inner_rules.next().unwrap().as_str();
        let args = inner_rules.clone().rev().skip(1).rev().next();
        let mut func_args: Vec<&str> = vec![];
        if args.iter().len() > 0 {
            func_args = args.unwrap().into_inner().map(|arg| arg.as_str()).collect();
        }

        for inner_rule in inner_rules.last().unwrap().into_inner() {
            parsed.push(parse_expression(inner_rule))
        }

        functions.push((
            name,
            func_args,
            parsed,
        ));
    }



    // Cache functions
    let data = bincode::serialize(&functions).unwrap();
    fs::create_dir_all(".compute/").unwrap();
    File::create(format!(".compute/{}", hash))
        .unwrap()
        .write_all(&data)
        .unwrap();

    if functions
        .clone()
        .into_iter()
        .filter(|function| function.0 == "main")
        .collect::<Vec<(&str, Vec<&str>, Vec<Vec<Expr>>)>>()
        .len()
        == 0 && check_main
    {
        error("No main function", "Add 'func main() {}' to your file");
    }

    let mut return_functions =
        functions.iter()
        .map(|(a, b, c)| {
            (
                a.to_string(),
                b.iter().map(|s| s.to_string()).collect(),
                c.clone(),
            )
        })
        .collect::<Vec<(String, Vec<String>, Vec<Vec<Expr>>)>>();

    return_functions.append(&mut imported_functions);

    return_functions
}
