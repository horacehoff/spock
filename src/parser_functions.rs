use crate::parser::parse_code;
use crate::parser::Expr;
use crate::util::error;
use fancy_regex::Regex;
use std::fs;
use std::fs::File;
use std::io::Write;

pub fn parse_functions(content: &str) -> Vec<(String, Vec<String>, Vec<Vec<Expr>>)> {
    let mut functions: Vec<(&str, Vec<&str>, Vec<Vec<Expr>>)> = vec![];

    let hash = blake3::hash(content.as_bytes()).to_string();
    // if Path::new(&format!(".compute/{}", hash)).exists() {
    //     let file = File::open(&format!(".compute/{}", hash)).unwrap();
    //     let mut reader = BufReader::new(file);
    //     let mut buffer = Vec::new();
    //     reader.read_to_end(&mut buffer).unwrap();
    //
    //     let deserialized_data: Vec<(String, Vec<String>, Vec<Vec<Expr>>)> = bincode::deserialize(&buffer)
    //         .expect(error_msg!("Failed to read from cache", "Delete the .compute folder"));
    //     return deserialized_data;
    // }

    let comment_regex = Regex::new(r"(?m)(?<=\}|;|\{)\s*//.*$").unwrap();
    let content = comment_regex.replace_all(content, "").to_string();

    let mut i = 1;
    for line in content.lines() {
        let last_char = line.trim().chars().last().unwrap();
        if !(last_char == ';' || last_char == '{' || last_char == '}') {
            error(
                format!("Syntax error in line {}: \n{}", i, line).as_str(),
                "",
            );
        }
        i += 1;
    }

    // Parse functions
    let function_regex =
        Regex::new(r"(?ms)^func\s+(\w+)\s*\((.*?)\)\s*\{(.*?)}(?=((\s*func|\z)))").unwrap();
    let function_results: Vec<_> = function_regex.captures_iter(&content).collect();

    for func_match in function_results.iter() {
        let function = func_match.as_ref().unwrap();
        let parsed = parse_code(function.get(3).unwrap().as_str().trim());
        let args = function
            .get(2)
            .unwrap()
            .as_str()
            .split(",")
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>();
        functions.push((
            function.get(1).unwrap().as_str(),
            if args == vec![""] { vec![] } else { args },
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
        == 0
    {
        error("No main function", "Add 'func main() {}' to your file");
    }

    functions
        .iter()
        .map(|(a, b, c)| {
            (
                a.to_string(),
                b.iter().map(|s| s.to_string()).collect(),
                c.clone(),
            )
        })
        .collect()
}
