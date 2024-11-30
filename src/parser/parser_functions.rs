use crate::error_msg;
use crate::parser::parse_code;
use crate::parser::Types;
use crate::util::error;
use fancy_regex::Regex;
use goblin::Object;
use libloading::{library_filename, Library};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use smol_str::{SmolStr, ToSmolStr};
use std::fs;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;

pub fn parse_functions(
    content: &str,
    check_main: bool,
) -> Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)> {
    let mut functions: Vec<(&str, Vec<&str>, Vec<Vec<Types>>)> = vec![];

    let mut imported_functions: Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)> = vec![];
    let matches: Vec<(usize, &str)> = content.match_indices("\nimport").collect();
    if content.starts_with("import") {
        let mut i = 7;
        let mut match_content = &content[0..i];
        while !match_content.chars().last().unwrap().eq(&'\n') {
            i += 1;
            match_content = &content[0..i];
        }
        let name = String::from("./") + match_content.replace("import", "").trim().trim_end_matches(';');

        if name.ends_with(".compute") {
            // IS COMPUTE FILE
            let file_content = fs::read_to_string(&name).expect(error_msg!(format!(
            "Cannot find module {}",
            name.trim_start_matches("./")
        )));
            imported_functions.append(&mut parse_functions(&file_content, false));
        } else {
            // IS LIB -> .dll, .so, .dylib

            let libname = if cfg!(windows){format!("{name}.dll")} else if cfg!(target_os = "linux") {format!("{name}.so")} else {format!("{name}.dylib")};

            // let buffer = fs::read(path)
        }



    }
    for content_match in matches {
        let match_index = content_match.0;
        let mut i = 7;
        let mut match_content = &content[match_index..match_index + i];
        while !match_content.chars().last().unwrap().eq(&'\n') {
            i += 1;
            match_content = &content[match_index..match_index + i];
        }
        let name = String::from("./") + match_content.replace("import", "").trim();
        let file_content = fs::read_to_string(&name).expect(error_msg!(format!(
            "Cannot find module {}",
            name.trim_start_matches("./")
        )));
        imported_functions.append(&mut parse_functions(&file_content, false));
    }

    // let hash = blake3::hash(content.as_bytes()).to_string();
    // if Path::new(&format!(".compute/{}", hash)).exists() {
    //     let file = File::open(&format!(".compute/{}", hash)).unwrap();
    //     let mut reader = BufReader::with_capacity(128 * 1024, file);
    //     let mut buffer = Vec::new();
    //     reader.read_to_end(&mut buffer).unwrap();
    //
    //     let mut deserialized_data: Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)> =
    //         bincode::deserialize(&buffer).expect(error_msg!(
    //             "Failed to read from cache",
    //             "Delete the .compute folder"
    //         ));
    //     deserialized_data.append(&mut imported_functions);
    //     return deserialized_data;
    // }

    let comment_regex = Regex::new(r"(?m)(?<=\}|;|\{|.)\s*//.*$").unwrap();
    let mut content: String = comment_regex.replace_all(content, "").to_string();
    // let mut content: String = comment_regex.replace_all(content, "").to_string().lines().map(|ln| ln.trim()).collect();
    let mut i: i8 = 1;
    for content_lines in content.lines() {
        if !(content_lines.starts_with("import")
            || content_lines.starts_with("replace")
            || content_lines.trim().is_empty())
        {
            if !(content_lines.ends_with('{')
                || content_lines.ends_with("}")
                || content_lines.ends_with(';'))
            {
                if content_lines.starts_with("if")
                    || content_lines.starts_with("for")
                    || content_lines.starts_with("while")
                    || content_lines == "}"
                {
                    error(&format!("Missing bracket at line {i}"), "");
                } else {
                    error(&format!("Missing semicolon at line {i}"), "");
                }
            }
        }
        i += 1;
    }
    content = content.lines().map(str::trim).collect();

    let replace_regex =
        Regex::new(r"(?m)^replace (.+?)\s*->\s*(.+?)(?=\s*(?:func|import|replace|\z))").unwrap();
    for macro_match in replace_regex.captures_iter(&content.clone()) {
        let re_match = macro_match.unwrap();
        content = content
            .replace(re_match.get(0).unwrap().as_str(), "")
            .replace(
                re_match.get(1).unwrap().as_str().trim(),
                re_match.get(2).unwrap().as_str().trim(),
            );
    }

    // Parse functions
    let function_regex =
        Regex::new(r"(?ms)func\s+(\w+)\s*\((.*?)\)\s*\{(.*?)}\s*?(?=func|\z)").unwrap();
    let function_results: Vec<_> = function_regex.captures_iter(&content).collect();

    for func_match in function_results {
        let function = func_match.as_ref().unwrap();
        let parsed = parse_code(function.get(3).unwrap().as_str().trim());
        let args = function
            .get(2)
            .unwrap()
            .as_str()
            .split(',')
            .map(str::trim)
            .collect::<Vec<&str>>();
        functions.push((
            function.get(1).unwrap().as_str(),
            if args == vec![""] { vec![] } else { args },
            parsed,
        ));
    }
    // println!("CURRENT FUNCS{:?}", functions);

    // Cache functions
    // let data = bincode::serialize(&functions).unwrap();
    // fs::create_dir_all(".compute/").unwrap();
    // File::create(format!(".compute/{}", hash))
    //     .unwrap()
    //     .write_all(&data)
    //     .unwrap();

    if !functions
        .iter()
        .any(|function| function.0 == "main") && check_main
    {
        error("No main function", "Add 'func main() {}' to your file");
    }

    let mut return_functions = functions
        .iter()
        .map(|(a, b, c)| {
            (
                a.to_smolstr(),
                b.iter().map(smol_str::ToSmolStr::to_smolstr).collect(),
                c.clone(),
            )
        })
        .collect::<Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)>>();

    return_functions.append(&mut imported_functions);

    return_functions
}
