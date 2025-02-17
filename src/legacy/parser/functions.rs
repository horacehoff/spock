use crate::error;
use crate::instr_set::{parser_to_instr_set, Instr};
use crate::parser::{parse_code, ComputeParser, Functions, ParserInstr, Rule};
use internment::Intern;
use pest::Parser;
use std::fs;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;

pub fn parse_functions(content: String, check_main: bool) -> Functions {
    // TODO
    // IMPORT, COMMENTS, "replace" MACROS

    // hash input file's content to check if it has already been parsed
    let hash = blake3::hash(content.as_bytes()).to_string();

    let mut functions: Functions = Vec::with_capacity(
        content
            .as_bytes()
            .windows("\nfunc".len())
            .filter(|&w| w == "\nfunc".as_bytes())
            .count()
            + 1,
    );
    // if true --> file has never been parsed
    if !Path::new(&format!(".computeeee/{}", hash)).exists() {
        // Counts the number of functions that aren't defined in the text's first line, + 1 to account for any function defined in the first line
        let funcs = ComputeParser::parse(Rule::prog, &content)
            .unwrap()
            .next()
            .unwrap()
            .into_inner();
        for function in funcs {
            let mut func = function.into_inner();
            // name is next
            let name = Intern::<String>::from_ref(func.next().unwrap().as_str());
            // next is either function args or function code (see .pest file to understand why)
            let args_or_block = func.next().unwrap();
            let mut args: Vec<Intern<String>> = Vec::new();
            let code_txt: &str = if args_or_block.as_rule() == Rule::function_decl_args {
                args = args_or_block
                    .into_inner()
                    .map(|x| Intern::from_ref(x.as_span().as_str()))
                    .collect();
                func.next().unwrap().as_str()
            } else {
                args_or_block.as_str()
            };

            // parse function code
            let parsed = parse_code(code_txt);
            // flatten the nested vec
            let flat_code: Vec<ParserInstr> = parsed.iter().flat_map(|line| line.clone()).collect();
            // println!("{flat_code:?}");
            let mut locals: Vec<Intern<String>> = Vec::with_capacity(
                flat_code
                    .iter()
                    .filter(|obj| matches!(obj, ParserInstr::String(_)))
                    .count(),
            );
            let mut variables: Vec<Intern<String>> = args.clone();
            let mut consts_pos: Vec<Instr> = Vec::new();

            // convert "parser code" to the instr set
            let instr_code = parser_to_instr_set(
                flat_code,
                false,
                &mut locals,
                &mut variables,
                &mut consts_pos,
            );

            functions.push((name, args, instr_code, locals, variables, consts_pos))
        }

        if !functions
            .iter()
            .any(|(name, _, _, _, _, _)| **name == "main")
            && check_main
        {
            error!("No main function", "Add 'func main() {}' to your file");
        }

        let data = bincode::serialize(&functions).unwrap();
        fs::create_dir_all(".compute/").unwrap();
        File::create(format!(".compute/{}", hash))
            .unwrap()
            .write_all(&data)
            .unwrap();
    } else {
        // file has already been parsed, load it from cache
        let file = File::open(format!(".compute/{}", hash)).unwrap();
        let mut reader = BufReader::with_capacity(2048, file);
        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer).unwrap();

        functions = bincode::deserialize(&buffer).unwrap_or_else(|_| {
            error!("Failed to read from cache", "Delete the .compute folder");
        });
    }

    functions
}
