use crate::error_msg;
use crate::instr_set::parser_to_instr_set;
use crate::parser::{parse_code, ComputeParser, Functions, ParserInstr, Rule};
use crate::util::error;
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
    if !Path::new(&format!(".compute/{}", hash)).exists() {
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
            let flat_code: Vec<ParserInstr> = parsed
                .iter()
                .flat_map(|line| line.iter().cloned())
                .collect();
            let mut locals: Vec<Intern<String>> = Vec::with_capacity(
                flat_code
                    .iter()
                    .filter(|obj| matches!(obj, ParserInstr::String(_)))
                    .count(),
            );
            println!("FLATTENED CODE IS {flat_code:?}");
            // convert "parser code" to the instr set
            let instr_code = parser_to_instr_set(flat_code, false, &mut locals);
            functions.push((name, args, instr_code, locals))
        }

        if !functions.iter().any(|(name, _, _, _)| **name == "main") && check_main {
            error("No main function", "Add 'func main() {}' to your file");
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
        let mut reader = BufReader::with_capacity(128 * 1024, file);
        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer).unwrap();

        functions = bincode::deserialize(&buffer).unwrap_or_else(|_| {
            panic!(
                "{}",
                error_msg!("Failed to read from cache", "Delete the .compute folder")
            )
        });
    }

    functions
}
