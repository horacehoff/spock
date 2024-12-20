#![allow(clippy::too_many_lines)]
#[path = "types/array.rs"]
mod array;
#[path = "functions/builtin.rs"]
mod builtin;
#[path = "types/file.rs"]
mod file;
#[path = "types/float.rs"]
mod float;
#[path = "types/integer.rs"]
mod integer;
#[path = "functions/namespaces.rs"]
mod namespaces;
#[path = "parser/parser.rs"]
mod parser;
#[path = "parser/parser_functions.rs"]
mod parser_functions;
mod preprocess;
#[path = "types/string.rs"]
mod string;
mod util;

use crate::array::array_ops;
use crate::builtin::builtin_functions;
use crate::float::float_ops;
use crate::integer::integer_ops;
use crate::namespaces::namespace_functions;
use crate::parser::{parse_code, BasicOperator, FunctionPropertyCallBlock, Types};
use crate::parser_functions::parse_functions;
use crate::preprocess::preprocess;
use crate::string::{string_ops, to_title_case};
use crate::util::{error, get_printable_form};
use branches::likely;
use branches::unlikely;
use gxhash::HashMap;
use smol_str::{SmolStr, StrExt as _, ToSmolStr as _};
use snmalloc_rs::SnMalloc;
use std::fs;
use std::io::Write as _;
use std::time::Instant;
use unroll::unroll_for_loops;

#[global_allocator]
static ALLOC: SnMalloc = SnMalloc;

#[unroll_for_loops]
#[inline(always)]
fn process_stack(
    stack_in: &[Types],
    variables: &HashMap<SmolStr, Types>,
    functions: &[(SmolStr, &[SmolStr], &[Types])],
) -> Types {
    let mut output: Types = match stack_in.first().unwrap_or(&Types::Integer(0)) {
        Types::VariableIdentifier(ref var) => variables
            .get(var)
            .unwrap_or_else(|| {
                error(&format!("Unknown variable '{var}'"), "");
                std::process::exit(1)
            })
            .clone(),
        Types::Wrap(ref x) => process_stack(x, variables, functions),
        other => {
            if !matches!(
                other,
                Types::FunctionCall(_)
                    | Types::NamespaceFunctionCall(_)
                    | Types::Priority(_)
                    | Types::Array(_, _, _)
            ) {
                other.clone()
            } else {
                preprocess(variables, functions, other)
            }
        }
    };
    let mut current_operator: BasicOperator = BasicOperator::Null;
    let mut process: Types;
    for p_element in stack_in.iter().skip(1) {
        let element = match p_element {
            Types::VariableIdentifier(var) => variables.get(var).unwrap_or_else(|| {
                error(&format!("Unknown variable '{var}'"), "");
                std::process::exit(1)
            }),
            Types::Wrap(x) => &process_stack(x, variables, functions),
            other => {
                if !matches!(
                    other,
                    Types::FunctionCall(_)
                        | Types::NamespaceFunctionCall(_)
                        | Types::Priority(_)
                        | Types::Array(_, _, _)
                ) {
                    other
                } else {
                    process = preprocess(variables, functions, other);
                    &process
                }
            }
        };

        match element {
            Types::Operation(ref op) => current_operator = *op,
            Types::Integer(ref x) => output = integer_ops(*x, &output, current_operator),
            Types::String(ref x) => output = string_ops(x, &output, current_operator),
            Types::Float(ref x) => output = float_ops(*x, &output, current_operator),
            Types::Array(ref x, _, false) => output = array_ops(x, &output, current_operator),
            Types::Property(ref block) => {
                let args: Vec<Types> = util::split_vec_box(&block.args, Types::Separator)
                    .iter()
                    .map(|w| process_stack(w, variables, functions))
                    .collect();
                match output {
                    Types::String(ref str) => string_props!(str, args, block.name, output),
                    Types::Float(num) => float_props!(num, args, block.name, output),
                    Types::Integer(num) => integer_props!(num, args, block.name, output),
                    Types::Array(ref arr, _, false) => {
                        array_props!(arr, args, block.name, output)
                    }

                    // OBJECTS
                    Types::File(ref filepath) => file_props!(filepath, args, block.name, output),
                    _ => error(
                        &format!(
                            "Unknown function '{}' for object {}",
                            block.name,
                            get_printable_type!(output)
                        ),
                        "",
                    ),
                }
            }
            Types::Or(ref x) | Types::And(ref x) => {
                let parsed_exp = process_stack(x, variables, functions);
                if_let!(
                    likely,
                    Types::Bool(inbool),
                    output,
                    {
                        if_let!(likely, Types::Bool(sidebool), parsed_exp, {
                            output = Types::Bool(if matches!(element, Types::Or(_)) {
                                inbool || sidebool
                            } else {
                                inbool && sidebool
                            });
                        }, else {
                            error(format!("{parsed_exp:?} is not a Boolean").as_str(), "");
                        });
                    }, else
                    {
                        error(format!("{output:?} is not a Boolean").as_str(), "");
                    }
                );
            }
            Types::Null => {
                if output == Types::Null {
                    match current_operator {
                        BasicOperator::Equal => output = Types::Bool(true),
                        BasicOperator::NotEqual => output = Types::Bool(false),
                        _ => error(
                            &format!(
                                "Cannot perform operation '{current_operator:?}' between Null and Null"
                            ),
                            "",
                        ),
                    }
                }
            }
            _ => error(&format!("TODO STACK {element:?}"), ""),
        }
    }
    output
}

#[inline(always)]
fn process_line_logic(
    line_array: &[Types],
    variables: &mut HashMap<SmolStr, Types>,
    functions: &[(SmolStr, &[SmolStr], &[Types])],
) -> Types {
    for line in line_array.iter() {
        match line {
            Types::Wrap(ref x) => {
                let x = process_line_logic(x, variables, functions);
                if x != Types::Null {
                    return x;
                }
            }
            Types::VariableDeclaration(ref block) => {
                if !block.is_declared {
                    variables.insert(
                        block.name.to_smolstr(),
                        process_stack(&block.value, variables, functions),
                    );
                } else {
                    let calculated = process_stack(&block.value, variables, functions);
                    if let Some(x) = variables.get_mut(&block.name) {
                        *x = calculated;
                    }
                }
            }
            // Types::VariableRedeclaration(ref x, ref y) => {
            //     let calculated = process_stack(y, variables, functions);
            //     if let Some(x) = variables.get_mut(x) {
            //         *x = calculated;
            //     }
            // }
            // Types::NamespaceFunctionCall(ref namespace, ref y, ref z) => {
            Types::NamespaceFunctionCall(ref block) => {
                let args: Vec<Types> = util::split_vec_box(&block.args, Types::Separator)
                    .iter()
                    .map(|w| process_stack(w, variables, functions))
                    .collect();
                if unlikely(!namespace_functions(&block.namespace, &block.name, &args).1) {
                    error(
                        &format!(
                            "Unknown function '{}'",
                            block.namespace.join(".") + "." + &block.name
                        ),
                        "",
                    );
                };
            }
            Types::FunctionCall(ref block) => {
                let args: Vec<Types> = util::split_vec_box(&block.args, Types::Separator)
                    .iter()
                    .map(|x| process_stack(x, variables, functions))
                    .collect();
                let (_, matched) = builtin_functions(&block.name, &args);
                if block.name == "executeline" && !matched {
                    assert_args_number!("executeline", args.len(), 1_usize);
                    if_let!(likely, Types::String(line), &args[0], {
                        process_stack(&parse_code(line)[0], variables, functions);
                    }, else {
                        error(&format!("Cannot execute line {:?}", &args[0]), "");
                    });
                } else if !matched {
                    let target_function: &(SmolStr, &[SmolStr], &[Types]) = functions
                        .iter()
                        .find(|func| func.0 == *block.name)
                        .unwrap_or_else(|| {
                            error(&format!("Unknown function '{}'", block.name), "");
                            std::process::exit(1)
                        });
                    assert_args_number!(block.name, args.len(), target_function.1.len());
                    let mut target_args: HashMap<SmolStr, Types> = target_function
                        .1
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| (arg.to_smolstr(), args[i].clone()))
                        .collect();

                    process_function(target_function.2, &mut target_args, functions);
                }
            }
            Types::PropertyFunction(ref block) => {
                let result = process_line_logic(
                    &[Types::FunctionCall(Box::from(FunctionPropertyCallBlock {
                        name: block.func1_name.to_smolstr(),
                        args: block.func1_args.clone(),
                    }))],
                    variables,
                    functions,
                );
                return process_stack(
                    &[
                        result,
                        Types::Property(Box::from(FunctionPropertyCallBlock {
                            name: block.func2_name.to_smolstr(),
                            args: block.func2_args.clone(),
                        })),
                    ],
                    variables,
                    functions,
                );
            }
            Types::Condition(ref block) => {
                // let data = block;
                if let Types::Bool(true) = process_stack(&block.condition, variables, functions) {
                    let out = process_line_logic(&block.code, variables, functions);
                    if Types::Null != out {
                        // if out != Types::Break {
                        return out;
                        // }
                        // error("Cannot break in a conditional statement","Remove the \"break\" statement");
                    }
                } else {
                    for else_block in &block.else_blocks {
                        if else_block.0.is_empty()
                            || process_stack(&else_block.0, variables, functions)
                                == Types::Bool(true)
                        {
                            let out = process_line_logic(&else_block.1, variables, functions);
                            if out != Types::Null {
                                return out;
                            }
                        }
                    }
                }
            }
            Types::Loop(ref block) => {
                let loop_array = process_stack(&block.arr, variables, functions);
                if let Types::Array(target_array, false, false) = loop_array {
                    variables.insert(block.id.to_smolstr(), Types::Null);
                    for elem in target_array {
                        if let Some(value) = variables.get_mut(&block.id) {
                            *value = elem;
                        }

                        let out: Types = process_line_logic(&block.code, variables, functions);
                        if out != Types::Null {
                            variables.remove(&block.id);
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    variables.remove(&block.id);
                } else if let Types::String(ref target_string) = loop_array {
                    variables.insert(block.id.to_smolstr(), Types::Null);
                    for elem in target_string.chars() {
                        if let Some(value) = variables.get_mut(&block.id) {
                            *value = Types::String(elem.to_smolstr());
                        }

                        let out: Types = process_line_logic(&block.code, variables, functions);
                        if out != Types::Null {
                            variables.remove(&block.id);
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    variables.remove(&block.id);
                }
            }
            Types::While(ref block) => {
                while let Types::Bool(true) = process_stack(&block.condition, variables, functions)
                {
                    let out = process_line_logic(&block.code, variables, functions);
                    if out != Types::Null {
                        if out == Types::Break {
                            break;
                        }
                        return out;
                    }
                }
            }
            Types::FunctionReturn(ref x) => {
                return process_stack(x, variables, functions);
            }
            Types::Break => {
                return Types::Break;
            }
            _ => error(&format!("TODO LINE LOGIC {line:?}"), ""),
        }
    }
    Types::Null
}

fn process_function(
    lines: &[Types],
    variables: &mut HashMap<SmolStr, Types>,
    functions: &[(SmolStr, &[SmolStr], &[Types])],
) -> Types {
    let processed = process_line_logic(lines, variables, functions);
    if processed != Types::Null {
        return processed;
    }
    Types::Null
}

fn main() {
    dbg!(std::mem::size_of::<Types>());
    dbg!(std::mem::size_of::<BasicOperator>());
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args().skip(1).collect();
    //     if args.is_empty() {
    //         println!(
    //             "
    //   ______   ______   .___  ___. .______    __    __  .___________. _______
    //  /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
    // |  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
    // |  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
    // |  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
    //  \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
    // \x1b[3mLive long and prosper!\x1b[0m\n- Spock
    //
    // To run a file, run: `compute <file>`
    // To get help, run `compute -h`
    //         "
    //         );
    //         return;
    //     } else if args == vec!["-h"] {
    //         println!(
    //             "
    //   ______   ______   .___  ___. .______    __    __  .___________. _______
    //  /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
    // |  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
    // |  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
    // |  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
    //  \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
    // \x1b[3mHelp me, Obi-Wan Kenobi. You’re my only hope.\x1b[0m\n- Princess Leia
    //
    // compute [filename] [-c]
    //
    // positional arguments:
    //   filename
    //
    // options:
    //   -c, --clear-cache    Delete the cache folder
    //         "
    //         );
    //         return;
    //     } else if args.len() >= 2
    //         && (&args[1] == "-c" || &args[1] == "--clear-cache")
    //         && Path::new(".compute").exists()
    //     {
    //         remove_dir_all(Path::new(".compute")).unwrap_or_else(|_| {
    //             error("Failed to delete the cache folder (.compute)", "");
    //             std::process::exit(1)
    //         });
    //     }
    //     let arg = args.first().unwrap();

    let content = fs::read_to_string("example.compute").unwrap_or_else(|_| {
        error(&format!("Unable to read file '{args:?}'"), "");
        std::process::exit(1)
    });

    let now = Instant::now();

    let temp_funcs = parse_functions(content.trim(), true);
    let mut main_function: (SmolStr, &[SmolStr], Vec<Types>) = Default::default();

    let partial_convert: Vec<(SmolStr, &[SmolStr], Vec<Types>)> = temp_funcs
        .iter()
        .map(|(name, args, lines)| {
            let stack: Vec<Types> = lines
                .iter()
                .flat_map(|line| line.iter().map(|x| (*x).clone()))
                .collect();
            if name == "main" {
                main_function = (name.clone(), args.as_slice(), stack);
                return (name.clone(), args.as_slice(), vec![]);
            }
            return (name.clone(), args.as_slice(), stack);
        })
        .filter(|(name, _, _)| name != "main")
        .collect();

    let converted: Vec<(SmolStr, &[SmolStr], &[Types])> = partial_convert
        .iter()
        .map(|(name, args, lines)| (name.to_smolstr(), *args, lines.as_slice()))
        .collect();

    let functions: &[(SmolStr, &[SmolStr], &[Types])] = converted.as_slice();

    log!("PARSED IN: {:.2?}", now.elapsed());
    log!(
        "MAIN\n{}",
        main_function
            .2
            .iter()
            .map(|x| format!("{x:?}"))
            .collect::<Vec<String>>()
            .join("\n")
    );

    let now = Instant::now();

    let mut vars: HashMap<SmolStr, Types> = HashMap::default();
    process_function(&main_function.2, &mut vars, functions);

    log!("EXECUTED IN: {:.2?}", now.elapsed());
    log_release!("TOTAL: {:.2?}", totaltime.elapsed());
}
