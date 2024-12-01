#![allow(clippy::too_many_lines)]
#[path = "types/array.rs"]
mod array;
#[path = "types/file.rs"]
mod file;
#[path = "types/float.rs"]
mod float;
#[path = "types/integer.rs"]
mod integer;
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
use crate::float::float_ops;
use crate::integer::integer_ops;
use crate::namespaces::namespace_functions;
use crate::parser::{parse_code, BasicOperator, Types};
use crate::parser_functions::parse_functions;
use crate::preprocess::preprocess;
use crate::string::string_ops;
use crate::util::{error, get_printable_form};
use branches::likely;
use branches::unlikely;
use const_currying::const_currying;
use gxhash::HashMap;
use inflector::Inflector;
use smol_str::{SmolStr, StrExt, ToSmolStr};
use std::fs::remove_dir_all;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::time::Instant;
use std::{fs, io};
use unroll::unroll_for_loops;

#[const_currying]
fn builtin_functions(
    x: &str,
    #[maybe_const(dispatch = args, consts = [[Parser:Expr; 0]])] args: &Vec<Types>,
) -> (Types, bool) {
    match x {
        "print" => {
            assert_args_number!("print", args.len(), 1);
            if let Types::String(str) = &args[0] {
                println!("{}", str);
            } else {
                println!("{}", get_printable_form(&args[0]));
            }
            return (Types::Null, true);
        }
        "abs" => {
            assert_args_number!("abs", args.len(), 1);
            match &args[0] {
                Types::Float(val) => return (Types::Float(val.abs()), true),
                Types::Integer(val) => return (Types::Integer(val.abs()), true),
                _ => error(
                    &format!("Cannot get absolute value of {:?} type", &args[0]),
                    "Change type",
                ),
            }
            return (Types::Null, true);
        }
        "round" => {
            assert_args_number!("round", args.len(), 1);
            match &args[0] {
                Types::Float(val) => return (Types::Integer(val.round() as i64), true),
                Types::Integer(val) => return (Types::Integer(*val), true),
                _ => error(
                    &format!("Cannot round {} type", get_printable_type!(&args[0])),
                    "Change type",
                ),
            }
            return (Types::Null, true);
        }
        "len" => {
            assert_args_number!("len", args.len(), 1);
            match &args[0] {
                Types::String(val) => {
                    return (Types::Integer(val.len() as i64), true);
                }
                Types::Array(val) => {
                    return (Types::Integer(val.len() as i64), true);
                }
                _ => error(
                    &format!(
                        "Cannot get length of type {}",
                        get_printable_type!(&args[0])
                    ),
                    "Change type",
                ),
            }
            return (Types::Null, true);
        }
        "input" => {
            assert_args_number!("input", args.len(), 0, 1);
            if args.len() == 1 {
                if_let!(likely, Types::String(prompt), &args[0], {
                    print!("{}", prompt);
                }, else {
                    error(
                        &format!("Cannot print {} type", get_printable_type!(&args[0])),
                        "Change type",
                    );
                });
            }
            io::stdout().flush().unwrap();
            return (
                Types::String(
                    BufReader::new(io::stdin())
                        .lines()
                        .next()
                        .expect(error_msg!("Failed to read input"))
                        .unwrap()
                        .as_str()
                        .parse()
                        .unwrap(),
                ),
                true,
            );
        }
        "type" => {
            assert_args_number!("type", args.len(), 1);
            return (Types::String(get_printable_type!(&args[0]).into()), true);
        }
        "hash" => {
            assert_args_number!("hash", args.len(), 1);
            return (
                Types::String(
                    blake3::hash(
                        bincode::serialize(&args[0])
                            .expect(error_msg!(format!(
                                "Failed to compute hash of object {:?}",
                                &args[0]
                            )))
                            .as_ref(),
                    )
                    .to_smolstr(),
                ),
                true,
            );
        }
        "sqrt" => {
            assert_args_number!("sqrt", args.len(), 1);
            if let Types::Integer(int) = args[0] {
                return (Types::Float((int as f64).sqrt()), true);
            } else if let Types::Float(float) = args[0] {
                return (Types::Float(float.sqrt()), true);
            } else {
                error(
                    format!("Cannot calculate the square root of {:?}", args[0]).as_str(),
                    "",
                );
            }
        }
        "the_answer" => {
            println!(
                "42, the answer to the Ultimate Question of Life, the Universe, and Everything."
            );
            return (Types::Integer(42), true);
        }
        "range" => {
            assert_args_number!("sqrt", args.len(), 1, 3);
            if args.len() == 1 {
                if_let!(likely, Types::Integer(lim), args[0], {
                    // return (Types::Array((0..lim).map(Types::Integer).collect()), true)
                        let mut vec = Vec::with_capacity(lim as usize);
                        for i in 0..lim {
                            vec.push(Types::Integer(i));
                        }
                        return (Types::Array(vec), true);
                }, else {
                    error("Invalid range limit", "");
                })
            } else if args.len() == 2 {
                if_let!(likely, Types::Integer(lim), args[0], {
                    if_let!(Types::Integer(upplim), args[1], {
                        return (
                            Types::Array((lim..upplim).map(Types::Integer).collect()),
                            true,
                        )
                    }, else {
                        error("Invalid range limit", "");
                    })
                }, else {
                    error("Invalid range start", "");
                })
            } else if args.len() == 3 {
                if_let!(likely, Types::Integer(start), args[0], {
                    if_let!(Types::Integer(stop), args[1], {
                        if_let!(Types::Integer(step), args[2], {
                            if unlikely(step == 0) {
                                error("Step cannot be zero", "");
                            } else {
                                let range = if step > 0 {
                                    (start..stop).step_by(step as usize)
                                } else {
                                    (stop..start).step_by((-step) as usize)
                                };
                                return (Types::Array(range.map(Types::Integer).collect()), true)
                            }
                        }, else {
                            error("Invalid range step", "");
                        })
                    }, else {
                        error("Invalid range limit", "");
                    })
                }, else {
                    error("Invalid range start", "");
                })
            } else {
                error("Invalid range arguments", "");
            }
        }

        _ => return (Types::Null, false),
    }
    (Types::Null, false)
}

#[inline(always)]
#[unroll_for_loops]
fn process_stack(
    stack_in: &[Types],
    variables: &HashMap<SmolStr, Types>,
    functions: &[(SmolStr, Vec<SmolStr>, &[Vec<Types>])],
) -> Types {
    let mut output: Types = match stack_in.first().unwrap_or_else(|| {return &Types::Integer(0); panic!()}) {
        Types::VariableIdentifier(ref var) => variables
            .get(var)
            .unwrap_or_else(|| { error("Unknown variable","");panic!() })
            .to_owned(),
        other => {
            let value = preprocess(variables, functions, other);
            if value == Types::Null {
                other.to_owned()
            } else {
                value
            }
        }
    };
    let mut current_operator: BasicOperator = BasicOperator::Null;
    for p_element in stack_in.iter().skip(1) {
        let mut process = Types::Null;
        let element = if let Types::VariableIdentifier(var) = p_element {
            variables
            .get(var)
            .unwrap_or_else(|| { error(&format!("Unknown variable '{var}'"),"");panic!()})
        } else {
            process = preprocess(variables, functions, p_element);
            if process == Types::Null {
                p_element
            } else {
                &process
            }
        };

        match element {
            Types::Operation(ref op) => {
                current_operator = *op;
            }
            Types::Integer(ref x) => {
                output = integer_ops(*x, &output, current_operator);
            }
            Types::String(ref x) => {
                output = string_ops(x, &output, current_operator);
            }
            Types::Float(ref x) => {
                output = float_ops(*x, &output, current_operator);
            }
            Types::Array(ref x) => output = array_ops(x, &output, current_operator),
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
            Types::Property(x) => {
                // TODO
                todo!("Properties aren't implented yet!")
            }
            Types::PropertyFunction(ref x, ref y) => {
                let args: Vec<Types> = y
                    .iter()
                    .map(|arg| process_stack(arg, variables, functions))
                    .collect();

                if let Types::String(ref str) = &output {
                    string_props!(str, args, x, output);
                    // str.to_smolstr()
                } else if let Types::Float(num) = output {
                    float_props!(num, args, x, output);
                } else if let Types::Integer(num) = output {
                    integer_props!(num, args, x, output);
                } else if let Types::Array(ref arr) = output {
                    array_props!(arr, args, x, output);
                } else if let Types::File(ref filepath) = &output {
                    file_props!(filepath, args, x, output);
                }
            }
            Types::Or(ref x) => {
                let parsed_exp = process_stack(x, variables, functions);
                if_let!(
                    likely,
                    Types::Bool(inbool),
                    output,
                    {
                        if_let!(likely, Types::Bool(sidebool), parsed_exp, {
                            output = Types::Bool(inbool || sidebool);
                        }, else {
                            error(format!("{parsed_exp:?} is not a Boolean").as_str(), "");
                        });
                    }, else
                    {
                        error(format!("{output:?} is not a Boolean").as_str(), "");
                    }
                );
            }
            Types::And(ref x) => {
                let parsed_exp = process_stack(x, variables, functions);
                if_let!(
                    likely,
                    Types::Bool(inbool),
                    output,
                    {
                        if_let!(likely, Types::Bool(sidebool), parsed_exp, {
                            output = Types::Bool(inbool && sidebool);
                        }, else {
                            error(format!("{parsed_exp:?} is not a Boolean").as_str(), "");
                        });
                    }, else
                    {
                        error(format!("{output:?} is not a Boolean").as_str(), "");
                    }
                );
            }
            _ => todo!(),
        }
        // }
        // else {
        //     output = element.to_owned();
        // }
    }
    // println!("OUTP {output:?}");
    output
}

#[inline(always)]
fn process_line_logic(line_array: &[Types], variables: &mut HashMap<SmolStr, Types>) -> Types {
    for line in line_array {
        match line {
            Types::Wrap(ref x) => {
                let x = process_line_logic(x, variables);
                if x != Types::Null {
                    return x;
                }
            }
            Types::VariableDeclaration(ref x, ref y) => {
                variables.insert(x.to_smolstr(), process_stack(y, variables, &[]));
            }
            Types::VariableRedeclaration(ref x, ref y) => {
                let calculated = process_stack(y, variables, &[]);
                if let Some(x) = variables.get_mut(x) {
                    *x = calculated;
                }
            }
            Types::NamespaceFunctionCall(ref namespace, ref y, ref z) => {
                let args: Vec<Types> = z
                    .iter()
                    .map(|arg| process_stack(arg, variables, &[]))
                    .collect();
                if unlikely(!namespace_functions(namespace, y, &args).1) {
                    error(
                        &format!("Unknown function '{}'", namespace.join(".") + "." + y),
                        "",
                    );
                };
            }
            Types::FunctionCall(ref x, ref y) => {
                // println!("{:?}", y);
                let args: Vec<Types> = y
                    .iter()
                    .map(|arg| process_stack(arg, variables, &[]))
                    .collect();

                let (_, matched) = builtin_functions(x, &args);
                if x == "executeline" && !matched {
                    assert_args_number!("executeline", args.len(), 1);
                    if_let!(likely, Types::String(line), &args[0], {
                        process_stack(&parse_code(line)[0], variables, &[]);
                    }, else {
                        error(&format!("Cannot execute line {:?}", &args[0]), "");
                    });
                } else if !matched {
                    todo!("Functions are WIP")
                    // let target_function: &(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>) = functions
                    //     .into_iter()
                    //     .filter(|func| func.0 == *x)
                    //     .next()
                    //     .expect(error_msg!(&format!("Unknown function '{}'", x)));
                    // assert_args_number!(&x, args.len(), target_function.1.len());
                    // let mut target_args: Vec<Variable> = target_function
                    //     .1
                    //     .iter()
                    //     .enumerate()
                    //     .map(|(i, arg)| Variable {
                    //         name: arg.to_smolstr(),
                    //         value: args[i].clone(),
                    //     })
                    //     .collect();
                    // let len = target_args.len();
                    // process_function(
                    //     &target_function.2,
                    //     &mut target_args,
                    //     len,
                    //     &target_function.0,
                    //     functions,
                    //     None
                    // );
                }
            }
            Types::Condition(ref x, ref y, ref z) => {
                if let Types::Bool(true) = process_stack(x, variables, &[]) {
                    let out = process_line_logic(y, variables);
                    if Types::Null != out {
                        return out;
                    }
                } else {
                    for else_block in z {
                        if else_block.0.is_empty()
                            || process_stack(&else_block.0, variables, &[]) == Types::Bool(true)
                        {
                            let out = process_line_logic(&else_block.1, variables);
                            if out != Types::Null {
                                return out;
                            }
                        }
                    }
                }
            }
            Types::Loop(ref x, ref y, ref z) => {
                let loop_array = process_stack(y, variables, &[]);
                if let Types::Array(target_array) = loop_array {
                    variables.insert(x.to_smolstr(), Types::Null);
                    for elem in target_array {
                        if let Some(value) = variables.get_mut(x) {
                            *value = elem;
                        }

                        let out_expr: Types = process_line_logic(z, variables);
                        if out_expr != Types::Null {
                            variables.remove(x);
                            return out_expr;
                        }
                    }
                    variables.remove(x);
                } else if let Types::String(ref target_string) = loop_array {
                    for elem in target_string.chars() {
                        let out_expr: Types = {
                            let result = process_line_logic(z, variables);
                            // };
                            Types::Null
                        };
                    }
                }
            }
            Types::While(ref x, ref y) => {
                while let Types::Bool(true) = process_stack(x, variables, &[]) {
                    let out = process_line_logic(y, variables);
                    if out != Types::Null {
                        if out == Types::Break {
                            break;
                        }
                        return out;
                    }
                }
            }
            Types::FunctionReturn(ref x) => {
                return process_stack(x, variables, &[]);
            }
            Types::Break => {
                return Types::Break;
            }
            _ => panic!("{}", error_msg!("TODO!!")),
        }
    }
    Types::Null
}

fn process_function(lines: &[Vec<Types>], variables: &mut HashMap<SmolStr, Types>, is_loop: bool) -> Types {
    for line in lines {
        let processed:Types = process_line_logic(line, variables);
        if processed != Types::Null {
            return processed;
        };
    }
    Types::Null
}

fn main() {
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        println!(
            "
  ______   ______   .___  ___. .______    __    __  .___________. _______
 /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
|  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
|  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
|  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
 \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
\x1b[3mLive long and prosper!\x1b[0m\n- Spock

To run a file, run: `compute <file>`
To get help, run `compute -h`
        "
        );
        return;
    } else if args == vec!["-h"] {
        println!(
            "
  ______   ______   .___  ___. .______    __    __  .___________. _______
 /      | /  __  \\  |   \\/   | |   _  \\  |  |  |  | |           ||   ____|
|  ,----'|  |  |  | |  \\  /  | |  |_)  | |  |  |  | `---|  |----`|  |__
|  |     |  |  |  | |  |\\/|  | |   ___/  |  |  |  |     |  |     |   __|
|  `----.|  `--'  | |  |  |  | |  |      |  `--'  |     |  |     |  |____
 \\______| \\______/  |__|  |__| | _|       \\______/      |__|     |_______|\n
\x1b[3mHelp me, Obi-Wan Kenobi. You’re my only hope.\x1b[0m\n- Princess Leia

compute [filename] [-c]

positional arguments:
  filename

options:
  -c, --clear-cache    Delete the cache folder
        "
        );
        return;
    } else if args.len() >= 2
        && (&args[1] == "-c" || &args[1] == "--clear-cache")
        && Path::new(".compute").exists()
    {
        remove_dir_all(Path::new(".compute"))
            .unwrap_or_else(|_| {error("Failed to delete the cache folder (.compute)","");panic!()});
    }
    let arg = args.first().unwrap();

    let content =
        fs::read_to_string(arg).unwrap_or_else(|_| {error(&format!("Unable to read file '{arg}'"),"");panic!()});

    let now = Instant::now();
    let functions: Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)> =
        parse_functions(content.trim(), true);
    log!("PARSED IN: {:.2?}", now.elapsed());
    log!("FUNCTIONS {:?}", functions);

    let main_instructions = functions
        .clone()
        .into_iter()
        .filter(|function| function.0 == "main")
        .collect::<Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Types>>)>>();

    let now = Instant::now();
    // thread::Builder::new()
    //     // 16MB stack size
    //     .stack_size(16 * 1024 * 1024)
    //     .spawn(move || {
    //         process_function(&main_instructions[0].2, &mut Vec::new(), 0, "main", &functions);
    //     })
    //     .unwrap()
    //     .join()
    //     .unwrap();
    // process_function(&main_instructions[0].2, &mut Vec::new(), 0, "main", &functions,None);

    let mut vars: HashMap<SmolStr, Types> = HashMap::default();
    // process_line_logic(&Types::VariableDeclaration("hey".to_smolstr(), vec![Types::Integer(56)]), &mut vars);
    // process_line_logic(&Types::VariableRedeclaration("hey".to_smolstr(), vec![Types::Integer(512)]), &mut vars);

    process_function(&main_instructions.first().unwrap().2, &mut vars, false);
    // println!("{:?}VARS", vars);

    log_release!("EXECUTED IN: {:.2?}", now.elapsed());
    log_release!("TOTAL: {:.2?}", totaltime.elapsed());
}
