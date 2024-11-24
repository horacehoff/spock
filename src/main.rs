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
use crate::parser::{parse_code, BasicOperator, Expr, Variable};
use crate::parser_functions::parse_functions;
use crate::preprocess::preprocess;
use crate::string::string_ops;
use crate::util::{error, get_printable_form};
use branches::unlikely;
use const_currying::const_currying;
use inflector::Inflector;
use std::fs::remove_dir_all;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::time::Instant;
use std::{fs, io};
use branches::likely;
use smol_str::{SmolStr, StrExt, ToSmolStr};
use unroll::unroll_for_loops;

#[const_currying]
fn builtin_functions(
    x: &str,
    #[maybe_const(dispatch = args, consts = [[Parser:Expr; 0]])] args: &Vec<Expr>,
) -> (Expr, bool) {
    if x == "print" {
        assert_args_number!("print", args.len(), 1);
        if let Expr::String(str) = &args[0] {
            println!("{}", str);
        } else {
            println!("{}", get_printable_form(&args[0]));
        }

        (Expr::Null, true)
    } else if x == "abs" {
        assert_args_number!("abs", args.len(), 1);
        match &args[0] {
            Expr::Float(val) => return (Expr::Float(val.abs()), true),
            Expr::Integer(val) => return (Expr::Integer(val.abs()), true),
            _ => error(
                &format!("Cannot get absolute value of {:?} type", &args[0]),
                "Change type",
            ),
        }
        (Expr::Null, true)
    } else if x == "round" {
        assert_args_number!("round", args.len(), 1);
        match &args[0] {
            Expr::Float(val) => return (Expr::Integer(val.round() as i64), true),
            Expr::Integer(val) => return (Expr::Integer(*val), true),
            _ => error(
                &format!("Cannot round {} type", get_printable_type!(&args[0])),
                "Change type",
            ),
        }
        (Expr::Null, true)
    } else if x == "len" {
        assert_args_number!("len", args.len(), 1);
        match &args[0] {
            Expr::String(val) => {
                return (Expr::Integer(val.len() as i64), true);
            }
            Expr::Array(val) => {
                return (Expr::Integer(val.len() as i64), true);
            }
            _ => error(
                &format!(
                    "Cannot get length of type {}",
                    get_printable_type!(&args[0])
                ),
                "Change type",
            ),
        }
        (Expr::Null, true)
    } else if x == "input" {
        assert_args_number!("input", args.len(), 0, 1);
        if args.len() == 1 {
            if_let!(likely, Expr::String(prompt), &args[0], {
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
            Expr::String(
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
    } else if x == "type" {
        assert_args_number!("type", args.len(), 1);
        return (Expr::String(get_printable_type!(&args[0]).into()), true);
    } else if x == "hash" {
        assert_args_number!("hash", args.len(), 1);
        (
            Expr::String(
                blake3::hash(
                    bincode::serialize(&args[0])
                        .expect(error_msg!(format!(
                            "Failed to compute hash of object {:?}",
                            &args[0]
                        )))
                        .as_ref(),
                ).to_smolstr(),
            ),
            true,
        )
    } else if x == "sqrt" {
        assert_args_number!("sqrt", args.len(), 1);
        if let Expr::Integer(int) = args[0] {
            return (Expr::Float((int as f64).sqrt()), true);
        } else if let Expr::Float(float) = args[0] {
            return (Expr::Float(float.sqrt()), true);
        } else {
            error(
                format!("Cannot calculate the square root of {:?}", args[0]).as_str(),
                "",
            );
            (Expr::Null, false)
        }
    } else if x == "the_answer" {
        println!("42, the answer to the Ultimate Question of Life, the Universe, and Everything.");
        (Expr::Integer(42), true)
    } else if x == "range" {
        assert_args_number!("sqrt", args.len(), 1, 3);
        if args.len() == 1 {
            if_let!(likely, Expr::Integer(lim), args[0], {
                (Expr::Array((0..lim).map(Expr::Integer).collect()), true)
            }, else {
                error("Invalid range limit", "");
                (Expr::Null, false)
            })
        } else if args.len() == 2 {
            if_let!(likely, Expr::Integer(lim), args[0], {
                if_let!(Expr::Integer(upplim), args[1], {
                    (
                        Expr::Array((lim..upplim).map(Expr::Integer).collect()),
                        true,
                    )
                }, else {
                    error("Invalid range limit", "");
                    (Expr::Null, false)
                })
            }, else {
                error("Invalid range start", "");
                (Expr::Null, false)
            })
        } else if args.len() == 3 {
            if_let!(likely, Expr::Integer(start), args[0], {
                if_let!(Expr::Integer(stop), args[1], {
                    if_let!(Expr::Integer(step), args[2], {
                        if unlikely(step == 0) {
                            error("Step cannot be zero", "");
                            (Expr::Null, false)
                        } else {
                            let range = if step > 0 {
                                (start..stop).step_by(step as usize)
                            } else {
                                (stop..start).step_by((-step) as usize)
                            };
                            (Expr::Array(range.map(Expr::Integer).collect()), true)
                        }
                    }, else {
                        error("Invalid range step", "");
                        (Expr::Null, false)
                    })
                }, else {
                    error("Invalid range limit", "");
                    (Expr::Null, false)
                })
            }, else {
                error("Invalid range start", "");
                (Expr::Null, false)
            })
        } else {
            error("Invalid range arguments", "");
            (Expr::Null, false)
        }
    } else {
        (Expr::Null, false)
    }
}


#[unroll_for_loops]
fn process_stack(
    stack_in: &Vec<Expr>,
    variables: &Vec<Variable>,
    functions: &Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Expr>>)>,
) -> Expr {
    let mut output: Expr = Expr::Null;
    let mut current_operator: BasicOperator = BasicOperator::Null;
    for p_element in stack_in {
        let mut element: &Expr = p_element;
        let mut val: Expr = Expr::Null;
        if let Expr::VariableIdentifier(ref var) = element {
            let variable = &variables
                .iter()
                .find(|x| x.name.eq(var))
                .expect(&error_msg!(format!("Unknown variable '{}'", var)));
            element = &variable.value
        } else {
            val = preprocess(variables, functions, element);
            if val != Expr::Null {
                element = &val;
            }
        }

        if output != Expr::Null {
            match element {
                Expr::Operation(op) => {
                    current_operator = *op;
                }
                Expr::OR(ref x) => {
                    let parsed_exp = process_stack(&x, variables, functions);
                    if_let!(
                        likely,
                        Expr::Bool(inbool),
                        output,
                        {
                            if_let!(likely, Expr::Bool(sidebool), parsed_exp, {
                                output = Expr::Bool(inbool || sidebool)
                            }, else {
                                error(format!("{:?} is not a Boolean", parsed_exp).as_str(), "");
                            });
                        }, else
                        {
                            error(format!("{:?} is not a Boolean", output).as_str(), "");
                        }
                    );
                }
                Expr::AND(ref x) => {
                    let parsed_exp = process_stack(&x, variables, functions);
                    if_let!(
                        likely,
                        Expr::Bool(inbool),
                        output,
                        {
                            if_let!(likely, Expr::Bool(sidebool), parsed_exp, {
                                output = Expr::Bool(inbool && sidebool)
                            }, else {
                                error(format!("{:?} is not a Boolean", parsed_exp).as_str(), "");
                            });
                        }, else
                        {
                            error(format!("{:?} is not a Boolean", output).as_str(), "");
                        }
                    )
                }
                Expr::String(x) => {
                    output = string_ops(x.to_smolstr(), output, current_operator);
                }
                Expr::Float(x) => {
                    output = float_ops(*x, output, current_operator);
                }
                Expr::Integer(x) => {
                    output = integer_ops(*x, output, current_operator);
                }
                Expr::Array(x) => output = array_ops(x.to_owned(), output, current_operator),
                Expr::Null => {
                    if output == Expr::Null {
                        match current_operator {
                            BasicOperator::Equal => output = Expr::Bool(true),
                            BasicOperator::NotEqual => output = Expr::Bool(false),
                            _ => error(
                                &format!(
                                    "Cannot perform operation '{:?}' between Null and Null",
                                    current_operator
                                ),
                                "",
                            ),
                        }
                    }
                }
                Expr::Property(x) => {
                    // TODO
                    todo!("Properties aren't implented yet!")
                }
                Expr::PropertyFunction(ref x, ref y) => {
                    let args: Vec<Expr> = y
                        .iter()
                        .map(|arg| process_stack(&arg, &variables, &functions))
                        .collect();

                    if let Expr::String(ref str) = &output {
                        string_props!(str, args, x, output);
                        // str.to_smolstr()
                    } else if let Expr::Float(num) = output {
                        float_props!(num, args, x, output);
                    } else if let Expr::Integer(num) = output {
                        integer_props!(num, args, x, output);
                    } else if let Expr::Array(ref arr) = output {
                        array_props!(arr, args, x, output);
                    } else if let Expr::File(ref filepath) = &output {
                        file_props!(filepath, args, x, output);
                    }
                }
                _ => todo!(),
            }
        } else {
            output = element.to_owned();
        }
    }
    output
}

#[unroll_for_loops]
#[const_currying]
fn process_function(
    lines: &Vec<Vec<Expr>>,
    variables: &mut Vec<Variable>,
    expected_variables_len: usize,
    name: &str,
    #[maybe_const(dispatch = args, consts = [[Parser:Expr; 0]])] functions: &Vec<(
        SmolStr,
        Vec<SmolStr>,
        Vec<Vec<Expr>>,
    )>,
) -> (Expr, Vec<Variable>) {
    if unlikely(variables.len() != expected_variables_len) {
        error(
            &format!(
                "Careful! Function '{}' expected {} arguments, but received {}",
                name,
                expected_variables_len,
                variables.len()
            ),
            "Remove the excess arguments",
        )
    }
    let mut return_variables: Vec<Variable> = vec![];

    for instructions in lines {
        for instruction in instructions {
            match instruction {
                Expr::VariableDeclaration(x, y) => variables.push(Variable {
                    name: x.to_smolstr(),
                    value: process_stack(&y, &variables, &functions),
                }),
                Expr::VariableRedeclaration(x, y) => {
                    let processed = process_stack(&y, &variables, &functions);
                    if let Some(position) = variables.iter().position(|var| var.name == *x) {
                        // If the variable exists, update it directly
                        variables[position].value = processed;
                    } else {
                        return_variables.push(Variable {
                            name: x.to_smolstr(),
                            value: processed,
                        });
                    }
                }
                Expr::NamespaceFunctionCall(z, x, y) => {
                    let args: Vec<Expr> = y
                        .iter()
                        .map(|arg| process_stack(&arg, &variables, &functions))
                        .collect();
                    if unlikely(!namespace_functions(&z, &x, &args).1) {
                        error(
                            &format!("Unknown function '{}'", z.join(".") + "." + &x),
                            "",
                        );
                    };
                }
                Expr::FunctionCall(x, y) => {
                    // println!("{:?}", y);
                    let args: Vec<Expr> = y
                        .iter()
                        .map(|arg| process_stack(arg, variables, functions))
                        .collect();

                    let matched = builtin_functions(&x, &args);
                    if x == "executeline" && !matched.1 {
                        assert_args_number!("executeline", args.len(), 1);
                        if_let!(likely, Expr::String(line), &args[0], {
                            process_stack(&parse_code(line)[0], &variables, &functions);
                            continue;
                        }, else {
                            error(&format!("Cannot execute line {:?}", &args[0]), "")
                        })
                    } else if !matched.1 {
                        let target_function: &(SmolStr, Vec<SmolStr>, Vec<Vec<Expr>>) = functions
                            .into_iter()
                            .filter(|func| func.0 == *x)
                            .next()
                            .expect(error_msg!(&format!("Unknown function '{}'", x)));
                        assert_args_number!(&x, args.len(), target_function.1.len());
                        let mut target_args: Vec<Variable> = target_function
                            .1
                            .iter()
                            .enumerate()
                            .map(|(i, arg)| Variable {
                                name: arg.to_smolstr(),
                                value: args[i].clone(),
                            })
                            .collect();
                        let len = target_args.len();
                        process_function(
                            &target_function.2,
                            &mut target_args,
                            len,
                            &target_function.0,
                            functions,
                        );
                        // println!("{:?}", target_args)
                    }
                }
                Expr::FunctionReturn(x) => {
                    return (process_stack(x, &variables, functions), return_variables);
                }
                Expr::Condition(x, y, z) => {
                    if process_stack(x, &variables, functions) == Expr::Bool(true) {
                        let out = process_function(y, variables, variables.len(), name, functions);
                        if Expr::Null != out.0 {
                            return out;
                        }
                    } else {
                        for else_block in z {
                            if else_block.0.len() == 0 {
                                let out = process_function(
                                    &else_block.1,
                                    variables,
                                    variables.len(),
                                    name,
                                    functions,
                                );
                                if Expr::Null == out.0 {
                                    if out.1 != vec![] {
                                        for replace_var in out.1 {
                                            let indx = variables
                                                .iter()
                                                .position(|var| var.name == replace_var.name)
                                                .unwrap();
                                            variables[indx] = replace_var;
                                        }
                                    }
                                    break;
                                } else {
                                    return out;
                                }
                            }
                            if process_stack(&else_block.0, &variables, &functions)
                                == Expr::Bool(true)
                            {
                                let out = process_function(
                                    &else_block.1,
                                    variables,
                                    variables.len(),
                                    name,
                                    functions,
                                );
                                if Expr::Null == out.0 {
                                    if out.1 != vec![] {
                                        for replace_var in out.1 {
                                            let indx = variables
                                                .iter()
                                                .position(|var| var.name == replace_var.name)
                                                .unwrap();
                                            variables[indx] = replace_var;
                                        }
                                    }
                                    break;
                                } else {
                                    return out;
                                }
                            }
                        }
                    }
                }
                Expr::Loop(x, y, z) => {
                    let loop_array = process_stack(&y, &variables, &functions);
                    if let Expr::Array(target_array) = loop_array {

                        let mut builtin_vars = [
                            &variables[..],
                            &vec![Variable {
                                name: x.to_owned(),
                                value: Expr::Null,
                            }][..],
                        ]
                            .concat();
                        let position = builtin_vars.len()-1;
                        let len = builtin_vars.len();

                        for elem in target_array {
                            builtin_vars[position].value = elem;

                            let (out_expr, out_replacements) = process_function(z, &mut builtin_vars, len, name, functions);
                            if out_expr != Expr::Null {
                                return (out_expr, out_replacements);
                            }
                            for replace_var in out_replacements {
                                let indx = variables
                                    .iter()
                                    .position(|var| var.name == replace_var.name)
                                    .unwrap();
                                variables[indx] = replace_var.to_owned();
                                builtin_vars[indx] = replace_var;
                            }
                        }
                    } else if let Expr::String(ref target_string) = loop_array {
                        let mut builtin_vars = [
                            &variables[..],
                            &vec![Variable {
                                name: x.to_smolstr(),
                                value: Expr::Null,
                            }][..],
                        ]
                            .concat();
                        let position = builtin_vars.len()-1;
                        let len = builtin_vars.len();
                        for elem in target_string.chars() {
                            builtin_vars[position].value = Expr::String(elem.to_smolstr());
                            let (out_expr, out_replacements) = process_function(z, &mut builtin_vars, len, name, functions);
                            if out_expr != Expr::Null {
                                return (out_expr, out_replacements);
                            }
                            for replace_var in out_replacements {
                                let indx = variables
                                    .iter()
                                    .position(|var| var.name == replace_var.name)
                                    .unwrap();
                                variables[indx] = replace_var.to_owned();
                                builtin_vars[indx] = replace_var;
                            }
                        }
                    }
                }
                Expr::While(x, y) => {
                    while process_stack(x, &variables, functions) == Expr::Bool(true) {
                        let out = process_function(y, variables, variables.len(), name, functions);
                        if Expr::Null != out.0 {
                            return out;
                        }
                        if out.1 != vec![] {
                            for replace_var in out.1 {
                                let indx = variables
                                    .iter()
                                    .position(|var| var.name == replace_var.name)
                                    .unwrap();
                                variables[indx] = replace_var;
                            }
                        }
                    }
                }
                _ => {
                    process_stack(&instructions, &variables, &functions);
                    break;
                }
            }
            // println!("{:?}", instruction)
        }
        // println!("{:?}", instructions)
    }
    (Expr::Null, return_variables)
    // println!("{:?}", variables)
}

fn main() {
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.len() == 0 {
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
            .expect(error_msg!("Failed to delete the cache folder (.compute)"));
    }
    let arg = args.first().unwrap();

    let content =
        fs::read_to_string(arg).expect(error_msg!(format!("Unable to read file '{}'", arg)));

    let now = Instant::now();
    let functions: Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Expr>>)> =
        parse_functions(content.trim(), true);
    log!("PARSED IN: {:.2?}", now.elapsed());
    log!("FUNCTIONS {:?}", functions);

    let main_instructions = functions
        .clone()
        .into_iter()
        .filter(|function| function.0 == "main")
        .collect::<Vec<(SmolStr, Vec<SmolStr>, Vec<Vec<Expr>>)>>();

    let now = Instant::now();
    // thread::Builder::new()
    //     // 16MB stack size
    //     .stack_size(16 * 1024 * 1024)
    //     .spawn(move || {
    //         process_function(&main_instructions[0].2, &mut vec![], 0, "main", &functions);
    //     })
    //     .unwrap()
    //     .join()
    //     .unwrap();
    process_function(&main_instructions[0].2, &mut vec![], 0, "main", &functions);
    println!("EXECUTED IN: {:.2?}", now.elapsed());
    println!("TOTAL: {:.2?}", totaltime.elapsed());
}
