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
// use smol_str::{SmolStr, StrExt as _, ToSmolStr as _};
// use smartstring::alias::String;
use gxhash::HashMapExt;
use snmalloc_rs::SnMalloc;
use std::fs;
use std::io::Write as _;
use std::ops::Deref;
use std::time::Instant;
use unroll::unroll_for_loops;

#[global_allocator]
static ALLOC: SnMalloc = SnMalloc;

#[unroll_for_loops]
// #[inline(always)]
fn process_stack(
    stack_in: &[Types],
    variables: &Vec<(String, Types)>,
    functions: &[(String, &[String], &[Types])],
) -> Types {
    let mut output: Types = match stack_in.first().unwrap_or(&Types::Integer(0)) {
        Types::VariableIdentifier(ref var) => variables
            .iter()
            .find(|(name, _)| name == var)
            .unwrap_or_else(|| {
                error(&format!("Unknown variable '{var}'"), "");
                std::process::exit(1)
            })
            .1
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
                preprocess(other, variables, functions)
            }
        }
    };
    let mut current_operator: BasicOperator = BasicOperator::Null;
    for p_element in stack_in.iter().skip(1) {
        let element: &Types = match p_element {
            // Types::VariableIdentifier(ref var) => variables.get(var).unwrap_or_else(|| {
            //     error(&format!("Unknown variable '{var}'"), "");
            //     std::process::exit(1)
            // }),
            Types::VariableIdentifier(ref var) => {
                &variables
                    .iter()
                    .find(|(name, _)| name == var)
                    .unwrap_or_else(|| {
                        error(&format!("Unknown variable '{var}'"), "");
                        std::process::exit(1)
                    })
                    .1
            }
            Types::Wrap(ref x) => &process_stack(x, variables, functions),
            other => {
                if !matches!(
                    other,
                    Types::FunctionCall(_)
                        | Types::NamespaceFunctionCall(_)
                        | Types::Priority(_)
                        | Types::Array(_, true, false)
                        | Types::Array(_, false, true)
                ) {
                    other
                } else {
                    &preprocess(other, variables, functions)
                }
            }
        };

        match element {
            Types::Operation(ref op) => current_operator = *op,
            Types::Integer(ref x) => {
                output = integer_ops(*x, output, current_operator);
            }
            Types::String(ref x) => output = string_ops(x, output, current_operator),
            Types::Float(ref x) => output = float_ops(*x, output, current_operator),
            Types::Array(ref x, _, false) => output = array_ops(x, output, current_operator),
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
                    Types::File(ref filepath) => {
                        file_props!(filepath.to_string(), args, block.name, output)
                    }
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

// #[inline(always)]
fn process_lines(
    line_array: &[Types],
    variables: &mut Vec<(String, Types)>,
    functions: &[(String, &[String], &[Types])],
) -> Types {
    for line in line_array {
        match line {
            Types::VariableDeclaration(ref block) => {
                if !block.is_declared {
                    variables.push((
                        block.name.parse().unwrap(),
                        process_stack(&block.value, variables, functions),
                    ));
                } else {
                    let calculated = process_stack(&block.value, variables, functions);
                    // if let Some(x) = variables.get_mut((&block.name, _)) {
                    //     *x = calculated;
                    // }
                    // variables.fin
                    if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.name) {
                        x.1 = calculated;
                    }
                }
            }
            Types::NamespaceFunctionCall(ref block) => {
                let args: Vec<Types> = util::split_vec_box(&block.args, Types::Separator)
                    .iter()
                    .map(|w| process_stack(w, variables, functions))
                    .collect();
                if unlikely(!namespace_functions(&block.namespace, &block.name, &args).1) {
                    error(
                        &format!(
                            "Unknown function '{}'",
                            block.namespace.join(".") + "." + block.name.as_str()
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
                    let target_function: &(String, &[String], &[Types]) = functions
                        .iter()
                        .find(|func| func.0 == *block.name)
                        .unwrap_or_else(|| {
                            error(&format!("Unknown function '{}'", block.name), "");
                            std::process::exit(1)
                        });
                    assert_args_number!(block.name, args.len(), target_function.1.len());
                    let mut target_args: Vec<(String, Types)> = target_function
                        .1
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| (arg.parse().unwrap(), args[i].clone()))
                        .collect();

                    process_lines(target_function.2, &mut target_args, functions);
                }
            }
            Types::PropertyFunction(ref block) => {
                return process_stack(
                    &[
                        process_lines(
                            &[Types::FunctionCall(Box::from(FunctionPropertyCallBlock {
                                name: block.func1_name.parse().unwrap(),
                                args: block.func1_args.clone(),
                            }))],
                            variables,
                            functions,
                        ),
                        Types::Property(Box::from(FunctionPropertyCallBlock {
                            name: block.func2_name.parse().unwrap(),
                            args: block.func2_args.clone(),
                        })),
                    ],
                    variables,
                    functions,
                );
            }
            Types::Condition(ref block) => {
                // let data = block;
                if process_stack(&block.condition, variables, functions) == Types::Bool(true) {
                    let out = process_lines(&block.code, variables, functions);
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
                            let out = process_lines(&else_block.1, variables, functions);
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
                    variables.push((block.id.parse().unwrap(), Types::Null));
                    for elem in target_array {
                        // if let Some(value) = variables.get_mut(&block.id) {
                        //     *value = elem;
                        // }
                        if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.id) {
                            x.1 = elem;
                        }

                        let out: Types = process_lines(&block.code, variables, functions);
                        if out != Types::Null {
                            variables.swap_remove(
                                variables
                                    .iter()
                                    .position(|(name, _)| *name == block.id)
                                    .unwrap(),
                            );
                            // variables.remove(&block.id);
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    // variables.remove(&block.id);
                    variables.swap_remove(
                        variables
                            .iter()
                            .position(|(name, _)| *name == block.id)
                            .unwrap(),
                    );
                } else if let Types::String(ref target_string) = loop_array {
                    variables.push((block.id.parse().unwrap(), Types::Null));
                    for elem in target_string.chars() {
                        // if let Some(value) = variables.get_mut(&block.id) {
                        //     *value = Types::String(elem.to_smolstr());
                        // }
                        if let Some(x) = variables.iter_mut().find(|(name, _)| *name == block.id) {
                            x.1 = Types::String(elem.to_string().parse().unwrap());
                        }

                        let out: Types = process_lines(&block.code, variables, functions);
                        if out != Types::Null {
                            // variables.remove(&block.id);
                            variables.swap_remove(
                                variables
                                    .iter()
                                    .position(|(name, _)| *name == block.id)
                                    .unwrap(),
                            );
                            if out == Types::Break {
                                break;
                            }
                            return out;
                        }
                    }
                    // variables.remove(&block.id);
                    variables.swap_remove(
                        variables
                            .iter()
                            .position(|(name, _)| *name == block.id)
                            .unwrap(),
                    );
                }
            }
            Types::While(ref block) => {
                while let Types::Bool(true) = process_stack(&block.condition, variables, functions)
                {
                    // let now = Instant::now();
                    let out = process_lines(&block.code, variables, functions);
                    // println!("WHILE ITERATION {:.2?}", now.elapsed());
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
            Types::Wrap(ref x) => {
                let x = process_lines(x, variables, functions);
                if x != Types::Null {
                    return x;
                }
            }
            _ => error(&format!("TODO LINE LOGIC {line:?}"), ""),
        }
    }
    Types::Null
}

// pub fn store_instr(inp: Vec<Instructions>) -> Vec<Instructions> {
//     let mut new = vec![];
//     for x in inp {
//         if matches!(x, )
//     }
// }

fn simplify(lines: Vec<Types>, store: bool, current_num: usize) -> (Vec<Types>, usize) {
    let mut test: Vec<Types> = vec![];
    let mut i: usize = current_num + 1;
    // let mut instr_id: usize = 0;
    for x in lines {
        // instr_id += 1;
        match x {
            Types::VariableDeclaration(block) => {
                let x = block.name;
                let y = block.value;
                if block.is_declared {
                    let result = simplify(Vec::from(y), true, i);
                    i = result.1 + 1;
                    test.extend(result.0);
                    test.push(Types::VarReplace(x, result.1));
                } else {
                    let result = simplify(Vec::from(y), true, i);
                    i = result.1 + 1;
                    test.extend(result.0);
                    test.push(Types::VarStore(x, result.1));
                }
            }
            Types::FunctionCall(block) => {
                let name = block.name;
                let y = block.args;
                let mut args: Vec<usize> = vec![];
                for x in y {
                    if let Types::Wrap(w) = x {
                        let result = simplify(Vec::from(w), true, i);
                        i = result.1 + 1;
                        test.extend(result.0);
                        args.push(result.1);
                    } else {
                        let result = simplify(vec![x], true, i);
                        i = result.1 + 1;
                        test.extend(result.0);
                        args.push(result.1);
                    }
                }
                test.push(Types::FuncCall(name, args));
            }
            Types::FunctionReturn(ret) => {
                let result = simplify(Vec::from(ret), true, i);
                i = result.1 + 1;
                test.extend(result.0);
                test.push(Types::FuncReturn(result.1));
            }
            Types::Condition(block) => {
                let condition = simplify(Vec::from(block.condition), true, i);
                i = condition.1 + 1;
                let in_code = simplify(Vec::from(block.code), false, i);
                i = in_code.1 + 1;
                let added = in_code.0.len();
                test.extend(condition.0);
                test.push(Types::IF(condition.1, added));
                test.extend(in_code.0);

                // if block.else_blocks.len() > 0 {
                //     for else_block in block.else_blocks {
                //         let condition = simplify(Vec::from(else_block.0), true, i);
                //         i = condition.1 + 1;
                //         let in_code = simplify(Vec::from(else_block.1), false, i);
                //         i = in_code.1 + 1;
                //         let added = in_code.0.len();
                //     }
                // }
            }
            Types::While(block) => {
                let condition = simplify(Vec::from(block.condition), true, i);
                i = condition.1 + 1;
                let in_code = simplify(Vec::from(block.code), false, i);
                i = in_code.1 + 1;
                let added = in_code.0.len();
                let sum = condition.0.len() + 1 + added;
                test.extend(condition.0);
                test.push(Types::IF(condition.1, added + 1));
                test.extend(in_code.0);
                test.push(Types::JUMP(-(sum as i64)))
            }
            _ => test.push(x),
        }
    }
    if store {
        test.insert(0, Types::STARTSTORE(i + 1));
        test.push(Types::STOP)
        // test.push(Types::STOP(i + 1))
    }
    (test, i + 1)
}

macro_rules! check_first_to_register {
    ($elem: expr, $depth: expr, $i: expr, $register: expr) => {
        if !$register.iter().any(|(x, _)| x == $depth.last().unwrap()) {
            $register.push((*$depth.last().unwrap(), $elem));
            $i += 1;
            continue;
        }
    };
}

fn execute(lines: &mut Vec<Types>) {
    let mut register: Vec<(usize, Types)> = Vec::new();

    let mut depth: Vec<usize> = Vec::new();

    let mut operator: Vec<(usize, BasicOperator)> = Vec::new();

    let mut variables: Vec<(String, Types)> = Vec::new();

    let mut i: usize = 0;
    while i < lines.len() {
        // if !matches!(
        //     &lines[i],
        //     Types::FunctionCall(_)
        //         | Types::NamespaceFunctionCall(_)
        //         | Types::Priority(_)
        //         | Types::Array(_, _, _)
        //         | Types::STARTSTORE(_)
        //         | Types::STOP
        //         | Types::Operation(_)
        // ) && !register.iter().any(|(x, _)| x == depth.last().unwrap())
        // {
        //     register.push((*depth.last().unwrap(), lines[i].clone()));
        //     i += 1;
        //     continue;
        // }
        if let Types::VariableIdentifier(id) = &lines[i] {
            lines[i] = variables.iter().find(|(x, _)| x == id).unwrap().1.clone();
        }
        match lines[i] {
            Types::STARTSTORE(num) => depth.push(num),
            Types::STOP => {
                depth.pop();
            }
            Types::Operation(op) => {
                if depth.len() > 0 {
                    operator.push((*depth.last().unwrap(), op))
                }
            }
            Types::VarStore(ref str, id) => variables.push((
                str.to_string(),
                register
                    .swap_remove(register.iter().position(|(x, _)| x.eq(&id)).unwrap())
                    .1,
            )),
            // Types::VarReplace(ref str, id) => variables.push((
            //     str.clone(),
            //     register
            //         .swap_remove(register.iter().position(|(x, _)| x.eq(&id)).unwrap())
            //         .1,
            // )),
            Types::IF(condition_id, jump_size) => {
                let condition = register
                    .remove(
                        register
                            .iter()
                            .position(|(id, _)| *id == condition_id)
                            .unwrap(),
                    )
                    .1;
                if !matches!(condition, Types::Bool(_)) {
                    error(
                        &format!("'{}' is not a boolean", get_printable_form(&condition)),
                        "",
                    );
                } else if condition == Types::Bool(false) {
                    i += jump_size;
                }
            }

            Types::FuncCall(ref name, ref args) => {
                if name == "print" {
                    println!(
                        "{}",
                        get_printable_form(
                            &register
                                .swap_remove(
                                    register.iter().position(|(id, _)| *id == args[0]).unwrap()
                                )
                                .1
                        )
                    )
                } else {
                }
            }

            // PRIMITIVE TYPES
            Types::Integer(int) => {
                check_first_to_register!(Types::Integer(int), depth, i, register);
                if let Some(elem) = register
                    .iter_mut()
                    .find(|(id, _)| id == depth.last().unwrap())
                {
                    match elem.1 {
                        Types::Integer(parent) => {
                            match operator
                                .swap_remove(
                                    operator
                                        .iter()
                                        .position(|(x, _)| x == depth.last().unwrap())
                                        .unwrap(),
                                )
                                .1
                            {
                                BasicOperator::Add => {
                                    *elem = (elem.0, Types::Integer(parent + int))
                                }
                                BasicOperator::Inferior => {
                                    *elem = (elem.0, Types::Bool(parent < int))
                                }
                                _ => {}
                            }
                        }
                        _ => todo!(),
                    }
                }
            }
            Types::String(ref str) => {
                check_first_to_register!(Types::String(str.to_string()), depth, i, register)
            }
            _ => {}
        }
        i += 1;

        println!("---\nREGISTER {register:?}\nVARIABLES {variables:?}")
    }
}

fn main() {
    dbg!(std::mem::size_of::<Types>());
    // dbg!(std::mem::size_of::<BasicOperator>());
    // dbg!(std::mem::size_of::<Types>());
    let totaltime = Instant::now();
    let args: Vec<String> = std::env::args()
        .skip(1)
        .map(|x| x.parse::<String>().unwrap())
        .collect();
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
    let mut main_function: (String, &[String], Vec<Types>) = Default::default();

    let partial_convert: Vec<(String, &[String], Vec<Types>)> = temp_funcs
        .iter()
        .map(|(name, args, lines)| {
            let stack: Vec<Types> = lines
                .iter()
                .flat_map(|line| line.iter().map(|x| (*x).clone()))
                .collect();
            if name == "main" {
                main_function = (name.to_string().parse().unwrap(), args, stack);
                return (name.clone(), args.as_slice(), vec![]);
            }
            return (name.clone(), args.as_slice(), stack);
        })
        .filter(|(name, _, _)| *name != "main")
        .collect();

    let converted: Vec<(String, &[String], &[Types])> = partial_convert
        .iter()
        .map(|(name, args, lines)| (name.parse().unwrap(), *args, lines.as_slice()))
        .collect();

    let functions: &[(String, &[String], &[Types])] = converted.as_slice();

    log!("PARSED IN: {:.2?}", now.elapsed());
    log!(
        "MAIN\n{}",
        main_function
            .2
            .iter()
            .map(|x| format!("{x:?}").parse().unwrap())
            .collect::<Vec<String>>()
            .join("\n")
    );

    let now = Instant::now();

    // let mut vars: HashMap<SmolStr, Types> = HashMap::default();
    // process_lines(&main_function.2, &mut vec![], functions);
    let mut code = simplify(main_function.2, false, 0).0;
    println!(
        "{}",
        &code
            .iter()
            .map(|x| format!("{:?}", x))
            .collect::<Vec<String>>()
            .join("\n")
    );
    execute(&mut code);

    log!("EXECUTED IN: {:.2?}", now.elapsed());
    log_release!("TOTAL: {:.2?}", totaltime.elapsed());
}
