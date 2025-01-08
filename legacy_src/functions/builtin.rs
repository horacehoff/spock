use crate::parser::Types;
use crate::{assert_args_number, error, error_msg, get_printable_form, get_printable_type, if_let};
use branches::{likely, unlikely};
use const_currying::const_currying;

#[const_currying]
pub fn builtin_functions(
    x: &str,
    #[maybe_const(dispatch = args, consts = [[Parser:Expr; 0]])] args: &[Types],
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
                Types::Array(val, _, _) => {
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
            stdout().flush().unwrap();
            return (
                Types::String(
                    BufReader::new(stdin())
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
                    .to_string()
                    .parse()
                    .unwrap(),
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
                        return (Types::Array(vec, false, false), true);
                }, else {
                    error("Invalid range limit", "");
                })
            } else if args.len() == 2 {
                if_let!(likely, Types::Integer(lim), args[0], {
                    if_let!(Types::Integer(upplim), args[1], {
                        return (
                            Types::Array((lim..upplim).map(Types::Integer).collect(), false, false),
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
                                return (Types::Array(range.map(Types::Integer).collect(), false, false), true)
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
