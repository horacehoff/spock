#[macro_export]
macro_rules! array_props {
    ($arr: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "len" => {
                assert_args_number!("len", $args.len(), 0);
                $output = Expr::Integer($arr.len() as i64)
            }
            "add" => {
                assert_args_number!("add", $args.len(), 1);
                let mut new_vec = $arr.clone();
                new_vec.push($args[0].clone());
                $output = Expr::Array(new_vec);
            }
            "remove" => {
                assert_args_number!("add", $args.len(), 1);
                let mut new_vec = $arr.clone();
                let index = new_vec.iter().position(|x| *x == $args[0]).unwrap();
                new_vec.remove(index);
                $output = Expr::Array(new_vec);
            }
            "clear" => {
                assert_args_number!("clear", $args.len(), 0);
                $output = Expr::Array(Box::from(vec![]));
            }
            "reverse" => {
                assert_args_number!("clear", $args.len(), 0);
                let mut new_vec = $arr.clone();
                new_vec.reverse();
                $output = Expr::Array(Box::from(new_vec))
            }
            "sort" => {
                assert_args_number!("sort", $args.len(), 0);
                let mut new_vec: Vec<Expr> = *$arr.clone();
                new_vec.sort_by(|a, b| match a {
                    Expr::Integer(x) => match b {
                        Expr::Integer(y) => x.cmp(y),
                        Expr::Float(y) => x.cmp(&(*y as i64)),
                        _ => {
                            error(format!("Cannot compare Integer with {:?}", b).as_str(), "");
                            std::cmp::Ordering::Equal
                        }
                    },
                    Expr::Float(x) => match b {
                        Expr::Integer(y) => (*x as i64).cmp(y),
                        Expr::Float(y) => x.partial_cmp(y).unwrap(),
                        _ => {
                            error(format!("Cannot compare Integer with {:?}", b).as_str(), "");
                            std::cmp::Ordering::Equal
                        }
                    },
                    _ => {
                        error(format!("Cannot sort {:?}", a).as_str(), "");
                        std::cmp::Ordering::Equal
                    }
                });
                $output = Expr::Array(Box::from(new_vec));
            }
            "index" => {
                assert_args_number!("index", $args.len(), 1);
                $output = Expr::Integer(
                    $arr.clone()
                        .iter()
                        .position(|elem| *elem == $args[0])
                        .expect(error_msg!(format!(
                            "{:?} was not found in the list",
                            $args[0]
                        ))) as i64,
                )
            }
            "extend" => {
                assert_args_number!("extend", $args.len(), 1);
                let mut new_vec: Vec<Expr> = *$arr.clone();
                if let Expr::Array(x) = $args[0].clone() {
                    new_vec.extend(*x);
                    $output = Expr::Array(Box::from(new_vec));
                } else {
                    error(format!("{:?} is not a list", $args[0]).as_str(), "");
                }
            }
            "insert" => {
                assert_args_number!("insert", $args.len(), 2);
                let mut new_vec: Vec<Expr> = *$arr.clone();
                if let Expr::Integer(x) = $args[0] {
                    new_vec.insert(x as usize, $args[1].clone());
                    $output = Expr::Array(Box::from(new_vec))
                } else {
                    error(format!("{:?} is not a valid index", $args[0]).as_str(), "");
                }
            }
            "pop" => {
                assert_args_number!("pop", $args.len(), 1);
                let mut new_vec: Vec<Expr> = *$arr.clone();
                if let Expr::Integer(x) = $args[0] {
                    new_vec.remove(x as usize);
                    $output = Expr::Array(Box::from(new_vec))
                } else {
                    error(format!("{:?} is not a valid index", $args[0]).as_str(), "");
                }
            }
            _ => {}
        }
    };
}
