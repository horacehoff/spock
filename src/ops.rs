use crate::Opcode;
use crate::parser::Expr;

fn get_precedence(operator: &Expr) -> u8 {
    if let Expr::Opcode(op) = operator {
        match op {
            Opcode::BoolOr => 1,
            Opcode::BoolAnd => 2,
            Opcode::Eq | Opcode::NotEq => 3,
            Opcode::Inf | Opcode::InfEq | Opcode::Sup | Opcode::SupEq => 4,
            Opcode::Add | Opcode::Sub | Opcode::Neg => 5,
            Opcode::Mul | Opcode::Div | Opcode::Mod => 6,
            Opcode::Pow => 7,
        }
    } else {
        unreachable!()
    }
}

fn is_left_associative(operator: &Expr) -> bool {
    if let Expr::Opcode(op) = operator {
        !matches!(op, Opcode::Pow)
    } else {
        unreachable!()
    }
}

pub fn op_to_rpn(operation_input: Vec<Expr>) -> Vec<Expr> {
    let mut return_vector: Vec<Expr> = Vec::new();
    let mut op_stack: Vec<Expr> = Vec::new();
    for x in operation_input {
        // Num, function,...
        if !matches!(x, Expr::Opcode(_) | Expr::LPAREN | Expr::RPAREN) {
            return_vector.push(x);
        } else if matches!(x, Expr::Opcode(_)) && x != Expr::LPAREN && x != Expr::RPAREN {
            // operator
            while !op_stack.is_empty()
                && op_stack.last().unwrap() != &Expr::LPAREN
                && (get_precedence(op_stack.last().unwrap()) > get_precedence(&x)
                    || (get_precedence(op_stack.last().unwrap()) == get_precedence(&x)
                        && is_left_associative(&x)))
            {
                return_vector.push(op_stack.pop().unwrap());
            }
            op_stack.push(x);
        } else if x == Expr::LPAREN {
            op_stack.push(x);
        } else if x == Expr::RPAREN {
            while op_stack.last().unwrap() != &Expr::LPAREN {
                assert!(!op_stack.is_empty(), "MISMATCHED PARENTHESES");
                return_vector.push(op_stack.pop().unwrap());
            }
            op_stack.pop();
        }
    }
    while !op_stack.is_empty() {
        assert_ne!(
            op_stack.last().unwrap(),
            &Expr::LPAREN,
            "MISMATCHED PARENTHESES"
        );
        return_vector.push(op_stack.pop().unwrap());
    }

    return_vector
}

pub fn remove_priority(x: Expr) -> Vec<Expr> {
    match x {
        Expr::Op(w) => {
            let mut output = Vec::new();
            for x in w {
                output.extend(remove_priority(x));
            }
            output
        }
        Expr::Priority(x) => {
            let mut output: Vec<Expr> = Vec::with_capacity(3);
            output.push(Expr::LPAREN);
            output.extend(remove_priority(*x));
            output.push(Expr::RPAREN);
            output
        }
        _ => vec![x],
    }
}
