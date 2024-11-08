use crate::error_msg;
use crate::{get_printable_type};
use crate::parser::{BasicOperator, Expr};
use crate::util::error;

#[inline(always)]
pub fn string_ops(x:String, output: Expr, current_operator: BasicOperator) -> Expr {
    if let Expr::String(value) = &output {
        match current_operator {
            BasicOperator::Add => {
                Expr::String(value.to_owned() + &x)
            }
            _ => {error(&format!("Cannot perform operation '{:?}' between String and String", current_operator),""); Expr::Null},
        }
    } else if let Expr::Integer(value) = &output {
        match current_operator {
            BasicOperator::Multiply => {
                Expr::String(x.repeat(*value as usize))
            }
            _ => {error(&format!("Cannot perform operation '{:?}' between Integer and String", current_operator),""); Expr::Null},
        }
    } else {
        error(&format!("Cannot perform operation '{:?}' between {:?} and String", current_operator, get_printable_type!(output)), "");
        Expr::Null
    }
}

#[macro_export]
macro_rules! file_props {
    ($filepath: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
        "read" => {
            assert_args_number!("read", $args.len(), 0);
            let filecontent = fs::read_to_string($filepath).expect(error_msg!(format!("Failed to read {}", $filepath)));
            $output = Expr::String(filecontent)
        },
        "write" => {
            assert_args_number!("write", $args.len(), 1);
            if let Expr::String(filecontent) = $args[0].clone() {
                let mut f = fs::OpenOptions::new().write(true).truncate(true).open(&$filepath).expect(error_msg!(format!("Failed to open {}", $filepath)));
                f.write_all(filecontent.as_ref()).expect(error_msg!(format!("Failed to write {} to {}", filecontent, $filepath)));
                f.flush().unwrap();
            } else {
                error(&format!("Invalid file content: {:?}", get_printable_form($args[0].clone())),"");
            }
        },
        "append" => {
            assert_args_number!("append", $args.len(), 1);
            if let Expr::String(filecontent) = $args[0].clone() {
                let mut f = fs::OpenOptions::new().write(true).append(true).open(&$filepath).expect(error_msg!(format!("Failed to open {}", $filepath)));
                f.write_all(filecontent.as_ref()).expect(error_msg!(format!("Failed to append {} to {}", filecontent, $filepath)));
                f.flush().unwrap();

            } else {
                error(&format!("Invalid file content: {:?}", get_printable_form($args[0].clone())),"");
            }
        }
        _ => {}
    }
    };
}