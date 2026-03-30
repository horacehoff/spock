use crate::Data;
use crate::execute;
use crate::parse;

macro_rules! run_and_check_registers {
    ($contents:expr, $expected:expr) => {
        let contents = $contents;
        let filename = "test.spock";
        let (instructions, mut registers, mut arrays, instr_src, fn_registers) =
            parse(contents, filename);
        execute(
            &instructions,
            &mut registers,
            &mut arrays,
            &instr_src,
            (filename, &contents),
            &fn_registers,
        );
        assert!(registers.contains($expected));
    };
}

#[test]
pub fn rec_fib_1() {
    run_and_check_registers!(
        "
        fn fib(n) {
            if n <= 1 {return n;}
            else {return fib(n-1)+fib(n-2);}
        }

        fn main() {
            let x = fib(1);
            print(x);
        }
        ",
        &1.into()
    );
}

#[test]
pub fn rec_fib_25() {
    run_and_check_registers!(
        "
        fn fib(n) {
            if n <= 1 {return n;}
            else {return fib(n-1)+fib(n-2);}
        }

        fn main() {
            let x = fib(25);
            print(x);
        }
        ",
        &75025.into()
    );
}
