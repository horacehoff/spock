use crate::Instr;
use crate::parse;

macro_rules! run_and_check_registers {
    ($contents:expr, $expected:expr) => {
        let contents = $contents;
        let filename = "test.spock";
        let (
            instructions,
            mut registers,
            mut arrays,
            instr_src,
            fn_registers,
            _,
            allocated_arg_count,
            allocated_call_depth,
            _,
        ) = parse(contents, filename, true);
        println!("{contents}");
        crate::vm::execute(
            &instructions,
            &mut registers,
            &mut arrays,
            &instr_src,
            &[(filename.into(), contents.into())],
            &fn_registers,
            &[],
            allocated_arg_count,
            allocated_call_depth,
        );
        assert!(instructions.iter().any(|x| {
            if let Instr::Print(tgt) = x {
                registers[(*tgt) as usize] == $expected
            } else {
                false
            }
        }));
    };
}

#[test]
pub fn rec_fib_1() {
    run_and_check_registers!(
        "
        function fib(n) {
            if n <= 1 {return n;}
            else {return fib(n-1)+fib(n-2);}
        }

        function main() {
            let x = fib(1);
            print(x);
        }
        ",
        1.into()
    );
}

#[test]
pub fn rec_fib_25() {
    run_and_check_registers!(
        "
        function fib(n) {
            if n <= 1 {return n;}
            else {return fib(n-1)+fib(n-2);}
        }

        function main() {
            let x = fib(25);
            print(x);
        }
        ",
        75025.into()
    );
}

#[test]
pub fn while_and_condition() {
    run_and_check_registers!(
        "
        function main() {
        let count = 0;
        let limit = 1000000;
        let result = 1;
        while count < limit {
            result *= 2;
            if result > 1000000 {
                result %= 1000000;
            }
            count += 1;
        }
        print(result);
        }
        ",
        109376.into()
    );
}

#[test]
pub fn iter_fib_40() {
    run_and_check_registers!(
        "
        function main() {
        let n = 40;
        let a=0;
        let b=1;
        let c=0;
        let i=0;
        while i < (n-1) {
           c = a+b;
           a = b;
           b = c;
           i = i+1;
        }
        print(c);
        }
        ",
        102334155.into()
    );
}
