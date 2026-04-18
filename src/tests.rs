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

#[test]
pub fn string_split_len() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "hello world";
            let parts = s.split(" ");
            print(parts.len());
        }
        "#,
        2.into()
    );
}

#[test]
pub fn string_contains_replace() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "hello world";
            print(s.contains("world"));
        }
        "#,
        true.into()
    );
}

#[test]
pub fn for_loop_sum() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [1, 2, 3, 4, 5];
            let sum = 0;
            for x in arr {
                sum += x;
            }
            print(sum);
        }
        ",
        15.into()
    );
}

#[test]
pub fn array_sort() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [3, 1, 4, 1, 5, 9, 2, 6];
            arr.sort();
            print(arr[0]);
        }
        ",
        1.into()
    );
}

#[test]
pub fn array_push_len() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [1, 2, 3];
            arr.push(4);
            print(arr.len());
        }
        ",
        4.into()
    );
}

#[test]
pub fn int_for_loop() {
    run_and_check_registers!(
        "
        function main() {
            let sum = 0;
            for i in 0..10 {
                sum += i;
            }
            print(sum);
        }
        ",
        45.into()
    );
}

#[test]
pub fn string_trim() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "  hello  ";
            let t = s.trim();
            print(t.len());
        }
        "#,
        5.into()
    );
}

#[test]
pub fn recursive_factorial() {
    run_and_check_registers!(
        "
        function fact(n) {
            if n <= 1 { return 1; }
            else { return n * fact(n - 1); }
        }
        function main() {
            print(fact(10));
        }
        ",
        3628800.into()
    );
}

#[test]
pub fn inline_condition_basic() {
    run_and_check_registers!(
        "
        function main() {
            let x = 10;
            let result = if x > 5 { 1 } else { 0 };
            print(result);
        }
        ",
        1.into()
    );
}

#[test]
pub fn inline_condition_else_branch() {
    run_and_check_registers!(
        "
        function main() {
            let x = 3;
            let result = if x > 5 { 1 } else { 0 };
            print(result);
        }
        ",
        0.into()
    );
}

#[test]
pub fn inline_condition_else_if() {
    run_and_check_registers!(
        "
        function main() {
            let x = 5;
            let result = if x > 10 { 2 } else if x > 3 { 1 } else { 0 };
            print(result);
        }
        ",
        1.into()
    );
}

#[test]
pub fn inline_condition_as_arg() {
    run_and_check_registers!(
        "
        function main() {
            let x = 42;
            print(if x == 42 { 99 } else { 0 });
        }
        ",
        99.into()
    );
}
