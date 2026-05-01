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
pub fn iter_fib_40_loop() {
    run_and_check_registers!(
        "
        function main() {
            let sum = 0;
            for _ in 0..200000 {
                let a = 0;
                let b = 1;
                let c = 0;
                for i in 0..39 {
                    c = a + b;
                    a = b;
                    b = c;
                }
                sum += (b % 10);
            }
            print(sum);
        }
        ",
        1000000.into()
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
pub fn array_partition() {
    run_and_check_registers!(
        "
        function main() {
            let x = [1,2,3,0,4,5,6];
            let p = x.partition(0);
            print(p[0][0]+p[1][2]);
        }
        ",
        7.into()
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

#[test]
pub fn float_addition() {
    run_and_check_registers!(
        "
        function main() {
            let x = 1.5 + 2.5;
            print(x);
        }
        ",
        4.0f64.into()
    );
}

#[test]
pub fn float_sqrt() {
    run_and_check_registers!(
        "
        function main() {
            let x = float(144).sqrt();
            print(x);
        }
        ",
        12.0f64.into()
    );
}

#[test]
pub fn float_floor() {
    run_and_check_registers!(
        "
        function main() {
            let x = 3.9;
            print(x.floor());
        }
        ",
        3.0f64.into()
    );
}

#[test]
pub fn float_abs() {
    run_and_check_registers!(
        "
        function main() {
            let x = -7.5;
            print(x.abs());
        }
        ",
        7.5f64.into()
    );
}

#[test]
pub fn int_to_float_conversion() {
    run_and_check_registers!(
        "
        function main() {
            let x = float(42);
            print(x);
        }
        ",
        42.0f64.into()
    );
}

#[test]
pub fn float_to_int_conversion() {
    run_and_check_registers!(
        "
        function main() {
            let x = int(3.9);
            print(x);
        }
        ",
        3.into()
    );
}

#[test]
pub fn int_to_str_conversion() {
    run_and_check_registers!(
        r#"
        function main() {
            let x = str(42);
            print(x.len());
        }
        "#,
        2.into()
    );
}

#[test]
pub fn string_starts_ends_with() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "hello world";
            let a = s.starts_with("hello");
            let b = s.ends_with("world");
            print(a && b);
        }
        "#,
        true.into()
    );
}

#[test]
pub fn string_replace() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "hello world";
            let r = s.replace("world", "spock");
            print(r.len());
        }
        "#,
        11.into()
    );
}

#[test]
pub fn string_find() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "hello world";
            print(s.find("world"));
        }
        "#,
        6.into()
    );
}

#[test]
pub fn string_repeat() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "ab";
            print(s.repeat(3).len());
        }
        "#,
        6.into()
    );
}

#[test]
pub fn array_contains() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [1, 2, 3, 4, 5];
            print(arr.contains(3));
        }
        ",
        true.into()
    );
}

#[test]
pub fn array_reverse() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [1, 2, 3];
            arr.reverse();
            print(arr[0]);
        }
        ",
        3.into()
    );
}

#[test]
pub fn array_remove() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [10, 20, 30];
            arr.remove(1);
            print(arr.len());
        }
        ",
        2.into()
    );
}

#[test]
pub fn array_join() {
    run_and_check_registers!(
        r#"
        function main() {
            let arr = ["a", "b", "c"];
            let s = arr.join(",");
            print(s.len());
        }
        "#,
        5.into()
    );
}

#[test]
pub fn array_modify_index() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [1, 2, 3];
            arr[1] = 99;
            print(arr[1]);
        }
        ",
        99.into()
    );
}

#[test]
pub fn break_loop() {
    run_and_check_registers!(
        "
        function main() {
            let x = 0;
            for i in 0..100 {
                if i == 5 { break; }
                x += 1;
            }
            print(x);
        }
        ",
        5.into()
    );
}

#[test]
pub fn continue_in_loop() {
    run_and_check_registers!(
        "
        function main() {
            let sum = 0;
            for i in 0..10 {
                if (i % 2) == 0 { continue; }
                sum += i;
            }
            print(sum);
        }
        ",
        25.into()
    );
}

#[test]
pub fn nested_loops() {
    run_and_check_registers!(
        "
        function main() {
            let count = 0;
            for i in 0..4 {
                for j in 0..4 {
                    count += 1;
                }
            }
            print(count);
        }
        ",
        16.into()
    );
}

#[test]
pub fn bool_and_operator() {
    run_and_check_registers!(
        "
        function main() {
            let x = 5;
            print(x > 3 && x < 10);
        }
        ",
        true.into()
    );
}

#[test]
pub fn bool_or_operator() {
    run_and_check_registers!(
        "
        function main() {
            let x = 15;
            print(x < 3 || x > 10);
        }
        ",
        true.into()
    );
}

#[test]
pub fn negation() {
    run_and_check_registers!(
        "
        function main() {
            let x = 5;
            print(-x);
        }
        ",
        (-5).into()
    );
}

#[test]
pub fn power_operator() {
    run_and_check_registers!(
        "
        function main() {
            let x = 2 ^ 10;
            print(x);
        }
        ",
        1024.into()
    );
}

#[test]
pub fn multi_arg_function() {
    run_and_check_registers!(
        "
        function add(a, b) { return a + b; }
        function main() {
            print(add(3, 4));
        }
        ",
        7.into()
    );
}

#[test]
pub fn function_called_after_loop() {
    run_and_check_registers!(
        "
        function double(n) { return n * 2; }
        function main() {
            let sum = 0;
            for i in 0..10 { sum += i; }
            print(double(sum));
        }
        ",
        90.into()
    );
}

#[test]
pub fn recursive_fn_inside_for_loop() {
    run_and_check_registers!(
        "
        function fib(n) {
            if n <= 1 { return n; }
            return fib(n-1) + fib(n-2);
        }
        function main() {
            let x = [0, 1, 2];
            let sum = 0;
            for i in x {
                sum += fib(i);
            }
            print(sum);
        }
        ",
        2.into()
    );
}

#[test]
pub fn recursive_fib_after_loop() {
    run_and_check_registers!(
        "
        function fib(n) {
            if n <= 1 { return n; }
            return fib(n - 1) + fib(n - 2);
        }
        function main() {
            let x = 0;
            for i in 0..100 { x += i; }
            print(fib(10));
        }
        ",
        55.into()
    );
}

#[test]
pub fn sieve_of_eratosthenes() {
    run_and_check_registers!(
        "
        function main() {
            let limit = 100000;
            let sieve = range(limit);
            sieve[0] = 0;
            sieve[1] = 0;
            let i = 2;
            while (i * i) <= limit {
                if sieve[i] != 0 {
                    let j = i * i;
                    while j < limit {
                        sieve[j] = 0;
                        j += i;
                    }
                }
                i += 1;
            }
            let count = 0;
            for x in sieve {
                if x != 0 { count += 1; }
            }
            print(count);
        }
        ",
        9592.into()
    );
}

#[test]
pub fn collatz_steps() {
    run_and_check_registers!(
        "
        function main() {
            let n = 27;
            let steps = 0;
            while n != 1 {
                if (n % 2) == 0 {
                    n /= 2;
                } else {
                    n = n * 3 + 1;
                }
                steps += 1;
            }
            print(steps);
        }
        ",
        111.into()
    );
}

#[test]
pub fn string_word_count() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "the quick brown fox jumps";
            let words = s.split(" ");
            print(words.len());
        }
        "#,
        5.into()
    );
}

#[test]
pub fn range_sum() {
    run_and_check_registers!(
        "
        function main() {
            let arr = range(101);
            let sum = 0;
            for x in arr {
                sum += x;
            }
            print(sum);
        }
        ",
        5050.into()
    );
}

#[test]
pub fn bubble_sort() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [5, 3, 8, 1, 9, 2, 7, 4, 6];
            let n = arr.len();
            for i in 0..n {
                for j in 0..(n - 1) {
                    if arr[j] > arr[j + 1] {
                        let tmp = arr[j];
                        arr[j] = arr[j + 1];
                        arr[j + 1] = tmp;
                    }
                }
            }
            print(arr[0]+arr[8]);
        }
        ",
        10.into()
    );
}

#[test]
pub fn for_loop_called_twice() {
    run_and_check_registers!(
        "
        function sum(arr) {
            let s = 0;
            for x in arr {
                s += x;
            }
            return s;
        }
        function main() {
            sum([1, 2, 3]);
            print(sum([1, 2, 3]));
        }
        ",
        6.into()
    );
}

// --- two for loops in sequence in the same function ---
#[test]
pub fn two_for_loops_in_sequence() {
    run_and_check_registers!(
        "
        function main() {
            let a = [1, 2, 3];
            let b = [10, 20, 30];
            let sum = 0;
            for x in a { sum += x; }
            for x in b { sum += x; }
            print(sum);
        }
        ",
        66.into()
    );
}

// --- early return from inside a for loop ---
#[test]
pub fn early_return_from_for_loop() {
    run_and_check_registers!(
        "
        function first_positive(arr) {
            for x in arr {
                if x > 0 { return x; }
            }
            return 0;
        }
        function main() {
            print(first_positive([-3, -1, 5, 8]));
        }
        ",
        5.into()
    );
}

// --- early return from inside a while loop ---
#[test]
pub fn early_return_from_while_loop() {
    run_and_check_registers!(
        "
        function find(limit) {
            let i = 0;
            while i < limit {
                if i == 7 { return i; }
                i += 1;
            }
            return -1;
        }
        function main() {
            print(find(20));
        }
        ",
        7.into()
    );
}

// --- nested function call as argument ---
#[test]
pub fn nested_fn_call_as_arg() {
    run_and_check_registers!(
        "
        function double(n) { return n * 2; }
        function inc(n)    { return n + 1; }
        function main() {
            print(double(inc(double(3))));
        }
        ",
        14.into()
    );
}

// --- function with multiple sequential for loops called twice ---
#[test]
pub fn multi_loop_fn_called_twice() {
    run_and_check_registers!(
        "
        function run(arr) {
            let s = 0;
            for x in arr { s += x; }
            for x in arr { s += x; }
            print(s);
        }
        function main() {
            run([1, 2, 3]);
            run([1, 2, 3]);
        }
        ",
        12.into()
    );
}

// --- while loop function called twice ---
#[test]
pub fn while_fn_called_twice() {
    run_and_check_registers!(
        "
        function count_down(n) {
            let s = 0;
            while n > 0 {
                s += n;
                n -= 1;
            }
            return s;
        }
        function main() {
            count_down(5);
            print(count_down(5));
        }
        ",
        15.into()
    );
}

// --- function returning an array ---
#[test]
pub fn function_returns_array() {
    run_and_check_registers!(
        "
        function make(n) {
            return [n, n * 2, n * 3];
        }
        function main() {
            let arr = make(4);
            print(arr[0]+arr[1]+arr[2]);
        }
        ",
        24.into()
    );
}

// --- passing an array to a function and reading it ---
#[test]
pub fn pass_array_to_function() {
    run_and_check_registers!(
        "
        function last(arr) {
            let n = arr.len();
            return arr[n - 1];
        }
        function main() {
            print(last([7, 8, 9]));
        }
        ",
        9.into()
    );
}

// --- string split result iterated ---
#[test]
pub fn string_split_then_iterate() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "a,b,c,d,e";
            let parts = s.split(",");
            let count = 0;
            for p in parts { count += 1; }
            print(count);
        }
        "#,
        5.into()
    );
}

// --- deeply nested conditions ---
#[test]
pub fn deeply_nested_conditions() {
    run_and_check_registers!(
        "
        function classify(n) {
            if n < 0 {
                return 0;
            } else {
                if n < 10 {
                    return 1;
                } else {
                    if n < 100 {
                        return 2;
                    } else {
                        return 3;
                    }
                }
            }
        }
        function main() {
            print(classify(50));
        }
        ",
        2.into()
    );
}

// --- break inside a while loop ---
#[test]
pub fn break_in_while_loop() {
    run_and_check_registers!(
        "
        function main() {
            let i = 0;
            while i < 1000 {
                if i == 42 { break; }
                i += 1;
            }
            print(i);
        }
        ",
        42.into()
    );
}

// --- for loop with _ discard variable ---
#[test]
pub fn for_loop_discard_var() {
    run_and_check_registers!(
        "
        function main() {
            let count = 0;
            for _ in [0, 0, 0, 0, 0] { count += 1; }
            print(count);
        }
        ",
        5.into()
    );
}

// --- integer range loop called twice inside another function ---
#[test]
pub fn int_range_loop_fn_called_twice() {
    run_and_check_registers!(
        "
        function sum_to(n) {
            let s = 0;
            for i in 0..n { s += i; }
            return s;
        }
        function main() {
            sum_to(10);
            print(sum_to(10));
        }
        ",
        45.into()
    );
}

#[test]
pub fn inc_int_to_basic() {
    // y = x + 1 where y != x  →  IncIntTo(x_reg, y_reg)
    run_and_check_registers!(
        "
        function main() {
            let x = 5;
            let y = x + 1;
            print(y);
        }
        ",
        6.into()
    );
}

#[test]
pub fn dec_int_to_basic() {
    // y = x - 1 where y != x  →  DecIntTo(x_reg, y_reg)
    run_and_check_registers!(
        "
        function main() {
            let x = 5;
            let y = x - 1;
            print(y);
        }
        ",
        4.into()
    );
}

#[test]
pub fn inc_int_commutative() {
    // y = 1 + x  (addition is commutative, same fast path)
    run_and_check_registers!(
        "
        function main() {
            let x = 10;
            let y = 1 + x;
            print(y);
        }
        ",
        11.into()
    );
}

#[test]
pub fn inc_int_to_chained() {
    // z = y + 1 where y = x + 1  (two successive IncIntTo)
    run_and_check_registers!(
        "
        function main() {
            let x = 3;
            let y = x + 1;
            let z = y + 1;
            print(z);
        }
        ",
        5.into()
    );
}

#[test]
pub fn inc_int_as_function_arg() {
    // x + 1 passed directly as an argument
    run_and_check_registers!(
        "
        function identity(n) { return n; }
        function main() {
            let x = 7;
            print(identity(x + 1));
        }
        ",
        8.into()
    );
}

#[test]
pub fn dec_int_as_return_value() {
    // return x - 1 from a function
    run_and_check_registers!(
        "
        function pred(n) { return n - 1; }
        function main() {
            print(pred(20));
        }
        ",
        19.into()
    );
}

#[test]
pub fn inc_int_in_condition() {
    // if (x + 1) > threshold — exercises IncIntTo inside a comparison
    run_and_check_registers!(
        "
        function main() {
            let x = 9;
            let result = 0;
            if x + 1 > 9 { result = 1; }
            print(result);
        }
        ",
        1.into()
    );
}

#[test]
pub fn inc_int_does_not_mutate_source() {
    // After y = x + 1, x must still hold its original value
    run_and_check_registers!(
        "
        function main() {
            let x = 41;
            let y = x + 1;
            print(x);
        }
        ",
        41.into()
    );
}

#[test]
pub fn dec_int_does_not_mutate_source() {
    run_and_check_registers!(
        "
        function main() {
            let x = 41;
            let y = x - 1;
            print(x);
        }
        ",
        41.into()
    );
}

#[test]
pub fn int_wraps_on_overflow() {
    // 2147483647 + 1 wraps to -2147483648 (i32 wrapping semantics)
    run_and_check_registers!(
        "
        function main() {
            let x = 2147483647;
            x += 1;
            print(x);
        }
        ",
        (-2147483648_i32).into()
    );
}

#[test]
pub fn int_wraps_on_underflow() {
    // -2147483648 is i32::MIN; subtracting 1 wraps around to i32::MAX = 2147483647
    run_and_check_registers!(
        "
        function main() {
            let x = -2147483648;
            x -= 1;
            print(x);
        }
        ",
        2147483647_i32.into()
    );
}

#[test]
pub fn negative_int_literal() {
    // -2147483648 is i32::MIN and must parse without "integer too big" error
    run_and_check_registers!(
        "
        function main() {
            let x = -2147483648;
            print(x);
        }
        ",
        (-2147483648_i32).into()
    );
}

#[test]
pub fn string_exactly_6_chars() {
    // 6-char strings are stored inline (small string)
    run_and_check_registers!(
        r#"
        function main() {
            let s = "abcdef";
            print(s.len());
        }
        "#,
        6.into()
    );
}

#[test]
pub fn string_exactly_7_chars() {
    // 7-char strings go into the pool (large string)
    run_and_check_registers!(
        r#"
        function main() {
            let s = "abcdefg";
            print(s.len());
        }
        "#,
        7.into()
    );
}

#[test]
pub fn string_small_to_large_concat() {
    // Concatenating two small strings can produce a large one
    run_and_check_registers!(
        r#"
        function main() {
            let a = "abc";
            let b = "defgh";
            let c = a + b;
            print(c.len());
        }
        "#,
        8.into()
    );
}

#[test]
pub fn string_escape_newline() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "a\nb";
            print(s.len());
        }
        "#,
        3.into()
    );
}

#[test]
pub fn string_escape_tab() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "a\tb";
            print(s.len());
        }
        "#,
        3.into()
    );
}

#[test]
pub fn string_escape_backslash() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "a\\b";
            print(s.len());
        }
        "#,
        3.into()
    );
}

#[test]
pub fn string_escape_quote() {
    run_and_check_registers!(
        r#"
        function main() {
            let s = "say \"hello\"";
            print(s.len());
        }
        "#,
        11.into()
    );
}

#[test]
pub fn empty_range_for_loop() {
    // 0..0 should iterate zero times
    run_and_check_registers!(
        "
        function main() {
            let count = 99;
            for _ in 0..0 { count += 1; }
            print(count);
        }
        ",
        99.into()
    );
}

#[test]
pub fn while_never_executes() {
    // Condition false from the start
    run_and_check_registers!(
        "
        function main() {
            let x = 5;
            while x > 10 { x += 1; }
            print(x);
        }
        ",
        5.into()
    );
}

#[test]
pub fn break_only_breaks_inner_loop() {
    run_and_check_registers!(
        "
        function main() {
            let outer = 0;
            for i in 0..3 {
                for j in 0..100 {
                    if j == 2 { break; }
                }
                outer += 1;
            }
            print(outer);
        }
        ",
        3.into()
    );
}

#[test]
pub fn empty_array_len() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [];
            print(arr.len());
        }
        ",
        0.into()
    );
}

#[test]
pub fn empty_array_iteration() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [];
            let count = 0;
            for _ in arr { count += 1; }
            print(count);
        }
        ",
        0.into()
    );
}

#[test]
pub fn single_element_array_len() {
    run_and_check_registers!(
        "
        function main() {
            let arr = [42];
            print(arr.len());
        }
        ",
        1.into()
    );
}

#[test]
pub fn array_after_all_removes() {
    // Remove all elements one by one, check length reaches 0
    run_and_check_registers!(
        "
        function main() {
            let arr = [1, 2, 3];
            arr.remove(0);
            arr.remove(0);
            arr.remove(0);
            print(arr.len());
        }
        ",
        0.into()
    );
}

#[test]
pub fn mutual_recursion() {
    run_and_check_registers!(
        "
        function is_even(n) {
            if n == 0 { return true; }
            return is_odd(n - 1);
        }
        function is_odd(n) {
            if n == 0 { return false; }
            return is_even(n - 1);
        }
        function main() {
            print(is_even(10));
        }
        ",
        true.into()
    );
}

#[test]
pub fn null_literal_store_and_compare() {
    // null literal should be expressible, storable, and equal to itself
    run_and_check_registers!(
        "
        function main() {
            let x = null;
            print(x == null);
        }
        ",
        true.into()
    );
}

#[test]
pub fn null_literal_as_default() {
    // null used as a sentinel / unset value, then overwritten
    run_and_check_registers!(
        "
        function main() {
            let result = null;
            result = 42;
            print(result);
        }
        ",
        42.into()
    );
}
