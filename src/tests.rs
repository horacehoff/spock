#[cfg(test)]
mod tests {
    use crate::Data;
    use crate::execute;
    use crate::get_vec_capacity;
    use crate::parse;
    use internment::Intern;

    #[test]
    fn fib_41() {
        let filename = "TEST_fib_41";
        let contents = "fn main() { for _ in range(0,200000) { let n = 40; let a=0; let b=1; let c=0; let i=0; while i < n { c = a+b; a = b; b = c; i = i+1; } } let x = if 1 == 1 {10} else {20}; if (x == 10) {x += 1;} }";
        let (instructions, mut consts, mut arrays, instr_src) = parse(contents, filename);
        let func_args_count = get_vec_capacity(&instructions);
        execute(
            &instructions,
            &mut consts,
            &mut Vec::with_capacity(func_args_count),
            &mut arrays,
            &instr_src,
            &contents,
            &filename,
        );
        assert!(consts.contains(&Data::Number(165580141.0)))
    }

    #[test]
    fn non_recursive_polymorphic_function() {
        let filename = "TEST_non_recursive_polymorphic_function";
        let contents = "fn add(x,y) {return x+y;}fn main() {print(add(10,20));print(add(\"a\",\"b\"));print(add([0,1],[2,3]));}";
        let (instructions, mut consts, mut arrays, instr_src) = parse(contents, filename);
        let func_args_count = get_vec_capacity(&instructions);
        execute(
            &instructions,
            &mut consts,
            &mut Vec::with_capacity(func_args_count),
            &mut arrays,
            &instr_src,
            &contents,
            &filename,
        );
        assert!(consts[5] == Data::Number(30.0));
        assert!(consts[11] == Data::String(Intern::from_ref("ab")));
    }
}
