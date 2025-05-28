#[cfg(test)]
mod tests {
    use crate::execute;
    use crate::get_vec_capacity;
    use crate::parse;
    use crate::Data;

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
}
