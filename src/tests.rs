
#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use crate::{execute, Data};
    use crate::Data::*;
    use crate::Instr::{Add, ApplyFunc, Cmp, Inf, Jmp, Mov, Mul, StoreFuncArg, Sub, Sup};

    #[test]
    fn fibonacci_40() {
        let instr = vec![Sub(0,5,6), Inf(4,6,7), Cmp(7,6), Add(1,2,3), Mov(2,1), Mov(3,2), Add(4,11,4),Jmp(6, true)];
        let mut consts = vec![Number(40.0), Number(0.0), Number(1.0), Number(0.0), Number(0.0), Number(1.0), Null, Null, Null, Null, Null, Number(1.0), Null];
        execute(&instr, &mut consts, 0, &mut HashMap::default());
        assert_eq!(consts[3], Number(102334155.0));
    }

    #[test]
    fn fibonacci_140() {
        let instr = vec![Sub(0,5,6), Inf(4,6,7), Cmp(7,6), Add(1,2,3), Mov(2,1), Mov(3,2), Add(4,11,4),Jmp(6, true)];
        let mut consts = vec![Number(70.0), Number(0.0), Number(1.0), Number(0.0), Number(0.0), Number(1.0), Null, Null, Null, Null, Null, Number(1.0), Null];
        execute(&instr, &mut consts, 0, &mut HashMap::default());
        assert_eq!(consts[3], Number(190392490709135.0));
    }

    #[test]
    fn array_push() {
        let instr = vec![StoreFuncArg(1), ApplyFunc(14, 0, 2)];
        let mut consts = vec![Array(0), Number(7.0), Null];
        let mut arrays:HashMap<u16, Vec<Data>> = HashMap::new();
        arrays.insert(0, vec![Number(1.0),Number(2.0),Number(3.0)]);
        execute(&instr, &mut consts, 1, &mut arrays);
        assert_eq!(arrays[&0], vec![Number(1.0),Number(2.0),Number(3.0),Number(7.0)]);
    }

    #[test]
    fn array_index() {
        let instr = vec![StoreFuncArg(2), ApplyFunc(6,0,3)];
        let mut consts = vec![Array(0), Null, Number(3.0), Null];
        let mut arrays:HashMap<u16, Vec<Data>> = HashMap::new();
        arrays.insert(0, vec![Number(1.0),Number(2.0),Number(3.0)]);
        execute(&instr, &mut consts, 1, &mut arrays);
        assert_eq!(consts[3], Number(2.0));
    }

    #[test]
    fn basic_condition() {
        let instr = vec![Sup(0, 2, 3), Cmp(3, 2), Mov(4, 1)];
        let mut consts = vec![Number(1.0), Bool(false), Number(0.0), Null, Bool(true)];
        execute(&instr, &mut consts, 0, &mut HashMap::default());
        assert_eq!(consts[3], Bool(true));
    }

    #[test]
    fn basic_while_loop() {
        let instr = vec![Inf(1, 2, 3), Cmp(3, 4), Mul(0, 4, 0), Add(1, 6, 1), Jmp(4, true)];
        let mut consts = vec![Number(1.0), Number(0.0), Number(10.0), Null, Number(5.0), Null, Number(1.0), Null];
        execute(&instr, &mut consts, 0, &mut HashMap::default());
        assert_eq!(consts[0], Number(9765625.0));
    }
}
