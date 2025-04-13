#[cfg(test)]
mod tests {
    use crate::Data::*;
    use crate::Instr::{Add, ApplyFunc, Cmp, GetIndex, Inf, InfCmp, IoDelete, IoOpen, Jmp, Mod, Mov, Mul, Num, Print, Range, StoreFuncArg, Str, Sub, SupCmp, Type};
    use crate::{Data, execute};
    use fnv::FnvHashMap;
    use internment::Intern;

    #[test]
    fn fibonacci_40() {
        let instr = vec![
            Sub(0, 5, 6),
            InfCmp(4, 6, 6),
            Add(1, 2, 3),
            Mov(2, 1),
            Mov(3, 2),
            Add(4, 11, 4),
            Jmp(5, true),
            Print(3),
        ];
        let mut consts = vec![
            Number(40.0),
            Number(0.0),
            Number(1.0),
            Number(0.0),
            Number(0.0),
            Number(1.0),
            Null,
            Null,
            Null,
            Null,
            Null,
            Number(1.0),
            Null,
        ];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(0),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[2], Number(102334155.0));
    }

    #[test]
    fn fibonacci_140() {
        let instr = vec![
            Sub(0, 5, 6),
            InfCmp(4, 6, 6),
            Add(1, 2, 3),
            Mov(2, 1),
            Mov(3, 2),
            Add(4, 11, 4),
            Jmp(5, true),
            Print(3),
        ];
        let mut consts = vec![
            Number(140.0),
            Number(0.0),
            Number(1.0),
            Number(0.0),
            Number(0.0),
            Number(1.0),
            Null,
            Null,
            Null,
            Null,
            Null,
            Number(1.0),
            Null,
        ];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(0),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[2], Number(81055900096023530000000000000.0));
    }

    #[test]
    fn array_push() {
        let instr = vec![StoreFuncArg(1), ApplyFunc(14, 0, 2)];
        let mut consts = vec![Array(0), Number(7.0), Null];
        let mut arrays: FnvHashMap<u16, Vec<Data>> = FnvHashMap::default();
        arrays.insert(0, vec![Number(1.0), Number(2.0), Number(3.0)]);
        execute(&instr, &mut consts, &mut Vec::with_capacity(1), &mut arrays);
        assert_eq!(arrays[&0], vec![
            Number(1.0),
            Number(2.0),
            Number(3.0),
            Number(7.0)
        ]);
    }

    #[test]
    fn array_index() {
        let instr = vec![StoreFuncArg(2), ApplyFunc(6, 0, 3)];
        let mut consts = vec![Array(0), Null, Number(3.0), Null];
        let mut arrays: FnvHashMap<u16, Vec<Data>> = FnvHashMap::default();
        arrays.insert(0, vec![Number(1.0), Number(2.0), Number(3.0)]);
        execute(&instr, &mut consts, &mut Vec::with_capacity(1), &mut arrays);
        assert_eq!(consts[3], Number(2.0));
    }

    #[test]
    fn basic_condition() {
        let instr = vec![SupCmp(0, 2, 2), Mov(4, 1)];
        let mut consts = vec![Number(1.0), Bool(false), Number(0.0), Null, Bool(true)];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(0),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[1], Bool(true));
    }

    #[test]
    fn basic_while_loop() {
        let instr = vec![
            Inf(1, 2, 3),
            Cmp(3, 4),
            Mul(0, 4, 0),
            Add(1, 6, 1),
            Jmp(4, true),
        ];
        let mut consts = vec![
            Number(1.0),
            Number(0.0),
            Number(10.0),
            Null,
            Number(5.0),
            Null,
            Number(1.0),
            Null,
        ];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(0),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[0], Number(9765625.0));
    }

    #[test]
    fn mul_add_rpn() {
        let instr = vec![Mul(0, 1, 2), Add(3, 2, 5)];
        let mut consts = vec![Number(5.0), Number(2.0), Null, Number(2.0), Null, Null];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(0),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[5], Number(12.0));
    }

    #[test]
    fn loop_sum() {
        let mut consts = vec![Number(0.0), Number(99999999.0), Null, Number(1.0), Null];
        let instr = vec![InfCmp(0, 1, 3), Add(0, 3, 0), Jmp(2, true)];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(0),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[0], Number(99999999.0))
    }

    #[test]
    fn while_loop_mul_mod_add() {
        let mut consts = vec![
            Number(0.0),
            Number(1000000.0),
            Number(1.0),
            Null,
            Number(2.0),
            Null,
            Number(1000000.0),
            Null,
            Number(1000000.0),
            Null,
            Number(1.0),
            Null,
        ];
        let instr = vec![
            InfCmp(0, 1, 6),
            Mul(2, 4, 2),
            SupCmp(2, 6, 2),
            Mod(2, 8, 2),
            Add(0, 10, 0),
            Jmp(5, true),
            Print(2),
        ];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(0),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[2], Number(109376.0))
    }

    #[test]
    fn for_loop_sum() {
        let mut consts = vec![
            Number(0.0),
            Array(0),
            Number(420.0),
            Null,
            Null,
            Number(0.0),
            Null,
            Null,
            Null,
            Number(1.0),
        ];
        let instr = vec![
            StoreFuncArg(2),
            ApplyFunc(13, 1, 3),
            ApplyFunc(2, 3, 4),
            InfCmp(5, 4, 5),
            GetIndex(3, 5, 7),
            Add(0, 7, 0),
            Add(5, 9, 5),
            Jmp(4, true),
            Print(0),
        ];
        let mut arrays: FnvHashMap<u16, Vec<Data>> = FnvHashMap::default();
        arrays.insert(0, vec![
            Number(1.0),
            Number(2.0),
            Number(3.0),
            Number(4.0),
            Number(5.0),
        ]);
        execute(&instr, &mut consts, &mut Vec::with_capacity(1), &mut arrays);
        assert_eq!(consts[0], Number(6300.0))
    }

    #[test]
    fn io_loop() {
        let mut consts = vec![
            String(Intern::from_ref("")),
            Number(30.0),
            Number(0.0),
            Null,
            Null,
            Number(0.0),
            Null,
            Null,
            String(Intern::from_ref("test")),
            Null,
            Bool(true),
            Null,
            String(Intern::from_ref("THIS IS A TEST")),
            Bool(false),
            Null,
            Null,
            Null,
            Number(1.0),
            Null,
            Null,
        ];
        let instr = vec![
            Range(2, 1, 3),
            ApplyFunc(2, 3, 4),
            InfCmp(5, 4, 11),
            GetIndex(3, 5, 7),
            IoOpen(8, 11, 10),
            StoreFuncArg(12),
            StoreFuncArg(13),
            ApplyFunc(19, 11, 14),
            ApplyFunc(18, 11, 15),
            Add(0, 15, 0),
            IoDelete(8),
            Add(5, 17, 5),
            Jmp(10, true),
            ApplyFunc(2, 0, 19),
        ];
        execute(
            &instr,
            &mut consts,
            &mut Vec::with_capacity(2),
            &mut FnvHashMap::default(),
        );
        assert_eq!(consts[consts.len() - 1], Number(420.0))
    }


    #[test]
    fn builtin_funcs() {
        let instr = vec![Range(2, 1, 3), ApplyFunc(2, 3, 4), InfCmp(5, 4, 96), GetIndex(3, 5, 7), StoreFuncArg(8), ApplyFunc(14, 0, 9), Type(8, 10), StoreFuncArg(10), ApplyFunc(14, 0, 11), ApplyFunc(7, 8, 12), StoreFuncArg(12), ApplyFunc(14, 0, 13), ApplyFunc(7, 8, 14), Cmp(14, 11), Num(8, 16), ApplyFunc(17, 16, 17), StoreFuncArg(17), ApplyFunc(14, 0, 18), ApplyFunc(16, 16, 19), StoreFuncArg(19), ApplyFunc(14, 0, 20), ApplyFunc(15, 16, 21), StoreFuncArg(21), ApplyFunc(14, 0, 22), ApplyFunc(2, 8, 23), StoreFuncArg(23), ApplyFunc(14, 0, 24), ApplyFunc(0, 8, 25), StoreFuncArg(25), ApplyFunc(14, 0, 26), ApplyFunc(1, 8, 27), StoreFuncArg(27), ApplyFunc(14, 0, 28), ApplyFunc(4, 29, 30), StoreFuncArg(30), ApplyFunc(14, 0, 31), ApplyFunc(8, 29, 32), StoreFuncArg(32), ApplyFunc(14, 0, 33), ApplyFunc(9, 29, 34), StoreFuncArg(34), ApplyFunc(14, 0, 35), StoreFuncArg(38), ApplyFunc(5, 36, 37), StoreFuncArg(37), ApplyFunc(14, 0, 39), StoreFuncArg(41), ApplyFunc(10, 36, 40), StoreFuncArg(40), ApplyFunc(14, 0, 42), StoreFuncArg(44), ApplyFunc(11, 36, 43), StoreFuncArg(43), ApplyFunc(14, 0, 45), StoreFuncArg(46), ApplyFunc(3, 29, 47), StoreFuncArg(47), ApplyFunc(14, 0, 48), StoreFuncArg(50), ApplyFunc(6, 29, 49), StoreFuncArg(49), ApplyFunc(14, 0, 51), StoreFuncArg(53), ApplyFunc(12, 29, 52), StoreFuncArg(52), ApplyFunc(14, 0, 54), Range(56, 55, 57), StoreFuncArg(57), ApplyFunc(14, 0, 58), Range(59, 60, 61), StoreFuncArg(61), ApplyFunc(14, 0, 62), StoreFuncArg(64), ApplyFunc(13, 63, 66), StoreFuncArg(66), ApplyFunc(14, 0, 67), ApplyFunc(2, 66, 68), StoreFuncArg(68), ApplyFunc(14, 0, 69), StoreFuncArg(71), ApplyFunc(14, 70, 72), ApplyFunc(2, 70, 73), StoreFuncArg(73), ApplyFunc(14, 0, 74), StoreFuncArg(76), ApplyFunc(6, 70, 75), StoreFuncArg(75), ApplyFunc(14, 0, 77), StoreFuncArg(79), ApplyFunc(12, 70, 78), StoreFuncArg(78), ApplyFunc(14, 0, 80), StoreFuncArg(81), ApplyFunc(13, 70, 82), StoreFuncArg(82), ApplyFunc(14, 0, 83), Add(5, 84, 5), Jmp(95, true), ApplyFunc(2, 0, 86), InfCmp(87, 86, 6), GetIndex(0, 87, 89), Str(89, 90), Add(85, 90, 85), Add(87, 92, 87), Jmp(5, true)];
        let mut consts = vec![Array(0), Number(8.0), Number(0.0), Null, Null, Number(0.0), Null, Null, String(Intern::from_ref("4848.45416516546132498498498465465")), Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, String(Intern::from_ref("   Hello World   ")), Null, Null, Null, Null, Null, Null, String(Intern::from_ref("###Example###")), Null, String(Intern::from_ref("#")), Null, Null, String(Intern::from_ref("#")), Null, Null, String(Intern::from_ref("#")), Null, String(Intern::from_ref("World")), Null, Null, Null, String(Intern::from_ref("o")), Null, Null, String(Intern::from_ref("o")), Null, Number(5.0), Number(0.0), Null, Null, Number(3.0), Number(7.0), Null, Null, String(Intern::from_ref("Hi")), Number(3.0), Null, Null, Null, Null, Null, Array(1), Number(4.0), Null, Null, Null, Null, Number(2.0), Null, Null, Number(2.0), Null, Number(2.0), Null, Null, Number(1.0), String(Intern::from_ref("")), Null, Number(0.0), Null, Null, Null, Null, Number(1.0)];
        let mut arrays: FnvHashMap<u16, Vec<Data>> = FnvHashMap::default();
        arrays.insert(1, vec![Number(1.0), Number(2.0), Number(3.0)]);
        arrays.insert(0, vec![]);
        execute(&instr, &mut consts, &mut Vec::with_capacity(1), &mut arrays);
        assert_eq!(consts[consts.len() - 8], String(Intern::from_ref("4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi6411[1,2,3,4,1,2,3,4]4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi6511[1,2,3,4,4,1,2,3,4,4]4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi6611[1,2,3,4,4,4,1,2,3,4,4,4]4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi6711[1,2,3,4,4,4,4,1,2,3,4,4,4,4]4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi6811[1,2,3,4,4,4,4,4,1,2,3,4,4,4,4,4]4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi6911[1,2,3,4,4,4,4,4,4,1,2,3,4,4,4,4,4,4]4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi61011[1,2,3,4,4,4,4,4,4,4,1,2,3,4,4,4,4,4,4,4]4848.45416516546132498498498465465Stringtrue4848.454165165462484869.63084205411752344848.454165165461324984984984654654848.45416516546132498498498465465Hello WorldHello World      Hello WorldExampleExample######Exampletrue710[0,1,2,3,4][3,4,5,6]HiHiHi61111[1,2,3,4,4,4,4,4,4,4,4,1,2,3,4,4,4,4,4,4,4,4]")));
    }
}
