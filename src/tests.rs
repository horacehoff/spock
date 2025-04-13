#[cfg(test)]
mod tests {
    use crate::Data::*;
    use crate::Instr::{
        Add, ApplyFunc, Cmp, GetIndex, Inf, InfCmp, IoDelete, IoOpen, Jmp, Mod, Mov, Mul, Print,
        Range, StoreFuncArg, Sub, SupCmp,
    };
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
}
