use lalrpop_util::lalrpop_mod;

#[derive(Debug, Copy, Clone)]
pub enum Const {
    Integer(i32),
    Float(f32),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum Instr {
    Load(Const),

    // OPS
    Add,
    Sub,
    Mul,
    Div,
}

lalrpop_mod!(pub parser_grammar);

fn main() {
    dbg!(std::mem::size_of::<Const>());
    dbg!(std::mem::size_of::<Instr>());

    let expr = parser_grammar::FloatParser::new().parse("65.76");
    println!("{expr:?}");
    // assert!(expr.is_err(),"NOT ERR");
}
