#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Float(f32),
    String(String),
    Identifier(String),
    BinaryOp(BasicOperator, Box<Expr>),
    Priority(Box<Expr>)
}

#[derive(Debug)]
pub enum BasicOperator {
    AND,
    Sub,
    Modulo,
    Add,
    Multiply,
    Inferior,
    EQUAL,
    Power,
    OR,
    Superior,
    Divide
}

#[derive(Debug)]
pub struct VariableDeclaration {
    variable: String,
    value: Expr,
}
