use concat_string::concat_string;

use crate::Expr;

impl std::ops::Add<Expr> for Expr {
    type Output = Expr;

    fn add(self, rhs: Expr) -> Expr {
        match (self, rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x + y),
            (Expr::String(x), Expr::String(y)) => Expr::String(concat_string!(x, y)),
            (Expr::Array(x), Expr::Array(y)) => Expr::Array(Box::from([x, y].concat())),
            (l, r) => Expr::Add(Box::from(l), Box::from(r)),
        }
    }
}

impl std::ops::Sub<Expr> for Expr {
    type Output = Expr;

    fn sub(self, rhs: Expr) -> Expr {
        match (self, rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x - y),
            (l, r) => Expr::Sub(Box::from(l), Box::from(r)),
        }
    }
}

impl std::ops::Mul<Expr> for Expr {
    type Output = Expr;

    fn mul(self, rhs: Expr) -> Expr {
        match (self, rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x * y),
            (l, r) => Expr::Mul(Box::from(l), Box::from(r)),
        }
    }
}

impl std::ops::Div<Expr> for Expr {
    type Output = Expr;

    fn div(self, rhs: Expr) -> Expr {
        match (self, rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x / y),
            (l, r) => Expr::Div(Box::from(l), Box::from(r)),
        }
    }
}

impl std::ops::Rem<Expr> for Expr {
    type Output = Expr;

    fn rem(self, rhs: Expr) -> Expr {
        match (self, rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x % y),
            (l, r) => Expr::Mod(Box::from(l), Box::from(r)),
        }
    }
}

impl std::ops::Neg for Expr {
    type Output = Expr;

    fn neg(self) -> Expr {
        match self {
            Expr::Num(x) => Expr::Num(-x),
            l => Expr::Neg(Box::from(l)),
        }
    }
}
