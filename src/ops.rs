use crate::Expr;

impl std::ops::Add<Expr> for Expr {
    type Output = Expr;

    fn add(self, rhs: Expr) -> Expr {
        match (&self, &rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x + y),
            _ => Expr::Add(Box::from(self), Box::from(rhs)),
        }
    }
}

impl std::ops::Sub<Expr> for Expr {
    type Output = Expr;

    fn sub(self, rhs: Expr) -> Expr {
        match (&self, &rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x - y),
            _ => Expr::Sub(Box::from(self), Box::from(rhs)),
        }
    }
}

impl std::ops::Mul<Expr> for Expr {
    type Output = Expr;

    fn mul(self, rhs: Expr) -> Expr {
        match (&self, &rhs) {
            (Expr::Num(x), Expr::Num(y)) => Expr::Num(x * y),
            _ => Expr::Mul(Box::from(self), Box::from(rhs)),
        }
    }
}
