use crate::ast::Expr;

pub fn eval(expr: Expr) -> Expr {
    match expr {
        Expr::IntLit(_) => expr,
    }
}
