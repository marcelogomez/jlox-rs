use crate::token::Literal;
use crate::token::Operator;

pub enum Expr {
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(Operator, Box<Expr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(left, op, right) => {
                write!(f, "({op:?} {left:?} {right:?})")
            }
            Self::Grouping(expr) => {
                write!(f, "(Grouping {expr:?})")
            }
            Self::Literal(lit) => {
                write!(f, "{lit:?}")
            }
            Self::Unary(op, expr) => {
                write!(f, "({op:?} {expr:?})")
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::Literal;
    use crate::token::Operator;

    #[test]
    fn test_pretty_printer() {
        let left = Expr::Unary(
            Operator::Minus,
            Box::new(Expr::Literal(Literal::Number(123.0))),
        );
        assert_eq!(format!("{:?}", left), "(Minus Number(123.0))");

        let right = Expr::Grouping(Box::new(Expr::Literal(Literal::Number(45.67))));
        assert_eq!(format!("{:?}", right), "(Grouping Number(45.67))");

        let expr = Expr::Binary(Box::new(left), Operator::Star, Box::new(right));
        assert_eq!(
            format!("{expr:?}"),
            "(Star (Minus Number(123.0)) (Grouping Number(45.67)))"
        );
    }
}
