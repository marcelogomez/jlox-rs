use crate::expr::Expr;
use crate::token::Literal;
use crate::token::Operator;

pub enum RPNOperator {
    Add,
    Sub,
    Mul,
    Div,
}

impl TryFrom<&Operator> for RPNOperator {
    type Error = RPNError;

    fn try_from(value: &Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Minus => Ok(RPNOperator::Sub),
            Operator::Plus => Ok(RPNOperator::Add),
            Operator::Slash => Ok(RPNOperator::Div),
            Operator::Star => Ok(RPNOperator::Mul),
            _ => Err(RPNError::NonArithmeticOperator),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RPNError {
    StackUnderflow,
    NonNumericLiteral,
    NonArithmeticOperator,
    BadUnaryOperator,
}

type RPNResult<T> = Result<T, RPNError>;

pub fn evaluate(expr: &Expr) -> RPNResult<f64> {
    let mut stack = vec![];
    evaluate_impl(expr, &mut stack)?;
    assert!(stack.len() == 1);
    stack.pop().ok_or(RPNError::StackUnderflow)
}

fn evaluate_impl(expr: &Expr, stack: &mut Vec<f64>) -> RPNResult<()> {
    match expr {
        Expr::Binary(left, op, right) => {
            evaluate_impl(left, stack)?;
            evaluate_impl(right, stack)?;

            let rpn_op = RPNOperator::try_from(op)?;
            let right_val = stack.pop().ok_or(RPNError::StackUnderflow)?;
            let left_val = stack.pop().ok_or(RPNError::StackUnderflow)?;
            stack.push(binary_op_eval(left_val, rpn_op, right_val));

            Ok(())
        }
        Expr::Grouping(expr) => evaluate_impl(expr, stack),
        Expr::Literal(Literal::Number(n)) => {
            stack.push(*n);
            Ok(())
        }
        Expr::Literal(_) => Err(RPNError::NonNumericLiteral),
        Expr::Unary(op, expr) => {
            evaluate_impl(expr, stack)?;

            let rpn_op = RPNOperator::try_from(op)?;
            let val = stack.pop().ok_or(RPNError::StackUnderflow)?;
            stack.push(unary_op_eval(rpn_op, val)?);

            Ok(())
        }
    }
}

fn unary_op_eval(op: RPNOperator, val: f64) -> RPNResult<f64> {
    match op {
        RPNOperator::Add => Ok(val),
        RPNOperator::Sub => Ok(-val),
        RPNOperator::Mul => Err(RPNError::BadUnaryOperator),
        RPNOperator::Div => Err(RPNError::BadUnaryOperator),
    }
}

fn binary_op_eval(left: f64, op: RPNOperator, right: f64) -> f64 {
    match op {
        RPNOperator::Add => left + right,
        RPNOperator::Sub => left - right,
        RPNOperator::Mul => left * right,
        RPNOperator::Div => left / right,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::Literal;
    use crate::token::Operator;

    #[test]
    fn test_rpn_str() {
        let left = Expr::Binary(
            Box::new(Expr::Literal(Literal::Number(1.0))),
            Operator::Plus,
            Box::new(Expr::Literal(Literal::Number(2.0))),
        );
        // 1 2 +
        assert_eq!(evaluate(&left).unwrap(), 3.0);

        let right = Expr::Binary(
            Box::new(Expr::Literal(Literal::Number(3.0))),
            Operator::Minus,
            Box::new(Expr::Literal(Literal::Number(5.0))),
        );
        // 3 5 -
        assert_eq!(evaluate(&right).unwrap(), -2.0);

        let expr = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(left))),
            Operator::Star,
            Box::new(Expr::Grouping(Box::new(right))),
        );

        // 1 2 + 3 5 - *
        assert_eq!(evaluate(&expr).unwrap(), -6.0);
    }
}
