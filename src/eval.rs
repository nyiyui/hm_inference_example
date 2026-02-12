use crate::ast::{BinaryOp, Expr, UnaryOp};

fn subst(e: &Expr, v: &Expr, x: &str) -> Expr {
    match e {
        Expr::Int(_) | Expr::Bool(_) => e.clone(),
        Expr::Var(y) => {
            if y == x {
                v.clone()
            } else {
                e.clone()
            }
        }
        Expr::OpUnary(uop, e1) => Expr::OpUnary(uop.clone(), Box::new(subst(e1, v, x))),
        Expr::OpBinary(bop, e1, e2) => Expr::OpBinary(
            bop.clone(),
            Box::new(subst(e1, v, x)),
            Box::new(subst(e2, v, x)),
        ),
        Expr::If(cond, e1, e2) => Expr::If(
            Box::new(subst(cond, v, x)),
            Box::new(subst(e1, v, x)),
            Box::new(subst(e2, v, x)),
        ),
        Expr::Closure(y, e1) => {
            if y == x {
                e.clone()
            } else {
                Expr::Closure(y.clone(), Box::new(subst(e1, v, x)))
            }
        }
        Expr::Application(e1, e2) => {
            Expr::Application(Box::new(subst(e1, v, x)), Box::new(subst(e2, v, x)))
        }
        Expr::Let(y, e1, e2) => {
            let e1_prime = subst(e1, v, x);
            if y == x {
                Expr::Let(y.clone(), Box::new(e1_prime), e2.clone())
            } else {
                Expr::Let(y.clone(), Box::new(e1_prime), Box::new(subst(e2, v, x)))
            }
        }
    }
}

pub fn eval(e: &Expr) -> Result<Expr, String> {
    match e {
        Expr::Int(_) | Expr::Bool(_) | Expr::Closure(_, _) => Ok(e.clone()),
        Expr::Var(x) => Err(format!("unbound variable {} while evaluating {}", x, e)),
        Expr::OpUnary(uop, e1) => eval_uop(uop, e1),
        Expr::OpBinary(bop, e1, e2) => eval_bop(bop, e1, e2),
        Expr::Application(e1, e2) => {
            let e1_val = eval(e1)?;
            match e1_val {
                Expr::Closure(x, body) => {
                    let substituted = subst(&body, e2, &x);
                    eval(&substituted)
                }
                _ => Err("application of non-closure".to_string()),
            }
        }
        Expr::Let(x, e1, e2) => {
            let e1_val = eval(e1)?;
            let substituted = subst(e2, &e1_val, x);
            eval(&substituted)
        }
        Expr::If(cond, e_then, e_else) => {
            let cond_val = eval(cond)?;
            match cond_val {
                Expr::Bool(true) => eval(e_then),
                Expr::Bool(false) => eval(e_else),
                _ => Err("guard must be bool".to_string()),
            }
        }
    }
}

fn eval_uop(uop: &UnaryOp, e1: &Expr) -> Result<Expr, String> {
    let e1_val = eval(e1)?;
    match (uop, e1_val) {
        (UnaryOp::Not, Expr::Bool(true)) => Ok(Expr::Bool(false)),
        (UnaryOp::Not, Expr::Bool(false)) => Ok(Expr::Bool(true)),
        (UnaryOp::Neg, Expr::Int(a)) => Ok(Expr::Int(-a)),
        _ => Err("unary op with given operand not defined".to_string()),
    }
}

fn eval_bop(bop: &BinaryOp, e1: &Expr, e2: &Expr) -> Result<Expr, String> {
    let e1_val = eval(e1)?;
    let e2_val = eval(e2)?;
    match (bop, e1_val, e2_val) {
        (BinaryOp::Add, Expr::Int(a), Expr::Int(b)) => Ok(Expr::Int(a + b)),
        (BinaryOp::Mul, Expr::Int(a), Expr::Int(b)) => Ok(Expr::Int(a * b)),
        (BinaryOp::And, Expr::Bool(a), Expr::Bool(b)) => Ok(Expr::Bool(a && b)),
        (BinaryOp::Or, Expr::Bool(a), Expr::Bool(b)) => Ok(Expr::Bool(a || b)),
        (BinaryOp::Equal, Expr::Int(a), Expr::Int(b)) => Ok(Expr::Bool(a == b)),
        (BinaryOp::Equal, Expr::Bool(a), Expr::Bool(b)) => Ok(Expr::Bool(a == b)),
        _ => Err("binary op with given operands not defined".to_string()),
    }
}
