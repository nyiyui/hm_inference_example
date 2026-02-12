use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
    Add,
    Mul,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Int(i32),
    Bool(bool),
    OpUnary(UnaryOp, Box<Expr>),
    OpBinary(BinaryOp, Box<Expr>, Box<Expr>),
    Closure(String, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Var(x) => write!(f, "{}", x),
            Expr::Int(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::OpUnary(op, e) => match op {
                UnaryOp::Not => write!(f, "not {}", e),
                UnaryOp::Neg => write!(f, "-{}", e),
            },
            Expr::OpBinary(op, e1, e2) => match op {
                BinaryOp::Equal => write!(f, "{} = {}", e1, e2),
                BinaryOp::Add => write!(f, "{} + {}", e1, e2),
                BinaryOp::Mul => write!(f, "{} * {}", e1, e2),
                BinaryOp::And => write!(f, "{} && {}", e1, e2),
                BinaryOp::Or => write!(f, "{} || {}", e1, e2),
            },
            Expr::Closure(x, e) => write!(f, "{} -> {}", x, e),
            Expr::Application(e1, e2) => write!(f, "({}) ({})", e1, e2),
            Expr::Let(x, e1, e2) => write!(f, "let {} = {} in {}", x, e1, e2),
            Expr::If(e1, e2, e3) => write!(f, "if {} then {} else {}", e1, e2, e3),
        }
    }
}
