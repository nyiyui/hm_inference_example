use crate::ast::{BinaryOp, Expr, UnaryOp};
use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current_token(&self) -> &Token {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            &Token::Eof
        }
    }

    fn peek_token(&self, offset: usize) -> &Token {
        if self.pos + offset < self.tokens.len() {
            &self.tokens[self.pos + offset]
        } else {
            &Token::Eof
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, expected: Token) {
        if self.current_token() != &expected {
            panic!(
                "Expected {:?}, got {:?}",
                expected,
                self.current_token()
            );
        }
        self.advance();
    }

    pub fn parse(&mut self) -> Expr {
        let expr = self.parse_expr();
        self.expect(Token::Eof);
        expr
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_let_or_if()
    }

    fn parse_let_or_if(&mut self) -> Expr {
        match self.current_token() {
            Token::Let => {
                self.advance();
                let Token::IdValue(name) = self.current_token().clone() else {
                    panic!("Expected identifier after 'let'");
                };
                self.advance();
                self.expect(Token::Equals);
                let e1 = self.parse_expr();
                self.expect(Token::In);
                let e2 = self.parse_expr();
                Expr::Let(name, Box::new(e1), Box::new(e2))
            }
            Token::If => {
                self.advance();
                let cond = self.parse_expr();
                self.expect(Token::Then);
                let then_branch = self.parse_expr();
                self.expect(Token::Else);
                let else_branch = self.parse_expr();
                Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch))
            }
            _ => self.parse_closure(),
        }
    }

    fn parse_closure(&mut self) -> Expr {
        let start_pos = self.pos;
        
        // Try to parse as closure: x -> expr
        if let Token::IdValue(param) = self.current_token().clone() {
            self.advance();
            if let Token::RArrow = self.current_token() {
                self.advance();
                let body = self.parse_expr();
                return Expr::Closure(param, Box::new(body));
            }
            // Not a closure, backtrack
            self.pos = start_pos;
        }
        
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expr {
        let mut left = self.parse_and();
        
        while let Token::Or = self.current_token() {
            self.advance();
            let right = self.parse_and();
            left = Expr::OpBinary(BinaryOp::Or, Box::new(left), Box::new(right));
        }
        
        left
    }

    fn parse_and(&mut self) -> Expr {
        let mut left = self.parse_equality();
        
        while let Token::And = self.current_token() {
            self.advance();
            let right = self.parse_equality();
            left = Expr::OpBinary(BinaryOp::And, Box::new(left), Box::new(right));
        }
        
        left
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_additive();
        
        while let Token::Equals = self.current_token() {
            self.advance();
            let right = self.parse_additive();
            left = Expr::OpBinary(BinaryOp::Equal, Box::new(left), Box::new(right));
        }
        
        left
    }

    fn parse_additive(&mut self) -> Expr {
        let mut left = self.parse_multiplicative();
        
        while let Token::Plus = self.current_token() {
            self.advance();
            let right = self.parse_multiplicative();
            left = Expr::OpBinary(BinaryOp::Add, Box::new(left), Box::new(right));
        }
        
        left
    }

    fn parse_multiplicative(&mut self) -> Expr {
        let mut left = self.parse_unary();
        
        while let Token::Times = self.current_token() {
            self.advance();
            let right = self.parse_unary();
            left = Expr::OpBinary(BinaryOp::Mul, Box::new(left), Box::new(right));
        }
        
        left
    }

    fn parse_unary(&mut self) -> Expr {
        match self.current_token() {
            Token::Not => {
                self.advance();
                let expr = self.parse_unary();
                Expr::OpUnary(UnaryOp::Not, Box::new(expr))
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary();
                Expr::OpUnary(UnaryOp::Neg, Box::new(expr))
            }
            _ => self.parse_application(),
        }
    }

    fn parse_application(&mut self) -> Expr {
        let mut left = self.parse_primary();
        
        loop {
            match self.current_token() {
                Token::Int(_) | Token::True | Token::False | 
                Token::IdValue(_) | Token::LParen => {
                    let right = self.parse_primary();
                    left = Expr::Application(Box::new(left), Box::new(right));
                }
                _ => break,
            }
        }
        
        left
    }

    fn parse_primary(&mut self) -> Expr {
        match self.current_token().clone() {
            Token::Int(n) => {
                self.advance();
                Expr::Int(n)
            }
            Token::True => {
                self.advance();
                Expr::Bool(true)
            }
            Token::False => {
                self.advance();
                Expr::Bool(false)
            }
            Token::IdValue(name) => {
                self.advance();
                Expr::Var(name)
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            _ => panic!("Unexpected token: {:?}", self.current_token()),
        }
    }
}
