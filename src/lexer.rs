use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(i32),
    IdValue(String),
    True,
    False,
    Plus,
    Minus,
    Times,
    Equals,
    Not,
    And,
    Or,
    Let,
    In,
    If,
    Then,
    Else,
    LParen,
    RParen,
    RArrow,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Int(n) => write!(f, "{}", n),
            Token::IdValue(s) => write!(f, "{}", s),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Times => write!(f, "*"),
            Token::Equals => write!(f, "="),
            Token::Not => write!(f, "!"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::RArrow => write!(f, "->"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn current_char(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn peek_char(&self, offset: usize) -> Option<char> {
        if self.pos + offset < self.input.len() {
            Some(self.input[self.pos + offset])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> i32 {
        let mut num_str = String::new();

        // Handle negative sign
        if self.current_char() == Some('-') {
            num_str.push('-');
            self.advance();
        }

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        num_str.parse().unwrap_or(0)
    }

    fn read_identifier(&mut self) -> String {
        let mut id = String::new();

        while let Some(ch) = self.current_char() {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' || ch == '\'' {
                id.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        id
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.current_char() {
            None => Token::Eof,
            Some(ch) => {
                if ch.is_ascii_digit() {
                    return Token::Int(self.read_number());
                }

                if ch == '-' {
                    if let Some(next) = self.peek_char(1) {
                        if next == '>' {
                            self.advance();
                            self.advance();
                            return Token::RArrow;
                        } else if next.is_ascii_digit() {
                            return Token::Int(self.read_number());
                        }
                    }
                    self.advance();
                    return Token::Minus;
                }

                if ch.is_alphabetic() {
                    let id = self.read_identifier();
                    return match id.as_str() {
                        "true" => Token::True,
                        "false" => Token::False,
                        "let" => Token::Let,
                        "in" => Token::In,
                        "if" => Token::If,
                        "then" => Token::Then,
                        "else" => Token::Else,
                        _ => Token::IdValue(id),
                    };
                }

                self.advance();
                match ch {
                    '!' => Token::Not,
                    '+' => Token::Plus,
                    '*' => Token::Times,
                    '=' => Token::Equals,
                    '(' => Token::LParen,
                    ')' => Token::RParen,
                    '&' => {
                        if self.current_char() == Some('&') {
                            self.advance();
                            Token::And
                        } else {
                            panic!("Unexpected character: {}", ch);
                        }
                    }
                    '|' => {
                        if self.current_char() == Some('|') {
                            self.advance();
                            Token::Or
                        } else {
                            panic!("Unexpected character: {}", ch);
                        }
                    }
                    _ => panic!("Unexpected character: {}", ch),
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}
