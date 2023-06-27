use phf::phf_map;
use phf::Map;

const KEYWORDS: Map<&str, Token> = phf_map! {
    "and" => Token::And,
    "class" => Token::Class,
    "else" => Token::Else,
    "false" => Token::False,
    "for" => Token::For,
    "fun" => Token::Fun,
    "if" => Token::If,
    "nil" => Token::Nil,
    "or" => Token::Or,
    "print" => Token::Print,
    "return" => Token::Return,
    "super" => Token::Super,
    "this" => Token::This,
    "true" => Token::True,
    "var" => Token::Var,
    "while" => Token::While,
};

pub fn get_keyword(identifier: &str) -> Option<Token> {
    KEYWORDS.get(identifier).cloned()
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // 1 or 2 char tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // EOF
    EOF,
}

#[derive(Debug, PartialEq)]
pub struct TokenPos {
    pub token: Token,
    pub offset: usize,
}
