use phf::phf_map;
use phf::Map;

const KEYWORDS: Map<&str, Keyword> = phf_map! {
    "and" => Keyword::And,
    "class" => Keyword::Class,
    "else" => Keyword::Else,
    "false" => Keyword::False,
    "for" => Keyword::For,
    "fun" => Keyword::Fun,
    "if" => Keyword::If,
    "nil" => Keyword::Nil,
    "or" => Keyword::Or,
    "print" => Keyword::Print,
    "return" => Keyword::Return,
    "super" => Keyword::Super,
    "this" => Keyword::This,
    "true" => Keyword::True,
    "var" => Keyword::Var,
    "while" => Keyword::While,
};

pub fn get_keyword(identifier: &str) -> Option<Keyword> {
    KEYWORDS.get(identifier).cloned()
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,

    Slash,

    Operator(Operator),
    Identifier(String),
    Literal(Literal),
    Keyword(Keyword),

    // EOF
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
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
}

#[derive(Debug, PartialEq)]
pub struct TokenPos {
    pub token: Token,
    pub offset: usize,
}
