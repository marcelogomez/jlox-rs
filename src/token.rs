use phf::Map;
use phf::phf_map;

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

impl Token {
    fn len(&self) -> usize {
        match self {
            Token::LeftParen => 1,
            Token::RightParen => 1,
            Token::LeftBrace => 1,
            Token::RightBrace => 1,
            Token::Comma => 1,
            Token::Dot => 1,
            Token::Minus => 1,
            Token::Plus => 1,
            Token::Semicolon => 1,
            Token::Slash => 1,
            Token::Star => 1,
            Token::Bang => 1,
            Token::BangEqual => 2,
            Token::Equal => 1,
            Token::EqualEqual => 2,
            Token::Greater => 1,
            Token::GreaterEqual => 2,
            Token::Less => 1,
            Token::LessEqual => 2,
            Token::And => 3,
            Token::Class => 5,
            Token::Else => 4,
            Token::False => 5,
            Token::For => 3,
            Token::Fun => 3,
            Token::If => 2,
            Token::Nil => 3,
            Token::Or => 2,
            Token::Print => 5,
            Token::Return => 6,
            Token::Super => 5,
            Token::This => 4,
            Token::True => 4,
            Token::Var => 3,
            Token::While => 5,
            Token::EOF => 0,
            Token::Identifier(iden) => iden.len(),
            Token::String(str) => str.len() + 2,
            Token::Number(num) => num.to_string().len(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenPos {
    pub token: Token,
    pub offset: usize,
}
