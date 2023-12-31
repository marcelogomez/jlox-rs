use std::str::CharIndices;
use std::str::FromStr;

use crate::token::get_keyword;
use crate::token::Literal;
use crate::token::Operator;
use crate::token::Token;
use crate::token::TokenPos;

use itertools::Itertools;
use itertools::MultiPeek;

#[derive(Debug, PartialEq)]
pub enum Error {
    MalformedString,
    MalformedNumber,
    UnknownToken(char),
    UnclosedComment,
}

#[derive(Debug, PartialEq)]
pub struct LexerError {
    pub line_number: usize,
    pub error: Error,
}

type LexerResult<T> = Result<T, LexerError>;

pub struct Lexer<'a> {
    code: &'a str,
    chars: MultiPeek<CharIndices<'a>>,
    line_number: usize,
    finished: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        Self {
            code,
            chars: code.char_indices().multipeek(),
            line_number: 1,
            finished: false,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexerResult<TokenPos>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let next = self.next_token();
        self.finished = matches!(
            next,
            Ok(TokenPos {
                token: Token::EOF,
                ..
            }),
        );
        Some(next)
    }
}

impl Lexer<'_> {
    fn next_token(&mut self) -> LexerResult<TokenPos> {
        if let Some((pos, char)) = self.chars.next() {
            let token = match char {
                // single character tokens
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                ';' => Token::Semicolon,
                ',' => Token::Comma,
                '.' => Token::Operator(Operator::Dot),
                '-' => Token::Operator(Operator::Minus),
                '+' => Token::Operator(Operator::Plus),
                '*' => Token::Operator(Operator::Star),
                // two character tokens
                '!' if self.advance_if(|c| c == &'=') => Token::Operator(Operator::BangEqual),
                '!' => Token::Operator(Operator::Bang),
                '=' if self.advance_if(|c| c == &'=') => Token::Operator(Operator::EqualEqual),
                '=' => Token::Operator(Operator::Equal),
                '<' if self.advance_if(|c| c == &'=') => Token::Operator(Operator::LessEqual),
                '<' => Token::Operator(Operator::Less),
                '>' if self.advance_if(|c| c == &'=') => Token::Operator(Operator::GreaterEqual),
                '>' => Token::Operator(Operator::Greater),
                ' ' | '\r' | '\t' => return self.next_token(),
                '\n' => {
                    self.line_number += 1;
                    return self.next_token();
                }
                // Comments or division
                '/' if self.advance_if(|c| c == &'/') => {
                    self.advance_while(|&c| c != '\n');
                    return self.next_token();
                }
                // Block comments
                '/' if self.advance_if(|c| c == &'*') => {
                    self.skip_comment_block()?;
                    return self.next_token();
                }
                '/' => Token::Slash,
                '"' => Token::Literal(Literal::String(self.read_string_literal(pos)?)),
                // Parse a number literal
                _ if char.is_ascii_digit() => {
                    Token::Literal(Literal::Number(self.read_number_literal(pos)?))
                }
                // Identifiers or keywords
                _ if is_valid_for_identifier(&char) => {
                    let iden = self.read_identifier(pos);
                    match get_keyword(iden) {
                        Some(keyword) => Token::Keyword(keyword),
                        None => Token::Identifier(iden.to_string()),
                    }
                }
                _ => {
                    return Err(LexerError {
                        line_number: self.line_number,
                        error: Error::UnknownToken(char),
                    })
                }
            };

            Ok(TokenPos { token, offset: pos })
        } else {
            Ok(TokenPos {
                token: Token::EOF,
                offset: self.code.len(),
            })
        }
    }

    fn skip_comment_block(&mut self) -> LexerResult<()> {
        let mut open_comments_count = 1;
        while let Some((_, cur)) = self.chars.next() {
            match cur {
                '/' if self.advance_if(|c| c == &'*') => open_comments_count += 1,
                '*' if self.advance_if(|c| c == &'/') => {
                    open_comments_count -= 1;
                    // Comment block is over, move to next iteration
                    if open_comments_count == 0 {
                        break;
                    }
                }
                '\n' => self.line_number += 1,
                _ => {}
            }
        }

        if open_comments_count > 0 {
            Err(LexerError {
                line_number: self.line_number,
                error: Error::UnclosedComment,
            })
        } else {
            Ok(())
        }
    }

    fn read_identifier(&mut self, pos: usize) -> &str {
        // Add 1 to account for the already read character at pos
        let iden_len = 1 + self.advance_while(is_valid_for_identifier);
        &self.code[pos..pos + iden_len]
    }

    fn read_number_literal(&mut self, pos: usize) -> LexerResult<f64> {
        // Add 1 to account for the already consumed char
        let decimal_part_len = 1 + self.advance_while(char::is_ascii_digit);
        let fractional_part_len = match (self.chars.peek().copied(), self.chars.peek()) {
            (Some((_, '.')), Some((_, c))) if c.is_ascii_digit() => {
                // Consume period and add account for it in the fractional part's length
                let _ = self.chars.next();
                1 + self.advance_while(char::is_ascii_digit)
            }
            _ => 0,
        };

        let lexeme = &self.code[pos..pos + decimal_part_len + fractional_part_len];
        f64::from_str(lexeme).map_err(|_error| LexerError {
            line_number: self.line_number,
            error: Error::MalformedNumber,
        })
    }

    fn read_string_literal(&mut self, pos: usize) -> LexerResult<String> {
        // clippy recommends using a for loop instead, but that would move
        // the iterator which is not allowed behind a mutable reference
        #[allow(clippy::while_let_on_iterator)]
        while let Some((end_pos, c)) = self.chars.next() {
            if c == '\n' {
                self.line_number += 1;
            }
            if c == '"' {
                return Ok(self.code[pos + 1..end_pos].to_string());
            }
        }

        Err(LexerError {
            line_number: self.line_number,
            error: Error::MalformedString,
        })
    }

    fn advance_if<P: Fn(&char) -> bool>(&mut self, predicate: P) -> bool {
        let advanced = if self
            .chars
            .peek()
            .filter(|(_pos, char)| predicate(char))
            .is_some()
        {
            self.chars.next();
            true
        } else {
            false
        };
        self.chars.reset_peek();
        advanced
    }

    fn advance_while<P: Fn(&char) -> bool>(&mut self, predicate: P) -> usize {
        let mut count = 0;
        while self
            .chars
            .peek()
            .filter(|(_pos, char)| predicate(char))
            .is_some()
        {
            let _ = self.chars.next();
            count += 1;
        }
        self.chars.reset_peek();
        count
    }
}

fn is_valid_for_identifier(c: &char) -> bool {
    c.is_alphanumeric() || c == &'_'
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::Keyword;
    use crate::token::Literal;
    use crate::token::Operator;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_single_line_comments() {
        let code = "// A\n// Comment\nvar x = 42;// XD;\n#";
        let tokens: Vec<_> = Lexer::new(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Keyword(Keyword::Var),
                    offset: 16
                }),
                Ok(TokenPos {
                    token: Token::Identifier("x".to_string()),
                    offset: 20
                }),
                Ok(TokenPos {
                    token: Token::Operator(Operator::Equal),
                    offset: 22
                }),
                Ok(TokenPos {
                    token: Token::Literal(Literal::Number(42.0)),
                    offset: 24
                }),
                Ok(TokenPos {
                    token: Token::Semicolon,
                    offset: 26
                }),
                Err(LexerError {
                    line_number: 4,
                    error: Error::UnknownToken('#'),
                }),
                Ok(TokenPos {
                    token: Token::EOF,
                    offset: 35
                }),
            ],
        );
    }

    #[test]
    fn test_unclosed_comments() {
        let code = "class X {}\n/*/**/";
        let tokens: Vec<_> = Lexer::new(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Keyword(Keyword::Class),
                    offset: 0
                }),
                Ok(TokenPos {
                    token: Token::Identifier("X".to_string()),
                    offset: 6
                }),
                Ok(TokenPos {
                    token: Token::LeftBrace,
                    offset: 8
                }),
                Ok(TokenPos {
                    token: Token::RightBrace,
                    offset: 9
                }),
                Err(LexerError {
                    line_number: 2,
                    error: Error::UnclosedComment
                }),
                Ok(TokenPos {
                    token: Token::EOF,
                    offset: 17
                }),
            ],
        );
    }

    #[test]
    fn test_nested_block_comments() {
        let code = "/*/*/**/*/*/\n;";
        let tokens: Vec<_> = Lexer::new(code).collect();
        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Semicolon,
                    offset: 13
                }),
                Ok(TokenPos {
                    token: Token::EOF,
                    offset: 14
                })
            ],
        );
    }

    #[test]
    fn test_multi_line_comments() {
        let code = "/* A\n* multi\n* line\n* comment **//*This is a comment*/ var x = 1.0;";
        let tokens: Vec<_> = Lexer::new(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Keyword(Keyword::Var),
                    offset: 55
                }),
                Ok(TokenPos {
                    token: Token::Identifier("x".to_string()),
                    offset: 59
                }),
                Ok(TokenPos {
                    token: Token::Operator(Operator::Equal),
                    offset: 61
                }),
                Ok(TokenPos {
                    token: Token::Literal(Literal::Number(1.0)),
                    offset: 63
                }),
                Ok(TokenPos {
                    token: Token::Semicolon,
                    offset: 66
                }),
                Ok(TokenPos {
                    token: Token::EOF,
                    offset: 67
                }),
            ],
        );
    }

    #[test]
    fn test_string_literals() {
        let code = "var x = \"This is a \nmulti\nline\nstring\nliteral\";\n";
        let tokens: Vec<_> = Lexer::new(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Keyword(Keyword::Var),
                    offset: 0
                }),
                Ok(TokenPos {
                    token: Token::Identifier("x".to_string()),
                    offset: 4
                }),
                Ok(TokenPos {
                    token: Token::Operator(Operator::Equal),
                    offset: 6
                }),
                Ok(TokenPos {
                    token: Token::Literal(Literal::String(
                        "This is a \nmulti\nline\nstring\nliteral".to_string()
                    )),
                    offset: 8
                }),
                Ok(TokenPos {
                    token: Token::Semicolon,
                    offset: 46
                }),
                Ok(TokenPos {
                    token: Token::EOF,
                    offset: 48
                }),
            ],
        );
    }

    #[test]
    fn test_lexer() {
        let code = "var _a_variable = 1.0 + 123.;# // XD;";

        let tokens: Vec<_> = Lexer::new(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Keyword(Keyword::Var),
                    offset: 0
                }),
                Ok(TokenPos {
                    token: Token::Identifier("_a_variable".to_string()),
                    offset: 4
                }),
                Ok(TokenPos {
                    token: Token::Operator(Operator::Equal),
                    offset: 16
                }),
                Ok(TokenPos {
                    token: Token::Literal(Literal::Number(1.0)),
                    offset: 18
                }),
                Ok(TokenPos {
                    token: Token::Operator(Operator::Plus),
                    offset: 22
                }),
                Ok(TokenPos {
                    token: Token::Literal(Literal::Number(123.0)),
                    offset: 24
                }),
                Ok(TokenPos {
                    token: Token::Operator(Operator::Dot),
                    offset: 27
                }),
                Ok(TokenPos {
                    token: Token::Semicolon,
                    offset: 28
                }),
                Err(LexerError {
                    line_number: 1,
                    error: Error::UnknownToken('#'),
                }),
                Ok(TokenPos {
                    token: Token::EOF,
                    offset: 37
                }),
            ],
        );
    }
}
