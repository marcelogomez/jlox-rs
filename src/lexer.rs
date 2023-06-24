use std::str::FromStr;

use crate::token::get_keyword;
use crate::token::Token;
use crate::token::TokenPos;

use itertools::Itertools;
use itertools::MultiPeek;

#[derive(Debug, PartialEq)]
pub enum Error {
    MalformedString,
    MalformedNumber,
    UnknownToken(char),
}

#[derive(Debug, PartialEq)]
pub struct LexerError {
    pub line_number: usize,
    pub error: Error,
}

type LexerResult<T> = Result<T, LexerError>;

pub fn lexer(code: &str) -> impl Iterator<Item = LexerResult<TokenPos>> + '_ {
    let mut line_number = 1;
    let mut chars = code.char_indices().multipeek();
    let mut finished = false;

    std::iter::from_fn(move || {
        if finished {
            return None;
        } 

        let next_token_result = next_token(code, &mut chars, &mut line_number);
        finished = matches!(
            next_token_result,
            Ok(TokenPos {
                token: Token::EOF,
                ..
            })
        );

        Some(next_token_result)
    })
}

fn next_token<I: Iterator<Item = (usize, char)>>(
    code: &str,
    chars: &mut MultiPeek<I>,
    line_number: &mut usize,
) -> LexerResult<TokenPos> {
    if let Some((pos, char)) = chars.next() {
        let token_result =
            match char {
                // single character tokens
                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),
                '{' => Ok(Token::LeftBrace),
                '}' => Ok(Token::RightBrace),
                ';' => Ok(Token::Semicolon),
                ',' => Ok(Token::Comma),
                '.' => Ok(Token::Dot),
                '-' => Ok(Token::Minus),
                '+' => Ok(Token::Plus),
                '*' => Ok(Token::Star),
                // two character tokens
                '!' if advance_if(chars, &'=') => Ok(Token::BangEqual),
                '!' => Ok(Token::Bang),
                '=' if advance_if(chars, &'=') => Ok(Token::EqualEqual),
                '=' => Ok(Token::Equal),
                '<' if advance_if(chars, &'=') => Ok(Token::LessEqual),
                '<' => Ok(Token::Less),
                '>' if advance_if(chars, &'=') => Ok(Token::Greater),
                '>' => Ok(Token::Greater),
                ' ' | '\r' | '\t' => return next_token(code, chars, line_number),
                '\n' => {
                    *line_number += 1;
                    return next_token(code, chars, line_number);
                }
                // Comments or division
                '/' if advance_if(chars, &'/') => {
                    advance_while(chars, |&c| c != '\n');
                    return next_token(code, chars, line_number);
                }
                // Multi line comment
                '/' if advance_if(chars, &'*') => {
                    while let (Some((_, cur)), Some((_, peek))) = (chars.next(), chars.peek()) {
                        match (cur, peek) {
                            // Comment is over, move to next iteration where we'll return EOF
                            ('*', '/') => {
                                // Consume the closing slash
                                let _ = chars.next();
                                return next_token(code, chars, line_number);
                            },
                            ('\n', _) => {
                                *line_number += 1;
                            }
                            _ => {},
                        }
                    }

                    return next_token(code, chars, line_number);
                }
                '/' => Ok(Token::Slash),
                '"' => {
                    let mut ret = Err(LexerError {
                        line_number: *line_number,
                        error: Error::MalformedString,
                    });
                    while let Some((end_pos, c)) = chars.peek().copied() {
                        let _ = chars.next();
                        // We support multi line strings
                        if c == '\n' {
                            *line_number += 1;
                        }
                        if c == '"' {
                            ret = Ok(Token::String(code[pos + 1..end_pos].to_string()));
                            break;
                        }
                    }
                    ret
                }
                // Parse a number literal
                _ if char.is_ascii_digit() => {
                    // Add 1 to account for the already consumed char
                    let decimal_part_len = 1 + advance_while(chars, char::is_ascii_digit);
                    let fractional_part_len = match (chars.peek().copied(), chars.peek()) {
                        (Some((_, '.')), Some((_, c))) if c.is_ascii_digit() => {
                            // Consume period and add account for it in the fractional part's length
                            let _ = chars.next();
                            1 + advance_while(chars, char::is_ascii_digit)
                        }
                        _ => 0,
                    };

                    f64::from_str(&code[pos..pos + decimal_part_len + fractional_part_len])
                        .map(Token::Number)
                        .map_err(|_error| LexerError {
                            line_number: *line_number,
                            error: Error::MalformedNumber,
                        })
                }
                // Identifiers or keywords
                _ if is_valid_for_identifier(&char) => {
                    let iden_len = advance_while(chars, is_valid_for_identifier);
                    let iden_val = &code[pos..pos + iden_len + 1];
                    Ok(get_keyword(iden_val)
                        .unwrap_or_else(|| Token::Identifier(iden_val.to_string())))
                }
                _ => Err(LexerError {
                    line_number: *line_number,
                    error: Error::UnknownToken(char),
                }),
            };

        token_result.map(|t| TokenPos {
            token: t,
            offset: pos,
        })
    } else {
        Ok(TokenPos {
            token: Token::EOF,
            offset: code.len(),
        })
    }
}

fn is_valid_for_identifier(c: &char) -> bool {
    c.is_alphanumeric() || c == &'_'
}

fn advance_while<I: Iterator<Item = (usize, char)>, P: Fn(&char) -> bool>(
    chars: &mut MultiPeek<I>,
    predicate: P,
) -> usize {
    let mut count = 0;
    while chars
        .peek()
        .filter(|(_pos, char)| predicate(char))
        .is_some()
    {
        let _ = chars.next();
        count += 1;
    }
    chars.reset_peek();
    count
}

fn advance_if<I: Iterator<Item = (usize, char)>>(chars: &mut MultiPeek<I>, test: &char) -> bool {
    let advanced = if chars.peek().filter(|(_pos, char)| char == test).is_some() {
        chars.next();
        true
    } else {
        false
    };
    chars.reset_peek();
    advanced
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_single_line_comments() {
        let code = "// A\n// Comment\nvar x = 42;// XD;\n#";
        let tokens: Vec<_> = lexer(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Var,
                    offset: 16
                }),
                Ok(TokenPos {
                    token: Token::Identifier("x".to_string()),
                    offset: 20
                }),
                Ok(TokenPos {
                    token: Token::Equal,
                    offset: 22
                }),
                Ok(TokenPos {
                    token: Token::Number(42.0),
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
    fn test_multi_line_comments() {
        let code = "/* A\n* multi\n* line\n* comment **//*This is a comment*/ var x = 1.0;";
        let tokens: Vec<_> = lexer(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Var,
                    offset: 55
                }),
                Ok(TokenPos {
                    token: Token::Identifier("x".to_string()),
                    offset: 59
                }),
                Ok(TokenPos {
                    token: Token::Equal,
                    offset: 61
                }),
                Ok(TokenPos {
                    token: Token::Number(1.0),
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
        let tokens: Vec<_> = lexer(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Var,
                    offset: 0
                }),
                Ok(TokenPos {
                    token: Token::Identifier("x".to_string()),
                    offset: 4
                }),
                Ok(TokenPos {
                    token: Token::Equal,
                    offset: 6
                }),
                Ok(TokenPos {
                    token: Token::String("This is a \nmulti\nline\nstring\nliteral".to_string()),
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

        let tokens: Vec<_> = lexer(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Var,
                    offset: 0
                }),
                Ok(TokenPos {
                    token: Token::Identifier("_a_variable".to_string()),
                    offset: 4
                }),
                Ok(TokenPos {
                    token: Token::Equal,
                    offset: 16
                }),
                Ok(TokenPos {
                    token: Token::Number(1.0),
                    offset: 18
                }),
                Ok(TokenPos {
                    token: Token::Plus,
                    offset: 22
                }),
                Ok(TokenPos {
                    token: Token::Number(123.0),
                    offset: 24
                }),
                Ok(TokenPos {
                    token: Token::Dot,
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
