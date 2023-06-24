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
                '/' if matches!(chars.peek(), Some((_pos, '/'))) => {
                    advance_while(chars, |&c| c != '\n');
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
                            ret = Ok(Token::String(code[pos..end_pos].to_string()));
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
                _ if char.is_alphanumeric() => {
                    let iden_len = advance_while(chars, |c| c.is_alphanumeric());
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
    fn test_lexer() {
        let code = "var a = 1.0 + 123.;# // XD;";

        let tokens: Vec<_> = lexer(code).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(TokenPos {
                    token: Token::Var,
                    offset: 0
                }),
                Ok(TokenPos {
                    token: Token::Identifier("a".to_string()),
                    offset: 4
                }),
                Ok(TokenPos {
                    token: Token::Equal,
                    offset: 6
                }),
                Ok(TokenPos {
                    token: Token::Number(1.0),
                    offset: 8
                }),
                Ok(TokenPos {
                    token: Token::Plus,
                    offset: 12
                }),
                Ok(TokenPos {
                    token: Token::Number(123.0),
                    offset: 14
                }),
                Ok(TokenPos {
                    token: Token::Dot,
                    offset: 17
                }),
                Ok(TokenPos {
                    token: Token::Semicolon,
                    offset: 18
                }),
                Err(LexerError {
                    line_number: 1,
                    error: Error::UnknownToken('#'),
                }),
                Ok(TokenPos {
                    token: Token::EOF,
                    offset: 27
                }),
            ],
        );
    }
}
