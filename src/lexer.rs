use std::iter::Peekable;
use std::str::FromStr;

use crate::token::get_keyword;
use crate::token::Token;
use crate::token::TokenPos;

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

pub fn lexer(code: String) -> Vec<LexerResult<TokenPos>> {
    let mut line_number = 1;
    let mut tokens: Vec<LexerResult<TokenPos>> = vec![];

    let mut chars = code.char_indices().peekable();
    while let Some((pos, char)) = chars.next() {
        let next_token = match char {
            // single character tokens
            '(' => Some(Ok(Token::LeftParen)),
            ')' => Some(Ok(Token::RightParen)),
            '{' => Some(Ok(Token::LeftBrace)),
            '}' => Some(Ok(Token::RightBrace)),
            ';' => Some(Ok(Token::Semicolon)),
            ',' => Some(Ok(Token::Comma)),
            '.' => Some(Ok(Token::Dot)),
            '-' => Some(Ok(Token::Minus)),
            '+' => Some(Ok(Token::Plus)),
            '*' => Some(Ok(Token::Star)),
            // two character tokens
            '!' if advance_if(&mut chars, &'=') => Some(Ok(Token::BangEqual)),
            '!' => Some(Ok(Token::Bang)),
            '=' if advance_if(&mut chars, &'=') => Some(Ok(Token::EqualEqual)),
            '=' => Some(Ok(Token::Equal)),
            '<' if advance_if(&mut chars, &'=') => Some(Ok(Token::LessEqual)),
            '<' => Some(Ok(Token::Less)),
            '>' if advance_if(&mut chars, &'=') => Some(Ok(Token::Greater)),
            '>' => Some(Ok(Token::Greater)),
            ' ' | '\r' | '\t' => None, // whitespace is no-op
            '\n' => {
                line_number += 1;
                None
            }
            // Comments or division
            '/' if matches!(chars.peek(), Some((_pos, '/'))) => {
                advance_while(&mut chars, |&c| c != '\n');
                None
            }
            '/' => Some(Ok(Token::Slash)),
            '"' => {
                let mut ret = Some(Err(LexerError {
                    line_number,
                    error: Error::MalformedString,
                }));
                while let Some((end_pos, c)) = chars.peek().copied() {
                    let _ = chars.next();
                    // We support multi line strings
                    if c == '\n' {
                        line_number += 1;
                    }
                    if c == '"' {
                        ret = Some(Ok(Token::String(code[pos..end_pos].to_string())));
                        break;
                    }
                }
                ret
            }
            // Parse a number literal
            _ if char.is_ascii_digit() => {
                // consume decimal part
                let decimal_part_len = 1 + advance_while(&mut chars, char::is_ascii_digit);

                // check if there's a fractional part
                let (has_period, fractional_part_len) = if let Some((_pos, '.')) = chars.peek() {
                    // Consume the period
                    let _ = chars.next();
                    (true, advance_while(&mut chars, char::is_ascii_digit))
                } else {
                    (false, 0)
                };

                let end_pos = chars.peek().map(|(pos, _char)| *pos).unwrap_or(code.len());

                tokens.push(match f64::from_str(&code[pos..end_pos]) {
                    Ok(val) => Ok(TokenPos {
                        token: Token::Number(val),
                        offset: pos,
                    }),
                    Err(_) => Err(LexerError {
                        line_number,
                        error: Error::MalformedNumber,
                    }),
                });

                // If there was no fractional part but there was a period,
                // then we must interpret that as a dot
                if has_period && fractional_part_len == 0 {
                    tokens.push(Ok(TokenPos {
                        token: Token::Dot,
                        offset: pos + decimal_part_len,
                    }));
                }

                // TODO: Figure out a less hacky way of doing this
                None
            }
            // Identifiers or keywords
            _ if char.is_alphanumeric() => {
                let iden_len = advance_while(&mut chars, |c| c.is_alphanumeric());
                let iden_val = &code[pos..pos + iden_len + 1];
                Some(Ok(get_keyword(iden_val)
                    .unwrap_or_else(|| Token::Identifier(iden_val.to_string()))))
            }
            _ => Some(Err(LexerError {
                line_number,
                error: Error::UnknownToken(char),
            })),
        };

        if let Some(token_result) = next_token {
            tokens.push(token_result.map(|token| TokenPos { token, offset: pos }));
        }
    }

    tokens.push(Ok(TokenPos {
        token: Token::EOF,
        offset: code.len(),
    }));

    tokens
}

fn advance_while<I: Iterator<Item = (usize, char)>, P: Fn(&char) -> bool>(
    chars: &mut Peekable<I>,
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
    count
}

fn advance_if<I: Iterator<Item = (usize, char)>>(chars: &mut Peekable<I>, test: &char) -> bool {
    if chars.peek().filter(|(_pos, char)| char == test).is_some() {
        chars.next();
        true
    } else {
        false
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_lexer() {
        let code = "var a = 1.0 + 123.;# // XD;".to_string();

        let tokens = lexer(code);

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
