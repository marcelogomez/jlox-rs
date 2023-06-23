use std::iter::Peekable;
use std::str::FromStr;

use crate::token::Token;
use crate::token::TokenPos;

#[derive(Debug, PartialEq)]
pub enum Error {
    MalformedString,
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
            '!' => Some(Ok(if advance_if(&mut chars, &'=') {
                Token::BangEqual
            } else {
                Token::Bang
            })),
            '=' => Some(Ok(if advance_if(&mut chars, &'=') {
                Token::EqualEqual
            } else {
                Token::Equal
            })),
            '<' => Some(Ok(if advance_if(&mut chars, &'=') {
                Token::LessEqual
            } else {
                Token::Less
            })),
            '>' => Some(Ok(if advance_if(&mut chars, &'=') {
                Token::GreaterEqual
            } else {
                Token::Greater
            })),
            // We're inside a comment, advance the cursor until the end of the line
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
            ' ' | '\r' | '\t' => None, // no-op
            '\n' => {
                line_number += 1;
                None
            }
            // Parse a number literal
            _ if chars
                .peek()
                .filter(|(_pos, c)| c.is_ascii_digit())
                .is_some() =>
            {
                // consume decimal part
                let _ = advance_while(&mut chars, char::is_ascii_digit);

                // check if there's a fractional part
                let (has_period, fractional_part_len) = if let Some((_pos, '.')) = chars.peek() {
                    // Consume the period
                    let _ = chars.next();
                    (true, advance_while(&mut chars, char::is_ascii_digit))
                } else {
                    (false, 0)
                };

                let end_pos = chars.peek().map(|(pos, _char)| *pos).unwrap_or(code.len());
                // TODO: Handle error
                let val = f64::from_str(&code[pos..end_pos]).unwrap();

                tokens.push(Ok(TokenPos {
                    token: Token::Number(val),
                    offset: pos,
                }));

                // If there was no fractional part but there was a period,
                // then we must interpret that as a dot
                if has_period && fractional_part_len == 0 {
                    tokens.push(Ok(TokenPos {
                        token: Token::Dot,
                        offset: pos,
                    }));
                }

                // TODO: Figure out a less hacky way of doing this
                None
            }
            // Identifiers or keywords
            _ if chars
                .peek()
                .filter(|(_pos, char)| char.is_alphanumeric())
                .is_some() =>
            {
                let iden_len = advance_while(&mut chars, |c| c.is_alphanumeric());
                let iden_val = code[pos..pos + iden_len].to_string();
                // TODO: Check if this is a keyword
                Some(Ok(Token::Identifier(iden_val)))
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
}
