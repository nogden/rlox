use std::{
    iter,
    slice::SliceIndex,
    str::CharIndices,
};

use crate::{
    token::{TokenType, Token},
    error::{ParseError, ParseError::*},
};

pub trait Scanner {
    fn tokens(&self) -> Tokens;
}

type NextToken<'s> = Option<Result<Token<'s>, ParseError<'s>>>;

impl Scanner for str {
    fn tokens(&self) -> Tokens {
        Tokens {
            source_code: self,
            chars: self.char_indices().peekable(),
            current_line: 1,
            ended: false
        }
    }
}

#[derive(Clone)]
pub struct Tokens<'s> {
    source_code: &'s str,
    chars: iter::Peekable<CharIndices<'s>>,
    current_line: usize,
    ended: bool,
}

impl<'s> Tokens<'s> {
    fn advance(&mut self) {
        let _ = self.chars.next();
    }

    fn found(
        &self, token: TokenType<'s>, location: impl SliceIndex<str, Output=str>
    ) -> NextToken<'s> {
        Some(Ok(Token {
            token_type: token,
            lexeme: &self.source_code[location],
            line: self.current_line
        }))
    }

    fn followed_by(&mut self, expected: char) -> bool {
        match self.chars.peek() {
            Some((_, character)) if *character == expected => {
                self.advance();
                true
            },

            _ => false
        }
    }

    fn comment(&mut self) {
        while let Some((_, character)) = self.chars.peek() {
            if *character == '\n' {
                break // Don't consume the newline, it will update current_line
            }
            self.advance();
        }
    }

    fn string(
        &mut self, opening_quote: usize
    ) -> Result<Token<'s>, ParseError<'s>> {
        use TokenType::String;
        let start_line = self.current_line;
        let first_char = opening_quote + 1;

        while let Some((index, character)) = self.chars.peek() {
            match character {
                '\n' => self.current_line += 1,
                '"' => {
                    let closing_quote = *index;
                    self.advance();  // Skip the closing '"'
                    let string = if first_char == closing_quote {
                        ""
                    } else {
                        &self.source_code[first_char..closing_quote]
                    };
                    return Ok(Token {
                        token_type: String(string),
                        lexeme: &self.source_code[opening_quote..=closing_quote],
                        line: start_line
                    });
                },
                _ => { /* Include in string */ }
            }
            self.advance()
        }

        Err(UnterminatedString(Token {
            token_type: String(""),
            lexeme: &self.source_code[opening_quote..],
            line: self.current_line
        }))
    }

    fn number(&mut self, first_numeral: usize) -> NextToken<'s> {
        use TokenType::Number;
        let mut last_numeral = first_numeral;

        while let Some((index, '0'..='9')) = self.chars.peek() {
            last_numeral = *index;
            self.advance()
        }

        let mut has_fractional_part = false;

        // We only have 1 character of read-ahead so this dance is needed
        let mut iter = self.chars.clone();
        if let Some((_, '.')) = iter.next() {
            if let Some((_, '0'..='9')) = iter.next() {
                has_fractional_part = true;
            }
        }

        if has_fractional_part {
            self.advance();  // Consume decimal point

            while let Some((index, '0'..='9')) = self.chars.peek() {
                last_numeral = *index;
                self.advance();
            }
        }

        let number: f64 = self.source_code[first_numeral..=last_numeral]
            .parse().unwrap();
        self.found(Number(number), first_numeral..=last_numeral)
    }

    fn identifier(&mut self, first_char: usize) -> NextToken<'s> {
        use TokenType::Identifier;
        let mut last_char = first_char;

        while let Some((index, character)) = self.chars.peek() {
            if character.is_alphanumeric() || *character == '_' {
                last_char = *index;
                self.advance()
            } else {
                break
            }
        }

        let identifier = &self.source_code[first_char..=last_char];
        if let Some(token_type) = self.keyword(identifier) {
            self.found(token_type, first_char..=last_char)
        } else {
            self.found(Identifier(identifier), first_char..=last_char)
        }
    }

    fn keyword(&self, identifier: &str) -> Option<TokenType<'s>> {
        use TokenType::*;

        match identifier {
            "and"    => Some(And),
            "class"  => Some(Class),
            "else"   => Some(Else),
            "false"  => Some(False),
            "fun"    => Some(Fun),
            "for"    => Some(For),
            "if"     => Some(If),
            "nil"    => Some(Nil),
            "or"     => Some(Or),
            "print"  => Some(Print),
            "return" => Some(Return),
            "super"  => Some(Super),
            "this"   => Some(This),
            "true"   => Some(True),
            "var"    => Some(Var),
            "while"  => Some(While),
            _        => None,
        }
    }
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Result<Token<'s>, ParseError<'s>>;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenType::*;

        while let Some((index, character)) = self.chars.next() {
            match character {
                '{' => return self.found(LeftBrace, index..=index),
                '}' => return self.found(RightBrace, index..=index),
                '(' => return self.found(LeftParen, index..=index),
                ')' => return self.found(RightParen, index..=index),
                ',' => return self.found(Comma, index..=index),
                '.' => return self.found(Dot, index..=index),
                '-' => return self.found(Minus, index..=index),
                '+' => return self.found(Plus, index..=index),
                ';' => return self.found(Semicolon, index..=index),
                '*' => return self.found(Star, index..=index),
                '!' => if self.followed_by('=') {
                    return self.found(BangEqual, index..=index)
                } else {
                    return self.found(Bang, index..=index)
                },
                '=' => if self.followed_by('=') {
                    return self.found(EqualEqual, index..=index + 1)
                } else {
                    return self.found(Equal, index..=index)
                },
                '<' => if self.followed_by('=') {
                    return self.found(LessEqual, index..=index + 1)
                } else {
                    return self.found(Less, index..=index)
                },
                '>' => if self.followed_by('=') {
                    return self.found(GreaterEqual, index..=index + 1)
                } else {
                    return self.found(Greater, index..=index)
                },
                '/' => if self.followed_by('/') {
                    self.comment()
                } else {
                    return self.found(Slash, index..=index)
                },
                ' ' | '\r' | '\t' => { /* Ignore whitespace */ },
                '\n' => self.current_line += 1,
                '"' => return Some(self.string(index)),
                '0'..='9' => return self.number(index),
                c if c == '_' || c.is_alphabetic()
                    => return self.identifier(index),

                _ => return Some(Err(UnexpectedCharacter {
                    character: &self.source_code[index..=index],
                    line: self.current_line,
                }))
            };
        }

        // Eof is a fake token that we add it before terminating iteration
        if ! self.ended {
            self.ended = true;
            Some(Ok(Token {
                token_type: Eof,
                lexeme: "EOF",
                line: self.current_line
            }))
        } else {
            None
        }
    }
}
