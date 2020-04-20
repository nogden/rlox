use std::{
    fmt,
    str::CharIndices,
    iter,
    ops::RangeInclusive,
};

#[derive(Clone, Copy, PartialEq, Debug)]
enum TokenType<'s> {
    // Single character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Identifier, String(&'s str), Number(f64),

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof,
}

#[derive(Clone, Debug)]
pub struct Token<'s> {
    token_type: TokenType<'s>,
    lexeme: &'s str,
    line: usize,
}

impl<'s> Token<'s> {
    fn new(token_type: TokenType<'s>, lexeme: &'s str, line: usize) -> Token<'s> {
        Token {token_type, lexeme, line}
    }
}

impl<'s> fmt::Display for Token<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {} tbd", self.token_type, self.lexeme)
    }
}

#[derive(Clone, Copy, PartialEq)]
struct SourceCode<'s>(&'s str);

impl<'s> SourceCode<'s> {
    fn tokens(&self) -> Tokens<'s> {
        Tokens {
            source_code: self.0,
            chars: self.0.char_indices().peekable(),
            token_start: 0,
            current_line: 1
        }
    }
}

struct Tokens<'s> {
    source_code: &'s str,
    chars: iter::Peekable<CharIndices<'s>>,
    token_start: usize,
    current_line: usize,
}

impl<'s> Tokens<'s> {
    fn advance(&mut self) {
        let _ = self.chars.next();
    }

    fn found(
        &self, token: TokenType<'s>, location: RangeInclusive<usize>
    ) -> Option<Token<'s>> {
        Some(Token {
            token_type: token,
            lexeme: &self.source_code[location],
            line: self.current_line
        })
    }

    fn followed_by(&mut self, character: char) -> bool {
        match self.chars.peek() {
            Some((_, c)) if c == &character => {
                self.advance();
                true
            },

            _ => false
        }
    }

    fn comment(&mut self) {
        while let Some((_, character)) = self.chars.peek() {
            if character == &'\n' {
                break // Don't consume the newline, it will update current_line
            }
            self.advance();
        }
    }

    fn string(&mut self) -> Option<Token<'s>> {
        use TokenType::String;
        let start_line = self.current_line;

        while let Some((index, character)) = self.chars.peek() {
            let index = *index;
            match character {
                '\n' => self.current_line += 1,
                '"' => {
                    self.advance();
                    let string = &self.source_code[self.token_start..=index];
                    return self.found(String(string), self.token_start..=index);
                },
                _ => { /* Include in string */ }
            }
            self.advance();
        }

        eprintln!("Unterminated string starting on line {}", start_line);
        None
    }

    fn number(&mut self, start_index: usize) -> Option<Token<'s>> {
        use TokenType::Number;
        let mut index = start_index;

        while let Some((i, '0'..='9')) = self.chars.peek() {
            index = *i;
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

            while let Some((i, '0'..='9')) = self.chars.peek() {
                index = *i;
                self.advance();
            }
        }

        let number: f64 = self.source_code[start_index..=index].parse().unwrap();
        self.found(Number(number), start_index..=index)
    }
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Token<'s>;

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
                '"' => {
                    self.token_start = index + 1;  // Don't include the '"'
                    return self.string();
                },
                '0'..='9' => {
                    self.token_start = index;
                    return self.number(index)
                }

                // TODO(nick): Collect errors for future processing.
                _ => eprintln!(
                    "Unexpected character: {} on line {}",
                    character, self.current_line
                )
            };
        }

        None // End of input
    }
}

pub fn tokens<'s>(source: &'s str) -> Vec<Token<'s>> {
    let source_code = SourceCode(source);
    let eof = iter::once(Token {
        token_type: TokenType::Eof,
        lexeme: &source[source.len()..],
        line: source.lines().count(),
    });

    source_code.tokens().chain(eof).collect()
}
