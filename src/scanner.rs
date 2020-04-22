use std::{
    fmt, iter,
    slice::SliceIndex,
    str::CharIndices,
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
    Identifier(&'s str), String(&'s str), Number(f64),

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

impl<'s> fmt::Display for Token<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {} tbd", self.token_type, self.lexeme)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct SourceCode<'s>(pub &'s str);

impl<'s> SourceCode<'s> {
    pub fn tokens(&self) -> Tokens<'s> {
        Tokens {
            source_code: self.0,
            chars: self.0.char_indices().peekable(),
            current_line: 1,
            ended: false
        }
    }
}

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

    fn string(&mut self, start_index: usize) -> Option<Token<'s>> {
        use TokenType::String;
        let start_line = self.current_line;
        let mut end_index = start_index;

        while let Some((index, character)) = self.chars.peek() {
            match character {
                '\n' => self.current_line += 1,
                '"' => {
                    self.advance();
                    let string = &self.source_code[start_index..=end_index];
                    return self.found(String(string), start_index..=end_index);
                },
                _ => { /* Include in string */ }
            }
            end_index = *index;
            self.advance()
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

    fn identifier(&mut self, start_index: usize) -> Option<Token<'s>> {
        use TokenType::Identifier;
        let mut index = start_index;

        while let Some((i, c)) = self.chars.peek() {
            if !c.is_alphanumeric() && c != &'_' {
                break
            }
            index = *i;
            self.advance()
        }

        let identifier = &self.source_code[start_index..=index];
        if let Some(token_type) = self.keyword(identifier) {
            self.found(token_type, start_index..=index)
        } else {
            self.found(Identifier(identifier), start_index..=index)
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
                '"' => return self.string(index + 1),  // Don't include the '"'
                '0'..='9' => return self.number(index),
                c if c == '_' || c.is_alphabetic()
                    => return self.identifier(index),

                // TODO(nick): Collect errors for future processing.
                _ => eprintln!(
                    "Unexpected character: {} on line {}",
                    character, self.current_line
                )
            };
        }

        // Eof isn't really a thing, we add it before terminating iteration
        if ! self.ended {
            self.ended = true;
            let buffer_end = self.source_code.len();
            self.found(Eof, buffer_end..buffer_end)
        } else {
            None
        }
    }
}
