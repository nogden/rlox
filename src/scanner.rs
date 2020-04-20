use std::{
    fmt,
    str::CharIndices,
    iter,
};

#[derive(Clone, Copy, PartialEq, Debug)]
enum TokenType {
    // Single character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Identifier, String, Number,

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof,
}

#[derive(Clone, Debug)]
pub struct Token<'s> {
    token_type: TokenType,
    lexeme: &'s str,
    line: usize,
}

impl<'s> Token<'s> {
    fn new(token_type: TokenType, lexeme: &'s str, line: usize) -> Token<'s> {
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
        &self, token: TokenType, index: usize, length: usize
    ) -> Option<Token<'s>> {
        Some(Token {
            token_type: token,
            lexeme: &self.source_code[index..index+length],
            line: self.current_line
        })
    }

    fn followed_by(&mut self, character: char) -> bool {
        match self.chars.peek() {
            Some((_i, c)) if c == &character => {
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
        let start_line = self.current_line;

        while let Some((index, character)) = self.chars.peek() {
            let index = *index;
            match character {
                '\n' => self.current_line += 1,
                '"' => {
                    self.advance();
                    let string_length = index - self.token_start;
                    return self.found(
                        TokenType::String, self.token_start, string_length
                    );
                },
                _ => { /* Include in string */ }
            }
            self.advance();
        }

        eprintln!("Unterminated string starting on line {}", start_line);
        None
    }
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenType::*;

        while let Some((index, character)) = self.chars.next() {
            match character {
                '{' => return self.found(LeftBrace, index, 1),
                '}' => return self.found(RightBrace, index, 1),
                '(' => return self.found(LeftParen, index, 1),
                ')' => return self.found(RightParen, index, 1),
                ',' => return self.found(Comma, index, 1),
                '.' => return self.found(Dot, index, 1),
                '-' => return self.found(Minus, index, 1),
                '+' => return self.found(Plus, index, 1),
                ';' => return self.found(Semicolon, index, 1),
                '*' => return self.found(Star, index, 1),
                '!' => if self.followed_by('=') {
                    return self.found(BangEqual, index, 2)
                } else {
                    return self.found(Bang, index, 1)
                },
                '=' => if self.followed_by('=') {
                    return self.found(EqualEqual, index, 2)
                } else {
                    return self.found(Equal, index, 1)
                },
                '<' => if self.followed_by('=') {
                    return self.found(LessEqual, index, 2)
                } else {
                    return self.found(Less, index, 1)
                },
                '>' => if self.followed_by('=') {
                    return self.found(GreaterEqual, index, 2)
                } else {
                    return self.found(Greater, index, 1)
                },
                '/' => if self.followed_by('/') {
                    self.comment()
                } else {
                    return self.found(Slash, index, 1)
                },
                ' ' | '\r' | '\t' => { /* Ignore whitespace */ },
                '\n' => self.current_line += 1,
                '"' => {
                    self.token_start = index + 1;  // Don't include the '"'
                    return self.string();
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
