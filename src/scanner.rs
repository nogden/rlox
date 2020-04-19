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
            chars: self.0.char_indices(),
            token_start: 0,
            current_line: 1
        }
    }
}

struct Tokens<'s> {
    source_code: &'s str,
    chars: CharIndices<'s>,
    token_start: usize,
    current_line: usize,
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenType::*;

        while let Some((index, character)) = self.chars.next() {
            match character {
                '{' => return Some(Token {
                    token_type: LeftBrace,
                    lexeme: &self.source_code[index..index+1],
                    line: self.current_line
                }),

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
