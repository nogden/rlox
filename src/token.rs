use std::fmt;

#[rustfmt::skip]
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenType<'s> {
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

#[derive(Clone, Copy, Debug)]
pub struct Token<'s> {
    pub token_type: TokenType<'s>,
    pub line: usize,
    pub lexeme: &'s str,
}

impl<'s> fmt::Display for Token<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl<'s> TokenType<'s> {
    pub fn symbol(&self) -> &'static str {
        use TokenType::*;

        #[rustfmt::skip]
        match self {
            LeftParen    => "'('",
            RightParen   => "')'",
            LeftBrace    => "'{'",
            RightBrace   => "'}'",
            Comma        => "','",
            Dot          => "'.'",
            Minus        => "'-'",
            Plus         => "'+'",
            Semicolon    => "';'",
            Slash        => "'/'",
            Star         => "'*'",
            Bang         => "'!'",
            BangEqual    => "'!='",
            Equal        => "'='",
            EqualEqual   => "'=='",
            Greater      => "'>'",
            GreaterEqual => "'>='",
            Less         => "'<'",
            LessEqual    => "'<='",
            Identifier   => "identifier",
            String(_)    => "string",
            Number(_)    => "number",
            And          => "'and'",
            Class        => "'class'",
            Else         => "'else'",
            False        => "'false'",
            Fun          => "'fun'",
            For          => "'for'",
            If           => "'if'",
            Nil          => "'nil'",
            Or           => "'or'",
            Print        => "'print'",
            Return       => "'return'",
            Super        => "'super'",
            This         => "'this'",
            True         => "'true'",
            Var          => "'var'",
            While        => "'while'",
            Eof          => "EOF",
        }
    }
}
