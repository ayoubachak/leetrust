// scanner.rs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    // One or two character tokens
    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual, Less, LessEqual,
    // Literals
    Identifier, String, Number,
    // String Interpolation
    InterpolationStart,
    InterpolationEnd,
    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    // Error and EOF
    Error, Eof
}

#[derive(Clone, Copy, Debug)]
pub struct Token<'a> {
    pub type_: TokenType,
    pub start: &'a str,
    pub length: usize,
    pub line: usize,
}

impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Token {
            type_: TokenType::Eof,  // Or any other default type
            start: "",
            length: 0,
            line: 0,
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { source, start: 0, current: 0, line: 1 }
    }

    fn advance(&mut self) -> char {
        let c = self.source.as_bytes()[self.current] as char;
        self.current += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.as_bytes()[self.current] as char
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.as_bytes()[self.current + 1] as char
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn is_alpha(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.as_bytes()[self.current] as char != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => { self.advance(); },
                '\n' => { self.line += 1; self.advance(); },
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        break;
                    }
                },
                _ => break,
            }
        }
    }

    fn check_keyword(&self, start: usize, length: usize, rest: &str, type_: TokenType) -> TokenType {
        if self.current - self.start == start + length && &self.source[self.start + start..self.start + start + length] == rest {
            type_
        } else {
            TokenType::Identifier
        }
    }

    fn identifier_type(&self) -> TokenType {
        match self.source.as_bytes()[self.start] as char {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' => if self.current - self.start > 1 {
                match self.source.as_bytes()[self.start + 1] as char {
                    'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                    'o' => self.check_keyword(2, 1, "r", TokenType::For),
                    'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                    _ => TokenType::Identifier,
                }
            } else {
                TokenType::Identifier
            },
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => if self.current - self.start > 1 {
                match self.source.as_bytes()[self.start + 1] as char {
                    'h' => self.check_keyword(2, 2, "is", TokenType::This),
                    'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                    _ => TokenType::Identifier,
                }
            } else {
                TokenType::Identifier
            },
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        // Here we would use identifierType() to determine if it is a keyword or just an identifier
        self.make_token(self.identifier_type())
    }

    fn number(&mut self) -> Token<'a> {
        while self.peek().is_digit(10) {
            self.advance();
        }

        // Handle decimals
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();  // consume the '.'
            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token<'a> {
        let start = self.current;
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
    
        // Ensure string is terminated correctly
        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }
    
        // Create a string slice from the start to the current position minus the quote
        let token = Token {
            type_: TokenType::String,
            start: &self.source[start..self.current],
            length: self.current - start,
            line: self.line,
        };
    
        self.advance();  // Skip the closing quote.
        token
    }

    fn make_token(&self, type_: TokenType) -> Token<'a> {
        Token {
            type_,
            start: &self.source[self.start..self.current],
            length: self.current - self.start,
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token<'a> {
        Token {
            type_: TokenType::Error,
            start: message,
            length: message.len(),
            line: self.line,
        }
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;
    
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }
    
        let c = self.advance();
        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            ';' => self.make_token(TokenType::Semicolon),
            '*' => self.make_token(TokenType::Star),
            '!' => if self.match_char('=') { self.make_token(TokenType::BangEqual) } else { self.make_token(TokenType::Bang) },
            '=' => if self.match_char('=') { self.make_token(TokenType::EqualEqual) } else { self.make_token(TokenType::Equal) },
            '<' => if self.match_char('=') { self.make_token(TokenType::LessEqual) } else { self.make_token(TokenType::Less) },
            '>' => if self.match_char('=') { self.make_token(TokenType::GreaterEqual) } else { self.make_token(TokenType::Greater) },
            '/' => {
                if self.match_char('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    self.scan_token() // Ignore the comment and scan the next token
                } else {
                    self.make_token(TokenType::Slash)
                }
            },
            '"' => self.string(),
            _ => if c.is_digit(10) {
                self.number()
            } else if c.is_alphabetic() || c == '_' {
                self.identifier()
            } else {
                self.error_token("Unexpected character.")
            },
        }
    }

    

}
