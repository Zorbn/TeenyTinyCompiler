use crate::error_reporting::report_error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Eof,
    Newline,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Ident,
    String,
    Comma,
    Colon,
    Period,

    // Keywords
    Print,
    Let,
    Return,

    If,
    Then,
    EndIf,

    While,
    Repeat,
    EndWhile,

    Function,
    Do,
    EndFunction,

    True,
    False,

    Struct,
    Of,
    EndStruct,

    IntLiteral,
    FloatLiteral,

    // Operators
    Eq,
    Plus,
    Minus,
    Asterisk,
    Slash,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

#[derive(Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,
    pub text_start: usize,
    pub text_end: usize,
}

impl Token {
    pub fn new(text_start: usize, text_end: usize, token_type: TokenType) -> Self {
        Self {
            token_type,
            text_start,
            text_end,
        }
    }

    pub fn from_single(text_start: usize, token_type: TokenType) -> Self {
        Self {
            token_type,
            text_start,
            text_end: text_start + 1,
        }
    }
}

// TODO: Rename to keyword_to_token or something?
fn check_if_keyword(token_text: &[u8]) -> Option<TokenType> {
    use TokenType::*;

    match token_text {
        b"print" => Some(Print),
        b"let" => Some(Let),
        b"return" => Some(Return),

        b"if" => Some(If),
        b"then" => Some(Then),
        b"endif" => Some(EndIf),

        b"while" => Some(While),
        b"repeat" => Some(Repeat),
        b"endwhile" => Some(EndWhile),

        b"function" => Some(Function),
        b"do" => Some(Do),
        b"endfunction" => Some(EndFunction),

        b"struct" => Some(Struct),
        b"of" => Some(Of),
        b"endstruct" => Some(EndStruct),

        b"true" => Some(True),
        b"false" => Some(False),

        _ => None,
    }
}

pub struct Lexer {
    source: Vec<u8>,
    current_char: u8,
    current_position: usize,
}

impl Lexer {
    pub fn new(source: Vec<u8>) -> Self {
        let mut lexer = Self {
            source,
            current_char: 0,
            current_position: 0,
        };

        lexer.reset_position();

        lexer
    }

    pub fn reset_position(&mut self) {
        self.current_char = 0;
        self.current_position = 0;
        self.next_char();
        self.current_position = 0;
    }

    pub fn get_token(&mut self) -> Token {
        let token = self.get_current_token();
        self.next_char();
        token
    }

    pub fn get_source(&self) -> &Vec<u8> {
        &self.source
    }

    fn next_char(&mut self) {
        self.current_position += 1;
        self.current_char = if self.current_position >= self.source.len() {
            b'\0'
        } else {
            self.source[self.current_position]
        };
    }

    fn peek(&self) -> u8 {
        if self.current_position + 1 >= self.source.len() {
            return b'\0';
        }

        self.source[self.current_position + 1]
    }

    fn abort(&self, message: &str) {
        report_error(&self.source, message, self.current_position);
    }

    fn skip_whitespace(&mut self) {
        while let b' ' | b'\t' | b'\r' = self.current_char {
            self.next_char();
        }
    }

    fn skip_comment(&mut self) {
        if self.current_char != b'#' {
            return;
        }

        while self.current_char != b'\n' {
            self.next_char();
        }
    }

    fn get_current_token(&mut self) -> Token {
        use TokenType::*;

        self.skip_whitespace();
        self.skip_comment();

        match self.current_char {
            b'+' => return Token::from_single(self.current_position, Plus),
            b'-' => return Token::from_single(self.current_position, Minus),
            b'*' => return Token::from_single(self.current_position, Asterisk),
            b'/' => return Token::from_single(self.current_position, Slash),
            b'\n' => return Token::from_single(self.current_position, Newline),
            b'(' => return Token::from_single(self.current_position, LParen),
            b')' => return Token::from_single(self.current_position, RParen),
            b'{' => return Token::from_single(self.current_position, LBrace),
            b'}' => return Token::from_single(self.current_position, RBrace),
            b'[' => return Token::from_single(self.current_position, LBracket),
            b']' => return Token::from_single(self.current_position, RBracket),
            b',' => return Token::from_single(self.current_position, Comma),
            b':' => return Token::from_single(self.current_position, Colon),
            b'.' => return Token::from_single(self.current_position, Period),
            b'\0' => return Token::from_single(self.current_position, Eof),
            _ => {}
        }

        if self.current_char == b'=' {
            if self.peek() == b'=' {
                let last_position = self.current_position;
                self.next_char();
                return Token::new(last_position, self.current_position + 1, EqEq);
            } else {
                return Token::from_single(self.current_position, Eq);
            }
        }

        if self.current_char == b'>' {
            if self.peek() == b'=' {
                let last_position = self.current_position;
                self.next_char();
                return Token::new(last_position, self.current_position + 1, GtEq);
            } else {
                return Token::from_single(self.current_position, Gt);
            }
        }

        if self.current_char == b'<' {
            if self.peek() == b'=' {
                let last_position = self.current_position;
                self.next_char();
                return Token::new(last_position, self.current_position + 1, LtEq);
            } else {
                return Token::from_single(self.current_position, Lt);
            }
        }

        if self.current_char == b'!' {
            if self.peek() == b'=' {
                let last_position = self.current_position;
                self.next_char();
                return Token::new(last_position, self.current_position + 1, NotEq);
            } else {
                self.abort(&format!(
                    "Expected !=, got !{}",
                    char::from_u32(self.peek() as u32)
                        .expect("Unknown token is not a valid character")
                ));
            }
        }

        if self.current_char == b'"' {
            // Get characters inside the quotation marks.
            self.next_char();
            let start_position = self.current_position;

            while self.current_char != b'"' {
                // Don't allow special characters inside the string, this is to make it easier to
                // transpile string to C strings. TODO: This language could support escape codes, or
                // automatically escape these characters when transpiling them to C.
                if let b'\r' | b'\n' | b'\t' | b'\\' | b'%' = self.current_char {
                    self.abort("Illegal character in string.");
                }

                self.next_char();
            }

            return Token::new(start_position, self.current_position, String);
        }

        if self.current_char.is_ascii_digit() {
            let start_position = self.current_position;

            while self.peek().is_ascii_digit() {
                self.next_char();
            }

            let token_type = if self.peek() == b'.' {
                // This number has a decimal.
                self.next_char();

                // Decimal numbers must have at least one digit after the decimal.
                if !self.peek().is_ascii_digit() {
                    self.abort("Illegal character in number.");
                }

                while self.peek().is_ascii_digit() {
                    self.next_char();
                }

                FloatLiteral
            } else {
                IntLiteral
            };

            return Token::new(start_position, self.current_position + 1, token_type);
        }

        if self.current_char.is_ascii_alphabetic() {
            let start_position = self.current_position;
            while self.peek().is_ascii_alphanumeric() {
                self.next_char();
            }

            let token_type =
                match check_if_keyword(&self.source[start_position..self.current_position + 1]) {
                    Some(token_type) => token_type,
                    None => Ident,
                };

            return Token::new(start_position, self.current_position + 1, token_type);
        }

        self.abort(&format!(
            "Unknown token \"{}\"",
            // TODO: Don't crash here.
            char::from_u32(self.current_char as u32)
                .expect("Unknown token is not a valid character")
        ));

        unreachable!()
    }
}
