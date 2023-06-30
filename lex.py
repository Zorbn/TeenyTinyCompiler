import enum
import sys

class TokenType(enum.Enum):
    EOF = -1
    NEWLINE = 0
    LPAREN = 1
    RPAREN = 2
    NUMBER = 3
    IDENT = 4
    STRING = 5
    COMMA = 6
    COLON = 7

    # Keywords:
    LABEL = 101
    GOTO = 102
    PRINT = 103
    INPUT = 104
    LET = 105
    RETURN = 106

    IF = 107
    THEN = 108
    ENDIF = 109

    WHILE = 110
    REPEAT = 111
    ENDWHILE = 112

    FUNCTION = 113
    DO = 114
    ENDFUNCTION = 115

    # Operators:
    EQ = 201
    PLUS = 202
    MINUS = 203
    ASTERISK = 204
    SLASH = 205
    EQEQ = 206
    NOTEQ = 207
    LT = 208
    LTEQ = 209
    GT = 210
    GTEQ = 211

    # Types:
    INT = 301
    FLOAT = 302

class Token:
    def __init__(self, token_text, token_kind) -> None:
        self.text = token_text
        self.kind = token_kind

    @staticmethod
    def check_if_keyword(token_text):
        for kind in TokenType:
            # Relies on keyword enums having an integer value in the 1XX range.
            if kind.name == token_text and kind.value >= 100 and kind.value < 200:
                return kind

        return None

class Lexer:
    def __init__(self, source):
        self.source = source + "\n"
        self.current_char = ""
        self.current_position = -1
        self.next_char()

    def next_char(self):
        self.current_position += 1
        if self.current_position >= len(self.source):
            self.current_char = "\0" # EOF
        else:
            self.current_char = self.source[self.current_position]

    def peek(self):
        if self.current_position + 1 >= len(self.source):
            return "\0"
        return self.source[self.current_position + 1]

    def abort(self, message):
        sys.exit(f"Lexing error! {message}")

    def skip_whitespace(self):
        while self.current_char == " " or self.current_char == "\t" or self.current_char == "\r":
            self.next_char()

    def skip_comment(self):
        if self.current_char != '#':
            return

        while self.current_char != '\n':
            self.next_char()

    def get_token(self):
        self.skip_whitespace()
        self.skip_comment()

        token = None

        if self.current_char == "+": # Plus
            token = Token(self.current_char, TokenType.PLUS)
        elif self.current_char == "-": # Minus
            token = Token(self.current_char, TokenType.MINUS)
        elif self.current_char == "*": # Asterisk
            token = Token(self.current_char, TokenType.ASTERISK)
        elif self.current_char == "/": # Slash
            token = Token(self.current_char, TokenType.SLASH)
        elif self.current_char == "\n": # Newline
            token = Token(self.current_char, TokenType.NEWLINE)
        elif self.current_char == "(": # LParen
            token = Token(self.current_char, TokenType.LPAREN)
        elif self.current_char == ")": # RParen
            token = Token(self.current_char, TokenType.RPAREN)
        elif self.current_char == ",": # Comma
            token = Token(self.current_char, TokenType.COMMA)
        elif self.current_char == "\0": # EOF
            token = Token(self.current_char, TokenType.EOF)

        elif self.current_char == "=":
            if self.peek() == "=": # ==
                last_char = self.current_char
                self.next_char()
                token = Token(last_char + self.current_char, TokenType.EQEQ)
            else: # =
                token = Token(self.current_char, TokenType.EQ)

        elif self.current_char == ">":
            if self.peek() == "=": # >=
                last_char = self.current_char
                self.next_char()
                token = Token(last_char + self.current_char, TokenType.GTEQ)
            else: # >
                token = Token(self.current_char, TokenType.GT)

        elif self.current_char == "<":
            if self.peek() == "=": # <=
                last_char = self.current_char
                self.next_char()
                token = Token(last_char + self.current_char, TokenType.LTEQ)
            else: # <
                token = Token(self.current_char, TokenType.LT)

        elif self.current_char == "!":
            if self.peek() == "=": # !=
                last_char = self.current_char
                self.next_char()
                token = Token(last_char + self.current_char, TokenType.NOTEQ)
            else: # !
                self.abort(f"Expected !=, got !{self.peek()}")

        elif self.current_char == "\"": # String
            #  Get characters between the quotation marks.
            self.next_char()
            start_position = self.current_position

            while self.current_char != '\"':
                # Don't allow special characters inside the string, this is to make it easier to
                # transpile string to C strings. TODO: This language could support escape codes, or
                # automatically escape these characters when transpiling them to C.
                if (self.current_char == '\r' or self.current_char == '\n' or
                    self.current_char == '\t' or self.current_char == '\\' or
                    self.current_char == '%'):
                    self.abort("Illegal character in string.")

                self.next_char()

            token_text = self.source[start_position : self.current_position]
            token = Token(token_text, TokenType.STRING)

        elif self.current_char.isdigit(): # Number
            start_position = self.current_position
            while self.peek().isdigit():
                self.next_char()
            if self.peek() == ".": # This number has a decimal.
                self.next_char()

                # Decimal number must have at least one digit after the decimal.
                if not self.peek().isdigit():
                    self.abort("Illegal character in number.")

                while self.peek().isdigit():
                    self.next_char()

            token_text = self.source[start_position : self.current_position + 1]
            token = Token(token_text, TokenType.NUMBER)

        elif self.current_char.isalpha():
            start_position = self.current_position
            while self.peek().isalnum():
                self.next_char()

            token_text = self.source[start_position : self.current_position + 1]
            keyword = Token.check_if_keyword(token_text)
            if keyword == None: # Identifier
                token = Token(token_text, TokenType.IDENT)
            else: # Keyword
                token = Token(token_text, keyword)

        else: # Unkown
            self.abort(f"Unknown token: \"{self.current_char}\"")

        self.next_char()
        return token