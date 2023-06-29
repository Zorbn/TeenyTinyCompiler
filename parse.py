import sys

from lex import *
from environment import *

class Parser:
    def __init__(self, lexer, emitter):
        self.lexer = lexer
        self.emitter = emitter

        self.labels_declared = set()
        self.labels_gotoed = set()

        self.current_token = None
        self.peek_token = None
        # Call next token twice to initialize both current and peek.
        self.next_token()
        self.next_token()

    def check_token(self, kind):
        return kind == self.current_token.kind

    def check_peek(self, kind):
        return kind == self.peek_token.kind

    def match(self, kind):
        if not self.check_token(kind):
            self.abort(f"Expected \"{kind.name}\" got \"{self.current_token.kind.name}\"")
        self.next_token()

    def next_token(self):
        self.current_token = self.peek_token
        self.peek_token = self.lexer.get_token()

    def abort(self, message):
        sys.exit(f"Parsing error! {message}")

    # Return true if the current token is a comparison operator.
    def is_comparison_operator(self):
        return (self.check_token(TokenType.GT) or self.check_token(TokenType.GTEQ) or
                self.check_token(TokenType.LT) or self.check_token(TokenType.LTEQ) or
                self.check_token(TokenType.EQEQ) or self.check_token(TokenType.NOTEQ))

    """
    Rules
    """

    # program ::= {statement}
    def program(self):
        self.emitter.header_line("#define _CRT_SECURE_NO_WARNINGS")
        self.emitter.header_line("#include <stdio.h>")
        self.emitter.header_line("int main(void) {")

        # Skip leading newlines.
        while self.check_token(TokenType.NEWLINE):
            self.next_token()

        environment = Environment(None)

        # A program is composed of a series of statements.
        self.block(TokenType.EOF, environment)

        self.emitter.emit_line("return 0;")
        self.emitter.emit_line("}")

        # Make sure all referenced labels have been declared.
        for label in self.labels_gotoed:
            if label not in self.labels_declared:
                self.abort(f"Attempting to GOTO an undeclared label \"{label}\"")

    def statement(self, environment):
        # "PRINT" (expression | string)
        if self.check_token(TokenType.PRINT):
            self.next_token()

            if self.check_token(TokenType.STRING):
                self.emitter.emit_line(f"printf(\"{self.current_token.text}\\n\");")
                self.next_token()
            else:
                self.emitter.emit("printf(\"%.2f\\n\", (float)(")
                self.expression(environment)
                self.emitter.emit_line("));")

        # "IF" comparison "THEN" newline {statement} "ENDIF" newline
        elif self.check_token(TokenType.IF):
            self.next_token()
            self.emitter.emit("if (")
            self.comparison(environment)

            self.match(TokenType.THEN)
            self.newline()
            self.emitter.emit_line(") {")

            self.block(TokenType.ENDIF, environment)

            self.emitter.emit_line("}")

        # "WHILE" comparision "REPEAT" {statement} "ENDWHILE"
        elif self.check_token(TokenType.WHILE):
            self.next_token()
            self.emitter.emit("while (")
            self.comparison(environment)

            self.match(TokenType.REPEAT)
            self.newline()
            self.emitter.emit_line(") {")

            self.block(TokenType.ENDWHILE, environment)

            self.emitter.emit_line("}")

        # "LABEL" ident
        elif self.check_token(TokenType.LABEL):
            self.next_token()

            # Make sure the label doesn't already exist.
            if self.current_token.text in self.labels_declared:
                self.abort(f"Label already exists: \"{self.current_token.text}\"")
            self.labels_declared.add(self.current_token.text)

            self.emitter.emit_line(f"{self.current_token.text}:")
            self.match(TokenType.IDENT)

        # "GOTO" ident
        elif self.check_token(TokenType.GOTO):
            self.next_token()
            self.labels_gotoed.add(self.current_token.text)
            self.emitter.emit_line(f"goto {self.current_token.text};")
            self.match(TokenType.IDENT)

        # "LET" ident "=" expression
        elif self.check_token(TokenType.LET):
            self.next_token()

            # Make sure the ident exists in the symbol table.
            if not environment.has_symbol(self.current_token.text):
                environment.add_symbol(self.current_token.text)
                # Emit the variables type if it is being declared.
                self.emitter.emit("float ")


            self.emitter.emit(f"{self.current_token.text} = ")
            self.match(TokenType.IDENT)
            self.match(TokenType.EQ)
            self.expression(environment)
            self.emitter.emit_line(";")

        # "INPUT" ident
        elif self.check_token(TokenType.INPUT):
            self.next_token()

            # Make sure the ident exists in the symbol table.
            if not environment.has_symbol(self.current_token.text):
                environment.add_symbol(self.current_token.text)
                self.emitter.header_line(f"float {self.current_token.text};")

            self.emitter.emit_line(f"if (0 == scanf(\"%f\", &{self.current_token.text})) {{")
            self.emitter.emit_line(f"{self.current_token.text} = 0;")
            self.emitter.emit("scanf(\"%")
            self.emitter.emit_line("*s\");")
            self.emitter.emit_line("}")
            self.match(TokenType.IDENT)

        else: # Unknown
            self.abort(f"Invalid statement at \"{self.current_token.text}\" ({self.current_token.kind.name})")

        self.newline()

    # block ::= {statement} terminator
    def block(self, terminator, enclosing_environment):
        environment = Environment(enclosing_environment)

        while not self.check_token(terminator):
            self.statement(environment)

        self.match(terminator)

    # newline ::= '\n'+
    def newline(self):
        self.match(TokenType.NEWLINE)

        while self.check_token(TokenType.NEWLINE):
            self.next_token()

    # comparison ::= expression (("==" | "!=" | ">" | ">=" | "<" | "<=") expression)+
    def comparison(self, environment):
        self.expression(environment)

        if self.is_comparison_operator():
            self.emitter.emit(self.current_token.text)
            self.next_token()
            self.expression(environment)
        else:
            self.abort(f"Expected comparison operator at: \"{self.current_token.text}\"")

        while self.is_comparison_operator():
            self.emitter.emit(self.current_token.text)
            self.next_token()
            self.expression(environment)

    # expression ::= term {( "-" | "+" ) term}
    def expression(self, environment):
        self.term(environment)

        while self.check_token(TokenType.PLUS) or self.check_token(TokenType.MINUS):
            self.emitter.emit(self.current_token.text)
            self.next_token()
            self.term(environment)

    # term ::= unary {( "/" | "*" ) unary}
    def term(self, environment):
        self.unary(environment)

        while self.check_token(TokenType.ASTERISK) or self.check_token(TokenType.SLASH):
            self.emitter.emit(self.current_token.text)
            self.next_token()
            self.unary(environment)

    # unary ::= ["+" | "-"] primary
    def unary(self, environment):
        # Optional unary +/-
        if self.check_token(TokenType.PLUS) or self.check_token(TokenType.MINUS):
            self.emitter.emit(self.current_token.text)
            self.next_token()

        self.primary(environment)

    # primary ::= number | ident
    def primary(self, environment):
        if self.check_token(TokenType.NUMBER):
            self.emitter.emit(self.current_token.text)
            self.next_token()
        elif self.check_token(TokenType.IDENT):
            # Make sure that the variable exists before it is used.
            if not environment.has_symbol(self.current_token.text):
                self.abort(f"Referencing variable before assignment: \"{self.current_token.text}\"")

            self.emitter.emit(self.current_token.text)
            self.next_token()
        else:
            self.abort(f"Unexpected token at \"{self.current_token.text}\"")