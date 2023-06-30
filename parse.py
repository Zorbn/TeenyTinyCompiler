import sys

from lex import *
from emit import *
from environment import *

class Parser:
    def __init__(self, lexer, emitter):
        self.lexer = lexer
        self.emitter = emitter

        self.labels_declared = set()
        self.labels_gotoed = set()
        self.functions_declared = set()

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

    # program ::= block
    def program(self):
        self.emitter.set_region(EmitRegion.PREPROCESSOR)
        self.emitter.emit_line("#define _CRT_SECURE_NO_WARNINGS")
        self.emitter.emit_line("#include <stdio.h>")
        self.emitter.set_region(EmitRegion.HEADER)
        self.emitter.emit_line("int main(void) {")
        self.emitter.set_region(EmitRegion.CODE)

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

        # "IF" comparison "THEN" newline block "ENDIF" newline
        elif self.check_token(TokenType.IF):
            self.next_token()
            self.emitter.emit("if (")
            self.comparison(environment)

            self.match(TokenType.THEN)
            self.newline()
            self.emitter.emit_line(") {")

            self.block(TokenType.ENDIF, environment)

            self.emitter.emit_line("}")

        # "WHILE" comparision "REPEAT" block "ENDWHILE"
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
                self.emitter.set_region(EmitRegion.HEADER)
                self.emitter.emit_line(f"float {self.current_token.text};")
                self.emitter.set_region(EmitRegion.CODE)

            self.emitter.emit_line(f"if (0 == scanf(\"%f\", &{self.current_token.text})) {{")
            self.emitter.emit_line(f"{self.current_token.text} = 0;")
            self.emitter.emit("scanf(\"%")
            self.emitter.emit_line("*s\");")
            self.emitter.emit_line("}")
            self.match(TokenType.IDENT)

        # TODO: Add corresponding call statement, make sure when calling
        # that the function exists and has the correct number of arguments.
        # TODO: Add return values.
        # TODO: Make sure function names don't collide with others when C is generated.
        # "FUNCTION" ident parameters "DO" block "ENDFUNCTION"
        elif self.check_token(TokenType.FUNCTION):
            self.next_token()

            # Parse the function prototype into the emit buffer.
            self.emitter.set_region(EmitRegion.BUFFERED)

            # Make sure the function doesn't already exist.
            if self.current_token.text in self.functions_declared:
                self.abort(f"Function already exists: \"{self.current_token.text}\"")
            self.functions_declared.add(self.current_token.text)
            self.emitter.emit(f"void {self.current_token.text}(")
            self.next_token()

            function_environment = Environment(None)

            self.parameters(function_environment)

            self.match(TokenType.DO)
            self.newline()

            # The function prototype has now been parsed, write it into both the
            # function region and the prototype region.
            prototype = self.emitter.get_buffer();
            self.emitter.set_region(EmitRegion.PROTOTYPE)
            self.emitter.emit_line(f"{prototype});")
            self.emitter.set_region(EmitRegion.FUNCTION)
            self.emitter.emit_line(f"{prototype}) {{")
            self.emitter.clear_buffer()

            self.block(TokenType.ENDFUNCTION, function_environment)

            self.emitter.emit_line("}")

            self.emitter.set_region(EmitRegion.CODE)

        else: # Unknown
            self.expression(environment)
            self.emitter.emit_line(";")
            # TODO: This branch used to be an error, self.abort(f"Invalid statement at \"{self.current_token.text}\" ({self.current_token.kind.name})")
            # but now it allows any expression, should that be valid? This change was made to support function calls as statements.

        self.newline()

    # block ::= {statement} terminator
    def block(self, terminator, enclosing_environment):
        environment = Environment(enclosing_environment)

        while not self.check_token(terminator):
            self.statement(environment)

        self.match(terminator) # TODO: Should this happen here or outside block() after it is run?

    # newline ::= '\n'+
    def newline(self):
        self.match(TokenType.NEWLINE)

        while self.check_token(TokenType.NEWLINE):
            self.next_token()

    # parameters ::= "(" ("," ident)* ")"
    def parameters(self, environment):
        self.match(TokenType.LPAREN)

        count = 0
        while not self.check_token(TokenType.RPAREN):
            if count > 0:
                self.match(TokenType.COMMA)
                self.emitter.emit(", ")

            self.emitter.emit(f"float {self.current_token.text}")
            environment.add_symbol(self.current_token.text)

            count += 1
            self.next_token()

        self.match(TokenType.RPAREN)

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

    # unary ::= ["+" | "-"] call
    def unary(self, environment):
        # Optional unary +/-
        if self.check_token(TokenType.PLUS) or self.check_token(TokenType.MINUS):
            self.emitter.emit(self.current_token.text)
            self.next_token()

        self.call(environment)

    # call ::= ident arguments | primary # TODO: This may be incorrect BNF because call checks for parenthesis to choose between the two options.
    def call(self, environment):
        if not self.check_token(TokenType.IDENT) or not self.check_peek(TokenType.LPAREN):
            return self.primary(environment)

        self.emitter.emit(f"{self.current_token.text}(")
        self.next_token()
        self.arguments(environment)
        self.emitter.emit(")")

    # arguments ::= "(" ("," expression)* ")"
    def arguments(self, environment):
        self.match(TokenType.LPAREN)

        count = 0
        while not self.check_token(TokenType.RPAREN):
            if count > 0:
                self.match(TokenType.COMMA)
                self.emitter.emit(", ")

            self.expression(environment)
            count += 1

        self.match(TokenType.RPAREN)

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