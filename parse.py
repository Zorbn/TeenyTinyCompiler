import sys
from dataclasses import dataclass

from lex import *
from emit import *
from environment import *

# TODO: Unify Kind/Type in variable names
class ValueType(enum.Enum):
    INT = 0
    FLOAT = 1

    @staticmethod
    def from_token_type(token_kind):
        if token_kind == TokenType.INT:
            return ValueType.INT
        elif token_kind == TokenType.FLOAT:
            return ValueType.FLOAT

        return None

    @staticmethod
    def to_c_type(value_kind):
        if value_kind == ValueType.INT:
            return "int"
        elif value_kind == ValueType.FLOAT:
            return "float"

        return None

@dataclass
class FunctionDeclaration:
    return_type: ValueType
    parameter_types: list[ValueType]

class Parser:
    def __init__(self, lexer, emitter):
        self.lexer = lexer
        self.emitter = emitter

        self.labels_declared = set()
        self.labels_gotoed = set()
        self.functions_declared = {}
        self.functions_called = set()

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

    def combine_type(self, old_kind, new_kind):
        if old_kind is None:
            return new_kind
        elif new_kind is not None and old_kind != new_kind:
            self.abort(f"Cannot mix types, expected {old_kind} got {new_kind}")

        return old_kind

    def token_to_types(self):
        value_type = ValueType.from_token_type(self.current_token.kind)
        if value_type is None:
            self.abort(f"Expected a type, but got \"{self.current_token.text}\"")
        return (value_type, ValueType.to_c_type(value_type))

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

        # Make sure all called functions have been declared.
        # TODO: This broke when adding types, fix it.
        # for (name, declaration) in self.functions_called:
        #     if name not in self.functions_declared:
        #         self.abort(f"Calling undefined function: \"{name}\"")

        #     parameter_count = self.functions_declared[name]
        #     if argument_count != parameter_count:
        #         self.abort(f"Calling function \"{self.current_token.text}\" with incorrect number of arguments: expected {parameter_count} but got {argument_count}")


    # TODO: (Ongoing) Keep track of if blocks return a value: https://stackoverflow.com/questions/21945891/how-do-i-check-whether-all-code-paths-return-a-value
    # TODO: All blocks should be able to return a value, which will make more sense once all code must be in a function. (one the frontend, that's already true in the backend)
    def statement(self, environment):
        return_kind = None

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

        # "LET" ident ":" type "=" expression
        elif self.check_token(TokenType.LET):
            self.next_token()

            variable_name = self.current_token.text
            # Make sure the ident doesn't exist in the symbol table.
            if environment.has_symbol(variable_name):
                self.abort(f"Variable already exists: \"{variable_name}\"")
            self.next_token()

            self.match(TokenType.COLON)
            (value_type, c_type) = self.token_to_types()
            environment.add_symbol(variable_name, value_type)
            self.next_token()

            self.emitter.emit(f"{c_type} {variable_name} = ")
            self.match(TokenType.EQ)
            self.expression(environment)
            self.emitter.emit_line(";")

        # ident "=" expression
        elif self.check_token(TokenType.IDENT) and self.check_peek(TokenType.EQ):
            # Make sure the ident exists in the symbol table.
            if not environment.has_symbol(self.current_token.text):
                self.abort(f"Variable doesn't exist: \"{self.current_token.text}\"")

            self.emitter.emit(f"{self.current_token.text} = ")
            self.match(TokenType.IDENT)
            self.match(TokenType.EQ)
            self.expression(environment)
            self.emitter.emit_line(";")

        # TODO: This is weird in the way it creates a variable, and doesn't mesh well with the language, remove it when it can be replaced by the stdlib.
        # "INPUT" ident
        elif self.check_token(TokenType.INPUT):
            self.next_token()

            # Make sure the ident exists in the symbol table.
            if not environment.has_symbol(self.current_token.text):
                environment.add_symbol(self.current_token.text, ValueType.FLOAT)
                self.emitter.set_region(EmitRegion.HEADER)
                self.emitter.emit_line(f"float {self.current_token.text};")
                self.emitter.set_region(EmitRegion.CODE)

            self.emitter.emit_line(f"if (0 == scanf(\"%f\", &{self.current_token.text})) {{")
            self.emitter.emit_line(f"{self.current_token.text} = 0;")
            self.emitter.emit("scanf(\"%")
            self.emitter.emit_line("*s\");")
            self.emitter.emit_line("}")
            self.match(TokenType.IDENT)

        # TODO: Make sure return type is correct.
        # TODO: Make sure function names don't collide with others when C is generated. Maybe don't include C headers in the same file as the output, put all of the C code in a seperate file and then just import it's header in the main output code?
        # "FUNCTION" ident parameters ":" type "DO" block "ENDFUNCTION"
        elif self.check_token(TokenType.FUNCTION):
            self.next_token()

            # Parse the function prototype into the emit buffer.
            self.emitter.set_region(EmitRegion.BUFFERED)

            # Make sure the function doesn't already exist.
            function_name = self.current_token.text
            if function_name in self.functions_declared:
                self.abort(f"Function already exists: \"{function_name}\"")
            self.emitter.emit(f"{function_name}(")
            self.next_token()

            function_environment = Environment(None)

            parameter_types = self.parameters(function_environment)

            self.match(TokenType.COLON)
            (value_type, c_type) = self.token_to_types()
            self.functions_declared[function_name] = FunctionDeclaration(value_type, parameter_types)
            self.next_token()
            self.match(TokenType.DO)
            self.newline()

            # The function prototype has now been parsed, write it into both the
            # function region and the prototype region.
            prototype = self.emitter.get_buffer();
            self.emitter.set_region(EmitRegion.PROTOTYPE)
            self.emitter.emit_line(f"{c_type} {prototype});")
            self.emitter.set_region(EmitRegion.FUNCTION)
            self.emitter.emit_line(f"{c_type} {prototype}) {{")
            self.emitter.clear_buffer()

            return_kind = self.block(TokenType.ENDFUNCTION, function_environment)
            if return_kind is None:
                self.abort(f"Function does not return a value on all code paths")

            self.emitter.emit_line("}")

            self.emitter.set_region(EmitRegion.CODE)

        elif self.check_token(TokenType.RETURN):
            self.next_token()
            self.emitter.emit("return ")
            expression_kind = self.expression(environment)
            self.emitter.emit_line(";")
            return_kind = expression_kind

        else: # Unknown
            self.expression(environment)
            self.emitter.emit_line(";")
            # TODO: This branch used to be an error, self.abort(f"Invalid statement at \"{self.current_token.text}\" ({self.current_token.kind.name})")
            # but now it allows any expression, should that be valid? This change was made to support function calls as statements.

        self.newline()
        return return_kind

    # block ::= {statement} terminator
    def block(self, terminator, enclosing_environment):
        environment = Environment(enclosing_environment)

        return_kind = None

        while not self.check_token(terminator):
            statement_return_kind = self.statement(environment)
            return_kind = self.combine_type(return_kind, statement_return_kind)

        self.match(terminator) # TODO: Should this happen here or outside block() after it is run?
        return return_kind

    # newline ::= '\n'+
    def newline(self):
        self.match(TokenType.NEWLINE)

        while self.check_token(TokenType.NEWLINE):
            self.next_token()

    # parameters ::= "(" ("," ident ":" type)* ")"
    def parameters(self, environment):
        self.match(TokenType.LPAREN)

        parameter_types = []
        while not self.check_token(TokenType.RPAREN):
            if len(parameter_types) > 0:
                self.match(TokenType.COMMA)
                self.emitter.emit(", ")

            parameter_name = self.current_token.text
            self.next_token()
            self.match(TokenType.COLON)
            (value_type, c_type) = self.token_to_types()
            environment.add_symbol(parameter_name, value_type)
            self.next_token()

            self.emitter.emit(f"{c_type} {parameter_name}")

            parameter_types.append(value_type)

        self.match(TokenType.RPAREN)
        return parameter_types

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
        return count

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

    # TODO: Check expression types
    # expression ::= term {( "-" | "+" ) term}
    def expression(self, environment):
        kind = self.term(environment)

        while self.check_token(TokenType.PLUS) or self.check_token(TokenType.MINUS):
            self.emitter.emit(self.current_token.text)
            self.next_token()
            term_kind = self.term(environment)
            kind = self.combine_type(kind, term_kind)

        return kind

    # term ::= unary {( "/" | "*" ) unary}
    def term(self, environment):
        kind = self.unary(environment)

        while self.check_token(TokenType.ASTERISK) or self.check_token(TokenType.SLASH):
            self.emitter.emit(self.current_token.text)
            self.next_token()
            unary_kind = self.unary(environment)
            kind = self.combine_type(kind, unary_kind)

        return kind

    # unary ::= ["+" | "-"] call
    def unary(self, environment):
        # Optional unary +/-
        if self.check_token(TokenType.PLUS) or self.check_token(TokenType.MINUS):
            self.emitter.emit(self.current_token.text)
            self.next_token()

        return self.call(environment)

    # TODO: Make sure function arguments are the correct types.
    # call ::= ident arguments | primary # TODO: This may be incorrect BNF because call checks for parenthesis to choose between the two options.
    def call(self, environment):
        if not self.check_token(TokenType.IDENT) or not self.check_peek(TokenType.LPAREN):
            return self.primary(environment)

        function_name = self.current_token.text
        self.emitter.emit(f"{function_name}(")
        self.next_token()
        argument_count = self.arguments(environment)
        self.emitter.emit(")")

        self.functions_called.add((function_name, argument_count))

        # TODO: Here it is necessary to now function return type (and return it), but often we don't. Looks like 2 passes are needed.
        return self.functions_declared[function_name].return_type

    # primary ::= number | ident
    def primary(self, environment):
        if self.check_token(TokenType.INT):
            self.emitter.emit(self.current_token.text)
            self.next_token()
            return ValueType.INT

        if self.check_token(TokenType.FLOAT):
            # TODO: Maybe make sure that float literals end in .Xf?
            self.emitter.emit(self.current_token.text)
            self.next_token()
            return ValueType.FLOAT

        if self.check_token(TokenType.IDENT):
            # Make sure that the variable exists before it is used.
            if not environment.has_symbol(self.current_token.text):
                self.abort(f"Referencing variable before assignment: \"{self.current_token.text}\"")

            kind = environment.get_symbol_type(self.current_token.text)
            self.emitter.emit(self.current_token.text)
            self.next_token()
            return kind

        self.abort(f"Unexpected token at \"{self.current_token.text}\"")