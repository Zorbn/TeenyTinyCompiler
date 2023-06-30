import sys
from dataclasses import dataclass

from lex import *
from emit import *
from environment import *

class ValueType(enum.Enum):
    INT = 0
    FLOAT = 1

    @staticmethod
    def from_token_type(token_type):
        if token_type == TokenType.INT:
            return ValueType.INT
        elif token_type == TokenType.FLOAT:
            return ValueType.FLOAT

        return None

    @staticmethod
    def to_c_type(value_type):
        if value_type == ValueType.INT:
            return "int"
        elif value_type == ValueType.FLOAT:
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

        self.reset_tokens()

    def reset_tokens(self):
        self.lexer.reset_position()

        self.current_token = None
        self.peek_token = None
        # Call next token twice to initialize both current and peek.
        self.next_token()
        self.next_token()

    def check_token(self, token_type):
        return token_type == self.current_token.token_type

    def check_peek(self, token_type):
        return token_type == self.peek_token.token_type

    def match(self, token_type):
        if not self.check_token(token_type):
            self.abort(f"Expected \"{token_type.name}\" got \"{self.current_token.token_type.name}\"")
        self.next_token()

    def next_token(self):
        self.current_token = self.peek_token
        self.peek_token = self.lexer.get_token()

    def abort(self, message):
        sys.exit(f"Parsing error! {message}")

    def combine_type(self, old_type, new_type):
        if old_type is None:
            return new_type
        elif new_type is not None and old_type != new_type:
            self.abort(f"Cannot mix types, expected {old_type} got {new_type}")

        return old_type

    def token_to_types(self):
        value_type = ValueType.from_token_type(self.current_token.token_type)
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

        # Do a first pass to make sure functions are order-independent.
        self.function_prototypes()
        self.reset_tokens()

        # A program is composed of a series of statements.
        self.block(TokenType.EOF, environment)

        self.emitter.emit_line("return 0;")
        self.emitter.emit_line("}")

        # Make sure all referenced labels have been declared.
        for label in self.labels_gotoed:
            if label not in self.labels_declared:
                self.abort(f"Attempting to GOTO an undeclared label \"{label}\"")

    def function_prototypes(self):
        while not self.check_token(TokenType.EOF):
            if self.check_token(TokenType.FUNCTION):
                self.function(True)

            self.next_token()

    # TODO: (Ongoing) Keep track of if blocks return a value: https://stackoverflow.com/questions/21945891/how-do-i-check-whether-all-code-paths-return-a-value
    # TODO: All blocks should be able to return a value, which will make more sense once all code must be in a function. (one the frontend, that's already true in the backend)
    def statement(self, environment):
        return_type = None

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

        # TODO: Make sure function names don't collide with others when C is generated. Maybe don't include C headers in the same file as the output, put all of the C code in a seperate file and then just import it's header in the main output code?
        # "FUNCTION" ident parameters ":" type "DO" block "ENDFUNCTION"
        elif self.check_token(TokenType.FUNCTION):
            self.function(False)

        elif self.check_token(TokenType.RETURN):
            self.next_token()
            self.emitter.emit("return ")
            expression_type = self.expression(environment)
            self.emitter.emit_line(";")
            return_type = expression_type

        else: # Unknown
            self.expression(environment)
            self.emitter.emit_line(";")
            # TODO: This branch used to be an error, self.abort(f"Invalid statement at \"{self.current_token.text}\" ({self.current_token.token_type.name})")
            # but now it allows any expression, should that be valid? This change was made to support function calls as statements.

        self.newline()
        return return_type

    # block ::= {statement} terminator
    def block(self, terminator, enclosing_environment):
        environment = Environment(enclosing_environment)

        return_type = None

        while not self.check_token(terminator):
            statement_return_type = self.statement(environment)
            return_type = self.combine_type(return_type, statement_return_type)

        self.match(terminator) # TODO: Should this happen here or outside block() after it is run?
        return return_type

    # newline ::= '\n'+
    def newline(self):
        self.match(TokenType.NEWLINE)

        while self.check_token(TokenType.NEWLINE):
            self.next_token()

    # "FUNCTION" ident parameters ":" type "DO" block "ENDFUNCTION"
    def function(self, parse_prototype):
        if self.emitter.match_region(EmitRegion.FUNCTION):
            self.abort(f"Functions declarations cannot be nested")

        self.next_token()

        function_name = self.current_token.text

        if parse_prototype:
            # Make sure the function doesn't already exist.
            if function_name in self.functions_declared:
                self.abort(f"Function already exists: \"{function_name}\"")

        # Buffer the code being emitted because the return
        # type is determined after the parameters are emitted.
        self.emitter.set_region(EmitRegion.BUFFERED)
        self.emitter.emit(f"{function_name}(")
        self.next_token()

        function_environment = Environment(None)

        parameter_types = self.parameters(function_environment)

        self.match(TokenType.COLON)
        (value_type, c_type) = self.token_to_types()
        if parse_prototype:
            self.functions_declared[function_name] = FunctionDeclaration(value_type, parameter_types)
        self.next_token()
        self.match(TokenType.DO)
        self.newline()

        buffer = self.emitter.get_buffer()
        self.emitter.clear_buffer()

        # The function prototype has now been parsed, either
        # emit it or use it as part of the function body.
        if parse_prototype:
            self.emitter.set_region(EmitRegion.PROTOTYPE)
            self.emitter.emit_line(f"{c_type} {buffer});")
        else:
            self.emitter.set_region(EmitRegion.FUNCTION)
            self.emitter.emit_line(f"{c_type} {buffer}) {{")

            return_type = self.block(TokenType.ENDFUNCTION, function_environment)
            if return_type is None:
                self.abort(f"Function does not return a value on all code paths")
            prototype_return_type = self.functions_declared[function_name].return_type
            if return_type != prototype_return_type:
                self.abort(f"Function does not return a value of the correct type: expect {prototype_return_type} got {return_type}")

            self.emitter.emit_line("}")

        self.emitter.set_region(EmitRegion.CODE)

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

        argument_types = []
        while not self.check_token(TokenType.RPAREN):
            if len(argument_types) > 0:
                self.match(TokenType.COMMA)
                self.emitter.emit(", ")

            expression_type = self.expression(environment)
            argument_types.append(expression_type)

        self.match(TokenType.RPAREN)
        return argument_types

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
        expression_type = self.term(environment)

        while self.check_token(TokenType.PLUS) or self.check_token(TokenType.MINUS):
            self.emitter.emit(self.current_token.text)
            self.next_token()
            term_type = self.term(environment)
            expression_type = self.combine_type(expression_type, term_type)

        return expression_type

    # term ::= unary {( "/" | "*" ) unary}
    def term(self, environment):
        term_type = self.unary(environment)

        while self.check_token(TokenType.ASTERISK) or self.check_token(TokenType.SLASH):
            self.emitter.emit(self.current_token.text)
            self.next_token()
            unary_type = self.unary(environment)
            term_type = self.combine_type(term_type, unary_type)

        return term_type

    # unary ::= ["+" | "-"] call
    def unary(self, environment):
        # Optional unary +/-
        if self.check_token(TokenType.PLUS) or self.check_token(TokenType.MINUS):
            self.emitter.emit(self.current_token.text)
            self.next_token()

        return self.call(environment)

    # call ::= ident arguments | primary # TODO: This may be incorrect BNF because call checks for parenthesis to choose between the two options.
    def call(self, environment):
        if not self.check_token(TokenType.IDENT) or not self.check_peek(TokenType.LPAREN):
            return self.primary(environment)

        function_name = self.current_token.text
        self.emitter.emit(f"{function_name}(")
        self.next_token()
        argument_types = self.arguments(environment)
        self.emitter.emit(")")

        if function_name not in self.functions_declared:
            self.abort(f"Calling undefined function: \"{function_name}\"")

        parameter_types = self.functions_declared[function_name].parameter_types

        argument_count = len(argument_types)
        parameter_count = len(parameter_types)
        if argument_count != parameter_count:
            self.abort(f"Calling function \"{function_name}\" with incorrect number of parameters: expected {parameter_count} got {argument_count}")

        for i in range(argument_count):
            if argument_types[i] != parameter_types[i]:
                self.abort(f"Calling function \"{function_name}\" with incorrect parameter: expected {parameter_types[i]} got {argument_types[i]}")

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

            ident_type = environment.get_symbol_type(self.current_token.text)
            self.emitter.emit(self.current_token.text)
            self.next_token()
            return ident_type

        self.abort(f"Unexpected token at \"{self.current_token.text}\"")