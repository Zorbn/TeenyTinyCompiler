use crate::{emitter::*, error_reporting::report_error, parser::*};

// TODO: Codegen structs.

fn primitive_type_to_c_type(primitive_type: PrimitiveType) -> &'static str {
    match primitive_type {
        PrimitiveType::Int => "int",
        PrimitiveType::Float => "float",
        PrimitiveType::Bool => "bool",
        _ => "void",
    }
}

pub struct CodeGenerator {
    parser: Parser,
    emitter: Emitter,
}

impl CodeGenerator {
    pub fn new(parser: Parser) -> Self {
        Self {
            parser,
            emitter: Emitter::new(),
        }
    }

    pub fn emit(&mut self, program_index: usize, output_file: &str) {
        self.program(program_index);
        self.emitter
            .write_file(output_file)
            .expect("Failed to write generated code to the output file");
    }

    fn abort(&self, message: &str, start: usize) {
        report_error(self.parser.lexer.get_source(), message, start)
    }

    fn type_id_to_c_type(&self, type_id: usize) -> String {
        match &self.parser.types[type_id] {
            TypeDefinition::Primitive { primitive_type } => primitive_type_to_c_type(*primitive_type).into(),
            TypeDefinition::Struct { name_start, name_end, .. } => self.parser.get_text(*name_start, *name_end).into(),
        }
    }

    fn program(&mut self, index: usize) {
        let Node { node_type: NodeType::Program { function_indices, struct_indices }, .. } = self.parser.ast[index].clone() else { unreachable!() };
        self.emitter.set_region(EmitRegion::Preprocessor);
        self.emitter.emit_line("#define _CRT_SECURE_NO_WARNINGS");
        self.emitter.emit_line("#include <stdio.h>");
        self.emitter.emit_line("#include <stdbool.h>");

        self.emitter.set_region(EmitRegion::Body);

        for (i, struct_index) in struct_indices.iter().enumerate() {
            // Seperate struct definitions by a newline.
            if i > 0 {
                self.emitter.emit_line("");
            }

            self.struct_node(*struct_index);
        }

        if struct_indices.len() > 0 {
            self.emitter.emit_line("");
        }

        for (i, function_index) in function_indices.iter().enumerate() {
            // Seperate function definitions by a newline.
            if i > 0 {
                self.emitter.emit_line("");
            }

            self.function(*function_index);
        }
    }

    fn block(&mut self, index: usize) {
        let Node { node_type: NodeType::Block { statement_indices }, .. } = self.parser.ast[index].clone() else { unreachable!() };
        self.emitter.emit_line("{");
        self.emitter.indent();

        for statement_index in statement_indices.iter() {
            let statement_node = &self.parser.ast[*statement_index];
            match statement_node.node_type {
                NodeType::StatementPrintString { .. } => {
                    self.statement_print_string(*statement_index)
                }
                NodeType::StatementPrintExpression { .. } => {
                    self.statement_print_expression(*statement_index)
                }
                NodeType::StatementIf { .. } => self.statement_if(*statement_index),
                NodeType::StatementWhile { .. } => self.statement_while(*statement_index),
                NodeType::StatementVariableDeclaration { .. } => {
                    self.statement_variable_declaration(*statement_index)
                }
                NodeType::StatementVariableAssignment { .. } => {
                    self.statement_variable_assignment(*statement_index)
                }
                NodeType::StatementReturnValue { .. } => {
                    self.statement_return_value(*statement_index)
                }
                NodeType::StatementReturn { .. } => self.statement_return(*statement_index),
                NodeType::StatementExpression { .. } => self.statement_expression(*statement_index),
                _ => self.abort(
                    "Encountered a non-statement node within a block",
                    statement_node.node_start,
                ),
            }
        }

        self.emitter.unindent();
        self.emitter.emit_line("}");
    }

    fn expression(&mut self, index: usize) {
        let Node { node_type: NodeType::Expression {
            term_index,
            trailing_terms,
        }, .. } = self.parser.ast[index].clone() else { unreachable!() };

        self.term(term_index);

        for trailing_term in trailing_terms.iter() {
            self.emitter.emit(match trailing_term.op {
                ExpressionOp::Plus => "+",
                ExpressionOp::Minus => "-",
            });

            self.term(trailing_term.term_index);
        }
    }

    fn string(&mut self, index: usize) {
        let Node { node_type: NodeType::String { text_start, text_end }, .. } = self.parser.ast[index] else { unreachable!() };
        let text = self.parser.get_text(text_start, text_end);
        self.emitter.emit(text);
    }

    fn statement_print_string(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementPrintString { string_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.emitter.emit("printf(\"");
        self.string(string_index);
        self.emitter.emit_line("\\n\");");
    }

    fn statement_print_expression(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementPrintExpression { expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.emitter.emit("printf(\"%.2f\\n\", (float)(");
        self.expression(expression_index);
        self.emitter.emit_line("));");
    }

    fn term(&mut self, index: usize) {
        let Node { node_type: NodeType::Term {
            unary_index,
            trailing_unaries,
        }, .. } = self.parser.ast[index].clone() else { unreachable!() };

        self.unary(unary_index);

        for trailing_unary in trailing_unaries.iter() {
            self.emitter.emit(match trailing_unary.op {
                TermOp::Multiply => "*",
                TermOp::Divide => "/",
            });

            self.unary(trailing_unary.unary_index);
        }
    }

    fn unary(&mut self, index: usize) {
        let Node { node_type: NodeType::Unary { op, call_index }, .. } = self.parser.ast[index] else { unreachable!() };
        match op {
            Some(UnaryOp::Plus) => self.emitter.emit("+"),
            Some(UnaryOp::Minus) => self.emitter.emit("-"),
            None => {}
        }

        let call_node = &self.parser.ast[call_index];
        match call_node.node_type {
            NodeType::CallFunction { .. } => self.call_function(call_index),
            NodeType::CallPrimary { .. } => self.call_primary(call_index),
            _ => self.abort(
                "Encountered a non-call node within a unary",
                call_node.node_start,
            ),
        }
    }

    fn call_function(&mut self, index: usize) {
        let Node { node_type: NodeType::CallFunction {
            name_start,
            name_end,
            arguments_index,
        }, .. } = self.parser.ast[index] else { unreachable!() };
        let function_name = self.parser.get_text(name_start, name_end);
        self.emitter.emit(function_name);
        self.arguments(arguments_index);
    }

    fn call_primary(&mut self, index: usize) {
        let Node { node_type: NodeType::CallPrimary { primary_index }, .. } = self.parser.ast[index] else { unreachable!() };
        let primary_node = &self.parser.ast[primary_index];
        match primary_node.node_type {
            NodeType::PrimaryInt { .. } => self.primary_int(primary_index),
            NodeType::PrimaryFloat { .. } => self.primary_float(primary_index),
            NodeType::PrimaryBool { .. } => self.primary_bool(primary_index),
            NodeType::PrimaryIdent { .. } => self.primary_ident(primary_index),
            NodeType::PrimaryStruct { .. } => self.primary_struct(primary_index),
            _ => self.abort(
                "Encountered a non-primary node within a call primary",
                primary_node.node_start,
            ),
        }
    }

    fn primary_int(&mut self, index: usize) {
        let Node { node_type: NodeType::PrimaryInt { text_start, text_end }, .. } = self.parser.ast[index] else { unreachable!() };
        let text = self.parser.get_text(text_start, text_end);
        self.emitter.emit(text);
    }

    fn primary_float(&mut self, index: usize) {
        let Node { node_type: NodeType::PrimaryFloat { text_start, text_end }, .. } = self.parser.ast[index] else { unreachable!() };
        let text = self.parser.get_text(text_start, text_end);
        self.emitter.emit(text);
        self.emitter.emit("f");
    }

    fn primary_bool(&mut self, index: usize) {
        let Node { node_type: NodeType::PrimaryBool { is_true }, .. } = self.parser.ast[index] else { unreachable!() };
        self.emitter.emit(match is_true {
            true => "true",
            false => "false",
        });
    }

    fn primary_ident(&mut self, index: usize) {
        let Node { node_type: NodeType::PrimaryIdent { text_start, text_end }, .. } = self.parser.ast[index] else { unreachable!() };
        let text = self.parser.get_text(text_start, text_end);
        self.emitter.emit(text);
    }

    fn primary_struct(&mut self, index: usize) {
        let Node { node_type: NodeType::PrimaryStruct { name_start, name_end, argument_list }, .. } = self.parser.ast[index].clone() else { unreachable!() };
        let name = self.parser.get_text(name_start, name_end);

        self.emitter.emit("(");
        self.emitter.emit(name);
        self.emitter.emit(")");
        self.emitter.emit_line(" {");
        self.emitter.indent();

        for argument in argument_list.iter() {
            let argument_name = self.parser.get_text(argument.name_start, argument.name_end);
            self.emitter.emit(".");
            self.emitter.emit(argument_name);
            self.emitter.emit(" = ");
            self.expression(argument.expression_index);
            self.emitter.emit_line(",");
        }

        self.emitter.unindent();
        self.emitter.emit("}");
    }

    fn arguments(&mut self, index: usize) {
        let Node { node_type: NodeType::Arguments { expression_indices }, .. } = self.parser.ast[index].clone() else { unreachable!() };
        self.emitter.emit("(");
        for (i, expression_index) in expression_indices.iter().enumerate() {
            if i > 0 {
                self.emitter.emit(", ");
            }

            self.expression(*expression_index);
        }
        self.emitter.emit(")");
    }

    fn comparison(&mut self, index: usize) {
        let Node { node_type: NodeType::Comparison {
            left_expression_index,
            op,
            right_expression_index,
        }, .. } = self.parser.ast[index] else { unreachable!() };
        self.expression(left_expression_index);
        self.emitter.emit(match op {
            ComparisonOp::EqEq => "==",
            ComparisonOp::NotEq => "!=",
            ComparisonOp::Lt => "<",
            ComparisonOp::LtEq => "<=",
            ComparisonOp::Gt => ">",
            ComparisonOp::GtEq => ">=",
        });
        self.expression(right_expression_index);
    }

    fn statement_if(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementIf { comparison_index, block_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.emitter.emit("if (");
        self.comparison(comparison_index);
        self.emitter.emit(") ");
        self.block(block_index);
    }

    fn statement_while(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementWhile { comparison_index, block_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.emitter.emit("while (");
        self.comparison(comparison_index);
        self.emitter.emit(") ");
        self.block(block_index);
    }

    fn statement_variable_declaration(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementVariableDeclaration { name_start, name_end, type_id, expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        let name = self.parser.get_text(name_start, name_end);
        let c_type = self.type_id_to_c_type(type_id);
        self.emitter.emit(&c_type);
        self.emitter.emit(" ");
        self.emitter.emit(name);
        self.emitter.emit(" = ");
        self.expression(expression_index);
        self.emitter.emit_line(";");
    }

    fn statement_variable_assignment(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementVariableAssignment { name_start, name_end, expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        let name = self.parser.get_text(name_start, name_end);
        self.emitter.emit(name);
        self.emitter.emit(" = ");
        self.expression(expression_index);
        self.emitter.emit_line(";");
    }

    fn statement_return_value(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementReturnValue { expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.emitter.emit("return ");
        self.expression(expression_index);
        self.emitter.emit_line(";");
    }

    fn statement_return(&mut self, _index: usize) {
        self.emitter.emit_line("return;");
    }

    fn statement_expression(&mut self, index: usize) {
        let Node { node_type: NodeType::StatementExpression { expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index);
        self.emitter.emit_line(";");
    }

    fn function(&mut self, index: usize) {
        let Node { node_type: NodeType::Function { name_start, name_end, parameters_index, block_index }, .. } = self.parser.ast[index] else { unreachable!() };
        let Node { node_type: NodeType::Parameters { return_type_id, .. }, .. } = self.parser.ast[parameters_index].clone() else { unreachable!() };
        let c_return_type = self.type_id_to_c_type(return_type_id);

        self.emitter.set_region(EmitRegion::Prototype);
        self.emitter.emit(&c_return_type);
        self.emitter.emit(" ");
        self.emitter
            .emit(self.parser.get_text(name_start, name_end));
        self.parameters(parameters_index);
        self.emitter.emit_line(";");

        self.emitter.set_region(EmitRegion::Body);
        self.emitter.emit(&c_return_type);
        self.emitter.emit(" ");
        self.emitter
            .emit(self.parser.get_text(name_start, name_end));
        self.parameters(parameters_index);
        self.emitter.emit(" ");
        self.block(block_index);
    }

    fn struct_node(&mut self, index: usize) {
        let Node { node_type: NodeType::Struct { name_start, name_end, field_list }, .. } = self.parser.ast[index].clone() else { unreachable!() };

        let name = self.parser.get_text(name_start, name_end);
        self.emitter.set_region(EmitRegion::Prototype);
        self.emitter.emit("typedef struct ");
        self.emitter.emit(name);
        self.emitter.emit(" ");
        self.emitter.emit(name);
        self.emitter.emit_line(";");

        self.emitter.set_region(EmitRegion::Body);
        self.emitter.emit("struct ");
        self.emitter.emit(name);
        self.emitter.emit_line(" {");
        self.emitter.indent();

        for field in field_list.iter() {
            let field_name = self.parser.get_text(field.name_start, field.name_end);
            let c_type = self.type_id_to_c_type(field.type_id);
            self.emitter.emit(&c_type);
            self.emitter.emit(" ");
            self.emitter.emit(field_name);
            self.emitter.emit_line(";");
        }

        self.emitter.unindent();
        self.emitter.emit_line("};");
    }

    fn parameters(&mut self, index: usize) {
        let Node { node_type: NodeType::Parameters { input_list, .. }, .. } = self.parser.ast[index].clone() else { unreachable!() };
        self.emitter.emit("(");

        if input_list.is_empty() {
            self.emitter.emit("void");
        }

        for (i, parameter) in input_list.iter().enumerate() {
            if i > 0 {
                self.emitter.emit(", ");
            }

            let c_type = self.type_id_to_c_type(parameter.type_id);
            self.emitter.emit(&c_type);
            self.emitter.emit(" ");
            let name = self
                .parser
                .get_text(parameter.name_start, parameter.name_end);
            self.emitter.emit(name);
        }

        self.emitter.emit(")");
    }
}
