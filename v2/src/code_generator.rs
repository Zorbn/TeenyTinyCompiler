use crate::parser::{Node, Parser};

pub struct CodeGenerator {
    parser: Parser,
}

impl CodeGenerator {
    pub fn new(parser: Parser) -> Self {
        Self { parser }
    }

    pub fn emit(&mut self) {
        let program_index = self.parser.program();
        self.program(program_index);
    }

    fn abort(&self, message: &str) {
        panic!("Code generation error! {message}");
    }

    fn program(&mut self, index: usize) {
        let Node::Program { block_index } = self.parser.ast[index] else { unreachable!() };
        self.block(block_index);
    }

    fn block(&mut self, index: usize) {
        let Node::Block { statement_indices } = self.parser.ast[index].clone() else { unreachable!() };
        for statement_index in statement_indices.iter() {
            match &self.parser.ast[*statement_index] {
                Node::StatementPrintString { .. } => self.statement_print_string(*statement_index),
                Node::StatementPrintExpression { .. } => {
                    self.statement_print_expression(*statement_index)
                }
                Node::StatementIf { .. } => self.statement_if(*statement_index),
                Node::StatementWhile { .. } => self.statement_while(*statement_index),
                Node::StatementVariableDeclaration { .. } => {
                    self.statement_variable_declaration(*statement_index)
                }
                Node::StatementVariableAssignment { .. } => {
                    self.statement_variable_assignment(*statement_index)
                }
                Node::StatementReturn { .. } => self.statement_return(*statement_index),
                Node::StatementExpression { .. } => self.statement_expression(*statement_index),
                Node::StatementFunction { .. } => self.statement_function(*statement_index),
                _ => self.abort("Encountered a non-statement node within a block"),
            }
        }
    }

    fn expression(&mut self, index: usize) {
        let Node::Expression {
            term_index,
            trailing_terms,
        } = self.parser.ast[index].clone() else { unreachable!() };
        self.term(term_index);

        for trailing_term in trailing_terms.iter() {
            self.term(trailing_term.term_index);
        }
    }

    fn string(&mut self, index: usize) {
        todo!()
    }

    fn statement_print_string(&mut self, index: usize) {
        let Node::StatementPrintString { string_index } = self.parser.ast[index] else { unreachable!() };
        self.string(string_index);
    }

    fn statement_print_expression(&mut self, index: usize) {
        let Node::StatementPrintExpression { expression_index } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index);
    }

    fn term(&mut self, index: usize) {
        let Node::Term {
            unary_index,
            trailing_unaries,
        } = self.parser.ast[index].clone() else { unreachable!() };
        self.unary(unary_index);

        for trailing_unary in trailing_unaries.iter() {
            self.unary(trailing_unary.unary_index);
        }
    }

    fn unary(&mut self, index: usize) {
        todo!()
    }

    fn call_function(&mut self, index: usize) {
        let Node::CallFunction {
            name_start,
            name_end,
            arguments_index,
        } = self.parser.ast[index] else { unreachable!() };
        self.arguments(arguments_index);
    }

    fn call_primary(&mut self, index: usize) {
        let Node::CallPrimary { primary_index } = self.parser.ast[index] else { unreachable!() };
        match &self.parser.ast[primary_index] {
            Node::PrimaryInt { .. } => self.primary_int(primary_index),
            Node::PrimaryFloat { .. } => self.primary_float(primary_index),
            Node::PrimaryBool { .. } => self.primary_bool(primary_index),
            Node::PrimaryIdent { .. } => self.primary_ident(primary_index),
            _ => self.abort("Encountered a non-primary node within a call primary"),
        }
    }

    fn primary_int(&mut self, index: usize) {
        todo!()
    }

    fn primary_float(&mut self, index: usize) {
        todo!()
    }

    fn primary_bool(&mut self, index: usize) {
        todo!()
    }

    fn primary_ident(&mut self, index: usize) {
        todo!()
    }

    fn arguments(&mut self, index: usize) {
        let Node::Arguments { expression_indices } = self.parser.ast[index].clone() else { unreachable!() };
        for expression_index in expression_indices.iter() {
            self.expression(*expression_index);
        }
    }

    fn comparison(&mut self, index: usize) {
        let Node::Comparison {
            left_expression_index,
            op,
            right_expression_index,
        } = self.parser.ast[index] else { unreachable!() };
        self.expression(left_expression_index);
        self.expression(right_expression_index);
    }

    fn statement_if(&mut self, index: usize) {
        let Node::StatementIf { comparison_index, block_index } = self.parser.ast[index] else { unreachable!() };
        self.comparison(comparison_index);
        self.block(block_index);
    }

    fn statement_while(&mut self, index: usize) {
        let Node::StatementWhile { comparison_index, block_index } = self.parser.ast[index] else { unreachable!() };
        self.comparison(comparison_index);
        self.block(block_index);
    }

    fn statement_variable_declaration(&mut self, index: usize) {
        let Node::StatementVariableDeclaration { name_start, name_end, value_type, expression_index } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index);
    }

    fn statement_variable_assignment(&mut self, index: usize) {
        let Node::StatementVariableAssignment { name_start, name_end, expression_index } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index);
    }

    fn statement_return(&mut self, index: usize) {
        let Node::StatementReturn { expression_index } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index);
    }

    fn statement_expression(&mut self, index: usize) {
        let Node::StatementExpression { expression_index } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index);
    }

    fn statement_function(&mut self, index: usize) {
        let Node::StatementFunction { name_start, name_end, parameters_index, block_index } = self.parser.ast[index] else { unreachable!() };
        self.parameters(parameters_index);
        self.block(block_index);
    }

    fn parameters(&mut self, index: usize) {
        let Node::Parameters { list } = self.parser.ast[index].clone() else { unreachable!() };
        for parameter in list.iter() {
            todo!()
        }
    }
}
