use std::{collections::HashMap, sync::Arc};

use crate::{environment::*, error_reporting::report_error, parser::*};

// TODO: Typecheck structs: struct instance creations, usages, etc.
// TODO: Make sure initialized structs have been declared, and declared structs haven't been declared already.

#[derive(Clone)]
struct FunctionDeclaration {
    parameter_type_ids: Arc<Vec<usize>>,
    return_type_id: usize,
}

pub struct Checker<'a> {
    parser: &'a Parser,
    function_declarations: HashMap<String, FunctionDeclaration>,
}

impl<'a> Checker<'a> {
    pub fn new(parser: &'a Parser) -> Self {
        Self {
            parser,
            function_declarations: HashMap::new(),
        }
    }

    pub fn check(&mut self, program_index: usize) {
        self.program(program_index);
    }

    fn abort(&self, message: &str, start: usize) {
        report_error(self.parser.lexer.get_source(), message, start)
    }

    fn combine_type_ids(&self, current_type_id: usize, new_type_id: usize, start: usize) -> usize {
        if current_type_id == PrimitiveType::Void as usize {
            return new_type_id;
        }

        if new_type_id == PrimitiveType::Void as usize || current_type_id == new_type_id {
            return current_type_id;
        }

        self.abort(
            &format!(
                "Cannot mix types, expected {:?} but got {:?}",
                self.parser.types[current_type_id], self.parser.types[new_type_id]
            ),
            start,
        );
        unreachable!()
    }

    fn program(&mut self, index: usize) {
        let Node { node_type: NodeType::Program { function_indices, .. }, .. } = self.parser.ast[index].clone() else { unreachable!() };

        for function_index in function_indices.iter() {
            self.register_function(*function_index);
        }

        for function_index in function_indices.iter() {
            let environment = new_environment_ref(None);
            self.function(*function_index, environment);
        }
    }

    fn block(&mut self, index: usize, mut environment: EnvironmentRef) -> usize {
        environment = new_environment_ref(Some(environment));

        let Node { node_type: NodeType::Block { statement_indices }, .. } = self.parser.ast[index].clone() else { unreachable!() };

        let mut return_type_id = PrimitiveType::Void as usize;

        for statement_index in statement_indices.iter() {
            let statement_node = &self.parser.ast[*statement_index];
            let statement_return_type = match statement_node.node_type {
                NodeType::StatementPrintString { .. } => {
                    self.statement_print_string(*statement_index)
                }
                NodeType::StatementPrintExpression { .. } => {
                    self.statement_print_expression(*statement_index, environment.clone())
                }
                NodeType::StatementIf { .. } => {
                    self.statement_if(*statement_index, environment.clone())
                }
                NodeType::StatementWhile { .. } => {
                    self.statement_while(*statement_index, environment.clone())
                }
                NodeType::StatementVariableDeclaration { .. } => {
                    self.statement_variable_declaration(*statement_index, environment.clone())
                }
                NodeType::StatementVariableAssignment { .. } => {
                    self.statement_variable_assignment(*statement_index, environment.clone())
                }
                NodeType::StatementReturnValue { .. } => {
                    self.statement_return_value(*statement_index, environment.clone())
                }
                NodeType::StatementReturn { .. } => {
                    self.statement_return(*statement_index, environment.clone())
                }
                NodeType::StatementExpression { .. } => {
                    self.statement_expression(*statement_index, environment.clone())
                }
                _ => {
                    self.abort(
                        "Encountered a non-statement node within a block",
                        statement_node.node_start,
                    );
                    unreachable!()
                }
            };

            return_type_id = self.combine_type_ids(
                return_type_id,
                statement_return_type,
                statement_node.node_start,
            );
        }

        return_type_id
    }

    fn expression(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::Expression {
            term_index,
            trailing_terms,
        }, node_start } = self.parser.ast[index].clone() else { unreachable!() };

        let mut expression_type_id = self.term(term_index, environment.clone());

        for trailing_term in trailing_terms.iter() {
            let term_type_id = self.term(trailing_term.term_index, environment.clone());
            expression_type_id =
                self.combine_type_ids(expression_type_id, term_type_id, node_start);
        }

        expression_type_id
    }

    // fn string(&mut self, _index: usize) {
    // }

    fn statement_print_string(&mut self, _index: usize) -> usize {
        // let Node::StatementPrintString { string_index } = self.parser.ast[index] else { unreachable!() };
        // self.string(string_index);

        PrimitiveType::Void as usize
    }

    fn statement_print_expression(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::StatementPrintExpression { expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index, environment);

        PrimitiveType::Void as usize
    }

    fn term(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::Term {
            unary_index,
            trailing_unaries,
        }, node_start } = self.parser.ast[index].clone() else { unreachable!() };

        let mut term_type_id = self.unary(unary_index, environment.clone());

        for trailing_unary in trailing_unaries.iter() {
            let unary_type_id = self.unary(trailing_unary.unary_index, environment.clone());
            term_type_id = self.combine_type_ids(term_type_id, unary_type_id, node_start);
        }

        term_type_id
    }

    fn unary(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::Unary { call_index, .. }, .. } = self.parser.ast[index] else { unreachable!() };

        let call_node = &self.parser.ast[call_index];
        match call_node.node_type {
            NodeType::CallFunction { .. } => self.call_function(call_index, environment),
            NodeType::CallPrimary { .. } => self.call_primary(call_index, environment),
            _ => {
                self.abort(
                    "Encountered a non-call node within a unary",
                    call_node.node_start,
                );
                unreachable!()
            }
        }
    }

    fn call_function(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::CallFunction {
            name_start,
            name_end,
            arguments_index,
        }, node_start } = self.parser.ast[index] else { unreachable!() };
        let Node { node_type: NodeType::Arguments { expression_indices }, .. } = self.parser.ast[arguments_index].clone() else { unreachable!() };
        let function_name = self.parser.get_text(name_start, name_end);
        match self.function_declarations.get(function_name) {
            Some(declaration) => {
                let declaration = declaration.clone();

                if declaration.parameter_type_ids.len() != expression_indices.len() {
                    self.abort(
                        &format!(
                            "Function \"{}\" expected {} arguments but got {}",
                            function_name,
                            declaration.parameter_type_ids.len(),
                            expression_indices.len()
                        ),
                        node_start,
                    );
                }

                // Check that argument types match parameter types.
                for (parameter, expression_index) in declaration
                    .parameter_type_ids
                    .iter()
                    .zip(expression_indices.iter())
                {
                    let expression_type_id =
                        self.expression(*expression_index, environment.clone());

                    if *parameter != expression_type_id {
                        self.abort(
                            &format!(
                                "Function \"{}\" expected argument of type \"{:?}\" but got argument of type \"{:?}\"",
                                function_name,
                                self.parser.types[*parameter],
                                self.parser.types[expression_type_id],
                            ),
                            node_start,
                        );
                    }
                }
            }
            None => self.abort(
                &format!("Trying to call undefined function \"{}\"", function_name),
                node_start,
            ),
        }

        // self.arguments(arguments_index);

        self.function_declarations[function_name].return_type_id
    }

    fn call_primary(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::CallPrimary { primary_index }, .. } = self.parser.ast[index] else { unreachable!() };
        let primary_node = &self.parser.ast[primary_index];
        match primary_node.node_type {
            NodeType::PrimaryInt { .. } => self.primary_int(primary_index),
            NodeType::PrimaryFloat { .. } => self.primary_float(primary_index),
            NodeType::PrimaryBool { .. } => self.primary_bool(primary_index),
            NodeType::PrimaryIdent { .. } => self.primary_ident(primary_index, environment),
            _ => {
                self.abort(
                    "Encountered a non-primary node within a call primary",
                    primary_node.node_start,
                );
                unreachable!()
            }
        }
    }

    fn primary_int(&mut self, _index: usize) -> usize {
        PrimitiveType::Int as usize
    }

    fn primary_float(&mut self, _index: usize) -> usize {
        PrimitiveType::Float as usize
    }

    fn primary_bool(&mut self, _index: usize) -> usize {
        PrimitiveType::Bool as usize
    }

    fn primary_ident(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::PrimaryIdent { text_start, text_end }, node_start } = self.parser.ast[index] else { unreachable!() };
        let text = self.parser.get_text(text_start, text_end);

        match environment.borrow().get_symbol_type_id(text) {
            Some(type_id) => type_id,
            None => {
                self.abort(
                    &format!("Referencing undefined variable \"{}\"", text),
                    node_start,
                );
                unreachable!()
            }
        }
    }

    // fn arguments(&mut self, index: usize) {
    //     let Node::Arguments { expression_indices } = self.parser.ast[index].clone() else { unreachable!() };
    // }

    fn comparison(&mut self, index: usize, environment: EnvironmentRef) {
        let Node { node_type: NodeType::Comparison {
            left_expression_index,
            right_expression_index,
            ..
        }, .. } = self.parser.ast[index] else { unreachable!() };
        self.expression(left_expression_index, environment.clone());
        self.expression(right_expression_index, environment);
    }

    fn statement_if(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::StatementIf { comparison_index, block_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.comparison(comparison_index, environment.clone());
        self.block(block_index, environment);

        PrimitiveType::Void as usize
    }

    fn statement_while(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::StatementWhile { comparison_index, block_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.comparison(comparison_index, environment.clone());
        self.block(block_index, environment);

        PrimitiveType::Void as usize
    }

    fn statement_variable_declaration(
        &mut self,
        index: usize,
        environment: EnvironmentRef,
    ) -> usize {
        let Node { node_type: NodeType::StatementVariableDeclaration { name_start, name_end, type_id, expression_index }, node_start } = self.parser.ast[index] else { unreachable!() };
        let name = self.parser.get_text(name_start, name_end);
        // TODO: This prevents shadowing, maybe it shouldn't?
        if environment.borrow().has_symbol(name) {
            self.abort(
                &format!("Variable already declared \"{}\"", name),
                node_start,
            );
        }
        let expression_type_id = self.expression(expression_index, environment.clone());
        if expression_type_id != type_id {
            self.abort(
                &format!(
                    "Cannot assign value of type \"{:?}\" to variable of type \"{:?}\"",
                    self.parser.types[expression_type_id], self.parser.types[type_id]
                ),
                node_start,
            );
        }

        environment.borrow_mut().add_symbol(name, type_id);

        PrimitiveType::Void as usize
    }

    fn statement_variable_assignment(
        &mut self,
        index: usize,
        environment: EnvironmentRef,
    ) -> usize {
        let Node { node_type: NodeType::StatementVariableAssignment { name_start, name_end, expression_index }, node_start } = self.parser.ast[index] else { unreachable!() };
        let name = self.parser.get_text(name_start, name_end);
        let type_id = match environment.borrow().get_symbol_type_id(name) {
            Some(value_type) => value_type,
            None => {
                self.abort(&format!("Variable not declared \"{}\"", name), node_start);
                unreachable!()
            }
        };
        let expression_type = self.expression(expression_index, environment);
        if expression_type != type_id {
            self.abort(
                &format!(
                    "Cannot assign value of type \"{:?}\" to variable of type \"{:?}\"",
                    expression_type, type_id
                ),
                node_start,
            );
        }

        PrimitiveType::Void as usize
    }

    fn statement_return_value(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::StatementReturnValue { expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index, environment)
    }

    fn statement_return(&mut self, _index: usize, _environment: EnvironmentRef) -> usize {
        PrimitiveType::Void as usize
    }

    fn statement_expression(&mut self, index: usize, environment: EnvironmentRef) -> usize {
        let Node { node_type: NodeType::StatementExpression { expression_index }, .. } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index, environment);

        PrimitiveType::Void as usize
    }

    fn register_function(&mut self, index: usize) {
        let Node { node_type: NodeType::Function { name_start, name_end, parameters_index, .. }, node_start } = self.parser.ast[index] else { unreachable!() };
        let Node { node_type: NodeType::Parameters { input_list, return_type_id }, .. } = self.parser.ast[parameters_index].clone() else { unreachable!() };

        let name = self.parser.get_text(name_start, name_end);
        if self.function_declarations.contains_key(name) {
            self.abort(
                &format!("Function already declared \"{}\"", name),
                node_start,
            );
        }

        let mut parameter_type_ids = Vec::new();

        for parameter in input_list.iter() {
            parameter_type_ids.push(parameter.type_id);
        }

        self.function_declarations.insert(
            name.into(),
            FunctionDeclaration {
                parameter_type_ids: Arc::new(parameter_type_ids),
                return_type_id,
            },
        );
    }

    fn function(&mut self, index: usize, environment: EnvironmentRef) {
        let Node { node_type: NodeType::Function { parameters_index, block_index, .. }, node_start } = self.parser.ast[index] else { unreachable!() };
        let Node { node_type: NodeType::Parameters { return_type_id, .. }, .. } = self.parser.ast[parameters_index].clone() else { unreachable!() };
        self.parameters(parameters_index, environment.clone());
        let block_return_type_id = self.block(block_index, environment);
        if block_return_type_id != return_type_id {
            self.abort(&format!("Cannot return a value of type \"{:?}\" from a function with a return type of \"{:?}\"",
                self.parser.types[block_return_type_id], self.parser.types[return_type_id]), node_start);
        }
    }

    fn parameters(&mut self, index: usize, environment: EnvironmentRef) {
        let Node { node_type: NodeType::Parameters { input_list, .. }, .. } = self.parser.ast[index].clone() else { unreachable!() };
        for parameter in input_list.iter() {
            let name = self
                .parser
                .get_text(parameter.name_start, parameter.name_end);
            environment
                .borrow_mut()
                .add_symbol(name, parameter.type_id);
        }
    }
}
