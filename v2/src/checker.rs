use std::collections::HashMap;

use crate::{environment::*, parser::*};

struct FunctionDeclaration {
    parameter_types: Vec<ValueType>,
    return_type: ReturnType,
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

    fn abort(&self, message: &str) {
        panic!("Checker error! {message}");
    }

    fn combine_value_type(&self, current_type: ValueType, new_type: ValueType) -> ValueType {
        if current_type == ValueType::Void {
            return new_type;
        }

        if new_type == ValueType::Void || current_type == new_type {
            return current_type;
        }

        self.abort(&format!(
            "Cannot mix types, expected {:?} but got {:?}",
            current_type, new_type
        ));
        unreachable!()
    }

    fn combine_return_type(&self, current_type: ReturnType, new_type: ReturnType) -> ReturnType {
        ReturnType::from(
            self.combine_value_type(current_type.to_value_type(), new_type.to_value_type()),
        )
    }

    fn program(&mut self, index: usize) {
        let Node::Program { function_indices } = self.parser.ast[index].clone() else { unreachable!() };

        for function_index in function_indices.iter() {
            self.register_function(*function_index);
        }

        for function_index in function_indices.iter() {
            let environment = new_environment_ref(None);
            self.function(*function_index, environment);
        }
    }

    fn block(&mut self, index: usize, mut environment: EnvironmentRef) -> ReturnType {
        environment = new_environment_ref(Some(environment));

        let Node::Block { statement_indices } = self.parser.ast[index].clone() else { unreachable!() };

        let mut return_type = ReturnType::from(ValueType::Void);

        for statement_index in statement_indices.iter() {
            let statement_return_type = match &self.parser.ast[*statement_index] {
                Node::StatementPrintString { .. } => self.statement_print_string(*statement_index),
                Node::StatementPrintExpression { .. } => {
                    self.statement_print_expression(*statement_index, environment.clone())
                }
                Node::StatementIf { .. } => {
                    self.statement_if(*statement_index, environment.clone())
                }
                Node::StatementWhile { .. } => {
                    self.statement_while(*statement_index, environment.clone())
                }
                Node::StatementVariableDeclaration { .. } => {
                    self.statement_variable_declaration(*statement_index, environment.clone())
                }
                Node::StatementVariableAssignment { .. } => {
                    self.statement_variable_assignment(*statement_index, environment.clone())
                }
                Node::StatementReturnValue { .. } => {
                    self.statement_return_value(*statement_index, environment.clone())
                }
                Node::StatementReturn{ .. } => {
                    self.statement_return(*statement_index, environment.clone())
                }
                Node::StatementExpression { .. } => {
                    self.statement_expression(*statement_index, environment.clone())
                }
                _ => {
                    self.abort("Encountered a non-statement node within a block");
                    unreachable!()
                }
            };

            return_type = self.combine_return_type(return_type, statement_return_type);
        }

        return_type
    }

    fn expression(&mut self, index: usize, environment: EnvironmentRef) -> ValueType {
        let Node::Expression {
            term_index,
            trailing_terms,
        } = self.parser.ast[index].clone() else { unreachable!() };

        let mut expression_type = self.term(term_index, environment.clone());

        for trailing_term in trailing_terms.iter() {
            let term_type = self.term(trailing_term.term_index, environment.clone());
            expression_type = self.combine_value_type(expression_type, term_type);
        }

        expression_type
    }

    // fn string(&mut self, _index: usize) {
    // }

    fn statement_print_string(&mut self, _index: usize) -> ReturnType {
        // let Node::StatementPrintString { string_index } = self.parser.ast[index] else { unreachable!() };
        // self.string(string_index);

        ReturnType::from(ValueType::Void)
    }

    fn statement_print_expression(
        &mut self,
        index: usize,
        environment: EnvironmentRef,
    ) -> ReturnType {
        let Node::StatementPrintExpression { expression_index } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index, environment);

        ReturnType::from(ValueType::Void)
    }

    fn term(&mut self, index: usize, environment: EnvironmentRef) -> ValueType {
        let Node::Term {
            unary_index,
            trailing_unaries,
        } = self.parser.ast[index].clone() else { unreachable!() };

        let mut term_type = self.unary(unary_index, environment.clone());

        for trailing_unary in trailing_unaries.iter() {
            let unary_type = self.unary(trailing_unary.unary_index, environment.clone());
            term_type = self.combine_value_type(term_type, unary_type);
        }

        term_type
    }

    fn unary(&mut self, index: usize, environment: EnvironmentRef) -> ValueType {
        let Node::Unary { call_index, .. } = self.parser.ast[index] else { unreachable!() };

        match self.parser.ast[call_index] {
            Node::CallFunction { .. } => self.call_function(call_index),
            Node::CallPrimary { .. } => self.call_primary(call_index, environment),
            _ => {
                self.abort("Encountered a non-call node within a unary");
                unreachable!()
            },
        }
    }

    fn call_function(&mut self, index: usize) -> ValueType {
        let Node::CallFunction {
            name_start,
            name_end,
            arguments_index,
        } = self.parser.ast[index] else { unreachable!() };
        let Node::Arguments { expression_indices } = self.parser.ast[arguments_index].clone() else { unreachable!() };
        let function_name = self.parser.get_text(name_start, name_end);
        match self.function_declarations.get(function_name) {
            Some(declaration) => {
                if declaration.parameter_types.len() != expression_indices.len() {
                    self.abort(&format!(
                        "Function \"{}\" expected {} arguments but got {}",
                        function_name,
                        declaration.parameter_types.len(),
                        expression_indices.len()
                    ));
                }

                // TODO: Check that argument types match parameter types.
            }
            None => self.abort(&format!(
                "Trying to call undefined function \"{}\"",
                function_name
            )),
        }

        // self.arguments(arguments_index);

        self.function_declarations[function_name]
            .return_type
            .to_value_type()
    }

    fn call_primary(&mut self, index: usize, environment: EnvironmentRef) -> ValueType {
        let Node::CallPrimary { primary_index } = self.parser.ast[index] else { unreachable!() };
        match &self.parser.ast[primary_index] {
            Node::PrimaryInt { .. } => self.primary_int(primary_index),
            Node::PrimaryFloat { .. } => self.primary_float(primary_index),
            Node::PrimaryBool { .. } => self.primary_bool(primary_index),
            Node::PrimaryIdent { .. } => self.primary_ident(primary_index, environment),
            _ => {
                self.abort("Encountered a non-primary node within a call primary");
                unreachable!()
            },
        }
    }

    fn primary_int(&mut self, _index: usize) -> ValueType {
        ValueType::Int
    }

    fn primary_float(&mut self, _index: usize) -> ValueType {
        ValueType::Float
    }

    fn primary_bool(&mut self, _index: usize) -> ValueType {
        ValueType::Bool
    }

    fn primary_ident(&mut self, index: usize, environment: EnvironmentRef) -> ValueType {
        let Node::PrimaryIdent { text_start, text_end } = self.parser.ast[index] else { unreachable!() };
        let text = self.parser.get_text(text_start, text_end);

        match environment.borrow().get_symbol_type(text) {
            Some(value_type) => value_type,
            None => {
                self.abort(&format!("Referencing undefined variable \"{}\"", text));
                unreachable!()
            },
        }
    }

    // fn arguments(&mut self, index: usize) {
    //     let Node::Arguments { expression_indices } = self.parser.ast[index].clone() else { unreachable!() };
    // }

    fn comparison(&mut self, index: usize, environment: EnvironmentRef) {
        let Node::Comparison {
            left_expression_index,
            right_expression_index,
            ..
        } = self.parser.ast[index] else { unreachable!() };
        self.expression(left_expression_index, environment.clone());
        self.expression(right_expression_index, environment);
    }

    fn statement_if(&mut self, index: usize, environment: EnvironmentRef) -> ReturnType {
        let Node::StatementIf { comparison_index, block_index } = self.parser.ast[index] else { unreachable!() };
        self.comparison(comparison_index, environment.clone());
        self.block(block_index, environment);

        ReturnType::from(ValueType::Void)
    }

    fn statement_while(&mut self, index: usize, environment: EnvironmentRef) -> ReturnType {
        let Node::StatementWhile { comparison_index, block_index } = self.parser.ast[index] else { unreachable!() };
        self.comparison(comparison_index, environment.clone());
        self.block(block_index, environment);

        ReturnType::from(ValueType::Void)
    }

    fn statement_variable_declaration(
        &mut self,
        index: usize,
        environment: EnvironmentRef,
    ) -> ReturnType {
        let Node::StatementVariableDeclaration { name_start, name_end, value_type, expression_index } = self.parser.ast[index] else { unreachable!() };
        let name = self.parser.get_text(name_start, name_end);
        // TODO: This prevents shadowing, maybe it shouldn't?
        if environment.borrow().has_symbol(name) {
            self.abort(&format!("Variable already declared \"{}\"", name));
        }
        let expression_type = self.expression(expression_index, environment.clone());
        if expression_type != value_type {
            self.abort(&format!("Cannot assign value of type \"{:?}\" to variable of type \"{:?}\"", expression_type, value_type));
        }

        environment.borrow_mut().add_symbol(name, value_type);

        ReturnType::from(ValueType::Void)
    }

    fn statement_variable_assignment(
        &mut self,
        index: usize,
        environment: EnvironmentRef,
    ) -> ReturnType {
        let Node::StatementVariableAssignment { name_start, name_end, expression_index } = self.parser.ast[index] else { unreachable!() };
        let name = self.parser.get_text(name_start, name_end);
        let value_type = match environment.borrow().get_symbol_type(name) {
            Some(value_type) => value_type,
            None => {
                self.abort(&format!("Variable not declared \"{}\"", name));
                unreachable!()
            },
        };
        let expression_type = self.expression(expression_index, environment);
        if expression_type != value_type {
            self.abort(&format!("Cannot assign value of type \"{:?}\" to variable of type \"{:?}\"", expression_type, value_type));
        }

        ReturnType::from(ValueType::Void)
    }

    fn statement_return_value(&mut self, index: usize, environment: EnvironmentRef) -> ReturnType {
        let Node::StatementReturnValue { expression_index } = self.parser.ast[index] else { unreachable!() };
        let expression_type = self.expression(expression_index, environment);

        ReturnType::from(expression_type)
    }

    fn statement_return(&mut self, _index: usize, _environment: EnvironmentRef) -> ReturnType {
        ReturnType::from(ValueType::Void)
    }

    fn statement_expression(&mut self, index: usize, environment: EnvironmentRef) -> ReturnType {
        let Node::StatementExpression { expression_index } = self.parser.ast[index] else { unreachable!() };
        self.expression(expression_index, environment);

        ReturnType::from(ValueType::Void)
    }

    fn register_function(&mut self, index: usize) {
        let Node::Function { name_start, name_end, parameters_index, .. } = self.parser.ast[index] else { unreachable!() };
        let Node::Parameters { input_list, return_type } = self.parser.ast[parameters_index].clone() else { unreachable!() };

        let name = self.parser.get_text(name_start, name_end);
        let mut parameter_types = Vec::new();

        for parameter in input_list.iter() {
            parameter_types.push(parameter.value_type);
        }

        self.function_declarations.insert(
            name.into(),
            FunctionDeclaration {
                parameter_types,
                return_type,
            },
        );
    }

    fn function(&mut self, index: usize, environment: EnvironmentRef) {
        let Node::Function { parameters_index, block_index, .. } = self.parser.ast[index] else { unreachable!() };
        let Node::Parameters { return_type, .. } = self.parser.ast[parameters_index].clone() else { unreachable!() };
        self.parameters(parameters_index, environment.clone());
        let block_return_type = self.block(block_index, environment);
        if block_return_type != return_type {
            self.abort(&format!("Cannot return a value of type \"{:?}\" from a function with a return type of \"{:?}\"", block_return_type, return_type));
        }
    }

    fn parameters(&mut self, index: usize, environment: EnvironmentRef) {
        let Node::Parameters { input_list, .. } = self.parser.ast[index].clone() else { unreachable!() };
        for parameter in input_list.iter() {
            let name = self
                .parser
                .get_text(parameter.name_start, parameter.name_end);
            environment
                .borrow_mut()
                .add_symbol(name, parameter.value_type);
        }
    }
}
