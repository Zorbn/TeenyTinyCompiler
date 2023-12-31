use std::{collections::HashMap, sync::Arc};

use crate::{
    error_reporting::report_error,
    lexer::{Lexer, Token, TokenType},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    Void,
    Int,
    Float,
    Bool,
}

#[derive(Debug)]
pub enum TypeDefinition {
    Primitive {
        primitive_type: PrimitiveType,
    },
    // TODO: Ensure struct names don't conflict with keywords in this language (maybe already covered?) or generated C code.
    // TODO: Prevent recursive struct fields.
    Struct {
        name_start: usize,
        name_end: usize,
        field_list: Arc<Vec<Field>>,
    },
    Array {
        element_type_id: usize,
        length: usize,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ExpressionOp {
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy)]
pub enum TermOp {
    Multiply,
    Divide,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy)]
pub enum ComparisonOp {
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

fn token_type_to_comparison_op(token_type: TokenType) -> Option<ComparisonOp> {
    match token_type {
        TokenType::EqEq => Some(ComparisonOp::EqEq),
        TokenType::NotEq => Some(ComparisonOp::NotEq),
        TokenType::Lt => Some(ComparisonOp::Lt),
        TokenType::LtEq => Some(ComparisonOp::LtEq),
        TokenType::Gt => Some(ComparisonOp::Gt),
        TokenType::GtEq => Some(ComparisonOp::GtEq),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TrailingTerm {
    pub op: ExpressionOp,
    pub term_index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct TrailingUnary {
    pub op: TermOp,
    pub unary_index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Field {
    pub name_start: usize,
    pub name_end: usize,
    pub type_id: usize,
}

#[derive(Debug)]
pub struct FieldName {
    pub name_start: usize,
    pub name_end: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Parameter {
    pub name_start: usize,
    pub name_end: usize,
    pub type_id: usize,
}

#[derive(Debug)]
pub struct NamedArgument {
    pub name_start: usize,
    pub name_end: usize,
    pub expression_index: usize,
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Program {
        function_indices: Arc<Vec<usize>>,
        struct_indices: Arc<Vec<usize>>,
    },
    Block {
        statement_indices: Arc<Vec<usize>>,
    },
    Expression {
        term_index: usize,
        trailing_terms: Arc<Vec<TrailingTerm>>,
    },
    String {
        text_start: usize,
        text_end: usize,
    },
    StatementPrintString {
        string_index: usize,
    },
    StatementPrintExpression {
        expression_index: usize,
    },
    Term {
        unary_index: usize,
        trailing_unaries: Arc<Vec<TrailingUnary>>,
    },
    Unary {
        op: Option<UnaryOp>,
        call_index: usize,
    },
    CallFunction {
        name_start: usize,
        name_end: usize,
        arguments_index: usize,
    },
    CallPrimary {
        primary_index: usize,
    },
    PrimaryInt {
        text_start: usize,
        text_end: usize,
    },
    PrimaryFloat {
        text_start: usize,
        text_end: usize,
    },
    PrimaryBool {
        is_true: bool,
    },
    PrimaryStruct {
        name_start: usize,
        name_end: usize,
        argument_list: Arc<Vec<NamedArgument>>,
    },
    PrimaryArray {
        name_start: usize,
        name_end: usize,
        length: usize,
    },
    PrimaryVariable {
        variable_index: usize,
    },
    VariableIdent {
        text_start: usize,
        text_end: usize,
    },
    VariableArrayAccess {
        name_start: usize,
        name_end: usize,
        expression_index: usize,
    },
    VariableField {
        name_start: usize,
        name_end: usize,
        field_list: Arc<Vec<FieldName>>,
    },
    Arguments {
        expression_indices: Arc<Vec<usize>>,
    },
    Comparison {
        left_expression_index: usize,
        op: ComparisonOp,
        right_expression_index: usize,
    },
    StatementIf {
        comparison_index: usize,
        block_index: usize,
    },
    StatementWhile {
        comparison_index: usize,
        block_index: usize,
    },
    StatementVariableDeclaration {
        name_start: usize,
        name_end: usize,
        type_id: usize,
        expression_index: usize,
    },
    StatementVariableAssignment {
        variable_index: usize,
        expression_index: usize,
    },
    StatementReturnValue {
        expression_index: usize,
    },
    StatementReturn {},
    StatementExpression {
        expression_index: usize,
    },
    Function {
        name_start: usize,
        name_end: usize,
        parameters_index: usize,
        block_index: usize,
    },
    Parameters {
        input_list: Arc<Vec<Parameter>>,
        return_type_id: usize,
    },
    Struct {
        name_start: usize,
        name_end: usize,
        field_list: Arc<Vec<Field>>,
    },
}

#[derive(Debug, Clone)]
pub struct Node {
    pub node_type: NodeType,
    pub node_start: usize,
}

pub struct Parser {
    pub ast: Vec<Node>,
    pub simple_type_ids: HashMap<String, usize>,
    pub array_type_ids: HashMap<(String, usize), usize>,
    pub types: Vec<TypeDefinition>,
    pub lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    is_skipping_nodes: bool,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            ast: Vec::new(),
            lexer,
            current_token: Token::from_single(0, TokenType::Eof),
            peek_token: Token::from_single(0, TokenType::Eof),
            simple_type_ids: HashMap::new(),
            array_type_ids: HashMap::new(),
            types: Vec::new(),
            is_skipping_nodes: false,
        };

        parser.types.push(TypeDefinition::Primitive {
            primitive_type: PrimitiveType::Void,
        });
        parser
            .simple_type_ids
            .insert("void".into(), PrimitiveType::Void as usize);

        parser.types.push(TypeDefinition::Primitive {
            primitive_type: PrimitiveType::Int,
        });
        parser
            .simple_type_ids
            .insert("int".into(), PrimitiveType::Int as usize);

        parser.types.push(TypeDefinition::Primitive {
            primitive_type: PrimitiveType::Float,
        });
        parser
            .simple_type_ids
            .insert("float".into(), PrimitiveType::Float as usize);

        parser.types.push(TypeDefinition::Primitive {
            primitive_type: PrimitiveType::Bool,
        });
        parser
            .simple_type_ids
            .insert("bool".into(), PrimitiveType::Bool as usize);

        parser.reset_tokens();

        parser
    }

    pub fn get_text(&self, text_start: usize, text_end: usize) -> &str {
        std::str::from_utf8(&self.lexer.get_source()[text_start..text_end])
            .expect("Couldn't convert source slice to string")
    }

    fn reset_tokens(&mut self) {
        self.lexer.reset_position();

        // Call next token twice to initialize both current and peek.
        self.next_token();
        self.next_token();
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.get_token();
    }

    fn token_to_array_length(&mut self, token: Token) -> usize {
        let length_text = self.get_text(token.text_start, token.text_end);

        match length_text.parse::<usize>() {
            Ok(length) => length,
            Err(_) => {
                self.abort(&format!("Invalid array length \"{}\"", length_text,));
                unreachable!()
            }
        }
    }

    fn get_type_id(&mut self) -> usize {
        let start_token = self.current_token;
        self.next_token();
        if !self.check_token(TokenType::LBracket) {
            // This is just a simple type.
            let text = self.get_text(start_token.text_start, start_token.text_end);
            return *self
                .simple_type_ids
                .get(text)
                .unwrap_or(&(PrimitiveType::Void as usize));
        }

        // TODO: This code only supports single dimensional arrays right now.
        // This is an array.
        self.next_token();
        let length_token = self.current_token;
        self.next_token();
        self.match_token(TokenType::RBracket);

        let length = self.token_to_array_length(length_token);

        let text = self.get_text(start_token.text_start, start_token.text_end);
        let element_type_id = *self
            .simple_type_ids
            .get(text)
            .unwrap_or(&(PrimitiveType::Void as usize));

        let key = (text.into(), length);
        if let Some(existing_type_id) = self.array_type_ids.get(&key) {
            return *existing_type_id;
        }

        let type_id = self.types.len();
        self.types.push(TypeDefinition::Array {
            element_type_id,
            length,
        });
        self.array_type_ids.insert(key, type_id);

        type_id
    }

    fn abort(&self, message: &str) {
        report_error(
            self.lexer.get_source(),
            message,
            self.current_token.text_start,
        );
    }

    fn check_token(&self, token_type: TokenType) -> bool {
        self.current_token.token_type == token_type
    }

    fn check_peek(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn match_token(&mut self, token_type: TokenType) {
        self.match_in_place(token_type);
        self.next_token();
    }

    fn match_in_place(&mut self, token_type: TokenType) {
        if !self.check_token(token_type) {
            self.abort(&format!(
                "Expected token {:?}, got {:?}",
                token_type, self.current_token.token_type
            ));
        }
    }

    fn add_node(&mut self, node: Node) -> usize {
        if self.is_skipping_nodes {
            return 0;
        }

        let node_index = self.ast.len();
        self.ast.push(node);
        node_index
    }

    /*
     * Rules
     */

    // program ::= {function | struct}
    pub fn program(&mut self) -> usize {
        let text_start = self.current_token.text_start;

        while self.check_token(TokenType::Newline) {
            self.next_token();
        }

        let mut function_indices = Vec::new();
        let mut struct_indices = Vec::new();

        // TODO: Is this the correct way to go about it, or is there a way to parse functions without first parsing structs?
        /*
         * Parsing is done in multiple passes:
         * 1. Parse types like structs, so they are ready to be used in functions.
         * 2. Parse functions, using the types previously parsed.
         * When a node doesn't need to be parsed in the current passed, it is still
         * technically parsed in order to skip the correct amount of tokens,
         * but no nodes are added to the AST.
         */
        for i in 0..2 {
            while !self.check_token(TokenType::Eof) {
                if self.check_token(TokenType::Function) {
                    if i == 0 {
                        self.is_skipping_nodes = true;
                    }

                    if let Some(function_index) = self.function() {
                        function_indices.push(function_index)
                    }
                } else if self.check_token(TokenType::Struct) {
                    if i == 1 {
                        self.is_skipping_nodes = true;
                    }

                    if let Some(struct_index) = self.struct_node() {
                        struct_indices.push(struct_index)
                    }
                } else {
                    self.abort("Expected function or struct definition");
                }

                self.is_skipping_nodes = false;
                self.newline();
            }

            self.match_token(TokenType::Eof);
            self.reset_tokens();
        }

        self.add_node(Node {
            node_type: NodeType::Program {
                function_indices: Arc::new(function_indices),
                struct_indices: Arc::new(struct_indices),
            },
            node_start: text_start,
        })
    }

    // block :: {statement} terminator
    fn block(&mut self, terminator: TokenType) -> usize {
        let text_start = self.current_token.text_start;
        let mut statement_indices = Vec::new();

        while !self.check_token(terminator) {
            statement_indices.push(self.statement());
        }

        self.match_token(terminator);

        self.add_node(Node {
            node_type: NodeType::Block {
                statement_indices: Arc::new(statement_indices),
            },
            node_start: text_start,
        })
    }

    // TODO: Consider using semicolons to terminate lines, allowing newlines to be treated as whitespace?
    fn newline(&mut self) {
        if !self.check_token(TokenType::Newline) && !self.check_token(TokenType::Eof) {
            self.abort("Expected line to be terminated")
        }

        while self.check_token(TokenType::Newline) {
            self.next_token();
        }
    }

    fn statement(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        let node_type;

        // "print" (expression | string)
        if self.check_token(TokenType::Print) {
            self.next_token();

            if self.check_token(TokenType::String) {
                let string_start = self.current_token.text_start;
                let string_index = self.add_node(Node {
                    node_type: NodeType::String {
                        text_start: string_start,
                        text_end: self.current_token.text_end,
                    },
                    node_start: string_start,
                });
                node_type = Some(NodeType::StatementPrintString { string_index });
                self.next_token();
            } else {
                let expression_index = self.expression();
                node_type = Some(NodeType::StatementPrintExpression { expression_index });
            }
        }
        // "if" comparison "then" newline block "endif" newline
        else if self.check_token(TokenType::If) {
            self.next_token();
            let comparison_index = self.comparison();
            self.match_token(TokenType::Then);
            self.newline();
            let block_index = self.block(TokenType::EndIf);

            node_type = Some(NodeType::StatementIf {
                comparison_index,
                block_index,
            });
        }
        // "while" comparison "repeat" block "endwhile"
        else if self.check_token(TokenType::While) {
            self.next_token();
            let comparison_index = self.comparison();
            self.match_token(TokenType::Repeat);
            self.newline();
            let block_index = self.block(TokenType::EndWhile);

            node_type = Some(NodeType::StatementWhile {
                comparison_index,
                block_index,
            });
        }
        // "let" ident ":" type "=" expression
        else if self.check_token(TokenType::Let) {
            self.next_token();
            let name_start = self.current_token.text_start;
            let name_end = self.current_token.text_end;
            self.next_token();
            self.match_token(TokenType::Colon);
            let type_id = self.get_type_id();
            self.match_token(TokenType::Eq);
            let expression_index = self.expression();

            node_type = Some(NodeType::StatementVariableDeclaration {
                name_start,
                name_end,
                type_id,
                expression_index,
            });
        }
        // variable "=" expression
        else if self.check_token(TokenType::Ident) && !self.check_peek(TokenType::LParen) {
            let variable_index = self.variable();
            self.match_token(TokenType::Eq);
            let expression_index = self.expression();

            node_type = Some(NodeType::StatementVariableAssignment {
                variable_index,
                expression_index,
            });
        }
        // return ::= "return" [expression]
        else if self.check_token(TokenType::Return) {
            self.next_token();

            if self.check_token(TokenType::Newline) {
                node_type = Some(NodeType::StatementReturn {});
            } else {
                let expression_index = self.expression();
                node_type = Some(NodeType::StatementReturnValue { expression_index });
            }
        }
        // expression
        else {
            let expression_index = self.expression();

            node_type = Some(NodeType::StatementExpression { expression_index });
        }

        self.newline();
        self.add_node(Node {
            node_type: node_type.expect("Failed to create node from statement"),
            node_start: text_start,
        })
    }

    // TODO: Make sure function names don't collide with others when C is generated. Maybe don't include C headers in the same file as the output, put all of the C code in a seperate file and then just import it's header in the main output code?
    // "function" ident parameters "do" block "endfunction"
    fn function(&mut self) -> Option<usize> {
        let text_start = self.current_token.text_start;
        self.next_token();

        let name_start = self.current_token.text_start;
        let name_end = self.current_token.text_end;
        self.next_token();
        let parameters_index = self.parameters();
        self.match_token(TokenType::Do);
        self.newline();
        let block_index = self.block(TokenType::EndFunction);

        if self.is_skipping_nodes {
            return None;
        }

        Some(self.add_node(Node {
            node_type: NodeType::Function {
                name_start,
                name_end,
                parameters_index,
                block_index,
            },
            node_start: text_start,
        }))
    }

    fn struct_node(&mut self) -> Option<usize> {
        let text_start = self.current_token.text_start;
        self.next_token();

        let name_start = self.current_token.text_start;
        let name_end = self.current_token.text_end;
        self.next_token();
        self.match_token(TokenType::Of);
        self.newline();

        let mut field_list = Vec::new();

        while !self.check_token(TokenType::EndStruct) {
            let field_name_start = self.current_token.text_start;
            let field_name_end = self.current_token.text_end;
            self.next_token();
            self.match_token(TokenType::Colon);
            let type_id = self.get_type_id();
            self.newline();

            field_list.push(Field {
                name_start: field_name_start,
                name_end: field_name_end,
                type_id,
            });
        }

        self.match_token(TokenType::EndStruct);

        let field_list = Arc::new(field_list);

        if self.is_skipping_nodes {
            return None;
        }

        let name = self.get_text(name_start, name_end).into();
        if self.simple_type_ids.contains_key(&name) {
            self.abort(&format!("Duplicate struct definition for \"{}\"", name));
        }

        let struct_type_id = self.types.len();
        self.types.push(TypeDefinition::Struct {
            name_start,
            name_end,
            field_list: field_list.clone(),
        });
        self.simple_type_ids.insert(name, struct_type_id);

        Some(self.add_node(Node {
            node_type: NodeType::Struct {
                name_start,
                name_end,
                field_list,
            },
            node_start: text_start,
        }))
    }

    // parameters ::= "(" ("," ident ":" type)* ")" ":" type
    fn parameters(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        self.match_token(TokenType::LParen);

        let mut list = Vec::new();
        while !self.check_token(TokenType::RParen) {
            if !list.is_empty() {
                self.match_token(TokenType::Comma);
            }

            let name_start = self.current_token.text_start;
            let name_end = self.current_token.text_end;
            self.next_token();
            self.match_token(TokenType::Colon);
            let type_id = self.get_type_id();

            list.push(Parameter {
                name_start,
                name_end,
                type_id,
            });
        }

        self.match_token(TokenType::RParen);
        self.match_token(TokenType::Colon);
        let return_type_id = self.get_type_id();

        self.add_node(Node {
            node_type: NodeType::Parameters {
                input_list: Arc::new(list),
                return_type_id,
            },
            node_start: text_start,
        })
    }

    // arguments ::= "(" ("," expression)* ")"
    fn arguments(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        self.match_token(TokenType::LParen);

        let mut expression_indices = Vec::new();
        while !self.check_token(TokenType::RParen) {
            if !expression_indices.is_empty() {
                self.match_token(TokenType::Comma);
            }

            expression_indices.push(self.expression());
        }

        self.match_token(TokenType::RParen);

        self.add_node(Node {
            node_type: NodeType::Arguments {
                expression_indices: Arc::new(expression_indices),
            },
            node_start: text_start,
        })
    }

    // comparison ::= expression ("==" | "!=" | ">" | ">=" | "<" | "<=") expression
    fn comparison(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        let left_expression_index = self.expression();

        let op = match token_type_to_comparison_op(self.current_token.token_type) {
            Some(op) => op,
            None => {
                let current_token_string =
                    self.get_text(self.current_token.text_start, self.current_token.text_end);
                self.abort(&format!(
                    "Expected comparsion operator at: \"{}\"",
                    current_token_string
                ));
                unreachable!()
            }
        };
        self.next_token();

        let right_expression_index = self.expression();

        self.add_node(Node {
            node_type: NodeType::Comparison {
                left_expression_index,
                op,
                right_expression_index,
            },
            node_start: text_start,
        })
    }

    // expression ::= term {("-" | "+") term}
    fn expression(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        let term_index = self.term();
        let mut trailing_terms = Vec::new();

        while self.check_token(TokenType::Plus) || self.check_token(TokenType::Minus) {
            let op = match self.current_token.token_type {
                TokenType::Plus => ExpressionOp::Plus,
                TokenType::Minus => ExpressionOp::Minus,
                _ => unreachable!(),
            };

            self.next_token();
            let term_index = self.term();

            trailing_terms.push(TrailingTerm { op, term_index })
        }

        self.add_node(Node {
            node_type: NodeType::Expression {
                term_index,
                trailing_terms: Arc::new(trailing_terms),
            },
            node_start: text_start,
        })
    }

    // term ::= unary {("/" | "*") unary}
    fn term(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        let unary_index = self.unary();
        let mut trailing_unaries = Vec::new();

        while self.check_token(TokenType::Asterisk) || self.check_token(TokenType::Slash) {
            let op = match self.current_token.token_type {
                TokenType::Asterisk => TermOp::Multiply,
                TokenType::Slash => TermOp::Divide,
                _ => unreachable!(),
            };

            self.next_token();
            let unary_index = self.unary();

            trailing_unaries.push(TrailingUnary { op, unary_index })
        }

        self.add_node(Node {
            node_type: NodeType::Term {
                unary_index,
                trailing_unaries: Arc::new(trailing_unaries),
            },
            node_start: text_start,
        })
    }

    // unary ::= ["+" | "-"] call
    fn unary(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        let op = match self.current_token.token_type {
            TokenType::Plus => Some(UnaryOp::Plus),
            TokenType::Minus => Some(UnaryOp::Minus),
            _ => None,
        };

        let call_index = self.call();

        self.add_node(Node {
            node_type: NodeType::Unary { op, call_index },
            node_start: text_start,
        })
    }

    // call ::= ident arguments | primary
    fn call(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        if !self.check_token(TokenType::Ident) || !self.check_peek(TokenType::LParen) {
            let primary_index = self.primary();
            return self.add_node(Node {
                node_type: NodeType::CallPrimary { primary_index },
                node_start: text_start,
            });
        }

        let name_start = self.current_token.text_start;
        let name_end = self.current_token.text_end;
        self.next_token();

        let arguments_index = self.arguments();

        self.add_node(Node {
            node_type: NodeType::CallFunction {
                name_start,
                name_end,
                arguments_index,
            },
            node_start: text_start,
        })
    }

    // primary ::= int | float | bool | variable
    fn primary(&mut self) -> usize {
        let text_start = self.current_token.text_start;
        let first_token_text = self.get_text(text_start, self.current_token.text_end);
        let node_type = match self.current_token.token_type {
            TokenType::IntLiteral => NodeType::PrimaryInt {
                text_start,
                text_end: self.current_token.text_end,
            },
            TokenType::FloatLiteral => NodeType::PrimaryFloat {
                text_start,
                text_end: self.current_token.text_end,
            },
            TokenType::True => NodeType::PrimaryBool { is_true: true },
            TokenType::False => NodeType::PrimaryBool { is_true: false },
            TokenType::Ident if self.check_peek(TokenType::LBrace) => {
                let name_start = text_start;
                let name_end = self.current_token.text_end;
                self.next_token();
                self.match_token(TokenType::LBrace);
                self.newline();

                let mut argument_list = Vec::new();

                while !self.check_token(TokenType::RBrace) {
                    let field_name_start = self.current_token.text_start;
                    let field_name_end = self.current_token.text_end;
                    self.next_token();
                    self.match_token(TokenType::Colon);
                    let expression_index = self.expression();
                    self.newline();

                    argument_list.push(NamedArgument {
                        name_start: field_name_start,
                        name_end: field_name_end,
                        expression_index,
                    });
                }

                NodeType::PrimaryStruct {
                    name_start,
                    name_end,
                    argument_list: Arc::new(argument_list),
                }
            }
            // This could be an array literal or an array access.
            // We'll finding by checking if this is the name of a type.
            // TODO: Doesn't support nested arrays yet, because we only check simple types.
            TokenType::Ident
                if self.check_peek(TokenType::LBracket)
                    && self.simple_type_ids.contains_key(first_token_text) =>
            {
                let name_start = text_start;
                let name_end = self.current_token.text_end;

                self.next_token();
                self.next_token();

                let length = self.token_to_array_length(self.current_token);
                self.next_token();
                self.match_in_place(TokenType::RBracket);

                NodeType::PrimaryArray {
                    name_start,
                    name_end,
                    length,
                }
            }
            TokenType::Ident => {
                let node = Node {
                    node_type: NodeType::PrimaryVariable {
                        variable_index: self.variable(),
                    },
                    node_start: text_start,
                };
                return self.add_node(node);
            }
            _ => {
                let current_token_string =
                    self.get_text(self.current_token.text_start, self.current_token.text_end);
                self.abort(&format!("Unexpected token at \"{}\"", current_token_string));
                unreachable!()
            }
        };

        self.next_token();

        self.add_node(Node {
            node_type,
            node_start: text_start,
        })
    }

    // TODO: This needs to be recursive to support complex access, ie: var.array[other_var].x, or var.array[other_array[other_var]].y
    fn variable(&mut self) -> usize {
        let name_start = self.current_token.text_start;
        let name_end = self.current_token.text_end;

        self.next_token();

        let node_type = if self.check_token(TokenType::LBracket) {
            self.next_token();
            let expression_index = self.expression();
            self.match_token(TokenType::RBracket);

            NodeType::VariableArrayAccess {
                name_start,
                name_end,
                expression_index,
            }
        } else if self.check_token(TokenType::Period) {
            let mut field_list = Vec::new();

            loop {
                self.match_token(TokenType::Period);
                let field_name_start = self.current_token.text_start;
                let field_name_end = self.current_token.text_end;

                field_list.push(FieldName {
                    name_start: field_name_start,
                    name_end: field_name_end,
                });

                self.next_token();
                if !self.check_token(TokenType::Period) {
                    break;
                }
            }

            NodeType::VariableField {
                name_start,
                name_end,
                field_list: Arc::new(field_list),
            }
        } else {
            NodeType::VariableIdent {
                text_start: name_start,
                text_end: name_end,
            }
        };

        self.add_node(Node {
            node_type,
            node_start: name_start,
        })
    }
}
