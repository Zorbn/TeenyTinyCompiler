use std::sync::Arc;

use crate::{lexer::{Lexer, Token, TokenType}, error_reporting::get_line_number};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Void,
    Int,
    Float,
    Bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ReturnType(ValueType);

impl ReturnType {
    pub fn from(value_type: ValueType) -> ReturnType {
        ReturnType(value_type)
    }

    pub fn to_value_type(self) -> ValueType {
        self.0
    }
}

fn token_type_to_value_type(token_type: TokenType) -> ValueType {
    match token_type {
        TokenType::Int => ValueType::Int,
        TokenType::Float => ValueType::Float,
        TokenType::Bool => ValueType::Bool,
        _ => ValueType::Void,
    }
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
pub struct Parameter {
    pub name_start: usize,
    pub name_end: usize,
    pub value_type: ValueType,
}

#[derive(Debug, Clone)]
pub enum Node {
    Program {
        function_indices: Arc<Vec<usize>>,
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
    PrimaryIdent {
        text_start: usize,
        text_end: usize,
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
        value_type: ValueType,
        expression_index: usize,
    },
    StatementVariableAssignment {
        name_start: usize,
        name_end: usize,
        expression_index: usize,
    },
    StatementReturnValue {
        expression_index: usize,
    },
    StatementReturn{
    },
    StatementExpression {
        expression_index: usize,
    },
    // TODO: Ensure function declarations can't be shadowed or nested.
    Function {
        name_start: usize,
        name_end: usize,
        parameters_index: usize,
        block_index: usize,
    },
    Parameters {
        input_list: Arc<Vec<Parameter>>,
        return_type: ReturnType,
    },
}

// TODO: Determine how to organize the following operations:
// - Type checking
// - Check if all used variables/functions were declared first
// - Emit code based on resulting AST from the parser

pub struct Parser {
    pub ast: Vec<Node>,
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            ast: Vec::new(),
            lexer,
            current_token: Token::from_single(0, TokenType::Eof),
            peek_token: Token::from_single(0, TokenType::Eof),
        };

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

    fn abort(&self, message: &str) {
        let line_number = get_line_number(self.lexer.get_source(), self.current_token.text_start);
        println!("Parsing error at line {line_number}! {message}");
        std::process::exit(-2);
    }

    fn check_token(&self, token_type: TokenType) -> bool {
        self.current_token.token_type == token_type
    }

    fn check_peek(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn match_token(&mut self, token_type: TokenType) {
        if !self.check_token(token_type) {
            self.abort(&format!(
                "Expected token {:?}, got {:?}",
                token_type, self.current_token.token_type
            ));
        }

        self.next_token();
    }

    fn add_node(&mut self, node: Node) -> usize {
        let node_index = self.ast.len();
        self.ast.push(node);
        node_index
    }

    /*
     * Rules
     */

    // program ::= {function}
    pub fn program(&mut self) -> usize {
        while self.check_token(TokenType::Newline) {
            self.next_token();
        }

        let mut function_indices = Vec::new();

        while !self.check_token(TokenType::Eof) {
            function_indices.push(self.function());
            self.newline();
        }

        self.match_token(TokenType::Eof);

        self.add_node(Node::Program { function_indices: Arc::new(function_indices) })
    }

    // block :: {statement} terminator
    fn block(&mut self, terminator: TokenType) -> usize {
        let mut statement_indices = Vec::new();

        while !self.check_token(terminator) {
            statement_indices.push(self.statement());
        }

        self.match_token(terminator);

        self.add_node(Node::Block { statement_indices: Arc::new(statement_indices) })
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
        let node;

        // "print" (expression | string)
        if self.check_token(TokenType::Print) {
            self.next_token();

            if self.check_token(TokenType::String) {
                let string_index = self.add_node(Node::String {
                    text_start: self.current_token.text_start,
                    text_end: self.current_token.text_end,
                });
                node = Some(Node::StatementPrintString { string_index });
                self.next_token();
            } else {
                let expression_index = self.expression();
                node = Some(Node::StatementPrintExpression { expression_index });
            }
        }
        // "if" comparison "then" newline block "endif" newline
        else if self.check_token(TokenType::If) {
            self.next_token();
            let comparison_index = self.comparison();
            self.match_token(TokenType::Then);
            self.newline();
            let block_index = self.block(TokenType::EndIf);

            node = Some(Node::StatementIf {
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

            node = Some(Node::StatementWhile {
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
            let value_type = token_type_to_value_type(self.current_token.token_type);
            self.next_token();
            self.match_token(TokenType::Eq);
            let expression_index = self.expression();

            node = Some(Node::StatementVariableDeclaration {
                name_start,
                name_end,
                value_type,
                expression_index,
            });
        }
        // ident "=" expression
        else if self.check_token(TokenType::Ident) && self.check_peek(TokenType::Eq) {
            let name_start = self.current_token.text_start;
            let name_end = self.current_token.text_end;
            self.next_token();
            self.match_token(TokenType::Eq);
            let expression_index = self.expression();

            node = Some(Node::StatementVariableAssignment {
                name_start,
                name_end,
                expression_index,
            });
        }
        // return ::= "return" [expression]
        else if self.check_token(TokenType::Return) {
            self.next_token();

            if self.check_token(TokenType::Newline) {
                node = Some(Node::StatementReturn{});
            } else {
                let expression_index = self.expression();
                node = Some(Node::StatementReturnValue { expression_index });
            }
        }
        // expression
        else {
            let expression_index = self.expression();

            node = Some(Node::StatementExpression { expression_index });
        }

        self.newline();
        self.add_node(node.expect("Failed to create node from statement"))
    }

    // TODO: Make sure function names don't collide with others when C is generated. Maybe don't include C headers in the same file as the output, put all of the C code in a seperate file and then just import it's header in the main output code?
    // "function" ident parameters "do" block "endfunction"
    fn function(&mut self) -> usize {
        self.next_token();

        let name_start = self.current_token.text_start;
        let name_end = self.current_token.text_end;
        self.next_token();
        let parameters_index = self.parameters();
        self.match_token(TokenType::Do);
        self.newline();
        let block_index = self.block(TokenType::EndFunction);

        self.add_node(Node::Function {
            name_start,
            name_end,
            parameters_index,
            block_index,
        })
    }

    // parameters ::= "(" ("," ident ":" type)* ")" ":" type
    fn parameters(&mut self) -> usize {
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
            let value_type = token_type_to_value_type(self.current_token.token_type);
            self.next_token();

            list.push(Parameter {
                name_start,
                name_end,
                value_type,
            });
        }

        self.match_token(TokenType::RParen);
        self.match_token(TokenType::Colon);
        let return_type = ReturnType(token_type_to_value_type(self.current_token.token_type));
        self.next_token();

        self.add_node(Node::Parameters { input_list: Arc::new(list), return_type })
    }

    // arguments ::= "(" ("," expression)* ")"
    fn arguments(&mut self) -> usize {
        self.match_token(TokenType::LParen);

        let mut expression_indices = Vec::new();
        while !self.check_token(TokenType::RParen) {
            if !expression_indices.is_empty() {
                self.match_token(TokenType::Comma);
            }

            expression_indices.push(self.expression());
        }

        self.match_token(TokenType::RParen);

        self.add_node(Node::Arguments { expression_indices: Arc::new(expression_indices) })
    }

    // comparison ::= expression ("==" | "!=" | ">" | ">=" | "<" | "<=") expression
    fn comparison(&mut self) -> usize {
        let left_expression_index = self.expression();

        let op = match token_type_to_comparison_op(self.current_token.token_type) {
            Some(op) => op,
            None => {
                let current_token_string = self.get_text(self.current_token.text_start, self.current_token.text_end);
                self.abort(&format!(
                    "Expected comparsion operator at: \"{}\"",
                    current_token_string
                ));
                unreachable!()
            }
        };
        self.next_token();

        let right_expression_index = self.expression();

        self.add_node(Node::Comparison {
            left_expression_index,
            op,
            right_expression_index,
        })
    }

    // expression ::= term {("-" | "+") term}
    fn expression(&mut self) -> usize {
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

        self.add_node(Node::Expression {
            term_index,
            trailing_terms: Arc::new(trailing_terms),
        })
    }

    // term ::= unary {("/" | "*") unary}
    fn term(&mut self) -> usize {
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

        self.add_node(Node::Term {
            unary_index,
            trailing_unaries: Arc::new(trailing_unaries),
        })
    }

    // unary ::= ["+" | "-"] call
    fn unary(&mut self) -> usize {
        let op = match self.current_token.token_type {
            TokenType::Plus => Some(UnaryOp::Plus),
            TokenType::Minus => Some(UnaryOp::Minus),
            _ => None,
        };

        let call_index = self.call();

        self.add_node(Node::Unary { op, call_index })
    }

    // call ::= ident arguments | primary
    fn call(&mut self) -> usize {
        if !self.check_token(TokenType::Ident) || !self.check_peek(TokenType::LParen) {
            let primary_index = self.primary();
            return self.add_node(Node::CallPrimary { primary_index });
        }

        let name_start = self.current_token.text_start;
        let name_end = self.current_token.text_end;
        self.next_token();

        let arguments_index = self.arguments();

        self.add_node(Node::CallFunction {
            name_start,
            name_end,
            arguments_index,
        })
    }

    // primary ::= int | float | bool | ident
    fn primary(&mut self) -> usize {
        let index = match self.current_token.token_type {
            TokenType::Int => self.add_node(Node::PrimaryInt {
                text_start: self.current_token.text_start,
                text_end: self.current_token.text_end,
            }),
            TokenType::Float => self.add_node(Node::PrimaryFloat {
                text_start: self.current_token.text_start,
                text_end: self.current_token.text_end,
            }),
            TokenType::True => self.add_node(Node::PrimaryBool {
                is_true: true,
            }),
            TokenType::False => self.add_node(Node::PrimaryBool {
                is_true: false,
            }),
            TokenType::Ident => self.add_node(Node::PrimaryIdent {
                text_start: self.current_token.text_start,
                text_end: self.current_token.text_end,
            }),
            _ => {
                let current_token_string = self.get_text(self.current_token.text_start, self.current_token.text_end);
                self.abort(&format!("Unexpected token at \"{}\"", current_token_string));
                unreachable!()
            }
        };

        self.next_token();

        index
    }
}
