mod lexer;
mod parser;
mod code_generator;
mod emitter;

use lexer::Lexer;
use parser::Parser;
use code_generator::CodeGenerator;

fn main() {
    let file_bytes = std::fs::read("functions.teeny").unwrap();
    let lexer = Lexer::new(file_bytes);
    let parser = Parser::new(lexer);
    let mut code_generator = CodeGenerator::new(parser);
    code_generator.emit();
}
