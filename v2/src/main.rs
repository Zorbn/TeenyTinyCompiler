mod lexer;
mod parser;
mod code_generator;
mod emitter;

use lexer::Lexer;
use parser::Parser;
use code_generator::CodeGenerator;

fn main() {
    let file_bytes = std::fs::read("test.teeny").unwrap();
    let lexer = Lexer::new(file_bytes);
    let parser = Parser::new(lexer);
    let mut code_generator = CodeGenerator::new(parser);
    code_generator.emit();

    // let mut token = lexer.get_token();
    // while token.token_type != lexer::TokenType::Eof {
    //     println!("{:?} \"{}\"", token.token_type, String::from_utf8_lossy(lexer.get_token_text(&token)));
    //     token = lexer.get_token();
    // }
}
