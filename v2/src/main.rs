mod checker;
mod code_generator;
mod emitter;
mod environment;
mod lexer;
mod parser;

use code_generator::CodeGenerator;
use lexer::Lexer;
use parser::Parser;

use crate::checker::Checker;

const CC: &str = "clang";
const CC_FLAGS: [&str; 2] = ["-std=c99", "-O3"];
const OUTPUT_DIR: &str = "build";
const OUTPUT_SRC: &str = "out.c";

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 2 {
        abort("Invalid command!", -1);
    }

    if args[1] == "help" {
        print_usage();
        return;
    }

    let file_bytes = std::fs::read(&args[1]).unwrap();
    let lexer = Lexer::new(file_bytes);
    let mut parser = Parser::new(lexer);
    let program_index = parser.program();

    println!("Checking code...");
    let mut checker = Checker::new(&parser);
    checker.check(program_index);

    if std::fs::create_dir_all(OUTPUT_DIR).is_err() {
        abort("Couldn't create output directory!", -2)
    }

    let output_src_path = format!("{}/{}", OUTPUT_DIR, OUTPUT_SRC);
    println!("Emitting code...");
    let mut code_generator = CodeGenerator::new(parser);
    code_generator.emit(program_index, &output_src_path);

    let output_exe_extension = if cfg!(target_os = "windows") {
        ".exe"
    } else {
        ""
    };
    let output_exe_path = format!("{}/out{}", OUTPUT_DIR, output_exe_extension);
    println!("Calling system compiler...");
    if std::process::Command::new(CC)
        .args([&output_src_path, "-o", &output_exe_path])
        .args(CC_FLAGS)
        .output()
        .is_err()
    {
        abort("Couldn't compile using the system compiler!", -3)
    }

    println!("Finished compiling!");
}

fn abort(message: &str, exit_code: i32) {
    println!("{}", message);
    print_usage();
    std::process::exit(exit_code);
}

fn print_usage() {
    println!("Usage:\nttc source\tcompile a source file\nttc help\tshow this message");
}
