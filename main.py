"""
POSSIBLE IMPROVEMENTS:
- Port to another language?
- Types (other than float).
- Add line numbers when reporting errors.
- Ability to declare extern c functions and call them.
- Add an else branch to the if statement, update does_return tracking accordingly.
- Logical and/or operators.
- For loop.
- Structures.
- Main function.
- Multiple files, importing.
- Heap allocations (w/ destructors?).
- Arrays.
- #line directives in generated code for better debugging: https://www.reddit.com/r/ProgrammingLanguages/comments/11i6kqh/what_could_go_wrong_making_a_vm_a_feeling_of_dread/
"""

import sys
import os
import subprocess

from lex import *
from emit import *
from parse import *

CC = "clang"
OUTPUT_DIR = "build"
OUTPUT_SRC = f"{OUTPUT_DIR}/out.c"
OUTPUT_EXE_EXTENSION = ".exe" if os.name == "nt" else ""

def main():
    # lexer = Lexer("FUNCTION testingTesting(a, b, c) DO")

    # token = lexer.get_token()
    # while token.kind != TokenType.EOF:
    #     print(token.kind)
    #     token = lexer.get_token()

    # return

    print("Teeny Tiny Compiler")

    if len(sys.argv) != 2:
        sys.exit("Error: source file not supplied.")

    with open(sys.argv[1], 'r') as input_file:
        source = input_file.read()

    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    lexer = Lexer(source)
    emitter = Emitter(OUTPUT_SRC)
    parser = Parser(lexer, emitter)

    parser.program()
    emitter.write_file()

    input_name = os.path.splitext(sys.argv[1])[0]
    output_exe = f"{OUTPUT_DIR}/{input_name}{OUTPUT_EXE_EXTENSION}"
    subprocess.run([CC, OUTPUT_SRC, "-std=c99", "-o", output_exe])

    print("Compiling completed.")

if __name__ == "__main__":
    main()