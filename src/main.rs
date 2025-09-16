#![allow(unused)]

use monkey::compiler::Compiler;
use monkey::lexer::Lexer;
use monkey::parser::{ParseError, Parser};
use monkey::vm::Vm;
use std::io::{BufRead, Read, Write};
use std::{env, io};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(),
        2 => execute(args[1].clone()),
        _ => panic!("usage: monkey <script>")
    }
}

fn execute(filename: String) {
    let mut file = std::fs::File::open(filename).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let mut lexer = Lexer::new(contents);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    if (!parser.errors().is_empty()) {
        print_parser_errors(&parser.errors());
        return;
    }

    let mut comp = Compiler::new();
    let result = comp.compile(&program);
    if result.is_err() {
        println!("Compilation failed: {}", result.unwrap_err());
    }

    let mut vm = Vm::new(&comp.bytecode());
    vm.run();
}

fn repl() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    println!("Monkey programming language");
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        if let Some(line) = lines.next() {
            let mut lexer = Lexer::new(line.unwrap());
            let mut parser = Parser::new(&mut lexer);

            let program = parser.parse_program();
            if (!parser.errors().is_empty()) {
                print_parser_errors(&parser.errors());
                continue;
            }

            let mut comp = Compiler::new();
            let result = comp.compile(&program);
            if result.is_err() {
                println!("Compilation failed: {}", result.unwrap_err());
                continue;
            }

            let mut vm = Vm::new(&comp.bytecode());
            vm.run();
            let stack_top = vm.last_popped_stack_elem();
            println!("{}", stack_top);
        } else {
            break;
        }
    }
}

fn print_parser_errors(errors: &Vec<ParseError>) {
    for error in errors {
        eprintln!("\terror: {}", error);
    }
}