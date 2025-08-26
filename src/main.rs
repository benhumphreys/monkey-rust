#![allow(unused)]

use std::io;
use std::io::{BufRead, Write};
use monkey::evaluator::eval_program;
use monkey::lexer::Lexer;
use monkey::parser::{ParseError, Parser};

fn main() {
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
            let evaluated = eval_program(&program);
            println!("{}", evaluated);
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