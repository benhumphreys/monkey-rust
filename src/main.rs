#![allow(unused)]

use monkey::lexer::Lexer;

fn main() {
    let lexer = Lexer::new(String::from("let x = 1 + 1;"));
}
