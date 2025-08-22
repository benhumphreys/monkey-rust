#![allow(dead_code)]

use crate::ast::{Expression, Program, Statement};
use crate::object::Object;
use crate::object::Object::Null;

pub fn eval_program(program: &Program) -> Object {
    let mut result: Box<Object> = Box::new(Null);
    for stmt in program.statements.clone() {
        result = Box::new(eval_statement(&stmt));
    }
    *result
}

fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::LetStatement(_, _, _) => {todo!()}
        Statement::ReturnStatement(_, _) => {todo!()}
        Statement::ExpressionStatement(_, expression) => { eval_expression(expression)}
        Statement::BlockStatement(_, _) => {todo!()}
    }
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        Expression::Identifier(_, _) => {todo!()}
        Expression::IntegerLiteral(_, value) => { Object::Integer(*value)}
        Expression::Boolean(_, _) => {todo!()}
        Expression::PrefixExpression(_, _, _) => {todo!()}
        Expression::InfixExpression(_, _, _, _) => {todo!()}
        Expression::IfExpression(_, _, _, _) => {todo!()}
        Expression::FunctionLiteral(_, _, _) => {todo!()}
        Expression::CallExpression(_, _, _) => {todo!()}
        Expression::Nil => {todo!()}
    }
}