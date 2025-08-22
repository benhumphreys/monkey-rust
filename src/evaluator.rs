#![allow(dead_code)]

use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

const OBJECT_BOOLEAN_TRUE: Object = Object::Boolean(true);
const OBJECT_BOOLEAN_FALSE: Object = Object::Boolean(false);
const OBJECT_NULL: Object = Object::Null;

pub fn eval_program(program: &Program) -> Object {
    let mut result: Box<Object> = Box::new(OBJECT_NULL);
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
        Expression::IntegerLiteral(_, value) => { Object::Integer(*value) }
        Expression::Boolean(_, value) => { if *value { OBJECT_BOOLEAN_TRUE } else { OBJECT_BOOLEAN_FALSE } }
        Expression::PrefixExpression(_, operator, right) => {
            let evaluated_right = eval_expression(&right);
            eval_prefix_expression(operator, evaluated_right)
        }
        Expression::InfixExpression(_, _, _, _) => {todo!()}
        Expression::IfExpression(_, _, _, _) => {todo!()}
        Expression::FunctionLiteral(_, _, _) => {todo!()}
        Expression::CallExpression(_, _, _) => {todo!()}
        Expression::Nil => {todo!()}
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => { eval_bang_operator_expression(right)}
        "-" => { eval_minus_prefix_operator_expression(right)}
        _ => {panic!()}
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(value) => { if value { OBJECT_BOOLEAN_FALSE } else { OBJECT_BOOLEAN_TRUE } },
        Object::Null => OBJECT_BOOLEAN_TRUE,
        _ => OBJECT_BOOLEAN_FALSE
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::Integer(value) = right {
        Object::Integer(-value)
    } else {
       panic!()
    }
}
