#![allow(dead_code)]

use crate::ast::{BlockStatement, Expression, Program, Statement};
use crate::object::Object;

const OBJECT_BOOLEAN_TRUE: Object = Object::Boolean(true);
const OBJECT_BOOLEAN_FALSE: Object = Object::Boolean(false);
const OBJECT_NULL: Object = Object::Null;

pub fn eval_program(program: &Program) -> Object {
    let mut result = OBJECT_NULL;

    for stmt in program.statements.clone() {
        result = eval_statement(&stmt);
        if let Object::ReturnValue(value) = result {
            return *value;
        }
    }

    result
}

fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::LetStatement(_, ident, expression) => {todo!()}
        Statement::ReturnStatement(_, expression) => {
            let val = eval_expression(expression);
            Object::ReturnValue(Box::new(val))
        }
        Statement::ExpressionStatement(_, expression) => { eval_expression(expression)}
        Statement::BlockStatement(token, statements) => {
            eval_block_statement(BlockStatement{token: token.clone(), statements: statements.clone()})
        }
    }
}

fn eval_block_statement(block_statement: BlockStatement) -> Object {
    let mut result = OBJECT_NULL;

    for stmt in block_statement.statements {
        result = eval_statement(&stmt);
        if let Object::ReturnValue(_) = result {
            return result
        }
    }
    result
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        Expression::Identifier(_, _) => {todo!()}
        Expression::IntegerLiteral(_, value) => { Object::Integer(*value) }
        Expression::Boolean(_, value) => { if *value { OBJECT_BOOLEAN_TRUE } else { OBJECT_BOOLEAN_FALSE } }
        Expression::PrefixExpression(_, operator, right) => {
            let evaluated_right = eval_expression(right);
            eval_prefix_expression(operator, evaluated_right)
        }
        Expression::InfixExpression(_, left, operator, right) => {
            let evaluated_left = eval_expression(left);
            let evaluated_right = eval_expression(right);
            eval_infix_expression(operator, evaluated_left, evaluated_right)
        }
        Expression::IfExpression(_, condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative)
        }
        Expression::FunctionLiteral(_, _, _) => {todo!()}
        Expression::CallExpression(_, _, _) => {todo!()}
        Expression::Nil => {todo!()}
    }
}

fn eval_if_expression(condition: &Expression, consequence: &BlockStatement, alternative: &Option<BlockStatement>) -> Object {
    if is_truthy(&eval_expression(condition)) {
        eval_block_statement(consequence.clone())
    } else if let Some(alternative_block) = alternative {
        eval_block_statement(alternative_block.clone())
    } else {
        OBJECT_NULL
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let maybe_left = object_to_int(&left);
    let maybe_right = object_to_int(&right);

    if maybe_left.is_some() && maybe_right.is_some() {
        return eval_integer_infix_expression(operator, maybe_left.unwrap(), maybe_right.unwrap())
    } else if operator == "==" {
        return Object::Boolean(left == right);
    } else  if operator == "!=" {
        return Object::Boolean(left != right);
    }

    OBJECT_NULL
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => {panic!()}
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

fn object_to_int(object: &Object) -> Option<i64> {
    if let Object::Integer(value) = object {
        Some(*value)
    } else {
        None
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(value) => *value,
        Object::Null => false,
        _ => true
    }
}