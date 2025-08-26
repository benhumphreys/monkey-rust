#![allow(dead_code)]

use crate::ast::{BlockStatement, Expression, Program, Statement};
use crate::object::{Object, ObjectType};

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
        if let Object::Error(_) = result {
            return result;
        }
    }

    result
}

fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::LetStatement(_, ident, expression) => {todo!()}
        Statement::ReturnStatement(_, expression) => {
            let val = eval_expression(expression);
            if is_error(&val) {
                return val;
            }
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
        if let Object::Error(_) = result {
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
            if is_error(&evaluated_right) {
                return evaluated_right;
            }
            eval_prefix_expression(operator, evaluated_right)
        }
        Expression::InfixExpression(_, left, operator, right) => {
            let evaluated_left = eval_expression(left);
            if is_error(&evaluated_left) {
                return evaluated_left;
            }

            let evaluated_right = eval_expression(right);
            if is_error(&evaluated_right) {
                return evaluated_right;
            }
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
    let evaluated_condition = eval_expression(condition);
    if is_error(&evaluated_condition) {
        return evaluated_condition;
    }

    if is_truthy(&evaluated_condition) {
        eval_block_statement(consequence.clone())
    } else if let Some(alternative_block) = alternative {
        eval_block_statement(alternative_block.clone())
    } else {
        OBJECT_NULL
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let maybe_int_left = object_to_int(&left);
    let maybe_int_right = object_to_int(&right);

    if maybe_int_left.is_some() && maybe_int_right.is_some() {
        eval_integer_infix_expression(operator, maybe_int_left.unwrap(), maybe_int_right.unwrap())
    } else if operator == "==" {
        Object::Boolean(left == right)
    } else if operator == "!=" {
        Object::Boolean(left != right)
    } else if left.object_type() != right.object_type() {
        Object::Error(format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type()))
    } else {
        Object::Error(format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))
    }
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
        _ => Object::Error(format!("unknown operator: {} {} {}", "INTEGER", operator, "INTEGER"))
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!("unknown operator: {}{}", operator, right.object_type()))
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
        Object::Error(format!("unknown operator: -{}", right.object_type()))
    }
}

fn object_to_int(object: &Object) -> Option<i64> {
    match object {
        Object::Integer(value) => Some(*value),
        _ => None
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(value) => *value,
        Object::Null => false,
        _ => true
    }
}

fn is_error(object: &Object) -> bool {
    match object {
        Object::Error(_) => true,
        _ => false
    }
}