#![allow(dead_code)]

use std::collections::HashMap;
use std::sync::LazyLock;
use crate::ast::{BlockStatement, Expression, Identifier, Program, Statement};
use crate::builtins::builtins;
use crate::environment::Environment;
use crate::object::{Object, ObjectType};

const BUILTINS: LazyLock<HashMap<String, Object>> = LazyLock::new(builtins);
const OBJECT_BOOLEAN_TRUE: Object = Object::Boolean(true);
const OBJECT_BOOLEAN_FALSE: Object = Object::Boolean(false);
const OBJECT_NULL: Object = Object::Null;

pub fn eval_program(program: &Program, env: &mut Environment) -> Object {
    let mut result = OBJECT_NULL;

    for stmt in &program.statements {
        result = eval_statement(stmt, env);
        if let Object::ReturnValue(value) = result {
            return *value;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }

    result
}

fn eval_statement(stmt: &Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::LetStatement(_, ident, expression) => {
            let val = eval_expression(expression, env);
            if is_error(&val) {
                return val;
            }

            env.set(ident.value.clone(), val.clone());
            val
        }
        Statement::ReturnStatement(_, expression) => {
            let val = eval_expression(expression, env);
            if is_error(&val) {
                return val;
            }
            Object::ReturnValue(Box::new(val))
        }
        Statement::ExpressionStatement(_, expression) => { eval_expression(expression, env)}
        Statement::BlockStatement(token, statements) => {
            eval_block_statement(BlockStatement{token: token.clone(), statements: statements.clone()}, env)
        }
    }
}

fn eval_block_statement(block_statement: BlockStatement, env: &mut Environment) -> Object {
    let mut result = OBJECT_NULL;

    for stmt in block_statement.statements {
        result = eval_statement(&stmt, env);
        if let Object::ReturnValue(_) = result {
            return result
        }
        if let Object::Error(_) = result {
            return result
        }
    }
    result
}

fn eval_expression(expr: &Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::Identifier(_, value) => eval_identifier(value, env),
        Expression::IntegerLiteral(_, value) => Object::Integer(*value),
        Expression::StringLiteral(_, value) => Object::StringObject(value.clone()),
        Expression::Boolean(_, value) => { if *value { OBJECT_BOOLEAN_TRUE } else { OBJECT_BOOLEAN_FALSE } }
        Expression::PrefixExpression(_, operator, right) => {
            let evaluated_right = eval_expression(right, env);
            if is_error(&evaluated_right) {
                return evaluated_right;
            }
            eval_prefix_expression(operator, evaluated_right)
        }
        Expression::InfixExpression(_, left, operator, right) => {
            let evaluated_left = eval_expression(left, env);
            if is_error(&evaluated_left) {
                return evaluated_left;
            }

            let evaluated_right = eval_expression(right, env);
            if is_error(&evaluated_right) {
                return evaluated_right;
            }
            eval_infix_expression(operator, &evaluated_left, &evaluated_right)
        }
        Expression::IfExpression(_, condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expression::FunctionLiteral(_, params, body) => {
            Object::Function(params.clone(), body.clone(), env.clone())
        }
        Expression::CallExpression(_, boxed_fn, arguments) => {
            let evaluated_function = eval_expression(boxed_fn.as_ref(), env);
            if is_error(&evaluated_function) {
                return evaluated_function;
            }

            let evaluated_args = eval_expressions(arguments, env);
            if evaluated_args.len() == 1 && is_error(evaluated_args.first().unwrap()) {
                return evaluated_args.first().unwrap().clone();
            }

            apply_function(evaluated_function, evaluated_args)
        }
    }
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    if let Object::Function(parameters, body, env) = function.clone() {
        let mut extended_env = extend_function_env(parameters, &env, args);
        let evaluated = eval_block_statement(body, &mut extended_env);
        unwrap_return_value(evaluated)
    } else if let Object::Builtin(function) = function.clone() {
        function(args)
    } else {
        Object::Error(format!("not a function: {}", function.object_type()))
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(value) = obj {
        *value
    } else {
        obj
    }
}

fn extend_function_env(fn_parameters: Vec<Identifier>, fn_env: &Environment, args: Vec<Object>) -> Environment {
    let mut env = Environment::new_enclosed_environment(fn_env);

    for n in 0..fn_parameters.len() {
        env.set(fn_parameters[n].value.clone(), args[n].clone());
    }
    env
}

fn eval_expressions(expressions: &Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut result = Vec::new();

    for expr in expressions {
        let evaluated = eval_expression(expr, env);
        if let Object::Error(_) = evaluated {
            return result
        }
        result.push(evaluated);
    }
    result
}

fn eval_identifier(ident: &str, env: &mut Environment) -> Object {
    match env.get(ident) {
        Some(value) => value,
        None => {
            if BUILTINS.contains_key(ident) {
                return BUILTINS[ident].clone();
            }
            Object::Error(format!("identifier not found: {}", ident))
        }
    }
}

fn eval_if_expression(condition: &Expression, consequence: &BlockStatement, alternative: &Option<BlockStatement>, env: &mut Environment) -> Object {
    let evaluated_condition = eval_expression(condition, env);
    if is_error(&evaluated_condition) {
        return evaluated_condition;
    }

    if is_truthy(&evaluated_condition) {
        eval_block_statement(consequence.clone(), env)
    } else if let Some(alternative_block) = alternative {
        eval_block_statement(alternative_block.clone(), env)
    } else {
        OBJECT_NULL
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => {
            eval_integer_infix_expression(operator, *l, *r)
        },
        (Object::StringObject(l), Object::StringObject(r)) => {
            eval_string_infix_expression(operator, l, r)
        },
        (_, _) => {
            if operator == "==" {
                Object::Boolean(left == right)
            } else if operator == "!=" {
                Object::Boolean(left != right)
            } else if left.object_type() != right.object_type() {
                Object::Error(format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type()))
            } else {
                Object::Error(format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))
            }

        }
    }
}

fn eval_string_infix_expression(operator: &str, left: &str, right: &str) -> Object {
    if operator != "+" {
        return Object::Error(format!("unknown operator: STRING {} STRING", operator))
    }
    Object::StringObject(left.to_string() + right)
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
    matches!(object, Object::Error(_))
}