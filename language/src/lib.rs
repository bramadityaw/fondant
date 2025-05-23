#![allow(unused)]
mod ast;
mod parser;

use ast::Expr;
use im;
use parser::ParseError;
use std::i32;

type Env = im::HashMap<String, Expr>;

fn evaluate(env: Env, expr: Expr) -> Expr {
    match expr {
        Expr::Literal(_) => expr,
        Expr::Var(name) => {
            let expr = env.get(&name).clone();
            match expr {
                Some(e) => {
                    let env = env.clone();
                    evaluate(env, e.clone())
                }
                None => Expr::Var(name),
            }
        }
        Expr::Let { name, def, body } => {
            let env = env.update(name, *def.clone());
            evaluate(env, *body)
        }
        Expr::Fun { args, body } => Expr::Closure { env, args, body },
        Expr::Closure { env, args, body } => Expr::Closure { env, args, body },
        Expr::App { caller, params } => {
            let closure = evaluate(env.clone(), *caller);
            match closure {
                Expr::Closure {
                    env: closure_env,
                    args,
                    body,
                } => {
                    assert!(params.len() == args.len());
                    let evaluated_args: Vec<_> = params
                        .into_iter()
                        .map(|param| evaluate(env.clone(), param))
                        .collect();

                    let extended_env = args
                        .into_iter()
                        .zip(evaluated_args)
                        .fold(closure_env, |env, (name, value)| env.update(name, value));

                    evaluate(extended_env, *body)
                }
                Expr::Var(name) => {
                    if is_builtin(&name) {
                        eval_builtin(name, params)
                    } else {
                        evaluate(env, Expr::Var(name))
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(name, "add" | "sub" | "mul" | "div")
}

fn eval_builtin(name: String, params: Vec<Expr>) -> Expr {
    assert!(params.len() == 2);
    let (lhs, rhs) = unsafe { (params.get_unchecked(0), params.get_unchecked(1)) };
    match (lhs, rhs) {
        (Expr::Literal(lhs), Expr::Literal(rhs)) => Expr::Literal(match name.as_str() {
            "add" => lhs + rhs,
            "sub" => lhs - rhs,
            "mul" => lhs * rhs,
            "div" => lhs / rhs,
            _ => unimplemented!(),
        }),
        _ => unimplemented!(),
    }
}

fn eval(input: &str) -> String {
    let env = im::HashMap::new();
    let (expr, _) = parser::parse_expr(input).unwrap();
    evaluate(env, expr).to_string()
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn eval_literals() {
        assert_eq!(eval("42"), "42");
        assert_eq!(eval("(42)"), "42");
        assert_eq!(eval("((((((42))))))"), "42");
    }

    #[test]
    fn eval_variables() {
        let env = im::HashMap::new().update("ten".into(), Expr::Literal(10));
        assert_eq!(evaluate(env, Expr::Var("ten".into())), Expr::Literal(10));
    }

    #[test]
    fn eval_fun_app() {
        assert_eq!(eval("(fun(x) x)(4)"), "4");
        assert_eq!(eval("(fun(x, y) x)(4, 5)"), "4");
    }

    #[test]
    fn let_var_bindings() {
        assert_eq!(eval("let x = 4 in x"), "4");
    }

    #[test]
    fn let_fun_bindings() {
        assert_eq!(eval("let id = fun(x) x in id(4)"), "4");
        assert_eq!(eval("let const = fun(x, y) x in const(4, 5)"), "4");
    }

    #[test]
    fn builtin_funs() {
        assert_eq!(eval("add(4, 2)"), "6");
        assert_eq!(eval("sub(4, 2)"), "2");
        assert_eq!(eval("mul(4, 2)"), "8");
        assert_eq!(eval("div(4, 2)"), "2");
    }
}
