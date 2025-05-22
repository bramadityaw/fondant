#![allow(unused)]

use im;
use std::{
    fmt::{Display, Pointer},
    i32,
};

mod errors;
#[macro_use]
mod util;

use errors::ParseError;
use util::{assume, either_one, ends_with, optional, skip_whitespace, starts_with};

type ParseResult<'a, T> = std::result::Result<(T, &'a str), ParseError<'a>>;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Literal(i32),
    Var(String),
    Fun {
        args: Vec<String>,
        body: Box<Expr>,
    },
    App {
        caller: Box<Expr>,
        params: Vec<Expr>,
    },
    Let {
        name: String,
        def: Box<Expr>,
        body: Box<Expr>,
    },
    Closure {
        env: Env,
        args: Vec<String>,
        body: Box<Expr>,
    },
}

type Env = im::HashMap<String, Expr>;

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(x) => x.fmt(f),
            Expr::Var(x) => x.fmt(f),
            Expr::Fun { args, body } => {
                f.write_str("fun (")?;
                for arg in args {
                    arg.fmt(f)?;
                }
                f.write_str(") ")?;
                Display::fmt(body, f)
            }
            Expr::Closure { env, args, body } => Display::fmt(
                &Expr::Fun {
                    args: args.clone(),
                    body: body.clone(),
                },
                f,
            ),
            Expr::Let { name, def, body } => {
                f.write_str("let ")?;
                f.write_str(name)?;
                f.write_str(" = ")?;
                Display::fmt(def, f)?;
                f.write_str(" in ")?;
                Display::fmt(body, f)
            }
            _ => todo!(),
        }
    }
}

fn evaluate(env: Env, expr: Expr) -> Expr {
    match expr {
        Expr::Literal(_) => expr,
        Expr::Var(name) => {
            let expr = env
                .get(&name)
                .clone()
                .expect(&format!("no variable '{name}' found"));
            let env = env.clone();
            evaluate(env, expr.clone())
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
                _ => unreachable!(),
            }
        }
    }
}

fn parse_var(input: &str) -> ParseResult<Expr> {
    let (ident, rest) = parse_ident(input)?;
    let (_, rest) = skip_whitespace(rest)?;
    if matches!(ident.as_str(), "let" | "in") {
        return Err(ParseError::Expected(
            "variable",
            format!("keyword: {ident}"),
        ));
    }
    Ok((Expr::Var(ident), rest))
}

fn parse_ident(input: &str) -> ParseResult<String> {
    Ok(parse_if!(input, is_ascii_alphabetic))
}

fn parse_digit(input: &str) -> ParseResult<Expr> {
    let (digits, rest) = parse_if!(input, is_ascii_digit);
    let num = digits.parse().map_err(|_| ParseError::BadDigit)?;
    Ok((Expr::Literal(num), rest))
}

fn parse_let(input: &str) -> ParseResult<Expr> {
    let (_, rest) = assume("let", input)?;
    let (name, rest) = parse_ident(rest)?;
    let (_, rest) = assume("=", rest)?;
    let (def, rest) = parse_expr(rest)?;
    let (_, rest) = assume("in", rest)?;
    let (body, rest) = parse_expr(rest).unwrap();
    Ok((
        Expr::Let {
            name: name,
            def: Box::new(def),
            body: Box::new(body),
        },
        rest,
    ))
}

fn parse_fun<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let (_, rest) = assume("fun", input)?;
    let (_, rest) = assume("(", rest)?;
    let (arg, rest) = parse_ident(rest)?;
    let (_, rest) = assume(")", rest)?;
    let (body, rest) = either_one(vec![parse_digit, parse_var, parse_let].as_slice(), rest)?;
    Ok((
        Expr::Fun {
            args: vec![arg],
            body: Box::new(body),
        },
        rest,
    ))
}

fn parse_app<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let (caller, rest) = either_one(vec![parse_paren_expr, parse_var].as_slice(), input)?;
    let (_, rest) = assume("(", rest)?;
    let (param, rest) = parse_expr(rest)?;
    let (_, rest) = assume(")", rest)?;
    Ok((
        Expr::App {
            caller: caller.into(),
            params: vec![param],
        },
        rest,
    ))
}

fn parse_paren_expr<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let (_, rest) = assume("(", input)?;
    let (expr, rest) = parse_expr(rest)?;
    let (_, rest) = assume(")", rest)?;
    Ok((expr, rest))
}

fn parse_expr<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let (expr, rest) = either_one(
        vec![
            parse_app,
            parse_paren_expr,
            parse_let,
            parse_fun,
            parse_digit,
            parse_var,
        ]
        .as_slice(),
        input,
    )?;
    Ok((dbg!(expr), rest))
}

fn eval(input: &str) -> String {
    let env = im::HashMap::new();
    let (expr, _) = parse_expr(input).map(|e| dbg!(e)).unwrap();
    dbg!(evaluate(env, expr).to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_fun_app() {
        assert_eq!(parse_app("x(4)").unwrap().0, {
            Expr::App {
                caller: Expr::Var("x".into()).into(),
                params: vec![Expr::Literal(4)],
            }
        });
        assert_eq!(parse_app("(fun(x) x)(4)").unwrap().0, {
            Expr::App {
                caller: Expr::Fun {
                    args: vec!["x".into()],
                    body: Expr::Var("x".into()).into(),
                }
                .into(),
                params: vec![Expr::Literal(4)],
            }
        });
    }

    #[test]
    fn parse_fun_expr() {
        assert_eq!(parse_fun("fun(x) x").unwrap().0, {
            Expr::Fun {
                args: vec!["x".into()],
                body: Expr::Var("x".into()).into(),
            }
        });
    }

    #[test]
    fn parse_let_expr() {
        assert_eq!(parse_let("let id = fun(x) x in id(4)").unwrap().0, {
            Expr::Let {
                name: "id".into(),
                def: Expr::Fun {
                    args: vec!["x".into()],
                    body: Expr::Var("x".into()).into(),
                }
                .into(),
                body: Expr::App {
                    caller: Expr::Var("id".into()).into(),
                    params: vec![Expr::Literal(4)],
                }
                .into(),
            }
        });
    }

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
    }

    #[test]
    fn let_var_bindings() {
        assert_eq!(eval("let x = 4 in x"), "4");
    }

    #[test]
    fn let_fun_bindings() {
        assert_eq!(eval("let id = fun(x) x in id(4)"), "4");
    }
}
