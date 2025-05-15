#![allow(unused)]

use im;
use std::i32;

mod errors;
#[macro_use]
mod util;

use errors::ParseError;
use util::{assume, either_one, ends_with, optional, skip_whitespace, starts_with};

type ParseResult<'a, T> = std::result::Result<(T, &'a str), ParseError<'a>>;

#[derive(Debug, Clone)]
enum Expr {
    Literal(i32),
    Var(String),
    Fun {
        args: Vec<String>,
        body: Box<Expr>,
    },
    Let {
        name: String,
        def: Box<Expr>,
        body: Box<Expr>,
    },
}

type Env<'a> = im::HashMap<String, Expr>;

fn evaluate<'a>(env: Env<'a>, expr: Expr) -> i32 {
    match expr {
        Expr::Literal(i) => i,
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
            dbg!(&env);
            evaluate(env, *body)
        }
        Expr::Fun { args, body } => todo!(),
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
    let (def, rest) = either_one(vec![parse_digit, parse_var, parse_let].as_slice(), rest)?;
    let (_, rest) = assume("in", rest)?;
    let (body, rest) = parse_expr(rest)?;
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

fn parse_expr<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let (_, rest) = optional(starts_with("(", input), input)?;
    let (expr, rest) = either_one(
        vec![parse_digit, parse_let, parse_fun, parse_var].as_slice(),
        rest,
    )?;
    let (_, rest) = optional(ends_with("(", rest), rest)?;
    Ok((expr, rest))
}

fn eval(input: &str) -> i32 {
    let env = im::HashMap::new();
    let (expr, _) = parse_expr(input).map(|e| dbg!(e)).unwrap();
    evaluate(env, expr)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literals() {
        assert_eq!(eval("42"), 42);
        assert_eq!(eval("(42)"), 42);
    }

    #[test]
    fn variables() {
        let env = im::HashMap::new().update("ten".into(), Expr::Literal(10));
        assert_eq!(evaluate(env, Expr::Var("ten".into())), 10);
    }

    #[test]
    fn let_var_bindings() {
        assert_eq!(eval("let x = 4 in x"), 4);
    }

    #[test]
    fn fun_app() {
        assert_eq!(eval("(fun(x) x) 4"), 4);
    }

    #[test]
    fn let_fun_bindings() {
        assert_eq!(eval("let id = fun(x) x in id(4)"), 4);
    }
}
