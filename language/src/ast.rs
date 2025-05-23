use std::fmt::Display;

use crate::Env;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
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
