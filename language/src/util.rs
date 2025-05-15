use crate::errors::ParseError;
use crate::{Expr, ParseResult};

pub fn assume<'a>(symbol: &'a str, input: &'a str) -> ParseResult<'a, &'a str> {
    if input.starts_with(symbol) {
        let rest = input
            .strip_prefix(symbol)
            .expect(&format!("checked if the input starts with {symbol}"));
        let (_, rest) = skip_whitespace(rest)?;
        Ok(("", rest))
    } else {
        Err(ParseError::Expected(symbol, input.into()))
    }
}

pub fn skip_whitespace(input: &str) -> ParseResult<&str> {
    Ok(("", input.trim_ascii_start()))
}

macro_rules! parse_if {
    ($input:expr, $f:ident) => {{
        let mut rest = $input.as_bytes();
        let mut ident = Vec::new();
        while let [current, next @ ..] = rest {
            if current.$f() {
                ident.push(*current);
            } else {
                break;
            }
            rest = next
        }
        // SAFETY: All Fondant source files must be valid UTF-8
        unsafe {
            let (_, rest) = skip_whitespace(std::str::from_utf8_unchecked(rest))?;
            (String::from_utf8_unchecked(ident), rest)
        }
    }};
}

pub fn either_one<'a, P>(parsers: &[P], input: &'a str) -> ParseResult<'a, Expr>
where
    P: Fn(&str) -> ParseResult<Expr>,
{
    match parsers {
        [parser, rest @ ..] => match parser(input) {
            Ok(expr) => Ok(expr),
            Err(_) => either_one(&rest, input),
        },
        &[] => Err(ParseError::InvalidInput),
    }
}

pub fn optional<'a>(p: ParseResult<'a, &'a str>, input: &'a str) -> ParseResult<'a, &'a str> {
    match p {
        Ok(res) => Ok(res),
        Err(_) => Ok(("", input)),
    }
}

pub fn starts_with<'a>(s: &'a str, input: &'a str) -> ParseResult<'a, &'a str> {
    assume(s, input)
}

pub fn ends_with<'a>(s: &'static str, input: &'a str) -> ParseResult<'a, &'a str> {
    let pat = s;
    let mut rest = input.as_bytes();
    let mut buf: Vec<u8> = Vec::new();
    while let [init @ .., end] = rest {
        buf.push(*end);
        if pat.len() != buf.len() {
            continue;
        }
        rest = init
    }
    unsafe { Ok(("", std::str::from_utf8_unchecked(rest))) }
}
