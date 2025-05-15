#[derive(Debug, Clone)]
pub enum ParseError<'a> {
    InvalidInput,
    BadDigit,
    Expected(&'a str, String),
}
