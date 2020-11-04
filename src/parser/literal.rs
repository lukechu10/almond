//! Parsing for JS literals.

use crate::ast::*;
use crate::parser::util::*;
use nom::{branch::alt, bytes::complete::*, combinator::*, IResult};
use nom_locate::position;

pub fn parse_literal(s: Span) -> IResult<Span, Node> {
    alt((parse_null_lit, parse_bool_lit))(s)
}

pub fn parse_null_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, _) = tag("null")(s)?;
    let (s, end) = position(s)?;
    Ok((s, LiteralValue::Null.into_node_kind().with_pos(start, end)))
}

pub fn parse_bool_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, val) = alt((value(false, tag("false")), value(true, tag("true"))))(s)?;
    let (s, end) = position(s)?;

    Ok((
        s,
        LiteralValue::Boolean(val)
            .into_node_kind()
            .with_pos(start, end),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test_bool_lit() {
        parse_bool_lit("true".into()).unwrap();
        parse_bool_lit("false".into()).unwrap();
        parse_bool_lit("bad".into()).unwrap_err();
        parse_bool_lit("badfalse".into()).unwrap_err();
    }
}
