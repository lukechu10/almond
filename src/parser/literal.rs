//! Parsing for JS literals.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*, IResult};
use nom_locate::position;

pub fn parse_literal(s: Span) -> IResult<Span, Node> {
    alt((parse_null_lit, parse_bool_lit))(s)
}

pub fn parse_null_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, _) = terminated(tag("null"), spaces0)(s)?;
    let (s, end) = position(s)?;
    Ok((s, LiteralValue::Null.into_node_kind().with_pos(start, end)))
}

pub fn parse_bool_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, val) = terminated(
        alt((value(false, tag("false")), value(true, tag("true")))),
        spaces0,
    )(s)?;
    let (s, end) = position(s)?;

    Ok((
        s,
        LiteralValue::Boolean(val)
            .into_node_kind()
            .with_pos(start, end),
    ))
}

pub fn parse_array_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, expr_list) = delimited(
        terminated(char('['), spaces0),
        terminated(parse_expr_list, spaces0),
        terminated(char(']'), spaces0),
    )(s)?;
    let (s, end) = position(s)?;

    Ok((
        s,
        NodeKind::ArrayExpression {
            elements: expr_list,
        }
        .with_pos(start, end),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool_lit() {
        parse_bool_lit("true".into()).unwrap();
        parse_bool_lit("false".into()).unwrap();
    }

    #[test]
    fn test_bool_lit_with_trailing_whitespace() {
        parse_bool_lit("true ".into()).unwrap();
        parse_bool_lit("false ".into()).unwrap();
    }

    #[test]
    fn test_bad_bool_lit() {
        parse_bool_lit("bad".into()).unwrap_err();
        parse_bool_lit("badfalse".into()).unwrap_err();
    }

    #[test]
    fn test_array_lit() {
        parse_array_lit("[]".into()).unwrap();
        parse_array_lit("[true, true]".into()).unwrap();
    }

    #[test]
    fn test_array_lit_with_whitespace() {
        parse_array_lit("[ true, true ]\n".into()).unwrap();
        parse_array_lit("[   true,\n true\t ]\t".into()).unwrap();
    }

    #[test]
    fn test_array_lit_with_trailing_comma() {
        parse_array_lit("[ true, true, ]\n".into()).unwrap();
        parse_array_lit("[   true,\n true,\t ]\t".into()).unwrap();
    }
}
