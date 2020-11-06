//! Parsing for JS expressions.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*, IResult};
use nom_locate::position;

pub fn parse_expr(s: Span) -> IResult<Span, Node> {
    parse_primary_expr(s)
}

/// Parse an atomic expression — either a single token that is an
// expression, an expression started by a keyword like `function` or
// `new`, or an expression wrapped in punctuation like `()`, `[]`,
// or `{}`.
pub fn parse_primary_expr(s: Span) -> ParseResult<Node> {
    alt((
        parse_this_expr,
        parse_identifier,
        parse_paren_expr,
        literal::parse_literal,
    ))(s)
}

pub fn parse_this_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, _) = tag("this")(s)?;
    let (s, end) = position(s)?;
    let (s, _) = spaces0(s)?;
    Ok((s, NodeKind::ThisExpression.with_pos(start, end)))
}

pub fn parse_paren_expr(s: Span) -> ParseResult<Node> {
    delimited(ws0(char('(')), parse_expr, ws0(char(')')))(s)
}

pub fn parse_expr_list(s: Span) -> IResult<Span, Vec<Option<Node>>> {
    separated_list0(ws0(char(',')), opt(parse_expr))(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test_this_expr() {
        parse_this_expr("this".into()).unwrap();
        parse_this_expr("notthis".into()).unwrap_err();
    }

    #[test]
    fn smoke_test_primary_expr() {
        parse_primary_expr("this".into()).unwrap();
        parse_primary_expr("myVar".into()).unwrap();
    }

    #[test]
    fn smoke_test_paren_expr() {
        parse_paren_expr("(true)".into()).unwrap();
    }
}
