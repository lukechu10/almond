//! Parsing for JS expressions.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*, IResult};
use nom_locate::position;

#[must_use]
pub fn parse_this_expr(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    tag("this")(s)?;
    let (s, end) = position(s)?;
    Ok((s, NodeKind::ThisExpression.with_pos(start, end)))
}

/// Parse an atomic expression — either a single token that is an
// expression, an expression started by a keyword like `function` or
// `new`, or an expression wrapped in punctuation like `()`, `[]`,
// or `{}`.
#[must_use]
pub fn parse_primary_expr(s: Span) -> IResult<Span, Node> {
    alt((parse_this_expr, literal::parse_literal))(s)
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
    }
}
