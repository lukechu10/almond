//! Parsing for JS expressions.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*, IResult};
use nom_locate::position;

pub fn expr(s: Span) -> IResult<Span, Node> {
    primary_expr(s)
}

/// Parse an atomic expression — either a single token that is an
// expression, an expression started by a keyword like `function` or
// `new`, or an expression wrapped in punctuation like `()`, `[]`,
// or `{}`.
pub fn primary_expr(s: Span) -> IResult<Span, Node> {
    alt((this_expr, identifier, paren_expr, literal::literal))(s)
}

pub fn this_expr(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, _) = tag("this")(s)?;
    let (s, end) = position(s)?;
    let (s, _) = spaces0(s)?;
    Ok((s, NodeKind::ThisExpression.with_pos(start, end)))
}

pub fn paren_expr(s: Span) -> IResult<Span, Node> {
    delimited(
        terminated(char('('), spaces0),
        terminated(expr, spaces0),
        terminated(char(')'), spaces0),
    )(s)
}

pub fn expr_list(s: Span) -> IResult<Span, Vec<Option<Node>>> {
    separated_list0(
        terminated(char(','), spaces0),
        opt(terminated(expr, spaces0)),
    )(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test_this_expr() {
        this_expr("this".into()).unwrap();
        this_expr("notthis".into()).unwrap_err();
    }

    #[test]
    fn smoke_test_primary_expr() {
        primary_expr("this".into()).unwrap();
        primary_expr("myVar".into()).unwrap();
    }

    #[test]
    fn smoke_test_paren_expr() {
        paren_expr("(true)".into()).unwrap();
    }
}
