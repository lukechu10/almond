//! Parsing for JS expressions.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*, IResult};
use nom_locate::position;

/// Alias for `parse_expr_bp(s, 0)`.
pub fn parse_expr(s: Span) -> IResult<Span, Node> {
    parse_expr_bp(s, 0)
}

/// Parse an atomic expression — either a single token that is an
// expression, an expression started by a keyword like `function` or
// `new`, or an expression wrapped in punctuation like `()`, `[]`,
// or `{}`.
pub fn parse_primary_expr(s: Span) -> ParseResult<Node> {
    alt((
        parse_this_expr,
        parse_identifier,
        literal::parse_literal,
        parse_paren_expr,
    ))(s)
}

pub fn parse_this_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, _) = tag("this")(s)?;
    let (s, end) = position(s)?;
    let (s, _) = sp0(s)?;
    Ok((s, NodeKind::ThisExpression.with_pos(start, end)))
}

pub fn parse_paren_expr(s: Span) -> ParseResult<Node> {
    delimited(ws0(char('(')), parse_expr, ws0(char(')')))(s)
}

fn parse_opt_expr_in_list(s: Span) -> ParseResult<Option<Node>> {
    alt((
        value(None, peek(char(','))),
        map(parse_expr, |expr| Some(expr)),
    ))(s)
}

pub fn parse_expr_list_with_opt_expr(s: Span) -> ParseResult<Vec<Option<Node>>> {
    terminated(
        separated_list0(ws0(char(',')), parse_opt_expr_in_list),
        // eat trailing comma
        ws0(opt(char(','))),
    )(s)
}

pub fn parse_expr_list(s: Span) -> ParseResult<Vec<Node>> {
    terminated(
        separated_list0(ws0(char(',')), parse_expr),
        // eat trailing comma
        ws0(opt(char(','))),
    )(s)
}

fn parse_computed_member_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, (object, property)) = tuple((
        parse_member_expr,
        delimited(ws0(char('[')), parse_expr, char(']')),
    ))(s)?;
    let (s, end): (Span, Span) = position(s)?;
    let (s, _) = sp0(s)?;
    Ok((
        s,
        NodeKind::MemberExpression {
            computed: true,
            property: Box::new(property),
            object: Box::new(object),
        }
        .with_pos(start, end),
    ))
}

fn parse_prop_member_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, (object, property)) = tuple((
        parse_member_expr,
        preceded(ws0(char('.')), parse_identifier),
    ))(s)?;
    let (s, end): (Span, Span) = position(s)?;
    let (s, _) = sp0(s)?;
    Ok((
        s,
        NodeKind::MemberExpression {
            computed: false,
            property: Box::new(property),
            object: Box::new(object),
        }
        .with_pos(start, end),
    ))
}

fn parse_new_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, (callee, arguments)) = preceded(
        ws1(keyword_new),
        pair(
            parse_member_expr,
            delimited(ws0(char('(')), parse_expr_list, char(')')),
        ),
    )(s)?;
    let (s, end) = position(s)?;
    let (s, _) = sp0(s)?;
    Ok((
        s,
        NodeKind::NewExpression {
            callee: Box::new(callee),
            arguments,
        }
        .with_pos(start, end),
    ))
}

pub fn parse_member_expr(s: Span) -> ParseResult<Node> {
    alt((
        // terminated(parse_primary_expr, not(alt((char('.'), char('['))))),
        parse_prop_member_expr,     // '.'
        parse_computed_member_expr, // '['
        parse_new_expr,             // new
        parse_primary_expr,
    ))(s)
}

pub fn parse_call_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, (callee, arguments)) = pair(
        parse_member_expr,
        delimited(ws0(char('(')), parse_expr_list, char(')')),
    )(s)?;
    let (s, end) = position(s)?;
    let (s, _) = sp0(s)?;
    Ok((
        s,
        NodeKind::CallExpression {
            callee: Box::new(callee),
            arguments,
        }
        .with_pos(start, end),
    ))
}

pub fn parse_lhs_expr(s: Span) -> ParseResult<Node> {
    alt((parse_call_expr, parse_member_expr))(s)
}

// fn prefix_expr(s: Span) -> ParseResult<Node> {

// }

// fn postfix_expr(s: Span) -> ParseResult<Node> {

// }

// pub fn parse_unary_expr(s: Span) -> ParseResult<Node> {
//     ws0(alt((prefix_expr, postfix_expr)))(s)
// }

/// Pratt parsing for prefix operators. Called in `parse_expr_bp`.
fn parse_prefix_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, (unary_op, BindingPower(_, right_bp))) = parse_prefix_operator(s)?;
    let (s, rhs) = parse_expr_bp(s, right_bp)?;
    let (s, end) = position(s)?;

    let node_kind = match unary_op {
        PrefixOperator::Unary(unary_op) => NodeKind::UnaryExpression {
            argument: Box::new(rhs),
            operator: unary_op,
            prefix: true,
        },
        PrefixOperator::Update(unary_op) => NodeKind::UpdateExpression {
            argument: Box::new(rhs),
            operator: unary_op,
            prefix: true,
        },
    };
    Ok((s, node_kind.with_pos(start, end)))
}

/// Pratt parsing for expressions with operator precedence.
/// Check out [https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) to see how Pratt parsing works.
pub fn parse_expr_bp(s: Span, min_bp: i32) -> ParseResult<Node> {
    let (mut s, mut lhs) = alt((parse_prefix_expr, parse_primary_expr))(s)?;

    loop {
        // do not override s just yet
        let (s_tmp, (op, BindingPower(left_bp, right_bp))) = match parse_infix_operator(s) {
            Ok(res) => res,
            Err(_) => break, // do not return from function, just break from loop.
        };

        if left_bp < min_bp {
            break;
        }

        // ok, now we can override s
        s = s_tmp;

        let (s_tmp, rhs) = parse_expr_bp(s, right_bp)?;
        s = s_tmp;

        let start = lhs.clone().start;
        let end = rhs.clone().end;

        let node_kind = match op {
            InfixOperator::Binary(op) => NodeKind::BinaryExpression {
                left: Box::new(lhs),
                right: Box::new(rhs),
                operator: op,
            },
            InfixOperator::Logical(op) => NodeKind::LogicalExpression {
                left: Box::new(lhs),
                right: Box::new(rhs),
                operator: op,
            },
            InfixOperator::Assignment(op) => NodeKind::AssignmentExpression {
                left: Box::new(lhs),
                right: Box::new(rhs),
                operator: op,
            },
            InfixOperator::DotOperator => NodeKind::MemberExpression {
                object: Box::new(lhs),
                property: Box::new(rhs),
                computed: false,
            },
        };
        lhs = node_kind.with_pos(start, end);
    }

    Ok((s, lhs))
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_json_snapshot;

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

    #[test]
    #[ignore]
    fn test_member_expr() {
        assert_json_snapshot!(parse_member_expr("a.b".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("a.b.c".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("a[1]".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("a[0]".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("a[[]]".into()).unwrap().1);
    }

    #[test]
    #[ignore]
    fn test_new_expr() {
        assert_json_snapshot!(parse_member_expr("new Array()".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("new Array(1)".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("new Array(1,)".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("new Array(1,2)".into()).unwrap().1);
        assert_json_snapshot!(parse_member_expr("new Foo.Bar(true)".into()).unwrap().1);
    }

    #[test]
    #[ignore]
    fn test_call_expr() {
        assert_json_snapshot!(parse_call_expr("foo()".into()).unwrap().1);
        assert_json_snapshot!(parse_call_expr("foo.bar()".into()).unwrap().1);
        assert_json_snapshot!(parse_call_expr("foo.bar.baz()".into()).unwrap().1);
        assert_json_snapshot!(parse_call_expr("foo.bar(baz)".into()).unwrap().1);
        assert_json_snapshot!(parse_call_expr("foo.bar(baz, 1, 2, 3)".into()).unwrap().1);
    }

    #[test]
    fn test_expr_bp_infix() {
        assert_json_snapshot!(parse_expr("1 + 2".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 - 2 - 3".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 * 2 + 3".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 + 2 * 3".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 * 2 + 3 * 4".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("(1 + 2) * 3".into()).unwrap().1);
    }
    #[test]
    fn test_expr_bp_prefix() {
        assert_json_snapshot!(parse_expr("-1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("+1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("+(+1)".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 + -2".into()).unwrap().1); // same as 1 + (-1)

        assert_json_snapshot!(parse_expr("++x".into()).unwrap().1);
    }

    #[test]
    fn test_expr_bp_assignment() {
        assert_json_snapshot!(parse_expr("x = 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x = 1 + 2".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x += 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x += y += 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x += x * x".into()).unwrap().1);
    }

    #[test]
    fn test_expr_bp_member_expr() {
        assert_json_snapshot!(parse_expr("x.y".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x.y.z".into()).unwrap().1);
    }
}
