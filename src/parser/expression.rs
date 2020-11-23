//! Parsing for JS expressions.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom_locate::position;

/// Alias for `parse_expr_bp(s, 0)`.
pub fn parse_expr(s: Span) -> ParseResult<Node> {
    context("expression", |s| parse_expr_bp(s, 0, false))(s)
}

/// Alias for `parse_expr_bp(s, 1)`. Should be used when parsing expressions in expression lists.
/// This prevents matching the sequence (`,`) operator.
pub fn parse_expr_no_seq(s: Span) -> ParseResult<Node> {
    context("expression no seq", |s| parse_expr_bp(s, 1, false))(s)
}

/// Parse an atomic expression — either a single token that is an
// expression, an expression started by a keyword like `function`.
pub fn parse_primary_expr(s: Span) -> ParseResult<Node> {
    alt((
        parse_this_expr,
        parse_identifier,
        literal::parse_literal,
        parse_function_expr,
        parse_paren_expr,
    ))(s)
}

/// Parse an atomic expression — either a single token that is an
/// expression, an expression started by a keyword like `function`.
/// This variant of `parse_primary_expr` allows parsing an identifier that is a reserved name.
/// This method should be used instead of `parse_primary_expr` when preceding token is `.` operator (for member expression).
/// # Spec
/// http://www.ecma-international.org/ecma-262/#sec-property-accessors
pub fn parse_primary_expr_allow_reserved(s: Span) -> ParseResult<Node> {
    alt((
        parse_this_expr,
        parse_identifier_name, // note that this is different from `parse_identifier` which does not allow reserved name.
        literal::parse_literal,
        parse_function_expr,
        parse_paren_expr,
    ))(s)
}

pub fn parse_this_expr(s: Span) -> ParseResult<Node> {
    map(
        spanned(ws0(pair(tag("this"), not(identifier_continue)))),
        |(_, start, end)| NodeKind::ThisExpression.with_pos(start, end),
    )(s)
}

pub fn parse_paren_expr(s: Span) -> ParseResult<Node> {
    context(
        "paren expression",
        delimited(ws0(char('(')), parse_expr, ws0(char(')'))),
    )(s)
}

fn parse_opt_expr_in_list(s: Span) -> ParseResult<Option<Node>> {
    alt((value(None, peek(char(','))), map(parse_expr_no_seq, Some)))(s)
}

pub fn parse_expr_list_with_opt_expr(s: Span) -> ParseResult<Vec<Option<Node>>> {
    context(
        "expression list with optional expression",
        terminated(
            separated_list0(ws0(char(',')), parse_opt_expr_in_list),
            // trailing comma
            ws0(opt(char(','))),
        ),
    )(s)
}

pub fn parse_expr_list(s: Span) -> ParseResult<Vec<Node>> {
    context(
        "expression list",
        terminated(
            separated_list0(ws0(char(',')), parse_expr_no_seq),
            // trailing comma
            ws0(opt(char(','))),
        ),
    )(s)
}

/// Pratt parsing for prefix operators. Called in `parse_expr_bp`.
fn parse_prefix_expr(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, (prefix_op, BindingPower(_, right_bp))) = parse_prefix_operator(s)?;
    let (s, rhs) = parse_expr_bp(
        s, right_bp, /* reserved is only used after `.` operator */ false,
    )?;

    let (mut s, mut end) = position(s)?;

    let node_kind = match prefix_op {
        PrefixOperator::Unary(prefix_op) => NodeKind::UnaryExpression {
            argument: Box::new(rhs),
            operator: prefix_op,
            prefix: true,
        },
        PrefixOperator::Update(prefix_op) => NodeKind::UpdateExpression {
            argument: Box::new(rhs),
            operator: prefix_op,
            prefix: true,
        },
        PrefixOperator::New => {
            let (s_tmp, arguments) = opt(delimited(ws0(char('(')), parse_expr_list, char(')')))(s)?;
            s = s_tmp;
            let (s_tmp, end_tmp) = position(s)?;
            s = s_tmp;
            end = end_tmp;

            let (s_tmp, _) = sp0(s)?;
            s = s_tmp;

            NodeKind::NewExpression {
                callee: Box::new(rhs),
                arguments: arguments.unwrap_or_default(),
            }
        }
    };
    Ok((s, node_kind.with_pos(start, end)))
}

/// Pratt parsing for expressions with operator precedence.
/// Check out [https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) to see how Pratt parsing works.
/// # Params
/// * `min_bp` - The minimal binding power to accept.
/// * `allow_reserved` - If `true`, will call `parse_primary_expr_allow_reserved` instead of `parse_primary_expr`. In almost all cases, this should be `false`.
pub fn parse_expr_bp(s: Span, min_bp: i32, allow_reserved: bool) -> ParseResult<Node> {
    let (mut s, mut lhs) = alt((
        parse_prefix_expr,
        if allow_reserved {
            parse_primary_expr_allow_reserved
        } else {
            parse_primary_expr
        },
    ))(s)?;

    loop {
        if let Ok((s_tmp, (postfix_op, BindingPower(left_bp, _), mut end))) =
            parse_postfix_operator(s)
        {
            if left_bp < min_bp {
                break;
            }
            s = s_tmp;
            let start = lhs.clone().start;

            let node_kind = match postfix_op {
                PostfixOperator::Update(postfix_op) => NodeKind::UpdateExpression {
                    argument: Box::new(lhs),
                    operator: postfix_op,
                    prefix: false,
                },
                PostfixOperator::ComputedMember => {
                    // array access
                    let (s_tmp, property) = terminated(parse_expr, char(']'))(s)?;
                    s = s_tmp;

                    let (s_tmp, end_tmp) = position(s)?;
                    s = s_tmp;
                    end = end_tmp;

                    let (s_tmp, _) = sp0(s)?;
                    s = s_tmp;

                    NodeKind::MemberExpression {
                        object: Box::new(lhs),
                        property: Box::new(property),
                        computed: true,
                    }
                }
                PostfixOperator::FuncCall => {
                    // array access
                    let (s_tmp, arguments) = terminated(parse_expr_list, char(')'))(s)?;
                    s = s_tmp;

                    let (s_tmp, end_tmp) = position(s)?;
                    s = s_tmp;
                    end = end_tmp;

                    let (s_tmp, _) = sp0(s)?;
                    s = s_tmp;

                    NodeKind::CallExpression {
                        callee: Box::new(lhs),
                        arguments,
                    }
                }
            };

            lhs = node_kind.with_pos(start, end);

            continue;
        }

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

        if let InfixOperator::TernaryOperator = op {
            let (s_tmp, mhs) = parse_expr_bp(s, right_bp, false)?;
            s = s_tmp;

            let (s_tmp, _) = ws0(tag(":"))(s)?;
            s = s_tmp;

            let (s_tmp, rhs) = parse_expr_bp(s, right_bp, false)?;
            s = s_tmp;

            let start = lhs.clone().start;
            let end = rhs.clone().end;

            let node_kind = NodeKind::ConditionalExpression {
                test: Box::new(lhs),
                consequent: Box::new(mhs),
                alternate: Box::new(rhs),
            };

            lhs = node_kind.with_pos(start, end);
            continue;
        }

        let (s_tmp, rhs) = parse_expr_bp(s, right_bp, op == InfixOperator::DotOperator)?;
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
            InfixOperator::SequenceOperator => NodeKind::SequenceExpression {
                expressions: match &lhs.kind {
                    NodeKind::SequenceExpression { expressions } => {
                        let mut expressions = expressions.clone();
                        expressions.push(rhs);
                        expressions
                    }
                    _ => vec![lhs, rhs],
                },
            },
            InfixOperator::TernaryOperator => unreachable!("handled earlier"),
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
    fn test_identifier_expr() {
        assert_json_snapshot!(parse_expr("myIdentifier".into()).unwrap().1);
    }

    #[test]
    fn test_expr_bp_guard_in() {
        assert_json_snapshot!(
            parse_expr_bp("myIdentifier in foo".into(), 25, false)
                .unwrap()
                .1
        );
        // should only parse myIdentifier
    }

    #[test]
    fn test_paren_expr() {
        assert_json_snapshot!(parse_expr("(1)".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("(((1)))".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("(((1 + 1)))".into()).unwrap().1);
    }

    #[test]
    fn test_member_expr() {
        assert_json_snapshot!(parse_expr("a.b".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("a.b.c".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("a[1]".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("a[0]".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("a[[]]".into()).unwrap().1);
    }

    #[test]
    fn test_new_expr() {
        assert_json_snapshot!(parse_expr("new Array()".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("new Array".into()).unwrap().1); // paren optional
        assert_json_snapshot!(parse_expr("new Array(1)".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("new Array(1,)".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("new Array(1,2)".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("new Foo.Bar(true)".into()).unwrap().1);
    }

    #[test]
    fn test_call_expr() {
        assert_json_snapshot!(parse_expr("foo()".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("foo.bar()".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("foo.bar.baz()".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("foo.bar(baz)".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("foo.bar(baz, 1, 2, 3)".into()).unwrap().1);

        assert_json_snapshot!(
            parse_expr("console.log(\"Hello World!\")".into())
                .unwrap()
                .1
        );
    }

    #[test]
    fn test_sequence_expr() {
        assert_json_snapshot!(parse_expr("1,2,3".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("(1,2,3)".into()).unwrap().1);
    }

    #[test]
    fn test_ternary_expr() {
        assert_json_snapshot!(parse_expr("true ? x : y".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("a ? x : b ? y : z".into()).unwrap().1);
        // should be parsed as a ? x : (b ? y : z)
    }

    #[test]
    fn test_expr_bp_infix() {
        assert_json_snapshot!(parse_expr("1 + 2".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 - 2 - 3".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 * 2 + 3".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 + 2 * 3".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 * 2 + 3 * 4".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("(1 + 2) * 3".into()).unwrap().1);

        assert_json_snapshot!(parse_expr("true && false".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x < y".into()).unwrap().1);

        // equality
        assert_json_snapshot!(parse_expr("x == 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x != 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x === 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x !== 1".into()).unwrap().1);

        // relational
        assert_json_snapshot!(parse_expr("x < 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x > 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x <= 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x >= 1".into()).unwrap().1);

        // bitwise
        assert_json_snapshot!(parse_expr("x << 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x >> 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x >>> 1".into()).unwrap().1);

        // logical
        assert_json_snapshot!(parse_expr("x && y".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x || y".into()).unwrap().1);
    }

    #[test]
    fn test_expr_bp_infix_str_concat() {
        assert_json_snapshot!(parse_expr(r#""a" + "b""#.into()).unwrap().1);
        assert_json_snapshot!(parse_expr(r#""a" + 123"#.into()).unwrap().1);
        assert_json_snapshot!(parse_expr(r#""a" + {}"#.into()).unwrap().1);
        assert_json_snapshot!(parse_expr(r#"[] + {}"#.into()).unwrap().1);
    }

    #[test]
    fn test_expr_bp_prefix() {
        assert_json_snapshot!(parse_expr("-1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("+1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("+(+1)".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("1 + -2".into()).unwrap().1); // same as 1 + (-2)
        assert_json_snapshot!(parse_expr("++x".into()).unwrap().1);

        assert_json_snapshot!(parse_expr("typeof x".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("typeof module === \"object\"".into()).unwrap().1);

        assert_json_snapshot!(parse_expr("!x".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("!(x && y)".into()).unwrap().1);
    }

    #[test]
    fn test_expr_bp_postfix() {
        assert_json_snapshot!(parse_expr("x++".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x++\t".into()).unwrap().1); // make sure end pos does not include trailing whitespace
        assert_json_snapshot!(parse_expr("x--".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x++ + 1".into()).unwrap().1); // ++ binds tighter than +
    }

    #[test]
    fn test_expr_bp_assignment() {
        assert_json_snapshot!(parse_expr("x = 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x = 1 + 2".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x += 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x += y += 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x += x * x".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x = a ? b : c".into()).unwrap().1);
        assert_json_snapshot!(
            parse_expr("x = a ? myFunc : function () { return c; }".into())
                .unwrap()
                .1
        );

        assert_json_snapshot!(parse_expr("hello.value = \"world\"".into()).unwrap().1);
        assert_json_snapshot!(
            parse_expr("myFunc = function () { return 0; }".into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(parse_expr("a = b = c;".into()).unwrap().1);

        assert_json_snapshot!(parse_expr("x -= x * x".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x *= x * x".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x /= x * x".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x %= x * x".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x <<= 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x >>= 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x >>>= 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x |= 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x ^= 1".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x &= 1".into()).unwrap().1);

        assert_json_snapshot!(parse_expr("x[1] = a".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x[\"foo\"] = a".into()).unwrap().1);

        assert_json_snapshot!(
            parse_expr(
                r#"identifier = "(?:\\\\[\\da-fA-F]{1,6}" + whitespace +
				"?|\\\\[^\\r\\n\\f]|[\\w-]|[^\0-\\x7f])+""#
                    .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_expr_bp_member_expr() {
        assert_json_snapshot!(parse_expr("x.y".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x.y.z".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x.y[z]".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x.y[\"z\"]".into()).unwrap().1);
        assert_json_snapshot!(parse_expr("x.y[arr.length - 1]".into()).unwrap().1);

        assert_json_snapshot!(
            parse_expr("x.y()\n// abc\n/* 123 */\n.z()".into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(
            parse_expr("x.y(123)\n// abc\n/* 123 */\n.z(abc)".into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(
            parse_expr(
                r#"test
                    .then(fn)
                    // properties can be reserved words ("catch" keyword)
                    .catch(function (error) {
                        console.error(error);
                    });"#
                    .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_expr_bp_no_seq() {
        assert_json_snapshot!(parse_expr_no_seq("x, y".into()).unwrap().1); // should not parse ", y"
        assert_json_snapshot!(
            parse_expr_no_seq(
                "test() ? function () { return 0; } : function() { return 1; }, y".into()
            )
            .unwrap()
            .1
        ); // should not parse ", y"
    }
}
