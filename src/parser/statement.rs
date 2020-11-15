//! Parsing for JS statements.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::*, bytes::complete::*, combinator::*};
use nom_locate::position;

pub fn parse_stmt(s: Span) -> ParseResult<Node> {
    alt((
        parse_block,
        parse_var_stmt,
        parse_empty_stmt,
        parse_labeled_stmt, // labeled_stmt is before expr_stmt to prevent parsing label as an identifier expr_stmt
        parse_expr_stmt,
        parse_if_stmt,
        parse_iteration_stmt,
        parse_continue_stmt,
        parse_break_stmt,
        parse_return_stmt,
        parse_with_stmt,
        parse_switch_stmt,
        parse_throw_stmt,
        parse_try_stmt,
        parse_debugger_stmt,
    ))(s)
}

pub fn parse_block(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(ws0(tag("{")), parse_stmt_list, ws0(tag("}")))),
        |(body, start, end)| NodeKind::BlockStatement { body }.with_pos(start, end),
    )(s)
}

pub fn parse_stmt_list(s: Span) -> ParseResult<Vec<Node>> {
    many0(parse_stmt)(s)
}

fn parse_initializer(s: Span) -> ParseResult<Node> {
    preceded(ws0(tag("=")), parse_expr_no_seq)(s)
}

fn parse_var_declaration(s: Span) -> ParseResult<Node> {
    map(
        tuple((
            position,
            pair(parse_identifier, opt(parse_initializer)),
            position,
        )),
        |(start, (id, init), end)| {
            NodeKind::VariableDeclarator {
                id: Box::new(id),
                init: Box::new(init),
            }
            .with_pos(start, end)
        },
    )(s)
}

fn parse_declaration_list(s: Span) -> ParseResult<Vec<Node>> {
    separated_list1(ws0(tag(",")), ws0(parse_var_declaration))(s)
}

pub fn parse_var_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(
            ws1(keyword_var),
            parse_declaration_list,
            opt(ws0(semi)),
        )),
        |(declarations, start, end)| {
            NodeKind::VariableDeclaration {
                declarations,
                kind: VariableDeclarationKind::Var,
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_var_stmt_no_semi(s: Span) -> ParseResult<Node> {
    map(
        spanned(preceded(ws1(keyword_var), parse_declaration_list)),
        |(declarations, start, end)| {
            NodeKind::VariableDeclaration {
                declarations,
                kind: VariableDeclarationKind::Var,
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_empty_stmt(s: Span) -> ParseResult<Node> {
    map(spanned(tag(";")), |(_, start, end)| {
        NodeKind::EmptyStatement.with_pos(start, end)
    })(s)
}

pub fn parse_expr_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(preceded(
            // ExpressionStatement cannot start with `function` keyword. Function expressions must be wrapped in parenthesis.
            // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/function#Syntax
            not(keyword_function), 
            terminated(parse_expr, opt(ws0(semi))))),
        |(expr, start, end)| {
            NodeKind::ExpressionStatement {
                expression: Box::new(expr),
                directive: None,
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_if_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            preceded(
                ws0(keyword_if),
                pair(
                    delimited(ws0(tag("(")), parse_expr, ws0(tag(")"))),
                    parse_stmt,
                ),
            ),
            opt(preceded(ws0(keyword_else), parse_stmt)),
        )),
        |(((test, consequent), alternate), start, end)| {
            NodeKind::IfStatement {
                test: Box::new(test),
                consequent: Box::new(consequent),
                alternate: Box::new(alternate),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_iteration_stmt(s: Span) -> ParseResult<Node> {
    alt((
        parse_do_while_stmt,
        parse_while_stmt,
        parse_for_stmt,
        parse_for_in_stmt,
    ))(s)
}

pub fn parse_do_while_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            preceded(ws0(keyword_do), parse_stmt),
            delimited(
                pair(ws0(keyword_while), ws0(tag("("))),
                parse_expr,
                pair(ws0(tag(")")), opt(ws0(semi))),
            ),
        )),
        |((body, test), start, end)| {
            NodeKind::DoWhileStatement {
                body: Box::new(body),
                test: Box::new(test),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_while_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            delimited(
                pair(ws0(keyword_while), ws0(tag("("))),
                parse_expr,
                ws0(tag(")")),
            ),
            parse_stmt,
        )),
        |((test, body), start, end)| {
            NodeKind::WhileStatement {
                body: Box::new(body),
                test: Box::new(test),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_for_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            delimited(
                pair(ws0(keyword_for), ws0(tag("("))),
                tuple((
                    terminated(
                        opt(alt((parse_var_stmt_no_semi, parse_expr))),
                        ws0(tag(";")),
                    ),
                    terminated(opt(parse_expr), ws0(tag(";"))),
                    opt(parse_expr),
                )),
                ws0(tag(")")),
            ),
            parse_stmt,
        )),
        |(((init, test, update), body), start, end)| {
            NodeKind::ForStatement {
                init: Box::new(init),
                test: Box::new(test),
                update: Box::new(update),
                body: Box::new(body),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_for_in_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            delimited(
                pair(ws0(keyword_for), ws0(tag("("))),
                separated_pair(
                    alt((parse_var_stmt, |s| parse_expr_bp(s, 23 /* no in */))),
                    ws0(keyword_in),
                    parse_expr,
                ),
                ws0(tag(")")),
            ),
            parse_stmt,
        )),
        |(((left, right), body), start, end)| {
            NodeKind::ForInStatement {
                left: Box::new(left),
                right: Box::new(right),
                body: Box::new(body),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_continue_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(
            ws_no_nl0(keyword_continue),
            opt(parse_identifier),
            // ws0 is on outside to eat space after ws_no_nl0 in continue keyword
            ws0(opt(semi)),
        )),
        |(label, start, end)| {
            NodeKind::ContinueStatement {
                label: Box::new(label),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_break_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(
            ws_no_nl0(keyword_break),
            opt(parse_identifier),
            // ws0 is on outside to eat space after ws_no_nl0 in break keyword
            ws0(opt(semi)),
        )),
        |(label, start, end)| {
            NodeKind::BreakStatement {
                label: Box::new(label),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_return_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(
            ws_no_nl0(keyword_return),
            opt(parse_expr),
            // ws0 is on outside to eat space after ws_no_nl0 in return keyword
            ws0(opt(semi)),
        )),
        |(argument, start, end)| {
            NodeKind::ReturnStatement {
                argument: Box::new(argument),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_with_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            delimited(
                pair(ws0(keyword_with), ws0(tag("("))),
                parse_expr,
                ws0(tag(")")),
            ),
            parse_stmt,
        )),
        |((object, body), start, end)| {
            NodeKind::WithStatement {
                object: Box::new(object),
                body: Box::new(body),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_switch_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            delimited(
                pair(ws0(keyword_switch), ws0(tag("("))),
                parse_expr,
                ws0(tag(")")),
            ),
            parse_case_block,
        )),
        |((descriminant, cases), start, end)| {
            NodeKind::SwitchStatement {
                discriminant: Box::new(descriminant),
                cases,
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_case_block(s: Span) -> ParseResult<Vec<Node>> {
    delimited(
        ws0(tag("{")),
        map(
            tuple((
                many0(parse_case_clause),
                opt(parse_default_clause),
                many0(parse_case_clause),
            )),
            |(mut first, second, third)| {
                if let Some(second) = second {
                    first.push(second);
                }
                first.extend(third);
                first
            },
        ),
        ws0(tag("}")),
    )(s)
}

pub fn parse_case_clause(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            delimited(ws0(keyword_case), parse_expr, ws0(tag(":"))),
            many0(parse_stmt),
        )),
        |((test, consequent), start, end)| {
            NodeKind::SwitchCase {
                test: Box::new(Some(test)),
                consequent,
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_default_clause(s: Span) -> ParseResult<Node> {
    map(
        spanned(preceded(
            pair(ws0(keyword_default), ws0(tag(":"))),
            many0(parse_stmt),
        )),
        |(consequent, start, end)| {
            NodeKind::SwitchCase {
                test: Box::new(None),
                consequent,
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_labeled_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(separated_pair(parse_identifier, ws0(tag(":")), parse_stmt)),
        |((label, body), start, end)| {
            NodeKind::LabeledStatement {
                label: Box::new(label),
                body: Box::new(body),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_try_stmt(s: Span) -> ParseResult<Node> {
    map(
        verify(
            spanned(tuple((
                preceded(ws0(keyword_try), parse_block),
                opt(parse_catch),
                opt(parse_finally),
            ))),
            |((_, handler, finalizer), _, _)| 
                // make sure there is at least one catch or finally (or both)
                handler.is_some() || finalizer.is_some(),
        ),
        |((block, handler, finalizer), start, end)| {
            NodeKind::TryStatement {
                block: Box::new(block),
                handler: Box::new(handler),
                finalizer: Box::new(finalizer),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_throw_stmt(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(ws0(keyword_throw), parse_expr, opt(ws0(semi)))),
        |(argument, start, end)| {
            NodeKind::ThrowStatement {
                argument: Box::new(argument),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_catch(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(
            delimited(
                pair(ws0(keyword_catch), ws0(tag("("))),
                parse_formal_param,
                ws0(tag(")")),
            ),
            parse_block,
        )),
        |((param, body), start, end)| {
            NodeKind::CatchClause {
                param: Box::new(param),
                body: Box::new(body),
            }
            .with_pos(start, end)
        },
    )(s)
}

pub fn parse_finally(s: Span) -> ParseResult<Node> {
    preceded(ws0(keyword_finally), parse_block)(s)
}

pub fn parse_debugger_stmt(s: Span) -> ParseResult<Node> {
    map(spanned(terminated(ws0(keyword_debugger), opt(ws0(semi)))), |(_, start, end)| NodeKind::DebuggerStatement.with_pos(start, end))(s)
}

pub fn parse_formal_param_list(s: Span) -> ParseResult<Vec<Node>> {
    many0(parse_formal_param)(s)
}

pub fn parse_formal_param(s: Span) -> ParseResult<Node> {
    parse_identifier(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_json_snapshot;

    #[test]
    fn test_var_stmt() {
        assert_json_snapshot!(parse_stmt("var x = 1;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("var x, y = 1;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("var x = 1, y;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("var x".into()).unwrap().1); // auto insert semi
        assert_json_snapshot!(parse_stmt("var x\t".into()).unwrap().1);
    }

    #[test]
    fn test_empty_stmt() {
        assert_json_snapshot!(parse_stmt(";".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt(";\t".into()).unwrap().1);
    }

    #[test]
    fn test_expr_stmt() {
        assert_json_snapshot!(parse_stmt("x + y;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("x + y\n".into()).unwrap().1);
    }

    #[test]
    fn test_if_stmt() {
        assert_json_snapshot!(parse_stmt("if (x) { 1; }".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("if (x) 1;".into()).unwrap().1);

        assert_json_snapshot!(parse_stmt("if (x) { 1; } else { 2; }".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("if (x) 1; else 2;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("if (x) 1\nelse 2".into()).unwrap().1);
    }

    #[test]
    fn test_do_while_stmt() {
        assert_json_snapshot!(
            parse_stmt(
                "do {
                    1;
                } while (x);"
                    .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_while_stmt() {
        assert_json_snapshot!(
            parse_stmt(
                "while (x) {
                    1;
                }"
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_for_stmt() {
        assert_json_snapshot!(
            parse_stmt(
                "for (var x = 0; x < 10; x++) {
                    x;
                }"
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(parse_stmt("for (;true;) { }".into()).unwrap().1);
    }

    #[test]
    fn test_for_in_stmt() {
        assert_json_snapshot!(
            parse_stmt(
                "for (var elem in arr) {
                    elem;
                }"
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_continue_stmt() {
        assert_json_snapshot!(parse_stmt("continue;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("continue a;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("continue\na;".into()).unwrap().1); // should be ContinueStatement followed by ExpressionStatement
        assert_json_snapshot!(parse_stmt("continue a".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("continue\t".into()).unwrap().1);
    }

    #[test]
    fn test_break_stmt() {
        assert_json_snapshot!(parse_stmt("break;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("break a;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("break\na;".into()).unwrap().1); // should be ContinueStatement followed by ExpressionStatement
        assert_json_snapshot!(parse_stmt("break a".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("break\t".into()).unwrap().1);
    }

    #[test]
    fn test_return_stmt() {
        assert_json_snapshot!(parse_stmt("return;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("return a;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("return\na;".into()).unwrap().1); // should be ContinueStatement followed by ExpressionStatement
        assert_json_snapshot!(parse_stmt("return a".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("return\t".into()).unwrap().1);
    }

    #[test]
    fn test_with_stmt() {
        assert_json_snapshot!(
            parse_stmt("with (object) { expression; }".into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(parse_stmt("with (object) expression;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("with (object) expression".into()).unwrap().1);
    }

    #[test]
    fn test_switch_stmt() {
        assert_json_snapshot!(
            parse_stmt(
                "switch (x) {
                    case t:
                        something;
                        break;
                }"
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_stmt(
                "switch (x) {
                    case t:
                        break;
                    default:
                        something;
                        break;
                }"
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_stmt(
                "switch (x) {
                    case t:
                        break;
                    default:
                        something;
                        break;
                    case other:
                        break;
                }"
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_labeled_stmt() {
        assert_json_snapshot!(parse_stmt("label: stmt;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("label: stmt".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("$: console.log(a);".into()).unwrap().1);
        assert_json_snapshot!(
            parse_stmt(
                "label: {
                    stmt;
                }"
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_throw_stmt() {
        assert_json_snapshot!(parse_stmt("throw true;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("throw 3;".into()).unwrap().1);
    }

    #[test]
    fn test_try_stmt() {
        assert_json_snapshot!(parse_stmt("try { something; } catch (err) {}".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("try { something; } finally {}".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("try { something; } catch (err) {} finally {}".into()).unwrap().1);
        parse_stmt("try { something; }".into()).unwrap_err();
    }

    #[test]
    fn test_debugger_stmt() {
        assert_json_snapshot!(parse_stmt("debugger;".into()).unwrap().1);
        assert_json_snapshot!(parse_stmt("debugger".into()).unwrap().1);
    }
}
