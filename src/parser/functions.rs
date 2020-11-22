//! Parsing for JS functions and programs.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::InputLength;
use nom_locate::position;

/// Parses a complete JS program
pub fn parse_program(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;
    let (s, _) = sp0(s)?; // eat all preceding whitespace

    let (s, body) = parse_function_body_inner(s)?;
    if s.input_len() != 0 {
        panic!("The source code was not completely parsed. This is a bug with the parser.\nUnparsed code starting on line {}", s.location_line());
    }

    let (s, end) = position(s)?; // Program loc should include all trailing whitespace
    Ok((s, NodeKind::Program { body }.with_pos(start, end)))
}

pub fn parse_declaration(s: Span) -> ParseResult<Node> {
    parse_function_declaration(s)
}

pub fn parse_function_declaration(s: Span) -> ParseResult<Node> {
    let parse_function_declaration_signature = pair(
        preceded(ws1(keyword_function), parse_identifier),
        delimited(ws0(tag("(")), parse_formal_param_list, ws0(tag(")"))),
    );

    context(
        "function declaration",
        map(
            spanned(pair(
                parse_function_declaration_signature,
                parse_function_body,
            )),
            |(((id, params), body), start, end)| {
                NodeKind::FunctionDeclaration {
                    function: Function {
                        id: Box::new(Some(id)),
                        params,
                        body: Box::new(body),
                    },
                }
                .with_pos(start, end)
            },
        ),
    )(s)
}

pub fn parse_function_expr(s: Span) -> ParseResult<Node> {
    let parse_function_expr_signature = pair(
        preceded(ws0(keyword_function), opt(parse_identifier)),
        delimited(ws0(tag("(")), parse_formal_param_list, ws0(tag(")"))),
    );

    context(
        "function expression",
        map(
            spanned(pair(parse_function_expr_signature, parse_function_body)),
            |(((id, params), body), start, end)| {
                NodeKind::FunctionExpression {
                    function: Function {
                        id: Box::new(id),
                        params,
                        body: Box::new(body),
                    },
                }
                .with_pos(start, end)
            },
        ),
    )(s)
}

pub fn parse_function_body(s: Span) -> ParseResult<Node> {
    context(
        "function body",
        map(
            spanned(delimited(
                ws0(tag("{")),
                parse_function_body_inner,
                ws0(tag("}")),
            )),
            |(body, start, end)| NodeKind::BlockStatement { body }.with_pos(start, end),
        ),
    )(s)
}

pub fn parse_function_body_inner(s: Span) -> ParseResult<Vec<Node>> {
    let parse_directive_list = many0(parse_directive);
    let parse_source_element_list = many0(parse_source_elem);

    context("function body inner", map(
        pair(parse_directive_list, parse_source_element_list),
        |(mut directives, stmts)| {
            directives.extend(stmts);
            directives
        },
    ))(s)
}

pub fn parse_formal_param_list(s: Span) -> ParseResult<Vec<Node>> {
    terminated(
        separated_list0(ws0(char(',')), parse_formal_param),
        // trailing comma
        opt(ws0(char(','))),
    )(s)
}

pub fn parse_formal_param(s: Span) -> ParseResult<Node> {
    parse_identifier(s)
}

/// Example: `"use strict";`
pub fn parse_directive(s: Span) -> ParseResult<Node> {
    let parse_directive_literal = spanned(terminated(ws0(string_lit), opt(ws0(semi))));
    map(parse_directive_literal, |(node, start, end)| {
        match &node.kind {
            NodeKind::Literal { value } => match value {
                LiteralValue::String(directive) => {
                    let directive = directive.clone();
                    NodeKind::ExpressionStatement {
                        expression: Box::new(node),
                        directive: Some(directive),
                    }
                    .with_pos(start, end)
                }
                _ => unreachable!(
                    "string_lit should return a literal with value LiteralValue::String"
                ),
            },
            _ => unreachable!("string_lit should return a Node::Identifier"),
        }
    })(s)
}

pub fn parse_source_elem(s: Span) -> ParseResult<Node> {
    alt((parse_declaration, parse_stmt))(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_json_snapshot;

    #[test]
    fn test_directive() {
        assert_json_snapshot!(parse_directive("\"use strict\"".into()).unwrap().1);
    }

    #[test]
    fn test_function_declaration() {
        assert_json_snapshot!(
            parse_function_declaration(
                r#"function a() {
                    "use strict"
                    return 1
                }"#
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_function_declaration(r#"function a() {}"#.into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(
            parse_function_declaration(r#"function a(param1, param2) {}"#.into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(
            parse_function_declaration(r#"function a(param1, param2,) {}"#.into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(
            parse_function_declaration(
                r#"function a() {
                    "use strict"
                    "also directive"
                    a;
                    "not directive"
                    return 1
                }"#
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_function_declaration(
                r#"function fib() {
                    if (x === 0) return 0;
                    else if (x === 1) return 1;
                    else return fib(x - 1) + fib(x - 2);
                }"#
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_program(
                r#"function foo() {
                    // stuff
                }; foo();"# // note the `;` after the function declaration. Should be parsed as an `EmptyStatement`.
                    .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_nested_function_declaration() {
        assert_json_snapshot!(
            parse_function_declaration(
                r#"function a() {
                    function b() {
                        a();
                    }
                }"#
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_function_expr() {
        parse_function_declaration(
            r#"function a() {
                // function expressions should be wrapped up in parenthesis
                function () {
                    a();
                }
            }"#
            .into(),
        )
        .unwrap_err();

        assert_json_snapshot!(
            parse_function_declaration(
                r#"function a() {
                    // anonymous function:
                    (function () {
                        a();
                    })
                }"#
                .into()
            )
            .unwrap()
            .1
        );

        assert_json_snapshot!(
            parse_function_declaration(
                r#"function a() {
                    // function expression
                    0, function b() {}
                }"#
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn parse_immediately_invoked_function_expr() {
        assert_json_snapshot!(
            parse_stmt(
                r#"(function () {
                    123; // something
                })()"#
                    .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_program() {
        assert_json_snapshot!(
            parse_program(
                r#"var x = 1;
                function foo() {
                    return x;
                }"#
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_program(
                r#"var x = 1;
                function foo() {
                    if (x) {
                        return x;
                    }
                }"#
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_program(
                r#"
                function fib() {
                    if (x === 0) return 0;
                    else if (x === 1) return 1;
                    else return fib(x - 1) + fib(x - 2);
                }
                var x = fib(10);"#
                    .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_program(
                r#"function foo() {
                    test;
                    // abc "123"-
                    // test
                    foo();
                }"#
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_empty_function() {
        assert_json_snapshot!(
            parse_program(
                r#"function foo(callback) {
                    // A👍a
                }"#
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn parse_program_immediately_invoked_function_expr() {
        assert_json_snapshot!(
            parse_program(
                r#"(function () {
                    123; // something
                })()"#
                    .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_program(
                r#"(function (global, factory) {
                    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
                        typeof define === 'function' && define.amd ? define(['exports'], factory) :
                            (global = global || self, factory(global.Library = {}));
                }(this, (function (exports) {})));"#
                    .into()
            )
            .unwrap()
            .1
        );
    }
}
