//! Parsing for JS functions and programs.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*};

/// Parses a complete JS program
pub fn parse_program(_s: Span) -> ParseResult<Node> {
    todo!();
}

pub fn parse_declaration(s: Span) -> ParseResult<Node> {
    parse_function_declaration(s)
}

pub fn parse_function_declaration(s: Span) -> ParseResult<Node> {
    let parse_function_declaration_signature = pair(
        preceded(ws0(keyword_function), parse_identifier),
        delimited(ws0(tag("(")), parse_formal_param_list, ws0(tag(")"))),
    );

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
    )(s)
}

pub fn parse_function_expr(_s: Span) -> ParseResult<Node> {
    todo!();
}

pub fn parse_function_body(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(
            ws0(tag("{")),
            parse_function_body_inner,
            ws0(tag("}")),
        )),
        |(body, start, end)| NodeKind::BlockStatement { body }.with_pos(start, end),
    )(s)
}

pub fn parse_function_body_inner(s: Span) -> ParseResult<Vec<Node>> {
    let parse_directive_list = many0(parse_directive);

    map(
        pair(parse_directive_list, parse_stmt_list),
        |(mut directives, stmts)| {
            directives.extend(stmts);
            directives
        },
    )(s)
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
    }
}
