//! Parsing for JS literals.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;

pub fn parse_literal(s: Span) -> ParseResult<Node> {
    ws0(alt((
        null_lit,
        bool_lit,
        numeric_lit,
        string_lit,
        array_lit,
        object_lit,
        regex_lit, // defined in regex.rs
    )))(s)
}

pub fn null_lit(s: Span) -> ParseResult<Node> {
    map(
        spanned(pair(tag("null"), not(identifier_continue))),
        |(_, start, end)| LiteralValue::Null.into_node_kind().with_pos(start, end),
    )(s)
}

pub fn bool_lit(s: Span) -> ParseResult<Node> {
    map(
        spanned(alt((
            value(false, pair(tag("false"), not(identifier_continue))),
            value(true, pair(tag("true"), not(identifier_continue))),
        ))),
        |(val, start, end)| {
            LiteralValue::Boolean(val)
                .into_node_kind()
                .with_pos(start, end)
        },
    )(s)
}

pub fn numeric_lit(s: Span) -> ParseResult<Node> {
    map(
        spanned(alt((octal_int_lit, hex_int_lit, decimal_lit))),
        |(num, start, end)| {
            LiteralValue::Number(num)
                .into_node_kind()
                .with_pos(start, end)
        },
    )(s)
}

fn decimal_lit(s: Span) -> ParseResult<f64> {
    recognize_float(s).map(|(s, float_str)| (s, float_str.parse::<f64>().unwrap()))
}

fn octal_int_lit(s: Span) -> ParseResult<f64> {
    map(preceded(char('0'), oct_digit1), |octal_str: Span| {
        i64::from_str_radix(octal_str.fragment(), 8).unwrap() as f64
    })(s)
}

fn hex_int_lit(s: Span) -> ParseResult<f64> {
    map(
        preceded(alt((tag("0x"), tag("0X"))), hex_digit1),
        |hex_str: Span| i64::from_str_radix(hex_str.fragment(), 16).unwrap() as f64,
    )(s)
}

/// Parses a valid character in a double quote string.
fn character_double_quote(s: Span) -> ParseResult<char> {
    let (input, c) = none_of("\"")(s)?;
    if c == '\\' {
        alt((
            map_res(anychar, |c| {
                Ok(match c {
                    '\'' | '"' | '\\' | '/' => c,
                    'b' => '\x08',
                    'f' => '\x0C',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    // vertical tab
                    'v' => '\x0b',
                    '0' => '\0',
                    _ => c,
                    #[allow(unreachable_patterns)]
                    // Only for type inference purposes. Case on top should catch all cases.
                    _ => return Err(()),
                })
            }),
            preceded(char('u'), unicode_esc_seq),
        ))(input)
    } else {
        Ok((input, c))
    }
}

/// Parses a valid character in a single quote string.
fn character_single_quote(s: Span) -> ParseResult<char> {
    let (input, c) = none_of("\'")(s)?;
    if c == '\\' {
        alt((
            map_res(anychar, |c| {
                Ok(match c {
                    '\'' | '"' | '\\' | '/' => c,
                    'b' => '\x08',
                    'f' => '\x0C',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    // vertical tab
                    'v' => '\x0b',
                    '0' => '\0',
                    _ => c,
                    #[allow(unreachable_patterns)]
                    // Only for type inference purposes. Case on top should catch all cases.
                    _ => return Err(()),
                })
            }),
            preceded(char('u'), unicode_esc_seq),
        ))(input)
    } else {
        Ok((input, c))
    }
}

pub fn string_lit(s: Span) -> ParseResult<Node> {
    context(
        "string literal",
        map(
            spanned(alt((
                context(
                    "string literal double quotes",
                    delimited(
                        char('"'),
                        fold_many0(character_double_quote, String::new(), |mut string, c| {
                            string.push(c);
                            string
                        }),
                        char('"'),
                    ),
                ),
                context(
                    "string literal single quotes",
                    delimited(
                        char('\''),
                        fold_many0(character_single_quote, String::new(), |mut string, c| {
                            string.push(c);
                            string
                        }),
                        char('\''),
                    ),
                ),
            ))),
            |(string, start, end)| {
                LiteralValue::String(string)
                    .into_node_kind()
                    .with_pos(start, end)
            },
        ),
    )(s)
}

pub fn array_lit(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(
            ws0(char('[')),
            parse_expr_list_with_opt_expr,
            ws0(char(']')),
        )),
        |(expr_list, start, end)| {
            NodeKind::ArrayExpression {
                elements: expr_list,
            }
            .with_pos(start, end)
        },
    )(s)
}

fn parse_property_name(s: Span) -> ParseResult<Node> {
    alt((
        parse_identifier_name, // not parse_identifier
        ws0(string_lit),
        ws0(numeric_lit),
    ))(s)
}

fn parse_property_assignment(s: Span) -> ParseResult<Node> {
    let simple = map(
        spanned(separated_pair(
            parse_property_name,
            ws0(tag(":")),
            parse_expr_no_seq,
        )),
        |((key, value), start, end)| {
            NodeKind::Property {
                key: Box::new(key),
                value: Box::new(value),
                kind: PropertyKind::Init,
            }
            .with_pos(start, end)
        },
    );
    let getter = map(
        spanned(tuple((
            ws0(keyword_get),
            parse_property_name,
            spanned(tuple((ws0(tag("(")), ws0(tag(")")), parse_function_body))),
        ))),
        |((_, key, ((_, _, body), start_func, end_func)), start, end)| {
            NodeKind::Property {
                key: Box::new(key),
                value: Box::new(
                    NodeKind::FunctionExpression {
                        function: Function {
                            body: Box::new(body),
                            id: Box::new(None),
                            params: Vec::new(),
                        },
                    }
                    .with_pos(start_func, end_func),
                ),
                kind: PropertyKind::Get,
            }
            .with_pos(start, end)
        },
    );
    let setter = map(
        spanned(tuple((
            ws0(keyword_set),
            parse_property_name,
            spanned(tuple((
                ws0(tag("(")),
                parse_formal_param,
                ws0(tag(")")),
                parse_function_body,
            ))),
        ))),
        |((_, key, ((_, param, _, body), start_func, end_func)), start, end)| {
            NodeKind::Property {
                key: Box::new(key),
                value: Box::new(
                    NodeKind::FunctionExpression {
                        function: Function {
                            body: Box::new(body),
                            id: Box::new(None),
                            params: vec![param],
                        },
                    }
                    .with_pos(start_func, end_func),
                ),
                kind: PropertyKind::Set,
            }
            .with_pos(start, end)
        },
    );

    alt((getter, setter, simple))(s)
}

pub fn object_lit(s: Span) -> ParseResult<Node> {
    map(
        spanned(delimited(
            ws0(tag("{")),
            alt((
                // with trailing comma
                terminated(
                    separated_list1(ws0(tag(",")), parse_property_assignment),
                    ws0(tag(",")),
                ),
                // without trailing comma
                separated_list0(ws0(tag(",")), parse_property_assignment),
            )),
            ws0(tag("}")),
        )),
        |(properties, start, end)| NodeKind::ObjectExpression { properties }.with_pos(start, end),
    )(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_json_snapshot;

    #[test]
    fn test_bool_lit() {
        bool_lit("true".into()).unwrap();
        bool_lit("false".into()).unwrap();
    }

    #[test]
    fn test_bool_lit_with_trailing_whitespace() {
        bool_lit("true ".into()).unwrap();
        bool_lit("false ".into()).unwrap();
    }

    #[test]
    fn test_bad_bool_lit() {
        bool_lit("bad".into()).unwrap_err();
        bool_lit("badfalse".into()).unwrap_err();
        bool_lit("falsebad".into()).unwrap_err();
    }

    #[test]
    fn test_numeric_lit() {
        assert_eq!(
            numeric_lit("123".into()).unwrap().1.kind,
            LiteralValue::Number(123.0).into_node_kind()
        );
        assert_eq!(
            numeric_lit("0123".into()).unwrap().1.kind,
            LiteralValue::Number(83.0).into_node_kind() // in octal
        );
        assert_eq!(
            numeric_lit("0x123".into()).unwrap().1.kind,
            LiteralValue::Number(291.0).into_node_kind() // in hex
        );
        assert_eq!(
            numeric_lit("0".into()).unwrap().1.kind,
            LiteralValue::Number(0.0).into_node_kind()
        );
        assert_eq!(
            numeric_lit("0x0".into()).unwrap().1.kind,
            LiteralValue::Number(0.0).into_node_kind() // hex 0
        );
    }

    #[test]
    fn test_string_lit() {
        assert_eq!(
            parse_literal(r#""my string""#.into()).unwrap().1.kind,
            LiteralValue::String("my string".into()).into_node_kind()
        );
        assert_eq!(
            parse_literal(r#"'my string'"#.into()).unwrap().1.kind,
            LiteralValue::String("my string".into()).into_node_kind()
        );
        assert_eq!(
            parse_literal(r#"'"quoted"'"#.into()).unwrap().1.kind,
            LiteralValue::String("\"quoted\"".into()).into_node_kind()
        );
        assert_eq!(
            parse_literal(r#""'single'""#.into()).unwrap().1.kind,
            LiteralValue::String("'single'".into()).into_node_kind()
        );
        assert_eq!(
            parse_literal(r#""\"""#.into()).unwrap().1.kind,
            LiteralValue::String("\"".into()).into_node_kind()
        );
        assert_eq!(
            parse_literal(r#""\n""#.into()).unwrap().1.kind,
            LiteralValue::String("\n".into()).into_node_kind()
        );
        assert_eq!(
            parse_literal(r#""?|\\\\[^\\r\\n\\f]|[\\w-]|[^\0-\\x7f])+""#.into())
                .unwrap()
                .1
                .kind,
            LiteralValue::String("?|\\\\[^\\r\\n\\f]|[\\w-]|[^\0-\\x7f])+".into()).into_node_kind()
        );
        // unicode escape sequences
        assert_eq!(
            parse_literal(r#""\uFFFD""#.into()).unwrap().1.kind,
            LiteralValue::String("\u{fffd}".into()).into_node_kind()
        );
        assert_eq!(
            parse_literal(r#""\ufffd""#.into()).unwrap().1.kind,
            LiteralValue::String("\u{fffd}".into()).into_node_kind()
        );
    }

    #[test]
    fn test_array_lit() {
        array_lit("[]".into()).unwrap();
        array_lit("[true, true]".into()).unwrap();
    }

    #[test]
    fn test_array_lit_with_whitespace() {
        array_lit("[ true, true ]\n".into()).unwrap();
        array_lit("[   true,\n true\t ]\t".into()).unwrap();
    }

    #[test]
    fn test_array_lit_with_trailing_comma() {
        assert_json_snapshot!(array_lit("[ true, true, ]\n".into()).unwrap().1);
        array_lit("[   true,\n true,\t ]\t".into()).unwrap();
    }

    #[test]
    fn test_obj_lit() {
        assert_json_snapshot!(parse_literal(r#"{ abc: "foo" }"#.into()).unwrap().1);
        parse_literal(r#"{ abc: 1, 2, 3 }"#.into()).unwrap_err();
        assert_json_snapshot!(
            parse_literal(r#"{ abc: 123, def: { foo: "bar" } }"#.into())
                .unwrap()
                .1
        );
        assert_json_snapshot!(parse_literal(r#"{}"#.into()).unwrap().1);
        assert_json_snapshot!(parse_literal(r#"{ }"#.into()).unwrap().1);

        assert_json_snapshot!(
            parse_literal(
                r#"{
                    test: function () {
                        return {};
                    },
                    value: 1 + 2,
                    foo: "abc"
                }"#
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_literal(
                r#"{
                    get: function () {
                        return {};
                    }
                }"#
                .into()
            )
            .unwrap()
            .1
        );
        assert_json_snapshot!(
            parse_literal(
                r#"{
                    get: test() ? function () {
                        return 0;
                    } : function () {
                        return 1;
                    },
                    foo: bar
                }"#
                .into()
            )
            .unwrap()
            .1
        );
    }

    #[test]
    fn test_obj_lit_with_trailing_comma() {
        assert_json_snapshot!(parse_literal(r#"{ abc: "foo", }"#.into()).unwrap().1);
        assert_json_snapshot!(
            parse_literal(r#"{ abc: 123, def: { foo: "bar", }, }"#.into())
                .unwrap()
                .1
        );
        parse_literal(r#"{,}"#.into()).unwrap_err();
        parse_literal(r#"{ , }"#.into()).unwrap_err();
    }

    #[test]
    fn test_obj_lit_getter_setter() {
        assert_json_snapshot!(
            parse_literal(
                r#"{
                    get a() {
                        a;
                    },
                    set b(x) {
                        b;
                    }
                }"#
                .into()
            )
            .unwrap()
            .1
        );
    }
}
