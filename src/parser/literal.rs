//! Parsing for JS literals.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*, number::complete::*, IResult};
use nom_locate::position;

pub fn parse_literal(s: Span) -> IResult<Span, Node> {
    ws0(alt((null_lit, bool_lit, array_lit)))(s)
}

pub fn null_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, _) = tag("null")(s)?;
    let (s, end) = position(s)?;
    Ok((s, LiteralValue::Null.into_node_kind().with_pos(start, end)))
}

pub fn bool_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, val) = alt((
        value(false, pair(tag("false"), not(identifier_continue))),
        value(true, pair(tag("true"), not(identifier_continue))),
    ))(s)?;
    let (s, end) = position(s)?;
    Ok((
        s,
        LiteralValue::Boolean(val)
            .into_node_kind()
            .with_pos(start, end),
    ))
}

pub fn numeric_lit(s: Span) -> ParseResult<Node> {
    let (s, start) = position(s)?;

    let (s, sign) =
        ws0(opt(char('-')))(s).map(|(s, minus)| (s, if minus.is_some() { -1 } else { 1 }))?;

    let (s, num) =
        alt((octal_int_lit, hex_int_lit, decimal_lit))(s).map(|(s, num)| (s, sign as f64 * num))?;
    let (s, end) = position(s)?;
    Ok((
        s,
        LiteralValue::Number(num)
            .into_node_kind()
            .with_pos(start, end),
    ))
}

fn decimal_lit(s: Span) -> ParseResult<f64> {
    recognize_float(s).map(|(s, float_str)| (s, float_str.parse::<f64>().unwrap()))
}

fn octal_int_lit(s: Span) -> ParseResult<f64> {
    let (s, octal_str) = preceded(char('0'), oct_digit1)(s)?;
    Ok((
        s,
        i64::from_str_radix(octal_str.fragment(), 8).unwrap() as f64,
    ))
}

fn hex_int_lit(s: Span) -> ParseResult<f64> {
    let (s, hex_str) = preceded(alt((tag("0x"), tag("0X"))), hex_digit1)(s)?;
    Ok((
        s,
        i64::from_str_radix(hex_str.fragment(), 16).unwrap() as f64,
    ))
}

pub fn array_lit(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, expr_list) = delimited(terminated(char('['), spaces0), parse_expr_list, char(']'))(s)?;
    let (s, end) = position(s)?;
    spaces0(s)?;
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
            numeric_lit("-123".into()).unwrap().1.kind,
            LiteralValue::Number(-123.0).into_node_kind()
        );
        assert_eq!(
            numeric_lit("-0".into()).unwrap().1.kind,
            LiteralValue::Number(-0.0).into_node_kind()
        );
        assert_eq!(
            numeric_lit("-0123".into()).unwrap().1.kind,
            LiteralValue::Number(-83.0).into_node_kind() // in octal
        );
        assert_eq!(
            numeric_lit("-0x123".into()).unwrap().1.kind,
            LiteralValue::Number(-291.0).into_node_kind() // in octal
        );
        assert_eq!(
            numeric_lit("- 123".into()).unwrap().1.kind,
            LiteralValue::Number(-123.0).into_node_kind()
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
        array_lit("[ true, true, ]\n".into()).unwrap();
        array_lit("[   true,\n true,\t ]\t".into()).unwrap();
    }
}
