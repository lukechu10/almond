//! Parsing for JS identifiers

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, character::complete::*, combinator::*, IResult};
use nom_locate::position;

/// Parses any 4 hex digits unicode escape sequence
fn unicode_esc_seq(s: Span) -> IResult<Span, char> {
    let (s, _) = tag("\\u")(s)?;
    let (s, hex_str) = recognize(count(one_of("1234567890abcdefABCDEF"), 4))(s)?;
    let hex_u32 = u32::from_str_radix(*hex_str, 16).unwrap(); // FIXME
    let char = std::char::from_u32(hex_u32).unwrap(); // FIXME
    Ok((s, char))
}

pub fn identifier_start(s: Span) -> IResult<Span, char> {
    verify(alt((unicode_esc_seq, anychar)), |c: &char| match c {
        c if unicode_xid::UnicodeXID::is_xid_start(*c) => true,
        '$' | '_' => true,
        _ => false,
    })(s)
}

pub fn identifier_continue(s: Span) -> IResult<Span, char> {
    verify(alt((unicode_esc_seq, anychar)), |c: &char| match c {
        c if unicode_xid::UnicodeXID::is_xid_continue(*c) => true,
        '$' | '_' => true,
        _ => false,
    })(s)
}

pub fn identifier(s: Span) -> IResult<Span, Node> {
    let (s, start) = position(s)?;
    let (s, name) = verify(
        map(
            pair(identifier_start, many0(identifier_continue)),
            |(c, v)| format!("{}{}", c, v.into_iter().collect::<String>()),
        ),
        |ident: &str| not(reserved_word)(ident.into()).is_ok(),
    )(s)?;
    let (s, _) = spaces0(s)?;
    let (s, end) = position(s)?;

    Ok((s, NodeKind::Identifier { name }.with_pos(start, end)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        all_consuming(identifier)("myVar".into()).unwrap();
        all_consuming(identifier)("abc123".into()).unwrap();
        all_consuming(identifier)("$".into()).unwrap();
        all_consuming(identifier)("_".into()).unwrap();
        all_consuming(identifier)("$elem".into()).unwrap();
        all_consuming(identifier)("_elem".into()).unwrap();
        all_consuming(identifier)("$123".into()).unwrap();
        all_consuming(identifier)("_123".into()).unwrap();

        // identifiers can start with keywords
        all_consuming(identifier)("var_id".into()).unwrap();
        all_consuming(identifier)("class_id".into()).unwrap();
    }

    #[test]
    fn test_identifier_with_trailing_whitespace() {
        all_consuming(identifier)("abc123 ".into()).unwrap();
        all_consuming(identifier)("abc123 \t\n ".into()).unwrap();
    }

    #[test]
    fn test_bad_identifier() {
        all_consuming(identifier)("123abc".into()).unwrap_err();
    }

    #[test]
    fn test_keyword_not_identifier() {
        all_consuming(identifier)("var".into()).unwrap_err();
        all_consuming(identifier)("class".into()).unwrap_err();
        all_consuming(identifier)("true".into()).unwrap_err();
        all_consuming(identifier)("null".into()).unwrap_err();
    }

    #[test]
    fn test_identifier_with_unicode_escape() {
        assert_eq!(
            identifier(r#"abc\u0061"#.into()).unwrap().1.kind,
            NodeKind::Identifier {
                name: "abca".into()
            }
        );
        assert_eq!(
            identifier(r#"abc\u0061\u0062123"#.into()).unwrap().1.kind,
            NodeKind::Identifier {
                name: "abcab123".into()
            }
        );
    }
}
