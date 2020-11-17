//! Parsing for JS identifiers

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom_locate::position;

/// Parses any 4 hex digits unicode escape sequence.
pub fn unicode_esc_seq(s: Span) -> ParseResult<char> {
    let (s, hex_str) = recognize(count(one_of("1234567890abcdefABCDEF"), 4))(s)?;
    let hex_u32 = u32::from_str_radix(*hex_str, 16).unwrap(); // FIXME
    let char = std::char::from_u32(hex_u32).unwrap(); // FIXME
    Ok((s, char))
}

/// Parses `"\\u"` followed by any 4 hex digits unicode escape sequence.
pub fn identifier_unicode_esc_sequence(s: Span) -> ParseResult<char> {
    preceded(tag("\\u"), unicode_esc_seq)(s)
}

pub fn identifier_start(s: Span) -> ParseResult<char> {
    verify(alt((identifier_unicode_esc_sequence, anychar)), |c: &char| match c {
        c if unicode_xid::UnicodeXID::is_xid_start(*c) => true,
        '$' | '_' => true,
        _ => false,
    })(s)
}

pub fn identifier_continue(s: Span) -> ParseResult<char> {
    verify(alt((identifier_unicode_esc_sequence, anychar)), |c: &char| match c {
        c if unicode_xid::UnicodeXID::is_xid_continue(*c) => true,
        '$' | '_' => true,
        _ => false,
    })(s)
}

/// ```ebnf
/// IdentifierName
/// ```
pub fn parse_identifier_name(s: Span) -> ParseResult<Node> {
    context("identifier", |s| {let (s, start) = position(s)?;
    let (s, name) =
        map(
            pair(identifier_start, many0(identifier_continue)),
            |(c, v)| format!("{}{}", c, v.into_iter().collect::<String>()),
        )(s)?;
    let (s, end) = position(s)?;

    let (s, _) = sp0(s)?;
    Ok((s, NodeKind::Identifier { name }.with_pos(start, end)))})(s)
}

/// ```ebnf
/// Identifier :: IdentifierName but not ReservedWord
/// ```
pub fn parse_identifier(s: Span) -> ParseResult<Node> {
    context("identifier", |s| {let (s, start) = position(s)?;
    let (s, name) = verify(
        map(
            pair(identifier_start, many0(identifier_continue)),
            |(c, v)| format!("{}{}", c, v.into_iter().collect::<String>()),
        ),
        |ident: &str| 
            // make sure identifier is not a reserved word
            not(parse_reserved_word)(ident.into()).is_ok(),
    )(s)?;
    let (s, end) = position(s)?;

    let (s, _) = sp0(s)?;
    Ok((s, NodeKind::Identifier { name }.with_pos(start, end)))})(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        all_consuming(parse_identifier)("myVar".into()).unwrap();
        all_consuming(parse_identifier)("abc123".into()).unwrap();
        all_consuming(parse_identifier)("$".into()).unwrap();
        all_consuming(parse_identifier)("_".into()).unwrap();
        all_consuming(parse_identifier)("$elem".into()).unwrap();
        all_consuming(parse_identifier)("_elem".into()).unwrap();
        all_consuming(parse_identifier)("$123".into()).unwrap();
        all_consuming(parse_identifier)("_123".into()).unwrap();

        // identifiers can start with keywords
        all_consuming(parse_identifier)("var_id".into()).unwrap();
        all_consuming(parse_identifier)("class_id".into()).unwrap();
    }

    #[test]
    fn test_identifier_with_trailing_whitespace() {
        all_consuming(parse_identifier)("abc123 ".into()).unwrap();
        all_consuming(parse_identifier)("abc123 \t\n ".into()).unwrap();
    }

    #[test]
    fn test_bad_identifier() {
        all_consuming(parse_identifier)("123abc".into()).unwrap_err();
    }

    #[test]
    fn test_keyword_not_identifier() {
        all_consuming(parse_identifier)("var".into()).unwrap_err();
        all_consuming(parse_identifier)("class".into()).unwrap_err();
        all_consuming(parse_identifier)("true".into()).unwrap_err();
        all_consuming(parse_identifier)("null".into()).unwrap_err();
    }

    #[test]
    fn test_identifier_with_unicode_escape() {
        assert_eq!(
            parse_identifier(r#"abc\u0061"#.into()).unwrap().1.kind,
            NodeKind::Identifier {
                name: "abca".into()
            }
        );
        assert_eq!(
            parse_identifier(r#"abc\u0061\u0062123"#.into())
                .unwrap()
                .1
                .kind,
            NodeKind::Identifier {
                name: "abcab123".into()
            }
        );
    }
}
