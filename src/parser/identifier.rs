use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{
    branch::alt, bytes::complete::*, character::complete::*, character::*, combinator::*,
    number::complete::*, IResult,
};
use nom_locate::position;

/// Parses any 4 hex digits unicode escape sequence
fn parse_unicode_escape_sequence(s: Span) -> IResult<Span, char> {
    let (s, _) = tag("\\u")(s)?;
    let (s, hex_str) = recognize(count(one_of("1234567890abcdefABCDEF"), 4))(s)?;
    let hex_u32 = u32::from_str_radix(*hex_str, 16).unwrap(); // FIXME
    let char = std::char::from_u32(hex_u32).unwrap(); // FIXME
    Ok((s, char))
}

pub fn parse_identifier(s: Span) -> IResult<Span, Node> {
    fn parse_identifier_start(s: Span) -> IResult<Span, ()> {
        let (s, _) = verify(
            alt((parse_unicode_escape_sequence, none_of(""))),
            |c: &char| match c {
                c if unicode_xid::UnicodeXID::is_xid_start(*c) => true,
                '$' | '_' => true,
                _ => false,
            },
        )(s)?;
        Ok((s, ()))
    }
    fn parse_identifier_continue(s: Span) -> IResult<Span, ()> {
        let (s, _) = verify(
            alt((parse_unicode_escape_sequence, none_of(""))),
            |c: &char| match c {
                c if unicode_xid::UnicodeXID::is_xid_continue(*c) => true,
                '$' | '_' => true,
                _ => false,
            },
        )(s)?;
        Ok((s, ()))
    }

    let (s, start) = position(s)?;
    let (s, name) = recognize(pair(
        parse_identifier_start,
        many0(parse_identifier_continue),
    ))(s)?;
    let (s, end) = position(s)?;

    Ok((
        s,
        NodeKind::Identifier {
            name: name.fragment().to_string(),
        }
        .with_pos(start, end),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        all_consuming(parse_identifier)("myVar".into()).unwrap();
        all_consuming(parse_identifier)("abc123".into()).unwrap();
        all_consuming(parse_identifier)("$".into()).unwrap();
        all_consuming(parse_identifier)("$elem".into()).unwrap();
        all_consuming(parse_identifier)("$123".into()).unwrap();

        all_consuming(parse_identifier)(r#"abc\u0061"#.into()).unwrap();

        all_consuming(parse_identifier)("123abc".into()).unwrap_err();
    }
}
