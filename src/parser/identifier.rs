use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, character::complete::*, combinator::*, IResult};
use nom_locate::position;

#[must_use]
pub fn parse_identifier(s: Span) -> IResult<Span, Node> {
    fn is_identifier_start(c: char) -> bool {
        match c {
            c if c.is_alphabetic() => true,
            '$' | '_' => true,
            // TODO: if c codepoint is above 128, check c is non ascii identifier start
            // TODO: check if c is in astral_set
            _ => false,
        }
    }
    fn is_identifier_char(c: char) -> bool {
        match c {
            c if c.is_alphanumeric() => true,
            '$' | '_' => true,
            // TODO: if c codepoint is above 128, check c is non ascii identifier start
            // TODO: check if c is in astral_set
            _ => false,
        }
    }

    let (s, start) = position(s)?;

    let (s, first) = satisfy(is_identifier_start)(s)?;
    let (s, rest) = take_while(is_identifier_char)(s)?;

    let (s, end) = position(s)?;

    Ok((
        s,
        NodeKind::Identifier {
            name: format!("{}{}", first, rest),
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

        all_consuming(parse_identifier)("123abc".into()).unwrap_err();
    }
}
