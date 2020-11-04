mod expression;
mod identifier;
mod literal;
mod util;
pub use expression::*;
pub use identifier::*;
pub use literal::*;
pub use util::*;

use nom::{
    branch::alt, bytes::complete::*, character::complete::*, combinator::*, multi::*, sequence::*,
    IResult,
};
use nom_locate::position;

#[must_use]
pub fn parse_line_terminator(s: Span) -> IResult<Span, ()> {
    if eof::<Span, ()>(s).is_ok() {
        Ok((s, ()))
    } else {
        let (s, _) = one_of("\n\r\u{2028}\u{2029}")(s)?;
        Ok((s, ()))
    }
}

pub fn is_line_terminator(c: char) -> bool {
    match c {
        '\n' | '\r' | '\u{2028}' | '\u{2029}' => true,
        _ => false,
    }
}

#[must_use]
pub fn parse_line_terminator_sequence(s: Span) -> IResult<Span, ()> {
    fn parse_carriage_return(s: Span) -> IResult<Span, ()> {
        let (s, _) = tag("\r\n")(s)?;
        Ok((s, ()))
    }
    alt((parse_line_terminator, parse_carriage_return))(s)?;
    Ok((s, ()))
}

#[must_use]
fn parse_single_line_comment(s: Span) -> IResult<Span, Span> {
    let (s, _) = tag("//")(s)?;
    let (s, comment) = take_while(|c| !is_line_terminator(c))(s)?;
    let (s, _) = parse_line_terminator(s)?;
    Ok((s, comment))
}

#[must_use]
fn parse_multi_line_comment(s: Span) -> IResult<Span, Span> {
    delimited(tag("/*"), take_until("*/"), tag("*/"))(s)
}

#[must_use]
pub fn parse_comment(s: Span) -> IResult<Span, ()> {
    let (s, _) = alt((parse_single_line_comment, parse_multi_line_comment))(s)?;
    Ok((s, ()))
}

#[must_use]
pub fn parse_program(i: &str) -> IResult<&str, &str> {
    nom::bytes::complete::tag("hello")(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test_comment() {
        all_consuming(parse_comment)("// abc".into()).unwrap();
        all_consuming(parse_comment)("/* abc */".into()).unwrap();
        all_consuming(parse_comment)("/* abc\n123 */".into()).unwrap();
        all_consuming(parse_comment)("/* abc\n123 */ foo".into()).unwrap_err();
    }

    #[test]
    fn test_comment() {
        assert_eq!(
            *parse_comment("// abc\ndef".into()).unwrap().0.fragment(),
            "def"
        );
        assert_eq!(
            *parse_comment("// abc\rdef".into()).unwrap().0.fragment(),
            "def"
        );
        assert_eq!(
            *parse_comment("// abc\u{2028}def".into())
                .unwrap()
                .0
                .fragment(),
            "def"
        );
        assert_eq!(
            *parse_comment("// abc\u{2029}def".into())
                .unwrap()
                .0
                .fragment(),
            "def"
        );
        assert_eq!(
            *parse_comment("// abc\r\ndef".into()).unwrap().0.fragment(),
            "\ndef"
        );
    }
}
