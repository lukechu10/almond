//! Utilities

use crate::parser::*;
use nom::{branch::*, bytes::complete::*, combinator::*, IResult, Parser};

pub type Span<'a> = nom_locate::LocatedSpan<&'a str>;
pub type ParseResult<'a, T> = IResult<Span<'a>, T>;

fn is_whitespace(c: &char) -> bool {
    match c {
        '\t'
        // vertical tab
        | '\x0b'
        // form feed
        | '\x0c'
        | ' '
        // no break space
        | '\u{00a0}'
        // byte order mark
        | '\u{feff}'
        // unicode space seperators
        | '\u{2000}'..='\u{200a}' | '\u{3000}' => true,
        _ => false
    }
}

/// Parses one character if it is whitespace (not including newline and comments).
pub fn whitespace(s: Span) -> ParseResult<()> {
    value((), verify(anychar, is_whitespace))(s)
}

/// # Grammar
/// ```ebnf
/// lineTerminator = "\n" | "\r" | "\u2028" | "\u2029"
/// ```
pub fn line_terminator(s: Span) -> IResult<Span, ()> {
    let (s, _) = one_of("\n\r\u{2028}\u{2029}")(s)?;
    Ok((s, ()))
}

pub fn is_line_terminator(c: char) -> bool {
    match c {
        '\n' | '\r' | '\u{2028}' | '\u{2029}' => true,
        _ => false,
    }
}

/// # Grammar
/// ```ebnf
/// lineTerminatorSequence = "\n" | "\r" ~"\n" | "\u2028" | "\u2029" | "\r\n"
/// ```
pub fn line_terminator_sequence(s: Span) -> IResult<Span, ()> {
    fn parse_carriage_return(s: Span) -> IResult<Span, ()> {
        let (s, _) = tag("\r\n")(s)?;
        Ok((s, ()))
    }
    alt((parse_carriage_return, line_terminator))(s)?;
    Ok((s, ()))
}

fn single_line_comment(s: Span) -> IResult<Span, Span> {
    let (s, _) = tag("//")(s)?;
    let (s, comment) = take_while(|c| !is_line_terminator(c))(s)?;
    let (s, _) = alt((value((), eof), line_terminator))(s)?;
    Ok((s, comment))
}

fn multi_line_comment(s: Span) -> IResult<Span, Span> {
    delimited(tag("/*"), take_until("*/"), tag("*/"))(s)
}

pub fn comment(s: Span) -> IResult<Span, ()> {
    value((), alt((single_line_comment, multi_line_comment)))(s)
}

pub fn sp(s: Span) -> ParseResult<()> {
    alt((whitespace, line_terminator, comment))(s)
}

pub fn sp0(s: Span) -> IResult<Span, ()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many0(sp))(s)
}

pub fn sp1(s: Span) -> IResult<Span, ()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many1(sp))(s)
}

/// Alias for `terminated(f, sp0)`.
pub fn ws0<'a, O1, F>(mut f: F) -> impl FnMut(Span<'a>) -> ParseResult<O1>
where
    F: Parser<Span<'a>, O1, nom::error::Error<Span<'a>>>,
{
    move |input: Span<'a>| {
        let (input, o1) = f.parse(input)?;
        sp0.parse(input).map(|(i, _)| (i, o1))
    }
}

/// Alias for `terminated(f, sp1)`.
pub fn ws1<'a, O1, F>(mut f: F) -> impl FnMut(Span<'a>) -> ParseResult<O1>
where
    F: Parser<Span<'a>, O1, nom::error::Error<Span<'a>>>,
{
    move |input: Span<'a>| {
        let (input, o1) = f.parse(input)?;
        sp1.parse(input).map(|(i, _)| (i, o1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test_comment() {
        all_consuming(comment)("// abc".into()).unwrap();
        all_consuming(comment)("/* abc */".into()).unwrap();
        all_consuming(comment)("/* abc\n123 */".into()).unwrap();
        all_consuming(comment)("/* abc\n123 */ foo".into()).unwrap_err();
    }

    #[test]
    fn test_comment() {
        assert_eq!(*comment("// abc\ndef".into()).unwrap().0.fragment(), "def");
        assert_eq!(*comment("// abc\rdef".into()).unwrap().0.fragment(), "def");
        assert_eq!(
            *comment("// abc\u{2028}def".into()).unwrap().0.fragment(),
            "def"
        );
        assert_eq!(
            *comment("// abc\u{2029}def".into()).unwrap().0.fragment(),
            "def"
        );
        assert_eq!(
            *comment("// abc\r\ndef".into()).unwrap().0.fragment(),
            "\ndef"
        );
    }
}
