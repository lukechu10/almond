//! Utilities.

use crate::parser::*;
use nom::{IResult, Offset, Parser, Slice};
use nom_locate::position;

pub type Span<'a> = nom_locate::LocatedSpan<&'a str>;
pub type ParseResult<'a, T> = IResult<Span<'a>, T>;

fn is_whitespace(c: &char) -> bool {
    matches!(
        c,
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
        | '\u{2000}'..='\u{200a}' | '\u{3000}'
    )
}

/// Parses one character if it is whitespace (not including newline and comments).
pub fn whitespace(s: Span) -> ParseResult<()> {
    value((), verify(anychar, is_whitespace))(s)
}

/// # Grammar
/// ```ebnf
/// lineTerminator = "\n" | "\r" | "\u2028" | "\u2029"
/// ```
pub fn line_terminator(s: Span) -> ParseResult<()> {
    let (s, _) = one_of("\n\r\u{2028}\u{2029}")(s)?;
    Ok((s, ()))
}

pub fn is_line_terminator(c: char) -> bool {
    matches!(c, '\n' | '\r' | '\u{2028}' | '\u{2029}')
}

/// # Grammar
/// ```ebnf
/// lineTerminatorSequence = "\n" | "\r" ~"\n" | "\u2028" | "\u2029" | "\r\n"
/// ```
pub fn line_terminator_sequence(s: Span) -> ParseResult<()> {
    fn parse_carriage_return(s: Span) -> ParseResult<()> {
        let (s, _) = tag("\r\n")(s)?;
        Ok((s, ()))
    }
    alt((parse_carriage_return, line_terminator))(s)?;
    Ok((s, ()))
}

fn single_line_comment(s: Span) -> ParseResult<Span> {
    let (s, _) = tag("//")(s)?;
    let (s, comment) = take_while(|c| !is_line_terminator(c))(s)?;
    let (s, _) = alt((value((), eof), line_terminator))(s)?;
    Ok((s, comment))
}

fn multi_line_comment(s: Span) -> ParseResult<Span> {
    delimited(tag("/*"), take_until("*/"), tag("*/"))(s)
}

fn multi_line_comment_no_nl(s: Span) -> ParseResult<Span> {
    verify(
        delimited(tag("/*"), take_until("*/"), tag("*/")),
        |s: &Span| !s.contains(is_line_terminator),
    )(s)
}

pub fn comment(s: Span) -> ParseResult<()> {
    value((), alt((single_line_comment, multi_line_comment)))(s)
}

pub fn sp(s: Span) -> ParseResult<()> {
    alt((whitespace, line_terminator, comment))(s)
}

pub fn sp_no_nl(s: Span) -> ParseResult<()> {
    alt((whitespace, value((), multi_line_comment_no_nl)))(s)
}

pub fn sp0(s: Span) -> ParseResult<()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many0(sp))(s)
}

pub fn sp1(s: Span) -> ParseResult<()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many1(sp))(s)
}

pub fn sp_no_nl0(s: Span) -> ParseResult<()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many0(sp_no_nl))(s)
}

pub fn sp_no_nl1(s: Span) -> ParseResult<()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many1(sp_no_nl))(s)
}

/// Alias for `terminated(f, sp0)`.
pub fn ws0<'a, O1, F>(mut f: F) -> impl FnMut(Span<'a>) -> ParseResult<O1>
where
    F: Parser<Span<'a>, O1, nom::error::Error<Span<'a>>>,
{
    move |s: Span<'a>| {
        let (s, o1) = f.parse(s)?;
        sp0.parse(s).map(|(i, _)| (i, o1))
    }
}

/// Alias for `terminated(f, sp1)`.
pub fn ws1<'a, O1, F>(mut f: F) -> impl FnMut(Span<'a>) -> ParseResult<O1>
where
    F: Parser<Span<'a>, O1, nom::error::Error<Span<'a>>>,
{
    move |s: Span<'a>| {
        let (s, o1) = f.parse(s)?;
        sp1.parse(s).map(|(i, _)| (i, o1))
    }
}

/// Alias for `terminated(f, sp_no_nl0)`.
pub fn ws_no_nl0<'a, O1, F>(mut f: F) -> impl FnMut(Span<'a>) -> ParseResult<O1>
where
    F: Parser<Span<'a>, O1, nom::error::Error<Span<'a>>>,
{
    move |s: Span<'a>| {
        let (s, o1) = f.parse(s)?;
        sp_no_nl0.parse(s).map(|(i, _)| (i, o1))
    }
}

/// Alias for `terminated(f, sp_no_nl1)`.
pub fn ws_no_nl1<'a, O1, F>(mut f: F) -> impl FnMut(Span<'a>) -> ParseResult<O1>
where
    F: Parser<Span<'a>, O1, nom::error::Error<Span<'a>>>,
{
    move |s: Span<'a>| {
        let (s, o1) = f.parse(s)?;
        sp_no_nl1.parse(s).map(|(i, _)| (i, o1))
    }
}

/// Applies a parser and records start and end position. Ignores whitespace by trimming the end of the matched str.
pub fn spanned<'a, O1, F>(mut f: F) -> impl FnMut(Span<'a>) -> ParseResult<(O1, Span, Span)>
where
    F: Parser<Span<'a>, O1, nom::error::Error<Span<'a>>>,
{
    move |s: Span<'a>| {
        let (matched_s, o1) = f.parse(s)?;
        let index = s.offset(&matched_s);
        let slice = s.slice(..index).trim_end();
        let (_, end) = preceded(take(slice.len()), position)(s)?;

        Ok((
            matched_s,
            (
                o1,
                /* start */ position(s)?.1,
                /* end */ position(end)?.1,
            ),
        ))
    }
}

/// A semicolon is "automatically inserted" if a newline or the end of the input stream is reached, or the offending token is `"}"`.
/// See https://es5.github.io/#x7.9 for more information.
pub fn semi(s: Span) -> ParseResult<()> {
    value((), tag(";"))(s)
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
