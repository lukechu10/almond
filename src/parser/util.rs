//! Utilities

use crate::parser::*;
use nom::{
    branch::*,
    bytes::complete::*,
    combinator::*,
    error::{Error, ParseError},
    IResult, Parser,
};

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

pub fn space(s: Span) -> ParseResult<()> {
    alt((whitespace, line_terminator, comment))(s)
}

pub fn spaces0(s: Span) -> IResult<Span, ()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many0(space))(s)
}

pub fn spaces1(s: Span) -> IResult<Span, ()> {
    if eof::<Span, ()>(s).is_ok() {
        return Ok((s, ()));
    }
    value((), many1(space))(s)
}
