//! Parsing for JS keywords

use crate::parser::util::*;
use crate::parser::*;

/// Succeeds if parsed a reserved word. Should be used with `not` to check if an identifier is not a reserved word.
pub fn parse_reserved_word(s: Span) -> ParseResult<()> {
    alt((
        parse_keyword,
        parse_future_reserved_word,
        value((), ws0(null_lit)),
        value((), ws0(bool_lit)),
    ))(s)
}

/// Succeeds if parsed is a keyword. Use `reserved_word` with `not` instead to check if an identifier is not a reserved word.
pub fn parse_keyword(s: Span) -> ParseResult<()> {
    ws0(alt((
        keyword_break,
        keyword_do,
        keyword_instanceof,
        keyword_typeof,
        keyword_case,
        keyword_else,
        keyword_new,
        keyword_var,
        keyword_catch,
        keyword_finally,
        keyword_return,
        keyword_void,
        keyword_continue,
        keyword_for,
        keyword_switch,
        keyword_while,
        keyword_debugger,
        keyword_function,
        keyword_this,
        keyword_with,
        alt((
            keyword_default,
            keyword_if,
            keyword_throw,
            keyword_delete,
            keyword_in,
            keyword_try,
        )),
    )))(s)
}

pub fn parse_future_reserved_word_lax(s: Span) -> ParseResult<()> {
    ws0(alt((
        keyword_class,
        keyword_enum,
        keyword_extends,
        keyword_super,
        keyword_const,
        keyword_export,
        keyword_import,
    )))(s)
}

pub fn parse_future_reserved_word_strict(s: Span) -> ParseResult<()> {
    ws0(alt((
        parse_future_reserved_word_lax,
        keyword_implements,
        keyword_let,
        keyword_private,
        keyword_public,
        keyword_interface,
        keyword_package,
        keyword_protected,
        keyword_static,
        keyword_yield,
    )))(s)
}

pub fn parse_future_reserved_word(s: Span) -> ParseResult<()> {
    parse_future_reserved_word_strict(s)
}

pub fn keyword_break(s: Span) -> ParseResult<()> {
    value((), pair(tag("break"), not(identifier_continue)))(s)
}
pub fn keyword_do(s: Span) -> ParseResult<()> {
    value((), pair(tag("do"), not(identifier_continue)))(s)
}
pub fn keyword_instanceof(s: Span) -> ParseResult<()> {
    value((), pair(tag("instanceof"), not(identifier_continue)))(s)
}
pub fn keyword_typeof(s: Span) -> ParseResult<()> {
    value((), pair(tag("typeof"), not(identifier_continue)))(s)
}
pub fn keyword_case(s: Span) -> ParseResult<()> {
    value((), pair(tag("case"), not(identifier_continue)))(s)
}
pub fn keyword_else(s: Span) -> ParseResult<()> {
    value((), pair(tag("else"), not(identifier_continue)))(s)
}
pub fn keyword_new(s: Span) -> ParseResult<()> {
    value((), pair(tag("new"), not(identifier_continue)))(s)
}
pub fn keyword_var(s: Span) -> ParseResult<()> {
    value((), pair(tag("var"), not(identifier_continue)))(s)
}
pub fn keyword_catch(s: Span) -> ParseResult<()> {
    value((), pair(tag("catch"), not(identifier_continue)))(s)
}
pub fn keyword_finally(s: Span) -> ParseResult<()> {
    value((), pair(tag("finally"), not(identifier_continue)))(s)
}
pub fn keyword_return(s: Span) -> ParseResult<()> {
    value((), pair(tag("return"), not(identifier_continue)))(s)
}
pub fn keyword_void(s: Span) -> ParseResult<()> {
    value((), pair(tag("void"), not(identifier_continue)))(s)
}
pub fn keyword_continue(s: Span) -> ParseResult<()> {
    value((), pair(tag("continue"), not(identifier_continue)))(s)
}
pub fn keyword_for(s: Span) -> ParseResult<()> {
    value((), pair(tag("for"), not(identifier_continue)))(s)
}
pub fn keyword_switch(s: Span) -> ParseResult<()> {
    value((), pair(tag("switch"), not(identifier_continue)))(s)
}
pub fn keyword_while(s: Span) -> ParseResult<()> {
    value((), pair(tag("while"), not(identifier_continue)))(s)
}
pub fn keyword_debugger(s: Span) -> ParseResult<()> {
    value((), pair(tag("debugger"), not(identifier_continue)))(s)
}
pub fn keyword_function(s: Span) -> ParseResult<()> {
    value((), pair(tag("function"), not(identifier_continue)))(s)
}
pub fn keyword_this(s: Span) -> ParseResult<()> {
    value((), pair(tag("this"), not(identifier_continue)))(s)
}
pub fn keyword_with(s: Span) -> ParseResult<()> {
    value((), pair(tag("with"), not(identifier_continue)))(s)
}
pub fn keyword_default(s: Span) -> ParseResult<()> {
    value((), pair(tag("default"), not(identifier_continue)))(s)
}
pub fn keyword_if(s: Span) -> ParseResult<()> {
    value((), pair(tag("if"), not(identifier_continue)))(s)
}
pub fn keyword_throw(s: Span) -> ParseResult<()> {
    value((), pair(tag("throw"), not(identifier_continue)))(s)
}
pub fn keyword_delete(s: Span) -> ParseResult<()> {
    value((), pair(tag("delete"), not(identifier_continue)))(s)
}
pub fn keyword_in(s: Span) -> ParseResult<()> {
    value((), pair(tag("in"), not(identifier_continue)))(s)
}
pub fn keyword_try(s: Span) -> ParseResult<()> {
    value((), pair(tag("try"), not(identifier_continue)))(s)
}
pub fn keyword_get(s: Span) -> ParseResult<()> {
    value((), pair(tag("get"), not(identifier_continue)))(s)
}
pub fn keyword_set(s: Span) -> ParseResult<()> {
    value((), pair(tag("set"), not(identifier_continue)))(s)
}
pub fn keyword_class(s: Span) -> ParseResult<()> {
    value((), pair(tag("class"), not(identifier_continue)))(s)
}
pub fn keyword_enum(s: Span) -> ParseResult<()> {
    value((), pair(tag("enum"), not(identifier_continue)))(s)
}
pub fn keyword_extends(s: Span) -> ParseResult<()> {
    value((), pair(tag("extends"), not(identifier_continue)))(s)
}
pub fn keyword_super(s: Span) -> ParseResult<()> {
    value((), pair(tag("super"), not(identifier_continue)))(s)
}
pub fn keyword_const(s: Span) -> ParseResult<()> {
    value((), pair(tag("const"), not(identifier_continue)))(s)
}
pub fn keyword_export(s: Span) -> ParseResult<()> {
    value((), pair(tag("export"), not(identifier_continue)))(s)
}
pub fn keyword_import(s: Span) -> ParseResult<()> {
    value((), pair(tag("import"), not(identifier_continue)))(s)
}
pub fn keyword_implements(s: Span) -> ParseResult<()> {
    value((), pair(tag("implements"), not(identifier_continue)))(s)
}
pub fn keyword_let(s: Span) -> ParseResult<()> {
    value((), pair(tag("let"), not(identifier_continue)))(s)
}
pub fn keyword_private(s: Span) -> ParseResult<()> {
    value((), pair(tag("private"), not(identifier_continue)))(s)
}
pub fn keyword_public(s: Span) -> ParseResult<()> {
    value((), pair(tag("public"), not(identifier_continue)))(s)
}
pub fn keyword_interface(s: Span) -> ParseResult<()> {
    value((), pair(tag("interface"), not(identifier_continue)))(s)
}
pub fn keyword_package(s: Span) -> ParseResult<()> {
    value((), pair(tag("package"), not(identifier_continue)))(s)
}
pub fn keyword_protected(s: Span) -> ParseResult<()> {
    value((), pair(tag("protected"), not(identifier_continue)))(s)
}
pub fn keyword_static(s: Span) -> ParseResult<()> {
    value((), pair(tag("static"), not(identifier_continue)))(s)
}
pub fn keyword_yield(s: Span) -> ParseResult<()> {
    value((), pair(tag("yield"), not(identifier_continue)))(s)
}
