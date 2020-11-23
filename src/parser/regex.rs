//! Parsing for JS regexp literals

use crate::ast::*;
use crate::parser::*;

fn escaped_regex_body(s: Span) -> ParseResult<Span> {
    recognize(preceded(char('\\'), one_of("\\/[]")))(s)
}

fn regex_class(s: Span) -> ParseResult<Span> {
    delimited(char('['), take_until("]"), char(']'))(s)
}

fn regex_char(s: Span) -> ParseResult<Span> {
    recognize(none_of("/"))(s)
}

fn regex_body(s: Span) -> ParseResult<String> {
    fold_many0(
        alt((escaped_regex_body, regex_class, regex_char)),
        format!(""),
        |mut string: String, s| {
            string.push_str(*s);
            string
        },
    )(s)
}

fn regex_flags(s: Span) -> ParseResult<String> {
    fold_many0(
        identifier_continue,
        format!(""),
        |mut flag: String, flag_char| {
            flag.push(flag_char);
            flag
        },
    )(s)
}

pub fn regex_lit(s: Span) -> ParseResult<Node> {
    map(
        spanned(recognize(tuple((
            tag("/"),
            regex_body,
            tag("/"),
            regex_flags,
        )))),
        |(regex_str, start, end)| {
            LiteralValue::RegExp(regex_str.to_string())
                .into_node_kind()
                .with_pos(start, end)
        },
    )(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_json_snapshot;

    #[test]
    fn test_regex_lit() {
        assert_json_snapshot!(parse_literal("/abc/g".into()).unwrap().1);
        assert_json_snapshot!(parse_literal(r#"/abc\\/g"#.into()).unwrap().1);
        assert_json_snapshot!(parse_literal(r#"/\\\//g"#.into()).unwrap().1);
        assert_json_snapshot!(parse_literal(r#"/[^/]*$/"#.into()).unwrap().1);
    }
}
