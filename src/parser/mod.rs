mod expression;
mod identifier;
mod keyword;
mod literal;
mod util;
pub use expression::*;
pub use identifier::*;
pub use keyword::*;
pub use literal::*;
pub use util::*;

use nom::{
    branch::alt, bytes::complete::*, character::complete::*, combinator::*, multi::*, sequence::*,
    IResult,
};
use nom_locate::position;

pub fn parse_program(i: &str) -> IResult<&str, &str> {
    nom::bytes::complete::tag("hello")(i)
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
