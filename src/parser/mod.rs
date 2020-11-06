//! Parsing for JS
//! # Whitespace Handling
//! All parse functions should handle leading whitespace. Preceding whitespace is only handled in top level parse function.

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

use nom::{character::complete::*, multi::*, sequence::*, IResult};

pub fn parse_program(i: &str) -> IResult<&str, &str> {
    nom::bytes::complete::tag("hello")(i)
}
