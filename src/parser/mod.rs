//! Parsing for JS
//! # Whitespace Handling
//! All functions named `parse_*` should handle leading whitespace. Preceding whitespace is only handled in top level parse function.

mod expression;
mod functions;
mod identifier;
mod keyword;
mod literal;
mod precedence;
mod statement;
mod util;
pub use expression::*;
pub use functions::*;
pub use identifier::*;
pub use keyword::*;
pub use literal::*;
pub use precedence::*;
pub use statement::*;
pub(crate) use util::*;

use nom::{character::complete::*, multi::*, sequence::*, IResult};

pub fn parse_program(i: &str) -> IResult<&str, &str> {
    nom::bytes::complete::tag("hello")(i)
}
