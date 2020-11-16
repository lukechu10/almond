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

pub(crate) use nom::{
    branch::alt, bytes::complete::*, character::complete::*, combinator::*,
    multi::*, number::complete::*, sequence::*,
};
