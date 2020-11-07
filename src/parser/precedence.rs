//! Precedence lookup table for various JS operators
//! Operator precedence parsing use Pratt parsing instead of LR because it is not possible with parser combinatorics.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*};

#[derive(Debug, Copy, Clone)]
pub struct BindingPower(pub i32, pub i32);

#[derive(Debug, Copy, Clone)]
pub enum InfixOperator {
    Binary(BinaryOperator),
    Logical(LogicalOperator),
    Assignment(AssignmentOperator),
    /// Should be transformed into a `MemberExpression` with `computed = false`.
    DotOperator,
}
impl From<BinaryOperator> for InfixOperator {
    fn from(op: BinaryOperator) -> Self {
        InfixOperator::Binary(op)
    }
}
impl From<LogicalOperator> for InfixOperator {
    fn from(op: LogicalOperator) -> Self {
        InfixOperator::Logical(op)
    }
}
impl From<AssignmentOperator> for InfixOperator {
    fn from(op: AssignmentOperator) -> Self {
        InfixOperator::Assignment(op)
    }
}

/// Parses a binary (infix) operator.
/// Returns a tuple containing the operator and the operator binding power.
/// Refer to [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence) for details on JS operator precedence.
/// # Binding power
/// The binding power of the operator is based on the Mozilla documentation with some modifications for associativity.  
/// The lowest binding power for an operator is 1. Precedence 0 is to accept any expression.
///
/// The minimum of the left and right binding powers is always an odd number.
/// The maximum of the left and right binding powers is the double of the precedence on the Mozilla documentation page.
/// # Example
/// * Addition - Addition has a precedence of `14` and is left associative. **Binding power**: `(27, 28)`.
/// * Assignment - Assignment has a precedence of `3` and is right associative. **Binding power**: `(6, 5)`.
///
/// Returns `Err` if cannot parse a valid binary operator.
pub fn parse_infix_operator(s: Span) -> ParseResult<(InfixOperator, BindingPower)> {
    ws0(alt((
        // Assignment
        alt((
            value(
                (AssignmentOperator::Equals.into(), BindingPower(6, 5)),
                tag("="),
            ),
            value(
                (AssignmentOperator::PlusEquals.into(), BindingPower(6, 5)),
                tag("+="),
            ),
            value(
                (AssignmentOperator::MinusEquals.into(), BindingPower(6, 5)),
                tag("-="),
            ),
            value(
                (
                    AssignmentOperator::AsteriskEquals.into(),
                    BindingPower(6, 5),
                ),
                tag("*="),
            ),
            value(
                (AssignmentOperator::SlashEquals.into(), BindingPower(6, 5)),
                tag("/="),
            ),
            value(
                (AssignmentOperator::PercentEquals.into(), BindingPower(6, 5)),
                tag("%="),
            ),
            value(
                (
                    AssignmentOperator::ZeroFillLeftShiftEquals.into(),
                    BindingPower(6, 5),
                ),
                tag("<<="),
            ),
            value(
                (
                    AssignmentOperator::SignedRightShiftEquals.into(),
                    BindingPower(6, 5),
                ),
                tag(">>="),
            ),
            value(
                (
                    AssignmentOperator::ZeroFillRightShiftEquals.into(),
                    BindingPower(6, 5),
                ),
                tag(">>>="),
            ),
            value(
                (
                    AssignmentOperator::BitwiseAndEquals.into(),
                    BindingPower(6, 5),
                ),
                tag("&="),
            ),
            value(
                (
                    AssignmentOperator::BitwiseXorEquals.into(),
                    BindingPower(6, 5),
                ),
                tag("^="),
            ),
            value(
                (
                    AssignmentOperator::BitwiseOrEquals.into(),
                    BindingPower(6, 5),
                ),
                tag("|="),
            ),
        )),
        // Logical
        alt((
            value(
                (LogicalOperator::LogicalOr.into(), BindingPower(11, 12)),
                tag("||"),
            ),
            value(
                (LogicalOperator::LogicalAnd.into(), BindingPower(13, 14)),
                tag("&&"),
            ),
        )),
        // Bitwise
        value(
            (BinaryOperator::BitwiseOr.into(), BindingPower(15, 16)),
            tag("|"),
        ),
        value(
            (BinaryOperator::BitwiseXor.into(), BindingPower(17, 18)),
            tag("^"),
        ),
        value(
            (BinaryOperator::BitwiseAnd.into(), BindingPower(18, 20)),
            tag("&"),
        ),
        // Equality
        value(
            (BinaryOperator::EqualsEquals.into(), BindingPower(18, 20)),
            tag("=="),
        ),
        value(
            (BinaryOperator::NotEquals.into(), BindingPower(18, 20)),
            tag("!="),
        ),
        value(
            (BinaryOperator::TripleEquals.into(), BindingPower(18, 20)),
            tag("==="),
        ),
        value(
            (BinaryOperator::TripleNotEquals.into(), BindingPower(18, 20)),
            tag("!=="),
        ),
        // Additive
        value(
            (BinaryOperator::Plus.into(), BindingPower(27, 28)),
            tag("+"),
        ),
        value(
            (BinaryOperator::Minus.into(), BindingPower(27, 28)),
            tag("-"),
        ),
        // Multiplicative
        value(
            (BinaryOperator::Asterisk.into(), BindingPower(29, 30)),
            tag("*"),
        ),
        value(
            (BinaryOperator::Slash.into(), BindingPower(29, 30)),
            tag("/"),
        ),
        value(
            (BinaryOperator::Percent.into(), BindingPower(29, 30)),
            tag("%"),
        ),
        // Member Access
        value((InfixOperator::DotOperator, BindingPower(39, 40)), tag(".")),
        // TODO
    )))(s)
}

#[derive(Debug, Copy, Clone)]
pub enum PrefixOperator {
    Unary(UnaryOperator),
    Update(UpdateOperator),
}
impl From<UnaryOperator> for PrefixOperator {
    fn from(op: UnaryOperator) -> Self {
        PrefixOperator::Unary(op)
    }
}
impl From<UpdateOperator> for PrefixOperator {
    fn from(op: UpdateOperator) -> Self {
        PrefixOperator::Update(op)
    }
}

/// Parses a unary (prefix) operator.
/// Returns a tuple containing the operator and the operator binding power.
/// Refer to [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence) for details on JS operator precedence.
/// # Binding power
/// The binding power of the operator is based on the Mozilla documentation with some modifications for associativity.  
/// The lowest binding power for an operator is 1. Precedence 0 is to accept any expression.
///
/// The minimum of the left and right binding powers is always an odd number or `-1` (left for prefix).
/// The maximum of the left and right binding powers is the double of the precedence on the Mozilla documentation page.
/// # Example
/// * Unary Minus - Unary Minus has a precedence of 17 and is prefix. **Binding power**: `(-1, 34)`.
///
/// Returns `Err` if cannot parse a valid binary operator.
pub fn parse_prefix_operator(s: Span) -> ParseResult<(PrefixOperator, BindingPower)> {
    ws0(alt((
        // Update
        // Note: Update is matched first to prevent "+" and "-" to be matched first.
        value(
            (UpdateOperator::Increment.into(), BindingPower(-1, 34)),
            tag("++"),
        ),
        value(
            (UpdateOperator::Decrement.into(), BindingPower(-1, 34)),
            tag("--"),
        ),
        // Unary
        value(
            (UnaryOperator::Minus.into(), BindingPower(-1, 34)),
            tag("-"),
        ),
        value((UnaryOperator::Plus.into(), BindingPower(-1, 34)), tag("+")),
        value(
            (UnaryOperator::LogicalNot.into(), BindingPower(-1, 34)),
            tag("!"),
        ),
        value(
            (UnaryOperator::BitwiseNot.into(), BindingPower(-1, 34)),
            tag("~"),
        ),
        value(
            (UnaryOperator::Typeof.into(), BindingPower(-1, 34)),
            keyword_typeof,
        ),
        value(
            (UnaryOperator::Void.into(), BindingPower(-1, 34)),
            keyword_void,
        ),
        value(
            (UnaryOperator::Delete.into(), BindingPower(-1, 34)),
            keyword_delete,
        ),
    )))(s)
}
