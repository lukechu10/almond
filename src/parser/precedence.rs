//! Precedence lookup table for various JS operators
//! Operator precedence parsing use Pratt parsing instead of LR because it is not possible with parser combinatorics.

use crate::ast::*;
use crate::parser::util::*;
use crate::parser::*;
use nom::{branch::alt, bytes::complete::*, combinator::*};
use nom_locate::position;

#[derive(Debug, Copy, Clone)]
pub struct BindingPower(pub i32, pub i32);

#[derive(Debug, Copy, Clone)]
pub enum InfixOperator {
    Binary(BinaryOperator),
    Logical(LogicalOperator),
    Assignment(AssignmentOperator),
    /// Should be transformed into a `MemberExpression` with `computed = false`.
    DotOperator,
    /// Should be transformed into a `SequenceExpression`.
    SequenceOperator,
    /// Eats `?`.
    TernaryOperator,
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
        // Comma / Sequence
        value(
            (InfixOperator::SequenceOperator, BindingPower(0, 1)),
            tag(","),
        ),
        // Ternary operator
        value(
            (InfixOperator::TernaryOperator, BindingPower(7, 8)),
            tag("?"),
        ),
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
        // Equality
        alt((
            // Note: Triple equals and triple not equals are before equals equals and not equals to prevent matching wrong operator.
            value(
                (BinaryOperator::TripleEquals.into(), BindingPower(18, 20)),
                tag("==="),
            ),
            value(
                (BinaryOperator::TripleNotEquals.into(), BindingPower(18, 20)),
                tag("!=="),
            ),
            value(
                (BinaryOperator::EqualsEquals.into(), BindingPower(18, 20)),
                tag("=="),
            ),
            value(
                (BinaryOperator::NotEquals.into(), BindingPower(18, 20)),
                tag("!="),
            ),
        )),
        // Assignment
        // Note: Assignment are after equality to prevent matching `==` as `('=', '=').
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
        // Bitwise
        alt((
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
        )),
        // Bitwise shift
        alt((
            value(
                (
                    BinaryOperator::ZeroFillLeftShift.into(),
                    BindingPower(25, 26),
                ),
                tag("<<"),
            ),
            value(
                (
                    BinaryOperator::ZeroFillRightShift.into(),
                    BindingPower(25, 26),
                ),
                tag(">>>"),
            ),
            value(
                (
                    BinaryOperator::SignedRightShift.into(),
                    BindingPower(25, 26),
                ),
                tag(">>"),
            ),
        )),
        // Relational
        alt((
            value(
                (BinaryOperator::Instanceof.into(), BindingPower(23, 24)),
                keyword_instanceof,
            ),
            value(
                (BinaryOperator::In.into(), BindingPower(23, 24)),
                keyword_in,
            ),
            value(
                (BinaryOperator::LessThanEquals.into(), BindingPower(23, 24)),
                tag("<="),
            ),
            value(
                (BinaryOperator::LessThan.into(), BindingPower(23, 24)),
                tag("<"),
            ),
            value(
                (
                    BinaryOperator::GreaterThanEquals.into(),
                    BindingPower(23, 24),
                ),
                tag(">="),
            ),
            value(
                (BinaryOperator::GreaterThan.into(), BindingPower(23, 24)),
                tag(">"),
            ),
        )),
        // Additive
        alt((
            value(
                (BinaryOperator::Plus.into(), BindingPower(27, 28)),
                tag("+"),
            ),
            value(
                (BinaryOperator::Minus.into(), BindingPower(27, 28)),
                tag("-"),
            ),
        )),
        // Multiplicative
        alt((
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
        )),
        // Member Access
        // Mozilla docs specify precedence of 20 so binding power of 39 but callee identifier should bind to `new` instead of argument list.
        value((InfixOperator::DotOperator, BindingPower(41, 42)), tag(".")),
        // TODO
    )))(s)
}

#[derive(Debug, Copy, Clone)]
pub enum PrefixOperator {
    Unary(UnaryOperator),
    Update(UpdateOperator),
    /// Eats `new` (does not eat identifier or argument list).
    New,
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
/// * Unary Minus - Unary Minus has a precedence of 17 and is prefix. **Binding power**: `(-1, 33)`.
///
/// Returns `Err` if cannot parse a valid binary operator.
pub fn parse_prefix_operator(s: Span) -> ParseResult<(PrefixOperator, BindingPower)> {
    ws0(alt((
        // Update
        // Note: Update is matched first to prevent "+" and "-" to be matched first.
        value(
            (UpdateOperator::Increment.into(), BindingPower(-1, 33)),
            tag("++"),
        ),
        value(
            (UpdateOperator::Decrement.into(), BindingPower(-1, 33)),
            tag("--"),
        ),
        // Unary
        value(
            (UnaryOperator::Minus.into(), BindingPower(-1, 33)),
            tag("-"),
        ),
        value((UnaryOperator::Plus.into(), BindingPower(-1, 33)), tag("+")),
        value(
            (UnaryOperator::LogicalNot.into(), BindingPower(-1, 33)),
            tag("!"),
        ),
        value(
            (UnaryOperator::BitwiseNot.into(), BindingPower(-1, 33)),
            tag("~"),
        ),
        value(
            (UnaryOperator::Typeof.into(), BindingPower(-1, 33)),
            keyword_typeof,
        ),
        value(
            (UnaryOperator::Void.into(), BindingPower(-1, 33)),
            keyword_void,
        ),
        value(
            (UnaryOperator::Delete.into(), BindingPower(-1, 33)),
            keyword_delete,
        ),
        // Mozilla docs specify precedence of 20 so binding power of 39 but callee identifier should bind to `new` instead of argument list.
        value((PrefixOperator::New, BindingPower(-1, 41)), keyword_new),
    )))(s)
}

#[derive(Debug, Copy, Clone)]
pub enum PostfixOperator {
    Update(UpdateOperator),
    /// Eats `[` (does not eat `]`).
    ComputedMember,
    /// Eats `(` (does not eat `)`).
    FuncCall,
}
impl From<UpdateOperator> for PostfixOperator {
    fn from(op: UpdateOperator) -> Self {
        PostfixOperator::Update(op)
    }
}

/// Parses a unary (postfix) operator.
/// Returns a tuple containing the operator and the operator binding power.
/// Refer to [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence) for details on JS operator precedence.
///
/// **Note**: Unlike other operator parsers, this parser also returns an additional `end` field for diagnostics to make it easier to find end of expression without trailing whitespace.
/// # Binding power
/// The binding power of the operator is based on the Mozilla documentation with some modifications for associativity.  
/// The lowest binding power for an operator is 1. Precedence 0 is to accept any expression.
///
/// The minimum of the left and right binding powers is always an odd number or `-1` (left for prefix).
/// The maximum of the left and right binding powers is the double of the precedence on the Mozilla documentation page.
/// # Example
/// * Postfix Increment - Postfix Increment has a precedence of 18 and is postfix. **Binding power**: `(35, -1)`.
///
/// Returns `Err` if cannot parse a valid binary operator.
pub fn parse_postfix_operator(s: Span) -> ParseResult<(PostfixOperator, BindingPower, Span)> {
    let (s, (postfix_op, bp)) = alt((
        value(
            (UpdateOperator::Increment.into(), BindingPower(35, -1)),
            tag("++"),
        ),
        value(
            (UpdateOperator::Decrement.into(), BindingPower(35, -1)),
            tag("--"),
        ),
        value(
            (PostfixOperator::ComputedMember, BindingPower(39, -1)),
            tag("["),
        ),
        value((PostfixOperator::FuncCall, BindingPower(39, -1)), tag("(")),
    ))(s)?;
    let (s, end) = position(s)?;
    let (s, _) = sp0(s)?;

    Ok((s, (postfix_op, bp, end)))
}
