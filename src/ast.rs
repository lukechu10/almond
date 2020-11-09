//! AST nodes for JS

use serde::{Serialize, Serializer};

/// A literal value.
#[derive(Debug, PartialEq, Clone, Serialize)]
#[serde(untagged)]
pub enum LiteralValue {
    String(String),
    Boolean(bool),
    Null,
    Number(f64),
    RegExp(String),
}

impl LiteralValue {
    pub fn into_node_kind<'a>(self) -> NodeKind<'a> {
        self.into()
    }
}

impl<'a> From<LiteralValue> for NodeKind<'a> {
    fn from(value: LiteralValue) -> Self {
        NodeKind::Literal { value }
    }
}

/// A function declaration or expression.
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Function<'a> {
    /// `type: Identifier | null`
    pub id: Option<Box<Node<'a>>>,
    /// `type: [ Pattern ]`
    pub params: Vec<Node<'a>>,
    /// `type: FunctionBody`
    pub body: Box<Node<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableDeclarationKind {
    Var,
}
impl Serialize for VariableDeclarationKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = match self {
            VariableDeclarationKind::Var => "var",
        };

        serializer.serialize_str(s)
    }
}

/// Ordinary property initializers have a kind value `"init"`; getters and setters have the kind values `"get"` and `"set"`, respectively.
#[derive(Debug, PartialEq, Clone)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}
impl Serialize for PropertyKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = match self {
            PropertyKind::Init => "init",
            PropertyKind::Get => "get",
            PropertyKind::Set => "set",
        };

        serializer.serialize_str(s)
    }
}

/// An unary operator token.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOperator {
    Minus,
    Plus,
    LogicalNot,
    BitwiseNot,
    Typeof,
    Void,
    Delete,
}
impl Serialize for UnaryOperator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = match self {
            UnaryOperator::Minus => "-",
            UnaryOperator::Plus => "+",
            UnaryOperator::LogicalNot => "!",
            UnaryOperator::BitwiseNot => "~",
            UnaryOperator::Typeof => "typeof",
            UnaryOperator::Void => "void",
            UnaryOperator::Delete => "delete",
        };

        serializer.serialize_str(s)
    }
}

/// An update (increment or decrement) operator token.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UpdateOperator {
    Increment,
    Decrement,
}
impl Serialize for UpdateOperator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = match self {
            UpdateOperator::Increment => "++",
            UpdateOperator::Decrement => "--",
        };

        serializer.serialize_str(s)
    }
}

/// A binary operator token.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    EqualsEquals,
    NotEquals,
    TripleEquals,
    TripleNotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    /// `<<`
    ZeroFillLeftShift,
    /// `>>`
    SignedRightShift,
    /// `>>>`
    ZeroFillRightShift,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    In,
    Instanceof,
}
impl Serialize for BinaryOperator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = match self {
            BinaryOperator::EqualsEquals => "==",
            BinaryOperator::NotEquals => "!=",
            BinaryOperator::TripleEquals => "===",
            BinaryOperator::TripleNotEquals => "!==",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEquals => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEquals => ">=",
            BinaryOperator::ZeroFillLeftShift => "<<",
            BinaryOperator::SignedRightShift => ">>",
            BinaryOperator::ZeroFillRightShift => ">>>",
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Asterisk => "*",
            BinaryOperator::Slash => "/",
            BinaryOperator::Percent => "%",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::In => "in",
            BinaryOperator::Instanceof => "instanceof",
        };

        serializer.serialize_str(s)
    }
}

/// An assignment operator token.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum AssignmentOperator {
    Equals,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    SlashEquals,
    PercentEquals,
    /// `<<=`
    ZeroFillLeftShiftEquals,
    /// `>>=`
    SignedRightShiftEquals,
    /// `>>>=`
    ZeroFillRightShiftEquals,
    /// `|=`
    BitwiseOrEquals,
    /// `^=`
    BitwiseXorEquals,
    /// `&=`
    BitwiseAndEquals,
}
impl Serialize for AssignmentOperator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = match self {
            AssignmentOperator::Equals => "=",
            AssignmentOperator::PlusEquals => "+=",
            AssignmentOperator::MinusEquals => "-=",
            AssignmentOperator::AsteriskEquals => "*=",
            AssignmentOperator::SlashEquals => "/=",
            AssignmentOperator::PercentEquals => "%=",
            AssignmentOperator::ZeroFillLeftShiftEquals => "<<=",
            AssignmentOperator::SignedRightShiftEquals => ">>=",
            AssignmentOperator::ZeroFillRightShiftEquals => ">>>=",
            AssignmentOperator::BitwiseOrEquals => "|=",
            AssignmentOperator::BitwiseXorEquals => "^=",
            AssignmentOperator::BitwiseAndEquals => "&=",
        };

        serializer.serialize_str(s)
    }
}

/// A logical operator token.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LogicalOperator {
    LogicalOr,
    LogicalAnd,
}
impl Serialize for LogicalOperator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = match self {
            LogicalOperator::LogicalOr => "||",
            LogicalOperator::LogicalAnd => "&&",
        };

        serializer.serialize_str(s)
    }
}

/// A mega enum with ever single possible AST node.
/// Refer to https://github.com/estree/estree/blob/master/es5.md for spec on AST nodes.
#[derive(Debug, PartialEq, Clone, Serialize)]
#[serde(tag = "type")]
pub enum NodeKind<'a> {
    /// An identifier. Note that an identifier may be an expression or a destructuring pattern.
    Identifier {
        name: String,
    },
    /// A literal token. Note that a literal can be an expression.
    Literal {
        value: LiteralValue,
    },
    /// A complete program source tree.
    Program {
        /// `type: [ Directive | Statement ]`
        body: Vec<Node<'a>>,
    },
    /*
    Statements
    */
    /// An expression statement, i.e., a statement consisting of a single expression.
    ExpressionStatement {
        /// `type: Expression`
        expression: Box<Node<'a>>,
    },
    /// A directive from the directive prologue of a script or function.
    /// The `directive` property is the raw string source of the directive without quotes.
    Directive {
        /// `type: Literal`
        expression: Box<Node<'a>>,
        /// The raw string source of the directive without quotes.
        directive: String,
    },
    /// A block statement, i.e., a sequence of statements surrounded by braces.
    BlockStatement {
        /// `type: [ Statement ]`
        body: Vec<Node<'a>>,
    },
    /// The body of a function, which is a block statement that may begin with directives.
    FunctionBody {
        /// `type: [ Directive | Statement ]`
        body: Box<Node<'a>>,
    },
    /// An empty statement, i.e., a solitary semicolon.
    EmptyStatement,
    /// A `debugger` statement.
    DebuggerStatement,
    /// A `with` statement.
    WithStatement,
    /*
    Statements / Control Flow
    */
    /// A `return` statement.
    ReturnStatement {
        /// `type: Expression | null`
        argument: Option<Box<Node<'a>>>,
    },
    /// A labeled statement, i.e., a statement prefixed by a `break`/`continue` label.
    LabeledStatement {
        /// `type: Identifier`
        label: Box<Node<'a>>,
        /// `type: Statement`
        body: Box<Node<'a>>,
    },
    /// A `break` statement.
    BreakStatement {
        /// `type: Identifier | null`
        label: Option<Box<Node<'a>>>,
    },
    /// A `continue` statement.
    ContinueStatement {
        /// `type: Identifier | null`
        label: Box<Option<Node<'a>>>,
    },
    /*
    Statements / Choice
    */
    /// An `if` statement.
    IfStatement {
        /// `type: Expression`
        test: Box<Node<'a>>,
        /// `type: Statement`
        consequent: Box<Node<'a>>,
        /// `type: Statement | null`
        alternate: Box<Option<Node<'a>>>,
    },
    /// A `switch` statement.
    SwitchStatement {
        /// `type: Expression`
        discriminant: Box<Node<'a>>,
        /// `type: [ SwitchCase ]`
        cases: Vec<Node<'a>>,
    },
    /// A `case` (if `test` is an `Expression`) or `default` (if `test === null`) clause in the body of a `switch` statement.
    SwitchCase {
        /// `type: Expression | null`
        test: Option<Box<Node<'a>>>,
        /// `type: [ Statement ]`
        consequent: Vec<Node<'a>>,
    },
    /*
    Statements / Exceptions
    */
    /// A `throw` statement.
    ThrowStatement {
        /// `type: Expression`
        argument: Box<Node<'a>>,
    },
    /// A `try` statement. If `handler` is `null` then `finalizer` must be a `BlockStatement`.
    TryStatement {
        /// `type: BlockStatement`
        block: Box<Node<'a>>,
        /// `type: CatchClause | null`
        handler: Option<Box<Node<'a>>>,
        /// `type: BlockStatement | null`
        finalizer: Option<Box<Node<'a>>>,
    },
    CatchClause {
        /// `type: Pattern`
        param: Box<Node<'a>>,
        /// `type: BlockStatement`
        body: Box<Node<'a>>,
    },
    /*
    Statements / Loops
    */
    /// A `while` statement.
    WhileStatement {
        /// `type: Expression`
        test: Box<Node<'a>>,
        /// `type: Statement`
        body: Box<Node<'a>>,
    },
    /// A `do`/`while` statement.
    DoWhileStatement {
        /// `type: Statement`
        body: Box<Node<'a>>,
        /// `type: Expression`
        test: Box<Node<'a>>,
    },
    /// A `for` statement.
    ForStatement {
        /// `type: VariableDeclaration | Expression | null`
        init: Box<Option<Node<'a>>>,
        /// `type: Expression | null`
        test: Box<Option<Node<'a>>>,
        /// `type: Expression | null`
        update: Box<Option<Node<'a>>>,
        /// `type: Statement`
        body: Box<Node<'a>>,
    },
    /// A `for`/`in` statement.
    ForInStatement {
        /// `type: VariableDeclaration |  Pattern`
        left: Box<Node<'a>>,
        /// `type: Expression`
        right: Box<Node<'a>>,
        /// `type: Statement`
        body: Box<Node<'a>>,
    },
    /*
    Statements / Declarations
    */
    /// A function declaration.
    /// Note that unlike in the parent interface `Function`, the `id` cannot be `null`.
    FunctionDeclaration {
        /// `type: Identifier`
        id: Box<Node<'a>>,
        /// `type: Function`
        #[serde(flatten)]
        function: Function<'a>,
    },
    /// A variable declaration.
    VariableDeclaration {
        /// `type: [ VariableDeclarator ]`
        declarations: Vec<Node<'a>>,
        kind: VariableDeclarationKind,
    },
    /// A variable declarator.
    VariableDeclarator {
        /// `type: Pattern`
        id: Box<Node<'a>>,
        /// `type: Expression | null`
        init: Box<Option<Node<'a>>>,
    },
    /*
    Expressions
    */
    /// A `this` expression.
    ThisExpression,
    /// An array expression. An element might be `null` if it represents a hole in a sparse array. E.g. `[1,,2]`.
    ArrayExpression {
        /// `type: [ Expression | null ]`
        elements: Vec<Option<Node<'a>>>,
    },
    /// An object expression.
    ObjectExpression {
        /// `type: [ Property ]`
        properties: Vec<Node<'a>>,
    },
    /// A literal property in an object expression can have either a string or number as its `value`.
    Property {
        /// `type: Literal | Identifier`
        key: Box<Node<'a>>,
        /// `type: Expression`
        value: Box<Node<'a>>,
        kind: PropertyKind,
    },
    /// A `function` expression.
    FunctionExpression {
        /// `type: Function`
        #[serde(flatten)]
        function: Function<'a>,
    },
    /*
    Expressions / Unary operations
    */
    /// An unary operator expression.
    UnaryExpression {
        operator: UnaryOperator,
        prefix: bool,
        /// `type: Expression`
        argument: Box<Node<'a>>,
    },
    /// An update (increment or decrement) operator expression.
    UpdateExpression {
        operator: UpdateOperator,
        /// `type: Expression`
        argument: Box<Node<'a>>,
        prefix: bool,
    },
    /*
    Expressions / Binary operations
    */
    /// A binary operator expression.
    BinaryExpression {
        operator: BinaryOperator,
        /// `type: Expression`
        left: Box<Node<'a>>,
        /// `type: Expression`
        right: Box<Node<'a>>,
    },
    /// An assignment operator expression.
    AssignmentExpression {
        operator: AssignmentOperator,
        /// `type: Pattern | Expression`
        left: Box<Node<'a>>,
        /// `type: Expression`
        right: Box<Node<'a>>,
    },
    /// A logical operator expression.
    LogicalExpression {
        operator: LogicalOperator,
        /// `type: Expression`
        left: Box<Node<'a>>,
        /// `type: Expression`
        right: Box<Node<'a>>,
    },
    /// A member expression. If `computed` is `true`, the node corresponds to a computed (`a[b]`) member expression and `property` is an `Expression`.
    /// If `computed` is `false`, the node corresponds to a static (`a.b`) member expression and `property` is an `Identifier`.
    MemberExpression {
        /// `type: Expression`
        object: Box<Node<'a>>,
        /// `type: Identifier | Literal`
        property: Box<Node<'a>>,
        computed: bool,
    },
    /*
    Expressions
    */
    /// A conditional expression, i.e., a ternary `?`/`:` expression.
    ConditionalExpression {
        /// `type: Expression`
        test: Box<Node<'a>>,
        /// `type: Expression`
        consequent: Box<Node<'a>>,
        /// `type: Expression`
        alternate: Box<Node<'a>>,
    },
    /// A function or method call expression.
    CallExpression {
        /// `type: Expression`
        callee: Box<Node<'a>>,
        /// `type: [ Expression ]`
        arguments: Vec<Node<'a>>,
    },
    /// A `new` expression.
    NewExpression {
        /// `type: Expression`
        callee: Box<Node<'a>>,
        /// `type: [ Expression ]`
        arguments: Vec<Node<'a>>,
    },
    /// A sequence expression, i.e., a comma-separated sequence of expressions.
    SequenceExpression {
        /// `type: [ Expression ]`
        expressions: Vec<Node<'a>>,
    },
    /*
    Patterns
    */
    Pattern,
    /*
    Misc.
    */
    /// An error node. Should be used when source is not syntaxically correct.
    Error,
}
impl<'a> NodeKind<'a> {
    /// Creates a `Node` from `NodeKind` with specified `pos`.
    pub fn with_pos(
        self,
        start: crate::parser::Span<'a>,
        end: crate::parser::Span<'a>,
    ) -> Node<'a> {
        Node {
            kind: self,
            start,
            end,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Node<'a> {
    #[serde(flatten)]
    pub kind: NodeKind<'a>,
    #[serde(serialize_with = "serialize_span")]
    pub start: crate::parser::Span<'a>,
    #[serde(serialize_with = "serialize_span")]
    pub end: crate::parser::Span<'a>,
}

fn serialize_span<S>(pos: &crate::parser::Span, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_u64(pos.location_offset() as u64)
}

impl<'a> Node<'a> {
    /// Returns the length of the AST node in source code.
    pub fn len(&self) -> usize {
        self.end.location_offset() - self.start.location_offset()
    }
}
