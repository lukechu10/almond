//! AST nodes for JS

use serde::{Serialize, Serializer};

/// A literal value.
#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum LiteralValue {
    String(String),
    Boolean(bool),
    Null,
    Number(f64),
    RegExp(String),
}

/// A function declaration or expression.
#[derive(Debug, Serialize)]
pub struct Function {
    /// `type: Identifier | null`
    pub id: Option<Box<Node>>,
    /// `type: [ Pattern ]`
    pub params: Vec<Node>,
    /// `type: FunctionBody`
    pub body: Box<Node>,
}

#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
    Plus,
    Not,
    Tilde,
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
            UnaryOperator::Not => "!",
            UnaryOperator::Tilde => "~",
            UnaryOperator::Typeof => "typeof",
            UnaryOperator::Void => "void",
            UnaryOperator::Delete => "delete",
        };

        serializer.serialize_str(s)
    }
}

/// An update (increment or decrement) operator token.
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum NodeKind {
    /// An identifier. Note that an identifier may be an expression or a destructuring pattern.
    Identifier {
        name: String,
    },
    /// A literal token. Note that a literal can be an expression.
    Literal {
        value: LiteralValue
    },
    /// A complete program source tree.
    Program {
        /// `type: [ Directive | Statement ]`
        body: Vec<Node>,
    },
    /*
    Statements
    */
    /// An expression statement, i.e., a statement consisting of a single expression.
    ExpressionStatement {
        /// `type: Expression`
        expression: Box<Node>,
    },
    /// A directive from the directive prologue of a script or function.
    /// The `directive` property is the raw string source of the directive without quotes.
    Directive {
        /// `type: Literal`
        expression: Box<Node>,
        /// The raw string source of the directive without quotes.
        directive: String,
    },
    /// A block statement, i.e., a sequence of statements surrounded by braces.
    BlockStatement {
        /// `type: [ Statement ]`
        body: Vec<Node>,
    },
    /// The body of a function, which is a block statement that may begin with directives.
    FunctionBody {
        /// `type: [ Directive | Statement ]`
        body: Box<Node>,
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
        argument: Option<Box<Node>>,
    },
    /// A labeled statement, i.e., a statement prefixed by a `break`/`continue` label.
    LabeledStatement {
        /// `type: Identifier`
        label: Box<Node>,
        /// `type: Statement`
        body: Box<Node>,
    },
    /// A `break` statement.
    BreakStatement {
        /// `type: Identifier | null`
        label: Option<Box<Node>>,
    },
    /// A `continue` statement.
    ContinueStatement {
        /// `type: Identifier | null`
        label: Option<Box<Node>>,
    },
    /*
    Statements / Choice
    */
    /// An `if` statement.
    IfStatement {
        /// `type: Expression`
        test: Box<Node>,
        /// `type: Statement`
        consequent: Box<Node>,
        /// `type: Statement | null`
        alternate: Option<Box<Node>>,
    },
    /// A `switch` statement.
    SwitchStatement {
        /// `type: Expression`
        discriminant: Box<Node>,
        /// `type: [ SwitchCase ]`
        cases: Vec<Node>,
    },
    /// A `case` (if `test` is an `Expression`) or `default` (if `test === null`) clause in the body of a `switch` statement.
    SwitchCase {
        /// `type: Expression | null`
        test: Option<Box<Node>>,
        /// `type: [ Statement ]`
        consequent: Vec<Node>,
    },
    /*
    Statements / Exceptions
    */
    /// A `throw` statement.
    ThrowStatement {
        /// `type: Expression`
        argument: Box<Node>,
    },
    /// A `try` statement. If `handler` is `null` then `finalizer` must be a `BlockStatement`.
    TryStatement {
        /// `type: BlockStatement`
        block: Box<Node>,
        /// `type: CatchClause | null`
        handler: Option<Box<Node>>,
        /// `type: BlockStatement | null`
        finalizer: Option<Box<Node>>,
    },
    CatchClause {
        /// `type: Pattern`
        param: Box<Node>,
        /// `type: BlockStatement`
        body: Box<Node>,
    },
    /*
    Statements / Loops
    */
    /// A `while` statement.
    WhileStatement {
        /// `type: Expression`
        test: Box<Node>,
        /// `type: Statement`
        body: Box<Node>,
    },
    /// A `do`/`while` statement.
    DoWhileStatement {
        /// `type: Statement`
        body: Box<Node>,
        /// `type: Expression`
        test: Box<Node>,
    },
    /// A `for` statement.
    ForStatement {
        /// `type: VariableDeclaration | Expression | null`
        init: Option<Box<Node>>,
        /// `type: Expression | null`
        test: Option<Box<Node>>,
        /// `type: Expression | null`
        update: Option<Box<Node>>,
        /// `type: Statement`
        body: Box<Node>,
    },
    /// A `for`/`in` statement.
    ForInStatement {
        /// `type: VariableDeclaration |  Pattern`
        left: Box<Node>,
        /// `type: Expression`
        right: Box<Node>,
        /// `type: Statement`
        body: Box<Node>,
    },
    /*
    Statements / Declarations
    */
    /// A function declaration.
    /// Note that unlike in the parent interface `Function`, the `id` cannot be `null`.
    FunctionDeclaration {
        /// `type: Identifier`
        id: Box<Node>,
        /// `type: Function`
        #[serde(flatten)]
        function: Function,
    },
    /// A variable declaration.
    VariableDeclaration {
        /// `type: [ VariableDeclarator ]`
        declarations: Vec<Node>,
        kind: VariableDeclarationKind,
    },
    /// A variable declarator.
    VariableDeclarator {
        /// `type: Pattern`
        id: Box<Node>,
        /// `type: Expression | null`
        init: Option<Box<Node>>,
    },
    /*
    Expressions
    */
    /// A `this` expression.
    ThisExpression,
    /// An array expression. An element might be `null` if it represents a hole in a sparse array. E.g. `[1,,2]`.
    ArrayExpression {
        /// `type: [ Expression | null ]`
        elements: Vec<Option<Node>>,
    },
    /// An object expression.
    ObjectExpression {
        /// `type: [ Property ]`
        properties: Vec<Node>,
    },
    /// A literal property in an object expression can have either a string or number as its `value`.
    Property {
        /// `type: Literal | Identifier`
        key: Box<Node>,
        /// `type: Expression`
        value: Box<Node>,
        kind: PropertyKind,
    },
    /// A `function` expression.
    FunctionExpression {
        /// `type: Function`
        #[serde(flatten)]
        function: Function,
    },
    /*
    Expressions / Unary operations
    */
    /// An unary operator expression.
    UnaryExpression {
        operator: UnaryOperator,
        prefix: bool,
        /// `type: Expression`
        argument: Box<Node>,
    },
    /// An update (increment or decrement) operator expression.
    UpdateExpression {
        operator: UpdateOperator,
        /// `type: Expression`
        argument: Box<Node>,
        prefix: bool,
    },
    /*
    Expressions / Binary operations
    */
    /// A binary operator expression.
    BinaryExpression {
        operator: BinaryOperator,
        /// `type: Expression`
        left: Box<Node>,
        /// `type: Expression`
        right: Box<Node>,
    },
    /// An assignment operator expression.
    AssignmentExpression {
        operator: AssignmentOperator,
        /// `type: Pattern | Expression`
        left: Box<Node>,
        /// `type: Expression`
        right: Box<Node>,
    },
    /// A logical operator expression.
    LogicalExpression {
        operator: LogicalOperator,
        /// `type: Expression`
        left: Box<Node>,
        /// `type: Expression`
        right: Box<Node>,
    },
    /// A member expression. If `computed` is `true`, the node corresponds to a computed (`a[b]`) member expression and `property` is an `Expression`.
    /// If `computed` is `false`, the node corresponds to a static (`a.b`) member expression and `property` is an `Identifier`.
    MemberExpression {
        /// `type: Expression`
        object: Box<Node>,
        /// `type: Expression`
        property: Box<Node>,
        computed: bool,
    },
    /*
    Expressions
    */
    /// A conditional expression, i.e., a ternary `?`/`:` expression.
    ConditionalExpression {
        /// `type: Expression`
        test: Box<Node>,
        /// `type: Expression`
        alternate: Box<Node>,
        /// `type: Expression`
        consequent: Box<Node>,
    },
    /// A function or method call expression.
    CallExpression {
        /// `type: Expression`
        callee: Box<Node>,
        /// `type: [ Expression ]`
        arguments: Vec<Node>,
    },
    /// A `new` expression.
    NewExpression {
        /// `type: Expression`
        callee: Box<Node>,
        /// `type: [ Expression ]`
        arguments: Vec<Node>,
    },
    /// A sequence expression, i.e., a comma-separated sequence of expressions.
    SequenceExpression {
        /// `type: [ Expression ]`
        expressions: Vec<Node>,
    },
    /*
    Patterns
    */
    Pattern,
}

#[derive(Debug, Serialize)]
pub struct Node {
    #[serde(flatten)]
    pub kind: NodeKind,
    /// 0-based start position of node in AST, inclusive.
    pub start: u32,
    /// 0-based end position of node in AST, exclusive.
    pub end: u32,
}
impl Node {
    /// Returns the length of the AST node in source code.
    pub fn len(&self) -> u32 {
        self.end - self.start
    }
}
