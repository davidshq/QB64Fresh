//! Statement AST nodes.
//!
//! Statements are constructs that perform actions but don't produce values.
//! In BASIC, most lines of code are statements: PRINT, IF, FOR, assignments, etc.
//!
//! # Statement Categories
//!
//! - **I/O**: PRINT, INPUT, LINE INPUT
//! - **Assignment**: LET (optional), variable = expression
//! - **Control flow**: IF/THEN/ELSE, SELECT CASE, GOTO, GOSUB
//! - **Loops**: FOR/NEXT, WHILE/WEND, DO/LOOP
//! - **Declarations**: DIM, CONST, TYPE
//! - **Procedures**: SUB, FUNCTION, CALL

use super::{Expr, Span};

/// A statement with its source location.
#[derive(Debug, Clone)]
pub struct Statement {
    /// The kind of statement.
    pub kind: StatementKind,
    /// Source location of this statement.
    pub span: Span,
}

impl Statement {
    /// Creates a new statement with the given kind and span.
    pub fn new(kind: StatementKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// The different kinds of statements in BASIC.
#[derive(Debug, Clone)]
pub enum StatementKind {
    /// `PRINT expr1; expr2, expr3`
    ///
    /// The `newline` field indicates whether to print a newline at the end.
    /// A trailing semicolon suppresses the newline.
    Print {
        /// Values to print.
        values: Vec<PrintItem>,
        /// Whether to print a newline at the end (false if trailing `;`).
        newline: bool,
    },

    /// `[LET] variable = expression`
    ///
    /// Assignment statement. The LET keyword is optional in modern BASIC.
    Let { name: String, value: Expr },

    /// `array(indices) = expression`
    ///
    /// Array element assignment statement.
    ArrayAssignment {
        /// Array name.
        name: String,
        /// Index expressions.
        indices: Vec<Expr>,
        /// Value to assign.
        value: Expr,
    },

    /// `DIM variable AS type` or `DIM array(size) AS type`
    Dim {
        /// Variable name.
        name: String,
        /// Array dimensions (empty if not an array).
        dimensions: Vec<ArrayDimension>,
        /// Type specification (if AS clause present).
        type_spec: Option<TypeSpec>,
        /// Whether SHARED was specified.
        shared: bool,
    },

    /// `CONST name = value`
    Const { name: String, value: Expr },

    /// Single-line: `IF condition THEN statement [ELSE statement]`
    /// Multi-line: `IF condition THEN ... [ELSEIF ...] [ELSE ...] END IF`
    If {
        /// The condition to test.
        condition: Expr,
        /// Statements to execute if condition is true.
        then_branch: Vec<Statement>,
        /// ELSEIF clauses (condition + statements).
        elseif_branches: Vec<(Expr, Vec<Statement>)>,
        /// Statements to execute if condition is false (ELSE branch).
        else_branch: Option<Vec<Statement>>,
    },

    /// `SELECT CASE expression ... END SELECT`
    SelectCase {
        /// The expression to match against.
        test_expr: Expr,
        /// The CASE clauses.
        cases: Vec<CaseClause>,
        /// CASE ELSE clause (if present).
        case_else: Option<Vec<Statement>>,
    },

    /// `FOR var = start TO end [STEP step] ... NEXT [var]`
    For {
        /// Loop variable name.
        variable: String,
        /// Starting value.
        start: Expr,
        /// Ending value.
        end: Expr,
        /// Step value (defaults to 1 if not specified).
        step: Option<Expr>,
        /// Loop body.
        body: Vec<Statement>,
        /// Variable name after NEXT (if specified, must match loop variable).
        next_variable: Option<String>,
    },

    /// `WHILE condition ... WEND`
    While {
        /// Loop condition.
        condition: Expr,
        /// Loop body.
        body: Vec<Statement>,
    },

    /// `DO [WHILE|UNTIL condition] ... LOOP [WHILE|UNTIL condition]`
    DoLoop {
        /// Pre-condition (DO WHILE/UNTIL).
        pre_condition: Option<DoCondition>,
        /// Loop body.
        body: Vec<Statement>,
        /// Post-condition (LOOP WHILE/UNTIL).
        post_condition: Option<DoCondition>,
    },

    /// `GOTO label` or `GOTO lineNumber`
    Goto { target: String },

    /// `GOSUB label` or `GOSUB lineNumber`
    Gosub { target: String },

    /// `RETURN` - Return from GOSUB
    Return,

    /// `EXIT FOR`, `EXIT WHILE`, `EXIT DO`, `EXIT SUB`, `EXIT FUNCTION`
    Exit { exit_type: ExitType },

    /// `END` - End program execution
    End,

    /// `STOP` - Stop execution (for debugging)
    Stop,

    /// `SWAP var1, var2` - Exchange values of two variables
    Swap {
        /// First variable to swap.
        left: Expr,
        /// Second variable to swap.
        right: Expr,
    },

    /// `_CONTINUE` - Continue to the next loop iteration (QB64)
    Continue {
        /// The type of loop to continue (For, While, Do).
        continue_type: ContinueType,
    },

    /// `INPUT [;]["prompt"{;|,}] variable[, variable...]`
    Input {
        /// Optional prompt string.
        prompt: Option<String>,
        /// Whether to show question mark after prompt.
        show_question_mark: bool,
        /// Variables to read into.
        variables: Vec<String>,
    },

    /// `LINE INPUT [;]["prompt";] variable$`
    LineInput {
        /// Optional prompt string.
        prompt: Option<String>,
        /// Variable to read into (must be string).
        variable: String,
    },

    /// `DATA value1, value2, ...` - compile-time data definition
    ///
    /// DATA statements define a pool of values that can be read using READ.
    /// Values can be numeric literals or string literals.
    Data {
        /// The literal values in this DATA statement.
        values: Vec<DataValue>,
    },

    /// `READ var1, var2, ...` - read from DATA pool
    ///
    /// READ statements consume values from the DATA pool in order.
    Read {
        /// Variables to read into.
        variables: Vec<String>,
    },

    /// `RESTORE [label]` - reset DATA pointer
    ///
    /// RESTORE resets the DATA read position. Optional label specifies
    /// a specific DATA statement to restore to.
    Restore {
        /// Optional label to restore to.
        label: Option<String>,
    },

    /// Label definition: `labelName:`
    Label { name: String },

    /// `SUB name [(parameters)] ... END SUB`
    SubDefinition {
        name: String,
        params: Vec<Parameter>,
        body: Vec<Statement>,
        is_static: bool,
    },

    /// `FUNCTION name [(parameters)] ... END FUNCTION`
    FunctionDefinition {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<TypeSpec>,
        body: Vec<Statement>,
        is_static: bool,
    },

    /// `TYPE TypeName ... END TYPE` - User-defined type definition
    ///
    /// Example:
    /// ```basic
    /// TYPE Person
    ///     name AS STRING * 20
    ///     age AS INTEGER
    /// END TYPE
    /// ```
    TypeDefinition {
        /// The name of the user-defined type.
        name: String,
        /// The members of the type.
        members: Vec<TypeMember>,
    },

    /// `CALL SubName(args)` or `SubName args`
    Call { name: String, args: Vec<Expr> },

    /// Expression used as a statement (e.g., function call with no return value used)
    Expression(Expr),

    /// Comment: `' text` or `REM text`
    Comment(String),

    // ==================== Preprocessor Directives (QB64) ====================
    /// `$INCLUDE: 'filename'` - Include another source file
    ///
    /// This is a compile-time directive that inserts the contents of another
    /// file at this location. The actual file inclusion is handled during
    /// preprocessing, not during parsing.
    IncludeDirective {
        /// The path to the file to include (without quotes).
        path: String,
    },

    /// `$IF condition THEN` ... `$ELSEIF` ... `$ELSE` ... `$END IF`
    ///
    /// Conditional compilation blocks. The condition is typically a compile-time
    /// constant like `WIN`, `LINUX`, `MAC`, or a user-defined symbol.
    ConditionalBlock {
        /// The condition expression (e.g., "WIN", "LINUX = -1").
        condition: String,
        /// Statements in the $IF block.
        then_branch: Vec<Statement>,
        /// $ELSEIF clauses (condition + statements).
        elseif_branches: Vec<(String, Vec<Statement>)>,
        /// $ELSE block statements.
        else_branch: Option<Vec<Statement>>,
    },

    /// Other `$metacommand` directives (e.g., $DYNAMIC, $STATIC, $ERROR, etc.)
    ///
    /// These are stored as-is for later processing.
    MetaCommand {
        /// The command name without the $ prefix (e.g., "DYNAMIC", "STATIC").
        command: String,
        /// Any arguments following the command.
        args: Option<String>,
    },
}

/// An item in a PRINT statement.
///
/// PRINT can have expressions separated by `;` (no spacing) or `,` (tab to next zone).
#[derive(Debug, Clone)]
pub struct PrintItem {
    /// The expression to print.
    pub expr: Expr,
    /// The separator after this item (if any).
    pub separator: Option<PrintSeparator>,
}

/// Separator between PRINT items.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintSeparator {
    /// `;` - Print next item immediately after
    Semicolon,
    /// `,` - Tab to next 14-column print zone
    Comma,
}

/// Array dimension specification.
#[derive(Debug, Clone)]
pub struct ArrayDimension {
    /// Lower bound (defaults to 0 or OPTION BASE setting).
    pub lower: Option<Expr>,
    /// Upper bound.
    pub upper: Expr,
}

/// Type specification for DIM statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    // Standard QBasic types
    Integer,
    Long,
    Single,
    Double,
    String,
    /// Fixed-length string: `STRING * n`
    FixedString(usize),

    // QB64 extended types
    Byte,
    Bit,
    Integer64,
    Float,
    Offset,
    Unsigned(Box<TypeSpec>),

    /// User-defined type reference
    UserDefined(String),
}

impl TypeSpec {
    /// Returns the type suffix character(s) for this type.
    pub fn suffix(&self) -> Option<&'static str> {
        match self {
            TypeSpec::Integer => Some("%"),
            TypeSpec::Long => Some("&"),
            TypeSpec::Single => Some("!"),
            TypeSpec::Double => Some("#"),
            TypeSpec::String | TypeSpec::FixedString(_) => Some("$"),
            TypeSpec::Byte => Some("%%"),
            TypeSpec::Integer64 => Some("&&"),
            TypeSpec::Float => Some("##"),
            TypeSpec::Offset => Some("%&"),
            _ => None,
        }
    }
}

/// A CASE clause in SELECT CASE.
#[derive(Debug, Clone)]
pub struct CaseClause {
    /// The values/conditions to match.
    pub matches: Vec<CaseMatch>,
    /// Statements to execute if matched.
    pub body: Vec<Statement>,
}

/// A single match condition in a CASE clause.
#[derive(Debug, Clone)]
pub enum CaseMatch {
    /// Single value: `CASE 1`
    Single(Expr),
    /// Range: `CASE 1 TO 10`
    Range { from: Expr, to: Expr },
    /// Comparison: `CASE IS > 5`
    Comparison { op: CaseCompareOp, value: Expr },
}

/// Comparison operators allowed in CASE IS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseCompareOp {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

/// DO loop condition type.
#[derive(Debug, Clone)]
pub struct DoCondition {
    /// Whether this is WHILE (true) or UNTIL (false).
    pub is_while: bool,
    /// The condition expression.
    pub condition: Expr,
}

/// Exit statement type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExitType {
    For,
    While,
    Do,
    Sub,
    Function,
}

/// Continue statement type (QB64 _CONTINUE).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContinueType {
    For,
    While,
    Do,
}

/// Parameter definition for SUB/FUNCTION.
#[derive(Debug, Clone)]
pub struct Parameter {
    /// Parameter name.
    pub name: String,
    /// Parameter type (if specified with AS).
    pub type_spec: Option<TypeSpec>,
    /// Whether this is a BYVAL parameter.
    pub by_val: bool,
}

/// Member definition for TYPE (user-defined type).
///
/// Represents a field within a TYPE...END TYPE block.
#[derive(Debug, Clone)]
pub struct TypeMember {
    /// Member name (field name).
    pub name: String,
    /// Member type specification.
    pub type_spec: TypeSpec,
}

/// A literal value in a DATA statement.
#[derive(Debug, Clone)]
pub enum DataValue {
    /// Integer literal.
    Integer(i64),
    /// Floating-point literal.
    Float(f64),
    /// String literal.
    String(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_spec_suffix() {
        assert_eq!(TypeSpec::Integer.suffix(), Some("%"));
        assert_eq!(TypeSpec::String.suffix(), Some("$"));
        assert_eq!(TypeSpec::Double.suffix(), Some("#"));
        assert_eq!(TypeSpec::UserDefined("MyType".to_string()).suffix(), None);
    }

    #[test]
    fn test_create_print_statement() {
        let stmt = Statement::new(
            StatementKind::Print {
                values: vec![],
                newline: true,
            },
            Span::new(0, 5),
        );
        assert!(matches!(
            stmt.kind,
            StatementKind::Print { newline: true, .. }
        ));
    }
}
