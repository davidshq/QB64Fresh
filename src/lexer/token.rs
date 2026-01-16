//! Token definitions for the QB64Fresh lexer.
//!
//! This module defines all tokens recognized by the BASIC lexer, including:
//! - Keywords (IF, THEN, PRINT, etc.)
//! - Operators (+, -, AND, OR, etc.)
//! - Literals (numbers, strings)
//! - Punctuation and delimiters
//!
//! ## Design Notes
//!
//! We use the `logos` crate for lexical analysis. Logos generates a fast,
//! table-driven lexer from token definitions using procedural macros.
//!
//! BASIC is case-insensitive, so keywords are matched with `(?i:...)` regex.

use logos::Logos;
use std::fmt;

/// A token with its location in the source code.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token
    pub kind: TokenKind,
    /// Byte offset where this token starts in the source
    pub span: std::ops::Range<usize>,
    /// The original text of the token (useful for identifiers, literals)
    pub text: String,
}

impl Token {
    /// Create a new token with the given kind, span, and text.
    pub fn new(kind: TokenKind, span: std::ops::Range<usize>, text: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            text: text.into(),
        }
    }
}

/// All possible token types in QB64/QBasic.
///
/// Tokens are grouped into categories:
/// - Keywords (control flow, declarations, I/O, etc.)
/// - Operators (arithmetic, comparison, logical)
/// - Literals (numbers, strings)
/// - Punctuation and delimiters
/// - Special tokens (comments, whitespace, errors)
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r]+")] // Skip horizontal whitespace (but not newlines!)
pub enum TokenKind {
    // ==================== Control Flow Keywords ====================

    /// IF keyword - begins conditional statement
    #[token("IF", ignore(ascii_case))]
    If,

    /// THEN keyword - follows IF condition
    #[token("THEN", ignore(ascii_case))]
    Then,

    /// ELSE keyword - alternative branch
    #[token("ELSE", ignore(ascii_case))]
    Else,

    /// ELSEIF keyword - chained conditional
    #[token("ELSEIF", ignore(ascii_case))]
    ElseIf,

    /// END keyword - used with IF, SUB, FUNCTION, TYPE, SELECT
    #[token("END", ignore(ascii_case))]
    End,

    /// FOR keyword - begins FOR loop
    #[token("FOR", ignore(ascii_case))]
    For,

    /// TO keyword - FOR loop range
    #[token("TO", ignore(ascii_case))]
    To,

    /// STEP keyword - FOR loop increment
    #[token("STEP", ignore(ascii_case))]
    Step,

    /// NEXT keyword - ends FOR loop
    #[token("NEXT", ignore(ascii_case))]
    Next,

    /// WHILE keyword - begins WHILE loop or DO WHILE
    #[token("WHILE", ignore(ascii_case))]
    While,

    /// WEND keyword - ends WHILE loop
    #[token("WEND", ignore(ascii_case))]
    Wend,

    /// DO keyword - begins DO loop
    #[token("DO", ignore(ascii_case))]
    Do,

    /// LOOP keyword - ends DO loop
    #[token("LOOP", ignore(ascii_case))]
    Loop,

    /// UNTIL keyword - loop condition
    #[token("UNTIL", ignore(ascii_case))]
    Until,

    /// SELECT keyword - begins SELECT CASE
    #[token("SELECT", ignore(ascii_case))]
    Select,

    /// CASE keyword - SELECT CASE branch
    #[token("CASE", ignore(ascii_case))]
    Case,

    /// GOTO keyword - unconditional jump
    #[token("GOTO", ignore(ascii_case))]
    Goto,

    /// GOSUB keyword - subroutine call (legacy)
    #[token("GOSUB", ignore(ascii_case))]
    Gosub,

    /// RETURN keyword - return from GOSUB or FUNCTION
    #[token("RETURN", ignore(ascii_case))]
    Return,

    /// EXIT keyword - early exit from loop/sub/function
    #[token("EXIT", ignore(ascii_case))]
    Exit,

    // ==================== Declaration Keywords ====================

    /// DIM keyword - variable declaration
    #[token("DIM", ignore(ascii_case))]
    Dim,

    /// REDIM keyword - resize dynamic array
    #[token("REDIM", ignore(ascii_case))]
    Redim,

    /// AS keyword - type specification
    #[token("AS", ignore(ascii_case))]
    As,

    /// SHARED keyword - module-level variable
    #[token("SHARED", ignore(ascii_case))]
    Shared,

    /// STATIC keyword - static variable
    #[token("STATIC", ignore(ascii_case))]
    Static,

    /// CONST keyword - constant declaration
    #[token("CONST", ignore(ascii_case))]
    Const,

    /// TYPE keyword - begins user-defined type
    #[token("TYPE", ignore(ascii_case))]
    Type,

    /// DECLARE keyword - forward declaration
    #[token("DECLARE", ignore(ascii_case))]
    Declare,

    /// SUB keyword - subroutine definition
    #[token("SUB", ignore(ascii_case))]
    Sub,

    /// FUNCTION keyword - function definition
    #[token("FUNCTION", ignore(ascii_case))]
    Function,

    /// LET keyword - assignment (optional in most BASIC)
    #[token("LET", ignore(ascii_case))]
    Let,

    // ==================== Type Keywords ====================

    /// INTEGER type
    #[token("INTEGER", ignore(ascii_case))]
    Integer,

    /// LONG type
    #[token("LONG", ignore(ascii_case))]
    Long,

    /// SINGLE type (single-precision float)
    #[token("SINGLE", ignore(ascii_case))]
    Single,

    /// DOUBLE type (double-precision float)
    #[token("DOUBLE", ignore(ascii_case))]
    Double,

    /// STRING type
    #[token("STRING", ignore(ascii_case))]
    String_,  // Underscore to avoid conflict with Rust's String

    // QB64 extended types
    /// _BYTE type (QB64)
    #[token("_BYTE", ignore(ascii_case))]
    Byte,

    /// _INTEGER64 type (QB64)
    #[token("_INTEGER64", ignore(ascii_case))]
    Integer64,

    /// _FLOAT type (QB64)
    #[token("_FLOAT", ignore(ascii_case))]
    Float,

    /// _OFFSET type (QB64 - pointer-sized integer)
    #[token("_OFFSET", ignore(ascii_case))]
    Offset,

    /// _UNSIGNED modifier (QB64)
    #[token("_UNSIGNED", ignore(ascii_case))]
    Unsigned,

    // ==================== I/O Keywords ====================

    /// PRINT statement
    #[token("PRINT", ignore(ascii_case))]
    Print,

    /// INPUT statement
    #[token("INPUT", ignore(ascii_case))]
    Input,

    /// OPEN statement
    #[token("OPEN", ignore(ascii_case))]
    Open,

    /// CLOSE statement
    #[token("CLOSE", ignore(ascii_case))]
    Close,

    /// READ statement (DATA)
    #[token("READ", ignore(ascii_case))]
    Read,

    /// DATA statement
    #[token("DATA", ignore(ascii_case))]
    Data,

    /// RESTORE statement
    #[token("RESTORE", ignore(ascii_case))]
    Restore,

    /// WRITE statement
    #[token("WRITE", ignore(ascii_case))]
    Write,

    /// GET statement (file/graphics)
    #[token("GET", ignore(ascii_case))]
    Get,

    /// PUT statement (file/graphics)
    #[token("PUT", ignore(ascii_case))]
    Put,

    /// LINE keyword (LINE INPUT, LINE graphics)
    #[token("LINE", ignore(ascii_case))]
    Line,

    // ==================== Logical Operators (Keywords) ====================

    /// AND operator
    #[token("AND", ignore(ascii_case))]
    And,

    /// OR operator
    #[token("OR", ignore(ascii_case))]
    Or,

    /// NOT operator
    #[token("NOT", ignore(ascii_case))]
    Not,

    /// XOR operator
    #[token("XOR", ignore(ascii_case))]
    Xor,

    /// EQV operator (equivalence)
    #[token("EQV", ignore(ascii_case))]
    Eqv,

    /// IMP operator (implication)
    #[token("IMP", ignore(ascii_case))]
    Imp,

    /// MOD operator
    #[token("MOD", ignore(ascii_case))]
    Mod,

    // ==================== Arithmetic Operators ====================

    /// + addition or string concatenation
    #[token("+")]
    Plus,

    /// - subtraction or negation
    #[token("-")]
    Minus,

    /// * multiplication
    #[token("*")]
    Star,

    /// / division (floating point)
    #[token("/")]
    Slash,

    /// \ integer division
    #[token("\\")]
    Backslash,

    /// ^ exponentiation
    #[token("^")]
    Caret,

    // ==================== Comparison Operators ====================

    /// = equals (assignment or comparison)
    #[token("=")]
    Equals,

    /// <> not equals
    #[token("<>")]
    NotEquals,

    /// < less than
    #[token("<")]
    LessThan,

    /// > greater than
    #[token(">")]
    GreaterThan,

    /// <= less than or equal
    #[token("<=")]
    LessEquals,

    /// >= greater than or equal
    #[token(">=")]
    GreaterEquals,

    // ==================== Punctuation ====================

    /// ( left parenthesis
    #[token("(")]
    LeftParen,

    /// ) right parenthesis
    #[token(")")]
    RightParen,

    /// , comma - argument separator
    #[token(",")]
    Comma,

    /// ; semicolon - PRINT separator
    #[token(";")]
    Semicolon,

    /// : colon - statement separator
    #[token(":")]
    Colon,

    /// . dot - member access
    #[token(".")]
    Dot,

    /// # hash - file number prefix
    #[token("#")]
    Hash,

    // ==================== Type Suffixes ====================
    // These appear at the end of identifiers to indicate type

    /// $ string type suffix
    #[token("$")]
    DollarSign,

    /// % integer type suffix
    #[token("%")]
    Percent,

    /// & long type suffix
    #[token("&")]
    Ampersand,

    /// ! single type suffix
    #[token("!")]
    Exclamation,

    /// @ _OFFSET type suffix (QB64)
    // Note: In QB64, @ is used as OFFSET suffix, but we'll handle this in parsing

    // ==================== Literals ====================

    /// Integer literal (decimal)
    /// Examples: 123, 0, 999999
    #[regex(r"[0-9]+", priority = 2)]
    IntegerLiteral,

    /// Hexadecimal literal
    /// Examples: &H1F, &HFF00
    #[regex(r"&[Hh][0-9A-Fa-f]+")]
    HexLiteral,

    /// Octal literal
    /// Examples: &O17, &O777
    #[regex(r"&[Oo][0-7]+")]
    OctalLiteral,

    /// Binary literal (QB64 extension)
    /// Examples: &B1010, &B11110000
    #[regex(r"&[Bb][01]+")]
    BinaryLiteral,

    /// Floating point literal
    /// Examples: 1.5, .5, 1., 1.5E10, 1.5D-3
    #[regex(r"[0-9]*\.[0-9]+([EeDd][+-]?[0-9]+)?|[0-9]+[EeDd][+-]?[0-9]+")]
    FloatLiteral,

    /// String literal
    /// Everything between double quotes
    #[regex(r#""[^"]*""#)]
    StringLiteral,

    // ==================== Identifiers ====================

    /// Identifier (variable, function, or label name)
    /// Must start with letter, can contain letters, digits, and underscores
    /// May end with type suffix ($, %, &, !, #)
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*[$%&!#]?")]
    Identifier,

    // ==================== Special Tokens ====================

    /// Comment - starts with '
    #[regex(r"'[^\n]*")]
    Comment,

    /// REM comment (traditional BASIC comment keyword)
    /// Note: Must be followed by space or end of line to distinguish from identifiers like REMOVE
    #[regex(r"(?i:REM)([ \t][^\n]*)?")]
    RemComment,

    /// Newline - significant in BASIC (ends statements)
    #[regex(r"\n")]
    Newline,

    /// Line continuation (underscore at end of line)
    #[regex(r"_[ \t]*\n")]
    LineContinuation,

    // ==================== Preprocessor (QB64) ====================

    /// $INCLUDE directive
    #[regex(r"\$INCLUDE\s*:\s*'[^']*'", ignore(ascii_case))]
    IncludeDirective,

    /// Other $ directives ($IF, $ELSE, $END IF, etc.)
    #[regex(r"\$[A-Za-z][A-Za-z0-9]*")]
    MetaCommand,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Keywords display as uppercase
            TokenKind::If => write!(f, "IF"),
            TokenKind::Then => write!(f, "THEN"),
            TokenKind::Else => write!(f, "ELSE"),
            TokenKind::ElseIf => write!(f, "ELSEIF"),
            TokenKind::End => write!(f, "END"),
            TokenKind::Print => write!(f, "PRINT"),
            // ... add more as needed

            // Operators display as symbols
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Equals => write!(f, "="),

            // Default: use debug representation
            _ => write!(f, "{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    /// Helper to collect all tokens from source
    fn lex_all(source: &str) -> Vec<TokenKind> {
        TokenKind::lexer(source)
            .filter_map(|r| r.ok())
            .collect()
    }

    #[test]
    fn test_keywords_case_insensitive() {
        assert_eq!(lex_all("IF"), vec![TokenKind::If]);
        assert_eq!(lex_all("if"), vec![TokenKind::If]);
        assert_eq!(lex_all("If"), vec![TokenKind::If]);
        assert_eq!(lex_all("iF"), vec![TokenKind::If]);
    }

    #[test]
    fn test_simple_print_statement() {
        let tokens = lex_all(r#"PRINT "Hello, World!""#);
        assert_eq!(tokens, vec![
            TokenKind::Print,
            TokenKind::StringLiteral,
        ]);
    }

    #[test]
    fn test_arithmetic_expression() {
        let tokens = lex_all("1 + 2 * 3");
        assert_eq!(tokens, vec![
            TokenKind::IntegerLiteral,
            TokenKind::Plus,
            TokenKind::IntegerLiteral,
            TokenKind::Star,
            TokenKind::IntegerLiteral,
        ]);
    }

    #[test]
    fn test_if_statement() {
        let tokens = lex_all("IF x > 10 THEN PRINT x");
        assert_eq!(tokens, vec![
            TokenKind::If,
            TokenKind::Identifier,
            TokenKind::GreaterThan,
            TokenKind::IntegerLiteral,
            TokenKind::Then,
            TokenKind::Print,
            TokenKind::Identifier,
        ]);
    }

    #[test]
    fn test_hex_literal() {
        let tokens = lex_all("&HFF &h1a");
        assert_eq!(tokens, vec![
            TokenKind::HexLiteral,
            TokenKind::HexLiteral,
        ]);
    }

    #[test]
    fn test_float_literals() {
        let tokens = lex_all("1.5 .5 1.5E10 1D-3");
        assert_eq!(tokens, vec![
            TokenKind::FloatLiteral,
            TokenKind::FloatLiteral,
            TokenKind::FloatLiteral,
            TokenKind::FloatLiteral,
        ]);
    }

    #[test]
    fn test_comment() {
        let tokens = lex_all("x = 1 ' this is a comment");
        assert_eq!(tokens, vec![
            TokenKind::Identifier,
            TokenKind::Equals,
            TokenKind::IntegerLiteral,
            TokenKind::Comment,
        ]);
    }

    #[test]
    fn test_type_suffixes() {
        let tokens = lex_all("name$ count% total& value! num#");
        assert_eq!(tokens, vec![
            TokenKind::Identifier,  // name$ - suffix included in identifier
            TokenKind::Identifier,  // count%
            TokenKind::Identifier,  // total&
            TokenKind::Identifier,  // value!
            TokenKind::Identifier,  // num#
        ]);
    }
}
