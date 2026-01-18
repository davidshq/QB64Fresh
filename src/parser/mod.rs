//! Parser for QB64Fresh BASIC.
//!
//! The parser transforms a stream of tokens into an Abstract Syntax Tree (AST).
//! It uses recursive descent for statements and Pratt parsing (precedence climbing)
//! for expressions.
//!
//! # Example
//!
//! ```
//! use qb64fresh::lexer::lex;
//! use qb64fresh::parser::Parser;
//!
//! let source = r#"
//!     PRINT "Hello, World!"
//!     x = 1 + 2
//! "#;
//!
//! let tokens = lex(source);
//! let mut parser = Parser::new(&tokens);
//! let program = parser.parse().expect("parse failed");
//!
//! assert_eq!(program.statements.len(), 2);
//! ```
//!
//! # Module Structure
//!
//! The parser is split into focused modules:
//! - [`tokens`] - Token navigation utilities (peek, advance, match, expect)
//! - [`expressions`] - Pratt parser for expressions
//! - [`statements`] - Statement dispatcher and simple statements
//! - [`control_flow`] - IF/FOR/WHILE/DO/SELECT parsing
//! - [`procedures`] - SUB/FUNCTION/TYPE definitions
//! - [`directives`] - Preprocessor directives ($INCLUDE, $IF)
//! - [`error`] - Parse error types
//!
//! # Error Recovery
//!
//! The parser attempts to recover from errors and continue parsing to report
//! multiple errors at once. This provides better feedback to users than stopping
//! at the first error.

mod control_flow;
mod directives;
mod error;
mod expressions;
mod procedures;
mod statements;
mod tokens;

pub use error::ParseError;

use crate::ast::Program;
use crate::lexer::Token;

/// Parser for BASIC source code.
///
/// The parser consumes a slice of tokens and produces an AST.
/// Errors are collected and returned at the end rather than failing immediately.
pub struct Parser<'a> {
    /// The tokens to parse.
    tokens: &'a [Token],
    /// Current position in the token stream.
    current: usize,
    /// Collected parse errors.
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser for the given tokens.
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    /// Parses the token stream into a program AST.
    ///
    /// Returns the program if successful, or the collected errors if parsing failed.
    pub fn parse(&mut self) -> Result<Program, Vec<ParseError>> {
        let statements = self.parse_program();

        if self.errors.is_empty() {
            Ok(Program::new(statements))
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Parses a complete program (sequence of statements).
    fn parse_program(&mut self) -> Vec<crate::ast::Statement> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            // Skip any leading newlines
            self.skip_newlines();

            if self.is_at_end() {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(()) => {
                    // Error already recorded; try to recover
                    self.synchronize();
                }
            }
        }

        statements
    }
}

/// Operator precedence levels for Pratt parsing.
///
/// Higher values mean higher precedence (bind tighter).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub(crate) enum Precedence {
    Lowest = 0,
    EqvImp = 1,         // EQV, IMP
    Or = 2,             // OR, XOR
    And = 3,            // AND
    Not = 4,            // NOT (handled as unary)
    Comparison = 5,     // =, <>, <, >, <=, >=
    Additive = 6,       // +, -
    Multiplicative = 7, // *, /, \, MOD
    Unary = 8,          // - (negation), NOT
    Power = 9,          // ^
}

impl Precedence {
    pub(crate) fn from_u8(val: u8) -> Self {
        match val {
            0 => Precedence::Lowest,
            1 => Precedence::EqvImp,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::Not,
            5 => Precedence::Comparison,
            6 => Precedence::Additive,
            7 => Precedence::Multiplicative,
            8 => Precedence::Unary,
            _ => Precedence::Power,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::StatementKind;
    use crate::lexer::lex;

    fn parse(source: &str) -> Result<Program, Vec<ParseError>> {
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        parser.parse()
    }

    #[test]
    fn test_parse_integer_literal() {
        let program = parse("PRINT 42").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_string_literal() {
        let program = parse(r#"PRINT "Hello""#).unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_binary_expression() {
        let program = parse("PRINT 1 + 2").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_precedence() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let program = parse("x = 1 + 2 * 3").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_assignment() {
        let program = parse("x = 5").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::Let { .. }
        ));
    }

    #[test]
    fn test_parse_dim() {
        let program = parse("DIM x AS INTEGER").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::Dim { .. }
        ));
    }

    #[test]
    fn test_parse_if_single_line() {
        let program = parse("IF x > 0 THEN PRINT x").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::If { .. }
        ));
    }

    #[test]
    fn test_parse_for_loop() {
        let program = parse(
            r#"
FOR i = 1 TO 10
    PRINT i
NEXT
"#,
        )
        .unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::For { .. }
        ));
    }

    #[test]
    fn test_parse_while_loop() {
        let program = parse(
            r#"
WHILE x > 0
    x = x - 1
WEND
"#,
        )
        .unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::While { .. }
        ));
    }

    #[test]
    fn test_parse_multiple_statements() {
        let program = parse(
            r#"
PRINT "Hello"
x = 5
PRINT x
"#,
        )
        .unwrap();
        assert_eq!(program.statements.len(), 3);
    }

    #[test]
    fn test_parse_unterminated_string() {
        // Unterminated string (missing closing quote)
        let result = parse(r#"PRINT "Hello"#);
        assert!(result.is_err());

        // The error should be an UnterminatedString error
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        assert!(matches!(errors[0], ParseError::UnterminatedString { .. }));
    }

    #[test]
    fn test_parse_input_with_prompt_semicolon() {
        // INPUT "Name"; x$ - valid, shows "?"
        let program = parse(r#"INPUT "Enter name"; name$"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Input {
                prompt: Some(_),
                show_question_mark: true,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_input_with_prompt_comma() {
        // INPUT "Name", x$ - valid, no "?"
        let program = parse(r#"INPUT "Enter name", name$"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Input {
                prompt: Some(_),
                show_question_mark: false,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_input_prompt_missing_separator() {
        // INPUT "Name" x$ - invalid, missing separator
        let result = parse(r#"INPUT "Enter name" name$"#);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_input_no_prompt() {
        // INPUT x$ - valid, no prompt
        let program = parse("INPUT name$").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Input { prompt: None, .. }
        ));
    }

    // ========================================================
    // Phase 1 Feature Tests: File I/O
    // ========================================================

    #[test]
    fn test_parse_open_for_input() {
        let program = parse(r#"OPEN "test.txt" FOR INPUT AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::OpenFile { .. }
        ));
    }

    #[test]
    fn test_parse_open_for_output() {
        let program = parse(r#"OPEN "output.dat" FOR OUTPUT AS #2"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::OpenFile { .. }
        ));
    }

    #[test]
    fn test_parse_open_for_append() {
        let program = parse(r#"OPEN "log.txt" FOR APPEND AS #3"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::OpenFile { .. }
        ));
    }

    #[test]
    fn test_parse_open_for_binary() {
        let program = parse(r#"OPEN "data.bin" FOR BINARY AS #1"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::OpenFile { .. }
        ));
    }

    #[test]
    fn test_parse_open_for_random() {
        let program = parse(r#"OPEN "records.dat" FOR RANDOM AS #1 LEN = 100"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::OpenFile { .. }
        ));
    }

    #[test]
    fn test_parse_close() {
        let program = parse("CLOSE #1").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::CloseFile { .. }
        ));
    }

    #[test]
    fn test_parse_close_multiple() {
        let program = parse("CLOSE #1, #2, #3").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::CloseFile { file_nums } = &program.statements[0].kind {
            assert_eq!(file_nums.len(), 3);
        } else {
            panic!("Expected CloseFile statement");
        }
    }

    #[test]
    fn test_parse_print_to_file() {
        let program = parse(r#"PRINT #1, "Hello""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FilePrint { .. }
        ));
    }

    #[test]
    fn test_parse_input_from_file() {
        let program = parse("INPUT #1, x, y$").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileInput { .. }
        ));
    }

    #[test]
    fn test_parse_line_input_from_file() {
        let program = parse("LINE INPUT #1, line$").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileLineInput { .. }
        ));
    }

    #[test]
    fn test_parse_get() {
        let program = parse("GET #1, 10, record").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileGet { .. }
        ));
    }

    #[test]
    fn test_parse_put() {
        let program = parse("PUT #1, 10, record").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FilePut { .. }
        ));
    }

    #[test]
    fn test_parse_seek() {
        let program = parse("SEEK #1, 100").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileSeek { .. }
        ));
    }

    // ========================================================
    // Phase 1 Feature Tests: Error Handling
    // ========================================================

    #[test]
    fn test_parse_on_error_goto() {
        let program = parse("ON ERROR GOTO errorHandler").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::OnErrorGoto { .. }
        ));
    }

    #[test]
    fn test_parse_on_error_goto_zero() {
        let program = parse("ON ERROR GOTO 0").unwrap();
        assert_eq!(program.statements.len(), 1);
        // "0" means disable error handling - stored as target "0"
        if let StatementKind::OnErrorGoto { target } = &program.statements[0].kind {
            assert_eq!(target, "0");
        } else {
            panic!("Expected OnErrorGoto statement");
        }
    }

    #[test]
    fn test_parse_on_error_resume_next() {
        let program = parse("ON ERROR RESUME NEXT").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::OnErrorResumeNext { .. }
        ));
    }

    #[test]
    fn test_parse_resume() {
        let program = parse("RESUME").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ResumeStmt { .. }
        ));
    }

    #[test]
    fn test_parse_resume_next() {
        let program = parse("RESUME NEXT").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ResumeStmt { .. }
        ));
    }

    #[test]
    fn test_parse_resume_label() {
        let program = parse("RESUME startOver").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ResumeStmt { .. }
        ));
    }

    #[test]
    fn test_parse_error_statement() {
        let program = parse("ERROR 53").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::ErrorStmt { .. }
        ));
    }

    // ========================================================
    // Phase 1 Feature Tests: Computed Control Flow
    // ========================================================

    #[test]
    fn test_parse_on_goto() {
        let program = parse("ON choice GOTO label1, label2, label3").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OnGoto { targets, .. } = &program.statements[0].kind {
            assert_eq!(targets.len(), 3);
        } else {
            panic!("Expected OnGoto statement");
        }
    }

    #[test]
    fn test_parse_on_gosub() {
        let program = parse("ON menuItem GOSUB sub1, sub2").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::OnGosub { targets, .. } = &program.statements[0].kind {
            assert_eq!(targets.len(), 2);
        } else {
            panic!("Expected OnGosub statement");
        }
    }

    // ========================================================
    // Phase 1 Feature Tests: DEF FN
    // ========================================================

    #[test]
    fn test_parse_def_fn_single_line() {
        let program = parse("DEF FNdouble(x) = x * 2").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::DefFn { .. }
        ));
    }

    #[test]
    fn test_parse_def_fn_with_type() {
        let program = parse("DEF FNsquare%(n%) = n% * n%").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::DefFn { .. }
        ));
    }

    // ========================================================
    // Phase 1 Feature Tests: Variable/Scope Enhancements
    // ========================================================

    #[test]
    fn test_parse_common() {
        let program = parse("COMMON x, y$, z%").unwrap();
        assert_eq!(program.statements.len(), 1);
        if let StatementKind::CommonStmt { variables, .. } = &program.statements[0].kind {
            assert_eq!(variables.len(), 3);
        } else {
            panic!("Expected CommonStmt");
        }
    }

    #[test]
    fn test_parse_common_shared() {
        let program = parse("COMMON SHARED counter%").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::CommonStmt { shared: true, .. }
        ));
    }

    #[test]
    fn test_parse_redim() {
        let program = parse("REDIM array(100)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Redim {
                preserve: false,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_redim_preserve() {
        let program = parse("REDIM _PRESERVE buffer$(newSize)").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Redim { preserve: true, .. }
        ));
    }

    #[test]
    fn test_parse_write_to_file() {
        let program = parse(r#"WRITE #1, "data", 42, 3.14"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::FileWrite { .. }
        ));
    }
}
