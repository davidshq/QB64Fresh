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
//! - [`audio`] - BEEP, SOUND, PLAY, _SND* statements
//! - [`file_io`] - OPEN, CLOSE, GET, PUT, SEEK, WRITE # statements
//! - [`graphics`] - SCREEN, LINE, CIRCLE, PAINT, VIEW, WINDOW, _DISPLAY, etc.
//! - [`system`] - SHELL, KILL, NAME, MKDIR, BLOAD, BSAVE, _CLIPBOARD$, etc.
//! - [`error`] - Parse error types
//!
//! # Error Recovery
//!
//! The parser attempts to recover from errors and continue parsing to report
//! multiple errors at once. This provides better feedback to users than stopping
//! at the first error.

mod audio;
mod control_flow;
mod directives;
mod error;
mod expressions;
mod file_io;
mod graphics;
mod procedures;
mod statements;
mod system;
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
    /// Whether we're at the start of a logical line (after newline, not after colon).
    /// This is used to distinguish label definitions from procedure calls.
    at_line_start: bool,
}

impl<'a> Parser<'a> {
    /// Creates a new parser for the given tokens.
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
            at_line_start: true, // Start of file is start of line
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
    ///
    /// Statements are separated by either newlines or colons.
    /// BASIC allows multiple statements on one line: `x = 1: y = 2: PRINT x + y`
    fn parse_program(&mut self) -> Vec<crate::ast::Statement> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            // Skip any statement separators (newlines and colons)
            self.skip_statement_separators();

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
mod tests;
