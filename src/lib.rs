//! # QB64Fresh
//!
//! A modern BASIC compiler compatible with QBasic/QuickBASIC and QB64 extensions.
//!
//! ## Architecture
//!
//! The compiler follows a traditional pipeline:
//!
//! ```text
//! Source (.bas) → Lexer → Parser → AST → Semantic Analysis → IR → CodeGen → C → Executable
//! ```
//!
//! Each phase is implemented as a separate module:
//!
//! - [`lexer`] - Tokenizes source code into a stream of tokens
//! - [`parser`] - Builds an Abstract Syntax Tree from tokens
//! - [`ast`] - AST type definitions
//! - `semantic` - (TODO) Type checking and symbol resolution
//! - `codegen` - (TODO) Code generation backends
//!
//! ## Example
//!
//! ```
//! use qb64fresh::lexer::lex;
//! use qb64fresh::parser::Parser;
//!
//! let source = r#"
//!     PRINT "Hello, World!"
//!     x = 1 + 2 * 3
//! "#;
//!
//! let tokens = lex(source);
//! let mut parser = Parser::new(&tokens);
//! let program = parser.parse().expect("parse error");
//!
//! println!("Parsed {} statements", program.statements.len());
//! ```

pub mod ast;
pub mod lexer;
pub mod parser;

// Future modules (uncomment as implemented):
// pub mod semantic;
// pub mod ir;
// pub mod codegen;

/// Re-export commonly used types for convenience
pub mod prelude {
    pub use crate::ast::{Expr, ExprKind, Program, Span, Statement, StatementKind};
    pub use crate::lexer::{Lexer, Token, TokenKind};
    pub use crate::parser::{ParseError, Parser};
}
