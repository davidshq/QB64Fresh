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
//! - [`semantic`] - Type checking and symbol resolution
//! - [`codegen`] - Code generation backends (C backend implemented)
//! - [`lsp`] - Language Server Protocol implementation for IDE integration
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
pub mod codegen;
pub mod lexer;
pub mod lsp;
pub mod parser;
pub mod semantic;

/// Re-export commonly used types for convenience
pub mod prelude {
    pub use crate::ast::{Expr, ExprKind, Program, Span, Statement, StatementKind};
    pub use crate::codegen::{CBackend, CodeGenError, CodeGenerator, GeneratedOutput, RuntimeMode};
    pub use crate::lexer::{Lexer, Token, TokenKind};
    pub use crate::parser::{ParseError, Parser};
    pub use crate::semantic::{BasicType, SemanticAnalyzer, SemanticError, TypedProgram};
}
