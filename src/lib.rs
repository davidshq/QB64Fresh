#![warn(missing_docs)]

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
//! For tools (formatter, linter, debugger, LSP), use the **[`compiler_api`]** facade
//! instead of calling phases directly: it exposes "parse only," "parse + analyze,"
//! "analyze from AST," and "compile" so that dependency boundaries stay stable.
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
pub mod compiler_api;
pub mod error_formatting;
pub mod lexer;
pub mod library;
pub mod lsp;
pub mod parser;
pub mod preprocessor;
pub mod semantic;

/// C header parsing for automatic DECLARE LIBRARY support.
///
/// This module is optional and provides the ability to parse C header files
/// to automatically extract function signatures for use with DECLARE LIBRARY.
#[cfg(feature = "header-parsing")]
pub mod header_parser;

/// Re-export commonly used types and compiler API for convenience.
pub mod prelude {
    pub use crate::ast::{Expr, ExprKind, Program, Span, Statement, StatementKind};
    pub use crate::codegen::{CBackend, CodeGenError, CodeGenerator, GeneratedOutput, RuntimeMode};
    pub use crate::compiler_api::{
        analyze, compile, parse, parse_and_analyze, parse_tokens, CompileApiError, CompileOptions,
        ParseApiError, ParseOptions,
    };
    pub use crate::lexer::{Lexer, Token, TokenKind};
    pub use crate::parser::{ParseError, Parser};
    pub use crate::preprocessor::{
        EmbeddedFile, PreprocessResult, PreprocessorError, preprocess, preprocess_file,
    };
    pub use crate::semantic::{BasicType, SemanticAnalyzer, SemanticError, TypedProgram};
}
