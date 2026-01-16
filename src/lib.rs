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
//! - `parser` - (TODO) Builds an Abstract Syntax Tree from tokens
//! - `ast` - (TODO) AST type definitions
//! - `semantic` - (TODO) Type checking and symbol resolution
//! - `codegen` - (TODO) Code generation backends
//!
//! ## Example
//!
//! ```no_run
//! use qb64fresh::lexer::Lexer;
//!
//! let source = r#"
//!     PRINT "Hello, World!"
//! "#;
//!
//! let lexer = Lexer::new(source);
//! for token in lexer {
//!     println!("{:?}", token);
//! }
//! ```

pub mod lexer;

// Future modules (uncomment as implemented):
// pub mod parser;
// pub mod ast;
// pub mod semantic;
// pub mod ir;
// pub mod codegen;

/// Re-export commonly used types for convenience
pub mod prelude {
    pub use crate::lexer::{Lexer, Token, TokenKind};
}
