//! QB64Fresh Code Formatter Library
//!
//! This library provides code formatting capabilities for QB64/QBasic source files.
//! It can be used both as a library and through the `qb64fresh-fmt` CLI tool.
//!
//! ## Features
//!
//! - **Keyword capitalization**: Standardize keyword casing (UPPERCASE, lowercase, Title Case)
//! - **Indentation**: Consistent indentation for control structures
//! - **Spacing**: Normalize spacing around operators and after keywords
//! - **Line length**: Optionally wrap long lines
//! - **Comment alignment**: Align trailing comments
//!
//! ## Example
//!
//! ```
//! use qb64fresh_fmt::{Formatter, FormatterConfig};
//!
//! let source = "if x>1 then print x";
//! let config = FormatterConfig::default();
//! let formatter = Formatter::new(config);
//! let formatted = formatter.format(source).unwrap();
//!
//! assert_eq!(formatted, "IF x > 1 THEN PRINT x\n");
//! ```

mod config;
mod error;
mod formatter;
mod rules;

pub use config::{FormatterConfig, IndentStyle, KeywordCase};
pub use error::{FormatError, FormatResult};
pub use formatter::Formatter;
