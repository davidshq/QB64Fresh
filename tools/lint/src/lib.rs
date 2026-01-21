//! QB64Fresh Code Linter Library
//!
//! This library provides linting capabilities for QB64/QBasic source files.
//! It can be used both as a library and through the `qb64fresh-lint` CLI tool.
//!
//! ## Features
//!
//! - **Unused code detection**: Find unused variables, labels, and procedures
//! - **Style checks**: Warn about GOTO/GOSUB, line numbers, deep nesting
//! - **Correctness hints**: Detect unreachable code, implicit variables
//! - **Configurable**: Enable/disable rules, adjust severity levels
//!
//! ## Example
//!
//! ```
//! use qb64fresh_lint::{Linter, LintConfig};
//!
//! let source = r#"
//!     DIM unused AS INTEGER
//!     PRINT "hello"
//! "#;
//!
//! let config = LintConfig::default();
//! let linter = Linter::new(config);
//! let diagnostics = linter.check_source(source).unwrap();
//!
//! for diag in diagnostics {
//!     println!("{}: {}", diag.severity.name(), diag.message);
//! }
//! ```

mod config;
mod error;
pub mod rules;

pub use config::{LintCategory, LintConfig, RuleConfig, Severity};
pub use error::{LintError, LintResult};
pub use rules::{LintDiagnostic, LintRule, RuleInfo, RuleRegistry};

use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;

/// The main linter interface.
///
/// The `Linter` runs all configured lint rules against BASIC source code
/// and returns a list of diagnostics.
pub struct Linter {
    config: LintConfig,
    registry: RuleRegistry,
}

impl Linter {
    /// Creates a new linter with the given configuration.
    pub fn new(config: LintConfig) -> Self {
        Self {
            config,
            registry: RuleRegistry::new(),
        }
    }

    /// Creates a linter with default configuration.
    pub fn default_config() -> Self {
        Self::new(LintConfig::default())
    }

    /// Creates a linter with strict configuration (warnings as errors).
    pub fn strict() -> Self {
        Self::new(LintConfig::strict())
    }

    /// Creates a linter with pedantic configuration (all rules enabled).
    pub fn pedantic() -> Self {
        Self::new(LintConfig::pedantic())
    }

    /// Returns a reference to the current configuration.
    pub fn config(&self) -> &LintConfig {
        &self.config
    }

    /// Returns a mutable reference to the configuration.
    pub fn config_mut(&mut self) -> &mut LintConfig {
        &mut self.config
    }

    /// Returns a list of all available rule names.
    pub fn available_rules(&self) -> Vec<&'static str> {
        self.registry.rule_names()
    }

    /// Checks the given source code and returns lint diagnostics.
    ///
    /// This method parses the source code and runs all enabled lint rules.
    /// Parse errors are returned as `LintError`, not as diagnostics.
    pub fn check_source(&self, source: &str) -> LintResult<Vec<LintDiagnostic>> {
        // Lex
        let tokens = lex(source);

        // Parse
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().map_err(|errors| {
            let messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
            LintError::ParseError {
                message: messages.join("; "),
            }
        })?;

        // Run lint rules
        let mut diagnostics = self.registry.check_all(&program, source, &self.config);

        // Apply max_diagnostics limit
        if self.config.max_diagnostics > 0 && diagnostics.len() > self.config.max_diagnostics {
            diagnostics.truncate(self.config.max_diagnostics);
        }

        Ok(diagnostics)
    }

    /// Checks a file and returns lint diagnostics.
    pub fn check_file(&self, path: &std::path::Path) -> LintResult<Vec<LintDiagnostic>> {
        let source = std::fs::read_to_string(path).map_err(|e| LintError::ReadError {
            path: path.to_path_buf(),
            source: e,
        })?;

        self.check_source(&source)
    }

    /// Checks multiple files and returns diagnostics for each.
    ///
    /// The returned map is keyed by file path.
    pub fn check_files(
        &self,
        paths: &[std::path::PathBuf],
    ) -> std::collections::HashMap<std::path::PathBuf, LintResult<Vec<LintDiagnostic>>> {
        paths
            .iter()
            .map(|p| (p.clone(), self.check_file(p)))
            .collect()
    }
}

impl Default for Linter {
    fn default() -> Self {
        Self::default_config()
    }
}

/// Counts diagnostics by severity.
#[derive(Debug, Default, Clone)]
pub struct DiagnosticCounts {
    /// Number of error-level diagnostics.
    pub errors: usize,
    /// Number of warning-level diagnostics.
    pub warnings: usize,
    /// Number of hint-level diagnostics.
    pub hints: usize,
}

impl DiagnosticCounts {
    /// Creates counts from a list of diagnostics.
    pub fn from_diagnostics(diagnostics: &[LintDiagnostic]) -> Self {
        let mut counts = Self::default();
        for diag in diagnostics {
            match diag.severity {
                Severity::Error => counts.errors += 1,
                Severity::Warning => counts.warnings += 1,
                Severity::Hint => counts.hints += 1,
                Severity::Off => {}
            }
        }
        counts
    }

    /// Returns the total number of diagnostics.
    pub fn total(&self) -> usize {
        self.errors + self.warnings + self.hints
    }

    /// Returns true if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.errors > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linter_creation() {
        let linter = Linter::default();
        assert!(!linter.available_rules().is_empty());
    }

    #[test]
    fn test_check_clean_source() {
        let linter = Linter::new(LintConfig::permissive());
        let source = r#"
DIM x AS INTEGER
x = 5
PRINT x
"#;
        let result = linter.check_source(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_with_issues() {
        let linter = Linter::pedantic();
        let source = r#"
DIM unused AS INTEGER
PRINT "hello"
"#;
        let result = linter.check_source(source);
        assert!(result.is_ok());
        let diagnostics = result.unwrap();
        // Should have at least the unused variable warning
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostic_counts() {
        let diagnostics = vec![
            LintDiagnostic::new(
                "test",
                LintCategory::Correctness,
                Severity::Error,
                "error",
                qb64fresh::ast::Span::new(0, 1),
            ),
            LintDiagnostic::new(
                "test",
                LintCategory::Style,
                Severity::Warning,
                "warning",
                qb64fresh::ast::Span::new(0, 1),
            ),
            LintDiagnostic::new(
                "test",
                LintCategory::Style,
                Severity::Hint,
                "hint",
                qb64fresh::ast::Span::new(0, 1),
            ),
        ];

        let counts = DiagnosticCounts::from_diagnostics(&diagnostics);
        assert_eq!(counts.errors, 1);
        assert_eq!(counts.warnings, 1);
        assert_eq!(counts.hints, 1);
        assert_eq!(counts.total(), 3);
        assert!(counts.has_errors());
    }
}
