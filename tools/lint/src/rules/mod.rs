//! Lint rules for QB64Fresh.
//!
//! This module contains all the lint rules that can be run against BASIC code.
//! Each rule checks for specific patterns that might indicate bugs, style issues,
//! or opportunities for improvement.
//!
//! # Rule Categories
//!
//! - **Correctness**: Likely bugs or undefined behavior
//! - **Style**: Coding conventions and best practices
//! - **Performance**: Inefficient patterns
//! - **Complexity**: Overly complex code
//! - **Deprecated**: Old constructs with better alternatives

mod complexity;
mod correctness;
mod style;
mod unused;

pub use complexity::*;
pub use correctness::*;
pub use style::*;
pub use unused::*;

use crate::config::{LintCategory, LintConfig, Severity};
use qb64fresh::ast::{Program, Span, Statement};

/// A lint diagnostic produced by a rule.
#[derive(Debug, Clone)]
pub struct LintDiagnostic {
    /// The rule that produced this diagnostic.
    pub rule_name: &'static str,
    /// Category of this lint.
    pub category: LintCategory,
    /// Severity level.
    pub severity: Severity,
    /// Human-readable message.
    pub message: String,
    /// Source location.
    pub span: Span,
    /// Optional suggestion for fixing the issue.
    pub suggestion: Option<String>,
    /// Optional additional notes.
    pub notes: Vec<String>,
}

impl LintDiagnostic {
    /// Creates a new diagnostic.
    pub fn new(
        rule_name: &'static str,
        category: LintCategory,
        severity: Severity,
        message: impl Into<String>,
        span: Span,
    ) -> Self {
        Self {
            rule_name,
            category,
            severity,
            message: message.into(),
            span,
            suggestion: None,
            notes: vec![],
        }
    }

    /// Adds a suggestion to this diagnostic.
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// Adds a note to this diagnostic.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

/// Metadata about a lint rule.
#[derive(Debug, Clone)]
pub struct RuleInfo {
    /// Unique identifier for the rule.
    pub name: &'static str,
    /// Category of the rule.
    pub category: LintCategory,
    /// Default severity.
    pub default_severity: Severity,
    /// Short description of what the rule checks.
    pub description: &'static str,
}

/// Trait implemented by all lint rules.
pub trait LintRule: Send + Sync {
    /// Returns metadata about this rule.
    fn info(&self) -> RuleInfo;

    /// Checks the given program and returns any diagnostics.
    ///
    /// The `source` parameter is the original source code, useful for
    /// extracting text at specific spans.
    fn check(&self, program: &Program, source: &str, config: &LintConfig) -> Vec<LintDiagnostic>;

    /// Returns true if this rule should run given the current configuration.
    fn is_enabled(&self, config: &LintConfig) -> bool {
        let info = self.info();
        config.severity_for(info.name, info.category) != Severity::Off
    }
}

/// Collection of all available lint rules.
pub struct RuleRegistry {
    rules: Vec<Box<dyn LintRule>>,
}

impl RuleRegistry {
    /// Creates a new registry with all built-in rules.
    pub fn new() -> Self {
        let rules: Vec<Box<dyn LintRule>> = vec![
            // Correctness rules
            Box::new(UnusedVariableRule),
            Box::new(UnusedLabelRule),
            Box::new(UnreachableCodeRule),
            // Style rules
            Box::new(GotoUsageRule),
            Box::new(GosubUsageRule),
            Box::new(LineNumberRule),
            Box::new(ImplicitVariableRule),
            Box::new(MagicNumberRule),
            // Complexity rules
            Box::new(DeepNestingRule),
            Box::new(LongProcedureRule),
            Box::new(TooManyParametersRule),
        ];

        Self { rules }
    }

    /// Returns an iterator over all rules.
    pub fn iter(&self) -> impl Iterator<Item = &dyn LintRule> {
        self.rules.iter().map(|r| r.as_ref())
    }

    /// Returns a list of all rule names.
    pub fn rule_names(&self) -> Vec<&'static str> {
        self.rules.iter().map(|r| r.info().name).collect()
    }

    /// Runs all enabled rules against the program.
    pub fn check_all(
        &self,
        program: &Program,
        source: &str,
        config: &LintConfig,
    ) -> Vec<LintDiagnostic> {
        let mut diagnostics = Vec::new();

        for rule in &self.rules {
            if rule.is_enabled(config) {
                let rule_diagnostics = rule.check(program, source, config);
                diagnostics.extend(rule_diagnostics);
            }
        }

        // Sort by source location
        diagnostics.sort_by_key(|d| d.span.start);

        diagnostics
    }
}

impl Default for RuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper to collect all statements recursively from a program.
pub fn collect_all_statements(program: &Program) -> Vec<&Statement> {
    let mut statements = Vec::new();
    for stmt in &program.statements {
        collect_statements_recursive(stmt, &mut statements);
    }
    statements
}

fn collect_statements_recursive<'a>(stmt: &'a Statement, out: &mut Vec<&'a Statement>) {
    out.push(stmt);

    use qb64fresh::ast::StatementKind;
    match &stmt.kind {
        StatementKind::If {
            then_branch,
            elseif_branches,
            else_branch,
            ..
        } => {
            for s in then_branch {
                collect_statements_recursive(s, out);
            }
            for (_, branch) in elseif_branches {
                for s in branch {
                    collect_statements_recursive(s, out);
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    collect_statements_recursive(s, out);
                }
            }
        }
        StatementKind::For { body, .. }
        | StatementKind::While { body, .. }
        | StatementKind::DoLoop { body, .. } => {
            for s in body {
                collect_statements_recursive(s, out);
            }
        }
        StatementKind::SelectCase {
            cases, case_else, ..
        }
        | StatementKind::SelectEveryCase {
            cases, case_else, ..
        } => {
            for case in cases {
                for s in &case.body {
                    collect_statements_recursive(s, out);
                }
            }
            if let Some(else_stmts) = case_else {
                for s in else_stmts {
                    collect_statements_recursive(s, out);
                }
            }
        }
        StatementKind::SubDefinition { body, .. }
        | StatementKind::FunctionDefinition { body, .. } => {
            for s in body {
                collect_statements_recursive(s, out);
            }
        }
        StatementKind::ConditionalBlock {
            then_branch,
            elseif_branches,
            else_branch,
            ..
        } => {
            for s in then_branch {
                collect_statements_recursive(s, out);
            }
            for (_, branch) in elseif_branches {
                for s in branch {
                    collect_statements_recursive(s, out);
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    collect_statements_recursive(s, out);
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_creation() {
        let registry = RuleRegistry::new();
        let names = registry.rule_names();

        // Correctness rules
        assert!(names.contains(&"unused_variable"));
        assert!(names.contains(&"unused_label"));
        assert!(names.contains(&"unreachable_code"));
        // Style rules
        assert!(names.contains(&"goto_usage"));
        assert!(names.contains(&"gosub_usage"));
        assert!(names.contains(&"line_numbers"));
        assert!(names.contains(&"implicit_variable"));
        assert!(names.contains(&"magic_number"));
        // Complexity rules
        assert!(names.contains(&"deep_nesting"));
        assert!(names.contains(&"long_procedure"));
        assert!(names.contains(&"too_many_parameters"));
    }

    #[test]
    fn test_diagnostic_builder() {
        let diag = LintDiagnostic::new(
            "test_rule",
            LintCategory::Style,
            Severity::Warning,
            "Test message",
            Span::new(0, 10, 1),
        )
        .with_suggestion("Try this instead")
        .with_note("Additional context");

        assert_eq!(diag.rule_name, "test_rule");
        assert!(diag.suggestion.is_some());
        assert_eq!(diag.notes.len(), 1);
    }
}
