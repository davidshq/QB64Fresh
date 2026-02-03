//! Complexity lint rules.
//!
//! These rules detect overly complex code that should be refactored
//! for better maintainability and readability.

use qb64fresh::ast::{Program, StatementKind};

use super::{LintDiagnostic, LintRule, RuleInfo};
use crate::config::{LintCategory, LintConfig, Severity};

/// Warns about procedures (SUB/FUNCTION) that are too long.
///
/// Long procedures are harder to understand, test, and maintain.
/// Consider breaking them into smaller, focused procedures.
///
/// # Example
///
/// ```basic
/// SUB ProcessData
///     ' ... 100+ lines of code ...  ' Warning: procedure too long
/// END SUB
/// ```
pub struct LongProcedureRule;

impl LintRule for LongProcedureRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "long_procedure",
            category: LintCategory::Complexity,
            default_severity: Severity::Warning,
            description: "Warns about SUB/FUNCTION procedures longer than 50 lines",
        }
    }

    fn check(&self, program: &Program, source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();
        const MAX_LINES: usize = 50;

        for stmt in &program.statements {
            let (name, _body, span) = match &stmt.kind {
                StatementKind::SubDefinition { name, body, .. } => (name.as_str(), body, stmt.span),
                StatementKind::FunctionDefinition { name, body, .. } => {
                    (name.as_str(), body, stmt.span)
                }
                _ => continue,
            };

            // Count lines in the procedure body
            let line_count = count_lines_in_span(source, span);

            if line_count > MAX_LINES {
                diagnostics.push(
                    LintDiagnostic::new(
                        info.name,
                        info.category,
                        severity,
                        format!(
                            "Procedure '{}' is {} lines long (max: {})",
                            name, line_count, MAX_LINES
                        ),
                        stmt.span,
                    )
                    .with_suggestion("Consider breaking this into smaller, focused procedures")
                    .with_note("Smaller procedures are easier to understand, test, and maintain"),
                );
            }
        }

        diagnostics
    }
}

/// Count the number of lines spanned by a source range.
fn count_lines_in_span(source: &str, span: qb64fresh::ast::Span) -> usize {
    let start = span.start.min(source.len());
    let end = span.end.min(source.len());
    let slice = &source[start..end];
    slice.lines().count()
}

/// Warns about procedures with too many parameters.
///
/// Procedures with many parameters are hard to call correctly and
/// often indicate the need for a TYPE to group related values.
///
/// # Example
///
/// ```basic
/// SUB DrawRect(x1, y1, x2, y2, color, filled, thickness, pattern)
///     ' Warning: too many parameters (8, max: 5)
/// END SUB
/// ```
pub struct TooManyParametersRule;

impl LintRule for TooManyParametersRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "too_many_parameters",
            category: LintCategory::Complexity,
            default_severity: Severity::Warning,
            description: "Warns about SUB/FUNCTION with more than 5 parameters",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();
        const MAX_PARAMS: usize = 5;

        for stmt in &program.statements {
            let (name, params) = match &stmt.kind {
                StatementKind::SubDefinition { name, params, .. } => (name.as_str(), params),
                StatementKind::FunctionDefinition { name, params, .. } => (name.as_str(), params),
                _ => continue,
            };

            if params.len() > MAX_PARAMS {
                diagnostics.push(
                    LintDiagnostic::new(
                        info.name,
                        info.category,
                        severity,
                        format!(
                            "Procedure '{}' has {} parameters (max: {})",
                            name,
                            params.len(),
                            MAX_PARAMS
                        ),
                        stmt.span,
                    )
                    .with_suggestion("Consider grouping related parameters into a TYPE")
                    .with_note("Fewer parameters make procedures easier to call and understand"),
                );
            }
        }

        diagnostics
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use qb64fresh::lexer::lex;
    use qb64fresh::parser::Parser;

    fn parse_and_check<R: LintRule>(rule: &R, source: &str) -> Vec<LintDiagnostic> {
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().expect("Parse failed");
        let config = LintConfig::pedantic();
        rule.check(&program, source, &config)
    }

    #[test]
    fn test_short_procedure_ok() {
        let source = r#"
SUB ShortProcedure
    PRINT "line 1"
    PRINT "line 2"
    PRINT "line 3"
END SUB
"#;
        let diagnostics = parse_and_check(&LongProcedureRule, source);
        assert!(diagnostics.is_empty(), "Short procedure should not warn");
    }

    #[test]
    fn test_few_parameters_ok() {
        let source = r#"
SUB DrawLine(x1, y1, x2, y2)
    LINE (x1, y1)-(x2, y2)
END SUB
"#;
        let diagnostics = parse_and_check(&TooManyParametersRule, source);
        assert!(diagnostics.is_empty(), "4 parameters should not warn");
    }

    #[test]
    fn test_too_many_parameters() {
        let source = r#"
SUB TooMany(a, b, c, d, e, f, g)
    PRINT a, b, c, d, e, f, g
END SUB
"#;
        let diagnostics = parse_and_check(&TooManyParametersRule, source);
        assert!(!diagnostics.is_empty(), "7 parameters should warn");
    }
}
