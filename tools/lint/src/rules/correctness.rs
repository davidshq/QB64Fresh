//! Correctness lint rules.
//!
//! These rules detect patterns that are likely bugs or will cause
//! unexpected behavior at runtime.

use qb64fresh::ast::{Program, StatementKind};

use super::{collect_all_statements, LintDiagnostic, LintRule, RuleInfo};
use crate::config::{LintCategory, LintConfig, Severity};

/// Detects code that can never be executed.
///
/// # Examples
///
/// ```basic
/// PRINT "start"
/// END
/// PRINT "unreachable"  ' Warning: unreachable code
///
/// SUB Test
///     RETURN
///     PRINT "never runs"  ' Warning: unreachable code
/// END SUB
/// ```
pub struct UnreachableCodeRule;

impl LintRule for UnreachableCodeRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "unreachable_code",
            category: LintCategory::Correctness,
            default_severity: Severity::Warning,
            description: "Detects code that can never be executed",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();

        // Check main program body
        check_unreachable_in_block(&program.statements, &info, severity, &mut diagnostics);

        // Check SUB/FUNCTION bodies
        for stmt in &program.statements {
            match &stmt.kind {
                StatementKind::SubDefinition { body, .. }
                | StatementKind::FunctionDefinition { body, .. } => {
                    check_unreachable_in_block(body, &info, severity, &mut diagnostics);
                }
                _ => {}
            }
        }

        diagnostics
    }
}

/// Checks a block of statements for unreachable code.
fn check_unreachable_in_block(
    statements: &[qb64fresh::ast::Statement],
    info: &RuleInfo,
    severity: Severity,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let mut found_terminator = false;
    let mut terminator_span = None;

    for stmt in statements {
        // If we already found a terminator, this code is unreachable
        if found_terminator {
            // Skip labels - they can be jumped to
            if matches!(stmt.kind, StatementKind::Label { .. }) {
                found_terminator = false; // Reset - label makes code reachable again
                continue;
            }

            // Skip comments - they're not really "code"
            if matches!(stmt.kind, StatementKind::Comment(_)) {
                continue;
            }

            diagnostics.push(
                LintDiagnostic::new(
                    info.name,
                    info.category,
                    severity,
                    "Code after this point is unreachable",
                    stmt.span,
                )
                .with_note(format!(
                    "Previous statement at byte {} terminates execution",
                    terminator_span.unwrap_or(0)
                )),
            );

            // Only report once per block
            break;
        }

        // Check if this statement terminates execution
        match &stmt.kind {
            StatementKind::End { .. } | StatementKind::System { .. } | StatementKind::Stop => {
                found_terminator = true;
                terminator_span = Some(stmt.span.start);
            }

            // GOTO always transfers control (unless it's conditional)
            StatementKind::Goto { .. } => {
                found_terminator = true;
                terminator_span = Some(stmt.span.start);
            }

            // EXIT SUB/FUNCTION terminates the procedure
            StatementKind::Exit { exit_type } => {
                use qb64fresh::ast::ExitType;
                if matches!(exit_type, ExitType::Sub | ExitType::Function) {
                    found_terminator = true;
                    terminator_span = Some(stmt.span.start);
                }
            }

            // Recursively check nested blocks
            StatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                check_unreachable_in_block(then_branch, info, severity, diagnostics);
                for (_, branch) in elseif_branches {
                    check_unreachable_in_block(branch, info, severity, diagnostics);
                }
                if let Some(else_stmts) = else_branch {
                    check_unreachable_in_block(else_stmts, info, severity, diagnostics);
                }
            }

            StatementKind::For { body, .. }
            | StatementKind::While { body, .. }
            | StatementKind::DoLoop { body, .. } => {
                check_unreachable_in_block(body, info, severity, diagnostics);
            }

            StatementKind::SelectCase {
                cases, case_else, ..
            }
            | StatementKind::SelectEveryCase {
                cases, case_else, ..
            } => {
                for case in cases {
                    check_unreachable_in_block(&case.body, info, severity, diagnostics);
                }
                if let Some(else_stmts) = case_else {
                    check_unreachable_in_block(else_stmts, info, severity, diagnostics);
                }
            }

            _ => {}
        }
    }
}

/// Detects implicit variable declarations (variables used without DIM).
///
/// While BASIC allows implicit variable declaration, it can lead to bugs
/// from typos. This lint encourages using OPTION _EXPLICIT.
///
/// # Example
///
/// ```basic
/// counter = 0        ' Warning: implicit variable declaration
/// conter = conter + 1  ' Typo! Creates new variable instead of error
/// PRINT counter      ' Still 0!
/// ```
pub struct ImplicitVariableRule;

impl LintRule for ImplicitVariableRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "implicit_variable",
            category: LintCategory::Correctness,
            default_severity: Severity::Hint, // Just a hint by default
            description: "Detects variables used without explicit DIM declaration",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();

        // Check if OPTION _EXPLICIT is enabled
        let has_option_explicit = collect_all_statements(program)
            .iter()
            .any(|s| matches!(s.kind, StatementKind::OptionExplicit));

        // If OPTION _EXPLICIT is enabled, the compiler handles this
        if has_option_explicit {
            return vec![];
        }

        // Collect explicitly declared variables
        let mut declared = std::collections::HashSet::new();

        for stmt in collect_all_statements(program) {
            match &stmt.kind {
                StatementKind::Dim { variables, .. } => {
                    for var in variables {
                        declared.insert(var.name.to_uppercase());
                    }
                }
                StatementKind::Const { definitions } => {
                    for (name, _) in definitions {
                        declared.insert(name.to_uppercase());
                    }
                }
                StatementKind::For { variable, .. } => {
                    declared.insert(variable.to_uppercase());
                }
                StatementKind::SubDefinition { params, .. }
                | StatementKind::FunctionDefinition { params, .. } => {
                    for param in params {
                        declared.insert(param.name.to_uppercase());
                    }
                }
                _ => {}
            }
        }

        // Find assignments to undeclared variables
        for stmt in collect_all_statements(program) {
            if let StatementKind::Let { name, .. } = &stmt.kind {
                let upper = name.to_uppercase();
                if !declared.contains(&upper) {
                    // First assignment implicitly declares
                    declared.insert(upper.clone());

                    diagnostics.push(
                        LintDiagnostic::new(
                            info.name,
                            info.category,
                            severity,
                            format!("Variable '{}' is implicitly declared", name),
                            stmt.span,
                        )
                        .with_suggestion(
                            "Add 'OPTION _EXPLICIT' and declare with DIM to catch typos",
                        ),
                    );
                }
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
    fn test_unreachable_after_end() {
        let source = r#"
PRINT "start"
END
PRINT "unreachable"
"#;
        let diagnostics = parse_and_check(&UnreachableCodeRule, source);
        assert!(
            !diagnostics.is_empty(),
            "Should detect unreachable code after END"
        );
    }

    #[test]
    fn test_label_makes_code_reachable() {
        let source = r#"
GOTO skip
PRINT "skipped"
skip:
PRINT "reachable"
"#;
        let diagnostics = parse_and_check(&UnreachableCodeRule, source);
        // The PRINT "reachable" should NOT be flagged because label makes it reachable
        let has_reachable_warning = diagnostics
            .iter()
            .any(|d| d.span.start > source.find("skip:").unwrap());
        assert!(
            !has_reachable_warning,
            "Code after label should not be flagged"
        );
    }

    #[test]
    fn test_implicit_variable_warning() {
        let source = r#"
x = 5
PRINT x
"#;
        let diagnostics = parse_and_check(&ImplicitVariableRule, source);
        assert!(
            !diagnostics.is_empty(),
            "Should warn about implicit variable"
        );
    }

    #[test]
    fn test_explicit_dim_no_warning() {
        let source = r#"
DIM x AS INTEGER
x = 5
PRINT x
"#;
        let diagnostics = parse_and_check(&ImplicitVariableRule, source);
        let has_x_warning = diagnostics
            .iter()
            .any(|d| d.message.to_uppercase().contains("'X'"));
        assert!(
            !has_x_warning,
            "Explicitly declared variable should not warn"
        );
    }
}
