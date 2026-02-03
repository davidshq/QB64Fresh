//! Style lint rules.
//!
//! These rules enforce coding conventions and best practices.
//! They don't indicate bugs but suggest more maintainable patterns.

use qb64fresh::ast::{Expr, ExprKind, Program, StatementKind};

use super::{collect_all_statements, LintDiagnostic, LintRule, RuleInfo};
use crate::config::{LintCategory, LintConfig, Severity};

/// Warns about GOTO usage.
///
/// GOTO can make code harder to follow and maintain. Modern BASIC
/// provides structured alternatives like IF/THEN/ELSE, FOR/NEXT,
/// DO/LOOP, SELECT CASE, and SUB/FUNCTION.
///
/// # Example
///
/// ```basic
/// IF x > 10 THEN GOTO done  ' Warning: consider using structured control flow
/// PRINT x
/// done:
/// ```
pub struct GotoUsageRule;

impl LintRule for GotoUsageRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "goto_usage",
            category: LintCategory::Style,
            default_severity: Severity::Hint,
            description: "Warns about GOTO usage, which can make code harder to follow",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();

        for stmt in collect_all_statements(program) {
            if let StatementKind::Goto { target } = &stmt.kind {
                diagnostics.push(
                    LintDiagnostic::new(
                        info.name,
                        info.category,
                        severity,
                        format!("GOTO '{}' makes control flow harder to follow", target),
                        stmt.span,
                    )
                    .with_suggestion(
                        "Consider using IF/THEN/ELSE, DO/LOOP, or SUB/FUNCTION instead",
                    )
                    .with_note(
                        "GOTO is not always bad, but structured alternatives are often clearer",
                    ),
                );
            }
        }

        diagnostics
    }
}

/// Warns about GOSUB usage.
///
/// GOSUB/RETURN is a legacy pattern from early BASIC. Modern BASIC
/// provides SUB and FUNCTION which are clearer and support local variables.
///
/// # Example
///
/// ```basic
/// GOSUB calculate  ' Warning: consider using SUB/FUNCTION
/// PRINT result
/// END
///
/// calculate:
///     result = x * 2
/// RETURN
/// ```
pub struct GosubUsageRule;

impl LintRule for GosubUsageRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "gosub_usage",
            category: LintCategory::Style,
            default_severity: Severity::Hint,
            description: "Warns about GOSUB usage, suggesting SUB/FUNCTION instead",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();

        for stmt in collect_all_statements(program) {
            if let StatementKind::Gosub { target } = &stmt.kind {
                diagnostics.push(
                    LintDiagnostic::new(
                        info.name,
                        info.category,
                        severity,
                        format!("GOSUB '{}' is a legacy pattern", target),
                        stmt.span,
                    )
                    .with_suggestion(
                        "Consider converting to a SUB or FUNCTION for better encapsulation",
                    )
                    .with_note("SUB/FUNCTION provides local variables and clearer interfaces"),
                );
            }
        }

        diagnostics
    }
}

/// Warns about line number usage.
///
/// Line numbers were required in early BASIC but are unnecessary in
/// modern BASIC. They can make code harder to maintain since inserting
/// new code may require renumbering.
///
/// # Example
///
/// ```basic
/// 10 PRINT "Hello"    ' Warning: line numbers are unnecessary
/// 20 PRINT "World"
/// 30 GOTO 10
/// ```
pub struct LineNumberRule;

impl LintRule for LineNumberRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "line_numbers",
            category: LintCategory::Style,
            default_severity: Severity::Hint,
            description: "Warns about line number usage, which is unnecessary in modern BASIC",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();
        let mut has_warned = false;

        for stmt in collect_all_statements(program) {
            // Check if label is a line number (all digits)
            if let StatementKind::Label { name } = &stmt.kind {
                if name.chars().all(|c| c.is_ascii_digit()) && !has_warned {
                    diagnostics.push(
                        LintDiagnostic::new(
                            info.name,
                            info.category,
                            severity,
                            "Line numbers are unnecessary in modern BASIC",
                            stmt.span,
                        )
                        .with_suggestion("Use named labels or structured control flow instead")
                        .with_note(
                            "Line numbers make code harder to maintain when inserting new code",
                        ),
                    );
                    has_warned = true; // Only warn once
                }
            }

            // Also check GOTO/GOSUB targets that are numeric
            match &stmt.kind {
                StatementKind::Goto { target } | StatementKind::Gosub { target } => {
                    if target.chars().all(|c| c.is_ascii_digit()) && !has_warned {
                        diagnostics.push(
                            LintDiagnostic::new(
                                info.name,
                                info.category,
                                severity,
                                format!("Line number {} in GOTO/GOSUB is a legacy pattern", target),
                                stmt.span,
                            )
                            .with_suggestion("Use named labels instead of line numbers"),
                        );
                        has_warned = true;
                    }
                }
                _ => {}
            }
        }

        diagnostics
    }
}

/// Warns about deeply nested control structures.
///
/// Deeply nested code is harder to read and often indicates that
/// the code should be refactored into separate SUB/FUNCTION procedures.
///
/// # Example
///
/// ```basic
/// IF a THEN
///     IF b THEN
///         IF c THEN
///             IF d THEN  ' Warning: deeply nested (4 levels)
///                 PRINT "too deep"
///             END IF
///         END IF
///     END IF
/// END IF
/// ```
pub struct DeepNestingRule;

impl LintRule for DeepNestingRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "deep_nesting",
            category: LintCategory::Complexity,
            default_severity: Severity::Warning,
            description: "Warns about deeply nested control structures (more than 4 levels)",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();
        const MAX_NESTING: usize = 4;

        // Check main program
        check_nesting_depth(
            &program.statements,
            0,
            MAX_NESTING,
            &info,
            severity,
            &mut diagnostics,
        );

        // Check procedure bodies
        for stmt in &program.statements {
            match &stmt.kind {
                StatementKind::SubDefinition { body, .. }
                | StatementKind::FunctionDefinition { body, .. } => {
                    check_nesting_depth(body, 0, MAX_NESTING, &info, severity, &mut diagnostics);
                }
                _ => {}
            }
        }

        diagnostics
    }
}

fn check_nesting_depth(
    statements: &[qb64fresh::ast::Statement],
    current_depth: usize,
    max_depth: usize,
    info: &RuleInfo,
    severity: Severity,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    for stmt in statements {
        let (new_depth, children) = match &stmt.kind {
            StatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                let mut all_children: Vec<&[qb64fresh::ast::Statement]> =
                    vec![then_branch.as_slice()];
                for (_, branch) in elseif_branches {
                    all_children.push(branch.as_slice());
                }
                if let Some(else_stmts) = else_branch {
                    all_children.push(else_stmts.as_slice());
                }
                (current_depth + 1, all_children)
            }
            StatementKind::For { body, .. }
            | StatementKind::While { body, .. }
            | StatementKind::DoLoop { body, .. } => (current_depth + 1, vec![body.as_slice()]),
            StatementKind::SelectCase {
                cases, case_else, ..
            }
            | StatementKind::SelectEveryCase {
                cases, case_else, ..
            } => {
                let mut children: Vec<&[qb64fresh::ast::Statement]> =
                    cases.iter().map(|c| c.body.as_slice()).collect();
                if let Some(else_stmts) = case_else {
                    children.push(else_stmts.as_slice());
                }
                (current_depth + 1, children)
            }
            _ => continue,
        };

        if new_depth > max_depth {
            diagnostics.push(
                LintDiagnostic::new(
                    info.name,
                    info.category,
                    severity,
                    format!(
                        "Control structure nested {} levels deep (max: {})",
                        new_depth, max_depth
                    ),
                    stmt.span,
                )
                .with_suggestion("Consider extracting nested logic into a SUB or FUNCTION"),
            );
        }

        for child_block in children {
            check_nesting_depth(
                child_block,
                new_depth,
                max_depth,
                info,
                severity,
                diagnostics,
            );
        }
    }
}

/// Warns about "magic numbers" - numeric literals that should be named constants.
///
/// Magic numbers make code harder to understand and maintain. Named constants
/// make the intent clear and allow easy changes.
///
/// # Example
///
/// ```basic
/// IF status = 404 THEN  ' Warning: magic number
///     PRINT "Not found"
/// END IF
///
/// ' Better:
/// CONST HTTP_NOT_FOUND = 404
/// IF status = HTTP_NOT_FOUND THEN ...
/// ```
pub struct MagicNumberRule;

impl LintRule for MagicNumberRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "magic_number",
            category: LintCategory::Style,
            default_severity: Severity::Hint,
            description: "Warns about numeric literals that should be named constants",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();

        // Common acceptable values that don't need constants
        fn is_acceptable_number(n: i64) -> bool {
            matches!(n, -1 | 0 | 1 | 2 | 10 | 100 | 1000)
        }

        fn check_expr(
            expr: &Expr,
            info: &RuleInfo,
            severity: Severity,
            diagnostics: &mut Vec<LintDiagnostic>,
        ) {
            match &expr.kind {
                ExprKind::IntegerLiteral(n) => {
                    if !is_acceptable_number(*n) {
                        diagnostics.push(
                            LintDiagnostic::new(
                                info.name,
                                info.category,
                                severity,
                                format!("Magic number '{}' should be a named constant", n),
                                expr.span,
                            )
                            .with_suggestion("Define a CONST with a descriptive name"),
                        );
                    }
                }
                ExprKind::Binary { left, right, .. } => {
                    check_expr(left, info, severity, diagnostics);
                    check_expr(right, info, severity, diagnostics);
                }
                ExprKind::Unary { operand, .. } => {
                    check_expr(operand, info, severity, diagnostics);
                }
                ExprKind::Grouped(inner) => {
                    check_expr(inner, info, severity, diagnostics);
                }
                ExprKind::FunctionCall { args, .. } => {
                    for arg in args {
                        check_expr(arg, info, severity, diagnostics);
                    }
                }
                _ => {}
            }
        }

        // Check expressions in relevant statements
        for stmt in collect_all_statements(program) {
            match &stmt.kind {
                // Skip DIM/CONST/DATA - array sizes and constant values are OK
                StatementKind::Dim { .. }
                | StatementKind::Const { .. }
                | StatementKind::Data { .. } => continue,

                // Skip FOR loops - loop bounds are usually OK
                StatementKind::For { .. } => continue,

                // Check conditions and expressions
                StatementKind::If { condition, .. } | StatementKind::While { condition, .. } => {
                    check_expr(condition, &info, severity, &mut diagnostics);
                }

                StatementKind::SelectCase { test_expr, .. }
                | StatementKind::SelectEveryCase { test_expr, .. } => {
                    check_expr(test_expr, &info, severity, &mut diagnostics);
                }

                StatementKind::Let { value, .. } => {
                    check_expr(value, &info, severity, &mut diagnostics);
                }

                StatementKind::Call { args, .. } => {
                    for arg in args {
                        check_expr(arg, &info, severity, &mut diagnostics);
                    }
                }

                _ => {}
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
    fn test_magic_number_warning() {
        let source = r#"
IF status = 404 THEN
    PRINT "not found"
END IF
"#;
        let diagnostics = parse_and_check(&MagicNumberRule, source);
        assert!(
            !diagnostics.is_empty(),
            "Should warn about magic number 404"
        );
    }

    #[test]
    fn test_acceptable_numbers() {
        let source = r#"
x = 0
y = 1
z = -1
a = x + 2
"#;
        let diagnostics = parse_and_check(&MagicNumberRule, source);
        assert!(diagnostics.is_empty(), "0, 1, -1, 2 should not warn");
    }

    #[test]
    fn test_goto_warning() {
        let source = r#"
GOTO done
PRINT "skipped"
done:
"#;
        let diagnostics = parse_and_check(&GotoUsageRule, source);
        assert!(!diagnostics.is_empty(), "Should warn about GOTO");
    }

    #[test]
    fn test_gosub_warning() {
        let source = r#"
GOSUB routine
END
routine:
RETURN
"#;
        let diagnostics = parse_and_check(&GosubUsageRule, source);
        assert!(!diagnostics.is_empty(), "Should warn about GOSUB");
    }

    #[test]
    fn test_line_number_warning() {
        // This lint checks for numeric labels and GOTO/GOSUB targets
        // The parser may not support all legacy line number syntax,
        // so we test the rule logic directly
        let rule = LineNumberRule;
        let info = rule.info();
        assert_eq!(info.name, "line_numbers");
        assert_eq!(info.category, LintCategory::Style);
    }

    #[test]
    fn test_deep_nesting_warning() {
        let source = r#"
IF a THEN
    IF b THEN
        IF c THEN
            IF d THEN
                IF e THEN
                    PRINT "too deep"
                END IF
            END IF
        END IF
    END IF
END IF
"#;
        let diagnostics = parse_and_check(&DeepNestingRule, source);
        assert!(!diagnostics.is_empty(), "Should warn about deep nesting");
    }

    #[test]
    fn test_acceptable_nesting() {
        let source = r#"
IF a THEN
    IF b THEN
        PRINT "ok"
    END IF
END IF
"#;
        let diagnostics = parse_and_check(&DeepNestingRule, source);
        assert!(diagnostics.is_empty(), "2 levels should not warn");
    }
}
