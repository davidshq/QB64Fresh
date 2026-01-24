//! Lint rules for detecting unused code.
//!
//! These rules identify variables, labels, and procedures that are declared
//! but never used, which often indicates dead code or typos.

use std::collections::{HashMap, HashSet};

use qb64fresh::ast::{Expr, ExprKind, Program, Span, Statement, StatementKind};

use super::{collect_all_statements, LintDiagnostic, LintRule, RuleInfo};
use crate::config::{LintCategory, LintConfig, Severity};

/// Detects variables that are declared but never read.
///
/// # Example
///
/// ```basic
/// DIM unused AS INTEGER  ' Warning: unused variable
/// unused = 42            ' Assignment doesn't count as "use"
/// PRINT "hello"
/// ```
pub struct UnusedVariableRule;

impl LintRule for UnusedVariableRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "unused_variable",
            category: LintCategory::Correctness,
            default_severity: Severity::Warning,
            description: "Detects variables that are declared but never read",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();

        // Track declared variables and their spans
        let mut declared: HashMap<String, Span> = HashMap::new();
        // Track variables that are read (not just assigned)
        let mut used: HashSet<String> = HashSet::new();

        // First pass: collect all declared variables
        for stmt in collect_all_statements(program) {
            match &stmt.kind {
                StatementKind::Dim { variables, .. } => {
                    for var in variables {
                        declared.insert(var.name.to_uppercase(), stmt.span);
                    }
                }
                StatementKind::For { variable, .. } => {
                    // FOR loop variable is implicitly declared
                    declared.insert(variable.to_uppercase(), stmt.span);
                }
                StatementKind::SubDefinition { params, .. }
                | StatementKind::FunctionDefinition { params, .. } => {
                    // Parameters are declared variables
                    for param in params {
                        declared.insert(param.name.to_uppercase(), stmt.span);
                    }
                }
                _ => {}
            }
        }

        // Second pass: collect all variable reads
        for stmt in collect_all_statements(program) {
            collect_used_variables_in_statement(stmt, &mut used);
        }

        // Report unused variables
        for (name, span) in &declared {
            // Skip loop variables and common patterns like "i", "j", "k"
            let lower = name.to_lowercase();
            if lower == "i" || lower == "j" || lower == "k" || lower == "n" {
                continue;
            }

            // Skip variables with underscore prefix (intentionally unused)
            if name.starts_with('_') {
                continue;
            }

            if !used.contains(name) {
                diagnostics.push(
                    LintDiagnostic::new(
                        info.name,
                        info.category,
                        severity,
                        format!("Variable '{}' is declared but never used", name),
                        *span,
                    )
                    .with_suggestion("Remove the variable or use it, or prefix with '_' to suppress this warning")
                );
            }
        }

        diagnostics
    }
}

/// Collects all variable names that are read (not just assigned) in a statement.
fn collect_used_variables_in_statement(stmt: &Statement, used: &mut HashSet<String>) {
    match &stmt.kind {
        // Assignments: the RHS is a use, but the LHS is not
        StatementKind::Let { value, .. } => {
            collect_used_variables_in_expr(value, used);
        }
        StatementKind::ArrayAssignment { indices, value, .. } => {
            for idx in indices {
                collect_used_variables_in_expr(idx, used);
            }
            collect_used_variables_in_expr(value, used);
        }
        StatementKind::ArrayFieldAssignment { indices, value, .. } => {
            for idx in indices {
                collect_used_variables_in_expr(idx, used);
            }
            collect_used_variables_in_expr(value, used);
        }

        // Control flow: condition expressions are uses
        StatementKind::If { condition, .. } | StatementKind::While { condition, .. } => {
            collect_used_variables_in_expr(condition, used);
        }

        StatementKind::DoLoop {
            pre_condition,
            post_condition,
            ..
        } => {
            if let Some(cond) = pre_condition {
                collect_used_variables_in_expr(&cond.condition, used);
            }
            if let Some(cond) = post_condition {
                collect_used_variables_in_expr(&cond.condition, used);
            }
        }

        // FOR loop: start, end, step are uses; loop variable is also read in comparisons
        StatementKind::For {
            variable,
            start,
            end,
            step,
            ..
        } => {
            used.insert(variable.to_uppercase()); // Loop variable IS used
            collect_used_variables_in_expr(start, used);
            collect_used_variables_in_expr(end, used);
            if let Some(s) = step {
                collect_used_variables_in_expr(s, used);
            }
        }

        // PRINT: all expressions are uses
        StatementKind::Print { values, .. } => {
            for item in values {
                collect_used_variables_in_expr(&item.expr, used);
            }
        }
        StatementKind::PrintUsing { format, values, .. } => {
            collect_used_variables_in_expr(format, used);
            for expr in values {
                collect_used_variables_in_expr(expr, used);
            }
        }

        // SELECT CASE
        StatementKind::SelectCase { test_expr, .. }
        | StatementKind::SelectEveryCase { test_expr, .. } => {
            collect_used_variables_in_expr(test_expr, used);
        }

        // Function/Sub calls
        StatementKind::Call { args, .. } => {
            for arg in args {
                collect_used_variables_in_expr(arg, used);
            }
        }

        // Expression statement
        StatementKind::Expression(expr) => {
            collect_used_variables_in_expr(expr, used);
        }

        // SWAP: both sides are reads and writes
        StatementKind::Swap { left, right } => {
            collect_used_variables_in_expr(left, used);
            collect_used_variables_in_expr(right, used);
        }

        // INPUT: targets get written, but prompt may be a variable
        StatementKind::Input { .. } | StatementKind::LineInput { .. } => {
            // Targets are writes, not reads
        }

        // Many other statement types...
        _ => {}
    }
}

/// Collects all variable names referenced in an expression.
fn collect_used_variables_in_expr(expr: &Expr, used: &mut HashSet<String>) {
    match &expr.kind {
        ExprKind::Identifier(name) => {
            used.insert(name.to_uppercase());
        }
        ExprKind::FunctionCall { name, args } => {
            // FunctionCall is also used for array access in BASIC
            // The name could be a variable (array) name
            used.insert(name.to_uppercase());
            for arg in args {
                collect_used_variables_in_expr(arg, used);
            }
        }
        ExprKind::FieldAccess { object, .. } => {
            collect_used_variables_in_expr(object, used);
        }
        ExprKind::Binary { left, right, .. } => {
            collect_used_variables_in_expr(left, used);
            collect_used_variables_in_expr(right, used);
        }
        ExprKind::Unary { operand, .. } => {
            collect_used_variables_in_expr(operand, used);
        }
        ExprKind::Grouped(inner) => {
            collect_used_variables_in_expr(inner, used);
        }
        ExprKind::CvFunc { value, .. }
        | ExprKind::MkDollarFunc { value, .. }
        | ExprKind::CastFunc { value, .. }
        | ExprKind::ValWithType { value, .. } => {
            collect_used_variables_in_expr(value, used);
        }
        ExprKind::MemGetTyped { mem, offset, .. } => {
            collect_used_variables_in_expr(mem, used);
            collect_used_variables_in_expr(offset, used);
        }
        // Literals don't contain variable references
        ExprKind::IntegerLiteral(_)
        | ExprKind::FloatLiteral(_)
        | ExprKind::StringLiteral(_)
        | ExprKind::ProcPtr { .. } => {}
    }
}

/// Detects labels that are defined but never referenced.
///
/// # Example
///
/// ```basic
/// unused_label:  ' Warning: label never used
/// PRINT "hello"
/// ```
pub struct UnusedLabelRule;

impl LintRule for UnusedLabelRule {
    fn info(&self) -> RuleInfo {
        RuleInfo {
            name: "unused_label",
            category: LintCategory::Correctness,
            default_severity: Severity::Warning,
            description: "Detects labels that are defined but never referenced by GOTO/GOSUB",
        }
    }

    fn check(&self, program: &Program, _source: &str, config: &LintConfig) -> Vec<LintDiagnostic> {
        let info = self.info();
        let severity = config.severity_for(info.name, info.category);
        if severity == Severity::Off {
            return vec![];
        }

        let mut diagnostics = Vec::new();

        // Track defined labels and their spans
        let mut defined: HashMap<String, Span> = HashMap::new();
        // Track referenced labels
        let mut referenced: HashSet<String> = HashSet::new();

        for stmt in collect_all_statements(program) {
            match &stmt.kind {
                StatementKind::Label { name } => {
                    defined.insert(name.to_uppercase(), stmt.span);
                }
                StatementKind::Goto { target } | StatementKind::Gosub { target } => {
                    referenced.insert(target.to_uppercase());
                }
                StatementKind::Restore { label: Some(label) } => {
                    referenced.insert(label.to_uppercase());
                }
                _ => {}
            }
        }

        // Report unused labels
        for (name, span) in &defined {
            if !referenced.contains(name) {
                diagnostics.push(
                    LintDiagnostic::new(
                        info.name,
                        info.category,
                        severity,
                        format!("Label '{}' is defined but never used", name),
                        *span,
                    )
                    .with_suggestion("Remove the unused label"),
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
    fn test_unused_variable_detected() {
        let source = r#"
DIM unused AS INTEGER
PRINT "hello"
"#;
        let diagnostics = parse_and_check(&UnusedVariableRule, source);
        assert!(!diagnostics.is_empty(), "Should detect unused variable");
        assert!(diagnostics[0].message.contains("UNUSED"));
    }

    #[test]
    fn test_used_variable_not_flagged() {
        let source = r#"
DIM x AS INTEGER
x = 5
PRINT x
"#;
        let diagnostics = parse_and_check(&UnusedVariableRule, source);
        // x is used in PRINT, so should not be flagged
        let has_x_unused = diagnostics.iter().any(|d| d.message.contains("'X'"));
        assert!(!has_x_unused, "Used variable should not be flagged");
    }

    #[test]
    fn test_unused_label_detected() {
        let source = r#"
unused_label:
PRINT "hello"
"#;
        let diagnostics = parse_and_check(&UnusedLabelRule, source);
        assert!(!diagnostics.is_empty(), "Should detect unused label");
    }

    #[test]
    fn test_used_label_not_flagged() {
        let source = r#"
start:
PRINT "hello"
GOTO start
"#;
        let diagnostics = parse_and_check(&UnusedLabelRule, source);
        let has_start_unused = diagnostics
            .iter()
            .any(|d| d.message.to_uppercase().contains("START"));
        assert!(!has_start_unused, "Used label should not be flagged");
    }
}
