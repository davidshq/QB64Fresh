//! Assignment statement type checking dispatch.
//!
//! This module provides a dispatch function for assignment-related statements
//! that delegates to methods in the parent assignments module.

use crate::ast::StatementKind;
use crate::semantic::typed_ir::TypedStatement;

use super::super::super::TypeChecker;

/// Type checks assignment-related statements.
pub(super) fn check_assignments_stmt(
    checker: &mut TypeChecker,
    kind: &StatementKind,
    span: crate::ast::Span,
) -> TypedStatement {
    match kind {
        StatementKind::Let { name, value } => checker.check_assignment(name, value, span),

        StatementKind::ArrayAssignment {
            name,
            indices,
            value,
        } => checker.check_array_assignment(name, indices, value, span),

        StatementKind::FieldAssignment {
            name,
            fields,
            value,
        } => checker.check_field_assignment(name, fields, value, span),

        StatementKind::ArrayFieldAssignment {
            name,
            indices,
            fields,
            value,
        } => checker.check_array_field_assignment(name, indices, fields, value, span),

        StatementKind::MidAssignment {
            target,
            start,
            length,
            value,
        } => checker.check_mid_assignment(target, start, length.as_ref(), value, span),

        StatementKind::AscAssignment {
            target,
            position,
            value,
        } => checker.check_asc_assignment(target, position, value, span),

        _ => unreachable!("Not an assignment statement"),
    }
}
