//! Definition statement type checking dispatch.
//!
//! This module provides a dispatch function for definition statements
//! that delegates to methods in the parent definitions module.

use crate::ast::StatementKind;
use crate::semantic::typed_ir::TypedStatement;

use super::super::super::TypeChecker;

/// Type checks definition-related statements.
pub(super) fn check_definitions_stmt(
    checker: &mut TypeChecker,
    kind: &StatementKind,
    span: crate::ast::Span,
) -> TypedStatement {
    match kind {
        StatementKind::Call { name, args } => checker.check_call(name, args, span),

        StatementKind::Dim { variables, shared } => checker.check_dim(variables, *shared, span),

        StatementKind::Const { definitions } => checker.check_const(definitions, span),

        StatementKind::DefType { type_kind, ranges } => {
            checker.check_deftype(type_kind, ranges, span)
        }

        StatementKind::Define { type_spec, ranges } => {
            checker.check_define(type_spec, ranges, span)
        }

        StatementKind::OptionBase { base } => checker.check_option_base(*base, span),

        StatementKind::OptionExplicit => {
            checker.symbols.set_explicit_mode(true);
            TypedStatement::new(
                crate::semantic::typed_ir::TypedStatementKind::OptionExplicit,
                span,
            )
        }

        StatementKind::OptionExplicitArray => {
            checker.symbols.set_explicit_array_mode(true);
            TypedStatement::new(
                crate::semantic::typed_ir::TypedStatementKind::OptionExplicitArray,
                span,
            )
        }

        StatementKind::Label { name } => TypedStatement::new(
            crate::semantic::typed_ir::TypedStatementKind::Label { name: name.clone() },
            span,
        ),

        StatementKind::SubDefinition {
            name,
            params,
            body,
            is_static,
        } => checker.check_sub_definition(name, params, body, *is_static, span),

        StatementKind::FunctionDefinition {
            name,
            params,
            return_type,
            body,
            is_static,
        } => checker.check_function_definition(name, params, return_type, body, *is_static, span),

        _ => unreachable!("Not a definition statement"),
    }
}
