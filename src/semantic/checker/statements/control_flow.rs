//! Control flow statement type checking dispatch.
//!
//! This module provides a dispatch function for control flow statements
//! that delegates to methods in the parent control_flow module.

use crate::ast::StatementKind;
use crate::semantic::typed_ir::TypedStatement;

use super::super::super::TypeChecker;
use crate::semantic::checker::ForLoopInfo;

/// Type checks control flow-related statements.
pub(super) fn check_control_flow_stmt(
    checker: &mut TypeChecker,
    kind: &StatementKind,
    span: crate::ast::Span,
) -> TypedStatement {
    match kind {
        StatementKind::If {
            condition,
            then_branch,
            elseif_branches,
            else_branch,
        } => checker.check_if(condition, then_branch, elseif_branches, else_branch, span),

        StatementKind::SelectCase {
            test_expr,
            cases,
            case_else,
        } => checker.check_select_case(test_expr, cases, case_else, false, span),

        StatementKind::SelectEveryCase {
            test_expr,
            cases,
            case_else,
        } => checker.check_select_case(test_expr, cases, case_else, true, span),

        StatementKind::For {
            variable,
            start,
            end,
            step,
            body,
            next_variable,
        } => checker.check_for(ForLoopInfo {
            variable,
            start,
            end,
            step,
            body,
            next_variable,
            span,
        }),

        StatementKind::While { condition, body } => checker.check_while(condition, body, span),

        StatementKind::DoLoop {
            pre_condition,
            body,
            post_condition,
        } => checker.check_do_loop(pre_condition, body, post_condition, span),

        StatementKind::Goto { target } => checker.check_goto(target, span),

        StatementKind::Gosub { target } => checker.check_gosub(target, span),

        StatementKind::Return => checker.check_return(span),

        StatementKind::Exit { exit_type } => checker.check_exit(*exit_type, span),

        StatementKind::End { exit_code } => {
            let typed_exit_code = exit_code.as_ref().map(|e| checker.check_expr(e));
            TypedStatement::new(
                crate::semantic::typed_ir::TypedStatementKind::End {
                    exit_code: typed_exit_code,
                },
                span,
            )
        }

        StatementKind::Stop => {
            TypedStatement::new(crate::semantic::typed_ir::TypedStatementKind::Stop, span)
        }

        StatementKind::Continue { continue_type } => TypedStatement::new(
            crate::semantic::typed_ir::TypedStatementKind::Continue {
                continue_type: *continue_type,
            },
            span,
        ),

        _ => unreachable!("Not a control flow statement"),
    }
}
