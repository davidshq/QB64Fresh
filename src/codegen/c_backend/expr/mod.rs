//! Expression code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for all expression types,
//! including literals, binary/unary operations, function calls, and array access.
//!
//! # Module organization
//!
//! - `mod.rs` (this file) — Thin dispatcher: match on `TypedExprKind`, delegate to submodules
//! - `literals.rs` — Integer, float, string literals and variable references
//! - `binary.rs` — Binary ops (arithmetic, comparison, string concat)
//! - `unary.rs` — Unary ops and grouped expressions
//! - `calls.rs` — Function calls, array access, field access, external calls, proc ptr
//! - `special.rs` — Convert, CvFunc, MkDollarFunc, CastFunc, ValWithType, MemGetTyped
//! - `helpers.rs` — escape_string, c_function_name, unwrap_qb_str_from_c, etc.

mod binary;
mod calls;
mod helpers;
mod literals;
mod special;
mod unary;

use std::collections::{HashMap, HashSet};

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedExpr, TypedExprKind};
use crate::semantic::types::BasicType;

use super::const_fold::{emit_folded, try_fold};

// Re-export public API for use by stmt, analysis, etc.
pub(super) use helpers::{c_function_name, emit_string_data_access, escape_string};
pub(crate) use helpers::unwrap_qb_str_from_c;

/// Context for emitting expressions, passed to submodules for recursive emit.
pub(super) struct ExprEmitCtx<'a> {
    pub no_shell: bool,
    pub variable_renames: &'a HashMap<String, String>,
    pub param_names: &'a HashSet<String>,
    pub byref_scalar_names: &'a HashSet<String>,
    pub byref_udt_names: &'a HashSet<String>,
    pub byref_string_names: &'a [String],
    pub byref_string_basic_names: &'a [String],
    pub dynamic_external_c_names: &'a HashSet<String>,
}

impl<'a> ExprEmitCtx<'a> {
    /// Emits C code for an expression (internal recursive entry).
    pub(super) fn emit(
        &self,
        expr: &TypedExpr,
        wrap_string_temps: bool,
    ) -> Result<String, CodeGenError> {
        if matches!(
            &expr.kind,
            TypedExprKind::Binary { .. }
                | TypedExprKind::Unary { .. }
                | TypedExprKind::FunctionCall { .. }
                | TypedExprKind::Grouped(_)
        ) && let Some(folded) = try_fold(expr)
        {
            return Ok(emit_folded(&folded));
        }

        match &expr.kind {
            TypedExprKind::IntegerLiteral(n) => Ok(literals::emit_integer_literal(*n)),

            TypedExprKind::FloatLiteral(n) => Ok(literals::emit_float_literal(*n)),

            TypedExprKind::StringLiteral(s) => Ok(literals::emit_string_literal(s, wrap_string_temps)),

            TypedExprKind::Variable(name) => literals::emit_variable(
                name,
                &expr.basic_type,
                self.variable_renames,
                self.param_names,
                self.byref_scalar_names,
                self.byref_string_names,
                self.byref_string_basic_names,
            ),

            TypedExprKind::Binary { left, op, right } => {
                binary::emit_binary_expr(self, left, op, right, wrap_string_temps)
            }

            TypedExprKind::Unary { op, operand } => {
                let operand_code = self.emit(operand, wrap_string_temps)?;
                Ok(unary::format_unary(op, &operand_code))
            }

            TypedExprKind::Grouped(inner) => {
                let inner_code = self.emit(inner, wrap_string_temps)?;
                Ok(unary::format_grouped(&inner_code))
            }

            TypedExprKind::FunctionCall { name, args, params } => {
                calls::emit_function_call(self, expr, name, args, params, wrap_string_temps)
            }

            TypedExprKind::ArrayAccess {
                name,
                indices,
                dimensions,
            } => {
                let array_code =
                    calls::emit_array_access(self, name, indices, dimensions)?;
                if matches!(expr.basic_type, BasicType::FixedString(_)) {
                    Ok(format!("qb_str_from_c({})", array_code))
                } else {
                    Ok(array_code)
                }
            }

            TypedExprKind::Convert { expr: inner, to_type } => {
                special::emit_convert(self, inner, to_type, wrap_string_temps)
            }

            TypedExprKind::FieldAccess { object, field } => {
                calls::emit_field_access(self, object, field, &expr.basic_type)
            }

            TypedExprKind::ArrayRef { name, .. } => Ok(calls::emit_array_ref(name)),

            TypedExprKind::ExternalFunctionCall {
                c_name,
                args,
                params,
                ..
            } => {
                let call_name = if self.dynamic_external_c_names.contains(c_name) {
                    format!("qb_dyn_{}", c_name)
                } else {
                    c_name.clone()
                };
                calls::emit_external_function_call(self, &call_name, args, params)
            }

            TypedExprKind::ProcPtr { wrapper_name, .. } => {
                Ok(calls::emit_proc_ptr(wrapper_name))
            }

            TypedExprKind::CvFunc { target_type, value } => {
                special::emit_cv_func(self, value, target_type)
            }

            TypedExprKind::MkDollarFunc { source_type, value } => {
                special::emit_mk_dollar_func(self, value, source_type)
            }

            TypedExprKind::CastFunc { target_type, value } => {
                special::emit_cast_func(self, value, target_type)
            }

            TypedExprKind::ValWithType { value, target_type } => {
                special::emit_val_with_type(self, value, target_type)
            }

            TypedExprKind::MemGetTyped {
                mem,
                offset,
                target_type,
            } => special::emit_mem_get_typed(self, mem, offset, target_type),
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_expr(
    expr: &TypedExpr,
    no_shell: bool,
    variable_renames: &HashMap<String, String>,
    param_names: &HashSet<String>,
    byref_scalar_names: &HashSet<String>,
    byref_udt_names: &HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &HashSet<String>,
) -> Result<String, CodeGenError> {
    let ctx = ExprEmitCtx {
        no_shell,
        variable_renames,
        param_names,
        byref_scalar_names,
        byref_udt_names,
        byref_string_names,
        byref_string_basic_names,
        dynamic_external_c_names,
    };
    ctx.emit(expr, false)
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_expr_external(
    expr: &TypedExpr,
    no_shell: bool,
    variable_renames: &HashMap<String, String>,
    param_names: &HashSet<String>,
    byref_scalar_names: &HashSet<String>,
    byref_udt_names: &HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &HashSet<String>,
) -> Result<String, CodeGenError> {
    let ctx = ExprEmitCtx {
        no_shell,
        variable_renames,
        param_names,
        byref_scalar_names,
        byref_udt_names,
        byref_string_names,
        byref_string_basic_names,
        dynamic_external_c_names,
    };
    ctx.emit(expr, true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, Span};
    use crate::semantic::typed_ir::{TypedExpr, TypedExprKind};
    use crate::semantic::types::BasicType;
    use std::collections::{HashMap, HashSet};

    #[test]
    fn test_emit_integer_literal() {
        let expr = TypedExpr::integer(42, Span::new(0, 2, 1));
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting integer literal should succeed");
        assert_eq!(result, "42LL");
    }

    #[test]
    fn test_emit_string_literal() {
        let expr = TypedExpr::string("Hello".to_string(), Span::new(0, 7, 1));
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting string literal should succeed");
        assert_eq!(result, "qb_string_new(\"Hello\")");
    }

    #[test]
    fn test_emit_power_operator_constant_folded() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(2, Span::new(0, 1, 1))),
                op: BinaryOp::Power,
                right: Box::new(TypedExpr::integer(3, Span::new(4, 5, 1))),
            },
            BasicType::Double,
            Span::new(0, 5, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting constant-folded power operator should succeed");
        assert_eq!(result, "8LL");
    }

    #[test]
    fn test_emit_power_operator_with_variable() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::new(
                    TypedExprKind::Variable("x".to_string()),
                    BasicType::Long,
                    Span::new(0, 1, 1),
                )),
                op: BinaryOp::Power,
                right: Box::new(TypedExpr::integer(3, Span::new(4, 5, 1))),
            },
            BasicType::Double,
            Span::new(0, 5, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting power operator with variable should succeed");
        assert_eq!(result, "pow(x, 3LL)");
    }

    #[test]
    fn test_emit_eqv_operator_constant_folded() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1, 1))),
                op: BinaryOp::Eqv,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting constant-folded EQV operator should succeed");
        assert_eq!(result, "-7LL");
    }

    #[test]
    fn test_emit_eqv_operator_with_variable() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::new(
                    TypedExprKind::Variable("x".to_string()),
                    BasicType::Long,
                    Span::new(0, 1, 1),
                )),
                op: BinaryOp::Eqv,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting EQV operator with variable should succeed");
        assert_eq!(result, "(~(x ^ 3LL))");
    }

    #[test]
    fn test_emit_imp_operator_constant_folded() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1, 1))),
                op: BinaryOp::Imp,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting constant-folded IMP operator should succeed");
        assert_eq!(result, "-5LL");
    }

    #[test]
    fn test_emit_imp_operator_with_variable() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::new(
                    TypedExprKind::Variable("x".to_string()),
                    BasicType::Long,
                    Span::new(0, 1, 1),
                )),
                op: BinaryOp::Imp,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting IMP operator with variable should succeed");
        assert_eq!(result, "((~x) | 3LL)");
    }

    #[test]
    fn test_escape_string() {
        assert_eq!(escape_string("Hello"), "Hello");
        assert_eq!(escape_string("Say \"Hi\""), "Say \\\"Hi\\\"");
        assert_eq!(escape_string("path\\file"), "path\\\\file");
        assert_eq!(escape_string("line1\nline2"), "line1\\nline2");
        assert_eq!(escape_string("tab\there"), "tab\\there");
        assert_eq!(escape_string("\x00\x1f"), "\\x00\\x1f");
        assert_eq!(escape_string("é"), "\\xc3\\xa9");
        assert_eq!(escape_string("😀"), "\\xf0\\x9f\\x98\\x80");
    }

    #[test]
    fn test_c_function_name() {
        assert_eq!(c_function_name("SIN"), "sin");
        assert_eq!(c_function_name("ABS"), "fabs");
        assert_eq!(c_function_name("LEN"), "qb_len");
        assert_eq!(c_function_name("CHR$"), "qb_chr");
        assert_eq!(c_function_name("myFunc"), "qb_myfunc");
    }

    #[test]
    fn test_unwrap_qb_str_from_c() {
        assert_eq!(unwrap_qb_str_from_c("x"), "x");
        assert_eq!(unwrap_qb_str_from_c("qb_str_from_c(buf)"), "buf");
        assert_eq!(unwrap_qb_str_from_c("qb_str_from_c(foo(1))"), "foo(1)");
        // Malformed: no closing paren - return inner slice up to end
        assert_eq!(unwrap_qb_str_from_c("qb_str_from_c(unclosed"), "unclosed");
    }
}
