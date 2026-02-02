//! Special expression code generation.
//!
//! Handles type conversions (Convert, CvFunc, MkDollarFunc, CastFunc),
//! ValWithType, and MemGetTyped.

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedExpr;
use crate::semantic::types::BasicType;

use super::super::types::c_type;
use super::ExprEmitCtx;

/// Emits C code for a Convert expression (type cast).
pub(super) fn emit_convert(
    ctx: &ExprEmitCtx,
    expr: &TypedExpr,
    to_type: &BasicType,
    wrap_string_temps: bool,
) -> Result<String, CodeGenError> {
    let inner_code = ctx.emit(expr, wrap_string_temps)?;
    if expr.basic_type.is_string() && to_type.is_string() {
        return Ok(inner_code);
    }
    if matches!(to_type, BasicType::FixedString(_)) {
        return Ok(inner_code);
    }
    let c_ty = c_type(to_type);
    Ok(format!("(({})({}))", c_ty, inner_code))
}

/// Emits C code for CvFunc (CVI, CVL, CVS, CVD, CVQ).
pub(super) fn emit_cv_func(
    ctx: &ExprEmitCtx,
    value: &TypedExpr,
    target_type: &BasicType,
) -> Result<String, CodeGenError> {
    let value_code = ctx.emit(value, false)?;
    let func = match target_type {
        BasicType::Integer => "qb_cvi",
        BasicType::Long => "qb_cvl",
        BasicType::Single => "qb_cvs",
        BasicType::Double => "qb_cvd",
        BasicType::Integer64 => "qb_cvq",
        _ => "qb_cvi",
    };
    Ok(format!("{}({})", func, value_code))
}

/// Emits C code for MkDollarFunc (MKI$, MKL$, MKS$, MKD$, MKQ$).
pub(super) fn emit_mk_dollar_func(
    ctx: &ExprEmitCtx,
    value: &TypedExpr,
    source_type: &BasicType,
) -> Result<String, CodeGenError> {
    let value_code = ctx.emit(value, false)?;
    let func = match source_type {
        BasicType::Integer => "qb_mki",
        BasicType::Long => "qb_mkl",
        BasicType::Single => "qb_mks",
        BasicType::Double => "qb_mkd",
        BasicType::Integer64 => "qb_mkq",
        _ => "qb_mki",
    };
    Ok(format!("{}({})", func, value_code))
}

/// Emits C code for CastFunc (explicit cast).
pub(super) fn emit_cast_func(
    ctx: &ExprEmitCtx,
    value: &TypedExpr,
    target_type: &BasicType,
) -> Result<String, CodeGenError> {
    let value_code = ctx.emit(value, false)?;
    let c_ty = c_type(target_type);
    Ok(format!("(({})({})", c_ty, value_code))
}

/// Emits C code for ValWithType (VAL with type specifier).
pub(super) fn emit_val_with_type(
    ctx: &ExprEmitCtx,
    value: &TypedExpr,
    target_type: &BasicType,
) -> Result<String, CodeGenError> {
    let value_code = ctx.emit(value, false)?;
    let func = match target_type {
        BasicType::Integer64 => "qb_val_int64",
        BasicType::UnsignedInteger64 => "qb_val_uint64",
        BasicType::Long => "qb_val_long",
        BasicType::UnsignedLong => "qb_val_ulong",
        BasicType::Integer => "qb_val_int",
        BasicType::UnsignedInteger => "qb_val_uint",
        BasicType::Double => "qb_val_double",
        BasicType::Single => "qb_val_float",
        _ => "qb_val",
    };
    Ok(format!("{}({})", func, value_code))
}

/// Emits C code for MemGetTyped (_MEMGET).
pub(super) fn emit_mem_get_typed(
    ctx: &ExprEmitCtx,
    mem: &TypedExpr,
    offset: &TypedExpr,
    target_type: &BasicType,
) -> Result<String, CodeGenError> {
    let mem_code = ctx.emit(mem, false)?;
    let offset_code = ctx.emit(offset, false)?;
    let c_ty = c_type(target_type);
    Ok(format!(
        "(*(({c_ty}*)((char*)({mem_code}).offset + ({offset_code}))))"
    ))
}
