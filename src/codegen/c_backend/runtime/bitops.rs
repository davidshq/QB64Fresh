//! Inline C runtime for bit operations (libqb bitops.h).
//!
//! Emits C implementations of qb_getubits, qb_getbits, qb_setbits for
//! inline runtime mode. Logic matches QB64pe libqb bitops.cpp.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits inline C definitions for bit field get/set functions.
///
/// These are used by TYPE bit-field access and DECLARE LIBRARY code.
pub(super) fn emit_bitops(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Bit operations (libqb bitops.h) */")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "uint64_t qb_getubits(uint32_t bsize, uint8_t *base, intptr_t i) {{"
    )?;
    writeln_code!(
        output,
        "    uint64_t bmask = ~((uint64_t)(-(int64_t)((uint64_t)1 << bsize)));"
    )?;
    writeln_code!(output, "    i *= (intptr_t)bsize;")?;
    writeln_code!(
        output,
        "    return ((*(uint64_t *)(base + (i >> 3))) >> (i & 7)) & bmask;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "int64_t qb_getbits(uint32_t bsize, uint8_t *base, intptr_t i) {{"
    )?;
    writeln_code!(output, "    int64_t bmask = ~(-((int64_t)1 << bsize));")?;
    writeln_code!(output, "    int64_t bval64;")?;
    writeln_code!(output, "    i *= (intptr_t)bsize;")?;
    writeln_code!(
        output,
        "    bval64 = (int64_t)(((*(uint64_t *)(base + (i >> 3))) >> (i & 7)) & (uint64_t)bmask);"
    )?;
    writeln_code!(output, "    if (bval64 & ((int64_t)1 << (bsize - 1)))")?;
    writeln_code!(output, "        return bval64 | ~bmask;")?;
    writeln_code!(output, "    return bval64;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    writeln_code!(
        output,
        "void qb_setbits(uint32_t bsize, uint8_t *base, intptr_t i, int64_t val) {{"
    )?;
    writeln_code!(output, "    uint64_t bmask = ((uint64_t)1 << bsize) - 1;")?;
    writeln_code!(output, "    uint64_t *bptr64;")?;
    writeln_code!(output, "    i *= (intptr_t)bsize;")?;
    writeln_code!(output, "    bptr64 = (uint64_t *)(base + (i >> 3));")?;
    writeln_code!(
        output,
        "    *bptr64 = (*bptr64 & ~((bmask << (i & 7)))) | ((uint64_t)(val & (int64_t)bmask) << (i & 7));"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
