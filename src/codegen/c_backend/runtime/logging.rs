//! Scoped logging for inline C runtime (libqb logging.h compatibility).
//!
//! Emits C code for libqb_log_init, libqb_log (variadic), libqb_log_qb64,
//! libqb_log_qbs, and macros libqb_log_with_scope_* and libqb_log_trace/info/warn/error.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits C code for scoped logging (loglevel, logscope, init, log, log_qbs, macros).
pub(super) fn emit_logging(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(
        output,
        "/* Scoped Logging (libqb logging.h compatibility) */"
    )?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "/* loglevel: 0=Trace, 1=Information, 2=Warning, 3=Error */"
    )?;
    writeln_code!(output, "#define QB_LOGLEVEL_TRACE        0")?;
    writeln_code!(output, "#define QB_LOGLEVEL_INFORMATION  1")?;
    writeln_code!(output, "#define QB_LOGLEVEL_WARNING      2")?;
    writeln_code!(output, "#define QB_LOGLEVEL_ERROR        3")?;
    writeln_code!(
        output,
        "/* logscope: 0=Runtime, 1=QB64, 2=Libqb, 3=Audio, 4=Image */"
    )?;
    writeln_code!(output, "#define QB_LOGSCOPE_RUNTIME      0")?;
    writeln_code!(output, "#define QB_LOGSCOPE_QB64         1")?;
    writeln_code!(output, "#define QB_LOGSCOPE_LIBQB        2")?;
    writeln_code!(output, "#define QB_LOGSCOPE_AUDIO        3")?;
    writeln_code!(output, "#define QB_LOGSCOPE_IMAGE        4")?;
    writeln_code!(output)?;

    writeln_code!(output, "static int _qb_log_min_level = 0;")?;
    writeln_code!(output)?;

    writeln_code!(output, "void libqb_log_init(void) {{")?;
    writeln_code!(output, "    _qb_log_min_level = 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "static void _qb_log_write(int lvl, int scope, const char* file, const char* func, int line, const char* msg) {{"
    )?;
    writeln_code!(output, "    if (lvl < _qb_log_min_level) return;")?;
    writeln_code!(
        output,
        "    static const char* level_names[] = {{ \"TRACE\", \"INFO\", \"WARN\", \"ERROR\" }};"
    )?;
    writeln_code!(
        output,
        "    static const char* scope_names[] = {{ \"Runtime\", \"QB64\", \"Libqb\", \"Audio\", \"Image\" }};"
    )?;
    writeln_code!(output, "    int li = (lvl >= 0 && lvl <= 3) ? lvl : 1;")?;
    writeln_code!(
        output,
        "    int si = (scope >= 0 && scope <= 4) ? scope : 2;"
    )?;
    writeln_code!(
        output,
        "    fprintf(stderr, \"[%.3f] %s [%s] %s:%d in %s %s\\n\","
    )?;
    writeln_code!(
        output,
        "            0.0, level_names[li], scope_names[si], file ? file : \"?\","
    )?;
    writeln_code!(
        output,
        "            line, func ? func : \"?\", msg ? msg : \"\");"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void libqb_log(int lvl, int scope, const char* file, const char* func, int line, const char* fmt, ...) {{"
    )?;
    writeln_code!(output, "    char buf[1024];")?;
    writeln_code!(output, "    va_list ap;")?;
    writeln_code!(output, "    va_start(ap, fmt);")?;
    writeln_code!(
        output,
        "    vsnprintf(buf, sizeof(buf), fmt ? fmt : \"\", ap);"
    )?;
    writeln_code!(output, "    va_end(ap);")?;
    writeln_code!(
        output,
        "    _qb_log_write(lvl, scope, file, func, line, buf);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void libqb_log_qb64(int lvl, int scope, const char* file, const char* func, int line, const char* fmt, ...) {{"
    )?;
    writeln_code!(output, "    char buf[1024];")?;
    writeln_code!(output, "    va_list ap;")?;
    writeln_code!(output, "    va_start(ap, fmt);")?;
    writeln_code!(
        output,
        "    vsnprintf(buf, sizeof(buf), fmt ? fmt : \"\", ap);"
    )?;
    writeln_code!(output, "    va_end(ap);")?;
    writeln_code!(
        output,
        "    _qb_log_write(lvl, scope, file, func, line, buf);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void libqb_log_qbs(int lvl, int scope, const char* file, const char* func, int line, qb_string* str) {{"
    )?;
    writeln_code!(output, "    if (!str) return;")?;
    writeln_code!(output, "    const char* msg = qb_string_data(str);")?;
    writeln_code!(
        output,
        "    _qb_log_write(lvl, scope, file, func, line, msg ? msg : \"\");"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "#define libqb_log_with_scope_trace(scope, fmt, ...) \\"
    )?;
    writeln_code!(
        output,
        "    libqb_log(QB_LOGLEVEL_TRACE, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)"
    )?;
    writeln_code!(
        output,
        "#define libqb_log_with_scope_info(scope, fmt, ...) \\"
    )?;
    writeln_code!(
        output,
        "    libqb_log(QB_LOGLEVEL_INFORMATION, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)"
    )?;
    writeln_code!(
        output,
        "#define libqb_log_with_scope_warn(scope, fmt, ...) \\"
    )?;
    writeln_code!(
        output,
        "    libqb_log(QB_LOGLEVEL_WARNING, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)"
    )?;
    writeln_code!(
        output,
        "#define libqb_log_with_scope_error(scope, fmt, ...) \\"
    )?;
    writeln_code!(
        output,
        "    libqb_log(QB_LOGLEVEL_ERROR, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)"
    )?;
    writeln_code!(output)?;

    writeln_code!(output, "#define libqb_log_trace(...) \\")?;
    writeln_code!(
        output,
        "    libqb_log_with_scope_trace(QB_LOGSCOPE_LIBQB, __VA_ARGS__)"
    )?;
    writeln_code!(output, "#define libqb_log_info(...) \\")?;
    writeln_code!(
        output,
        "    libqb_log_with_scope_info(QB_LOGSCOPE_LIBQB, __VA_ARGS__)"
    )?;
    writeln_code!(output, "#define libqb_log_warn(...) \\")?;
    writeln_code!(
        output,
        "    libqb_log_with_scope_warn(QB_LOGSCOPE_LIBQB, __VA_ARGS__)"
    )?;
    writeln_code!(output, "#define libqb_log_error(...) \\")?;
    writeln_code!(
        output,
        "    libqb_log_with_scope_error(QB_LOGSCOPE_LIBQB, __VA_ARGS__)"
    )?;
    writeln_code!(output)?;

    Ok(())
}
