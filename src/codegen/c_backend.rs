//! C code generation backend for QB64Fresh.
//!
//! This module generates portable C99 code from the typed intermediate representation.
//! The generated code uses `<stdint.h>` for predictable type sizes and a minimal
//! runtime library for I/O operations.
//!
//! # Generated Code Structure
//!
//! ```c
//! // Includes and runtime declarations
//! #include <stdio.h>
//! #include <stdint.h>
//! #include "qb_runtime.h"
//!
//! // Global variables
//! int32_t my_global;
//!
//! // Forward declarations for SUBs/FUNCTIONs
//! void sub_mysub(void);
//!
//! // SUB/FUNCTION definitions
//! void sub_mysub(void) { ... }
//!
//! // Main program
//! int main(int argc, char** argv) {
//!     // Main program statements
//!     return 0;
//! }
//! ```
//!
//! # Type Mapping
//!
//! | BASIC Type    | C Type        |
//! |---------------|---------------|
//! | INTEGER       | int16_t       |
//! | LONG          | int32_t       |
//! | _INTEGER64    | int64_t       |
//! | SINGLE        | float         |
//! | DOUBLE        | double        |
//! | STRING        | qb_string*    |

use crate::ast::{BinaryOp, ExitType, PrintSeparator, UnaryOp};
use crate::codegen::error::CodeGenError;
use crate::codegen::{CodeGenerator, GeneratedOutput};
use crate::semantic::typed_ir::{
    TypedCaseCompareOp, TypedCaseMatch, TypedDataValue, TypedDoCondition, TypedExpr, TypedExprKind,
    TypedParameter, TypedPrintItem, TypedProgram, TypedStatement, TypedStatementKind,
};
use crate::semantic::types::BasicType;
use std::fmt::Write;

/// Runtime mode for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RuntimeMode {
    /// Inline runtime code in generated output (no external dependencies).
    #[default]
    Inline,
    /// Use external runtime library (requires libqb64fresh_rt.a).
    External,
}

/// C code generation backend.
///
/// Produces portable C99 code that compiles with gcc, clang, or MSVC.
pub struct CBackend {
    /// Counter for generating unique labels.
    label_counter: u32,
    /// Current indentation level.
    indent: usize,
    /// Stack of loop labels for EXIT statements.
    loop_stack: Vec<LoopContext>,
    /// Runtime mode.
    runtime_mode: RuntimeMode,
    /// Map of DATA labels to their indices (for RESTORE with label).
    data_label_indices: std::collections::HashMap<String, usize>,
}

/// Context for the current loop (for EXIT statement handling).
#[derive(Clone)]
struct LoopContext {
    /// Label to break to.
    break_label: String,
    /// Type of loop (For, While, Do).
    loop_type: ExitType,
}

/// Information collected from DATA statements for code generation.
///
/// This includes both the data values and a mapping of labels to their
/// positions in the data pool (for RESTORE with label support).
struct DataPoolInfo {
    /// The data values as (value_string, type_tag) tuples.
    values: Vec<(String, &'static str)>,
    /// Map of uppercase label names to their DATA pool index.
    ///
    /// When a label appears before a DATA statement, it maps to the index
    /// of the first value in that DATA statement.
    label_indices: std::collections::HashMap<String, usize>,
}

impl Default for CBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl CBackend {
    /// Creates a new C backend with inline runtime.
    pub fn new() -> Self {
        Self {
            label_counter: 0,
            indent: 0,
            loop_stack: Vec::new(),
            runtime_mode: RuntimeMode::Inline,
            data_label_indices: std::collections::HashMap::new(),
        }
    }

    /// Creates a new C backend with the specified runtime mode.
    pub fn with_runtime_mode(runtime_mode: RuntimeMode) -> Self {
        Self {
            label_counter: 0,
            indent: 0,
            loop_stack: Vec::new(),
            runtime_mode,
            data_label_indices: std::collections::HashMap::new(),
        }
    }

    /// Generates a unique label name.
    fn next_label(&mut self, prefix: &str) -> String {
        let label = format!("_qb_{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Returns the current indentation string.
    fn indent_str(&self) -> String {
        "    ".repeat(self.indent)
    }

    /// Maps a BASIC type to its C representation.
    fn c_type(&self, basic_type: &BasicType) -> String {
        match basic_type {
            BasicType::Bit => "int8_t".to_string(),
            BasicType::Byte => "int8_t".to_string(),
            BasicType::Integer => "int16_t".to_string(),
            BasicType::Long => "int32_t".to_string(),
            BasicType::Integer64 => "int64_t".to_string(),
            BasicType::Offset => "intptr_t".to_string(),
            BasicType::Single => "float".to_string(),
            BasicType::Double => "double".to_string(),
            BasicType::Float => "long double".to_string(),
            BasicType::String => "qb_string*".to_string(),
            BasicType::FixedString(n) => format!("char[{}]", n + 1), // +1 for null terminator
            BasicType::UnsignedBit => "uint8_t".to_string(),
            BasicType::UnsignedByte => "uint8_t".to_string(),
            BasicType::UnsignedInteger => "uint16_t".to_string(),
            BasicType::UnsignedLong => "uint32_t".to_string(),
            BasicType::UnsignedInteger64 => "uint64_t".to_string(),
            BasicType::UserDefined(name) => format!("struct {}", name),
            BasicType::Array { element_type, .. } => {
                format!("{}*", self.c_type(element_type))
            }
            BasicType::Void => "void".to_string(),
            BasicType::Unknown => "int32_t".to_string(), // Default to LONG
        }
    }

    /// Returns the default initializer for a type.
    fn default_init(&self, basic_type: &BasicType) -> String {
        match basic_type {
            BasicType::String => "qb_string_new(\"\")".to_string(),
            BasicType::FixedString(_) => "\"\"".to_string(),
            BasicType::Single | BasicType::Double | BasicType::Float => "0.0".to_string(),
            _ => "0".to_string(),
        }
    }

    /// Converts a BASIC identifier to a valid C identifier.
    ///
    /// BASIC allows characters like `$`, `%` in names which are invalid in C.
    fn c_identifier(&self, name: &str) -> String {
        // Remove type suffixes and replace invalid characters
        name.replace('$', "_str")
            .replace('%', "_int")
            .replace('&', "_lng")
            .replace('!', "_sng")
            .replace('#', "_dbl")
            .replace('`', "_bit")
    }

    /// Emits the header section (includes and runtime declarations).
    fn emit_header(&self, output: &mut String) {
        writeln!(output, "/* Generated by QB64Fresh */").unwrap();
        writeln!(output, "#include <stdio.h>").unwrap();
        writeln!(output, "#include <stdlib.h>").unwrap();
        writeln!(output, "#include <stdint.h>").unwrap();
        writeln!(output, "#include <stdbool.h>").unwrap();
        writeln!(output, "#include <string.h>").unwrap();
        writeln!(output, "#include <math.h>").unwrap();
        writeln!(output, "#include <ctype.h>").unwrap();
        writeln!(output).unwrap();

        match self.runtime_mode {
            RuntimeMode::Inline => {
                // Inline runtime library declarations
                self.emit_runtime_declarations(output);
            }
            RuntimeMode::External => {
                // Use external runtime library
                writeln!(output, "#include \"qb64fresh_rt.h\"").unwrap();
                writeln!(output).unwrap();
                // Alias for compatibility with inline code style
                writeln!(output, "typedef QbString qb_string;").unwrap();
                writeln!(output).unwrap();
                writeln!(output, "/* Using external QB64Fresh runtime library */").unwrap();
                writeln!(
                    output,
                    "/* Compile with: gcc -I<include_path> program.c -L<lib_path> -lqb64fresh_rt */"
                )
                .unwrap();
                writeln!(output).unwrap();
            }
        }
    }

    /// Emits minimal runtime library declarations.
    fn emit_runtime_declarations(&self, output: &mut String) {
        writeln!(output, "/* QB64Fresh Runtime Library */").unwrap();
        writeln!(output).unwrap();

        // String type
        writeln!(output, "typedef struct qb_string {{").unwrap();
        writeln!(output, "    char* data;").unwrap();
        writeln!(output, "    size_t len;").unwrap();
        writeln!(output, "    size_t capacity;").unwrap();
        writeln!(output, "}} qb_string;").unwrap();
        writeln!(output).unwrap();

        // String functions - using memcpy with explicit lengths for safety
        writeln!(output, "qb_string* qb_string_new(const char* s) {{").unwrap();
        writeln!(output, "    qb_string* str = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    str->len = strlen(s);").unwrap();
        writeln!(output, "    str->capacity = str->len + 1;").unwrap();
        writeln!(output, "    str->data = malloc(str->capacity);").unwrap();
        writeln!(output, "    memcpy(str->data, s, str->len + 1);").unwrap();
        writeln!(output, "    return str;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "void qb_string_free(qb_string* s) {{").unwrap();
        writeln!(output, "    if (s) {{ free(s->data); free(s); }}").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(
            output,
            "qb_string* qb_string_concat(qb_string* a, qb_string* b) {{"
        )
        .unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = a->len + b->len;").unwrap();
        writeln!(output, "    result->capacity = result->len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(output, "    memcpy(result->data, a->data, a->len);").unwrap();
        writeln!(
            output,
            "    memcpy(result->data + a->len, b->data, b->len + 1);"
        )
        .unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // PRINT functions
        writeln!(output, "void qb_print_int(int64_t n) {{").unwrap();
        writeln!(output, "    printf(\"%lld\", (long long)n);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "void qb_print_float(double n) {{").unwrap();
        writeln!(output, "    printf(\"%g\", n);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "void qb_print_string(qb_string* s) {{").unwrap();
        writeln!(output, "    printf(\"%s\", s->data);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "void qb_print_newline(void) {{").unwrap();
        writeln!(output, "    printf(\"\\n\");").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "void qb_print_tab(void) {{").unwrap();
        writeln!(output, "    printf(\"\\t\");").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // INPUT functions
        writeln!(
            output,
            "void qb_input_string(const char* prompt, qb_string** var) {{"
        )
        .unwrap();
        writeln!(output, "    char buffer[1024];").unwrap();
        writeln!(output, "    if (prompt) printf(\"%s\", prompt);").unwrap();
        writeln!(output, "    if (fgets(buffer, sizeof(buffer), stdin)) {{").unwrap();
        writeln!(output, "        size_t len = strlen(buffer);").unwrap();
        writeln!(
            output,
            "        if (len > 0 && buffer[len-1] == '\\n') buffer[len-1] = '\\0';"
        )
        .unwrap();
        writeln!(output, "        if (*var) qb_string_free(*var);").unwrap();
        writeln!(output, "        *var = qb_string_new(buffer);").unwrap();
        writeln!(output, "    }}").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(
            output,
            "void qb_input_int(const char* prompt, int32_t* var) {{"
        )
        .unwrap();
        writeln!(output, "    if (prompt) printf(\"%s\", prompt);").unwrap();
        writeln!(output, "    scanf(\"%d\", var);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(
            output,
            "void qb_input_float(const char* prompt, double* var) {{"
        )
        .unwrap();
        writeln!(output, "    if (prompt) printf(\"%s\", prompt);").unwrap();
        writeln!(output, "    scanf(\"%lf\", var);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // String comparison (returns -1, 0, or 1 like strcmp but for BASIC semantics)
        writeln!(
            output,
            "int qb_string_compare(qb_string* a, qb_string* b) {{"
        )
        .unwrap();
        writeln!(output, "    return strcmp(a->data, b->data);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // Built-in function stubs
        writeln!(output, "int32_t qb_len(qb_string* s) {{").unwrap();
        writeln!(output, "    return (int32_t)s->len;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "qb_string* qb_chr(int32_t code) {{").unwrap();
        writeln!(output, "    char buf[2] = {{ (char)code, '\\0' }};").unwrap();
        writeln!(output, "    return qb_string_new(buf);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "int32_t qb_asc(qb_string* s) {{").unwrap();
        writeln!(
            output,
            "    return s->len > 0 ? (unsigned char)s->data[0] : 0;"
        )
        .unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // Math helper functions
        writeln!(output, "int32_t qb_sgn(double n) {{").unwrap();
        writeln!(output, "    return (n > 0) - (n < 0);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "double qb_pi(void) {{").unwrap();
        writeln!(output, "    return 3.14159265358979323846;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(
            output,
            "double qb_clamp(double value, double min, double max) {{"
        )
        .unwrap();
        writeln!(
            output,
            "    return value < min ? min : (value > max ? max : value);"
        )
        .unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        writeln!(output, "float qb_rnd(float seed) {{").unwrap();
        writeln!(output, "    (void)seed; /* QB RND ignores seed for now */").unwrap();
        writeln!(output, "    return (float)rand() / (float)RAND_MAX;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // String functions
        // LEFT$(s$, n) - returns first n characters
        writeln!(output, "qb_string* qb_left(qb_string* s, int32_t n) {{").unwrap();
        writeln!(output, "    if (n <= 0) return qb_string_new(\"\");").unwrap();
        writeln!(
            output,
            "    size_t len = (size_t)n < s->len ? (size_t)n : s->len;"
        )
        .unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = len;").unwrap();
        writeln!(output, "    result->capacity = len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(output, "    memcpy(result->data, s->data, len);").unwrap();
        writeln!(output, "    result->data[len] = '\\0';").unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // RIGHT$(s$, n) - returns last n characters
        writeln!(output, "qb_string* qb_right(qb_string* s, int32_t n) {{").unwrap();
        writeln!(output, "    if (n <= 0) return qb_string_new(\"\");").unwrap();
        writeln!(
            output,
            "    size_t len = (size_t)n < s->len ? (size_t)n : s->len;"
        )
        .unwrap();
        writeln!(output, "    size_t start = s->len - len;").unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = len;").unwrap();
        writeln!(output, "    result->capacity = len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(output, "    memcpy(result->data, s->data + start, len);").unwrap();
        writeln!(output, "    result->data[len] = '\\0';").unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // MID$(s$, start, len) - returns substring (1-based index)
        writeln!(
            output,
            "qb_string* qb_mid(qb_string* s, int32_t start, int32_t n) {{"
        )
        .unwrap();
        writeln!(
            output,
            "    if (start < 1 || n <= 0 || (size_t)start > s->len) return qb_string_new(\"\");"
        )
        .unwrap();
        writeln!(output, "    size_t idx = (size_t)(start - 1);").unwrap();
        writeln!(
            output,
            "    size_t len = (idx + (size_t)n > s->len) ? s->len - idx : (size_t)n;"
        )
        .unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = len;").unwrap();
        writeln!(output, "    result->capacity = len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(output, "    memcpy(result->data, s->data + idx, len);").unwrap();
        writeln!(output, "    result->data[len] = '\\0';").unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // INSTR(start, s$, find$) - find substring (1-based, returns 0 if not found)
        writeln!(
            output,
            "int32_t qb_instr(int32_t start, qb_string* s, qb_string* find) {{"
        )
        .unwrap();
        writeln!(
            output,
            "    if (start < 1 || (size_t)start > s->len || find->len == 0) return 0;"
        )
        .unwrap();
        writeln!(
            output,
            "    char* pos = strstr(s->data + start - 1, find->data);"
        )
        .unwrap();
        writeln!(output, "    return pos ? (int32_t)(pos - s->data + 1) : 0;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // STR$(n) - convert number to string
        writeln!(output, "qb_string* qb_str(double n) {{").unwrap();
        writeln!(output, "    char buf[64];").unwrap();
        writeln!(output, "    snprintf(buf, sizeof(buf), \" %g\", n);").unwrap();
        writeln!(output, "    return qb_string_new(buf);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // VAL(s$) - convert string to number
        writeln!(output, "double qb_val(qb_string* s) {{").unwrap();
        writeln!(output, "    return atof(s->data);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // UCASE$(s$) - convert to uppercase
        writeln!(output, "qb_string* qb_ucase(qb_string* s) {{").unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = s->len;").unwrap();
        writeln!(output, "    result->capacity = s->len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(
            output,
            "    for (size_t i = 0; i < s->len; i++) result->data[i] = toupper(s->data[i]);"
        )
        .unwrap();
        writeln!(output, "    result->data[s->len] = '\\0';").unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // LCASE$(s$) - convert to lowercase
        writeln!(output, "qb_string* qb_lcase(qb_string* s) {{").unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = s->len;").unwrap();
        writeln!(output, "    result->capacity = s->len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(
            output,
            "    for (size_t i = 0; i < s->len; i++) result->data[i] = tolower(s->data[i]);"
        )
        .unwrap();
        writeln!(output, "    result->data[s->len] = '\\0';").unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // LTRIM$(s$) - remove leading spaces
        writeln!(output, "qb_string* qb_ltrim(qb_string* s) {{").unwrap();
        writeln!(output, "    size_t start = 0;").unwrap();
        writeln!(
            output,
            "    while (start < s->len && s->data[start] == ' ') start++;"
        )
        .unwrap();
        writeln!(output, "    return qb_right(s, (int32_t)(s->len - start));").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // RTRIM$(s$) - remove trailing spaces
        writeln!(output, "qb_string* qb_rtrim(qb_string* s) {{").unwrap();
        writeln!(output, "    size_t end = s->len;").unwrap();
        writeln!(
            output,
            "    while (end > 0 && s->data[end - 1] == ' ') end--;"
        )
        .unwrap();
        writeln!(output, "    return qb_left(s, (int32_t)end);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // SPACE$(n) - return n spaces
        writeln!(output, "qb_string* qb_space(int32_t n) {{").unwrap();
        writeln!(output, "    if (n <= 0) return qb_string_new(\"\");").unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = (size_t)n;").unwrap();
        writeln!(output, "    result->capacity = result->len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(output, "    memset(result->data, ' ', result->len);").unwrap();
        writeln!(output, "    result->data[result->len] = '\\0';").unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // STRING$(n, char$) - return n copies of first char of string
        writeln!(
            output,
            "qb_string* qb_string_fill(int32_t n, qb_string* c) {{"
        )
        .unwrap();
        writeln!(
            output,
            "    if (n <= 0 || c->len == 0) return qb_string_new(\"\");"
        )
        .unwrap();
        writeln!(output, "    qb_string* result = malloc(sizeof(qb_string));").unwrap();
        writeln!(output, "    result->len = (size_t)n;").unwrap();
        writeln!(output, "    result->capacity = result->len + 1;").unwrap();
        writeln!(output, "    result->data = malloc(result->capacity);").unwrap();
        writeln!(output, "    memset(result->data, c->data[0], result->len);").unwrap();
        writeln!(output, "    result->data[result->len] = '\\0';").unwrap();
        writeln!(output, "    return result;").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // qb_str_from_c - create qb_string from C string (alias for qb_string_new)
        writeln!(output, "qb_string* qb_str_from_c(const char* s) {{").unwrap();
        writeln!(output, "    return qb_string_new(s);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();

        // qb_str_float - convert float to string (used by DATA/READ)
        writeln!(output, "qb_string* qb_str_float(double n) {{").unwrap();
        writeln!(output, "    char buf[64];").unwrap();
        writeln!(output, "    snprintf(buf, sizeof(buf), \"%g\", n);").unwrap();
        writeln!(output, "    return qb_string_new(buf);").unwrap();
        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();
    }

    /// Emits an expression, returning the C code.
    fn emit_expr(&self, expr: &TypedExpr) -> Result<String, CodeGenError> {
        match &expr.kind {
            TypedExprKind::IntegerLiteral(n) => Ok(format!("{}LL", n)),

            TypedExprKind::FloatLiteral(n) => {
                // Handle special floating point values
                if n.is_nan() {
                    Ok("(0.0/0.0)".to_string())
                } else if n.is_infinite() {
                    if n.is_sign_positive() {
                        Ok("(1.0/0.0)".to_string())
                    } else {
                        Ok("(-1.0/0.0)".to_string())
                    }
                } else {
                    Ok(format!("{:.17}", n))
                }
            }

            TypedExprKind::StringLiteral(s) => {
                // Escape the string for C
                let escaped = self.escape_string(s);
                Ok(format!("qb_string_new(\"{}\")", escaped))
            }

            TypedExprKind::Variable(name) => Ok(self.c_identifier(name)),

            TypedExprKind::Binary { left, op, right } => {
                let left_code = self.emit_expr(left)?;
                let right_code = self.emit_expr(right)?;

                // Handle string concatenation specially
                if left.basic_type.is_string() && matches!(op, BinaryOp::Add) {
                    return Ok(format!("qb_string_concat({}, {})", left_code, right_code));
                }

                // Handle string comparisons
                if left.basic_type.is_string() {
                    return self.emit_string_comparison(&left_code, &right_code, op);
                }

                // Handle operators that can't be expressed as simple C binary operators
                match op {
                    BinaryOp::Power => {
                        // Use pow() from math.h for exponentiation
                        return Ok(format!("pow({}, {})", left_code, right_code));
                    }
                    BinaryOp::Eqv => {
                        // EQV (equivalence) = bitwise XNOR = ~(a ^ b)
                        return Ok(format!("(~({} ^ {}))", left_code, right_code));
                    }
                    BinaryOp::Imp => {
                        // IMP (implication) = ~a | b
                        return Ok(format!("((~{}) | {})", left_code, right_code));
                    }
                    _ => {}
                }

                let op_str = self.c_binary_op(op, &left.basic_type)?;
                Ok(format!("({} {} {})", left_code, op_str, right_code))
            }

            TypedExprKind::Unary { op, operand } => {
                let operand_code = self.emit_expr(operand)?;
                let op_str = match op {
                    UnaryOp::Negate => "-",
                    UnaryOp::Not => "~", // Bitwise NOT for numeric types
                };
                Ok(format!("({}{})", op_str, operand_code))
            }

            TypedExprKind::Grouped(inner) => {
                let inner_code = self.emit_expr(inner)?;
                Ok(format!("({})", inner_code))
            }

            TypedExprKind::FunctionCall { name, args } => {
                let args_code: Result<Vec<_>, _> =
                    args.iter().map(|arg| self.emit_expr(arg)).collect();
                let args_str = args_code?.join(", ");
                let c_name = self.c_function_name(name);
                Ok(format!("{}({})", c_name, args_str))
            }

            TypedExprKind::ArrayAccess {
                name,
                indices,
                dimensions,
            } => {
                let c_name = self.c_identifier(name);
                let indices_code: Result<Vec<_>, _> =
                    indices.iter().map(|idx| self.emit_expr(idx)).collect();
                let indices_code = indices_code?;

                if dimensions.is_empty() || indices_code.len() == 1 {
                    // 1D array - simple index (subtracting lower bound)
                    if let Some(dim) = dimensions.first() {
                        Ok(format!("{}[{} - {}]", c_name, indices_code[0], dim.lower))
                    } else {
                        // No dimension info, use index as-is (shouldn't happen)
                        Ok(format!("{}[{}]", c_name, indices_code[0]))
                    }
                } else {
                    // Multi-dimensional array - calculate linear index
                    // For 2D: arr(i, j) -> arr[(i - lower1) * size2 + (j - lower2)]
                    // General formula: sum of (adjusted_index * stride)
                    let mut linear_parts = Vec::new();

                    for (i, (idx, dim)) in indices_code.iter().zip(dimensions.iter()).enumerate() {
                        let adjusted = format!("({} - {})", idx, dim.lower);

                        if i < dimensions.len() - 1 {
                            // Calculate stride: product of all remaining dimension sizes
                            let stride: i64 = dimensions[i + 1..]
                                .iter()
                                .map(|d| d.upper - d.lower + 1)
                                .product();
                            linear_parts.push(format!("{} * {}", adjusted, stride));
                        } else {
                            // Last dimension, no stride multiplication
                            linear_parts.push(adjusted);
                        }
                    }

                    Ok(format!("{}[{}]", c_name, linear_parts.join(" + ")))
                }
            }

            TypedExprKind::Convert { expr, to_type } => {
                let inner_code = self.emit_expr(expr)?;
                let c_type = self.c_type(to_type);

                // Handle string-to-string conversion (no-op)
                if expr.basic_type.is_string() && to_type.is_string() {
                    return Ok(inner_code);
                }

                Ok(format!("(({})({}))", c_type, inner_code))
            }

            TypedExprKind::FieldAccess { object, field } => {
                let obj_code = self.emit_expr(object)?;
                let c_field = self.c_identifier(field);
                Ok(format!("{}.{}", obj_code, c_field))
            }
        }
    }

    /// Maps a binary operator to C syntax.
    fn c_binary_op(&self, op: &BinaryOp, _left_type: &BasicType) -> Result<String, CodeGenError> {
        Ok(match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Subtract => "-".to_string(),
            BinaryOp::Multiply => "*".to_string(),
            BinaryOp::Divide => "/".to_string(),
            BinaryOp::IntDivide => "/".to_string(), // Integer division in C when both operands are int
            BinaryOp::Modulo => "%".to_string(),
            BinaryOp::Power => {
                // Handled specially in emit_expr using pow()
                unreachable!("Power operator should be handled in emit_expr")
            }
            BinaryOp::Equal => "==".to_string(),
            BinaryOp::NotEqual => "!=".to_string(),
            BinaryOp::LessThan => "<".to_string(),
            BinaryOp::LessEqual => "<=".to_string(),
            BinaryOp::GreaterThan => ">".to_string(),
            BinaryOp::GreaterEqual => ">=".to_string(),
            BinaryOp::And => "&".to_string(), // Bitwise AND in BASIC
            BinaryOp::Or => "|".to_string(),  // Bitwise OR
            BinaryOp::Xor => "^".to_string(), // Bitwise XOR
            BinaryOp::Eqv => {
                // Handled specially in emit_expr as ~(a ^ b)
                unreachable!("EQV operator should be handled in emit_expr")
            }
            BinaryOp::Imp => {
                // Handled specially in emit_expr as (~a) | b
                unreachable!("IMP operator should be handled in emit_expr")
            }
        })
    }

    /// Emits string comparison code.
    fn emit_string_comparison(
        &self,
        left: &str,
        right: &str,
        op: &BinaryOp,
    ) -> Result<String, CodeGenError> {
        let cmp = format!("qb_string_compare({}, {})", left, right);
        let result = match op {
            BinaryOp::Equal => format!("({} == 0)", cmp),
            BinaryOp::NotEqual => format!("({} != 0)", cmp),
            BinaryOp::LessThan => format!("({} < 0)", cmp),
            BinaryOp::LessEqual => format!("({} <= 0)", cmp),
            BinaryOp::GreaterThan => format!("({} > 0)", cmp),
            BinaryOp::GreaterEqual => format!("({} >= 0)", cmp),
            _ => {
                return Err(CodeGenError::unsupported(format!(
                    "operator {} on strings",
                    op.as_str()
                )));
            }
        };
        Ok(result)
    }

    /// Maps a BASIC function name to its C equivalent.
    fn c_function_name(&self, name: &str) -> String {
        let upper = name.to_uppercase();
        match upper.as_str() {
            // Math functions
            "ABS" => "fabs".to_string(),
            "SIN" => "sin".to_string(),
            "COS" => "cos".to_string(),
            "TAN" => "tan".to_string(),
            "ATN" => "atan".to_string(),
            "SQR" => "sqrt".to_string(),
            "LOG" => "log".to_string(),
            "EXP" => "exp".to_string(),
            "INT" => "floor".to_string(),
            "SGN" => "qb_sgn".to_string(),
            "RND" => "qb_rnd".to_string(),

            // QB64 extended math functions
            "_PI" => "qb_pi".to_string(),
            "_ASIN" => "asin".to_string(),
            "_ACOS" => "acos".to_string(),
            "_ATAN2" => "atan2".to_string(),
            "_HYPOT" => "hypot".to_string(),
            "_CEIL" => "ceil".to_string(),
            "_ROUND" => "round".to_string(),
            "_MIN" => "fmin".to_string(),
            "_MAX" => "fmax".to_string(),
            "_CLAMP" => "qb_clamp".to_string(),

            // String functions
            "LEN" => "qb_len".to_string(),
            "CHR$" => "qb_chr".to_string(),
            "ASC" => "qb_asc".to_string(),
            "LEFT$" => "qb_left".to_string(),
            "RIGHT$" => "qb_right".to_string(),
            "MID$" => "qb_mid".to_string(),
            "INSTR" => "qb_instr".to_string(),
            "STR$" => "qb_str".to_string(),
            "VAL" => "qb_val".to_string(),
            "UCASE$" => "qb_ucase".to_string(),
            "LCASE$" => "qb_lcase".to_string(),
            "LTRIM$" => "qb_ltrim".to_string(),
            "RTRIM$" => "qb_rtrim".to_string(),
            "SPACE$" => "qb_space".to_string(),
            "STRING$" => "qb_string_fill".to_string(),

            // Default: prefix with qb_ for user functions
            _ => format!("qb_{}", self.c_identifier(name).to_lowercase()),
        }
    }

    /// Escapes a string for C string literal.
    ///
    /// This function ensures the string is safe to embed in generated C code by
    /// escaping special characters. Non-ASCII characters are escaped as `\xNN`
    /// sequences for maximum C compiler portability.
    fn escape_string(&self, s: &str) -> String {
        let mut result = String::with_capacity(s.len() * 2); // Worst case: all escapes
        for c in s.chars() {
            match c {
                '"' => result.push_str("\\\""),
                '\\' => result.push_str("\\\\"),
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                // Escape all control characters (ASCII and Unicode)
                c if c.is_control() => {
                    // For ASCII control chars, use \xNN
                    // For Unicode control chars, escape each UTF-8 byte
                    for byte in c.to_string().as_bytes() {
                        result.push_str(&format!("\\x{:02x}", byte));
                    }
                }
                // Keep printable ASCII as-is
                c if c.is_ascii() => result.push(c),
                // Escape non-ASCII characters as UTF-8 byte sequences
                // This ensures C compiler compatibility regardless of source encoding
                c => {
                    for byte in c.to_string().as_bytes() {
                        result.push_str(&format!("\\x{:02x}", byte));
                    }
                }
            }
        }
        result
    }

    /// Emits a statement.
    fn emit_stmt(
        &mut self,
        stmt: &TypedStatement,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();

        match &stmt.kind {
            TypedStatementKind::Assignment {
                name,
                value,
                target_type,
            } => {
                let c_name = self.c_identifier(name);
                let value_code = self.emit_expr(value)?;

                // Handle type conversion if needed
                if value.basic_type != *target_type {
                    let c_type = self.c_type(target_type);
                    writeln!(
                        output,
                        "{}{} = ({})({});",
                        indent, c_name, c_type, value_code
                    )
                    .unwrap();
                } else {
                    writeln!(output, "{}{} = {};", indent, c_name, value_code).unwrap();
                }
            }

            TypedStatementKind::ArrayAssignment {
                name,
                indices,
                value,
                dimensions,
                element_type,
            } => {
                let c_name = self.c_identifier(name);
                let value_code = self.emit_expr(value)?;

                // Calculate the linear index
                let indices_code: Result<Vec<_>, _> =
                    indices.iter().map(|idx| self.emit_expr(idx)).collect();
                let indices_code = indices_code?;

                let index_expr = if dimensions.is_empty() || indices_code.len() == 1 {
                    // 1D array
                    if let Some(dim) = dimensions.first() {
                        format!("{} - {}", indices_code[0], dim.lower)
                    } else {
                        indices_code[0].clone()
                    }
                } else {
                    // Multi-dimensional array - calculate linear index
                    let mut linear_parts = Vec::new();

                    for (i, (idx, dim)) in indices_code.iter().zip(dimensions.iter()).enumerate() {
                        let adjusted = format!("({} - {})", idx, dim.lower);

                        if i < dimensions.len() - 1 {
                            let stride: i64 = dimensions[i + 1..]
                                .iter()
                                .map(|d| d.upper - d.lower + 1)
                                .product();
                            linear_parts.push(format!("{} * {}", adjusted, stride));
                        } else {
                            linear_parts.push(adjusted);
                        }
                    }

                    linear_parts.join(" + ")
                };

                // Handle type conversion if needed
                if value.basic_type != *element_type {
                    let c_type = self.c_type(element_type);
                    writeln!(
                        output,
                        "{}{}[{}] = ({})({});",
                        indent, c_name, index_expr, c_type, value_code
                    )
                    .unwrap();
                } else {
                    writeln!(
                        output,
                        "{}{}[{}] = {};",
                        indent, c_name, index_expr, value_code
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::Print { items, newline } => {
                for item in items {
                    self.emit_print_item(item, output)?;
                }
                if *newline {
                    writeln!(output, "{}qb_print_newline();", indent).unwrap();
                }
            }

            TypedStatementKind::Input {
                prompt,
                show_question_mark,
                variables,
            } => {
                // Build the prompt string
                let full_prompt = match prompt {
                    Some(p) => {
                        if *show_question_mark {
                            format!("{}? ", p)
                        } else {
                            p.clone()
                        }
                    }
                    None => {
                        if *show_question_mark {
                            "? ".to_string()
                        } else {
                            String::new()
                        }
                    }
                };

                for (i, (var_name, var_type)) in variables.iter().enumerate() {
                    let c_name = self.c_identifier(var_name);
                    let prompt_arg = if i == 0 && !full_prompt.is_empty() {
                        format!("\"{}\"", self.escape_string(&full_prompt))
                    } else {
                        "NULL".to_string()
                    };

                    if var_type.is_string() {
                        writeln!(
                            output,
                            "{}qb_input_string({}, &{});",
                            indent, prompt_arg, c_name
                        )
                        .unwrap();
                    } else if var_type.is_float() {
                        writeln!(
                            output,
                            "{}qb_input_float({}, &{});",
                            indent, prompt_arg, c_name
                        )
                        .unwrap();
                    } else {
                        writeln!(
                            output,
                            "{}qb_input_int({}, &{});",
                            indent, prompt_arg, c_name
                        )
                        .unwrap();
                    }
                }
            }

            TypedStatementKind::LineInput { prompt, variable } => {
                let c_name = self.c_identifier(variable);
                let prompt_arg = match prompt {
                    Some(p) => format!("\"{}\"", self.escape_string(p)),
                    None => "NULL".to_string(),
                };
                writeln!(
                    output,
                    "{}qb_input_string({}, &{});",
                    indent, prompt_arg, c_name
                )
                .unwrap();
            }

            TypedStatementKind::If {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                let cond_code = self.emit_expr(condition)?;
                writeln!(output, "{}if ({}) {{", indent, cond_code).unwrap();

                self.indent += 1;
                for stmt in then_branch {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;

                for (elseif_cond, elseif_body) in elseif_branches {
                    let elseif_code = self.emit_expr(elseif_cond)?;
                    writeln!(output, "{}}} else if ({}) {{", indent, elseif_code).unwrap();

                    self.indent += 1;
                    for stmt in elseif_body {
                        self.emit_stmt(stmt, output)?;
                    }
                    self.indent -= 1;
                }

                if let Some(else_body) = else_branch {
                    writeln!(output, "{}}} else {{", indent).unwrap();

                    self.indent += 1;
                    for stmt in else_body {
                        self.emit_stmt(stmt, output)?;
                    }
                    self.indent -= 1;
                }

                writeln!(output, "{}}}", indent).unwrap();
            }

            TypedStatementKind::SelectCase {
                test_expr,
                cases,
                case_else,
            } => {
                // Generate a temp variable for the test expression
                let test_var = self.next_label("select");
                let test_code = self.emit_expr(test_expr)?;
                let c_type = self.c_type(&test_expr.basic_type);

                writeln!(output, "{}{} {} = {};", indent, c_type, test_var, test_code).unwrap();

                let mut first = true;
                for case in cases {
                    let condition = self.emit_case_condition(&test_var, &case.matches)?;

                    if first {
                        writeln!(output, "{}if ({}) {{", indent, condition).unwrap();
                        first = false;
                    } else {
                        writeln!(output, "{}}} else if ({}) {{", indent, condition).unwrap();
                    }

                    self.indent += 1;
                    for stmt in &case.body {
                        self.emit_stmt(stmt, output)?;
                    }
                    self.indent -= 1;
                }

                if let Some(else_body) = case_else {
                    writeln!(output, "{}}} else {{", indent).unwrap();

                    self.indent += 1;
                    for stmt in else_body {
                        self.emit_stmt(stmt, output)?;
                    }
                    self.indent -= 1;
                }

                if !first {
                    writeln!(output, "{}}}", indent).unwrap();
                }
            }

            TypedStatementKind::For {
                variable,
                var_type,
                start,
                end,
                step,
                body,
            } => {
                let c_var = self.c_identifier(variable);
                let c_type = self.c_type(var_type);
                let start_code = self.emit_expr(start)?;
                let end_code = self.emit_expr(end)?;
                let step_code = match step {
                    Some(s) => self.emit_expr(s)?,
                    None => "1".to_string(),
                };

                // Create a break label for EXIT FOR
                let break_label = self.next_label("for_end");
                self.loop_stack.push(LoopContext {
                    break_label: break_label.clone(),
                    loop_type: ExitType::For,
                });

                // Store end and step in temp variables to avoid multiple evaluation
                // (important if they contain side effects like function calls)
                let end_var = self.next_label("for_end_val");
                let step_var = self.next_label("for_step");
                writeln!(output, "{}{} {} = {};", indent, c_type, end_var, end_code).unwrap();
                writeln!(output, "{}{} {} = {};", indent, c_type, step_var, step_code).unwrap();

                // BASIC FOR loops check condition based on step direction
                writeln!(
                    output,
                    "{}for ({} {} = {}; ({} > 0) ? ({} <= {}) : ({} >= {}); {} += {}) {{",
                    indent,
                    c_type,
                    c_var,
                    start_code,
                    step_var,
                    c_var,
                    end_var,
                    c_var,
                    end_var,
                    c_var,
                    step_var
                )
                .unwrap();

                self.indent += 1;
                for stmt in body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;

                writeln!(output, "{}}}", indent).unwrap();
                writeln!(output, "{}{}:;", indent, break_label).unwrap();

                self.loop_stack.pop();
            }

            TypedStatementKind::While { condition, body } => {
                let cond_code = self.emit_expr(condition)?;
                let break_label = self.next_label("while_end");

                self.loop_stack.push(LoopContext {
                    break_label: break_label.clone(),
                    loop_type: ExitType::While,
                });

                writeln!(output, "{}while ({}) {{", indent, cond_code).unwrap();

                self.indent += 1;
                for stmt in body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;

                writeln!(output, "{}}}", indent).unwrap();
                writeln!(output, "{}{}:;", indent, break_label).unwrap();

                self.loop_stack.pop();
            }

            TypedStatementKind::DoLoop {
                pre_condition,
                body,
                post_condition,
            } => {
                let break_label = self.next_label("do_end");

                self.loop_stack.push(LoopContext {
                    break_label: break_label.clone(),
                    loop_type: ExitType::Do,
                });

                match (pre_condition, post_condition) {
                    (Some(pre), None) => {
                        // DO WHILE/UNTIL ... LOOP
                        let cond = self.emit_do_condition(pre)?;
                        writeln!(output, "{}while ({}) {{", indent, cond).unwrap();
                    }
                    (None, Some(_post)) => {
                        // DO ... LOOP WHILE/UNTIL
                        writeln!(output, "{}do {{", indent).unwrap();
                    }
                    (None, None) => {
                        // DO ... LOOP (infinite)
                        writeln!(output, "{}for (;;) {{", indent).unwrap();
                    }
                    (Some(_), Some(_)) => {
                        return Err(CodeGenError::internal(
                            "DO loop cannot have both pre and post conditions",
                        ));
                    }
                }

                self.indent += 1;
                for stmt in body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;

                if let Some(post) = post_condition {
                    let cond = self.emit_do_condition(post)?;
                    writeln!(output, "{}}} while ({});", indent, cond).unwrap();
                } else {
                    writeln!(output, "{}}}", indent).unwrap();
                }

                writeln!(output, "{}{}:;", indent, break_label).unwrap();
                self.loop_stack.pop();
            }

            TypedStatementKind::Goto { target } => {
                let c_label = self.c_identifier(target);
                writeln!(output, "{}goto {};", indent, c_label).unwrap();
            }

            TypedStatementKind::Gosub { target } => {
                // GOSUB is tricky in C - we'd need a proper continuation or setjmp/longjmp
                // For now, just call it as a function (assumes label is converted to function)
                let c_label = self.c_identifier(target);
                writeln!(
                    output,
                    "{}/* GOSUB {} - treating as function call */",
                    indent, target
                )
                .unwrap();
                writeln!(output, "{}{}();", indent, c_label).unwrap();
            }

            TypedStatementKind::Return => {
                writeln!(output, "{}return;", indent).unwrap();
            }

            TypedStatementKind::Exit { exit_type } => {
                // Find the matching loop
                let label = self
                    .loop_stack
                    .iter()
                    .rev()
                    .find(|ctx| ctx.loop_type == *exit_type)
                    .map(|ctx| ctx.break_label.clone());

                if let Some(label) = label {
                    writeln!(output, "{}goto {};", indent, label).unwrap();
                } else {
                    // EXIT SUB or EXIT FUNCTION
                    writeln!(output, "{}return;", indent).unwrap();
                }
            }

            TypedStatementKind::End => {
                writeln!(output, "{}exit(0);", indent).unwrap();
            }

            TypedStatementKind::Stop => {
                writeln!(output, "{}/* STOP */", indent).unwrap();
                writeln!(output, "{}exit(1);", indent).unwrap();
            }

            TypedStatementKind::Call { name, args } => {
                let args_code: Result<Vec<_>, _> =
                    args.iter().map(|arg| self.emit_expr(arg)).collect();
                let args_str = args_code?.join(", ");
                let c_name = format!("qb_sub_{}", self.c_identifier(name).to_lowercase());
                writeln!(output, "{}{}({});", indent, c_name, args_str).unwrap();
            }

            TypedStatementKind::SubDefinition {
                name,
                params,
                body,
                is_static: _,
            } => {
                let c_name = format!("qb_sub_{}", self.c_identifier(name).to_lowercase());
                let params_str = self.emit_params(params);

                writeln!(output, "{}void {}({}) {{", indent, c_name, params_str).unwrap();

                self.indent += 1;
                for stmt in body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;

                writeln!(output, "{}}}", indent).unwrap();
                writeln!(output).unwrap();
            }

            TypedStatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                body,
                is_static: _,
            } => {
                let c_name = self.c_function_name(name);
                let c_ret_type = self.c_type(return_type);
                let params_str = self.emit_params(params);

                writeln!(
                    output,
                    "{}{} {}({}) {{",
                    indent, c_ret_type, c_name, params_str
                )
                .unwrap();

                // Declare the return variable (BASIC functions return via their name)
                let ret_var = self.c_identifier(name);
                writeln!(
                    output,
                    "    {} {} = {};",
                    c_ret_type,
                    ret_var,
                    self.default_init(return_type)
                )
                .unwrap();

                self.indent += 1;
                for stmt in body {
                    self.emit_stmt(stmt, output)?;
                }
                self.indent -= 1;

                writeln!(output, "    return {};", ret_var).unwrap();
                writeln!(output, "{}}}", indent).unwrap();
                writeln!(output).unwrap();
            }

            TypedStatementKind::Dim {
                name,
                basic_type,
                dimensions,
                shared: _,
            } => {
                let c_name = self.c_identifier(name);
                let c_type = self.c_type(basic_type);

                if dimensions.is_empty() {
                    // Simple variable
                    let init = self.default_init(basic_type);
                    writeln!(output, "{}{} {} = {};", indent, c_type, c_name, init).unwrap();
                } else {
                    // Array - declare pointer and allocate
                    let sizes: Vec<String> = dimensions
                        .iter()
                        .map(|d| format!("({})", d.upper - d.lower + 1))
                        .collect();
                    let size_expr = sizes.join(" * ");
                    // First declare the pointer, then allocate
                    writeln!(
                        output,
                        "{}{}* {} = malloc(sizeof({}) * {});",
                        indent, c_type, c_name, c_type, size_expr
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::Const {
                name,
                value,
                basic_type: _,
            } => {
                let c_name = self.c_identifier(name);
                let value_code = self.emit_expr(value)?;
                // Use const in C
                writeln!(
                    output,
                    "{}const {} {} = {};",
                    indent,
                    self.c_type(&value.basic_type),
                    c_name,
                    value_code
                )
                .unwrap();
            }

            TypedStatementKind::Label { name } => {
                let c_label = self.c_identifier(name);
                writeln!(output, "{}:", c_label).unwrap();
            }

            TypedStatementKind::Comment(text) => {
                writeln!(output, "{}/* {} */", indent, text).unwrap();
            }

            TypedStatementKind::Expression(expr) => {
                let expr_code = self.emit_expr(expr)?;
                writeln!(output, "{}{};", indent, expr_code).unwrap();
            }

            // Preprocessor directives - emit as comments for now
            // In a full implementation, these would be processed during a preprocessing phase
            TypedStatementKind::IncludeDirective { path } => {
                writeln!(output, "{}/* $INCLUDE: '{}' */", indent, path).unwrap();
            }

            TypedStatementKind::ConditionalBlock {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                // Emit conditional compilation as C preprocessor directives
                // This is a simplified approach - real QB64 uses its own preprocessor
                writeln!(output, "{}/* $IF {} */", indent, condition).unwrap();
                for stmt in then_branch {
                    self.emit_stmt(stmt, output)?;
                }
                for (elseif_cond, elseif_body) in elseif_branches {
                    writeln!(output, "{}/* $ELSEIF {} */", indent, elseif_cond).unwrap();
                    for stmt in elseif_body {
                        self.emit_stmt(stmt, output)?;
                    }
                }
                if let Some(else_body) = else_branch {
                    writeln!(output, "{}/* $ELSE */", indent).unwrap();
                    for stmt in else_body {
                        self.emit_stmt(stmt, output)?;
                    }
                }
                writeln!(output, "{}/* $END IF */", indent).unwrap();
            }

            TypedStatementKind::MetaCommand { command, args } => {
                let args_str = args.as_deref().unwrap_or("");
                writeln!(output, "{}/* ${} {} */", indent, command, args_str).unwrap();
            }

            TypedStatementKind::Swap { left, right } => {
                let left_code = self.emit_expr(left)?;
                let right_code = self.emit_expr(right)?;
                let c_type = self.c_type(&left.basic_type);

                // Generate a temp variable swap
                let temp_var = self.next_label("swap_temp");
                writeln!(output, "{}{} {} = {};", indent, c_type, temp_var, left_code).unwrap();
                writeln!(output, "{}{} = {};", indent, left_code, right_code).unwrap();
                writeln!(output, "{}{} = {};", indent, right_code, temp_var).unwrap();
            }

            TypedStatementKind::Continue { continue_type } => {
                // Find the matching loop's continue label
                // For now, we just use C's continue statement which works for the innermost loop
                // A more sophisticated approach would track loop labels like EXIT does
                let _ = continue_type; // Will use for specific loop type targeting later
                writeln!(output, "{}continue;", indent).unwrap();
            }

            TypedStatementKind::TypeDefinition { name, members } => {
                // Generate a C struct for the TYPE
                let c_name = self.c_identifier(name);
                writeln!(output, "{}typedef struct {} {{", indent, c_name).unwrap();

                for member in members {
                    let c_member_type = self.c_type(&member.basic_type);
                    let c_member_name = self.c_identifier(&member.name);

                    // Handle fixed-length strings specially
                    if let BasicType::FixedString(len) = &member.basic_type {
                        // Fixed-length string becomes a char array
                        writeln!(output, "{}    char {}[{}];", indent, c_member_name, len + 1)
                            .unwrap();
                    } else {
                        writeln!(output, "{}    {} {};", indent, c_member_type, c_member_name)
                            .unwrap();
                    }
                }

                writeln!(output, "{}}} {};", indent, c_name).unwrap();
                writeln!(output).unwrap();
            }

            TypedStatementKind::Data { .. } => {
                // DATA statements are handled in collect_data_values during global emission
                // Nothing to emit inline - the data is in the global _qb_data array
            }

            TypedStatementKind::Read { variables } => {
                // Each variable reads the next value from the DATA pool
                for (var_name, var_type) in variables {
                    let c_var = self.c_identifier(var_name);

                    // Generate READ with type conversion based on target variable type
                    match var_type {
                        BasicType::String | BasicType::FixedString(_) => {
                            // For strings, copy the string value
                            writeln!(
                                output,
                                "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 's') {{",
                                indent
                            )
                            .unwrap();
                            writeln!(
                                output,
                                "{}    {} = qb_str_from_c(_qb_data[_qb_data_ptr].v.s);",
                                indent, c_var
                            )
                            .unwrap();
                            writeln!(
                                output,
                                "{}}} else if (_qb_data_ptr < _qb_data_count) {{",
                                indent
                            )
                            .unwrap();
                            writeln!(
                                output,
                                "{}    {} = qb_str_float(_qb_data[_qb_data_ptr].v.n);",
                                indent, c_var
                            )
                            .unwrap();
                            writeln!(output, "{}}}", indent).unwrap();
                            writeln!(output, "{}_qb_data_ptr++;", indent).unwrap();
                        }
                        _ => {
                            // For numeric types, read the numeric value with conversion
                            let c_type = self.c_type(var_type);
                            writeln!(
                                output,
                                "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 'd') {{",
                                indent
                            )
                            .unwrap();
                            writeln!(
                                output,
                                "{}    {} = ({})_qb_data[_qb_data_ptr].v.n;",
                                indent, c_var, c_type
                            )
                            .unwrap();
                            writeln!(output, "{}}}", indent).unwrap();
                            writeln!(output, "{}_qb_data_ptr++;", indent).unwrap();
                        }
                    }
                }
            }

            TypedStatementKind::Restore { label } => {
                match label {
                    None => {
                        // RESTORE without label - reset to beginning
                        writeln!(output, "{}_qb_data_ptr = 0;", indent).unwrap();
                    }
                    Some(lbl) => {
                        // RESTORE with label - look up the label's DATA index
                        let label_upper = lbl.to_uppercase();
                        if let Some(&index) = self.data_label_indices.get(&label_upper) {
                            writeln!(
                                output,
                                "{}_qb_data_ptr = {}; /* RESTORE {} */",
                                indent, index, lbl
                            )
                            .unwrap();
                        } else {
                            // Label not found - emit warning comment and reset to 0
                            // This could happen if the label exists but has no DATA after it
                            writeln!(
                                output,
                                "{}/* Warning: RESTORE label '{}' not associated with DATA */",
                                indent, lbl
                            )
                            .unwrap();
                            writeln!(output, "{}_qb_data_ptr = 0;", indent).unwrap();
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Emits a PRINT item.
    fn emit_print_item(
        &self,
        item: &TypedPrintItem,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();
        let expr_code = self.emit_expr(&item.expr)?;

        // Choose the appropriate print function based on type
        if item.expr.basic_type.is_string() {
            writeln!(output, "{}qb_print_string({});", indent, expr_code).unwrap();
        } else if item.expr.basic_type.is_float() {
            writeln!(output, "{}qb_print_float({});", indent, expr_code).unwrap();
        } else {
            writeln!(output, "{}qb_print_int({});", indent, expr_code).unwrap();
        }

        // Handle separator
        if let Some(sep) = &item.separator {
            match sep {
                PrintSeparator::Comma => {
                    writeln!(output, "{}qb_print_tab();", indent).unwrap();
                }
                PrintSeparator::Semicolon => {
                    // No separator - items print adjacent
                }
            }
        }

        Ok(())
    }

    /// Emits a DO loop condition.
    fn emit_do_condition(&self, cond: &TypedDoCondition) -> Result<String, CodeGenError> {
        let cond_code = self.emit_expr(&cond.condition)?;
        if cond.is_while {
            Ok(cond_code)
        } else {
            // UNTIL is the opposite of WHILE
            Ok(format!("!({})", cond_code))
        }
    }

    /// Emits CASE match conditions.
    fn emit_case_condition(
        &self,
        test_var: &str,
        matches: &[TypedCaseMatch],
    ) -> Result<String, CodeGenError> {
        let conditions: Result<Vec<_>, _> = matches
            .iter()
            .map(|m| self.emit_single_case_match(test_var, m))
            .collect();
        Ok(conditions?.join(" || "))
    }

    /// Emits a single CASE match.
    fn emit_single_case_match(
        &self,
        test_var: &str,
        case_match: &TypedCaseMatch,
    ) -> Result<String, CodeGenError> {
        match case_match {
            TypedCaseMatch::Single(expr) => {
                let val = self.emit_expr(expr)?;
                Ok(format!("({} == {})", test_var, val))
            }
            TypedCaseMatch::Range { from, to } => {
                let from_code = self.emit_expr(from)?;
                let to_code = self.emit_expr(to)?;
                Ok(format!(
                    "({} >= {} && {} <= {})",
                    test_var, from_code, test_var, to_code
                ))
            }
            TypedCaseMatch::Comparison { op, value } => {
                let val = self.emit_expr(value)?;
                let c_op = match op {
                    TypedCaseCompareOp::Equal => "==",
                    TypedCaseCompareOp::NotEqual => "!=",
                    TypedCaseCompareOp::LessThan => "<",
                    TypedCaseCompareOp::LessEqual => "<=",
                    TypedCaseCompareOp::GreaterThan => ">",
                    TypedCaseCompareOp::GreaterEqual => ">=",
                };
                Ok(format!("({} {} {})", test_var, c_op, val))
            }
        }
    }

    /// Emits function/sub parameters.
    fn emit_params(&self, params: &[TypedParameter]) -> String {
        if params.is_empty() {
            return "void".to_string();
        }

        params
            .iter()
            .map(|p| {
                let c_type = self.c_type(&p.basic_type);
                let c_name = self.c_identifier(&p.name);
                if p.by_val {
                    format!("{} {}", c_type, c_name)
                } else {
                    // Pass by reference - use pointer
                    format!("{}* {}", c_type, c_name)
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Collects global variables and procedure declarations from the program.
    fn collect_globals(&self, program: &TypedProgram) -> (Vec<String>, Vec<String>) {
        let mut globals = Vec::new();
        let mut forward_decls = Vec::new();

        for stmt in &program.statements {
            match &stmt.kind {
                TypedStatementKind::Dim {
                    name,
                    basic_type,
                    dimensions,
                    shared: _,
                } if dimensions.is_empty() => {
                    let c_type = self.c_type(basic_type);
                    let c_name = self.c_identifier(name);
                    let init = self.default_init(basic_type);
                    globals.push(format!("{} {} = {};", c_type, c_name, init));
                }

                TypedStatementKind::SubDefinition { name, params, .. } => {
                    let c_name = format!("qb_sub_{}", self.c_identifier(name).to_lowercase());
                    let params_str = self.emit_params(params);
                    forward_decls.push(format!("void {}({});", c_name, params_str));
                }

                TypedStatementKind::FunctionDefinition {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    let c_name = self.c_function_name(name);
                    let c_ret_type = self.c_type(return_type);
                    let params_str = self.emit_params(params);
                    forward_decls.push(format!("{} {}({});", c_ret_type, c_name, params_str));
                }

                _ => {}
            }
        }

        (globals, forward_decls)
    }

    /// Collects all DATA values from the program into a flat array.
    ///
    /// Also tracks label positions for RESTORE with label support.
    /// Labels immediately before DATA statements map to that DATA's index.
    fn collect_data_values(&self, program: &TypedProgram) -> DataPoolInfo {
        let mut info = DataPoolInfo {
            values: Vec::new(),
            label_indices: std::collections::HashMap::new(),
        };
        // Pending labels: labels seen that haven't been assigned to a DATA index yet
        let mut pending_labels: Vec<String> = Vec::new();

        for stmt in &program.statements {
            self.collect_data_from_stmt(stmt, &mut info, &mut pending_labels);
        }

        info
    }

    /// Recursively collects DATA values from a statement and its children.
    ///
    /// Tracks labels that precede DATA statements so RESTORE can jump to them.
    fn collect_data_from_stmt(
        &self,
        stmt: &TypedStatement,
        info: &mut DataPoolInfo,
        pending_labels: &mut Vec<String>,
    ) {
        match &stmt.kind {
            TypedStatementKind::Label { name } => {
                // Record this label as pending - it will be associated with the
                // next DATA statement's starting index
                pending_labels.push(name.to_uppercase());
            }

            TypedStatementKind::Data {
                values: data_values,
            } => {
                // Associate any pending labels with the current DATA index
                let current_index = info.values.len();
                for label in pending_labels.drain(..) {
                    info.label_indices.insert(label, current_index);
                }

                // Collect the data values
                for val in data_values {
                    let (val_str, type_tag) = match val {
                        TypedDataValue::Integer(n) => (format!("{}.0", n), "d"),
                        TypedDataValue::Float(f) => (format!("{}", f), "d"),
                        TypedDataValue::String(s) => {
                            // Escape the string for C
                            let escaped = self.escape_string(s);
                            (format!("\"{}\"", escaped), "s")
                        }
                    };
                    info.values.push((val_str, type_tag));
                }
            }

            // Recurse into compound statements
            TypedStatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                for s in then_branch {
                    self.collect_data_from_stmt(s, info, pending_labels);
                }
                for (_, branch) in elseif_branches {
                    for s in branch {
                        self.collect_data_from_stmt(s, info, pending_labels);
                    }
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        self.collect_data_from_stmt(s, info, pending_labels);
                    }
                }
            }

            TypedStatementKind::For { body, .. }
            | TypedStatementKind::While { body, .. }
            | TypedStatementKind::DoLoop { body, .. } => {
                for s in body {
                    self.collect_data_from_stmt(s, info, pending_labels);
                }
            }

            TypedStatementKind::SelectCase {
                cases, case_else, ..
            } => {
                for case in cases {
                    for s in &case.body {
                        self.collect_data_from_stmt(s, info, pending_labels);
                    }
                }
                if let Some(else_stmts) = case_else {
                    for s in else_stmts {
                        self.collect_data_from_stmt(s, info, pending_labels);
                    }
                }
            }

            TypedStatementKind::SubDefinition { body, .. }
            | TypedStatementKind::FunctionDefinition { body, .. } => {
                for s in body {
                    self.collect_data_from_stmt(s, info, pending_labels);
                }
            }

            _ => {}
        }
    }
}

impl CodeGenerator for CBackend {
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError> {
        // We need mutability for label generation and loop stack
        let mut backend = CBackend::with_runtime_mode(self.runtime_mode);
        let mut output = String::new();

        // Header
        backend.emit_header(&mut output);

        // Collect globals and forward declarations
        let (globals, forward_decls) = backend.collect_globals(program);

        // Global variables
        if !globals.is_empty() {
            writeln!(output, "/* Global Variables */").unwrap();
            for decl in globals {
                writeln!(output, "{}", decl).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Forward declarations
        if !forward_decls.is_empty() {
            writeln!(output, "/* Forward Declarations */").unwrap();
            for decl in forward_decls {
                writeln!(output, "{}", decl).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Collect and emit DATA pool
        let data_pool = backend.collect_data_values(program);
        // Store label indices for RESTORE statement emission
        backend.data_label_indices = data_pool.label_indices;

        if !data_pool.values.is_empty() {
            writeln!(output, "/* DATA Pool */").unwrap();
            writeln!(output, "typedef struct {{ char type; union {{ double n; const char* s; }} v; }} _qb_data_item;").unwrap();
            write!(output, "static _qb_data_item _qb_data[] = {{").unwrap();
            for (i, (val, type_tag)) in data_pool.values.iter().enumerate() {
                if i > 0 {
                    write!(output, ",").unwrap();
                }
                if *type_tag == "d" {
                    write!(output, " {{'d', {{.n = {}}}}}", val).unwrap();
                } else {
                    write!(output, " {{'s', {{.s = {}}}}}", val).unwrap();
                }
            }
            writeln!(output, " }};").unwrap();
            writeln!(output, "static int _qb_data_ptr = 0;").unwrap();
            writeln!(
                output,
                "static const int _qb_data_count = {};",
                data_pool.values.len()
            )
            .unwrap();
            writeln!(output).unwrap();
        }

        // SUB/FUNCTION definitions (emit before main)
        for stmt in &program.statements {
            match &stmt.kind {
                TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. } => {
                    backend.emit_stmt(stmt, &mut output)?;
                }
                _ => {}
            }
        }

        // Main function
        writeln!(output, "int main(int argc, char** argv) {{").unwrap();
        writeln!(output, "    (void)argc; (void)argv;").unwrap();
        writeln!(output).unwrap();

        backend.indent = 1;

        // Emit main program statements (excluding SUB/FUNCTION definitions)
        for stmt in &program.statements {
            match &stmt.kind {
                TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. } => {
                    // Already emitted above
                }
                _ => {
                    backend.emit_stmt(stmt, &mut output)?;
                }
            }
        }

        writeln!(output).unwrap();
        writeln!(output, "    return 0;").unwrap();
        writeln!(output, "}}").unwrap();

        Ok(GeneratedOutput::c_code(output))
    }

    fn backend_name(&self) -> &str {
        "C"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::semantic::typed_ir::{TypedExpr, TypedStatement, TypedStatementKind};

    #[test]
    fn test_c_type_mapping() {
        let backend = CBackend::new();
        assert_eq!(backend.c_type(&BasicType::Integer), "int16_t");
        assert_eq!(backend.c_type(&BasicType::Long), "int32_t");
        assert_eq!(backend.c_type(&BasicType::Double), "double");
        assert_eq!(backend.c_type(&BasicType::String), "qb_string*");
    }

    #[test]
    fn test_c_identifier() {
        let backend = CBackend::new();
        assert_eq!(backend.c_identifier("name$"), "name_str");
        assert_eq!(backend.c_identifier("count%"), "count_int");
        assert_eq!(backend.c_identifier("myVar"), "myVar");
    }

    #[test]
    fn test_emit_integer_literal() {
        let backend = CBackend::new();
        let expr = TypedExpr::integer(42, Span::new(0, 2));
        let result = backend.emit_expr(&expr).unwrap();
        assert_eq!(result, "42LL");
    }

    #[test]
    fn test_emit_string_literal() {
        let backend = CBackend::new();
        let expr = TypedExpr::string("Hello".to_string(), Span::new(0, 7));
        let result = backend.emit_expr(&expr).unwrap();
        assert_eq!(result, "qb_string_new(\"Hello\")");
    }

    #[test]
    fn test_generate_empty_program() {
        let program = TypedProgram::new(vec![]);
        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("int main("));
        assert!(result.code.contains("return 0;"));
    }

    #[test]
    fn test_generate_print_statement() {
        let program = TypedProgram::new(vec![TypedStatement::new(
            TypedStatementKind::Print {
                items: vec![TypedPrintItem {
                    expr: TypedExpr::string("Hello".to_string(), Span::new(6, 13)),
                    separator: None,
                }],
                newline: true,
            },
            Span::new(0, 13),
        )]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("qb_print_string"));
        assert!(result.code.contains("qb_print_newline"));
    }

    #[test]
    fn test_generate_assignment() {
        let program = TypedProgram::new(vec![TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "x".to_string(),
                value: TypedExpr::integer(42, Span::new(4, 6)),
                target_type: BasicType::Long,
            },
            Span::new(0, 6),
        )]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("x = 42LL"));
    }

    #[test]
    fn test_emit_power_operator() {
        let backend = CBackend::new();
        // 2 ^ 3
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(2, Span::new(0, 1))),
                op: BinaryOp::Power,
                right: Box::new(TypedExpr::integer(3, Span::new(4, 5))),
            },
            BasicType::Double,
            Span::new(0, 5),
        );
        let result = backend.emit_expr(&expr).unwrap();
        assert_eq!(result, "pow(2LL, 3LL)");
    }

    #[test]
    fn test_emit_eqv_operator() {
        let backend = CBackend::new();
        // a EQV b  =>  ~(a ^ b)
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1))),
                op: BinaryOp::Eqv,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7))),
            },
            BasicType::Long,
            Span::new(0, 7),
        );
        let result = backend.emit_expr(&expr).unwrap();
        assert_eq!(result, "(~(5LL ^ 3LL))");
    }

    #[test]
    fn test_emit_imp_operator() {
        let backend = CBackend::new();
        // a IMP b  =>  (~a) | b
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1))),
                op: BinaryOp::Imp,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7))),
            },
            BasicType::Long,
            Span::new(0, 7),
        );
        let result = backend.emit_expr(&expr).unwrap();
        assert_eq!(result, "((~5LL) | 3LL)");
    }

    #[test]
    fn test_escape_string() {
        let backend = CBackend::new();

        // Basic escapes
        assert_eq!(backend.escape_string("Hello"), "Hello");
        assert_eq!(backend.escape_string("Say \"Hi\""), "Say \\\"Hi\\\"");
        assert_eq!(backend.escape_string("path\\file"), "path\\\\file");
        assert_eq!(backend.escape_string("line1\nline2"), "line1\\nline2");
        assert_eq!(backend.escape_string("tab\there"), "tab\\there");

        // ASCII control characters
        assert_eq!(backend.escape_string("\x00\x1f"), "\\x00\\x1f");

        // Non-ASCII characters are escaped as UTF-8 bytes for C portability
        // 'é' (U+00E9) is encoded as bytes [0xC3, 0xA9] in UTF-8
        assert_eq!(backend.escape_string("é"), "\\xc3\\xa9");

        // Emoji: '😀' (U+1F600) is encoded as bytes [0xF0, 0x9F, 0x98, 0x80]
        assert_eq!(backend.escape_string("😀"), "\\xf0\\x9f\\x98\\x80");
    }

    #[test]
    fn test_collect_data_with_labels() {
        // Test that labels before DATA statements are correctly tracked
        let backend = CBackend::new();

        // Create a program with:
        // myLabel:
        // DATA 1, 2, 3
        // DATA 4, 5
        // anotherLabel:
        // DATA 6
        let program = TypedProgram::new(vec![
            TypedStatement::new(
                TypedStatementKind::Label {
                    name: "myLabel".to_string(),
                },
                Span::new(0, 8),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![
                        TypedDataValue::Integer(1),
                        TypedDataValue::Integer(2),
                        TypedDataValue::Integer(3),
                    ],
                },
                Span::new(9, 20),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![TypedDataValue::Integer(4), TypedDataValue::Integer(5)],
                },
                Span::new(21, 30),
            ),
            TypedStatement::new(
                TypedStatementKind::Label {
                    name: "anotherLabel".to_string(),
                },
                Span::new(31, 44),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![TypedDataValue::Integer(6)],
                },
                Span::new(45, 51),
            ),
        ]);

        let data_pool = backend.collect_data_values(&program);

        // Should have 6 data values total
        assert_eq!(data_pool.values.len(), 6);

        // myLabel should point to index 0 (first DATA)
        assert_eq!(data_pool.label_indices.get("MYLABEL"), Some(&0));

        // anotherLabel should point to index 5 (after the first 5 values)
        assert_eq!(data_pool.label_indices.get("ANOTHERLABEL"), Some(&5));
    }

    #[test]
    fn test_restore_with_label_codegen() {
        // Test that RESTORE with label generates correct code
        let program = TypedProgram::new(vec![
            TypedStatement::new(
                TypedStatementKind::Label {
                    name: "testLabel".to_string(),
                },
                Span::new(0, 10),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![TypedDataValue::Integer(1), TypedDataValue::Integer(2)],
                },
                Span::new(11, 20),
            ),
            TypedStatement::new(
                TypedStatementKind::Restore {
                    label: Some("testLabel".to_string()),
                },
                Span::new(21, 36),
            ),
        ]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        // Should contain RESTORE that sets pointer to 0 (the label's index)
        assert!(
            result
                .code
                .contains("_qb_data_ptr = 0; /* RESTORE testLabel */")
        );
    }
}
