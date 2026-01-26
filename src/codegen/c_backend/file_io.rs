//! File I/O code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for file operations:
//! - OPEN, CLOSE
//! - PRINT #, WRITE #
//! - INPUT #, LINE INPUT #
//! - GET, PUT
//! - SEEK

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

use crate::ast::{FileAccess, FileLock, FileMode, PrintSeparator};
use crate::semantic::typed_ir::{TypedExpr, TypedInputTarget, TypedPrintItem};
use crate::semantic::types::BasicType;

use super::expr::emit_expr;
use super::stmt::StmtEmitter;
use super::types::c_identifier;

impl StmtEmitter {
    // ==================== File I/O Helper Methods ====================

    /// Emits an OPEN statement.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn emit_open_file(
        &self,
        indent: &str,
        filename: &TypedExpr,
        mode: FileMode,
        access: Option<FileAccess>,
        lock: Option<FileLock>,
        file_num: &TypedExpr,
        record_len: Option<&TypedExpr>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let filename_code = emit_expr(filename, self.no_shell)?;
        let file_num_code = emit_expr(file_num, self.no_shell)?;

        // Determine C fopen mode string
        let c_mode = match mode {
            FileMode::Input => "\"r\"",
            FileMode::Output => "\"w\"",
            FileMode::Append => "\"a\"",
            FileMode::Binary => match access {
                Some(FileAccess::Read) => "\"rb\"",
                Some(FileAccess::Write) => "\"wb\"",
                _ => "\"r+b\"",
            },
            FileMode::Random => "\"r+b\"",
        };

        // File access and lock modes are not yet implemented in the runtime
        // access: READ, WRITE, READ WRITE
        // lock: SHARED, LOCK READ, LOCK WRITE, LOCK READ WRITE, ONLY
        let _ = access;
        let _ = lock;

        // For external runtime, use qb_file_open_str which accepts QbString* directly
        // For inline runtime, use ->data access
        let filename_access = match self.runtime_mode {
            super::RuntimeMode::External => filename_code.to_string(),
            super::RuntimeMode::Inline => format!("{}->data", filename_code),
        };

        match self.runtime_mode {
            super::RuntimeMode::External => {
                writeln_code!(
                    output,
                    "{}qb_file_open_str({}, {}, {});",
                    indent,
                    file_num_code,
                    filename_access,
                    c_mode
                )?;
            }
            super::RuntimeMode::Inline => {
                writeln_code!(
                    output,
                    "{}qb_file_open({}, {}, {});",
                    indent,
                    file_num_code,
                    filename_access,
                    c_mode
                )?;
            }
        }

        // Handle record length for random access
        if let Some(rec_len) = record_len {
            let rec_len_code = emit_expr(rec_len, self.no_shell)?;
            writeln_code!(
                output,
                "{}qb_file_set_reclen({}, {});",
                indent,
                file_num_code,
                rec_len_code
            )?;
        }

        Ok(())
    }

    /// Emits an OPEN statement using legacy syntax: OPEN mode$, [#]filenum, filename[, reclen]
    pub(super) fn emit_open_file_legacy(
        &self,
        indent: &str,
        mode_expr: &TypedExpr,
        file_num: &TypedExpr,
        filename: &TypedExpr,
        record_len: Option<&TypedExpr>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let mode_code = emit_expr(mode_expr, self.no_shell)?;
        let file_num_code = emit_expr(file_num, self.no_shell)?;
        let filename_code = emit_expr(filename, self.no_shell)?;

        // The mode is a string expression that we'll pass to a runtime function
        // that interprets "O", "I", "A", "R", "B" at runtime
        // For external runtime, we need to use qb_string_data() to get const char*
        let mode_access = match self.runtime_mode {
            super::RuntimeMode::External => format!("qb_string_data({})", mode_code),
            super::RuntimeMode::Inline => format!("{}->data", mode_code),
        };
        let filename_access = match self.runtime_mode {
            super::RuntimeMode::External => format!("qb_string_data({})", filename_code),
            super::RuntimeMode::Inline => format!("{}->data", filename_code),
        };

        writeln_code!(
            output,
            "{}qb_file_open_legacy({}, {}, {});",
            indent,
            file_num_code,
            mode_access,
            filename_access
        )?;

        // Handle record length for random access
        if let Some(rec_len) = record_len {
            let rec_len_code = emit_expr(rec_len, self.no_shell)?;
            writeln_code!(
                output,
                "{}qb_file_set_reclen({}, {});",
                indent,
                file_num_code,
                rec_len_code
            )?;
        }

        Ok(())
    }

    /// Emits a CLOSE statement.
    pub(super) fn emit_close_file(
        &self,
        indent: &str,
        file_nums: &[TypedExpr],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        if file_nums.is_empty() {
            // Close all files
            writeln_code!(output, "{}qb_file_close_all();", indent)?;
        } else {
            for file_num in file_nums {
                let file_num_code = emit_expr(file_num, self.no_shell)?;
                writeln_code!(output, "{}qb_file_close({});", indent, file_num_code)?;
            }
        }
        Ok(())
    }

    /// Emits a PRINT # statement.
    pub(super) fn emit_file_print(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        items: &[TypedPrintItem],
        newline: bool,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num, self.no_shell)?;

        for item in items {
            let expr_code = emit_expr(&item.expr, self.no_shell)?;

            if item.expr.basic_type.is_string() {
                // For external runtime, ensure fixed-length strings are converted to qb_string*
                let string_expr = if matches!(item.expr.basic_type, BasicType::FixedString(_)) {
                    format!("qb_str_from_c({})", expr_code)
                } else {
                    expr_code
                };
                writeln_code!(
                    output,
                    "{}qb_file_print_string({}, {});",
                    indent,
                    file_num_code,
                    string_expr
                )?;
            } else if item.expr.basic_type.is_float() {
                writeln_code!(
                    output,
                    "{}qb_file_print_float({}, {});",
                    indent,
                    file_num_code,
                    expr_code
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}qb_file_print_int({}, {});",
                    indent,
                    file_num_code,
                    expr_code
                )?;
            }

            if let Some(sep) = &item.separator {
                match sep {
                    PrintSeparator::Comma => {
                        writeln_code!(output, "{}qb_file_print_tab({});", indent, file_num_code)?;
                    }
                    PrintSeparator::Semicolon => {}
                }
            }
        }

        if newline {
            writeln_code!(
                output,
                "{}qb_file_print_newline({});",
                indent,
                file_num_code
            )?;
        }

        Ok(())
    }

    /// Emits a WRITE # statement.
    pub(super) fn emit_file_write(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        values: &[TypedExpr],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num, self.no_shell)?;

        for (i, value) in values.iter().enumerate() {
            let expr_code = emit_expr(value, self.no_shell)?;

            if value.basic_type.is_string() {
                // WRITE quotes strings
                writeln_code!(
                    output,
                    "{}qb_file_write_string({}, {});",
                    indent,
                    file_num_code,
                    expr_code
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}qb_file_write_number({}, {});",
                    indent,
                    file_num_code,
                    expr_code
                )?;
            }

            // Add comma separator except for last item
            if i < values.len() - 1 {
                writeln_code!(
                    output,
                    "{}qb_file_write_char({}, ',');",
                    indent,
                    file_num_code
                )?;
            }
        }

        // WRITE always ends with newline
        writeln_code!(
            output,
            "{}qb_file_print_newline({});",
            indent,
            file_num_code
        )?;

        Ok(())
    }

    /// Emits an INPUT # statement.
    pub(super) fn emit_file_input(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        targets: &[TypedInputTarget],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use TypedInputTarget::*;
        let file_num_code = emit_expr(file_num, self.no_shell)?;

        for target in targets {
            let (target_code, var_type) = match target {
                Variable { name, basic_type } => (c_identifier(name), basic_type.clone()),
                ArrayElement {
                    name,
                    indices,
                    element_type,
                } => {
                    let c_arr = c_identifier(name);
                    let idx_code: Vec<_> = indices
                        .iter()
                        .map(|e| emit_expr(e, self.no_shell))
                        .collect::<Result<_, _>>()?;
                    // Use first index for 1D array syntax
                    let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                    (format!("{}[{}]", c_arr, idx), element_type.clone())
                }
                ArrayElementField {
                    name,
                    indices,
                    fields,
                    field_type,
                } => {
                    let c_arr = c_identifier(name);
                    let idx_code: Vec<_> = indices
                        .iter()
                        .map(|e| emit_expr(e, self.no_shell))
                        .collect::<Result<_, _>>()?;
                    let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                    let field_chain = fields.join(".");
                    (
                        format!("{}[{}].{}", c_arr, idx, field_chain),
                        field_type.clone(),
                    )
                }
                Field {
                    name,
                    fields,
                    field_type,
                } => {
                    let c_var = c_identifier(name);
                    let field_chain = fields.join(".");
                    (format!("{}.{}", c_var, field_chain), field_type.clone())
                }
            };

            match var_type {
                BasicType::String | BasicType::FixedString(_) => {
                    writeln_code!(
                        output,
                        "{}qb_file_input_string({}, &{});",
                        indent,
                        file_num_code,
                        target_code
                    )?;
                }
                _ if var_type.is_float() => {
                    writeln_code!(
                        output,
                        "{}qb_file_input_float({}, &{});",
                        indent,
                        file_num_code,
                        target_code
                    )?;
                }
                _ => {
                    writeln_code!(
                        output,
                        "{}qb_file_input_int({}, &{});",
                        indent,
                        file_num_code,
                        target_code
                    )?;
                }
            }
        }

        Ok(())
    }

    /// Emits a LINE INPUT # statement.
    pub(super) fn emit_file_line_input(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        target: &TypedInputTarget,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use TypedInputTarget::*;

        let file_num_code = emit_expr(file_num, self.no_shell)?;

        let target_code = match target {
            Variable { name, .. } => c_identifier(name),
            ArrayElement { name, indices, .. } => {
                let c_arr = c_identifier(name);
                let idx_code: Vec<_> = indices
                    .iter()
                    .map(|e| emit_expr(e, self.no_shell))
                    .collect::<Result<_, _>>()?;
                let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                format!("{}[{}]", c_arr, idx)
            }
            ArrayElementField {
                name,
                indices,
                fields,
                ..
            } => {
                let c_arr = c_identifier(name);
                let idx_code: Vec<_> = indices
                    .iter()
                    .map(|e| emit_expr(e, self.no_shell))
                    .collect::<Result<_, _>>()?;
                let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                let field_chain = fields.join(".");
                format!("{}[{}].{}", c_arr, idx, field_chain)
            }
            Field { name, fields, .. } => {
                let c_name = c_identifier(name);
                let field_chain = fields.join(".");
                format!("{}.{}", c_name, field_chain)
            }
        };

        writeln_code!(
            output,
            "{}qb_file_line_input({}, &{});",
            indent,
            file_num_code,
            target_code
        )?;

        Ok(())
    }

    /// Emits a GET statement.
    pub(super) fn emit_file_get(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        position: Option<&TypedExpr>,
        target: &TypedInputTarget,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use crate::semantic::typed_ir::TypedInputTarget::*;

        let file_num_code = emit_expr(file_num, self.no_shell)?;

        // Seek to position if specified
        if let Some(pos) = position {
            let pos_code = emit_expr(pos, self.no_shell)?;
            writeln_code!(
                output,
                "{}qb_file_seek_record({}, {});",
                indent,
                file_num_code,
                pos_code
            )?;
        }

        // Get target code and type for size calculation
        let (target_code, var_type) = match target {
            Variable { name, basic_type } => (c_identifier(name), basic_type.clone()),
            ArrayElement {
                name,
                indices,
                element_type,
            } => {
                let c_arr = c_identifier(name);
                let idx_code: Vec<_> = indices
                    .iter()
                    .map(|e| emit_expr(e, self.no_shell))
                    .collect::<Result<_, _>>()?;
                let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                (format!("{}[{}]", c_arr, idx), element_type.clone())
            }
            ArrayElementField {
                name,
                indices,
                fields,
                field_type,
            } => {
                let c_arr = c_identifier(name);
                let idx_code: Vec<_> = indices
                    .iter()
                    .map(|e| emit_expr(e, self.no_shell))
                    .collect::<Result<_, _>>()?;
                let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                let field_chain = fields.join(".");
                (
                    format!("{}[{}].{}", c_arr, idx, field_chain),
                    field_type.clone(),
                )
            }
            Field {
                name,
                fields,
                field_type,
            } => {
                let c_name = c_identifier(name);
                let field_chain = fields.join(".");
                (format!("{}.{}", c_name, field_chain), field_type.clone())
            }
        };

        // For strings, use specialized function that reads into the string's data buffer
        if matches!(var_type, BasicType::String) {
            writeln_code!(
                output,
                "{}qb_file_get_string({}, {});",
                indent,
                file_num_code,
                target_code
            )?;
        } else {
            let size = type_size(&var_type);
            writeln_code!(
                output,
                "{}qb_file_get({}, &{}, {});",
                indent,
                file_num_code,
                target_code,
                size
            )?;
        }

        Ok(())
    }

    /// Emits a PUT statement.
    pub(super) fn emit_file_put(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        position: Option<&TypedExpr>,
        target: &TypedInputTarget,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use crate::semantic::typed_ir::TypedInputTarget::*;

        let file_num_code = emit_expr(file_num, self.no_shell)?;

        // Seek to position if specified
        if let Some(pos) = position {
            let pos_code = emit_expr(pos, self.no_shell)?;
            writeln_code!(
                output,
                "{}qb_file_seek_record({}, {});",
                indent,
                file_num_code,
                pos_code
            )?;
        }

        // Get target code and type for size calculation
        let (target_code, var_type) = match target {
            Variable { name, basic_type } => (c_identifier(name), basic_type.clone()),
            ArrayElement {
                name,
                indices,
                element_type,
            } => {
                let c_arr = c_identifier(name);
                let idx_code: Vec<_> = indices
                    .iter()
                    .map(|e| emit_expr(e, self.no_shell))
                    .collect::<Result<_, _>>()?;
                let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                (format!("{}[{}]", c_arr, idx), element_type.clone())
            }
            ArrayElementField {
                name,
                indices,
                fields,
                field_type,
            } => {
                let c_arr = c_identifier(name);
                let idx_code: Vec<_> = indices
                    .iter()
                    .map(|e| emit_expr(e, self.no_shell))
                    .collect::<Result<_, _>>()?;
                let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                let field_chain = fields.join(".");
                (
                    format!("{}[{}].{}", c_arr, idx, field_chain),
                    field_type.clone(),
                )
            }
            Field {
                name,
                fields,
                field_type,
            } => {
                let c_name = c_identifier(name);
                let field_chain = fields.join(".");
                (format!("{}.{}", c_name, field_chain), field_type.clone())
            }
        };

        // For strings, use specialized function that writes from the string's data buffer
        if matches!(var_type, BasicType::String) {
            writeln_code!(
                output,
                "{}qb_file_put_string({}, {});",
                indent,
                file_num_code,
                target_code
            )?;
        } else {
            let size = type_size(&var_type);
            writeln_code!(
                output,
                "{}qb_file_put({}, &{}, {});",
                indent,
                file_num_code,
                target_code,
                size
            )?;
        }

        Ok(())
    }

    /// Emits a SEEK statement.
    pub(super) fn emit_file_seek(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        position: &TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num, self.no_shell)?;
        let pos_code = emit_expr(position, self.no_shell)?;

        writeln_code!(
            output,
            "{}qb_file_seek({}, {});",
            indent,
            file_num_code,
            pos_code
        )?;

        Ok(())
    }
}

/// Returns the size in bytes for a type (for GET/PUT).
fn type_size(ty: &BasicType) -> &'static str {
    match ty {
        BasicType::Bit | BasicType::UnsignedBit => "1",
        BasicType::Byte | BasicType::UnsignedByte => "1",
        BasicType::Integer | BasicType::UnsignedInteger => "2",
        BasicType::Long | BasicType::UnsignedLong => "4",
        BasicType::Integer64 | BasicType::UnsignedInteger64 => "8",
        BasicType::Single => "4",
        BasicType::Double => "8",
        BasicType::Float => "sizeof(long double)",
        BasicType::Offset => "sizeof(uintptr_t)",
        BasicType::String => "sizeof(qb_string*)",
        BasicType::FixedString(len) => {
            // This is a bit tricky - we return a static string
            // In practice, we'd compute this dynamically
            let _ = len;
            "256" // Placeholder
        }
        BasicType::UserDefined(_) => "sizeof(void*)",
        BasicType::Array { .. } => "sizeof(void*)",
        BasicType::Mem => "sizeof(qb_mem)",
        BasicType::Void | BasicType::Unknown => "4",
    }
}
