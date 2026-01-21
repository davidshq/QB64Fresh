//! Statement code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for all statement types,
//! including control flow, I/O operations, procedure definitions, and more.
//!
//! # Loop Handling
//!
//! Loops are tracked on a stack to support EXIT statements. Each loop
//! type (FOR, WHILE, DO) generates a break label that EXIT can target.

use std::collections::HashMap;
use std::fmt::Write;

use crate::ast::{
    AllowFullScreenMode, EventControlMode, ExitType, FileAccess, FileLock, FileMode,
    FullScreenMode, ImageScaleMode, PrintSeparator,
};
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{
    TypedArrayDimension, TypedCaseCompareOp, TypedCaseMatch, TypedDoCondition, TypedExpr,
    TypedInputTarget, TypedMember, TypedParameter, TypedPrintItem, TypedReadTarget, TypedStatement,
    TypedStatementKind,
};
use crate::semantic::types::BasicType;

use super::expr::{c_function_name, emit_expr, escape_string};
use super::types::{c_identifier, c_type, default_init};

/// Context for the current loop (for EXIT statement handling).
#[derive(Clone)]
pub(super) struct LoopContext {
    /// Label to break to.
    pub break_label: String,
    /// Type of loop (For, While, Do).
    pub loop_type: ExitType,
}

/// State required for statement emission.
///
/// This is passed through recursive statement emission to track
/// indentation, loop context, and label generation.
pub(super) struct StmtEmitter {
    /// Counter for generating unique labels.
    pub label_counter: u32,
    /// Current indentation level.
    pub indent: usize,
    /// Stack of loop labels for EXIT statements.
    pub loop_stack: Vec<LoopContext>,
    /// Map of DATA labels to their indices (for RESTORE with label).
    pub data_label_indices: HashMap<String, usize>,
}

impl StmtEmitter {
    /// Creates a new statement emitter.
    pub fn new() -> Self {
        Self {
            label_counter: 0,
            indent: 0,
            loop_stack: Vec::new(),
            data_label_indices: HashMap::new(),
        }
    }

    /// Generates a unique label name.
    pub fn next_label(&mut self, prefix: &str) -> String {
        let label = format!("_qb_{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Returns the current indentation string.
    fn indent_str(&self) -> String {
        "    ".repeat(self.indent)
    }

    /// Emits a statement.
    pub fn emit_stmt(
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
                self.emit_assignment(&indent, name, value, target_type, output)?;
            }

            TypedStatementKind::ArrayAssignment {
                name,
                indices,
                value,
                dimensions,
                element_type,
            } => {
                self.emit_array_assignment(
                    &indent,
                    name,
                    indices,
                    value,
                    dimensions,
                    element_type,
                    output,
                )?;
            }

            TypedStatementKind::ArrayFieldAssignment {
                name,
                indices,
                fields,
                value,
                dimensions,
                element_type: _,
                field_type: _,
            } => {
                self.emit_array_field_assignment(
                    &indent, name, indices, fields, value, dimensions, output,
                )?;
            }

            TypedStatementKind::MidAssignment {
                target,
                start,
                length,
                value,
            } => {
                // Target is an lvalue (variable, array element, or field access)
                // We need to pass its address to qb_mid_assign
                let target_code = emit_expr(target)?;
                let start_code = emit_expr(start)?;
                let value_code = emit_expr(value)?;
                if let Some(len_expr) = length {
                    let len_code = emit_expr(len_expr)?;
                    writeln!(
                        output,
                        "{}qb_mid_assign(&({}), {}, {}, {});",
                        indent, target_code, start_code, len_code, value_code
                    )
                    .unwrap();
                } else {
                    // No length specified - use -1 to indicate "rest of string"
                    writeln!(
                        output,
                        "{}qb_mid_assign(&({}), {}, -1, {});",
                        indent, target_code, start_code, value_code
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

            TypedStatementKind::PrintUsing {
                format,
                values,
                newline,
            } => {
                let format_code = emit_expr(format)?;
                // Generate code to print each value using the format string
                // We use a runtime function that handles format string parsing
                if values.is_empty() {
                    // Just print the format string as-is if no values
                    writeln!(
                        output,
                        "{}qb_print_using({}, NULL, 0);",
                        indent, format_code
                    )
                    .unwrap();
                } else {
                    // Build array of values
                    writeln!(output, "{}{{", indent).unwrap();
                    writeln!(output, "{}    QbPrintValue _pv[{}];", indent, values.len()).unwrap();
                    for (i, value) in values.iter().enumerate() {
                        let value_code = emit_expr(value)?;
                        match &value.basic_type {
                            BasicType::String => {
                                writeln!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_STRING; _pv[{}].str_val = {};",
                                    indent, i, i, value_code
                                )
                                .unwrap();
                            }
                            BasicType::Integer | BasicType::Long => {
                                writeln!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_INT; _pv[{}].int_val = (int64_t){};",
                                    indent, i, i, value_code
                                )
                                .unwrap();
                            }
                            BasicType::Single | BasicType::Double => {
                                writeln!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_DOUBLE; _pv[{}].dbl_val = (double){};",
                                    indent, i, i, value_code
                                )
                                .unwrap();
                            }
                            _ => {
                                // For other types, try to convert to double
                                writeln!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_DOUBLE; _pv[{}].dbl_val = (double){};",
                                    indent, i, i, value_code
                                )
                                .unwrap();
                            }
                        }
                    }
                    writeln!(
                        output,
                        "{}    qb_print_using({}, _pv, {});",
                        indent,
                        format_code,
                        values.len()
                    )
                    .unwrap();
                    writeln!(output, "{}}}", indent).unwrap();
                }
                if *newline {
                    writeln!(output, "{}qb_print_newline();", indent).unwrap();
                }
            }

            TypedStatementKind::Input {
                prompt,
                show_question_mark,
                same_line,
                targets,
            } => {
                self.emit_input(
                    &indent,
                    prompt,
                    *show_question_mark,
                    *same_line,
                    targets,
                    output,
                )?;
            }

            TypedStatementKind::LineInput { prompt, target } => {
                use TypedInputTarget::*;

                let target_code = match target {
                    Variable { name, .. } => c_identifier(name),
                    ArrayElement { name, indices, .. } => {
                        let c_arr = c_identifier(name);
                        let idx_code: Vec<_> =
                            indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
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
                        let idx_code: Vec<_> =
                            indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
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
                let prompt_arg = match prompt {
                    Some(p) => format!("\"{}\"", escape_string(p)),
                    None => "NULL".to_string(),
                };
                writeln!(
                    output,
                    "{}qb_input_string({}, &{});",
                    indent, prompt_arg, target_code
                )
                .unwrap();
            }

            TypedStatementKind::If {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                self.emit_if(
                    &indent,
                    condition,
                    then_branch,
                    elseif_branches,
                    else_branch,
                    output,
                )?;
            }

            TypedStatementKind::SelectCase {
                test_expr,
                cases,
                case_else,
            } => {
                self.emit_select_case(&indent, test_expr, cases, case_else, output)?;
            }

            TypedStatementKind::For {
                variable,
                var_type,
                start,
                end,
                step,
                body,
            } => {
                self.emit_for(&indent, variable, var_type, start, end, step, body, output)?;
            }

            TypedStatementKind::While { condition, body } => {
                self.emit_while(&indent, condition, body, output)?;
            }

            TypedStatementKind::DoLoop {
                pre_condition,
                body,
                post_condition,
            } => {
                self.emit_do_loop(&indent, pre_condition, body, post_condition, output)?;
            }

            TypedStatementKind::Goto { target } => {
                let c_label = c_identifier(target);
                writeln!(output, "{}goto {};", indent, c_label).unwrap();
            }

            TypedStatementKind::Gosub { target } => {
                let c_label = c_identifier(target);
                let return_label = self.next_label("gosub_ret");
                // Push return address onto stack and jump to subroutine
                writeln!(
                    output,
                    "{}_gosub_stack[_gosub_sp++] = &&{};",
                    indent, return_label
                )
                .unwrap();
                writeln!(output, "{}goto {};", indent, c_label).unwrap();
                writeln!(output, "{}{}:;", indent, return_label).unwrap();
            }

            TypedStatementKind::Return => {
                // RETURN from GOSUB - pop return address from stack and jump
                writeln!(
                    output,
                    "{}if (_gosub_sp > 0) goto *_gosub_stack[--_gosub_sp];",
                    indent
                )
                .unwrap();
            }

            TypedStatementKind::Exit { exit_type } => {
                self.emit_exit(&indent, exit_type, output)?;
            }

            TypedStatementKind::End => {
                writeln!(output, "{}exit(0);", indent).unwrap();
            }

            TypedStatementKind::Stop => {
                writeln!(output, "{}/* STOP */", indent).unwrap();
                writeln!(output, "{}exit(1);", indent).unwrap();
            }

            TypedStatementKind::System => {
                writeln!(output, "{}exit(0);", indent).unwrap();
            }

            TypedStatementKind::Sleep { seconds } => {
                if let Some(secs) = seconds {
                    let secs_code = emit_expr(secs)?;
                    writeln!(output, "{}qb_sleep((int){});", indent, secs_code).unwrap();
                } else {
                    // No argument - wait for keypress
                    writeln!(output, "{}qb_sleep_keypress();", indent).unwrap();
                }
            }

            TypedStatementKind::Wait {
                port,
                and_mask,
                xor_mask,
            } => {
                let port_code = emit_expr(port)?;
                let and_code = emit_expr(and_mask)?;
                if let Some(xor) = xor_mask {
                    let xor_code = emit_expr(xor)?;
                    writeln!(
                        output,
                        "{}qb_wait((int){}, (int){}, (int){});",
                        indent, port_code, and_code, xor_code
                    )
                    .unwrap();
                } else {
                    writeln!(
                        output,
                        "{}qb_wait((int){}, (int){}, 0);",
                        indent, port_code, and_code
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::Delay { seconds } => {
                let secs_code = emit_expr(seconds)?;
                writeln!(output, "{}qb_delay({});", indent, secs_code).unwrap();
            }

            TypedStatementKind::Limit { fps } => {
                let fps_code = emit_expr(fps)?;
                writeln!(output, "{}qb_limit((int){});", indent, fps_code).unwrap();
            }

            TypedStatementKind::Erase { arrays } => {
                for array_name in arrays {
                    let c_name = c_identifier(array_name).to_lowercase();
                    writeln!(output, "{}qb_array_erase(&arr_{});", indent, c_name).unwrap();
                }
            }

            TypedStatementKind::KeyClear => {
                writeln!(output, "{}qb_keyclear();", indent).unwrap();
            }

            TypedStatementKind::Call { name, args } => {
                let args_code: Result<Vec<_>, _> = args.iter().map(emit_expr).collect();
                let args_str = args_code?.join(", ");
                // Check for built-in SUBs with special C function names
                let c_name = match name.to_uppercase().as_str() {
                    "_WRITEFILE" => "qb_writefile".to_string(),
                    "_EXIT" => "qb_exit".to_string(),
                    "_ACCEPTFILEDROP" => "qb_acceptfiledrop".to_string(),
                    "_FINISHDROP" => "qb_finishdrop".to_string(),
                    "_CONSOLECURSOR" => "qb_consolecursor".to_string(),
                    "_CONSOLEFONT" => "qb_consolefont".to_string(),
                    "_CONTROLCHR" => "qb_controlchr".to_string(),
                    "_SETALPHA" => "qb_setalpha".to_string(),
                    "_PALETTECOLOR" => "qb_palettecolor".to_string(),
                    "_COPYPALETTE" => "qb_copypalette".to_string(),
                    "_BLEND" => "qb_blend".to_string(),
                    "_DONTBLEND" => "qb_dontblend".to_string(),
                    "_CLEARCOLOR" => "qb_clearcolor".to_string(),
                    "_DEPTHBUFFER" => "qb_depthbuffer".to_string(),
                    "_DISPLAYORDER" => "qb_displayorder".to_string(),
                    "_SNDLIMIT" => "qb_sndlimit".to_string(),
                    "_ICON" => "qb_icon".to_string(),
                    "_HIDE" => "qb_hide".to_string(),
                    "_SHOW" => "qb_show".to_string(),
                    "_ONTOP" => "qb_ontop".to_string(),
                    "_PRINTMODE" => "qb_printmode".to_string(),
                    // Session 032+ SUBs
                    "_SAVEIMAGE" => "qb_saveimage".to_string(),
                    "_SCREENPRINT" => "qb_screenprint".to_string(),
                    "_UPRINTSTRING" => "qb_uprintstring".to_string(),
                    "_MAPUNICODE" => "qb_mapunicode".to_string(),
                    "_LOGTRACE" => "qb_logtrace".to_string(),
                    "_LOGINFO" => "qb_loginfo".to_string(),
                    "_LOGWARN" => "qb_logwarn".to_string(),
                    "_LOGERROR" => "qb_logerror".to_string(),
                    "_LOGMINLEVEL" => "qb_logminlevel".to_string(),
                    "_SNDRAWBATCH" => "qb_sndrawbatch".to_string(),
                    "_MIDISOUNDBANK" => "qb_midisoundbank".to_string(),
                    "_NEWHANDLER" => "qb_newhandler".to_string(),
                    // Session 033+ SUBs
                    "_PRINTIMAGE" => "qb_printimage".to_string(),
                    "_CLEAR" => "qb_clear_resource".to_string(),
                    "_TOGGLE" => "qb_toggle".to_string(),
                    "_MAPTRIANGLE" => "qb_maptriangle".to_string(),
                    "_GLRENDER" => "qb_glrender".to_string(),
                    // Default: user-defined SUBs use qb_sub_ prefix
                    _ => format!("qb_sub_{}", c_identifier(name).to_lowercase()),
                };
                writeln!(output, "{}{}({});", indent, c_name, args_str).unwrap();
            }

            TypedStatementKind::SubDefinition {
                name,
                params,
                body,
                is_static: _,
            } => {
                self.emit_sub_definition(&indent, name, params, body, output)?;
            }

            TypedStatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                body,
                is_static: _,
            } => {
                self.emit_function_definition(&indent, name, params, return_type, body, output)?;
            }

            TypedStatementKind::Dim {
                variables,
                shared: _,
            } => {
                for var in variables {
                    self.emit_dim(&indent, &var.name, &var.basic_type, &var.dimensions, output)?;
                }
            }

            TypedStatementKind::Const { definitions } => {
                for (name, value, _basic_type) in definitions {
                    let c_name = c_identifier(name);
                    let value_code = emit_expr(value)?;
                    writeln!(
                        output,
                        "{}const {} {} = {};",
                        indent,
                        c_type(&value.basic_type),
                        c_name,
                        value_code
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::DefType => {
                // DEFxxx statements affect type inference but generate no C code
            }

            TypedStatementKind::OptionBase => {
                // OPTION BASE affects array bounds but generates no C code
            }

            TypedStatementKind::DefSeg { segment } => {
                // DEF SEG is a legacy statement for memory segment manipulation.
                // In modern QB64, this is largely a no-op, but we can emit a runtime call
                // for compatibility with PEEK/POKE/BLOAD/BSAVE.
                if let Some(seg_expr) = segment {
                    let seg_code = emit_expr(seg_expr)?;
                    writeln!(output, "{}qb_def_seg((int32_t){});", indent, seg_code).unwrap();
                } else {
                    // DEF SEG without argument resets to default segment
                    writeln!(output, "{}qb_def_seg(-1);", indent).unwrap();
                }
            }

            TypedStatementKind::Poke { address, value } => {
                // POKE writes a byte to memory within the current segment.
                let addr_code = emit_expr(address)?;
                let val_code = emit_expr(value)?;
                writeln!(
                    output,
                    "{}qb_poke((int32_t){}, (uint8_t){});",
                    indent, addr_code, val_code
                )
                .unwrap();
            }

            TypedStatementKind::Label { name } => {
                let c_label = c_identifier(name);
                writeln!(output, "{}:", c_label).unwrap();
            }

            TypedStatementKind::Comment(text) => {
                writeln!(output, "{}/* {} */", indent, text).unwrap();
            }

            TypedStatementKind::Expression(expr) => {
                let expr_code = emit_expr(expr)?;
                writeln!(output, "{}{};", indent, expr_code).unwrap();
            }

            TypedStatementKind::IncludeDirective { path } => {
                writeln!(output, "{}/* $INCLUDE: '{}' */", indent, path).unwrap();
            }

            TypedStatementKind::ConditionalBlock {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                // Unevaluated conditional block - emit all branches as comments
                // (This case should be rare now that conditions are evaluated)
                writeln!(output, "{}/* $IF {} */", indent, condition).unwrap();
                for s in then_branch {
                    self.emit_stmt(s, output)?;
                }
                for (elseif_cond, elseif_body) in elseif_branches {
                    writeln!(output, "{}/* $ELSEIF {} */", indent, elseif_cond).unwrap();
                    for s in elseif_body {
                        self.emit_stmt(s, output)?;
                    }
                }
                if let Some(else_body) = else_branch {
                    writeln!(output, "{}/* $ELSE */", indent).unwrap();
                    for s in else_body {
                        self.emit_stmt(s, output)?;
                    }
                }
                writeln!(output, "{}/* $END IF */", indent).unwrap();
            }

            TypedStatementKind::ConditionalBlockResolved {
                original_condition,
                statements,
            } => {
                // Evaluated conditional block - only emit the selected branch
                if !statements.is_empty() {
                    writeln!(
                        output,
                        "{}/* Conditional compilation: {} */",
                        indent, original_condition
                    )
                    .unwrap();
                    for s in statements {
                        self.emit_stmt(s, output)?;
                    }
                }
                // If statements is empty, no code is emitted (condition was false
                // and there was no matching branch)
            }

            TypedStatementKind::MetaCommand { command, args } => {
                let args_str = args.as_deref().unwrap_or("");
                writeln!(output, "{}/* ${} {} */", indent, command, args_str).unwrap();
            }

            TypedStatementKind::MetaLet { name, value } => {
                // Compile-time variable assignment - generates a comment
                writeln!(output, "{}/* $LET {} = {} */", indent, name, value).unwrap();
            }

            TypedStatementKind::MetaChecking { enabled } => {
                // Compile-time bounds checking directive - generates a comment
                let state = if *enabled { "ON" } else { "OFF" };
                writeln!(output, "{}/* $CHECKING:{} */", indent, state).unwrap();
            }

            TypedStatementKind::MetaConsole { only } => {
                // Console mode directive - affects program initialization
                // For now, generate a comment; actual console setup is runtime-dependent
                if *only {
                    writeln!(output, "{}/* $CONSOLE:ONLY - console-only mode */", indent).unwrap();
                } else {
                    writeln!(output, "{}/* $CONSOLE - enable console window */", indent).unwrap();
                }
            }

            TypedStatementKind::MetaScreenHide => {
                // Hide graphics window on startup
                writeln!(output, "{}/* $SCREENHIDE */", indent).unwrap();
            }

            TypedStatementKind::MetaScreenShow => {
                // Show graphics window on startup (default)
                writeln!(output, "{}/* $SCREENSHOW */", indent).unwrap();
            }

            TypedStatementKind::Swap { left, right } => {
                let left_code = emit_expr(left)?;
                let right_code = emit_expr(right)?;
                let c_ty = c_type(&left.basic_type);

                let temp_var = self.next_label("swap_temp");
                writeln!(output, "{}{} {} = {};", indent, c_ty, temp_var, left_code).unwrap();
                writeln!(output, "{}{} = {};", indent, left_code, right_code).unwrap();
                writeln!(output, "{}{} = {};", indent, right_code, temp_var).unwrap();
            }

            TypedStatementKind::Continue { continue_type } => {
                let _ = continue_type;
                writeln!(output, "{}continue;", indent).unwrap();
            }

            TypedStatementKind::TypeDefinition {
                name,
                members,
                custom_type,
            } => {
                self.emit_type_definition(&indent, name, members, *custom_type, output)?;
            }

            TypedStatementKind::Data { .. } => {
                // DATA statements are handled in collect_data_values during global emission
            }

            TypedStatementKind::Read { targets } => {
                self.emit_read(&indent, targets, output)?;
            }

            TypedStatementKind::Restore { label } => {
                self.emit_restore(&indent, label, output)?;
            }

            TypedStatementKind::Randomize { seed } => {
                if let Some(seed_expr) = seed {
                    // RANDOMIZE expr - seed with specific value
                    // TIMER is just a function call in the expression, no special handling needed
                    let seed_code = emit_expr(seed_expr)?;
                    writeln!(output, "{}qb_randomize((double)({}));", indent, seed_code).unwrap();
                } else {
                    // RANDOMIZE without arguments - for compatibility, use timer
                    // (in original BASIC, this would prompt the user)
                    writeln!(output, "{}qb_randomize_timer();", indent).unwrap();
                }
            }

            // ==================== File I/O Statements ====================
            TypedStatementKind::OpenFile {
                filename,
                mode,
                access,
                lock,
                file_num,
                record_len,
            } => {
                self.emit_open_file(
                    &indent,
                    filename,
                    *mode,
                    *access,
                    *lock,
                    file_num,
                    record_len.as_ref(),
                    output,
                )?;
            }

            TypedStatementKind::CloseFile { file_nums } => {
                self.emit_close_file(&indent, file_nums, output)?;
            }

            TypedStatementKind::FilePrint {
                file_num,
                items,
                newline,
            } => {
                self.emit_file_print(&indent, file_num, items, *newline, output)?;
            }

            TypedStatementKind::FileWrite { file_num, values } => {
                self.emit_file_write(&indent, file_num, values, output)?;
            }

            TypedStatementKind::FileInput { file_num, targets } => {
                self.emit_file_input(&indent, file_num, targets, output)?;
            }

            TypedStatementKind::FileLineInput { file_num, target } => {
                self.emit_file_line_input(&indent, file_num, target, output)?;
            }

            TypedStatementKind::FileGet {
                file_num,
                position,
                variable,
                var_type,
                index,
            } => {
                self.emit_file_get(
                    &indent,
                    file_num,
                    position.as_ref(),
                    variable,
                    var_type,
                    index.as_ref(),
                    output,
                )?;
            }

            TypedStatementKind::FilePut {
                file_num,
                position,
                variable,
                var_type,
                index,
            } => {
                self.emit_file_put(
                    &indent,
                    file_num,
                    position.as_ref(),
                    variable,
                    var_type,
                    index.as_ref(),
                    output,
                )?;
            }

            TypedStatementKind::FileSeek { file_num, position } => {
                self.emit_file_seek(&indent, file_num, position, output)?;
            }

            // ==================== Error Handling Statements ====================
            TypedStatementKind::OnErrorGoto { target } => {
                self.emit_on_error_goto(&indent, target, output)?;
            }

            TypedStatementKind::OnErrorResumeNext => {
                self.emit_on_error_resume_next(&indent, output)?;
            }

            TypedStatementKind::ResumeStmt { target } => {
                self.emit_resume(&indent, target, output)?;
            }

            TypedStatementKind::ErrorStmt { code } => {
                self.emit_error_stmt(&indent, code, output)?;
            }

            // ==================== Computed Control Flow ====================
            TypedStatementKind::OnGoto { selector, targets } => {
                self.emit_on_goto(&indent, selector, targets, output)?;
            }

            TypedStatementKind::OnGosub { selector, targets } => {
                self.emit_on_gosub(&indent, selector, targets, output)?;
            }

            // ==================== DEF FN ====================
            TypedStatementKind::DefFn {
                name,
                params,
                return_type,
                body,
            } => {
                self.emit_def_fn(name, params, return_type, body, output)?;
            }

            TypedStatementKind::DefFnMultiLine {
                name,
                params,
                return_type,
                body,
            } => {
                self.emit_def_fn_multiline(name, params, return_type, body, output)?;
            }

            // ==================== Variable/Scope Statements ====================
            TypedStatementKind::CommonStmt { shared, variables } => {
                // COMMON is handled at program level, emit a comment here
                let _ = shared;
                let _ = variables;
                writeln!(
                    output,
                    "{}/* COMMON statement - handled at program level */",
                    indent
                )
                .unwrap();
            }

            TypedStatementKind::SharedStmt { variables } => {
                // SHARED inside SUB/FUNCTION declares access to module-level shared vars
                // In C, these are already global, so just emit a comment
                let _ = variables;
                writeln!(
                    output,
                    "{}/* SHARED statement - variables accessed from module level */",
                    indent
                )
                .unwrap();
            }

            TypedStatementKind::StaticStmt { variables } => {
                // STATIC inside SUB/FUNCTION declares static local variables
                // In C, these are declared with the `static` keyword
                for var in variables {
                    let c_name = c_identifier(&var.name);
                    let c_ty = c_type(&var.basic_type);

                    if var.dimensions.is_empty() {
                        // Simple static variable
                        let init = default_init(&var.basic_type);
                        writeln!(output, "{}static {} {} = {};", indent, c_ty, c_name, init)
                            .unwrap();
                    } else {
                        // Static array
                        let sizes: Vec<String> = var
                            .dimensions
                            .iter()
                            .map(|d| format!("{}", d.upper - d.lower + 1))
                            .collect();
                        let array_dims = sizes.join("][");
                        writeln!(
                            output,
                            "{}static {} {}[{}] = {{0}};",
                            indent, c_ty, c_name, array_dims
                        )
                        .unwrap();
                    }
                }
            }

            TypedStatementKind::Redim {
                preserve,
                shared: _shared,
                variables,
            } => {
                // Note: SHARED affects symbol visibility (handled in semantic analysis),
                // but the generated code is the same - the array is allocated dynamically.
                // For C codegen, SHARED arrays are just global variables that can be
                // redimensioned at runtime.
                for var in variables {
                    self.emit_redim(
                        &indent,
                        *preserve,
                        &var.name,
                        &var.element_type,
                        &var.dimensions,
                        output,
                    )?;
                }
            }

            // ==================== Graphics Statements ====================
            TypedStatementKind::Screen {
                mode,
                color_switch,
                active_page,
                visual_page,
            } => {
                // SCREEN [mode][,[colorswitch]][,[apage]][,[vpage]]
                let mode_code = mode
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let color_code = color_switch
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let apage_code = active_page
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let vpage_code = visual_page
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                writeln!(
                    output,
                    "{}qb_gfx_screen((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){});",
                    indent, mode_code, color_code, apage_code, vpage_code
                )
                .unwrap();
            }

            TypedStatementKind::Cls { mode } => {
                if let Some(mode_expr) = mode {
                    let mode_code = emit_expr(mode_expr)?;
                    writeln!(output, "{}qb_gfx_cls_mode((int32_t){});", indent, mode_code).unwrap();
                } else {
                    writeln!(output, "{}qb_gfx_cls();", indent).unwrap();
                }
            }

            TypedStatementKind::Color {
                foreground,
                background,
                border,
            } => {
                // Use -1 as sentinel for "unchanged" - runtime will check this
                let fg_code = foreground
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let bg_code = background
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                // Border is ignored in modern systems (was CGA/EGA text mode only)
                // We accept it for compatibility but don't use it
                let _border_code = border.as_ref().map(emit_expr).transpose()?;
                writeln!(
                    output,
                    "{}qb_gfx_color((int32_t){}, (int32_t){});",
                    indent, fg_code, bg_code
                )
                .unwrap();
            }

            TypedStatementKind::Locate { row, col } => {
                // LOCATE with optional parameters - use -1 to indicate "unchanged"
                let row_code = row
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or("-1".to_string());
                let col_code = col
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or("-1".to_string());
                writeln!(
                    output,
                    "{}qb_gfx_locate((int32_t){}, (int32_t){});",
                    indent, row_code, col_code
                )
                .unwrap();
            }

            TypedStatementKind::Pset { step, x, y, color } => {
                let x_code = emit_expr(x)?;
                let y_code = emit_expr(y)?;
                let step_int = if *step { 1 } else { 0 };
                if let Some(c) = color {
                    let c_code = emit_expr(c)?;
                    writeln!(
                        output,
                        "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, (uint32_t){}, {});",
                        indent, x_code, y_code, c_code, step_int
                    )
                    .unwrap();
                } else {
                    // Use current foreground color (pass -1 to signal "use current")
                    writeln!(
                        output,
                        "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, 0xFFFFFFFF, {});",
                        indent, x_code, y_code, step_int
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::Preset { step, x, y } => {
                let x_code = emit_expr(x)?;
                let y_code = emit_expr(y)?;
                let step_int = if *step { 1 } else { 0 };
                // PRESET plots in background color - pass 0 (black) by default
                writeln!(
                    output,
                    "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, 0xFF000000, {});",
                    indent, x_code, y_code, step_int
                )
                .unwrap();
            }

            TypedStatementKind::Line {
                x1,
                y1,
                x2,
                y2,
                step2,
                color,
                box_style,
                style: _, // TODO: implement line style pattern support
            } => {
                let x2_code = emit_expr(x2)?;
                let y2_code = emit_expr(y2)?;
                let color_code = if let Some(c) = color {
                    emit_expr(c)?
                } else {
                    "0xFFFFFFFF".to_string() // Use current foreground
                };

                // Handle optional start coordinates (use 0,0 as default for now)
                let x1_code = if let Some(e) = x1 {
                    emit_expr(e)?
                } else {
                    "0".to_string()
                };
                let y1_code = if let Some(e) = y1 {
                    emit_expr(e)?
                } else {
                    "0".to_string()
                };

                // step2 flag indicates x2/y2 are relative to x1/y1
                let step_flag = if *step2 { "1" } else { "0" };

                match box_style {
                    None => {
                        // Plain line
                        writeln!(output, "{}qb_gfx_line_ex((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {}, (uint32_t){});",
                                 indent, x1_code, y1_code, x2_code, y2_code, step_flag, color_code).unwrap();
                    }
                    Some(false) => {
                        // Box (outline)
                        writeln!(output, "{}qb_gfx_box_ex((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {}, (uint32_t){}, 0);",
                                 indent, x1_code, y1_code, x2_code, y2_code, step_flag, color_code).unwrap();
                    }
                    Some(true) => {
                        // Filled box
                        writeln!(output, "{}qb_gfx_box_ex((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {}, (uint32_t){}, 1);",
                                 indent, x1_code, y1_code, x2_code, y2_code, step_flag, color_code).unwrap();
                    }
                }
            }

            TypedStatementKind::Circle {
                step,
                x,
                y,
                radius,
                color,
                filled,
            } => {
                let x_code = emit_expr(x)?;
                let y_code = emit_expr(y)?;
                let r_code = emit_expr(radius)?;
                let color_code = if let Some(c) = color {
                    emit_expr(c)?
                } else {
                    "0xFFFFFFFF".to_string()
                };
                let filled_int = if *filled { 1 } else { 0 };
                let step_int = if *step { 1 } else { 0 };
                writeln!(
                    output,
                    "{}qb_gfx_circle_step((int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, {}, {});",
                    indent, x_code, y_code, r_code, color_code, filled_int, step_int
                )
                .unwrap();
            }

            TypedStatementKind::Paint {
                step,
                x,
                y,
                color,
                border,
            } => {
                let x_code = emit_expr(x)?;
                let y_code = emit_expr(y)?;
                let color_code = if let Some(c) = color {
                    emit_expr(c)?
                } else {
                    "0xFFFFFFFF".to_string()
                };
                let border_code = if let Some(b) = border {
                    emit_expr(b)?
                } else {
                    color_code.clone() // Default border = fill color
                };
                let step_int = if *step { 1 } else { 0 };
                writeln!(
                    output,
                    "{}qb_gfx_paint_step((int32_t){}, (int32_t){}, (uint32_t){}, (uint32_t){}, {});",
                    indent, x_code, y_code, color_code, border_code, step_int
                )
                .unwrap();
            }

            TypedStatementKind::GfxDisplay => {
                writeln!(output, "{}qb_gfx_display();", indent).unwrap();
            }

            TypedStatementKind::Palette { attribute, color } => {
                match (attribute, color) {
                    (Some(attr), Some(col)) => {
                        let attr_code = emit_expr(attr)?;
                        let col_code = emit_expr(col)?;
                        writeln!(
                            output,
                            "{}qb_gfx_palette((int32_t){}, (uint32_t){});",
                            indent, attr_code, col_code
                        )
                        .unwrap();
                    }
                    _ => {
                        // PALETTE without arguments - reset all palette entries
                        writeln!(output, "{}qb_gfx_palette_reset();", indent).unwrap();
                    }
                }
            }

            TypedStatementKind::Pcopy { source, dest } => {
                let src_code = emit_expr(source)?;
                let dst_code = emit_expr(dest)?;
                writeln!(
                    output,
                    "{}qb_gfx_pcopy((int32_t){}, (int32_t){});",
                    indent, src_code, dst_code
                )
                .unwrap();
            }

            // ==================== Additional Graphics Statements ====================
            TypedStatementKind::Width { columns, rows } => {
                let cols_code = emit_expr(columns)?;
                if let Some(r) = rows {
                    let rows_code = emit_expr(r)?;
                    writeln!(
                        output,
                        "{}qb_gfx_set_width((uint32_t){}, (uint32_t){});",
                        indent, cols_code, rows_code
                    )
                    .unwrap();
                } else {
                    writeln!(
                        output,
                        "{}qb_gfx_set_width((uint32_t){}, 0);",
                        indent, cols_code
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::View {
                screen,
                coords,
                fill_color,
                border_color,
            } => {
                let screen_int = if *screen { 1 } else { 0 };
                if let Some(c) = coords {
                    let x1 = emit_expr(&c.x1)?;
                    let y1 = emit_expr(&c.y1)?;
                    let x2 = emit_expr(&c.x2)?;
                    let y2 = emit_expr(&c.y2)?;
                    let fill = fill_color
                        .as_ref()
                        .map(emit_expr)
                        .transpose()?
                        .unwrap_or_else(|| "-1".to_string());
                    let border = border_color
                        .as_ref()
                        .map(emit_expr)
                        .transpose()?
                        .unwrap_or_else(|| "-1".to_string());
                    writeln!(output, "{}qb_gfx_view({}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){});",
                             indent, screen_int, x1, y1, x2, y2, fill, border).unwrap();
                } else {
                    // Reset viewport
                    writeln!(output, "{}qb_gfx_view_reset();", indent).unwrap();
                }
            }

            TypedStatementKind::ViewPrint { top, bottom } => {
                if let (Some(t), Some(b)) = (top, bottom) {
                    let top_code = emit_expr(t)?;
                    let bottom_code = emit_expr(b)?;
                    writeln!(
                        output,
                        "{}qb_view_print((int32_t){}, (int32_t){});",
                        indent, top_code, bottom_code
                    )
                    .unwrap();
                } else {
                    // Reset text viewport
                    writeln!(output, "{}qb_view_print_reset();", indent).unwrap();
                }
            }

            TypedStatementKind::WindowCoords { screen, coords } => {
                let screen_int = if *screen { 1 } else { 0 };
                if let Some(c) = coords {
                    let x1 = emit_expr(&c.x1)?;
                    let y1 = emit_expr(&c.y1)?;
                    let x2 = emit_expr(&c.x2)?;
                    let y2 = emit_expr(&c.y2)?;
                    writeln!(
                        output,
                        "{}qb_gfx_window({}, (double){}, (double){}, (double){}, (double){});",
                        indent, screen_int, x1, y1, x2, y2
                    )
                    .unwrap();
                } else {
                    // Reset window coordinates
                    writeln!(output, "{}qb_gfx_window_reset();", indent).unwrap();
                }
            }

            TypedStatementKind::DrawCmd { commands } => {
                let cmd_code = emit_expr(commands)?;
                writeln!(output, "{}qb_gfx_draw({});", indent, cmd_code).unwrap();
            }

            TypedStatementKind::GraphicsGet {
                step1,
                x1,
                y1,
                step2,
                x2,
                y2,
                array_name,
                array_indices,
            } => {
                let x1_code = emit_expr(x1)?;
                let y1_code = emit_expr(y1)?;
                let x2_code = emit_expr(x2)?;
                let y2_code = emit_expr(y2)?;
                let arr_name = c_identifier(array_name);

                // Calculate array pointer - either base or with offset
                let arr_ptr = if array_indices.is_empty() {
                    arr_name.clone()
                } else {
                    // For multi-dimensional arrays, generate index expression
                    let indices: Vec<String> = array_indices
                        .iter()
                        .map(emit_expr)
                        .collect::<Result<_, _>>()?;
                    format!("&{}[{}]", arr_name, indices.join("]["))
                };

                // Generate appropriate function call based on step flags
                // step1 affects (x1, y1), step2 affects (x2, y2)
                let func_name = match (*step1, *step2) {
                    (false, false) => "qb_gfx_get",
                    (false, true) => "qb_gfx_get_step2",
                    (true, false) => "qb_gfx_get_step1",
                    (true, true) => "qb_gfx_get_step_both",
                };
                writeln!(
                    output,
                    "{}{}((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                    indent, func_name, x1_code, y1_code, x2_code, y2_code, arr_ptr
                )
                .unwrap();
            }

            TypedStatementKind::GraphicsPut {
                x,
                y,
                step,
                array_name,
                array_indices,
                clip,
                action,
                transparent_color,
            } => {
                use crate::ast::PutAction;

                let x_code = emit_expr(x)?;
                let y_code = emit_expr(y)?;
                let arr_name = c_identifier(array_name);

                // Calculate array pointer
                let arr_ptr = if array_indices.is_empty() {
                    arr_name.clone()
                } else {
                    // For multi-dimensional arrays, generate index expression
                    let indices: Vec<String> = array_indices
                        .iter()
                        .map(emit_expr)
                        .collect::<Result<_, _>>()?;
                    format!("&{}[{}]", arr_name, indices.join("]["))
                };

                // Map action to C constant
                let action_code = match action {
                    PutAction::Xor => "QB_PUT_XOR",
                    PutAction::Pset => "QB_PUT_PSET",
                    PutAction::Preset => "QB_PUT_PRESET",
                    PutAction::And => "QB_PUT_AND",
                    PutAction::Or => "QB_PUT_OR",
                };

                // QB64 extension: _CLIP with optional transparent color
                let trans_code = if let Some(tc) = transparent_color {
                    emit_expr(tc)?
                } else {
                    "-1".to_string() // No transparent color
                };

                let clip_flag = if *clip { "1" } else { "0" };

                if *step {
                    writeln!(
                        output,
                        "{}qb_gfx_put_step((int32_t){}, (int32_t){}, {}, {}, {}, (int32_t){});",
                        indent, x_code, y_code, arr_ptr, action_code, clip_flag, trans_code
                    )
                    .unwrap();
                } else {
                    writeln!(
                        output,
                        "{}qb_gfx_put((int32_t){}, (int32_t){}, {}, {}, {}, (int32_t){});",
                        indent, x_code, y_code, arr_ptr, action_code, clip_flag, trans_code
                    )
                    .unwrap();
                }
            }

            // ==================== QB64 Graphics Extensions ====================
            TypedStatementKind::FreeImage { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_gfx_freeimage((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::PutImage {
                dest_coords,
                source,
                dest,
                source_coords,
                scale_mode,
            } => {
                // Generate _PUTIMAGE call with all optional parameters
                let src_handle = source
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let dst_handle = dest
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());

                // Scale mode: 0 = default, 1 = smooth (bilinear), 2 = stretch (nearest-neighbor)
                let scale_code = match scale_mode {
                    ImageScaleMode::Default => "0",
                    ImageScaleMode::Smooth => "1",
                    ImageScaleMode::Stretch => "2",
                };

                if let (Some(dc), Some(sc)) = (dest_coords, source_coords) {
                    let dx1 = emit_expr(&dc.x1)?;
                    let dy1 = emit_expr(&dc.y1)?;
                    let dx2 = emit_expr(&dc.x2)?;
                    let dy2 = emit_expr(&dc.y2)?;
                    let sx1 = emit_expr(&sc.x1)?;
                    let sy1 = emit_expr(&sc.y1)?;
                    let sx2 = emit_expr(&sc.x2)?;
                    let sy2 = emit_expr(&sc.y2)?;
                    writeln!(output, "{}qb_gfx_putimage_full((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                             indent, dx1, dy1, dx2, dy2, src_handle, dst_handle, sx1, sy1, sx2, sy2, scale_code).unwrap();
                } else if let Some(dc) = dest_coords {
                    let dx1 = emit_expr(&dc.x1)?;
                    let dy1 = emit_expr(&dc.y1)?;
                    let dx2 = emit_expr(&dc.x2)?;
                    let dy2 = emit_expr(&dc.y2)?;
                    writeln!(output, "{}qb_gfx_putimage((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                             indent, dx1, dy1, dx2, dy2, src_handle, dst_handle, scale_code).unwrap();
                } else {
                    writeln!(
                        output,
                        "{}qb_gfx_putimage_simple((int32_t){}, (int32_t){}, {});",
                        indent, src_handle, dst_handle, scale_code
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::SourceImg { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_gfx_source((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::DestImg { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_gfx_dest((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::PrintStringStmt { x, y, text } => {
                let x_code = emit_expr(x)?;
                let y_code = emit_expr(y)?;
                let text_code = emit_expr(text)?;
                writeln!(
                    output,
                    "{}qb_gfx_printstring((int32_t){}, (int32_t){}, {});",
                    indent, x_code, y_code, text_code
                )
                .unwrap();
            }

            TypedStatementKind::AutoDisplay { enabled } => {
                let enable_int = if *enabled { 1 } else { 0 };
                writeln!(output, "{}qb_gfx_autodisplay({});", indent, enable_int).unwrap();
            }

            // ==================== Audio Statements ====================
            TypedStatementKind::Beep => {
                writeln!(output, "{}qb_beep();", indent).unwrap();
            }

            TypedStatementKind::SoundStmt {
                frequency,
                duration,
            } => {
                let freq_code = emit_expr(frequency)?;
                let dur_code = emit_expr(duration)?;
                writeln!(
                    output,
                    "{}qb_sound((double){}, (double){});",
                    indent, freq_code, dur_code
                )
                .unwrap();
            }

            TypedStatementKind::PlayStmt { commands } => {
                let cmd_code = emit_expr(commands)?;
                writeln!(output, "{}qb_play({});", indent, cmd_code).unwrap();
            }

            TypedStatementKind::SndClose { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_sndclose((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::SndPlay { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_sndplay((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::SndStop { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_sndstop((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::SndPause { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_sndpause((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::SndLoop { handle } => {
                let h_code = emit_expr(handle)?;
                writeln!(output, "{}qb_sndloop((int32_t){});", indent, h_code).unwrap();
            }

            TypedStatementKind::SndVol { handle, volume } => {
                let h_code = emit_expr(handle)?;
                let vol_code = emit_expr(volume)?;
                writeln!(
                    output,
                    "{}qb_sndvol((int32_t){}, (double){});",
                    indent, h_code, vol_code
                )
                .unwrap();
            }

            TypedStatementKind::SndBal {
                handle,
                x,
                y,
                z,
                channel,
            } => {
                let h_code = emit_expr(handle)?;
                let x_code = match x {
                    Some(e) => emit_expr(e)?,
                    None => "0.0".to_string(),
                };
                let y_code = match y {
                    Some(e) => emit_expr(e)?,
                    None => "0.0".to_string(),
                };
                let z_code = match z {
                    Some(e) => emit_expr(e)?,
                    None => "0.0".to_string(),
                };
                let ch_code = match channel {
                    Some(e) => emit_expr(e)?,
                    None => "0".to_string(),
                };
                writeln!(
                    output,
                    "{}qb_sndbal((int32_t){}, (double){}, (double){}, (double){}, (int32_t){});",
                    indent, h_code, x_code, y_code, z_code, ch_code
                )
                .unwrap();
            }

            TypedStatementKind::SndRaw { left, right } => {
                let left_code = emit_expr(left)?;
                if let Some(r) = right {
                    let right_code = emit_expr(r)?;
                    writeln!(
                        output,
                        "{}qb_sndraw_stereo((double){}, (double){});",
                        indent, left_code, right_code
                    )
                    .unwrap();
                } else {
                    writeln!(output, "{}qb_sndraw((double){});", indent, left_code).unwrap();
                }
            }

            TypedStatementKind::SndPlayFile {
                filename,
                volume,
                x,
                y,
                z,
            } => {
                let filename_code = emit_expr(filename)?;
                let volume_code = volume
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "1.0".to_string());
                let x_code = x
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "0.0".to_string());
                let y_code = y
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "0.0".to_string());
                let z_code = z
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "0.0".to_string());
                writeln!(
                    output,
                    "{}qb_sndplayfile({}->data, (double){}, (double){}, (double){}, (double){});",
                    indent, filename_code, volume_code, x_code, y_code, z_code
                )
                .unwrap();
            }

            TypedStatementKind::SndPlayCopy { handle, volume } => {
                let handle_code = emit_expr(handle)?;
                let volume_code = volume
                    .as_ref()
                    .map(emit_expr)
                    .transpose()?
                    .unwrap_or_else(|| "1.0".to_string());
                writeln!(
                    output,
                    "{}qb_sndplaycopy((int32_t){}, (double){});",
                    indent, handle_code, volume_code
                )
                .unwrap();
            }

            TypedStatementKind::SndSetPos { handle, position } => {
                let handle_code = emit_expr(handle)?;
                let position_code = emit_expr(position)?;
                writeln!(
                    output,
                    "{}qb_sndsetpos((int32_t){}, (double){});",
                    indent, handle_code, position_code
                )
                .unwrap();
            }

            // ==================== System Integration Statements ====================
            TypedStatementKind::Kill { filename } => {
                let filename_code = emit_expr(filename)?;
                writeln!(output, "{}qb_file_kill({}->data);", indent, filename_code).unwrap();
            }

            TypedStatementKind::Rename { old_name, new_name } => {
                let old_code = emit_expr(old_name)?;
                let new_code = emit_expr(new_name)?;
                writeln!(
                    output,
                    "{}qb_file_rename({}->data, {}->data);",
                    indent, old_code, new_code
                )
                .unwrap();
            }

            TypedStatementKind::Mkdir { path } => {
                let path_code = emit_expr(path)?;
                writeln!(output, "{}qb_mkdir({}->data);", indent, path_code).unwrap();
            }

            TypedStatementKind::Rmdir { path } => {
                let path_code = emit_expr(path)?;
                writeln!(output, "{}qb_rmdir({}->data);", indent, path_code).unwrap();
            }

            TypedStatementKind::Chdir { path } => {
                let path_code = emit_expr(path)?;
                writeln!(output, "{}qb_chdir({}->data);", indent, path_code).unwrap();
            }

            TypedStatementKind::ShellCmd { command } => {
                if let Some(cmd) = command {
                    let cmd_code = emit_expr(cmd)?;
                    writeln!(output, "{}qb_shell({}->data);", indent, cmd_code).unwrap();
                } else {
                    writeln!(output, "{}qb_shell(NULL);", indent).unwrap();
                }
            }

            TypedStatementKind::ShellHide { command } => {
                let cmd_code = emit_expr(command)?;
                writeln!(output, "{}qb_shell_hide({}->data);", indent, cmd_code).unwrap();
            }

            TypedStatementKind::Bload { filename, address } => {
                let filename_code = emit_expr(filename)?;
                if let Some(addr) = address {
                    let addr_code = emit_expr(addr)?;
                    writeln!(
                        output,
                        "{}qb_bload({}->data, (void*)(intptr_t){});",
                        indent, filename_code, addr_code
                    )
                    .unwrap();
                } else {
                    writeln!(output, "{}qb_bload({}->data, NULL);", indent, filename_code).unwrap();
                }
            }

            TypedStatementKind::Bsave {
                filename,
                address,
                length,
            } => {
                let filename_code = emit_expr(filename)?;
                let addr_code = emit_expr(address)?;
                let len_code = emit_expr(length)?;
                writeln!(
                    output,
                    "{}qb_bsave({}->data, (void*)(intptr_t){}, (size_t){});",
                    indent, filename_code, addr_code, len_code
                )
                .unwrap();
            }

            TypedStatementKind::Setmem { bytes } => {
                // SETMEM is a no-op in modern systems - just evaluate the expression
                let bytes_code = emit_expr(bytes)?;
                writeln!(
                    output,
                    "{}(void){}; /* SETMEM: no-op in flat memory model */",
                    indent, bytes_code
                )
                .unwrap();
            }

            TypedStatementKind::CallAbsolute { args: _, address } => {
                // CALL ABSOLUTE is a legacy statement that cannot be safely implemented
                let addr_code = emit_expr(address)?;
                writeln!(
                    output,
                    "{}fprintf(stderr, \"Warning: CALL ABSOLUTE at address %ld not supported in flat memory model\\n\", (long){});",
                    indent, addr_code
                )
                .unwrap();
            }

            // ==================== Mouse Input Statements ====================
            TypedStatementKind::MouseHide => {
                writeln!(output, "{}qb_mouse_hide();", indent).unwrap();
            }

            TypedStatementKind::MouseShow => {
                writeln!(output, "{}qb_mouse_show();", indent).unwrap();
            }

            TypedStatementKind::MouseMoveStmt { x, y } => {
                let x_code = emit_expr(x)?;
                let y_code = emit_expr(y)?;
                writeln!(
                    output,
                    "{}qb_mouse_move((int32_t){}, (int32_t){});",
                    indent, x_code, y_code
                )
                .unwrap();
            }

            // ==================== Clipboard Statement ====================
            TypedStatementKind::ClipboardSet { text } => {
                let text_code = emit_expr(text)?;
                writeln!(output, "{}qb_clipboard_set({}->data);", indent, text_code).unwrap();
            }

            // ==================== C Library Integration ====================
            TypedStatementKind::DeclareLibrary {
                library_name,
                is_dynamic,
                declarations,
            } => {
                // For static libraries, we just emit extern declarations
                // For dynamic libraries, we would need to emit dlopen/LoadLibrary code
                // at runtime, which is more complex and deferred for now.
                //
                // The actual function calls are handled in expression codegen
                // when the external function is called.
                if *is_dynamic {
                    writeln!(
                        output,
                        "{}// DECLARE DYNAMIC LIBRARY (runtime loading not yet implemented)",
                        indent
                    )
                    .unwrap();
                    if let Some(lib) = library_name {
                        writeln!(output, "{}// Library: {}", indent, lib).unwrap();
                    }
                } else {
                    writeln!(output, "{}// DECLARE LIBRARY - extern declarations", indent).unwrap();
                    if let Some(lib) = library_name {
                        writeln!(output, "{}// Library: {}", indent, lib).unwrap();
                    }
                }

                // Emit extern declarations for each function
                for decl in declarations {
                    self.emit_extern_declaration(&indent, decl, output);
                }
            }

            // Forward declarations - no code generated, just comments for documentation
            TypedStatementKind::DeclareSub { name } => {
                writeln!(output, "{}/* DECLARE SUB {} */", indent, name).unwrap();
            }

            TypedStatementKind::DeclareFunction { name } => {
                writeln!(output, "{}/* DECLARE FUNCTION {} */", indent, name).unwrap();
            }

            // ==================== Phase 7: Additional Statements ====================
            TypedStatementKind::Run { target } => {
                // RUN restarts the program or runs another - stub implementation
                if let Some(t) = target {
                    let target_code = emit_expr(t)?;
                    writeln!(output, "{}qb_run({});", indent, target_code).unwrap();
                } else {
                    writeln!(output, "{}qb_run(NULL);", indent).unwrap();
                }
            }

            TypedStatementKind::Chain { filename } => {
                let filename_code = emit_expr(filename)?;
                writeln!(output, "{}qb_chain({});", indent, filename_code).unwrap();
            }

            TypedStatementKind::Tron => {
                writeln!(output, "{}qb_trace_on = 1;", indent).unwrap();
            }

            TypedStatementKind::Troff => {
                writeln!(output, "{}qb_trace_on = 0;", indent).unwrap();
            }

            TypedStatementKind::Lprint { values, newline } => {
                // Print to printer (LPT1) - similar to PRINT but to a different stream
                for item in values {
                    let expr_code = emit_expr(&item.expr)?;
                    writeln!(output, "{}qb_lprint({});", indent, expr_code).unwrap();
                    if item.separator == Some(PrintSeparator::Comma) {
                        writeln!(output, "{}qb_lprint_tab();", indent).unwrap();
                    }
                }
                if *newline {
                    writeln!(output, "{}qb_lprint_newline();", indent).unwrap();
                }
            }

            TypedStatementKind::FilesStmt { filespec } => {
                if let Some(spec) = filespec {
                    let spec_code = emit_expr(spec)?;
                    writeln!(output, "{}qb_files({});", indent, spec_code).unwrap();
                } else {
                    writeln!(output, "{}qb_files(NULL);", indent).unwrap();
                }
            }

            TypedStatementKind::FieldStmt { file_num, fields } => {
                let file_num_code = emit_expr(file_num)?;
                writeln!(
                    output,
                    "{}qb_field_start((int32_t)({}));",
                    indent, file_num_code
                )
                .unwrap();
                for field in fields {
                    let width_code = emit_expr(&field.width)?;
                    writeln!(
                        output,
                        "{}qb_field_add((int32_t)({}), &{});",
                        indent, width_code, field.variable
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::Lset { variable, value } => {
                let value_code = emit_expr(value)?;
                writeln!(output, "{}qb_lset(&{}, {});", indent, variable, value_code).unwrap();
            }

            TypedStatementKind::Rset { variable, value } => {
                let value_code = emit_expr(value)?;
                writeln!(output, "{}qb_rset(&{}, {});", indent, variable, value_code).unwrap();
            }

            TypedStatementKind::OnKey { key_num, target } => {
                let key_code = emit_expr(key_num)?;
                writeln!(
                    output,
                    "{}qb_on_key((int32_t)({}), &&{});",
                    indent, key_code, target
                )
                .unwrap();
            }

            TypedStatementKind::KeyControl { key_num, mode } => {
                let key_code = emit_expr(key_num)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln!(
                    output,
                    "{}qb_key_control((int32_t)({}), {});",
                    indent, key_code, mode_code
                )
                .unwrap();
            }

            TypedStatementKind::OnTimer { interval, target } => {
                let interval_code = emit_expr(interval)?;
                writeln!(
                    output,
                    "{}qb_on_timer({}, &&{});",
                    indent, interval_code, target
                )
                .unwrap();
            }

            TypedStatementKind::TimerControl { mode } => {
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln!(output, "{}qb_timer_control({});", indent, mode_code).unwrap();
            }

            TypedStatementKind::StrigControl { button_num, mode } => {
                let btn_code = emit_expr(button_num)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln!(
                    output,
                    "{}qb_strig_control((int32_t)({}), {});",
                    indent, btn_code, mode_code
                )
                .unwrap();
            }

            TypedStatementKind::OnStrig { button_num, target } => {
                let btn_code = emit_expr(button_num)?;
                writeln!(
                    output,
                    "{}qb_on_strig((int32_t)({}), &&{});",
                    indent, btn_code, target
                )
                .unwrap();
            }

            TypedStatementKind::OnCom { port_num, target } => {
                let port_code = emit_expr(port_num)?;
                writeln!(
                    output,
                    "{}qb_on_com((int32_t)({}), &&{});",
                    indent, port_code, target
                )
                .unwrap();
            }

            TypedStatementKind::ComControl { port_num, mode } => {
                let port_code = emit_expr(port_num)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln!(
                    output,
                    "{}qb_com_control((int32_t)({}), {});",
                    indent, port_code, mode_code
                )
                .unwrap();
            }

            TypedStatementKind::OnPen { target } => {
                writeln!(output, "{}qb_on_pen(&&{});", indent, target).unwrap();
            }

            TypedStatementKind::PenControl { mode } => {
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln!(output, "{}qb_pen_control({});", indent, mode_code).unwrap();
            }

            TypedStatementKind::OnUevent { target } => {
                writeln!(output, "{}qb_on_uevent(&&{});", indent, target).unwrap();
            }

            TypedStatementKind::UeventControl { mode } => {
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln!(output, "{}qb_uevent_control({});", indent, mode_code).unwrap();
            }

            TypedStatementKind::UeventTrigger => {
                writeln!(output, "{}qb_uevent_trigger();", indent).unwrap();
            }

            TypedStatementKind::OnSignal { signal_num, target } => {
                let signal_code = emit_expr(signal_num)?;
                writeln!(
                    output,
                    "{}qb_on_signal((int32_t)({}), &&{});",
                    indent, signal_code, target
                )
                .unwrap();
            }

            TypedStatementKind::SignalControl { signal_num, mode } => {
                let signal_code = emit_expr(signal_num)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln!(
                    output,
                    "{}qb_signal_control((int32_t)({}), {});",
                    indent, signal_code, mode_code
                )
                .unwrap();
            }

            TypedStatementKind::OutPort { port, value } => {
                let port_code = emit_expr(port)?;
                let value_code = emit_expr(value)?;
                writeln!(
                    output,
                    "{}qb_out((int32_t)({}), (int32_t)({}));",
                    indent, port_code, value_code
                )
                .unwrap();
            }

            TypedStatementKind::InterruptStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let int_code = emit_expr(int_num)?;
                writeln!(
                    output,
                    "{}qb_interrupt((int32_t)({}), &{}, &{});",
                    indent, int_code, in_regs, out_regs
                )
                .unwrap();
            }

            TypedStatementKind::InterruptXStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let int_code = emit_expr(int_num)?;
                writeln!(
                    output,
                    "{}qb_interruptx((int32_t)({}), &{}, &{});",
                    indent, int_code, in_regs, out_regs
                )
                .unwrap();
            }

            TypedStatementKind::IoctlStmt {
                file_num,
                control_string,
            } => {
                let file_code = emit_expr(file_num)?;
                let string_code = emit_expr(control_string)?;
                writeln!(
                    output,
                    "{}qb_ioctl((int32_t)({}), {});",
                    indent, file_code, string_code
                )
                .unwrap();
            }

            TypedStatementKind::FreeStmt => {
                writeln!(output, "{}qb_free();", indent).unwrap();
            }

            TypedStatementKind::ClearStmt { stack_size } => {
                if let Some(size) = stack_size {
                    let size_code = emit_expr(size)?;
                    writeln!(output, "{}qb_clear((int32_t)({}));", indent, size_code).unwrap();
                } else {
                    writeln!(output, "{}qb_clear(0);", indent).unwrap();
                }
            }

            TypedStatementKind::ResetStmt => {
                writeln!(output, "{}qb_reset();", indent).unwrap();
            }

            // Window/Desktop statements (QB64)
            TypedStatementKind::TitleStmt { title } => {
                let title_code = emit_expr(title)?;
                writeln!(output, "{}qb_title({});", indent, title_code).unwrap();
            }

            TypedStatementKind::ScreenMoveStmt { x, y, center } => {
                if *center {
                    writeln!(output, "{}qb_screenmove_center();", indent).unwrap();
                } else {
                    let x_code = match x {
                        Some(e) => emit_expr(e)?,
                        None => "0".to_string(),
                    };
                    let y_code = match y {
                        Some(e) => emit_expr(e)?,
                        None => "0".to_string(),
                    };
                    writeln!(
                        output,
                        "{}qb_screenmove((int32_t)({}), (int32_t)({}));",
                        indent, x_code, y_code
                    )
                    .unwrap();
                }
            }

            TypedStatementKind::FullScreenStmt { mode } => {
                let mode_code = match mode {
                    FullScreenMode::Stretch => "0",
                    FullScreenMode::SquarePixels => "1",
                    FullScreenMode::Off => "2",
                };
                writeln!(output, "{}qb_fullscreen({});", indent, mode_code).unwrap();
            }

            TypedStatementKind::AllowFullScreenStmt { mode } => {
                let mode_code = match mode {
                    AllowFullScreenMode::Stretch => "0",
                    AllowFullScreenMode::SquarePixels => "1",
                    AllowFullScreenMode::All => "2",
                    AllowFullScreenMode::Off => "3",
                };
                writeln!(output, "{}qb_allowfullscreen({});", indent, mode_code).unwrap();
            }

            TypedStatementKind::ScreenIconStmt => {
                writeln!(output, "{}qb_screenicon();", indent).unwrap();
            }

            TypedStatementKind::IconStmt { handle } => {
                if let Some(h) = handle {
                    let handle_code = emit_expr(h)?;
                    writeln!(output, "{}qb_icon((int32_t)({}));", indent, handle_code).unwrap();
                } else {
                    writeln!(output, "{}qb_icon(0);", indent).unwrap();
                }
            }

            TypedStatementKind::ScreenHideStmt => {
                writeln!(output, "{}qb_screenhide();", indent).unwrap();
            }

            TypedStatementKind::ScreenShowStmt => {
                writeln!(output, "{}qb_screenshow();", indent).unwrap();
            }

            TypedStatementKind::ConsoleTitleStmt { title } => {
                let title_code = emit_expr(title)?;
                writeln!(output, "{}qb_consoletitle({});", indent, title_code).unwrap();
            }

            TypedStatementKind::ConsoleStmt { visible } => {
                writeln!(
                    output,
                    "{}qb_console({});",
                    indent,
                    if *visible { "1" } else { "0" }
                )
                .unwrap();
            }

            TypedStatementKind::AssertStmt { condition, message } => {
                let cond_code = emit_expr(condition)?;
                if let Some(msg) = message {
                    let msg_code = emit_expr(msg)?;
                    writeln!(output, "{}qb_assert({}, {});", indent, cond_code, msg_code).unwrap();
                } else {
                    writeln!(output, "{}qb_assert({}, NULL);", indent, cond_code).unwrap();
                }
            }

            TypedStatementKind::MetaAsserts => {
                writeln!(output, "{}/* $ASSERTS */", indent).unwrap();
            }

            TypedStatementKind::MetaNoPrefix => {
                writeln!(output, "{}/* $NOPREFIX */", indent).unwrap();
            }

            TypedStatementKind::MetaColor { depth } => {
                if let Some(d) = depth {
                    writeln!(output, "{}/* $COLOR:{} */", indent, d).unwrap();
                } else {
                    writeln!(output, "{}/* $COLOR:0 */", indent).unwrap();
                }
            }
        }

        Ok(())
    }

    /// Emits an extern declaration for a C library function.
    ///
    /// For external C functions, we need to emit C types, not BASIC types:
    /// - BYVAL STRING → const char* (not qb_string*)
    /// - STRING (by reference) → qb_string** (pointer to pointer for output)
    /// - Return STRING → char* (caller must handle with qb_string_new)
    fn emit_extern_declaration(
        &self,
        indent: &str,
        decl: &crate::semantic::typed_ir::TypedExternalDeclaration,
        output: &mut String,
    ) {
        use super::types::c_type;
        use crate::semantic::types::BasicType;

        // For external functions returning STRING, use char*
        let return_type = if decl.return_type == BasicType::String {
            "char*".to_string()
        } else {
            c_type(&decl.return_type)
        };

        let params: Vec<String> = decl
            .params
            .iter()
            .map(|p| {
                // For external functions, STRING params need special handling:
                // - BYVAL STRING → const char* (C string)
                // - BYREF STRING → qb_string** (pointer to BASIC string pointer)
                let param_type = if p.typ == BasicType::String {
                    if p.is_byval {
                        "const char*".to_string()
                    } else {
                        "qb_string**".to_string()
                    }
                } else {
                    c_type(&p.typ)
                };
                format!("{} {}", param_type, p.name)
            })
            .collect();
        let params_str = if params.is_empty() {
            "void".to_string()
        } else {
            params.join(", ")
        };

        writeln!(
            output,
            "{}extern {} {}({});",
            indent, return_type, decl.c_name, params_str
        )
        .unwrap();
    }

    // Helper methods for complex statements

    fn emit_assignment(
        &self,
        indent: &str,
        name: &str,
        value: &crate::semantic::typed_ir::TypedExpr,
        target_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let value_code = emit_expr(value)?;

        if value.basic_type != *target_type {
            let c_ty = c_type(target_type);
            writeln!(output, "{}{} = ({})({});", indent, c_name, c_ty, value_code).unwrap();
        } else {
            writeln!(output, "{}{} = {};", indent, c_name, value_code).unwrap();
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_array_assignment(
        &self,
        indent: &str,
        name: &str,
        indices: &[crate::semantic::typed_ir::TypedExpr],
        value: &crate::semantic::typed_ir::TypedExpr,
        dimensions: &[TypedArrayDimension],
        element_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let value_code = emit_expr(value)?;

        let indices_code: Result<Vec<_>, _> = indices.iter().map(emit_expr).collect();
        let indices_code = indices_code?;

        let index_expr = if dimensions.is_empty() || indices_code.len() == 1 {
            if let Some(dim) = dimensions.first() {
                format!("{} - {}", indices_code[0], dim.lower)
            } else {
                indices_code[0].clone()
            }
        } else {
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

        if value.basic_type != *element_type {
            let c_ty = c_type(element_type);
            writeln!(
                output,
                "{}{}[{}] = ({})({});",
                indent, c_name, index_expr, c_ty, value_code
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
        Ok(())
    }

    /// Emits a UDT array field assignment: `array(i).field = value`
    #[allow(clippy::too_many_arguments)]
    fn emit_array_field_assignment(
        &self,
        indent: &str,
        name: &str,
        indices: &[crate::semantic::typed_ir::TypedExpr],
        fields: &[String],
        value: &crate::semantic::typed_ir::TypedExpr,
        dimensions: &[TypedArrayDimension],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let value_code = emit_expr(value)?;

        let indices_code: Result<Vec<_>, _> = indices.iter().map(emit_expr).collect();
        let indices_code = indices_code?;

        // Calculate linear index for multi-dimensional arrays
        let index_expr = if dimensions.is_empty() || indices_code.len() == 1 {
            if let Some(dim) = dimensions.first() {
                format!("{} - {}", indices_code[0], dim.lower)
            } else {
                indices_code[0].clone()
            }
        } else {
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

        // Build field access chain: .field1.field2...
        let field_chain: String = fields
            .iter()
            .map(|f| format!(".{}", c_identifier(f)))
            .collect();

        writeln!(
            output,
            "{}{}[{}]{} = {};",
            indent, c_name, index_expr, field_chain, value_code
        )
        .unwrap();

        Ok(())
    }

    fn emit_input(
        &self,
        indent: &str,
        prompt: &Option<String>,
        show_question_mark: bool,
        same_line: bool,
        targets: &[TypedInputTarget],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use TypedInputTarget::*;

        // Note: same_line is currently stored but not used in codegen
        // The runtime would need to be updated to support this behavior
        // (keep cursor on same line after input instead of moving to new line)
        let _ = same_line;

        let full_prompt = match prompt {
            Some(p) => {
                if show_question_mark {
                    format!("{}? ", p)
                } else {
                    p.clone()
                }
            }
            None => {
                if show_question_mark {
                    "? ".to_string()
                } else {
                    String::new()
                }
            }
        };

        for (i, target) in targets.iter().enumerate() {
            let (target_code, var_type) = match target {
                Variable { name, basic_type } => (c_identifier(name), basic_type.clone()),
                ArrayElement {
                    name,
                    indices,
                    element_type,
                } => {
                    let c_arr = c_identifier(name);
                    let idx_code: Vec<_> =
                        indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
                    // Use first index for 1D array syntax (TODO: handle multi-dim)
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
                    let idx_code: Vec<_> =
                        indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
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

            let prompt_arg = if i == 0 && !full_prompt.is_empty() {
                format!("\"{}\"", escape_string(&full_prompt))
            } else {
                "NULL".to_string()
            };

            if var_type.is_string() {
                writeln!(
                    output,
                    "{}qb_input_string({}, &{});",
                    indent, prompt_arg, target_code
                )
                .unwrap();
            } else if var_type.is_float() {
                writeln!(
                    output,
                    "{}qb_input_float({}, &{});",
                    indent, prompt_arg, target_code
                )
                .unwrap();
            } else {
                writeln!(
                    output,
                    "{}qb_input_int({}, &{});",
                    indent, prompt_arg, target_code
                )
                .unwrap();
            }
        }
        Ok(())
    }

    fn emit_if(
        &mut self,
        indent: &str,
        condition: &crate::semantic::typed_ir::TypedExpr,
        then_branch: &[TypedStatement],
        elseif_branches: &[(crate::semantic::typed_ir::TypedExpr, Vec<TypedStatement>)],
        else_branch: &Option<Vec<TypedStatement>>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let cond_code = emit_expr(condition)?;
        writeln!(output, "{}if ({}) {{", indent, cond_code).unwrap();

        self.indent += 1;
        for stmt in then_branch {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        for (elseif_cond, elseif_body) in elseif_branches {
            let elseif_code = emit_expr(elseif_cond)?;
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
        Ok(())
    }

    fn emit_select_case(
        &mut self,
        indent: &str,
        test_expr: &crate::semantic::typed_ir::TypedExpr,
        cases: &[crate::semantic::typed_ir::TypedCaseClause],
        case_else: &Option<Vec<TypedStatement>>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let test_var = self.next_label("select");
        let test_code = emit_expr(test_expr)?;
        let c_ty = c_type(&test_expr.basic_type);

        writeln!(output, "{}{} {} = {};", indent, c_ty, test_var, test_code).unwrap();

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
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_for(
        &mut self,
        indent: &str,
        variable: &str,
        var_type: &BasicType,
        start: &crate::semantic::typed_ir::TypedExpr,
        end: &crate::semantic::typed_ir::TypedExpr,
        step: &Option<crate::semantic::typed_ir::TypedExpr>,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_var = c_identifier(variable);
        let c_ty = c_type(var_type);
        let start_code = emit_expr(start)?;
        let end_code = emit_expr(end)?;
        let step_code = match step {
            Some(s) => emit_expr(s)?,
            None => "1".to_string(),
        };

        let break_label = self.next_label("for_end");
        self.loop_stack.push(LoopContext {
            break_label: break_label.clone(),
            loop_type: ExitType::For,
        });

        let end_var = self.next_label("for_end_val");
        let step_var = self.next_label("for_step");
        writeln!(output, "{}{} {} = {};", indent, c_ty, end_var, end_code).unwrap();
        writeln!(output, "{}{} {} = {};", indent, c_ty, step_var, step_code).unwrap();

        writeln!(
            output,
            "{}for ({} {} = {}; ({} > 0) ? ({} <= {}) : ({} >= {}); {} += {}) {{",
            indent,
            c_ty,
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
        Ok(())
    }

    fn emit_while(
        &mut self,
        indent: &str,
        condition: &crate::semantic::typed_ir::TypedExpr,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let cond_code = emit_expr(condition)?;
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
        Ok(())
    }

    fn emit_do_loop(
        &mut self,
        indent: &str,
        pre_condition: &Option<TypedDoCondition>,
        body: &[TypedStatement],
        post_condition: &Option<TypedDoCondition>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let break_label = self.next_label("do_end");

        self.loop_stack.push(LoopContext {
            break_label: break_label.clone(),
            loop_type: ExitType::Do,
        });

        match (pre_condition, post_condition) {
            (Some(pre), None) => {
                let cond = self.emit_do_condition(pre)?;
                writeln!(output, "{}while ({}) {{", indent, cond).unwrap();
            }
            (None, Some(_post)) => {
                writeln!(output, "{}do {{", indent).unwrap();
            }
            (None, None) => {
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
        Ok(())
    }

    fn emit_exit(
        &self,
        indent: &str,
        exit_type: &ExitType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
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
        Ok(())
    }

    fn emit_sub_definition(
        &mut self,
        indent: &str,
        name: &str,
        params: &[TypedParameter],
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = format!("qb_sub_{}", c_identifier(name).to_lowercase());
        let params_str = emit_params(params);

        writeln!(output, "{}void {}({}) {{", indent, c_name, params_str).unwrap();

        self.indent += 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent -= 1;

        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output).unwrap();
        Ok(())
    }

    fn emit_function_definition(
        &mut self,
        indent: &str,
        name: &str,
        params: &[TypedParameter],
        return_type: &BasicType,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_function_name(name);
        let c_ret_type = c_type(return_type);
        let params_str = emit_params(params);

        writeln!(
            output,
            "{}{} {}({}) {{",
            indent, c_ret_type, c_name, params_str
        )
        .unwrap();

        let ret_var = c_identifier(name);
        writeln!(
            output,
            "    {} {} = {};",
            c_ret_type,
            ret_var,
            default_init(return_type)
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
        Ok(())
    }

    fn emit_dim(
        &self,
        indent: &str,
        name: &str,
        basic_type: &BasicType,
        dimensions: &[TypedArrayDimension],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let c_ty = c_type(basic_type);

        if dimensions.is_empty() {
            let init = default_init(basic_type);
            writeln!(output, "{}{} {} = {};", indent, c_ty, c_name, init).unwrap();
        } else {
            let sizes: Vec<String> = dimensions
                .iter()
                .map(|d| format!("({})", d.upper - d.lower + 1))
                .collect();
            let size_expr = sizes.join(" * ");
            writeln!(
                output,
                "{}{}* {} = malloc(sizeof({}) * {});",
                indent, c_ty, c_name, c_ty, size_expr
            )
            .unwrap();
        }
        Ok(())
    }

    fn emit_type_definition(
        &self,
        indent: &str,
        name: &str,
        members: &[TypedMember],
        custom_type: bool,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);

        // CUSTOMTYPE modifier indicates C-compatible (packed) memory layout
        // This uses #pragma pack to ensure no padding between members
        if custom_type {
            writeln!(output, "{}#pragma pack(push, 1)", indent).unwrap();
        }

        writeln!(output, "{}typedef struct {} {{", indent, c_name).unwrap();

        for member in members {
            let c_member_type = c_type(&member.basic_type);
            let c_member_name = c_identifier(&member.name);

            if let BasicType::FixedString(len) = &member.basic_type {
                writeln!(output, "{}    char {}[{}];", indent, c_member_name, len + 1).unwrap();
            } else {
                writeln!(output, "{}    {} {};", indent, c_member_type, c_member_name).unwrap();
            }
        }

        writeln!(output, "{}}} {};", indent, c_name).unwrap();

        if custom_type {
            writeln!(output, "{}#pragma pack(pop)", indent).unwrap();
        }

        writeln!(output).unwrap();
        Ok(())
    }

    fn emit_read(
        &self,
        indent: &str,
        targets: &[TypedReadTarget],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        for target in targets {
            let (c_target, var_type) = match target {
                TypedReadTarget::Variable { name, basic_type } => {
                    (c_identifier(name), basic_type.clone())
                }
                TypedReadTarget::ArrayElement {
                    name,
                    indices,
                    basic_type,
                } => {
                    let c_arr = c_identifier(name);
                    let idx_parts: Vec<_> = indices
                        .iter()
                        .map(emit_expr)
                        .collect::<Result<Vec<_>, _>>()?;
                    let idx_str = idx_parts.join("][");
                    (format!("{}[{}]", c_arr, idx_str), basic_type.clone())
                }
                TypedReadTarget::ArrayFieldElement {
                    name,
                    indices,
                    field,
                    basic_type,
                } => {
                    let c_arr = c_identifier(name);
                    let c_field = c_identifier(field);
                    let idx_parts: Vec<_> = indices
                        .iter()
                        .map(emit_expr)
                        .collect::<Result<Vec<_>, _>>()?;
                    let idx_str = idx_parts.join("][");
                    (
                        format!("{}[{}].{}", c_arr, idx_str, c_field),
                        basic_type.clone(),
                    )
                }
            };

            match var_type {
                BasicType::String | BasicType::FixedString(_) => {
                    writeln!(
                        output,
                        "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 's') {{",
                        indent
                    )
                    .unwrap();
                    writeln!(
                        output,
                        "{}    {} = qb_str_from_c(_qb_data[_qb_data_ptr].v.s);",
                        indent, c_target
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
                        indent, c_target
                    )
                    .unwrap();
                    writeln!(output, "{}}}", indent).unwrap();
                    writeln!(output, "{}_qb_data_ptr++;", indent).unwrap();
                }
                _ => {
                    let c_ty = c_type(&var_type);
                    writeln!(
                        output,
                        "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 'd') {{",
                        indent
                    )
                    .unwrap();
                    writeln!(
                        output,
                        "{}    {} = ({})_qb_data[_qb_data_ptr].v.n;",
                        indent, c_target, c_ty
                    )
                    .unwrap();
                    writeln!(output, "{}}}", indent).unwrap();
                    writeln!(output, "{}_qb_data_ptr++;", indent).unwrap();
                }
            }
        }
        Ok(())
    }

    fn emit_restore(
        &self,
        indent: &str,
        label: &Option<String>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        match label {
            None => {
                writeln!(output, "{}_qb_data_ptr = 0;", indent).unwrap();
            }
            Some(lbl) => {
                let label_upper = lbl.to_uppercase();
                if let Some(&index) = self.data_label_indices.get(&label_upper) {
                    writeln!(
                        output,
                        "{}_qb_data_ptr = {}; /* RESTORE {} */",
                        indent, index, lbl
                    )
                    .unwrap();
                } else {
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
        Ok(())
    }

    /// Emits a PRINT item.
    fn emit_print_item(
        &self,
        item: &TypedPrintItem,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();
        let expr_code = emit_expr(&item.expr)?;

        if item.expr.basic_type.is_string() {
            writeln!(output, "{}qb_print_string({});", indent, expr_code).unwrap();
        } else if item.expr.basic_type.is_float() {
            writeln!(output, "{}qb_print_float({});", indent, expr_code).unwrap();
        } else {
            writeln!(output, "{}qb_print_int({});", indent, expr_code).unwrap();
        }

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
        let cond_code = emit_expr(&cond.condition)?;
        if cond.is_while {
            Ok(cond_code)
        } else {
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
                let val = emit_expr(expr)?;
                Ok(format!("({} == {})", test_var, val))
            }
            TypedCaseMatch::Range { from, to } => {
                let from_code = emit_expr(from)?;
                let to_code = emit_expr(to)?;
                Ok(format!(
                    "({} >= {} && {} <= {})",
                    test_var, from_code, test_var, to_code
                ))
            }
            TypedCaseMatch::Comparison { op, value } => {
                let val = emit_expr(value)?;
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

    // ==================== File I/O Helper Methods ====================

    /// Emits an OPEN statement.
    #[allow(clippy::too_many_arguments)]
    fn emit_open_file(
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
        let filename_code = emit_expr(filename)?;
        let file_num_code = emit_expr(file_num)?;

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

        writeln!(
            output,
            "{}qb_file_open({}, {}->data, {});",
            indent, file_num_code, filename_code, c_mode
        )
        .unwrap();

        // Handle record length for random access
        if let Some(rec_len) = record_len {
            let rec_len_code = emit_expr(rec_len)?;
            writeln!(
                output,
                "{}qb_file_set_reclen({}, {});",
                indent, file_num_code, rec_len_code
            )
            .unwrap();
        }

        Ok(())
    }

    /// Emits a CLOSE statement.
    fn emit_close_file(
        &self,
        indent: &str,
        file_nums: &[TypedExpr],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        if file_nums.is_empty() {
            // Close all files
            writeln!(output, "{}qb_file_close_all();", indent).unwrap();
        } else {
            for file_num in file_nums {
                let file_num_code = emit_expr(file_num)?;
                writeln!(output, "{}qb_file_close({});", indent, file_num_code).unwrap();
            }
        }
        Ok(())
    }

    /// Emits a PRINT # statement.
    fn emit_file_print(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        items: &[TypedPrintItem],
        newline: bool,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num)?;

        for item in items {
            let expr_code = emit_expr(&item.expr)?;

            if item.expr.basic_type.is_string() {
                writeln!(
                    output,
                    "{}qb_file_print_string({}, {});",
                    indent, file_num_code, expr_code
                )
                .unwrap();
            } else if item.expr.basic_type.is_float() {
                writeln!(
                    output,
                    "{}qb_file_print_float({}, {});",
                    indent, file_num_code, expr_code
                )
                .unwrap();
            } else {
                writeln!(
                    output,
                    "{}qb_file_print_int({}, {});",
                    indent, file_num_code, expr_code
                )
                .unwrap();
            }

            if let Some(sep) = &item.separator {
                match sep {
                    PrintSeparator::Comma => {
                        writeln!(output, "{}qb_file_print_tab({});", indent, file_num_code)
                            .unwrap();
                    }
                    PrintSeparator::Semicolon => {}
                }
            }
        }

        if newline {
            writeln!(
                output,
                "{}qb_file_print_newline({});",
                indent, file_num_code
            )
            .unwrap();
        }

        Ok(())
    }

    /// Emits a WRITE # statement.
    fn emit_file_write(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        values: &[TypedExpr],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num)?;

        for (i, value) in values.iter().enumerate() {
            let expr_code = emit_expr(value)?;

            if value.basic_type.is_string() {
                // WRITE quotes strings
                writeln!(
                    output,
                    "{}qb_file_write_string({}, {});",
                    indent, file_num_code, expr_code
                )
                .unwrap();
            } else {
                writeln!(
                    output,
                    "{}qb_file_write_number({}, {});",
                    indent, file_num_code, expr_code
                )
                .unwrap();
            }

            // Add comma separator except for last item
            if i < values.len() - 1 {
                writeln!(
                    output,
                    "{}qb_file_write_char({}, ',');",
                    indent, file_num_code
                )
                .unwrap();
            }
        }

        // WRITE always ends with newline
        writeln!(
            output,
            "{}qb_file_print_newline({});",
            indent, file_num_code
        )
        .unwrap();

        Ok(())
    }

    /// Emits an INPUT # statement.
    fn emit_file_input(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        targets: &[TypedInputTarget],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use TypedInputTarget::*;
        let file_num_code = emit_expr(file_num)?;

        for target in targets {
            let (target_code, var_type) = match target {
                Variable { name, basic_type } => (c_identifier(name), basic_type.clone()),
                ArrayElement {
                    name,
                    indices,
                    element_type,
                } => {
                    let c_arr = c_identifier(name);
                    let idx_code: Vec<_> =
                        indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
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
                    let idx_code: Vec<_> =
                        indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
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
                    writeln!(
                        output,
                        "{}qb_file_input_string({}, &{});",
                        indent, file_num_code, target_code
                    )
                    .unwrap();
                }
                _ if var_type.is_float() => {
                    writeln!(
                        output,
                        "{}qb_file_input_float({}, &{});",
                        indent, file_num_code, target_code
                    )
                    .unwrap();
                }
                _ => {
                    writeln!(
                        output,
                        "{}qb_file_input_int({}, &{});",
                        indent, file_num_code, target_code
                    )
                    .unwrap();
                }
            }
        }

        Ok(())
    }

    /// Emits a LINE INPUT # statement.
    fn emit_file_line_input(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        target: &TypedInputTarget,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use TypedInputTarget::*;

        let file_num_code = emit_expr(file_num)?;

        let target_code = match target {
            Variable { name, .. } => c_identifier(name),
            ArrayElement { name, indices, .. } => {
                let c_arr = c_identifier(name);
                let idx_code: Vec<_> = indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
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
                let idx_code: Vec<_> = indices.iter().map(emit_expr).collect::<Result<_, _>>()?;
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

        writeln!(
            output,
            "{}qb_file_line_input({}, &{});",
            indent, file_num_code, target_code
        )
        .unwrap();

        Ok(())
    }

    /// Emits a GET statement.
    #[allow(clippy::too_many_arguments)]
    fn emit_file_get(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        position: Option<&TypedExpr>,
        variable: &str,
        var_type: &BasicType,
        index: Option<&TypedExpr>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num)?;
        let c_var = c_identifier(variable);

        // Seek to position if specified
        if let Some(pos) = position {
            let pos_code = emit_expr(pos)?;
            writeln!(
                output,
                "{}qb_file_seek_record({}, {});",
                indent, file_num_code, pos_code
            )
            .unwrap();
        }

        // Read the data - handle array indexing if present
        let size = type_size(var_type);
        if let Some(idx) = index {
            let idx_code = emit_expr(idx)?;
            writeln!(
                output,
                "{}qb_file_get({}, &{}[{}], {});",
                indent, file_num_code, c_var, idx_code, size
            )
            .unwrap();
        } else {
            writeln!(
                output,
                "{}qb_file_get({}, &{}, {});",
                indent, file_num_code, c_var, size
            )
            .unwrap();
        }

        Ok(())
    }

    /// Emits a PUT statement.
    #[allow(clippy::too_many_arguments)]
    fn emit_file_put(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        position: Option<&TypedExpr>,
        variable: &str,
        var_type: &BasicType,
        index: Option<&TypedExpr>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num)?;
        let c_var = c_identifier(variable);

        // Seek to position if specified
        if let Some(pos) = position {
            let pos_code = emit_expr(pos)?;
            writeln!(
                output,
                "{}qb_file_seek_record({}, {});",
                indent, file_num_code, pos_code
            )
            .unwrap();
        }

        // Write the data - handle array indexing if present
        let size = type_size(var_type);
        if let Some(idx) = index {
            let idx_code = emit_expr(idx)?;
            writeln!(
                output,
                "{}qb_file_put({}, &{}[{}], {});",
                indent, file_num_code, c_var, idx_code, size
            )
            .unwrap();
        } else {
            writeln!(
                output,
                "{}qb_file_put({}, &{}, {});",
                indent, file_num_code, c_var, size
            )
            .unwrap();
        }

        Ok(())
    }

    /// Emits a SEEK statement.
    fn emit_file_seek(
        &self,
        indent: &str,
        file_num: &TypedExpr,
        position: &TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let file_num_code = emit_expr(file_num)?;
        let pos_code = emit_expr(position)?;

        writeln!(
            output,
            "{}qb_file_seek({}, {});",
            indent, file_num_code, pos_code
        )
        .unwrap();

        Ok(())
    }

    // ==================== Error Handling Helper Methods ====================

    /// Emits ON ERROR GOTO.
    fn emit_on_error_goto(
        &self,
        indent: &str,
        target: &str,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        if target == "0" {
            writeln!(output, "{}_qb_error_handler = NULL;", indent).unwrap();
            writeln!(output, "{}_qb_error_resume_next = 0;", indent).unwrap();
        } else {
            let label = c_identifier(target);
            writeln!(output, "{}_qb_error_handler = &&{};", indent, label).unwrap();
            writeln!(output, "{}_qb_error_resume_next = 0;", indent).unwrap();
        }
        Ok(())
    }

    /// Emits ON ERROR RESUME NEXT.
    fn emit_on_error_resume_next(
        &self,
        indent: &str,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        writeln!(output, "{}_qb_error_resume_next = 1;", indent).unwrap();
        writeln!(output, "{}_qb_error_handler = NULL;", indent).unwrap();
        Ok(())
    }

    /// Emits RESUME statement.
    fn emit_resume(
        &self,
        indent: &str,
        target: &Option<crate::ast::ResumeTarget>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        match target {
            None => {
                // RESUME - retry the statement (complex, use goto)
                writeln!(
                    output,
                    "{}if (_qb_error_line) goto *_qb_error_line;",
                    indent
                )
                .unwrap();
            }
            Some(crate::ast::ResumeTarget::Next) => {
                // RESUME NEXT - continue at next statement
                writeln!(output, "{}_qb_err = 0;", indent).unwrap();
                writeln!(output, "{}/* RESUME NEXT - continue execution */", indent).unwrap();
            }
            Some(crate::ast::ResumeTarget::Label(label)) => {
                let c_label = c_identifier(label);
                writeln!(output, "{}_qb_err = 0;", indent).unwrap();
                writeln!(output, "{}goto {};", indent, c_label).unwrap();
            }
        }
        Ok(())
    }

    /// Emits ERROR statement.
    fn emit_error_stmt(
        &self,
        indent: &str,
        code: &TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let code_expr = emit_expr(code)?;
        writeln!(output, "{}qb_error({});", indent, code_expr).unwrap();
        Ok(())
    }

    // ==================== Computed Control Flow Helper Methods ====================

    /// Emits ON...GOTO.
    fn emit_on_goto(
        &self,
        indent: &str,
        selector: &TypedExpr,
        targets: &[String],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let sel_code = emit_expr(selector)?;

        writeln!(output, "{}switch ((int32_t)({}) - 1) {{", indent, sel_code).unwrap();
        for (i, target) in targets.iter().enumerate() {
            let c_label = c_identifier(target);
            writeln!(output, "{}    case {}: goto {}; break;", indent, i, c_label).unwrap();
        }
        writeln!(output, "{}    default: break;", indent).unwrap();
        writeln!(output, "{}}}", indent).unwrap();

        Ok(())
    }

    /// Emits ON...GOSUB.
    fn emit_on_gosub(
        &mut self,
        indent: &str,
        selector: &TypedExpr,
        targets: &[String],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let sel_code = emit_expr(selector)?;
        let return_label = self.next_label("on_gosub_ret");

        writeln!(output, "{}switch ((int32_t)({}) - 1) {{", indent, sel_code).unwrap();
        for (i, target) in targets.iter().enumerate() {
            let c_label = c_identifier(target);
            writeln!(
                output,
                "{}    case {}: _gosub_stack[_gosub_sp++] = &&{}; goto {}; break;",
                indent, i, return_label, c_label
            )
            .unwrap();
        }
        writeln!(output, "{}    default: break;", indent).unwrap();
        writeln!(output, "{}}}", indent).unwrap();
        writeln!(output, "{}{}:;", indent, return_label).unwrap();

        Ok(())
    }

    // ==================== DEF FN Helper Methods ====================

    /// Emits DEF FN as an inline function or macro.
    fn emit_def_fn(
        &self,
        name: &str,
        params: &[TypedParameter],
        return_type: &BasicType,
        body: &TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let fn_name = format!("_fn_{}", c_identifier(name));
        let c_return_type = c_type(return_type);

        // Emit as a static inline function
        let param_list = if params.is_empty() {
            "void".to_string()
        } else {
            params
                .iter()
                .map(|p| format!("{} {}", c_type(&p.basic_type), c_identifier(&p.name)))
                .collect::<Vec<_>>()
                .join(", ")
        };

        let body_code = emit_expr(body)?;

        writeln!(
            output,
            "static inline {} {}({}) {{ return {}; }}",
            c_return_type, fn_name, param_list, body_code
        )
        .unwrap();

        Ok(())
    }

    /// Emits multi-line DEF FN as a static function.
    ///
    /// In multi-line DEF FN, the return value is set by assigning to the
    /// function name (e.g., `FNSquare = x * x`). We emit a local variable
    /// for the return value and return it at the end.
    fn emit_def_fn_multiline(
        &mut self,
        name: &str,
        params: &[TypedParameter],
        return_type: &BasicType,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let fn_name = format!("_fn_{}", c_identifier(name));
        let c_return_type = c_type(return_type);

        // Parameter list
        let param_list = if params.is_empty() {
            "void".to_string()
        } else {
            params
                .iter()
                .map(|p| format!("{} {}", c_type(&p.basic_type), c_identifier(&p.name)))
                .collect::<Vec<_>>()
                .join(", ")
        };

        // Function header
        writeln!(
            output,
            "static {} {}({}) {{",
            c_return_type, fn_name, param_list
        )
        .unwrap();

        // Return value variable (initialized to default)
        let return_var = format!("_fn_{}", c_identifier(name));
        let init = default_init(return_type);
        writeln!(output, "    {} {} = {};", c_return_type, return_var, init).unwrap();

        // Emit body statements
        let old_indent = self.indent;
        self.indent = 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent = old_indent;

        // Return the result
        writeln!(output, "    return {};", return_var).unwrap();
        writeln!(output, "}}").unwrap();

        Ok(())
    }

    // ==================== REDIM Helper Methods ====================

    /// Emits REDIM statement.
    ///
    /// REDIM with _PRESERVE keeps existing array values and zeros new elements.
    /// Without _PRESERVE, the entire array is zeroed.
    ///
    /// Uses a static size-tracking variable to remember the array's byte size
    /// across multiple REDIMs, enabling proper _PRESERVE behavior.
    fn emit_redim(
        &self,
        indent: &str,
        preserve: bool,
        name: &str,
        element_type: &BasicType,
        dimensions: &[TypedArrayDimension],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let c_elem_type = c_type(element_type);
        let size_var = format!("{}_sz__", c_name);

        // Calculate total size
        if dimensions.is_empty() {
            writeln!(output, "{}/* REDIM {} - no dimensions */", indent, name).unwrap();
            return Ok(());
        }

        // Calculate new size expression
        let size_expr = dimensions
            .iter()
            .map(|d| format!("({} - {} + 1)", d.upper, d.lower))
            .collect::<Vec<_>>()
            .join(" * ");

        if preserve {
            // REDIM _PRESERVE: Keep existing values, zero only new elements
            // We track the old byte size with a static variable
            writeln!(output, "{}{{", indent).unwrap();
            writeln!(output, "{}    static size_t {} = 0;", indent, size_var).unwrap();
            writeln!(
                output,
                "{}    size_t new_sz__ = sizeof({}) * ({});",
                indent, c_elem_type, size_expr
            )
            .unwrap();
            writeln!(
                output,
                "{}    {} = realloc({}, new_sz__);",
                indent, c_name, c_name
            )
            .unwrap();
            // Zero only the new portion if array grew
            writeln!(
                output,
                "{}    if (new_sz__ > {}) memset((char*){} + {}, 0, new_sz__ - {});",
                indent, size_var, c_name, size_var, size_var
            )
            .unwrap();
            writeln!(output, "{}    {} = new_sz__;", indent, size_var).unwrap();
            writeln!(output, "{}}}", indent).unwrap();
        } else {
            // Regular REDIM: Reallocate and zero entire array
            writeln!(output, "{}{{", indent).unwrap();
            writeln!(
                output,
                "{}    size_t new_sz__ = sizeof({}) * ({});",
                indent, c_elem_type, size_expr
            )
            .unwrap();
            writeln!(
                output,
                "{}    {} = realloc({}, new_sz__);",
                indent, c_name, c_name
            )
            .unwrap();
            writeln!(output, "{}    memset({}, 0, new_sz__);", indent, c_name).unwrap();
            writeln!(output, "{}}}", indent).unwrap();
        }

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

/// Emits function/sub parameters.
pub(super) fn emit_params(params: &[TypedParameter]) -> String {
    if params.is_empty() {
        return "void".to_string();
    }

    params
        .iter()
        .map(|p| {
            let c_ty = c_type(&p.basic_type);
            let c_name = c_identifier(&p.name);
            if p.by_val {
                format!("{} {}", c_ty, c_name)
            } else {
                format!("{}* {}", c_ty, c_name)
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}
