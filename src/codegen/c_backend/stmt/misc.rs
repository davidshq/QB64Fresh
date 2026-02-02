//! Miscellaneous statement code generation.
//!
//! This module handles the emission of C code for miscellaneous statements that don't
//! fit into other categories, including SWAP, CONTINUE, RUN, CHAIN, event handlers,
//! window/desktop statements, and assertions.

use crate::ast::{AllowFullScreenMode, EventControlMode, FullScreenMode};
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedStatementKind;
use crate::semantic::types::BasicType;
use crate::writeln_code;

use super::StmtEmitter;
use crate::ast::PrintSeparator;
use crate::codegen::c_backend::expr::emit_string_data_access;
use crate::codegen::c_backend::types::{c_identifier, c_type};

/// Emits code for miscellaneous statements.
pub(super) fn emit_misc_stmt(
    emitter: &mut StmtEmitter,
    kind: &TypedStatementKind,
    indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    match kind {
        TypedStatementKind::Label { name } => {
            let c_label = emitter.proc_label(name);
            if emitter.codegen.emitted_labels.insert(c_label.clone()) {
                writeln_code!(output, "{}:", c_label)?;
            }
        }
        TypedStatementKind::Comment(text) => {
            writeln_code!(output, "{}/* {} */", indent, text)?;
        }
        TypedStatementKind::Expression(expr) => {
            let expr_code = emitter.emit_expr(expr)?;
            writeln_code!(output, "{}{};", indent, expr_code)?;
        }
        TypedStatementKind::IncludeDirective { path } => {
            writeln_code!(output, "{}/* $INCLUDE: '{}' */", indent, path)?;
        }
        TypedStatementKind::DefSeg { segment } => {
            if let Some(seg_expr) = segment {
                let seg_code = emitter.emit_expr(seg_expr)?;
                writeln_code!(output, "{}qb_def_seg((int32_t){});", indent, seg_code)?;
            } else {
                writeln_code!(output, "{}qb_def_seg(-1);", indent)?;
            }
        }
        TypedStatementKind::Poke { address, value } => {
            let addr_code = emitter.emit_expr(address)?;
            let val_code = emitter.emit_expr(value)?;
            writeln_code!(
                output,
                "{}qb_poke((int32_t){}, (uint8_t){});",
                indent,
                addr_code,
                val_code
            )?;
        }
        TypedStatementKind::MemPutTyped {
            mem,
            offset,
            value,
            value_type,
        } => {
            let mem_code = emitter.emit_expr(mem)?;
            let offset_code = emitter.emit_expr(offset)?;
            let value_code = emitter.emit_expr(value)?;
            let c_ty = c_type(value_type);
            writeln_code!(
                output,
                "{indent}*(({c_ty}*)((char*)({mem_code}).offset + ({offset_code}))) = ({c_ty})({value_code});"
            )?;
        }
        TypedStatementKind::ConditionalBlock {
            condition,
            then_branch,
            elseif_branches,
            else_branch,
        } => {
            writeln_code!(output, "{}/* $IF {} */", indent, condition)?;
            for s in then_branch {
                emitter.emit_stmt(s, output)?;
            }
            for (elseif_cond, elseif_body) in elseif_branches {
                writeln_code!(output, "{}/* $ELSEIF {} */", indent, elseif_cond)?;
                for s in elseif_body {
                    emitter.emit_stmt(s, output)?;
                }
            }
            if let Some(else_body) = else_branch {
                writeln_code!(output, "{}/* $ELSE */", indent)?;
                for s in else_body {
                    emitter.emit_stmt(s, output)?;
                }
            }
            writeln_code!(output, "{}/* $END IF */", indent)?;
        }
        TypedStatementKind::ConditionalBlockResolved {
            original_condition,
            statements,
        } => {
            if !statements.is_empty() {
                writeln_code!(
                    output,
                    "{}/* Conditional compilation: {} */",
                    indent,
                    original_condition
                )?;
                for s in statements {
                    emitter.emit_stmt(s, output)?;
                }
            }
        }
        TypedStatementKind::Call { name, args, params } => {
            super::call::emit_call_stmt(emitter, name, args, params, indent, output)?;
        }
        TypedStatementKind::Swap { left, right } => {
            let mut left_code = emitter.emit_expr(left)?;
            let mut right_code = emitter.emit_expr(right)?;
            let temp_var = emitter.next_label("swap_temp");

            // Fixed-length strings need special handling (C arrays can't be assigned directly)
            if let BasicType::FixedString(n) = &left.basic_type {
                // For fixed-length strings, use strcpy for the swap
                // Unwrap qb_str_from_c() wrappers since strcpy expects char* (array names)
                use crate::codegen::c_backend::expr::unwrap_qb_str_from_c;
                left_code = unwrap_qb_str_from_c(&left_code);
                right_code = unwrap_qb_str_from_c(&right_code);
                writeln_code!(output, "{}{{ char {}[{}];", indent, temp_var, n + 1)?;
                writeln_code!(output, "{}    strcpy({}, {});", indent, temp_var, left_code)?;
                writeln_code!(
                    output,
                    "{}    strcpy({}, {});",
                    indent,
                    left_code,
                    right_code
                )?;
                writeln_code!(
                    output,
                    "{}    strcpy({}, {}); }}",
                    indent,
                    right_code,
                    temp_var
                )?;
            } else {
                let c_ty = c_type(&left.basic_type);
                writeln_code!(output, "{}{} {} = {};", indent, c_ty, temp_var, left_code)?;
                writeln_code!(output, "{}{} = {};", indent, left_code, right_code)?;
                writeln_code!(output, "{}{} = {};", indent, right_code, temp_var)?;
            }
        }

        TypedStatementKind::Continue { continue_type } => {
            let _ = continue_type;
            writeln_code!(output, "{}continue;", indent)?;
        }

        TypedStatementKind::TypeDefinition { .. } => {
            // TYPE definitions are collected and emitted upfront by collect_type_definitions()
            // in mod.rs before global variables, so we skip them here.
        }

        TypedStatementKind::Data { .. } => {
            // DATA statements are handled in collect_data_values during global emission
        }

        TypedStatementKind::MouseHide => {
            writeln_code!(output, "{}qb_mouse_hide();", indent)?;
        }

        TypedStatementKind::MouseShow => {
            writeln_code!(output, "{}qb_mouse_show();", indent)?;
        }

        TypedStatementKind::MouseMoveStmt { x, y } => {
            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            writeln_code!(
                output,
                "{}qb_mouse_move((int32_t){}, (int32_t){});",
                indent,
                x_code,
                y_code
            )?;
        }
        TypedStatementKind::ClipboardSet { text } => {
            let text_code = emitter.emit_expr(text)?;
            let text_access =
                emit_string_data_access(text, &text_code, &emitter.config.runtime_mode);
            writeln_code!(output, "{}qb_clipboard_set({});", indent, text_access)?;
        }
        TypedStatementKind::DeclareLibrary {
            library_name,
            is_dynamic,
            declarations,
        } => {
            // Static libraries: emit extern declarations here.
            // Dynamic libraries: handle/pointers and init are emitted in the
            // dynamic library section (qb_init_dynamic_libs); calls use qb_dyn_<c_name>.
            if *is_dynamic {
                writeln_code!(
                    output,
                    "{}// DECLARE DYNAMIC LIBRARY (loaded in qb_init_dynamic_libs)",
                    indent
                )?;
                if let Some(lib) = library_name {
                    writeln_code!(output, "{}// Library: {}", indent, lib)?;
                }
                // No extern declarations - we use function pointers (qb_dyn_<c_name>) emitted above.
            } else {
                writeln_code!(output, "{}// DECLARE LIBRARY - extern declarations", indent)?;
                if let Some(lib) = library_name {
                    writeln_code!(output, "{}// Library: {}", indent, lib)?;
                }
                for decl in declarations {
                    emitter.emit_extern_declaration(indent, decl, output)?;
                }
            }
        }

        // Forward declarations - no code generated, just comments for documentation
        TypedStatementKind::DeclareSub { name } => {
            writeln_code!(output, "{}/* DECLARE SUB {} */", indent, name)?;
        }

        TypedStatementKind::DeclareFunction { name } => {
            writeln_code!(output, "{}/* DECLARE FUNCTION {} */", indent, name)?;
        }
        TypedStatementKind::Run { target } => {
            // RUN: NULL = no-op (restart); non-NULL = run program then exit (minimal implementation)
            if let Some(t) = target {
                let target_code = emitter.emit_expr(t)?;
                writeln_code!(output, "{}qb_run({});", indent, target_code)?;
            } else {
                writeln_code!(output, "{}qb_run(NULL);", indent)?;
            }
        }

        TypedStatementKind::Chain { filename } => {
            let filename_code = emitter.emit_expr(filename)?;
            writeln_code!(output, "{}qb_chain({});", indent, filename_code)?;
        }

        TypedStatementKind::Tron => {
            writeln_code!(output, "{}qb_trace_on = 1;", indent)?;
        }

        TypedStatementKind::Troff => {
            writeln_code!(output, "{}qb_trace_on = 0;", indent)?;
        }

        TypedStatementKind::Lprint { values, newline } => {
            // Print to printer (LPT1) - similar to PRINT but to a different stream
            for item in values {
                let expr_code = emitter.emit_expr(&item.expr)?;
                writeln_code!(output, "{}qb_lprint({});", indent, expr_code)?;
                if item.separator == Some(PrintSeparator::Comma) {
                    writeln_code!(output, "{}qb_lprint_tab();", indent)?;
                }
            }
            if *newline {
                writeln_code!(output, "{}qb_lprint_newline();", indent)?;
            }
        }

        TypedStatementKind::FilesStmt { filespec } => {
            if let Some(spec) = filespec {
                let spec_code = emitter.emit_expr(spec)?;
                writeln_code!(output, "{}qb_files({});", indent, spec_code)?;
            } else {
                writeln_code!(output, "{}qb_files(NULL);", indent)?;
            }
        }

        TypedStatementKind::FieldStmt { file_num, fields } => {
            let file_num_code = emitter.emit_expr(file_num)?;
            writeln_code!(
                output,
                "{}qb_field_start((int32_t)({}));",
                indent,
                file_num_code
            )?;
            for field in fields {
                let width_code = emitter.emit_expr(&field.width)?;
                writeln_code!(
                    output,
                    "{}qb_field_add((int32_t)({}), &{});",
                    indent,
                    width_code,
                    field.variable
                )?;
            }
        }

        TypedStatementKind::Lset { variable, value } => {
            let value_code = emitter.emit_expr(value)?;
            let c_var = c_identifier(variable);
            writeln_code!(output, "{}qb_lset(&{}, {});", indent, c_var, value_code)?;
        }

        TypedStatementKind::Rset { variable, value } => {
            let value_code = emitter.emit_expr(value)?;
            let c_var = c_identifier(variable);
            writeln_code!(output, "{}qb_rset(&{}, {});", indent, c_var, value_code)?;
        }

        TypedStatementKind::OnKey { key_num, target } => {
            let key_code = emitter.emit_expr(key_num)?;
            writeln_code!(
                output,
                "{}qb_on_key((int32_t)({}), &&{});",
                indent,
                key_code,
                target
            )?;
        }

        TypedStatementKind::KeyControl { key_num, mode } => {
            let key_code = emitter.emit_expr(key_num)?;
            let mode_code = match mode {
                EventControlMode::On => "1",
                EventControlMode::Off => "0",
                EventControlMode::Stop => "2",
            };
            writeln_code!(
                output,
                "{}qb_key_control((int32_t)({}), {});",
                indent,
                key_code,
                mode_code
            )?;
        }

        TypedStatementKind::OnTimer { interval, target } => {
            let interval_code = emitter.emit_expr(interval)?;
            writeln_code!(
                output,
                "{}qb_on_timer({}, &&{});",
                indent,
                interval_code,
                target
            )?;
        }

        TypedStatementKind::TimerControl { mode } => {
            let mode_code = match mode {
                EventControlMode::On => "1",
                EventControlMode::Off => "0",
                EventControlMode::Stop => "2",
            };
            writeln_code!(output, "{}qb_timer_control({});", indent, mode_code)?;
        }

        TypedStatementKind::StrigControl { button_num, mode } => {
            let btn_code = emitter.emit_expr(button_num)?;
            let mode_code = match mode {
                EventControlMode::On => "1",
                EventControlMode::Off => "0",
                EventControlMode::Stop => "2",
            };
            writeln_code!(
                output,
                "{}qb_strig_control((int32_t)({}), {});",
                indent,
                btn_code,
                mode_code
            )?;
        }

        TypedStatementKind::OnStrig { button_num, target } => {
            // Generate a unique event ID for this handler
            emitter.events.strig_event_counter += 1;
            let event_id = emitter.events.strig_event_counter;
            emitter
                .events
                .strig_handlers
                .push((event_id, target.clone()));

            let btn_code = emitter.emit_expr(button_num)?;
            writeln_code!(
                output,
                "{}qb_on_strig((int32_t)({}), {});",
                indent,
                btn_code,
                event_id
            )?;
        }

        TypedStatementKind::OnCom { port_num, target } => {
            let port_code = emitter.emit_expr(port_num)?;
            writeln_code!(
                output,
                "{}qb_on_com((int32_t)({}), &&{});",
                indent,
                port_code,
                target
            )?;
        }

        TypedStatementKind::ComControl { port_num, mode } => {
            let port_code = emitter.emit_expr(port_num)?;
            let mode_code = match mode {
                EventControlMode::On => "1",
                EventControlMode::Off => "0",
                EventControlMode::Stop => "2",
            };
            writeln_code!(
                output,
                "{}qb_com_control((int32_t)({}), {});",
                indent,
                port_code,
                mode_code
            )?;
        }

        TypedStatementKind::OnPen { target } => {
            writeln_code!(output, "{}qb_on_pen(&&{});", indent, target)?;
        }

        TypedStatementKind::PenControl { mode } => {
            let mode_code = match mode {
                EventControlMode::On => "1",
                EventControlMode::Off => "0",
                EventControlMode::Stop => "2",
            };
            writeln_code!(output, "{}qb_pen_control({});", indent, mode_code)?;
        }

        TypedStatementKind::OnUevent { target } => {
            writeln_code!(output, "{}qb_on_uevent(&&{});", indent, target)?;
        }

        TypedStatementKind::UeventControl { mode } => {
            let mode_code = match mode {
                EventControlMode::On => "1",
                EventControlMode::Off => "0",
                EventControlMode::Stop => "2",
            };
            writeln_code!(output, "{}qb_uevent_control({});", indent, mode_code)?;
        }

        TypedStatementKind::UeventTrigger => {
            writeln_code!(output, "{}qb_uevent_trigger();", indent)?;
        }

        TypedStatementKind::OnSignal { signal_num, target } => {
            let signal_code = emitter.emit_expr(signal_num)?;
            writeln_code!(
                output,
                "{}qb_on_signal((int32_t)({}), &&{});",
                indent,
                signal_code,
                target
            )?;
        }

        TypedStatementKind::SignalControl { signal_num, mode } => {
            let signal_code = emitter.emit_expr(signal_num)?;
            let mode_code = match mode {
                EventControlMode::On => "1",
                EventControlMode::Off => "0",
                EventControlMode::Stop => "2",
            };
            writeln_code!(
                output,
                "{}qb_signal_control((int32_t)({}), {});",
                indent,
                signal_code,
                mode_code
            )?;
        }

        TypedStatementKind::OutPort { port, value } => {
            let port_code = emitter.emit_expr(port)?;
            let value_code = emitter.emit_expr(value)?;
            writeln_code!(
                output,
                "{}qb_out((int32_t)({}), (int32_t)({}));",
                indent,
                port_code,
                value_code
            )?;
        }

        TypedStatementKind::InterruptStmt {
            int_num,
            in_regs,
            out_regs,
        } => {
            let int_code = emitter.emit_expr(int_num)?;
            writeln_code!(
                output,
                "{}qb_interrupt((int32_t)({}), &{}, &{});",
                indent,
                int_code,
                in_regs,
                out_regs
            )?;
        }

        TypedStatementKind::InterruptXStmt {
            int_num,
            in_regs,
            out_regs,
        } => {
            let int_code = emitter.emit_expr(int_num)?;
            writeln_code!(
                output,
                "{}qb_interruptx((int32_t)({}), &{}, &{});",
                indent,
                int_code,
                in_regs,
                out_regs
            )?;
        }

        TypedStatementKind::IoctlStmt {
            file_num,
            control_string,
        } => {
            let file_code = emitter.emit_expr(file_num)?;
            let string_code = emitter.emit_expr(control_string)?;
            writeln_code!(
                output,
                "{}qb_ioctl((int32_t)({}), {});",
                indent,
                file_code,
                string_code
            )?;
        }

        TypedStatementKind::FreeStmt => {
            writeln_code!(output, "{}qb_free();", indent)?;
        }

        TypedStatementKind::ClearStmt { stack_size } => {
            if let Some(size) = stack_size {
                let size_code = emitter.emit_expr(size)?;
                writeln_code!(output, "{}qb_clear((int32_t)({}));", indent, size_code)?;
            } else {
                writeln_code!(output, "{}qb_clear(0);", indent)?;
            }
        }

        TypedStatementKind::ResetStmt => {
            writeln_code!(output, "{}qb_reset();", indent)?;
        }

        // Window/Desktop statements (QB64)
        TypedStatementKind::TitleStmt { title } => {
            let title_code = emitter.emit_expr(title)?;
            writeln_code!(output, "{}qb_title({});", indent, title_code)?;
        }

        TypedStatementKind::ScreenMoveStmt { x, y, center } => {
            if *center {
                writeln_code!(output, "{}qb_screenmove_center();", indent)?;
            } else {
                let x_code = match x {
                    Some(e) => emitter.emit_expr(e)?,
                    None => "0".to_string(),
                };
                let y_code = match y {
                    Some(e) => emitter.emit_expr(e)?,
                    None => "0".to_string(),
                };
                writeln_code!(
                    output,
                    "{}qb_screenmove((int32_t)({}), (int32_t)({}));",
                    indent,
                    x_code,
                    y_code
                )?;
            }
        }

        TypedStatementKind::FullScreenStmt { mode } => {
            let mode_code = match mode {
                FullScreenMode::Stretch => "0",
                FullScreenMode::SquarePixels => "1",
                FullScreenMode::Off => "2",
            };
            writeln_code!(output, "{}qb_fullscreen({});", indent, mode_code)?;
        }

        TypedStatementKind::AllowFullScreenStmt { mode } => {
            let mode_code = match mode {
                AllowFullScreenMode::Stretch => "0",
                AllowFullScreenMode::SquarePixels => "1",
                AllowFullScreenMode::All => "2",
                AllowFullScreenMode::Off => "3",
            };
            writeln_code!(output, "{}qb_allowfullscreen({});", indent, mode_code)?;
        }

        TypedStatementKind::ScreenIconStmt => {
            writeln_code!(output, "{}qb_screenicon();", indent)?;
        }

        TypedStatementKind::IconStmt { handle } => {
            if let Some(h) = handle {
                let handle_code = emitter.emit_expr(h)?;
                writeln_code!(output, "{}qb_icon((int32_t)({}));", indent, handle_code)?;
            } else {
                writeln_code!(output, "{}qb_icon(0);", indent)?;
            }
        }

        TypedStatementKind::ScreenHideStmt => {
            writeln_code!(output, "{}qb_screenhide();", indent)?;
        }

        TypedStatementKind::ScreenShowStmt => {
            writeln_code!(output, "{}qb_screenshow();", indent)?;
        }

        TypedStatementKind::ConsoleTitleStmt { title } => {
            let title_code = emitter.emit_expr(title)?;
            writeln_code!(output, "{}qb_consoletitle({});", indent, title_code)?;
        }

        TypedStatementKind::ConsoleStmt { visible } => {
            writeln_code!(
                output,
                "{}qb_console({});",
                indent,
                if *visible { "1" } else { "0" }
            )?;
        }

        TypedStatementKind::AssertStmt { condition, message } => {
            let cond_code = emitter.emit_expr(condition)?;
            if let Some(msg) = message {
                let msg_code = emitter.emit_expr(msg)?;
                writeln_code!(output, "{}qb_assert({}, {});", indent, cond_code, msg_code)?;
            } else {
                writeln_code!(output, "{}qb_assert({}, NULL);", indent, cond_code)?;
            }
        }
        _ => return Ok(()), // Not a misc statement
    }
    Ok(())
}
