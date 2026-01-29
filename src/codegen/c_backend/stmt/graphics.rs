//! Graphics statement code generation.
//!
//! This module handles the emission of C code for all graphics-related statements,
//! including SCREEN, CLS, COLOR, PSET, LINE, CIRCLE, PAINT, and QB64 graphics extensions.

use crate::ast::ImageScaleMode;
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedStatementKind;
use crate::writeln_code;

use super::StmtEmitter;
use crate::codegen::c_backend::expr::emit_string_data_access;
use crate::codegen::c_backend::types::c_identifier;

/// Emits code for graphics-related statements.
pub(super) fn emit_graphics_stmt(
    emitter: &mut StmtEmitter,
    kind: &TypedStatementKind,
    indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    match kind {
        TypedStatementKind::Screen {
            mode,
            color_switch,
            active_page,
            visual_page,
        } => {
            // SCREEN [mode][,[colorswitch]][,[apage]][,[vpage]]
            let mode_code = mode
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());
            let color_code = color_switch
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());
            let apage_code = active_page
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());
            let vpage_code = visual_page
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());

            // Emit SCREEN call with error checking
            writeln_code!(
                output,
                "{}if (qb_gfx_screen((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){})) {{",
                indent,
                mode_code,
                color_code,
                apage_code,
                vpage_code
            )?;
            writeln_code!(
                output,
                "{}    fprintf(stderr, \"Error: Graphics initialization failed\\n\");",
                indent
            )?;
            writeln_code!(output, "{}    fflush(stderr);", indent)?;
            writeln_code!(output, "{}    return 1;", indent)?;
            writeln_code!(output, "{}}}", indent)?;

            // If $SCREENHIDE was requested, hide the window after initialization
            if emitter.screen_hide_requested {
                writeln_code!(output, "{}qb_screenhide();", indent)?;
            }
        }

        TypedStatementKind::Cls { mode } => {
            if let Some(mode_expr) = mode {
                let mode_code = emitter.emit_expr(mode_expr)?;
                writeln_code!(output, "{}qb_gfx_cls_mode((int32_t){});", indent, mode_code)?;
            } else {
                writeln_code!(output, "{}qb_gfx_cls();", indent)?;
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
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());
            let bg_code = background
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());
            // Border is ignored in modern systems (was CGA/EGA text mode only)
            // We accept it for compatibility but don't use it
            let _border_code = border.as_ref().map(|e| emitter.emit_expr(e)).transpose()?;
            writeln_code!(
                output,
                "{}qb_gfx_color((int32_t){}, (int32_t){});",
                indent,
                fg_code,
                bg_code
            )?;
        }

        TypedStatementKind::Locate { row, col } => {
            // LOCATE with optional parameters - use -1 to indicate "unchanged"
            let row_code = row
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or("-1".to_string());
            let col_code = col
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or("-1".to_string());
            writeln_code!(
                output,
                "{}qb_gfx_locate((int32_t){}, (int32_t){});",
                indent,
                row_code,
                col_code
            )?;
        }

        TypedStatementKind::Pset { step, x, y, color } => {
            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            let step_int = if *step { 1 } else { 0 };
            if let Some(c) = color {
                let c_code = emitter.emit_expr(c)?;
                writeln_code!(
                    output,
                    "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, (uint32_t){}, {});",
                    indent,
                    x_code,
                    y_code,
                    c_code,
                    step_int
                )?;
            } else {
                // Use current foreground color (pass -1 to signal "use current")
                writeln_code!(
                    output,
                    "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, 0xFFFFFFFF, {});",
                    indent,
                    x_code,
                    y_code,
                    step_int
                )?;
            }
        }

        TypedStatementKind::Preset { step, x, y } => {
            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            let step_int = if *step { 1 } else { 0 };
            // PRESET plots in background color - pass 0 (black) by default
            writeln_code!(
                output,
                "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, 0xFF000000, {});",
                indent,
                x_code,
                y_code,
                step_int
            )?;
        }

        TypedStatementKind::Line {
            x1,
            y1,
            x2,
            y2,
            step2,
            color,
            box_style,
            style,
        } => {
            let x2_code = emitter.emit_expr(x2)?;
            let y2_code = emitter.emit_expr(y2)?;
            let color_code = if let Some(c) = color {
                emitter.emit_expr(c)?
            } else {
                "0xFFFFFFFF".to_string() // Use current foreground
            };

            // Handle optional start coordinates (use 0,0 as default for now)
            let x1_code = if let Some(e) = x1 {
                emitter.emit_expr(e)?
            } else {
                "0".to_string()
            };
            let y1_code = if let Some(e) = y1 {
                emitter.emit_expr(e)?
            } else {
                "0".to_string()
            };

            // In LINE statement, STEP only applies to the endpoint (step2)
            // step1 = 0 (first point is absolute), step2 = whether endpoint is relative
            let step2_flag = if *step2 { "1" } else { "0" };

            // Style pattern: 16-bit value specifying line pattern (e.g., 0xCCCC for dashed)
            // - 0xFFFF = solid line (no pattern)
            // - Other values = bit pattern that repeats along the line
            // Only valid with box outlines (B), ignored for filled boxes (BF) and plain lines
            // The runtime will apply the pattern for box outlines, ignore it for others
            let style_code = if let Some(s) = style {
                // Cast to uint16_t to ensure proper type and mask to 16 bits
                // The style pattern is a 16-bit value where each bit represents
                // whether a pixel should be drawn (1) or skipped (0)
                format!("(uint16_t)(({}) & 0xFFFF)", emitter.emit_expr(s)?)
            } else {
                "0xFFFF".to_string() // No style = solid line
            };

            match box_style {
                None => {
                    // Plain line: style is ignored, but we pass it anyway for API consistency
                    writeln_code!(
                        output,
                        "{}qb_gfx_line_step((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, 0, {}, {});",
                        indent,
                        x1_code,
                        y1_code,
                        x2_code,
                        y2_code,
                        color_code,
                        step2_flag,
                        style_code
                    )?;
                }
                Some(false) => {
                    // Box (outline): style pattern applies
                    writeln_code!(
                        output,
                        "{}qb_gfx_box_step((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, 0, 0, {}, {});",
                        indent,
                        x1_code,
                        y1_code,
                        x2_code,
                        y2_code,
                        color_code,
                        step2_flag,
                        style_code
                    )?;
                }
                Some(true) => {
                    // Filled box: style is ignored, but we pass it anyway for API consistency
                    writeln_code!(
                        output,
                        "{}qb_gfx_box_step((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, 1, 0, {}, {});",
                        indent,
                        x1_code,
                        y1_code,
                        x2_code,
                        y2_code,
                        color_code,
                        step2_flag,
                        style_code
                    )?;
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
            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            let r_code = emitter.emit_expr(radius)?;
            let color_code = if let Some(c) = color {
                emitter.emit_expr(c)?
            } else {
                "0xFFFFFFFF".to_string()
            };
            let filled_int = if *filled { 1 } else { 0 };
            let step_int = if *step { 1 } else { 0 };
            writeln_code!(
                output,
                "{}qb_gfx_circle_step((int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, {}, {});",
                indent,
                x_code,
                y_code,
                r_code,
                color_code,
                filled_int,
                step_int
            )?;
        }

        TypedStatementKind::Paint {
            step,
            x,
            y,
            color,
            border,
        } => {
            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            let color_code = if let Some(c) = color {
                emitter.emit_expr(c)?
            } else {
                "0xFFFFFFFF".to_string()
            };
            let border_code = if let Some(b) = border {
                emitter.emit_expr(b)?
            } else {
                color_code.clone() // Default border = fill color
            };
            let step_int = if *step { 1 } else { 0 };
            writeln_code!(
                output,
                "{}qb_gfx_paint_step((int32_t){}, (int32_t){}, (uint32_t){}, (uint32_t){}, {});",
                indent,
                x_code,
                y_code,
                color_code,
                border_code,
                step_int
            )?;
        }

        TypedStatementKind::GfxDisplay => {
            writeln_code!(output, "{}qb_gfx_display();", indent)?;
        }

        TypedStatementKind::ControlChr { enabled } => {
            writeln_code!(
                output,
                "{}qb_controlchr({});",
                indent,
                if *enabled { "1" } else { "0" }
            )?;
        }

        TypedStatementKind::MapUnicode {
            unicode_value,
            char_position,
        } => {
            let unicode_code = emitter.emit_expr(unicode_value)?;
            let char_code = emitter.emit_expr(char_position)?;
            writeln_code!(
                output,
                "{}qb_mapunicode((int32_t){}, (int32_t){});",
                indent,
                unicode_code,
                char_code
            )?;
        }

        TypedStatementKind::GfxResize { enabled } => {
            writeln_code!(
                output,
                "{}qb_gfx_resize({});",
                indent,
                if *enabled { "1" } else { "0" }
            )?;
        }

        TypedStatementKind::Palette { attribute, color } => {
            match (attribute, color) {
                (Some(attr), Some(col)) => {
                    let attr_code = emitter.emit_expr(attr)?;
                    let col_code = emitter.emit_expr(col)?;
                    writeln_code!(
                        output,
                        "{}qb_gfx_palette((int32_t){}, (uint32_t){});",
                        indent,
                        attr_code,
                        col_code
                    )?;
                }
                _ => {
                    // PALETTE without arguments - reset all palette entries
                    writeln_code!(output, "{}qb_gfx_palette_reset();", indent)?;
                }
            }
        }

        TypedStatementKind::Pcopy { source, dest } => {
            let src_code = emitter.emit_expr(source)?;
            let dst_code = emitter.emit_expr(dest)?;
            writeln_code!(
                output,
                "{}qb_gfx_pcopy((int32_t){}, (int32_t){});",
                indent,
                src_code,
                dst_code
            )?;
        }
        TypedStatementKind::Width { columns, rows } => {
            let cols_code = emitter.emit_expr(columns)?;
            if let Some(r) = rows {
                let rows_code = emitter.emit_expr(r)?;
                writeln_code!(
                    output,
                    "{}qb_gfx_set_width((uint32_t){}, (uint32_t){});",
                    indent,
                    cols_code,
                    rows_code
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}qb_gfx_set_width((uint32_t){}, 0);",
                    indent,
                    cols_code
                )?;
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
                let x1 = emitter.emit_expr(&c.x1)?;
                let y1 = emitter.emit_expr(&c.y1)?;
                let x2 = emitter.emit_expr(&c.x2)?;
                let y2 = emitter.emit_expr(&c.y2)?;
                let fill = fill_color
                    .as_ref()
                    .map(|e| emitter.emit_expr(e))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let border = border_color
                    .as_ref()
                    .map(|e| emitter.emit_expr(e))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                writeln_code!(
                    output,
                    "{}qb_gfx_view({}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){});",
                    indent,
                    screen_int,
                    x1,
                    y1,
                    x2,
                    y2,
                    fill,
                    border
                )?;
            } else {
                // Reset viewport
                writeln_code!(output, "{}qb_gfx_view_reset();", indent)?;
            }
        }

        TypedStatementKind::ViewPrint { top, bottom } => {
            if let (Some(t), Some(b)) = (top, bottom) {
                let top_code = emitter.emit_expr(t)?;
                let bottom_code = emitter.emit_expr(b)?;
                writeln_code!(
                    output,
                    "{}qb_view_print((int32_t){}, (int32_t){});",
                    indent,
                    top_code,
                    bottom_code
                )?;
            } else {
                // Reset text viewport
                writeln_code!(output, "{}qb_view_print_reset();", indent)?;
            }
        }

        TypedStatementKind::WindowCoords { screen, coords } => {
            let screen_int = if *screen { 1 } else { 0 };
            if let Some(c) = coords {
                let x1 = emitter.emit_expr(&c.x1)?;
                let y1 = emitter.emit_expr(&c.y1)?;
                let x2 = emitter.emit_expr(&c.x2)?;
                let y2 = emitter.emit_expr(&c.y2)?;
                writeln_code!(
                    output,
                    "{}qb_gfx_window({}, (double){}, (double){}, (double){}, (double){});",
                    indent,
                    screen_int,
                    x1,
                    y1,
                    x2,
                    y2
                )?;
            } else {
                // Reset window coordinates
                writeln_code!(output, "{}qb_gfx_window_reset();", indent)?;
            }
        }

        TypedStatementKind::DrawCmd { commands } => {
            let cmd_code = emitter.emit_expr(commands)?;
            writeln_code!(output, "{}qb_gfx_draw({});", indent, cmd_code)?;
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
            let x1_code = emitter.emit_expr(x1)?;
            let y1_code = emitter.emit_expr(y1)?;
            let x2_code = emitter.emit_expr(x2)?;
            let y2_code = emitter.emit_expr(y2)?;
            let arr_name = c_identifier(array_name);

            // Calculate array pointer - either base or with offset
            let arr_ptr = if array_indices.is_empty() {
                arr_name.clone()
            } else {
                // For multi-dimensional arrays, generate index expression
                let indices: Vec<String> = array_indices
                    .iter()
                    .map(|e| emitter.emit_expr(e))
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
            writeln_code!(
                output,
                "{}{}((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                indent,
                func_name,
                x1_code,
                y1_code,
                x2_code,
                y2_code,
                arr_ptr
            )?;
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

            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            let arr_name = c_identifier(array_name);

            // Calculate array pointer
            let arr_ptr = if array_indices.is_empty() {
                arr_name.clone()
            } else {
                // For multi-dimensional arrays, generate index expression
                let indices: Vec<String> = array_indices
                    .iter()
                    .map(|e| emitter.emit_expr(e))
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
                emitter.emit_expr(tc)?
            } else {
                "-1".to_string() // No transparent color
            };

            let clip_flag = if *clip { "1" } else { "0" };

            if *step {
                writeln_code!(
                    output,
                    "{}qb_gfx_put_step((int32_t){}, (int32_t){}, {}, {}, {}, (int32_t){});",
                    indent,
                    x_code,
                    y_code,
                    arr_ptr,
                    action_code,
                    clip_flag,
                    trans_code
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}qb_gfx_put((int32_t){}, (int32_t){}, {}, {}, {}, (int32_t){});",
                    indent,
                    x_code,
                    y_code,
                    arr_ptr,
                    action_code,
                    clip_flag,
                    trans_code
                )?;
            }
        }
        TypedStatementKind::FreeImage { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_gfx_freeimage((int32_t){});", indent, h_code)?;
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
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());
            let dst_handle = dest
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "-1".to_string());

            // Scale mode: 0 = default, 1 = smooth (bilinear), 2 = stretch (nearest-neighbor)
            let scale_code = match scale_mode {
                ImageScaleMode::Default => "0",
                ImageScaleMode::Smooth => "1",
                ImageScaleMode::Stretch => "2",
            };

            if let (Some(dc), Some(sc)) = (dest_coords, source_coords) {
                let dx1 = emitter.emit_expr(&dc.x1)?;
                let dy1 = emitter.emit_expr(&dc.y1)?;
                let dx2 = emitter.emit_expr(&dc.x2)?;
                let dy2 = emitter.emit_expr(&dc.y2)?;
                let sx1 = emitter.emit_expr(&sc.x1)?;
                let sy1 = emitter.emit_expr(&sc.y1)?;
                let sx2 = emitter.emit_expr(&sc.x2)?;
                let sy2 = emitter.emit_expr(&sc.y2)?;
                writeln_code!(
                    output,
                    "{}qb_gfx_putimage_full((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                    indent,
                    dx1,
                    dy1,
                    dx2,
                    dy2,
                    src_handle,
                    dst_handle,
                    sx1,
                    sy1,
                    sx2,
                    sy2,
                    scale_code
                )?;
            } else if let Some(dc) = dest_coords {
                let dx1 = emitter.emit_expr(&dc.x1)?;
                let dy1 = emitter.emit_expr(&dc.y1)?;
                let dx2 = emitter.emit_expr(&dc.x2)?;
                let dy2 = emitter.emit_expr(&dc.y2)?;
                writeln_code!(
                    output,
                    "{}qb_gfx_putimage((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                    indent,
                    dx1,
                    dy1,
                    dx2,
                    dy2,
                    src_handle,
                    dst_handle,
                    scale_code
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}qb_gfx_putimage_simple((int32_t){}, (int32_t){}, {});",
                    indent,
                    src_handle,
                    dst_handle,
                    scale_code
                )?;
            }
        }

        TypedStatementKind::SourceImg { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_gfx_source((int32_t){});", indent, h_code)?;
        }

        TypedStatementKind::DestImg { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_gfx_dest((int32_t){});", indent, h_code)?;
        }

        TypedStatementKind::PrintStringStmt { x, y, text } => {
            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            let text_code = emitter.emit_expr(text)?;
            // qb_gfx_printstring expects const char*, not qb_string*
            let text_data = emit_string_data_access(text, &text_code, &emitter.config.runtime_mode);
            writeln_code!(
                output,
                "{}qb_gfx_printstring((int32_t){}, (int32_t){}, {});",
                indent,
                x_code,
                y_code,
                text_data
            )?;
        }

        TypedStatementKind::AutoDisplay { enabled } => {
            let enable_int = if *enabled { 1 } else { 0 };
            writeln_code!(output, "{}qb_gfx_autodisplay({});", indent, enable_int)?;
        }
        _ => return Ok(()), // Not a graphics statement
    }
    Ok(())
}
