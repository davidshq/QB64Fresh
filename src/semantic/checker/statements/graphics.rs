//! Graphics statement type checking.
//!
//! This module handles type checking for graphics statements including:
//! - Basic graphics: SCREEN, CLS, COLOR, LOCATE
//! - Drawing primitives: PSET, PRESET, LINE, CIRCLE, PAINT
//! - Display control: _DISPLAY, _CONTROLCHR, _MAPUNICODE
//! - Screen management: PALETTE, _RESIZE, PCOPY, WIDTH, VIEW, WINDOW
//! - Bitmap operations: GET, PUT, DRAW
//! - QB64 extensions: _FREEIMAGE, _PUTIMAGE, _SOURCE, _DEST, _PRINTSTRING, _AUTODISPLAY

use crate::ast::{Expr, ImageScaleMode, PutAction, Span, ViewCoords};
use crate::semantic::typed_ir::*;

use super::super::TypeChecker;

impl<'a> TypeChecker<'a> {
    // ==================== Basic Graphics ====================

    /// Type checks SCREEN statement.
    ///
    /// SCREEN sets the video mode. All parameters are optional.
    ///
    /// # Arguments
    /// * `mode` - Screen mode number (e.g., 12, 13)
    /// * `color_switch` - Color mode (0 or 1 for some modes)
    /// * `active_page` - Active drawing page
    /// * `visual_page` - Visible display page
    pub(in crate::semantic::checker) fn check_screen(
        &mut self,
        mode: Option<&Expr>,
        color_switch: Option<&Expr>,
        active_page: Option<&Expr>,
        visual_page: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_mode = mode.map(|e| self.check_expr(e));
        let typed_color_switch = color_switch.map(|e| self.check_expr(e));
        let typed_active_page = active_page.map(|e| self.check_expr(e));
        let typed_visual_page = visual_page.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Screen {
                mode: typed_mode,
                color_switch: typed_color_switch,
                active_page: typed_active_page,
                visual_page: typed_visual_page,
            },
            span,
        )
    }

    /// Type checks CLS statement.
    ///
    /// CLS clears the screen or viewport.
    ///
    /// # Arguments
    /// * `mode` - Clear mode (0=all, 1=graphics, 2=text)
    pub(in crate::semantic::checker) fn check_cls(
        &mut self,
        mode: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_mode = mode.map(|e| self.check_expr(e));
        TypedStatement::new(TypedStatementKind::Cls { mode: typed_mode }, span)
    }

    /// Type checks COLOR statement.
    ///
    /// COLOR sets the foreground and background colors.
    ///
    /// # Arguments
    /// * `foreground` - Foreground color
    /// * `background` - Background color
    /// * `border` - Border color (legacy, rarely used)
    pub(in crate::semantic::checker) fn check_color(
        &mut self,
        foreground: Option<&Expr>,
        background: Option<&Expr>,
        border: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_fg = foreground.map(|e| self.check_expr(e));
        let typed_bg = background.map(|e| self.check_expr(e));
        let typed_border = border.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Color {
                foreground: typed_fg,
                background: typed_bg,
                border: typed_border,
            },
            span,
        )
    }

    /// Type checks LOCATE statement.
    ///
    /// LOCATE positions the text cursor.
    ///
    /// # Arguments
    /// * `row` - Row position (1-based)
    /// * `col` - Column position (1-based)
    pub(in crate::semantic::checker) fn check_locate(
        &mut self,
        row: Option<&Expr>,
        col: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_row = row.map(|e| self.check_expr(e));
        let typed_col = col.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Locate {
                row: typed_row,
                col: typed_col,
            },
            span,
        )
    }

    // ==================== Drawing Primitives ====================

    /// Type checks PSET statement.
    ///
    /// PSET sets a pixel at the specified coordinates.
    ///
    /// # Arguments
    /// * `step` - Whether coordinates are relative to last graphics position
    /// * `x` - X coordinate
    /// * `y` - Y coordinate
    /// * `color` - Optional pixel color
    pub(in crate::semantic::checker) fn check_pset(
        &mut self,
        step: bool,
        x: &Expr,
        y: &Expr,
        color: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_x = self.check_expr(x);
        let typed_y = self.check_expr(y);
        let typed_color = color.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Pset {
                step,
                x: typed_x,
                y: typed_y,
                color: typed_color,
            },
            span,
        )
    }

    /// Type checks PRESET statement.
    ///
    /// PRESET resets a pixel at the specified coordinates (sets to background color).
    ///
    /// # Arguments
    /// * `step` - Whether coordinates are relative to last graphics position
    /// * `x` - X coordinate
    /// * `y` - Y coordinate
    pub(in crate::semantic::checker) fn check_preset(
        &mut self,
        step: bool,
        x: &Expr,
        y: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_x = self.check_expr(x);
        let typed_y = self.check_expr(y);

        TypedStatement::new(
            TypedStatementKind::Preset {
                step,
                x: typed_x,
                y: typed_y,
            },
            span,
        )
    }

    /// Type checks LINE statement.
    ///
    /// LINE draws a line or box between two points.
    ///
    /// # Arguments
    /// * `x1`, `y1` - Start point (optional, defaults to last position)
    /// * `x2`, `y2` - End point
    /// * `step2` - Whether end point is relative
    /// * `color` - Line color
    /// * `box_style` - None=line, Some(false)=box, Some(true)=filled box
    /// * `style` - Line style pattern (16-bit)
    #[allow(clippy::too_many_arguments)]
    pub(in crate::semantic::checker) fn check_line(
        &mut self,
        x1: Option<&Expr>,
        y1: Option<&Expr>,
        x2: &Expr,
        y2: &Expr,
        step2: bool,
        color: Option<&Expr>,
        box_style: Option<bool>,
        style: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_x1 = x1.map(|e| self.check_expr(e));
        let typed_y1 = y1.map(|e| self.check_expr(e));
        let typed_x2 = self.check_expr(x2);
        let typed_y2 = self.check_expr(y2);
        let typed_color = color.map(|e| self.check_expr(e));
        let typed_style = style.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Line {
                x1: typed_x1,
                y1: typed_y1,
                x2: typed_x2,
                y2: typed_y2,
                step2,
                color: typed_color,
                box_style,
                style: typed_style,
            },
            span,
        )
    }

    /// Type checks CIRCLE statement.
    ///
    /// CIRCLE draws a circle or ellipse.
    ///
    /// # Arguments
    /// * `step` - Whether center is relative
    /// * `x`, `y` - Center coordinates
    /// * `radius` - Circle radius
    /// * `color` - Circle color
    /// * `filled` - Whether the circle is filled (QB64 extension)
    #[allow(clippy::too_many_arguments)]
    pub(in crate::semantic::checker) fn check_circle(
        &mut self,
        step: bool,
        x: &Expr,
        y: &Expr,
        radius: &Expr,
        color: Option<&Expr>,
        filled: bool,
        span: Span,
    ) -> TypedStatement {
        let typed_x = self.check_expr(x);
        let typed_y = self.check_expr(y);
        let typed_radius = self.check_expr(radius);
        let typed_color = color.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Circle {
                step,
                x: typed_x,
                y: typed_y,
                radius: typed_radius,
                color: typed_color,
                filled,
            },
            span,
        )
    }

    /// Type checks PAINT statement.
    ///
    /// PAINT flood-fills an area with color.
    ///
    /// # Arguments
    /// * `step` - Whether coordinates are relative
    /// * `x`, `y` - Starting point
    /// * `color` - Fill color
    /// * `border` - Border color to stop at
    pub(in crate::semantic::checker) fn check_paint(
        &mut self,
        step: bool,
        x: &Expr,
        y: &Expr,
        color: Option<&Expr>,
        border: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_x = self.check_expr(x);
        let typed_y = self.check_expr(y);
        let typed_color = color.map(|e| self.check_expr(e));
        let typed_border = border.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Paint {
                step,
                x: typed_x,
                y: typed_y,
                color: typed_color,
                border: typed_border,
            },
            span,
        )
    }

    // ==================== Display Control ====================

    /// Type checks _DISPLAY statement (simple, no arguments).
    ///
    /// _DISPLAY updates the screen from the drawing buffer.
    pub(in crate::semantic::checker) fn check_gfx_display(&mut self, span: Span) -> TypedStatement {
        TypedStatement::new(TypedStatementKind::GfxDisplay, span)
    }

    /// Type checks _CONTROLCHR statement.
    ///
    /// _CONTROLCHR enables or disables control character handling.
    ///
    /// # Arguments
    /// * `enabled` - Whether control characters are enabled
    pub(in crate::semantic::checker) fn check_control_chr(
        &mut self,
        enabled: bool,
        span: Span,
    ) -> TypedStatement {
        TypedStatement::new(TypedStatementKind::ControlChr { enabled }, span)
    }

    /// Type checks _MAPUNICODE statement.
    ///
    /// _MAPUNICODE maps a Unicode code point to a character position.
    ///
    /// # Arguments
    /// * `unicode_value` - Unicode code point
    /// * `char_position` - Character position in font
    pub(in crate::semantic::checker) fn check_map_unicode(
        &mut self,
        unicode_value: &Expr,
        char_position: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_unicode = self.check_expr(unicode_value);
        let typed_char = self.check_expr(char_position);

        TypedStatement::new(
            TypedStatementKind::MapUnicode {
                unicode_value: typed_unicode,
                char_position: typed_char,
            },
            span,
        )
    }

    // ==================== Screen Management ====================

    /// Type checks PALETTE statement.
    ///
    /// PALETTE changes color attribute mappings.
    ///
    /// # Arguments
    /// * `attribute` - Color attribute to modify
    /// * `color` - New color value
    pub(in crate::semantic::checker) fn check_palette(
        &mut self,
        attribute: Option<&Expr>,
        color: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_attr = attribute.map(|e| self.check_expr(e));
        let typed_color = color.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Palette {
                attribute: typed_attr,
                color: typed_color,
            },
            span,
        )
    }

    /// Type checks $RESIZE statement (simple, bool argument).
    ///
    /// $RESIZE enables or disables window resizing.
    ///
    /// # Arguments
    /// * `enabled` - Whether resizing is enabled
    pub(in crate::semantic::checker) fn check_gfx_resize(
        &mut self,
        enabled: bool,
        span: Span,
    ) -> TypedStatement {
        TypedStatement::new(TypedStatementKind::GfxResize { enabled }, span)
    }

    /// Type checks PCOPY statement.
    ///
    /// PCOPY copies one screen page to another.
    ///
    /// # Arguments
    /// * `source` - Source page number
    /// * `dest` - Destination page number
    pub(in crate::semantic::checker) fn check_pcopy(
        &mut self,
        source: &Expr,
        dest: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_source = self.check_expr(source);
        let typed_dest = self.check_expr(dest);

        TypedStatement::new(
            TypedStatementKind::Pcopy {
                source: typed_source,
                dest: typed_dest,
            },
            span,
        )
    }

    /// Type checks WIDTH statement.
    ///
    /// WIDTH sets the screen width in columns.
    ///
    /// # Arguments
    /// * `columns` - Number of text columns
    /// * `rows` - Number of text rows (optional)
    pub(in crate::semantic::checker) fn check_width(
        &mut self,
        columns: &Expr,
        rows: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_columns = self.check_expr(columns);
        let typed_rows = rows.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::Width {
                columns: typed_columns,
                rows: typed_rows,
            },
            span,
        )
    }

    /// Type checks VIEW statement.
    ///
    /// VIEW defines a graphics viewport.
    ///
    /// # Arguments
    /// * `screen` - Whether SCREEN keyword was used
    /// * `coords` - Viewport coordinates
    /// * `fill_color` - Fill color for viewport
    /// * `border_color` - Border color for viewport
    pub(in crate::semantic::checker) fn check_view(
        &mut self,
        screen: bool,
        coords: Option<&ViewCoords>,
        fill_color: Option<&Expr>,
        border_color: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_coords = coords.map(|c| self.check_view_coords(c));
        let typed_fill = fill_color.map(|e| self.check_expr(e));
        let typed_border = border_color.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::View {
                screen,
                coords: typed_coords,
                fill_color: typed_fill,
                border_color: typed_border,
            },
            span,
        )
    }

    /// Type checks VIEW PRINT statement.
    ///
    /// VIEW PRINT defines a text viewport.
    ///
    /// # Arguments
    /// * `top` - Top row
    /// * `bottom` - Bottom row
    pub(in crate::semantic::checker) fn check_view_print(
        &mut self,
        top: Option<&Expr>,
        bottom: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_top = top.map(|e| self.check_expr(e));
        let typed_bottom = bottom.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::ViewPrint {
                top: typed_top,
                bottom: typed_bottom,
            },
            span,
        )
    }

    /// Type checks WINDOW statement.
    ///
    /// WINDOW defines a logical coordinate system.
    ///
    /// # Arguments
    /// * `screen` - Whether SCREEN keyword was used
    /// * `coords` - Window coordinates
    pub(in crate::semantic::checker) fn check_window(
        &mut self,
        screen: bool,
        coords: Option<&ViewCoords>,
        span: Span,
    ) -> TypedStatement {
        let typed_coords = coords.map(|c| self.check_view_coords(c));

        TypedStatement::new(
            TypedStatementKind::WindowCoords {
                screen,
                coords: typed_coords,
            },
            span,
        )
    }

    // ==================== Bitmap Operations ====================

    /// Type checks DRAW statement.
    ///
    /// DRAW executes turtle graphics commands.
    ///
    /// # Arguments
    /// * `commands` - String of drawing commands
    pub(in crate::semantic::checker) fn check_draw_cmd(
        &mut self,
        commands: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_commands = self.check_expr(commands);

        TypedStatement::new(
            TypedStatementKind::DrawCmd {
                commands: typed_commands,
            },
            span,
        )
    }

    /// Type checks GET (graphics) statement.
    ///
    /// GET captures a screen region into an array.
    ///
    /// # Arguments
    /// * `step1` - Whether first point is relative
    /// * `x1`, `y1` - First corner
    /// * `step2` - Whether second point is relative
    /// * `x2`, `y2` - Second corner
    /// * `array_name` - Target array name
    /// * `array_indices` - Array indices (for multi-dimensional arrays)
    #[allow(clippy::too_many_arguments)]
    pub(in crate::semantic::checker) fn check_gfx_get(
        &mut self,
        step1: bool,
        x1: &Expr,
        y1: &Expr,
        step2: bool,
        x2: &Expr,
        y2: &Expr,
        array_name: &str,
        array_indices: &[Expr],
        span: Span,
    ) -> TypedStatement {
        let typed_x1 = self.check_expr(x1);
        let typed_y1 = self.check_expr(y1);
        let typed_x2 = self.check_expr(x2);
        let typed_y2 = self.check_expr(y2);
        let typed_indices: Vec<_> = array_indices.iter().map(|e| self.check_expr(e)).collect();

        TypedStatement::new(
            TypedStatementKind::GraphicsGet {
                step1,
                x1: typed_x1,
                y1: typed_y1,
                step2,
                x2: typed_x2,
                y2: typed_y2,
                array_name: array_name.to_string(),
                array_indices: typed_indices,
            },
            span,
        )
    }

    /// Type checks PUT (graphics) statement.
    ///
    /// PUT draws an array-stored image to the screen.
    ///
    /// # Arguments
    /// * `x`, `y` - Destination position
    /// * `step` - Whether position is relative
    /// * `array_name` - Source array name
    /// * `array_indices` - Array indices
    /// * `clip` - Whether to clip at screen boundaries (QB64)
    /// * `action` - Pixel action (XOR, PSET, AND, OR, PRESET)
    /// * `transparent_color` - Transparent color (QB64)
    #[allow(clippy::too_many_arguments)]
    pub(in crate::semantic::checker) fn check_gfx_put(
        &mut self,
        x: &Expr,
        y: &Expr,
        step: bool,
        array_name: &str,
        array_indices: &[Expr],
        clip: bool,
        action: PutAction,
        transparent_color: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_x = self.check_expr(x);
        let typed_y = self.check_expr(y);
        let typed_indices: Vec<_> = array_indices.iter().map(|e| self.check_expr(e)).collect();
        let typed_transparent = transparent_color.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::GraphicsPut {
                x: typed_x,
                y: typed_y,
                step,
                array_name: array_name.to_string(),
                array_indices: typed_indices,
                clip,
                action,
                transparent_color: typed_transparent,
            },
            span,
        )
    }

    // ==================== QB64 Graphics Extensions ====================

    /// Type checks _FREEIMAGE statement.
    ///
    /// _FREEIMAGE releases an image handle.
    ///
    /// # Arguments
    /// * `handle` - Image handle to free
    pub(in crate::semantic::checker) fn check_free_image(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::FreeImage {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _PUTIMAGE statement.
    ///
    /// _PUTIMAGE copies an image region with optional scaling.
    ///
    /// # Arguments
    /// * `dest_coords` - Destination coordinates
    /// * `source` - Source image handle
    /// * `dest` - Destination image handle
    /// * `source_coords` - Source region coordinates
    /// * `scale_mode` - Scaling mode (_SMOOTH or _STRETCH)
    pub(in crate::semantic::checker) fn check_put_image(
        &mut self,
        dest_coords: Option<&ViewCoords>,
        source: Option<&Expr>,
        dest: Option<&Expr>,
        source_coords: Option<&ViewCoords>,
        scale_mode: ImageScaleMode,
        span: Span,
    ) -> TypedStatement {
        let typed_dest_coords = dest_coords.map(|c| Box::new(self.check_view_coords(c)));
        let typed_source = source.map(|e| self.check_expr(e));
        let typed_dest = dest.map(|e| self.check_expr(e));
        let typed_source_coords = source_coords.map(|c| Box::new(self.check_view_coords(c)));

        TypedStatement::new(
            TypedStatementKind::PutImage {
                dest_coords: typed_dest_coords,
                source: typed_source,
                dest: typed_dest,
                source_coords: typed_source_coords,
                scale_mode,
            },
            span,
        )
    }

    /// Type checks _SOURCE statement.
    ///
    /// _SOURCE sets the source image for subsequent operations.
    ///
    /// # Arguments
    /// * `handle` - Image handle
    pub(in crate::semantic::checker) fn check_source_img(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::SourceImg {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _DEST statement.
    ///
    /// _DEST sets the destination image for subsequent operations.
    ///
    /// # Arguments
    /// * `handle` - Image handle
    pub(in crate::semantic::checker) fn check_dest_img(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::DestImg {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _PRINTSTRING statement.
    ///
    /// _PRINTSTRING draws text at a pixel position.
    ///
    /// # Arguments
    /// * `x` - X pixel coordinate
    /// * `y` - Y pixel coordinate
    /// * `text` - Text to print
    pub(in crate::semantic::checker) fn check_print_string_stmt(
        &mut self,
        x: &Expr,
        y: &Expr,
        text: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_x = self.check_expr(x);
        let typed_y = self.check_expr(y);
        let typed_text = self.check_expr(text);

        TypedStatement::new(
            TypedStatementKind::PrintStringStmt {
                x: typed_x,
                y: typed_y,
                text: typed_text,
            },
            span,
        )
    }

    /// Type checks _AUTODISPLAY statement.
    ///
    /// _AUTODISPLAY enables or disables automatic screen updates.
    ///
    /// # Arguments
    /// * `enabled` - Whether auto-display is enabled
    pub(in crate::semantic::checker) fn check_auto_display(
        &mut self,
        enabled: bool,
        span: Span,
    ) -> TypedStatement {
        TypedStatement::new(TypedStatementKind::AutoDisplay { enabled }, span)
    }
}
