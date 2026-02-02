//! Shared helpers for expression code generation.
//!
//! This module provides string escaping, C name mapping, and fixed-string
//! conversion utilities used across the expression emitter.

use crate::semantic::typed_ir::{TypedExpr, TypedExprKind};
use crate::semantic::types::BasicType;

use super::super::types::c_identifier;

/// Maps a BASIC OpenGL/_GL* name to its C wrapper (call_gl* or call_glu*).
///
/// QB64pe uses wrappers that check sub_gl_called before calling gl*.
fn opengl_c_name(name: &str) -> String {
    let s = name.trim_start_matches('_');
    let (prefix, rest) = if s.to_uppercase().starts_with("GLU") {
        ("call_glu", &s[3..])
    } else if s.to_uppercase().starts_with("GL") {
        ("call_gl", &s[2..])
    } else {
        return format!(
            "qb_{}",
            name.to_lowercase().replace(['&', '%', '$', '!', '#'], "")
        );
    };
    let rest_lower = rest.to_lowercase();
    let first = rest_lower
        .chars()
        .next()
        .map(|c| c.to_uppercase().next().unwrap_or(c))
        .unwrap_or_default();
    let tail: String = rest_lower.chars().skip(1).collect();
    format!("{}{}{}", prefix, first, tail)
}

/// Maps a BASIC function name to its C equivalent.
///
/// Built-in BASIC functions are mapped to corresponding runtime functions
/// or standard C math functions. User-defined functions are prefixed with `qb_`.
pub(crate) fn c_function_name(name: &str) -> String {
    let upper = name.to_uppercase();
    // OpenGL _GL* / _GLU* (except _GLRENDER and _GLCOMPAT) map to call_gl* / call_glu*
    if (upper.starts_with("_GL") || upper.starts_with("_GLU"))
        && upper != "_GLRENDER"
        && upper != "_GLCOMPAT"
    {
        return opengl_c_name(name);
    }
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
        "_ROUND" => "qb_round_double".to_string(),
        "_MIN" => "fmin".to_string(),
        "_MAX" => "fmax".to_string(),
        "_CLAMP" => "qb_clamp".to_string(),

        // Hyperbolic functions
        "_SINH" => "sinh".to_string(),
        "_COSH" => "cosh".to_string(),
        "_TANH" => "tanh".to_string(),
        "_ASINH" => "asinh".to_string(),
        "_ACOSH" => "acosh".to_string(),
        "_ATANH" => "atanh".to_string(),

        // Reciprocal trig functions
        "_SEC" => "qb_sec".to_string(),
        "_CSC" => "qb_csc".to_string(),
        "_COT" => "qb_cot".to_string(),

        // Hyperbolic reciprocals
        "_SECH" => "qb_sech".to_string(),
        "_CSCH" => "qb_csch".to_string(),
        "_COTH" => "qb_coth".to_string(),

        // Inverse reciprocal trig
        "_ARCSEC" => "qb_arcsec".to_string(),
        "_ARCCSC" => "qb_arccsc".to_string(),
        "_ARCCOT" => "qb_arccot".to_string(),

        // Inverse hyperbolic reciprocals
        "_ARCSECH" => "qb_arcsech".to_string(),
        "_ARCCSCH" => "qb_arccsch".to_string(),
        "_ARCCOTH" => "qb_arccoth".to_string(),

        // Angle conversions (degrees <-> radians)
        "_D2R" => "qb_d2r".to_string(),
        "_R2D" => "qb_r2d".to_string(),

        // Gradian conversions
        "_D2G" => "qb_d2g".to_string(),
        "_G2D" => "qb_g2d".to_string(),
        "_G2R" => "qb_g2r".to_string(),
        "_R2G" => "qb_r2g".to_string(),

        // Negate
        "_NEGATE" => "qb_negate".to_string(),

        // String comparison
        "_STRCMP" => "qb_strcmp".to_string(),
        "_STRICMP" => "qb_stricmp".to_string(),

        // Bitwise operations
        "_SHL" => "qb_shl".to_string(),
        "_SHR" => "qb_shr".to_string(),
        "_ROL" => "qb_rol".to_string(),
        "_ROR" => "qb_ror".to_string(),
        "_READBIT" => "qb_readbit".to_string(),
        "_SETBIT" => "qb_setbit".to_string(),
        "_RESETBIT" => "qb_resetbit".to_string(),
        "_TOGGLEBIT" => "qb_togglebit".to_string(),

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

        // File I/O functions
        "EOF" => "qb_eof".to_string(),
        "LOF" => "qb_lof".to_string(),
        "LOC" => "qb_loc".to_string(),
        "FREEFILE" => "qb_freefile".to_string(),

        // Keyboard input functions
        "INKEY$" => "qb_inkey".to_string(),
        "INPUT$" => "qb_input_chars".to_string(),

        // Error handling functions
        "ERR" => "qb_err_code".to_string(),
        "ERL" => "qb_err_line".to_string(),
        "_ERRORLINE" => "qb_errorline".to_string(),
        "_ERRORMESSAGE$" => "qb_errormessage".to_string(),

        // Version information
        "VERSION$" => "qb_version".to_string(),

        // Utility functions
        "_COMMANDCOUNT" => "qb_commandcount".to_string(),
        "_ENVIRONCOUNT" => "qb_environcount".to_string(),

        // Environment functions
        "ENVIRON$" => "qb_environ".to_string(),
        "COMMAND$" => "qb_command".to_string(),
        "_CWD$" => "qb_cwd".to_string(),
        "_OS$" => "qb_os".to_string(),
        "_STARTDIR$" => "qb_startdir".to_string(),

        // Classic BASIC date/time functions
        "TIMER" => "qb_timer".to_string(),
        "DATE$" => "qb_date".to_string(),
        "TIME$" => "qb_time".to_string(),
        "TRIM$" => "qb_trim".to_string(),

        // Print formatting functions
        "TAB" => "qb_tab".to_string(),
        "SPC" => "qb_spc".to_string(),
        "POS" => "qb_pos".to_string(),
        "CSRLIN" => "qb_csrlin".to_string(),

        // QB64 keyboard extensions
        "_KEYHIT" => "qb_keyhit".to_string(),
        "_KEYDOWN" => "qb_keydown".to_string(),
        "_CINP" => "qb_cinp".to_string(),

        // Lock key state functions
        "_CAPSLOCK" => "qb_capslock".to_string(),
        "_NUMLOCK" => "qb_numlock".to_string(),
        "_SCROLLLOCK" => "qb_scrolllock".to_string(),

        // Phase 2: String enhancements
        "_INSTRREV" => "qb_instrrev".to_string(),
        "_TRIM$" => "qb_trim".to_string(),
        "MKI$" => "qb_mki".to_string(),
        "MKL$" => "qb_mkl".to_string(),
        "MKS$" => "qb_mks".to_string(),
        "MKD$" => "qb_mkd".to_string(),
        "CVI" => "qb_cvi".to_string(),
        "CVL" => "qb_cvl".to_string(),
        "CVS" => "qb_cvs".to_string(),
        "CVD" => "qb_cvd".to_string(),

        // Phase 2: QB64 Date/Time
        "_DATE$" => "qb_date64".to_string(),
        "_TIME$" => "qb_time64".to_string(),

        // Phase 2: Memory operations
        "_MEMNEW" => "qb_memnew".to_string(),
        "_MEMFREE" => "qb_memfree".to_string(),
        "_MEMGET" => "qb_memget".to_string(),
        "_MEMPUT" => "qb_memput".to_string(),
        "_MEMCOPY" => "qb_memcopy".to_string(),
        "_MEMFILL" => "qb_memfill".to_string(),
        "_OFFSET" => "qb_offset".to_string(),
        "_MEM" => "qb_mem".to_string(),

        // Phase 5: System Integration
        "_FILEEXISTS" => "qb_file_exists".to_string(),
        "_DIREXISTS" => "qb_dir_exists".to_string(),
        "_DIR$" => "qb_dir".to_string(),
        "_READFILE$" => "qb_readfile".to_string(),

        // Phase 5: Mouse Input
        "_MOUSEX" => "qb_mouse_x".to_string(),
        "_MOUSEY" => "qb_mouse_y".to_string(),
        "_MOUSEBUTTON" => "qb_mouse_button".to_string(),
        "_MOUSEINPUT" => "qb_mouse_input".to_string(),
        "_MOUSEMOVEMENTX" => "qb_mouse_movement_x".to_string(),
        "_MOUSEMOVEMENTY" => "qb_mouse_movement_y".to_string(),
        "_MOUSEWHEEL" => "qb_mouse_wheel".to_string(),

        // Phase 5: Clipboard
        "_CLIPBOARD$" => "qb_clipboard_get".to_string(),

        // Font support
        "_LOADFONT" => "qb_loadfont".to_string(),
        "_FONTHEIGHT" => "qb_fontheight".to_string(),
        "_FONTWIDTH" => "qb_fontwidth".to_string(),
        "_PRINTWIDTH" => "qb_printwidth".to_string(),
        "_FONT" => "qb_font".to_string(),
        "_FREEFONT" => "qb_freefont".to_string(),

        // Desktop/Window functions
        "_DESKTOPWIDTH" => "qb_desktopwidth".to_string(),
        "_DESKTOPHEIGHT" => "qb_desktopheight".to_string(),
        "_SCREENX" => "qb_screenx".to_string(),
        "_SCREENY" => "qb_screeny".to_string(),
        "_TITLE$" => "qb_title_get".to_string(),
        "_WINDOWHANDLE" => "qb_windowhandle".to_string(),
        "_WINDOWHASFOCUS" => "qb_windowhasfocus".to_string(),

        // Window control functions
        "_SCREENMOVE" => "qb_screenmove".to_string(),
        "_SCREENHIDE" => "qb_screenhide".to_string(),
        "_SCREENSHOW" => "qb_screenshow".to_string(),
        "_FULLSCREEN" => "qb_fullscreen".to_string(),
        "_SCREENCLICK" => "qb_screenclick".to_string(),
        "_SCREENPRINT" => "qb_screenprint".to_string(),
        "_SCREENIMAGE" => "qb_screenimage".to_string(),

        // Dialog boxes
        "_MESSAGEBOX" => "qb_messagebox".to_string(),
        "_INPUTBOX$" => "qb_inputbox".to_string(),
        "_OPENFILEDIALOG$" => "qb_openfiledialog".to_string(),
        "_SAVEFILEDIALOG$" => "qb_savefiledialog".to_string(),
        "_SELECTFOLDERDIALOG$" => "qb_selectfolderdialog".to_string(),

        // Binary/number-to-string conversion
        "HEX$" => "qb_hex".to_string(),
        "OCT$" => "qb_oct".to_string(),
        "_BIN$" => "qb_bin".to_string(),
        "_TOSTR$" => "qb_tostr".to_string(),

        // Inline conditional
        "_IIF" => "qb_iif".to_string(),
        "_IIF$" => "qb_iif_str".to_string(),

        // Phase 5: Networking
        "_OPENHOST" => "qb_net_openhost".to_string(),
        "_OPENCONNECTION" => "qb_net_openconnection".to_string(),
        "_OPENCLIENT" => "qb_net_openclient".to_string(),
        "_CONNECTED" => "qb_net_connected".to_string(),

        // Image buffer functions
        "_NEWIMAGE" => "qb_gfx_newimage".to_string(),
        "_LOADIMAGE" => "qb_gfx_loadimage".to_string(),
        "_COPYIMAGE" => "qb_gfx_copyimage".to_string(),
        "_WIDTH" => "qb_gfx_image_width".to_string(),
        "_HEIGHT" => "qb_gfx_image_height".to_string(),

        // Coordinate mapping and pixel query
        "PMAP" => "qb_gfx_pmap".to_string(),
        "POINT" => "qb_gfx_point".to_string(),

        // Event handling functions (QB4.5)
        "KEY" => "qb_key_status".to_string(),

        // Joystick functions (QB4.5)
        "STICK" => "qb_stick".to_string(),
        "STRIG" => "qb_strig".to_string(),

        // Memory functions (QB4.5)
        "FRE" => "qb_fre".to_string(),
        "PEEK" => "qb_peek".to_string(),

        // Port I/O functions (QB4.5)
        "INP" => "qb_inp".to_string(),

        // Light pen function (QB4.5 legacy)
        "PEN" => "qb_pen".to_string(),

        // Serial I/O functions (QB4.5)
        "ERDEV" => "qb_erdev".to_string(),
        "ERDEV$" => "qb_erdev_str".to_string(),
        "IOCTL$" => "qb_ioctl_str".to_string(),

        // Legacy BASIC functions
        "LPOS" => "qb_lpos".to_string(),
        "VARPTR" => "qb_varptr".to_string(),
        "VARPTR$" => "qb_varptr_str".to_string(),
        "VARSEG" => "qb_varseg".to_string(),
        "SADD" => "qb_sadd".to_string(),
        "FILEATTR" => "qb_fileattr".to_string(),

        // Microsoft Binary Format conversions
        "CVSMBF" => "qb_cvsmbf".to_string(),
        "CVDMBF" => "qb_cvdmbf".to_string(),
        "MKSMBF$" => "qb_mksmbf".to_string(),
        "MKDMBF$" => "qb_mkdmbf".to_string(),

        // QB64 Extension Functions (Session 031+)
        "_RED" => "qb_red".to_string(),
        "_GREEN" => "qb_green".to_string(),
        "_BLUE" => "qb_blue".to_string(),
        "_ALPHA" => "qb_alpha".to_string(),
        "_RED32" => "qb_red32".to_string(),
        "_GREEN32" => "qb_green32".to_string(),
        "_BLUE32" => "qb_blue32".to_string(),
        "_ALPHA32" => "qb_alpha32".to_string(),

        "_PIXELSIZE" => "qb_pixelsize".to_string(),
        "_SCREENEXISTS" => "qb_screenexists".to_string(),
        "_FPS" => "qb_fps".to_string(),

        "_FULLPATH$" => "qb_fullpath".to_string(),

        "_CRC32" => "qb_crc32".to_string(),
        "_MD5$" => "qb_md5".to_string(),
        "_ADLER32" => "qb_adler32".to_string(),
        "_BASE64ENCODE$" => "qb_base64encode".to_string(),
        "_BASE64DECODE$" => "qb_base64decode".to_string(),
        "_ENCODEURL$" => "qb_encodeurl".to_string(),
        "_DECODEURL$" => "qb_decodeurl".to_string(),
        "_DEFLATE$" => "qb_deflate".to_string(),
        "_INFLATE$" => "qb_inflate".to_string(),

        "_MEMEXISTS" => "qb_memexists".to_string(),

        "_DEFAULTCOLOR" => "qb_defaultcolor".to_string(),
        "_BACKGROUNDCOLOR" => "qb_backgroundcolor".to_string(),

        "_EXIT" => "qb_exit_state".to_string(),

        "_ANDALSO" => "qb_andalso".to_string(),
        "_ORELSE" => "qb_orelse".to_string(),

        "_FREETIMER" => "qb_freetimer".to_string(),

        "_CONSOLEINPUT" => "qb_consoleinput".to_string(),
        "_ECHO" => "qb_echo".to_string(),

        "_MOUSEHIDDEN" => "qb_mousehidden".to_string(),

        "_CLIPBOARDIMAGE" => "qb_clipboardimage".to_string(),

        "_DEVICES" => "qb_devices".to_string(),
        "_DEVICE$" => "qb_device_name".to_string(),
        "_DEVICEINPUT" => "qb_deviceinput".to_string(),
        "_LASTAXIS" => "qb_lastaxis".to_string(),
        "_LASTBUTTON" => "qb_lastbutton".to_string(),
        "_LASTWHEEL" => "qb_lastwheel".to_string(),
        "_AXIS" => "qb_axis".to_string(),
        "_BUTTON" => "qb_button".to_string(),
        "_BUTTONCHANGE" => "qb_buttonchange".to_string(),
        "_WHEEL" => "qb_wheel".to_string(),

        "_TOTALDROPPEDFILES" => "qb_totaldroppedfiles".to_string(),
        "_DROPPEDFILE" => "qb_droppedfile".to_string(),
        "_DROPPEDFILE$" => "qb_droppedfile_str".to_string(),

        "_RESIZE" => "qb_resize".to_string(),
        "_RESIZEWIDTH" => "qb_resizewidth".to_string(),
        "_RESIZEHEIGHT" => "qb_resizeheight".to_string(),
        "_SCALEDWIDTH" => "qb_scaledwidth".to_string(),
        "_SCALEDHEIGHT" => "qb_scaledheight".to_string(),

        "_COLORCHOOSERDIALOG" => "qb_colorchooserdialog".to_string(),
        "_NOTIFYPOPUP" => "qb_notifypopup".to_string(),

        "_SNDRAWDONE" => "qb_sndrawdone".to_string(),
        "_SNDOPENRAW" => "qb_sndopenraw".to_string(),
        "_SNDRAWLEN" => "qb_sndrawlen".to_string(),

        "_INCLERRORFILE$" => "qb_inclerrorfile".to_string(),
        "_INCLERRORLINE" => "qb_inclerrorline".to_string(),

        "_STATUSCODE" => "qb_statuscode".to_string(),

        "_CONNECTIONADDRESS" => "qb_connectionaddress".to_string(),
        "_CONNECTIONADDRESS$" => "qb_connectionaddress_str".to_string(),

        "_HSB32" => "qb_hsb32".to_string(),
        "_HSBA32" => "qb_hsba32".to_string(),
        "_HUE32" => "qb_hue32".to_string(),
        "_SATURATION32" => "qb_saturation32".to_string(),
        "_BRIGHTNESS32" => "qb_brightness32".to_string(),

        "_MEMELEMENT" => "qb_memelement".to_string(),
        "_MEMIMAGE" => "qb_memimage".to_string(),
        "_MEMSOUND" => "qb_memsound".to_string(),

        "_SNDNEW" => "qb_sndnew".to_string(),

        "_FILES$" => "qb_files_str".to_string(),

        "_LASTHANDLER" => "qb_lasthandler".to_string(),

        "_UCHARPOS" => "qb_ucharpos".to_string(),
        "_UFONTHEIGHT" => "qb_ufontheight".to_string(),
        "_ULINESPACING" => "qb_ulinespacing".to_string(),
        "_UPRINTWIDTH" => "qb_uprintwidth".to_string(),

        "_EMBEDDED$" => "qb_embedded".to_string(),

        "_SMOOTH" => "qb_smooth".to_string(),
        "_SMOOTHSHRUNK" => "qb_smoothshrunk".to_string(),
        "_SMOOTHSTRETCHED" => "qb_smoothstretched".to_string(),
        "_HARDWARE" => "qb_hardware".to_string(),
        "_HARDWARE1" => "qb_hardware1".to_string(),
        "_SOFTWARE" => "qb_software".to_string(),

        "_ANTICLOCKWISE" => "qb_anticlockwise".to_string(),
        "_CLOCKWISE" => "qb_clockwise".to_string(),

        "_KEEPBACKGROUND" => "qb_keepbackground".to_string(),
        "_FILLBACKGROUND" => "qb_fillbackground".to_string(),
        "_ONLYBACKGROUND" => "qb_onlybackground".to_string(),

        "_MIDDLE" => "qb_middle".to_string(),

        "_AUTO" => "qb_auto".to_string(),

        "_CLIP" => "qb_clip".to_string(),
        "_STRETCH" => "qb_stretch".to_string(),
        "_SEAMLESS" => "qb_seamless".to_string(),
        "_SQUAREPIXELS" => "qb_squarepixels".to_string(),
        "_BEHIND" => "qb_behind".to_string(),

        "_ALL" => "qb_all".to_string(),
        "_BLINK" => "qb_blink".to_string(),
        "_OFF" => "qb_off".to_string(),
        "_ONLY" => "qb_only".to_string(),

        "_WAVE" => "qb_wave".to_string(),
        "_DONTWAIT" => "qb_dontwait".to_string(),

        "_CONSOLETITLE$" => "qb_consoletitle_str".to_string(),
        "_CONSOLE" => "qb_console".to_string(),

        "_SHELLHIDE" => "qb_shellhide".to_string(),

        "_GLCOMPAT" => "qb_glcompat".to_string(),

        "_ASSERT" => "qb_assert".to_string(),
        "_ASSERTERROR$" => "qb_asserterror".to_string(),

        "_FULLSCREENSMOOTH" => "qb_fullscreensmooth".to_string(),
        "_ALLOWFULLSCREEN" => "qb_allowfullscreen".to_string(),
        "_DISPLAYWIDTH" => "qb_displaywidth".to_string(),
        "_DISPLAYHEIGHT" => "qb_displayheight".to_string(),

        "_SCREENBUFFER" => "qb_screenbuffer".to_string(),
        "_SCINKEY$" => "qb_scinkey".to_string(),

        "_YEAR" => "qb_year".to_string(),
        "_MONTH" => "qb_month".to_string(),
        "_DAY" => "qb_day".to_string(),
        "_WEEKDAY" => "qb_weekday".to_string(),
        "_HOUR" => "qb_hour".to_string(),
        "_MINUTE" => "qb_minute".to_string(),
        "_SECOND" => "qb_second".to_string(),

        "_SCREENICON" => "qb_screenicon".to_string(),

        // Default: prefix with qb_ for user functions
        _ => format!("qb_{}", c_identifier(name).to_lowercase()),
    }
}

/// Escapes a string for C string literal.
///
/// This function ensures the string is safe to embed in generated C code by
/// escaping special characters. Non-ASCII characters are escaped as `\xNN`
/// sequences for maximum C compiler portability.
pub(crate) fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() * 2);
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                for byte in c.to_string().as_bytes() {
                    result.push_str(&format!("\\x{:02x}", byte));
                }
            }
            c if c.is_ascii() => result.push(c),
            c => {
                for byte in c.to_string().as_bytes() {
                    result.push_str(&format!("\\x{:02x}", byte));
                }
            }
        }
    }
    result
}

/// Extracts the inner expression from a `qb_str_from_c()` wrapper.
///
/// If the code starts with `qb_str_from_c(`, finds the matching closing
/// parenthesis and returns the inner expression. Otherwise, returns the code unchanged.
pub(crate) fn unwrap_qb_str_from_c(code: &str) -> String {
    if let Some(stripped) = code.strip_prefix("qb_str_from_c(") {
        let mut depth = 0;
        let mut end_pos = stripped.len(); // default: use all if no matching ')'
        for (i, ch) in stripped.char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    if depth == 0 {
                        end_pos = i;
                        break;
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }
        stripped[..end_pos].to_string()
    } else {
        code.to_string()
    }
}

/// Checks if an expression represents a fixed-length string field that needs
/// conversion to qb_string* for use with built-in string functions.
pub(crate) fn needs_fixed_string_conversion(expr: &TypedExpr) -> bool {
    match &expr.kind {
        TypedExprKind::FieldAccess { .. } => {
            matches!(expr.basic_type, BasicType::FixedString(_))
        }
        TypedExprKind::ArrayAccess { .. } => {
            matches!(expr.basic_type, BasicType::FixedString(_))
        }
        TypedExprKind::Convert { expr: inner, .. } => {
            matches!(inner.basic_type, BasicType::FixedString(_))
                && matches!(
                    inner.kind,
                    TypedExprKind::FieldAccess { .. } | TypedExprKind::ArrayAccess { .. }
                )
        }
        _ => false,
    }
}

/// Safely extracts `const char*` from a string expression for use with C functions
/// that expect `const char*` (like file operations, system calls, etc.).
pub(crate) fn emit_string_data_access(
    expr: &TypedExpr,
    expr_code: &str,
    runtime_mode: &super::super::RuntimeMode,
) -> String {
    let needs_conversion = matches!(expr.basic_type, BasicType::FixedString(_))
        && !expr_code.starts_with("qb_str_from_c(")
        && !expr_code.starts_with("qb_string_");

    let qb_string_expr = if needs_conversion {
        format!("qb_str_from_c({})", expr_code)
    } else {
        expr_code.to_string()
    };

    runtime_mode.string_data_access(&qb_string_expr)
}
