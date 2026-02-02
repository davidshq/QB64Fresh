//! Built-in function and constant registration.
//!
//! This module registers all QB64 built-in functions, subs, and constants
//! into the symbol table during semantic analyzer initialization.
//!
//! # Organization
//!
//! Registration is split by category into submodules:
//!
//! - [`string`] - String and conversion (LEN, CHR$, ASC, LEFT$, MID$, etc.)
//! - [`math`] - Math (ABS, SIN, COS, _PI, _CEIL, etc.)
//! - [`misc`] - Bitwise, array, memory, system, mouse, clipboard
//! - [`io`] - I/O, console, timer, environment (TAB, EOF, INKEY$, ENVIRON$, etc.)
//! - [`graphics`] - Graphics, window, image, colors, dialogs, networking
//! - [`audio`] - Audio (_SNDOPEN, _SNDLEN, etc.)
//! - [`extended`] - Extended QB64 (hash, base64, device input, etc.) and SUBs
//! - [`subs`] - Built-in SUBs (statements)
//!
//! Helpers (`register_builtin_function`, `register_builtin_constant`, etc.)
//! and constant registration (_TRUE, _FALSE, character constants, etc.) live
//! in this file.

mod audio;
mod extended;
mod graphics;
mod io;
mod math;
mod misc;
mod string;
mod subs;

use super::symbols::{
    ConstValue, ParameterInfo, ProcedureEntry, ProcedureKind, Symbol, SymbolKind,
};
use super::types::BasicType;
use super::SemanticAnalyzer;

impl SemanticAnalyzer {
    /// Registers all built-in functions, subs, and constants.
    pub(super) fn register_builtins(&mut self) {
        self.register_builtin_constants();

        string::register_string_builtins(self);
        math::register_math_builtins(self);
        misc::register_misc_builtins(self);
        io::register_io_builtins(self);
        graphics::register_graphics_builtins(self);
        audio::register_audio_builtins(self);
        extended::register_extended_builtins(self);

        self.register_builtins_opengl();
    }

    /// Registers a single built-in function.
    pub(crate) fn register_builtin_function(
        &mut self,
        name: &str,
        params: &[(&str, BasicType)],
        return_type: BasicType,
    ) {
        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::BuiltIn,
            params: params
                .iter()
                .map(|(n, t)| ParameterInfo {
                    name: (*n).to_string(),
                    basic_type: t.clone(),
                    by_val: true,
                    is_optional: false,
                    is_array: false,
                })
                .collect(),
            return_type: Some(return_type),
            span: crate::ast::Span::new(0, 0, 1),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in function with optional parameters.
    pub(crate) fn register_builtin_function_with_optionals(
        &mut self,
        name: &str,
        params: &[(&str, BasicType, bool)],
        return_type: BasicType,
    ) {
        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::BuiltIn,
            params: params
                .iter()
                .map(|(n, t, opt)| ParameterInfo {
                    name: (*n).to_string(),
                    basic_type: t.clone(),
                    by_val: true,
                    is_optional: *opt,
                    is_array: false,
                })
                .collect(),
            return_type: Some(return_type),
            span: crate::ast::Span::new(0, 0, 1),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in SUB (procedure with no return value).
    pub(crate) fn register_builtin_sub(&mut self, name: &str, params: &[(&str, BasicType)]) {
        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::BuiltIn,
            params: params
                .iter()
                .map(|(n, t)| ParameterInfo {
                    name: (*n).to_string(),
                    basic_type: t.clone(),
                    by_val: true,
                    is_optional: false,
                    is_array: false,
                })
                .collect(),
            return_type: None,
            span: crate::ast::Span::new(0, 0, 1),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in SUB with optional parameters.
    pub(crate) fn register_builtin_sub_with_optionals(
        &mut self,
        name: &str,
        params: &[(&str, BasicType, bool)],
    ) {
        let entry = ProcedureEntry {
            name: name.to_string(),
            kind: ProcedureKind::BuiltIn,
            params: params
                .iter()
                .map(|(n, t, opt)| ParameterInfo {
                    name: (*n).to_string(),
                    basic_type: t.clone(),
                    by_val: true,
                    is_optional: *opt,
                    is_array: false,
                })
                .collect(),
            return_type: None,
            span: crate::ast::Span::new(0, 0, 1),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a single built-in constant (Long integer).
    ///
    /// Used by builtins_opengl for GL_* constants.
    pub(crate) fn register_builtin_constant(&mut self, name: &str, value: i64) {
        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(value),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0, 1),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(symbol);
    }

    /// Registers built-in constants (_TRUE, _FALSE, etc.).
    fn register_builtin_constants(&mut self) {
        let true_symbol = Symbol {
            name: "_TRUE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(-1),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0, 1),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(true_symbol);

        let false_symbol = Symbol {
            name: "_FALSE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(0),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0, 1),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(false_symbol);

        let none_symbol = Symbol {
            name: "_NONE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(0),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0, 1),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(none_symbol);

        self.register_character_constants();
        self.register_string_character_constants();
        self.register_error_constants();
        self.register_platform_constants();
        self.register_keyboard_constants();
    }

    /// Registers ASCII character constants (CHR$(0) through CHR$(31) names).
    fn register_character_constants(&mut self) {
        let mut define_char = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0, 1),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        define_char("_NUL", 0);
        define_char("_SOH", 1);
        define_char("_STX", 2);
        define_char("_ETX", 3);
        define_char("_EOT", 4);
        define_char("_ENQ", 5);
        define_char("_ACK", 6);
        define_char("_BEL", 7);
        define_char("_BS", 8);
        define_char("_HT", 9);
        define_char("_TAB", 9);
        define_char("_LF", 10);
        define_char("_VT", 11);
        define_char("_FF", 12);
        define_char("_CR", 13);
        define_char("_SO", 14);
        define_char("_SI", 15);
        define_char("_DLE", 16);
        define_char("_DC1", 17);
        define_char("_DC2", 18);
        define_char("_DC3", 19);
        define_char("_DC4", 20);
        define_char("_NAK", 21);
        define_char("_SYN", 22);
        define_char("_ETB", 23);
        define_char("_CAN", 24);
        define_char("_EM", 25);
        define_char("_SUB", 26);
        define_char("_ESC", 27);
        define_char("_FS", 28);
        define_char("_GS", 29);
        define_char("_RS", 30);
        define_char("_US", 31);
        define_char("_DEL", 127);
    }

    /// Registers _CHR_* string constants (actual character strings).
    fn register_string_character_constants(&mut self) {
        let mut define_str_char = |name: &str, value: &str| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::String(value.to_string()),
                },
                basic_type: BasicType::String,
                span: crate::ast::Span::new(0, 0, 1),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        define_str_char("_CHR_NUL", "\0");
        define_str_char("_CHR_SOH", "\x01");
        define_str_char("_CHR_STX", "\x02");
        define_str_char("_CHR_ETX", "\x03");
        define_str_char("_CHR_EOT", "\x04");
        define_str_char("_CHR_ENQ", "\x05");
        define_str_char("_CHR_ACK", "\x06");
        define_str_char("_CHR_BEL", "\x07");
        define_str_char("_CHR_BS", "\x08");
        define_str_char("_CHR_HT", "\t");
        define_str_char("_CHR_TAB", "\t");
        define_str_char("_CHR_LF", "\n");
        define_str_char("_CHR_VT", "\x0B");
        define_str_char("_CHR_FF", "\x0C");
        define_str_char("_CHR_CR", "\r");
        define_str_char("_CHR_SO", "\x0E");
        define_str_char("_CHR_SI", "\x0F");
        define_str_char("_CHR_DLE", "\x10");
        define_str_char("_CHR_DC1", "\x11");
        define_str_char("_CHR_DC2", "\x12");
        define_str_char("_CHR_DC3", "\x13");
        define_str_char("_CHR_DC4", "\x14");
        define_str_char("_CHR_NAK", "\x15");
        define_str_char("_CHR_SYN", "\x16");
        define_str_char("_CHR_ETB", "\x17");
        define_str_char("_CHR_CAN", "\x18");
        define_str_char("_CHR_EM", "\x19");
        define_str_char("_CHR_SUB", "\x1A");
        define_str_char("_CHR_ESC", "\x1B");
        define_str_char("_CHR_FS", "\x1C");
        define_str_char("_CHR_GS", "\x1D");
        define_str_char("_CHR_RS", "\x1E");
        define_str_char("_CHR_US", "\x1F");
        define_str_char("_CHR_DEL", "\x7F");

        define_str_char("_CHR_SPACE", " ");
        define_str_char("_CHR_EXCLAMATION", "!");
        define_str_char("_CHR_QUOTE", "\"");
        define_str_char("_CHR_HASH", "#");
        define_str_char("_CHR_DOLLAR", "$");
        define_str_char("_CHR_PERCENT", "%");
        define_str_char("_CHR_AMPERSAND", "&");
        define_str_char("_CHR_APOSTROPHE", "'");
        define_str_char("_CHR_LEFTBRACKET", "(");
        define_str_char("_CHR_RIGHTBRACKET", ")");
        define_str_char("_CHR_ASTERISK", "*");
        define_str_char("_CHR_PLUS", "+");
        define_str_char("_CHR_COMMA", ",");
        define_str_char("_CHR_MINUS", "-");
        define_str_char("_CHR_FULLSTOP", ".");
        define_str_char("_CHR_FORWARDSLASH", "/");
        define_str_char("_CHR_COLON", ":");
        define_str_char("_CHR_SEMICOLON", ";");
        define_str_char("_CHR_LESSTHAN", "<");
        define_str_char("_CHR_EQUAL", "=");
        define_str_char("_CHR_GREATERTHAN", ">");
        define_str_char("_CHR_QUESTION", "?");
        define_str_char("_CHR_ATSIGN", "@");
        define_str_char("_CHR_LEFTSQUAREBRACKET", "[");
        define_str_char("_CHR_BACKSLASH", "\\");
        define_str_char("_CHR_RIGHTSQUAREBRACKET", "]");
        define_str_char("_CHR_CARET", "^");
        define_str_char("_CHR_UNDERSCORE", "_");
        define_str_char("_CHR_GRAVE", "`");
        define_str_char("_CHR_LEFTCURLYBRACKET", "{");
        define_str_char("_CHR_VERTICALBAR", "|");
        define_str_char("_CHR_RIGHTCURLYBRACKET", "}");
        define_str_char("_CHR_TILDE", "~");

        define_str_char("_STR_EMPTY", "");
        define_str_char("_STR_CRLF", "\r\n");
        define_str_char("_STR_LF", "\n");
        define_str_char("_STR_CR", "\r");
    }

    /// Registers error code constants (ERR_* values).
    fn register_error_constants(&mut self) {
        let mut define_error = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0, 1),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        define_error("_ERR_NEXT_WITHOUT_FOR", 1);
        define_error("_ERR_SYNTAX_ERROR", 2);
        define_error("_ERR_RETURN_WITHOUT_GOSUB", 3);
        define_error("_ERR_OUT_OF_DATA", 4);
        define_error("_ERR_ILLEGAL_FUNCTION_CALL", 5);
        define_error("_ERR_OVERFLOW", 6);
        define_error("_ERR_OUT_OF_MEMORY", 7);
        define_error("_ERR_LABEL_NOT_DEFINED", 8);
        define_error("_ERR_SUBSCRIPT_OUT_OF_RANGE", 9);
        define_error("_ERR_DUPLICATE_DEFINITION", 10);
        define_error("_ERR_DIVISION_BY_ZERO", 11);
        define_error("_ERR_ILLEGAL_IN_DIRECT_MODE", 12);
        define_error("_ERR_TYPE_MISMATCH", 13);
        define_error("_ERR_OUT_OF_STRING_SPACE", 14);
        define_error("_ERR_STRING_TOO_LONG", 15);
        define_error("_ERR_STRING_FORMULA_TOO_COMPLEX", 16);
        define_error("_ERR_CANT_CONTINUE", 17);
        define_error("_ERR_FUNCTION_NOT_DEFINED", 18);
        define_error("_ERR_NO_RESUME", 19);
        define_error("_ERR_RESUME_WITHOUT_ERROR", 20);
        define_error("_ERR_DEVICE_TIMEOUT", 24);
        define_error("_ERR_DEVICE_FAULT", 25);
        define_error("_ERR_FOR_WITHOUT_NEXT", 26);
        define_error("_ERR_OUT_OF_PAPER", 27);
        define_error("_ERR_WHILE_WITHOUT_WEND", 29);
        define_error("_ERR_WEND_WITHOUT_WHILE", 30);
        define_error("_ERR_DUPLICATE_LABEL", 33);
        define_error("_ERR_SUBPROGRAM_NOT_DEFINED", 35);
        define_error("_ERR_ARGUMENT_COUNT_MISMATCH", 37);
        define_error("_ERR_ARRAY_NOT_DEFINED", 38);
        define_error("_ERR_VARIABLE_REQUIRED", 40);

        define_error("_ERR_FIELD_OVERFLOW", 50);
        define_error("_ERR_INTERNAL_ERROR", 51);
        define_error("_ERR_BAD_FILE_NAME_OR_NUMBER", 52);
        define_error("_ERR_FILE_NOT_FOUND", 53);
        define_error("_ERR_BAD_FILE_MODE", 54);
        define_error("_ERR_FILE_ALREADY_OPEN", 55);
        define_error("_ERR_FIELD_STATEMENT_ACTIVE", 56);
        define_error("_ERR_DEVICE_IO_ERROR", 57);
        define_error("_ERR_FILE_ALREADY_EXISTS", 58);
        define_error("_ERR_BAD_RECORD_LENGTH", 59);
        define_error("_ERR_DISK_FULL", 61);
        define_error("_ERR_INPUT_PAST_END_OF_FILE", 62);
        define_error("_ERR_BAD_RECORD_NUMBER", 63);
        define_error("_ERR_BAD_FILE_NAME", 64);
        define_error("_ERR_TOO_MANY_FILES", 67);
        define_error("_ERR_DEVICE_UNAVAILABLE", 68);
        define_error("_ERR_COMMUNICATION_BUFFER_OVERFLOW", 69);
        define_error("_ERR_PERMISSION_DENIED", 70);
        define_error("_ERR_DISK_NOT_READY", 71);
        define_error("_ERR_DISK_MEDIA_ERROR", 72);
        define_error("_ERR_FEATURE_UNAVAILABLE", 73);
        define_error("_ERR_RENAME_ACROSS_DISKS", 74);
        define_error("_ERR_PATH_FILE_ACCESS_ERROR", 75);
        define_error("_ERR_PATH_NOT_FOUND", 76);
    }

    /// Registers platform detection constants (_WINDOWS, _LINUX, _MACOSX, etc.).
    fn register_platform_constants(&mut self) {
        let mut define_platform = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0, 1),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        #[cfg(target_os = "windows")]
        {
            define_platform("_WINDOWS", -1);
            define_platform("_WIN", -1);
            define_platform("_LINUX", 0);
            define_platform("_MACOSX", 0);
            define_platform("_MAC", 0);
        }

        #[cfg(target_os = "linux")]
        {
            define_platform("_WINDOWS", 0);
            define_platform("_WIN", 0);
            define_platform("_LINUX", -1);
            define_platform("_MACOSX", 0);
            define_platform("_MAC", 0);
        }

        #[cfg(target_os = "macos")]
        {
            define_platform("_WINDOWS", 0);
            define_platform("_WIN", 0);
            define_platform("_LINUX", 0);
            define_platform("_MACOSX", -1);
            define_platform("_MAC", -1);
        }

        #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
        {
            define_platform("_WINDOWS", 0);
            define_platform("_WIN", 0);
            define_platform("_LINUX", 0);
            define_platform("_MACOSX", 0);
            define_platform("_MAC", 0);
        }

        #[cfg(target_pointer_width = "64")]
        {
            define_platform("_64BIT", -1);
            define_platform("_32BIT", 0);
        }

        #[cfg(target_pointer_width = "32")]
        {
            define_platform("_64BIT", 0);
            define_platform("_32BIT", -1);
        }

        #[cfg(not(any(target_pointer_width = "64", target_pointer_width = "32")))]
        {
            define_platform("_64BIT", 0);
            define_platform("_32BIT", 0);
        }
    }

    /// Registers keyboard scan code constants (_KEY_*).
    fn register_keyboard_constants(&mut self) {
        let mut define_key = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0, 1),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        define_key("_KEY_F1", 15104);
        define_key("_KEY_F2", 15360);
        define_key("_KEY_F3", 15616);
        define_key("_KEY_F4", 15872);
        define_key("_KEY_F5", 16128);
        define_key("_KEY_F6", 16384);
        define_key("_KEY_F7", 16640);
        define_key("_KEY_F8", 16896);
        define_key("_KEY_F9", 17152);
        define_key("_KEY_F10", 17408);
        define_key("_KEY_F11", 34048);
        define_key("_KEY_F12", 34304);

        define_key("_KEY_HOME", 18176);
        define_key("_KEY_END", 20224);
        define_key("_KEY_PAGEUP", 18688);
        define_key("_KEY_PAGEDOWN", 20736);
        define_key("_KEY_INSERT", 20992);
        define_key("_KEY_DELETE", 21248);

        define_key("_KEY_UP", 18432);
        define_key("_KEY_DOWN", 20480);
        define_key("_KEY_LEFT", 19200);
        define_key("_KEY_RIGHT", 19712);

        define_key("_KEY_LSHIFT", 100304);
        define_key("_KEY_RSHIFT", 100303);
        define_key("_KEY_LCTRL", 100306);
        define_key("_KEY_RCTRL", 100305);
        define_key("_KEY_LALT", 100308);
        define_key("_KEY_RALT", 100307);
        define_key("_KEY_CAPSLOCK", 100301);
        define_key("_KEY_NUMLOCK", 100300);
        define_key("_KEY_SCROLLLOCK", 100302);

        define_key("_KEY_PRINT", 100316);
        define_key("_KEY_PAUSE", 100319);
    }
}
