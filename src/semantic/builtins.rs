//! Built-in function and constant registration.
//!
//! This module registers all QB64 built-in functions, subs, and constants
//! into the symbol table during semantic analyzer initialization.
//!
//! # Organization
//!
//! - `register_builtins` - Main entry point that registers everything
//! - `register_builtin_function` - Helper for registering a function
//! - `register_builtin_function_with_optionals` - Function with optional params
//! - `register_builtin_sub` - Helper for registering a SUB
//! - `register_builtin_sub_with_optionals` - SUB with optional params
//! - `register_builtin_constants` - Core constants (_TRUE, _FALSE, etc.)
//! - `register_character_constants` - ASCII character constants
//! - `register_error_constants` - Error code constants
//! - `register_platform_constants` - Platform detection constants
//! - `register_keyboard_constants` - Keyboard scan codes

use super::SemanticAnalyzer;
use super::symbols::{
    ConstValue, ParameterInfo, ProcedureEntry, ProcedureKind, Symbol, SymbolKind,
};
use super::types::BasicType;

impl SemanticAnalyzer {
    /// Registers all built-in functions.
    pub(super) fn register_builtins(&mut self) {
        // Register built-in constants first
        self.register_builtin_constants();

        // String/UDT functions
        // Note: Using Long for integer parameters since integer literals default to Long in QB64
        // LEN() can return the length of a string OR the size of a UDT/fixed-length type
        self.register_builtin_function("LEN", &[("s", BasicType::Unknown)], BasicType::Long);
        self.register_builtin_function("CHR$", &[("n", BasicType::Long)], BasicType::String);
        // ASC can be called with 1 or 2 arguments:
        // ASC(s$) - returns ASCII of first character
        // ASC(s$, position%) - returns ASCII of character at position
        self.register_builtin_function_with_optionals(
            "ASC",
            &[
                ("s", BasicType::String, false),
                ("position", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "LEFT$",
            &[("s", BasicType::String), ("n", BasicType::Long)],
            BasicType::String,
        );
        self.register_builtin_function(
            "RIGHT$",
            &[("s", BasicType::String), ("n", BasicType::Long)],
            BasicType::String,
        );
        // MID$ can be called with 2 or 3 arguments: MID$(s$, start) or MID$(s$, start, len)
        self.register_builtin_function_with_optionals(
            "MID$",
            &[
                ("s", BasicType::String, false),
                ("start", BasicType::Long, false),
                ("len", BasicType::Long, true), // optional - if omitted, returns rest of string
            ],
            BasicType::String,
        );
        // INSTR can be called with 2 or 3 arguments:
        // INSTR(string, substring) - search from beginning
        // INSTR(start, string, substring) - search from position
        self.register_builtin_function_with_optionals(
            "INSTR",
            &[
                ("start_or_string", BasicType::Unknown, false), // can be Long or String
                ("string_or_find", BasicType::String, false),
                ("find", BasicType::String, true), // optional - if omitted, arg1 is string, arg2 is find
            ],
            BasicType::Long,
        );
        self.register_builtin_function("UCASE$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("LCASE$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("LTRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("RTRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("TRIM$", &[("s", BasicType::String)], BasicType::String);
        self.register_builtin_function("STR$", &[("n", BasicType::Double)], BasicType::String);
        self.register_builtin_function("VAL", &[("s", BasicType::String)], BasicType::Double);
        // STRING$ can take either a character code (integer) or a single-char string:
        // STRING$(n, charcode%) or STRING$(n, char$)
        // Using Unknown for the second parameter allows both
        self.register_builtin_function(
            "STRING$",
            &[("n", BasicType::Long), ("c", BasicType::Unknown)],
            BasicType::String,
        );
        self.register_builtin_function("SPACE$", &[("n", BasicType::Long)], BasicType::String);

        // Math functions
        self.register_builtin_function("ABS", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("SGN", &[("n", BasicType::Double)], BasicType::Integer);
        self.register_builtin_function("INT", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("FIX", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("CINT", &[("n", BasicType::Double)], BasicType::Integer);
        self.register_builtin_function("CLNG", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("CSNG", &[("n", BasicType::Double)], BasicType::Single);
        self.register_builtin_function("CDBL", &[("n", BasicType::Single)], BasicType::Double);
        self.register_builtin_function("SQR", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("LOG", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("EXP", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("SIN", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("COS", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("TAN", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("ATN", &[("n", BasicType::Double)], BasicType::Double);
        // RND can be called with 0 or 1 arguments: RND or RND(n)
        // RND with no args or RND(1) returns next random number
        // RND(0) returns the last random number generated
        // RND(negative) reseeds the generator
        self.register_builtin_function_with_optionals(
            "RND",
            &[("n", BasicType::Single, true)], // optional seed/mode parameter
            BasicType::Single,
        );

        // QB64 extended math functions
        self.register_builtin_function("_PI", &[], BasicType::Double);
        self.register_builtin_function("_ASIN", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ACOS", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function(
            "_ATAN2",
            &[("y", BasicType::Double), ("x", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_HYPOT",
            &[("x", BasicType::Double), ("y", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function("_CEIL", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function("_ROUND", &[("n", BasicType::Double)], BasicType::Long);
        self.register_builtin_function(
            "_MIN",
            &[("a", BasicType::Double), ("b", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_MAX",
            &[("a", BasicType::Double), ("b", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_CLAMP",
            &[
                ("value", BasicType::Double),
                ("min", BasicType::Double),
                ("max", BasicType::Double),
            ],
            BasicType::Double,
        );

        // Hyperbolic functions
        self.register_builtin_function("_SINH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_COSH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_TANH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ASINH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ACOSH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ATANH", &[("n", BasicType::Double)], BasicType::Double);

        // Reciprocal trig functions (sec, csc, cot)
        self.register_builtin_function("_SEC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_CSC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_COT", &[("n", BasicType::Double)], BasicType::Double);

        // Hyperbolic reciprocals
        self.register_builtin_function("_SECH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_CSCH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_COTH", &[("n", BasicType::Double)], BasicType::Double);

        // Inverse reciprocal trig (arcsec, arccsc, arccot)
        self.register_builtin_function("_ARCSEC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCSC", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCOT", &[("n", BasicType::Double)], BasicType::Double);

        // Inverse hyperbolic reciprocals
        self.register_builtin_function("_ARCSECH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCSCH", &[("n", BasicType::Double)], BasicType::Double);
        self.register_builtin_function("_ARCCOTH", &[("n", BasicType::Double)], BasicType::Double);

        // Angle conversions (degrees <-> radians)
        self.register_builtin_function(
            "_D2R",
            &[("degrees", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_R2D",
            &[("radians", BasicType::Double)],
            BasicType::Double,
        );

        // Gradian conversions
        self.register_builtin_function(
            "_D2G",
            &[("degrees", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_G2D",
            &[("gradians", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_G2R",
            &[("gradians", BasicType::Double)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_R2G",
            &[("radians", BasicType::Double)],
            BasicType::Double,
        );

        // Negate
        self.register_builtin_function("_NEGATE", &[("n", BasicType::Double)], BasicType::Double);

        // String comparison
        self.register_builtin_function(
            "_STRCMP",
            &[("a", BasicType::String), ("b", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_STRICMP",
            &[("a", BasicType::String), ("b", BasicType::String)],
            BasicType::Long,
        );

        // Bitwise operations
        self.register_builtin_function(
            "_SHL",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_SHR",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_ROL",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_ROR",
            &[("value", BasicType::Long), ("bits", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_READBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_SETBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_RESETBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_TOGGLEBIT",
            &[("value", BasicType::Long), ("bit", BasicType::Long)],
            BasicType::Long,
        );

        // Type conversion
        self.register_builtin_function("HEX$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("OCT$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("_BIN$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("_TOSTR$", &[("n", BasicType::Double)], BasicType::String);

        // Inline conditional
        self.register_builtin_function(
            "_IIF",
            &[
                ("cond", BasicType::Long),
                ("true_val", BasicType::Double),
                ("false_val", BasicType::Double),
            ],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_IIF$",
            &[
                ("cond", BasicType::Long),
                ("true_val", BasicType::String),
                ("false_val", BasicType::String),
            ],
            BasicType::String,
        );

        // Array functions
        // LBOUND/UBOUND can be called with 1 or 2 arguments:
        // LBOUND(arr) - returns lower bound of first dimension
        // LBOUND(arr, dimension) - returns lower bound of specified dimension
        self.register_builtin_function_with_optionals(
            "LBOUND",
            &[
                ("arr", BasicType::Unknown, false),
                ("dimension", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "UBOUND",
            &[
                ("arr", BasicType::Unknown, false),
                ("dimension", BasicType::Long, true),
            ],
            BasicType::Long,
        );

        // Timer/Date
        // TIMER can be called with 0 or 1 argument:
        // TIMER - returns seconds since midnight as single
        // TIMER(accuracy!) - QB64 extension with optional accuracy parameter
        self.register_builtin_function_with_optionals(
            "TIMER",
            &[("accuracy", BasicType::Single, true)],
            BasicType::Single,
        );
        self.register_builtin_function("DATE$", &[], BasicType::String);
        self.register_builtin_function("TIME$", &[], BasicType::String);

        // Print formatting functions
        // Use Long for parameters since integer literals default to Long
        self.register_builtin_function("TAB", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("SPC", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("POS", &[("n", BasicType::Long)], BasicType::Integer);
        self.register_builtin_function("CSRLIN", &[], BasicType::Integer);

        // SCREEN function - read character/attribute from text screen
        // SCREEN(row, col) - returns ASCII value of character
        // SCREEN(row, col, flag) - if flag<>0, returns color attribute instead
        self.register_builtin_function_with_optionals(
            "SCREEN",
            &[
                ("row", BasicType::Integer, false),
                ("col", BasicType::Integer, false),
                ("flag", BasicType::Integer, true), // optional - if non-zero, return color attribute
            ],
            BasicType::Integer,
        );

        // File I/O functions
        self.register_builtin_function("EOF", &[("fnum", BasicType::Integer)], BasicType::Integer);
        self.register_builtin_function("LOF", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("LOC", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("SEEK", &[("fnum", BasicType::Integer)], BasicType::Long);
        self.register_builtin_function("FREEFILE", &[], BasicType::Integer);

        // Keyboard input functions
        self.register_builtin_function("INKEY$", &[], BasicType::String);
        // INPUT$(n) - read n chars from keyboard
        // INPUT$(n, filenum) - read n chars from file
        self.register_builtin_function_with_optionals(
            "INPUT$",
            &[
                ("n", BasicType::Integer, false),
                ("filenum", BasicType::Integer, true),
            ],
            BasicType::String,
        );

        // QB64 keyboard extensions
        self.register_builtin_function("_KEYHIT", &[], BasicType::Long);
        self.register_builtin_function("_KEYDOWN", &[("code", BasicType::Long)], BasicType::Long);
        self.register_builtin_function("_CINP", &[], BasicType::Long);
        // _KEYCLEAR is a statement, not a function - handled separately

        // Lock key state functions
        self.register_builtin_function("_CAPSLOCK", &[], BasicType::Long);
        self.register_builtin_function("_NUMLOCK", &[], BasicType::Long);
        self.register_builtin_function("_SCROLLLOCK", &[], BasicType::Long);

        // Error handling functions
        self.register_builtin_function("ERR", &[], BasicType::Integer);
        self.register_builtin_function("ERL", &[], BasicType::Integer);
        // QB64 error handling extensions
        self.register_builtin_function("_ERRORLINE", &[], BasicType::Long);
        self.register_builtin_function("_ERRORMESSAGE$", &[], BasicType::String);

        // Utility functions
        self.register_builtin_function("_COMMANDCOUNT", &[], BasicType::Long);
        self.register_builtin_function("_ENVIRONCOUNT", &[], BasicType::Long);

        // Environment functions
        self.register_builtin_function(
            "ENVIRON$",
            &[("var", BasicType::String)],
            BasicType::String,
        );
        // COMMAND$ can be called with 0 or 1 argument:
        // COMMAND$ - returns entire command line
        // COMMAND$(n) - returns nth command line argument
        self.register_builtin_function_with_optionals(
            "COMMAND$",
            &[("index", BasicType::Long, true)],
            BasicType::String,
        );
        self.register_builtin_function("_CWD$", &[], BasicType::String);
        self.register_builtin_function("_OS$", &[], BasicType::String);
        self.register_builtin_function("_STARTDIR$", &[], BasicType::String);

        // Phase 2: String Enhancements
        // _INSTRREV can be called with 2 or 3 arguments:
        // _INSTRREV(source$, search$) - search from end
        // _INSTRREV(start, source$, search$) - search from position
        self.register_builtin_function_with_optionals(
            "_INSTRREV",
            &[
                ("start_or_source", BasicType::Unknown, false), // can be Long or String
                ("source_or_search", BasicType::String, false),
                ("search", BasicType::String, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function("_TRIM$", &[("s", BasicType::String)], BasicType::String);

        // Binary string packing/unpacking functions
        self.register_builtin_function("MKI$", &[("n", BasicType::Integer)], BasicType::String);
        self.register_builtin_function("MKL$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("MKS$", &[("n", BasicType::Single)], BasicType::String);
        self.register_builtin_function("MKD$", &[("n", BasicType::Double)], BasicType::String);
        self.register_builtin_function("CVI", &[("s", BasicType::String)], BasicType::Integer);
        self.register_builtin_function("CVL", &[("s", BasicType::String)], BasicType::Long);
        self.register_builtin_function("CVS", &[("s", BasicType::String)], BasicType::Single);
        self.register_builtin_function("CVD", &[("s", BasicType::String)], BasicType::Double);

        // Phase 2: QB64 Date/Time enhancements
        self.register_builtin_function("_DATE$", &[], BasicType::String);
        self.register_builtin_function("_TIME$", &[], BasicType::String);

        // Phase 2: Memory operations
        self.register_builtin_function("_MEMNEW", &[("size", BasicType::Offset)], BasicType::Mem);
        self.register_builtin_function("_MEMFREE", &[("mem", BasicType::Mem)], BasicType::Void);
        self.register_builtin_function(
            "_MEMGET",
            &[("mem", BasicType::Mem), ("offset", BasicType::Offset)],
            BasicType::Unknown, // Return type depends on context
        );
        self.register_builtin_function(
            "_MEMPUT",
            &[
                ("mem", BasicType::Mem),
                ("offset", BasicType::Offset),
                ("value", BasicType::Unknown),
            ],
            BasicType::Void,
        );
        self.register_builtin_function(
            "_MEMCOPY",
            &[
                ("source", BasicType::Mem),
                ("src_offset", BasicType::Offset),
                ("size", BasicType::Offset),
                ("dest", BasicType::Mem),
                ("dest_offset", BasicType::Offset),
            ],
            BasicType::Void,
        );
        self.register_builtin_function(
            "_MEMFILL",
            &[
                ("mem", BasicType::Mem),
                ("offset", BasicType::Offset),
                ("size", BasicType::Offset),
                ("value", BasicType::Unknown),
            ],
            BasicType::Void,
        );
        self.register_builtin_function(
            "_OFFSET",
            &[("variable", BasicType::Unknown)],
            BasicType::Offset,
        );
        self.register_builtin_function("_MEM", &[("variable", BasicType::Unknown)], BasicType::Mem);

        // Phase 5: System Integration
        self.register_builtin_function(
            "_FILEEXISTS",
            &[("path", BasicType::String)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_DIREXISTS",
            &[("path", BasicType::String)],
            BasicType::Integer,
        );
        self.register_builtin_function("_DIR$", &[("spec", BasicType::String)], BasicType::String);

        // SHELL function form: ret% = SHELL(command$)
        // Returns the exit code of the command (0 = success)
        self.register_builtin_function("SHELL", &[("command", BasicType::String)], BasicType::Long);

        // _SHELLHIDE function form: ret% = _SHELLHIDE(command$)
        // Returns the exit code of the command (0 = success), runs without visible console
        self.register_builtin_function(
            "_SHELLHIDE",
            &[("command", BasicType::String)],
            BasicType::Long,
        );

        // File content helpers
        self.register_builtin_function(
            "_READFILE$",
            &[("path", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_sub(
            "_WRITEFILE",
            &[("path", BasicType::String), ("content", BasicType::String)],
        );

        // Phase 5: Mouse Input
        self.register_builtin_function("_MOUSEX", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEY", &[], BasicType::Integer);
        self.register_builtin_function(
            "_MOUSEBUTTON",
            &[("button", BasicType::Integer)],
            BasicType::Integer,
        );
        self.register_builtin_function("_MOUSEINPUT", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEMOVEMENTX", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEMOVEMENTY", &[], BasicType::Integer);
        self.register_builtin_function("_MOUSEWHEEL", &[], BasicType::Integer);

        // Phase 5: Clipboard
        self.register_builtin_function("_CLIPBOARD$", &[], BasicType::String);

        // Sound functions
        // _SNDOPEN can have optional mode/requirements string
        self.register_builtin_function_with_optionals(
            "_SNDOPEN",
            &[
                ("file", BasicType::String, false),
                ("mode", BasicType::String, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function("_SNDOPENRAW", &[], BasicType::Long);
        self.register_builtin_function("_SNDCOPY", &[("handle", BasicType::Long)], BasicType::Long);
        self.register_builtin_function(
            "_SNDPLAYING",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_SNDPAUSED",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_SNDGETPOS",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );
        self.register_builtin_function(
            "_SNDLEN",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );

        // Font support
        // _LOADFONT can be called with 2 or 3 arguments:
        // _LOADFONT(file$, size%) - load font
        // _LOADFONT(file$, size%, style$) - load font with style ("BOLD,ITALIC,etc")
        self.register_builtin_function_with_optionals(
            "_LOADFONT",
            &[
                ("file", BasicType::String, false),
                ("size", BasicType::Long, false),
                ("style", BasicType::String, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function("_FONTHEIGHT", &[], BasicType::Long);
        self.register_builtin_function("_FONTWIDTH", &[], BasicType::Long);
        self.register_builtin_function(
            "_PRINTWIDTH",
            &[("text", BasicType::String)],
            BasicType::Long,
        ); // Get pixel width of text

        // Unicode font functions (stubs for QB64PE parity)
        self.register_builtin_function(
            "_UPRINTWIDTH",
            &[("text", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_UCHARPOS",
            &[("text", BasicType::String), ("pos", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_UFONTHEIGHT",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        self.register_builtin_function("_ULINESPACING", &[], BasicType::Long);
        self.register_builtin_function("_FONT", &[("handle", BasicType::Long)], BasicType::Long); // Sets current font, returns previous handle
        self.register_builtin_function(
            "_FREEFONT",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );

        // Desktop/Window functions
        self.register_builtin_function("_DESKTOPWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_DESKTOPHEIGHT", &[], BasicType::Long);
        self.register_builtin_function("_SCREENX", &[], BasicType::Long);
        self.register_builtin_function("_SCREENY", &[], BasicType::Long);
        self.register_builtin_function("_TITLE$", &[], BasicType::String);
        // _TITLE as function can take an optional string argument:
        // _TITLE - no effect as expression, exists for statement dual-use
        // _TITLE(title$) - sets the window title (returns void)
        self.register_builtin_function_with_optionals(
            "_TITLE",
            &[("title", BasicType::String, true)],
            BasicType::Long, // Returns 0 as placeholder
        );
        // _ICON as function can take 0 or 1 argument:
        // _ICON - returns current icon handle
        // _ICON(handle&) - sets window icon (also a statement)
        self.register_builtin_function_with_optionals(
            "_ICON",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        self.register_builtin_function("_WINDOWHANDLE", &[], BasicType::Long);
        self.register_builtin_function("_WINDOWHASFOCUS", &[], BasicType::Long);

        // Window control functions (also used as statements)
        self.register_builtin_function(
            "_SCREENMOVE",
            &[("x", BasicType::Long), ("y", BasicType::Long)],
            BasicType::Long, // Returns 0 for success
        );
        self.register_builtin_function("_SCREENHIDE", &[], BasicType::Long);
        self.register_builtin_function("_SCREENSHOW", &[], BasicType::Long);
        // _FULLSCREEN can take 0-1 argument:
        // _FULLSCREEN - returns current mode (0=windowed, 1=fullscreen, 2=desktop)
        // _FULLSCREEN mode& - sets mode and returns previous mode
        self.register_builtin_function_with_optionals(
            "_FULLSCREEN",
            &[("mode", BasicType::Long, true)],
            BasicType::Long,
        );
        // Windows-only desktop functions
        // _SCREENCLICK x, y [, button] - simulate mouse click on desktop
        self.register_builtin_function_with_optionals(
            "_SCREENCLICK",
            &[
                ("x", BasicType::Long, false),
                ("y", BasicType::Long, false),
                ("button", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        // _SCREENPRINT text$ - simulate keyboard input to focused window
        self.register_builtin_function(
            "_SCREENPRINT",
            &[("text", BasicType::String)],
            BasicType::Long,
        );
        // _SCREENIMAGE([x1, y1, x2, y2]) - capture desktop screenshot
        self.register_builtin_function_with_optionals(
            "_SCREENIMAGE",
            &[
                ("x1", BasicType::Long, true),
                ("y1", BasicType::Long, true),
                ("x2", BasicType::Long, true),
                ("y2", BasicType::Long, true),
            ],
            BasicType::Long,
        );

        // Alpha blending functions
        // _BLEND [handle&] - enable alpha blending for image (default: current destination)
        self.register_builtin_function_with_optionals(
            "_BLEND",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        // _DONTBLEND [handle&] - disable alpha blending for image
        self.register_builtin_function_with_optionals(
            "_DONTBLEND",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        // _CLEARCOLOR takes 0-2 arguments:
        // _CLEARCOLOR(handle&) - returns clear color for image (-1 if none)
        // _CLEARCOLOR color&, handle& - sets clear color (transparency key)
        // Note: QB64 syntax is: _CLEARCOLOR color&[, handle&] or _CLEARCOLOR [,handle&] to clear
        self.register_builtin_function_with_optionals(
            "_CLEARCOLOR",
            &[
                ("color", BasicType::Long, true),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );

        // Dialog boxes
        // _MESSAGEBOX can be called with 0-5 arguments (all optional):
        // _MESSAGEBOX() - simple message box with defaults
        // _MESSAGEBOX(title$) - with title
        // _MESSAGEBOX(title$, message$) - with message
        // _MESSAGEBOX(title$, message$, dialogType$) - with OK/Cancel etc.
        // _MESSAGEBOX(title$, message$, dialogType$, iconType$) - with icon
        // _MESSAGEBOX(title$, message$, dialogType$, iconType$, defaultButton&) - full
        self.register_builtin_function_with_optionals(
            "_MESSAGEBOX",
            &[
                ("title", BasicType::String, true),
                ("message", BasicType::String, true),
                ("dialogType", BasicType::String, true),
                ("iconType", BasicType::String, true),
                ("defaultButton", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_INPUTBOX$",
            &[("prompt", BasicType::String), ("title", BasicType::String)],
            BasicType::String,
        );
        // _OPENFILEDIALOG$ can take 2-5 arguments
        self.register_builtin_function_with_optionals(
            "_OPENFILEDIALOG$",
            &[
                ("title", BasicType::String, false),
                ("filter", BasicType::String, false),
                ("defaultDir", BasicType::String, true),
                ("defaultFile", BasicType::String, true),
                ("flags", BasicType::Long, true),
            ],
            BasicType::String,
        );
        // _SAVEFILEDIALOG$ can take 2-4 arguments
        self.register_builtin_function_with_optionals(
            "_SAVEFILEDIALOG$",
            &[
                ("title", BasicType::String, false),
                ("filter", BasicType::String, false),
                ("defaultDir", BasicType::String, true),
                ("defaultFile", BasicType::String, true),
            ],
            BasicType::String,
        );
        self.register_builtin_function(
            "_SELECTFOLDERDIALOG$",
            &[("title", BasicType::String)],
            BasicType::String,
        );

        // Phase 5: Networking
        // _OPENHOST takes a connection specification string like "TCP/IP:12345"
        self.register_builtin_function(
            "_OPENHOST",
            &[("connection_string", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_OPENCONNECTION",
            &[("host_handle", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_OPENCLIENT",
            &[("connection_string", BasicType::String)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_CONNECTED",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        // HTTP status code for network handles
        self.register_builtin_function(
            "_STATUSCODE",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );

        // Image buffer functions
        self.register_builtin_function(
            "_NEWIMAGE",
            &[
                ("width", BasicType::Long),
                ("height", BasicType::Long),
                ("mode", BasicType::Long),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_LOADIMAGE",
            &[("filename", BasicType::String), ("mode", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_COPYIMAGE",
            &[("source", BasicType::Long), ("mode", BasicType::Long)],
            BasicType::Long,
        );
        // Image dimension functions (take handle, return dimension)
        // _WIDTH can be called with 0 or 1 argument:
        // _WIDTH - returns width of current screen/image
        // _WIDTH(handle&) - returns width of specified image
        self.register_builtin_function_with_optionals(
            "_WIDTH",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        // _HEIGHT can take 0 or 1 argument: _HEIGHT or _HEIGHT(handle)
        self.register_builtin_function_with_optionals(
            "_HEIGHT",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );

        // Register remaining builtins (split for readability)
        self.register_graphics_builtins();
        self.register_audio_builtins();
        self.register_extended_builtins();
    }

    /// Registers graphics-related built-in functions.
    fn register_graphics_builtins(&mut self) {
        // Coordinate mapping function
        // PMAP(coordinate, function_code)
        // function_code: 0 = world X to screen X, 1 = world Y to screen Y,
        //                2 = screen X to world X, 3 = screen Y to world Y
        self.register_builtin_function(
            "PMAP",
            &[
                ("coordinate", BasicType::Double),
                ("function_code", BasicType::Long),
            ],
            BasicType::Double,
        );

        // POINT function - get pixel color or cursor coordinates
        // POINT(x, y) - returns color attribute of pixel at (x, y) - returns LONG
        // POINT(function) - returns cursor coordinates:
        //   0 = current logical X, 1 = current logical Y,
        //   2 = current physical X, 3 = current physical Y
        // When called with one argument, returns Double (for world coordinates)
        // When called with two arguments, returns Long (color value)
        self.register_builtin_function_with_optionals(
            "POINT",
            &[
                ("x_or_function", BasicType::Long, false), // X coordinate or function code
                ("y", BasicType::Long, true),              // Y coordinate (optional)
            ],
            BasicType::Long, // Returns color (Long) or coordinate (also Long for physical coords)
        );

        // Print formatting functions (legacy)
        // LPOS returns the current position of the line printer
        self.register_builtin_function("LPOS", &[("n", BasicType::Long)], BasicType::Integer);

        // Event Handling Functions (QB4.5)
        // KEY(n) function - check key trap status (returns event status: -1=enabled, 0=disabled, 1=event pending)
        self.register_builtin_function("KEY", &[("n", BasicType::Long)], BasicType::Integer);

        // Joystick Functions (QB4.5)
        // STICK(n) - returns joystick position
        // n=0: returns X coordinate of joystick A (and latches Y)
        // n=1: returns Y coordinate of joystick A
        // n=2: returns X coordinate of joystick B (and latches Y)
        // n=3: returns Y coordinate of joystick B
        self.register_builtin_function("STICK", &[("n", BasicType::Long)], BasicType::Integer);
        // STRIG(n) or STRIG(n, controller) - returns joystick trigger state
        // n=0: lower trigger A pressed since last STRIG(0)
        // n=1: lower trigger A currently pressed
        // n=2: lower trigger B pressed since last STRIG(2)
        // n=3: lower trigger B currently pressed
        // n=4: upper trigger A pressed since last STRIG(4)
        // n=5: upper trigger A currently pressed
        // n=6: upper trigger B pressed since last STRIG(6)
        // n=7: upper trigger B currently pressed
        // QB64 extension: optional controller parameter overrides the controller implied by n
        self.register_builtin_function_with_optionals(
            "STRIG",
            &[
                ("n", BasicType::Long, false),
                ("controller", BasicType::Long, true), // QB64 extension
            ],
            BasicType::Integer,
        );

        // Memory Functions (QB4.5)
        // FRE(n) - returns free memory
        // n=-1: largest block of free string space
        // n=-2: available stack space
        // n=0 or "string": free string space
        // n=any other: far heap space (legacy, returns large number on modern systems)
        self.register_builtin_function("FRE", &[("n", BasicType::Long)], BasicType::Long);

        // PEEK(address) - reads a byte from memory address within current DEF SEG segment
        // Returns a value from 0-255. In modern QB64, this uses an emulated memory model.
        self.register_builtin_function("PEEK", &[("address", BasicType::Long)], BasicType::Integer);

        // Port I/O Functions (QB4.5 - may be sandboxed)
        // INP(port) - reads a byte from hardware I/O port
        self.register_builtin_function("INP", &[("port", BasicType::Long)], BasicType::Integer);

        // Light Pen Functions (QB4.5 legacy)
        // PEN(n) - returns light pen information (stub - returns 0)
        self.register_builtin_function("PEN", &[("n", BasicType::Long)], BasicType::Integer);

        // Serial I/O Functions (QB4.5)
        // ERDEV - returns device error code
        self.register_builtin_function("ERDEV", &[], BasicType::Integer);
        // ERDEV$ - returns device error name
        self.register_builtin_function("ERDEV$", &[], BasicType::String);
        // IOCTL$(filenum) - returns device control string from driver
        self.register_builtin_function(
            "IOCTL$",
            &[("filenum", BasicType::Long)],
            BasicType::String,
        );

        // Memory/Legacy functions
        // VARPTR returns the offset address of a variable within its segment
        self.register_builtin_function(
            "VARPTR",
            &[("variable", BasicType::Unknown)],
            BasicType::Long,
        );
        // VARPTR$ returns a binary string representation of a variable's address
        self.register_builtin_function(
            "VARPTR$",
            &[("variable", BasicType::Unknown)],
            BasicType::String,
        );
        // VARSEG returns the segment address of a variable (returns 0 in flat memory model)
        self.register_builtin_function(
            "VARSEG",
            &[("variable", BasicType::Unknown)],
            BasicType::Long,
        );
        // SADD returns the address of a string's data
        self.register_builtin_function("SADD", &[("s", BasicType::String)], BasicType::Long);

        // File System functions
        // FILEATTR returns file mode or handle attributes
        self.register_builtin_function(
            "FILEATTR",
            &[
                ("filenum", BasicType::Integer),
                ("attribute", BasicType::Integer),
            ],
            BasicType::Integer,
        );

        // Type Conversion (Microsoft Binary Format)
        // CVSMBF converts a 4-byte MBF string to a SINGLE
        self.register_builtin_function("CVSMBF", &[("s", BasicType::String)], BasicType::Single);
        // CVDMBF converts an 8-byte MBF string to a DOUBLE
        self.register_builtin_function("CVDMBF", &[("s", BasicType::String)], BasicType::Double);
        // MKSMBF$ converts a SINGLE to a 4-byte MBF string
        self.register_builtin_function("MKSMBF$", &[("n", BasicType::Single)], BasicType::String);
        // MKDMBF$ converts a DOUBLE to an 8-byte MBF string
        self.register_builtin_function("MKDMBF$", &[("n", BasicType::Double)], BasicType::String);

        // Color creation functions
        // _RGB(r, g, b) or _RGB(r, g, b, handle) - creates a color value
        // _RGB32 has variants: _RGB32(i), _RGB32(i, a), _RGB32(r, g, b), _RGB32(r, g, b, a)
        self.register_builtin_function_with_optionals(
            "_RGB",
            &[
                ("red", BasicType::Long, false),
                ("green", BasicType::Long, false),
                ("blue", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        // _RGB32 is complex - can take 1, 2, 3, or 4 arguments
        // For now, register the 3-4 argument variant (most common)
        self.register_builtin_function_with_optionals(
            "_RGB32",
            &[
                ("red", BasicType::Long, false),
                ("green", BasicType::Long, false),
                ("blue", BasicType::Long, false),
                ("alpha", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        // _RGBA and _RGBA32 - explicit alpha channel
        self.register_builtin_function_with_optionals(
            "_RGBA",
            &[
                ("red", BasicType::Long, false),
                ("green", BasicType::Long, false),
                ("blue", BasicType::Long, false),
                ("alpha", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_RGBA32",
            &[
                ("red", BasicType::Long),
                ("green", BasicType::Long),
                ("blue", BasicType::Long),
                ("alpha", BasicType::Long),
            ],
            BasicType::Long,
        );

        // Color component extraction functions
        // _RED, _GREEN, _BLUE, _ALPHA extract color components (0-255)
        // Can take 1 or 2 arguments: _RED(color) or _RED(color, imagehandle)
        self.register_builtin_function_with_optionals(
            "_RED",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_GREEN",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_BLUE",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_ALPHA",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        // 32-bit variants (same functionality, for explicitness)
        self.register_builtin_function_with_optionals(
            "_RED32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_GREEN32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_BLUE32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_ALPHA32",
            &[
                ("color", BasicType::Long, false),
                ("handle", BasicType::Long, true),
            ],
            BasicType::Long,
        );

        // _PIXELSIZE returns bytes per pixel for current screen/image
        // 0 = text mode, 1 = 256 color, 4 = 32-bit color
        self.register_builtin_function_with_optionals(
            "_PIXELSIZE",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );

        // _PALETTECOLOR - dual purpose:
        // - As function: _PALETTECOLOR(attribute%[, imgHandle&]) returns palette color (LONG)
        // - As statement: _PALETTECOLOR attribute%, color&[, imgHandle&] sets palette color
        // Both forms are handled through function registration with optional parameters
        self.register_builtin_function_with_optionals(
            "_PALETTECOLOR",
            &[
                ("attribute", BasicType::Long, false),
                ("color_or_handle", BasicType::Long, true), // color (set) or handle (get)
                ("handle", BasicType::Long, true),          // handle when setting
            ],
            BasicType::Long,
        );

        // _SCREENEXISTS returns -1 if graphics window exists, 0 otherwise
        self.register_builtin_function("_SCREENEXISTS", &[], BasicType::Integer);

        // _EXIT - dual purpose:
        // - As function with 0 args: returns exit request state (non-zero if user requested exit)
        // - As statement with 1 arg: exits program with specific return code
        self.register_builtin_function_with_optionals(
            "_EXIT",
            &[("code", BasicType::Long, true)], // optional code parameter
            BasicType::Long,
        );

        // _DEFAULTCOLOR returns the default foreground color for the current _DEST
        self.register_builtin_function("_DEFAULTCOLOR", &[], BasicType::Long);
        // _BACKGROUNDCOLOR returns the background color of the current _DEST
        self.register_builtin_function("_BACKGROUNDCOLOR", &[], BasicType::Long);

        // _FULLPATH$ returns the full absolute path of a file/directory
        self.register_builtin_function(
            "_FULLPATH$",
            &[("path", BasicType::String)],
            BasicType::String,
        );

        // _FPS returns/sets frame rate limit (when called as function, returns current FPS)
        self.register_builtin_function("_FPS", &[], BasicType::Double);
    }

    /// Registers audio-related built-in functions.
    fn register_audio_builtins(&mut self) {
        // QB64 Audio Functions
        // _SNDOPEN loads a sound file and returns a handle
        // Optional second param: mode ("SYNC", "STREAM", "VOL", "PAUSE", "NODECODE")
        self.register_builtin_function_with_optionals(
            "_SNDOPEN",
            &[
                ("filename", BasicType::String, false),
                ("mode", BasicType::String, true),
            ],
            BasicType::Long,
        );
        // _SNDOPENRAW opens a raw sound buffer for audio output
        self.register_builtin_function("_SNDOPENRAW", &[], BasicType::Long);
        // _SNDCOPY creates a copy of a sound handle for independent playback
        self.register_builtin_function("_SNDCOPY", &[("handle", BasicType::Long)], BasicType::Long);
        // _SNDPLAYING returns -1 if sound is currently playing, 0 otherwise
        self.register_builtin_function(
            "_SNDPLAYING",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        // _SNDPAUSED returns -1 if sound is paused, 0 otherwise
        self.register_builtin_function(
            "_SNDPAUSED",
            &[("handle", BasicType::Long)],
            BasicType::Integer,
        );
        // _SNDGETPOS returns the current playback position in seconds
        self.register_builtin_function(
            "_SNDGETPOS",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );
        // _SNDLEN returns the total length of the sound in seconds
        self.register_builtin_function(
            "_SNDLEN",
            &[("handle", BasicType::Long)],
            BasicType::Double,
        );
        // _SNDRATE returns the sample rate of a sound (usually 44100)
        self.register_builtin_function("_SNDRATE", &[("handle", BasicType::Long)], BasicType::Long);
        // _SNDRAWLEN returns the amount of queued raw sound data in seconds
        self.register_builtin_function("_SNDRAWLEN", &[], BasicType::Double);
        // _SNDRAWDONE returns -1 if raw sound buffer is empty, 0 otherwise
        self.register_builtin_function("_SNDRAWDONE", &[], BasicType::Integer);
    }

    /// Registers extended QB64 built-in functions.
    fn register_extended_builtins(&mut self) {
        // Hash and encoding functions
        self.register_builtin_function("_CRC32", &[("data", BasicType::String)], BasicType::Long);
        self.register_builtin_function("_MD5$", &[("data", BasicType::String)], BasicType::String);
        self.register_builtin_function("_ADLER32", &[("data", BasicType::String)], BasicType::Long);

        // Base64 encoding/decoding
        self.register_builtin_function(
            "_BASE64ENCODE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_BASE64DECODE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );

        // URL encoding/decoding
        self.register_builtin_function(
            "_ENCODEURL$",
            &[("url", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_DECODEURL$",
            &[("url", BasicType::String)],
            BasicType::String,
        );

        // Compression (zlib deflate/inflate)
        self.register_builtin_function(
            "_DEFLATE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_INFLATE$",
            &[("data", BasicType::String)],
            BasicType::String,
        );

        // Memory extended functions
        self.register_builtin_function(
            "_MEMEXISTS",
            &[("mem", BasicType::Mem)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_MEMELEMENT",
            &[("mem", BasicType::Mem), ("index", BasicType::Offset)],
            BasicType::Mem,
        );
        self.register_builtin_function("_MEMIMAGE", &[("handle", BasicType::Long)], BasicType::Mem);
        self.register_builtin_function("_MEMSOUND", &[("handle", BasicType::Long)], BasicType::Mem);

        // Default color functions
        self.register_builtin_function_with_optionals(
            "_DEFAULTCOLOR",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );
        self.register_builtin_function_with_optionals(
            "_BACKGROUNDCOLOR",
            &[("handle", BasicType::Long, true)],
            BasicType::Long,
        );

        // Short-circuit logical operators (functions)
        // _ANDALSO returns second argument only if first is true
        self.register_builtin_function(
            "_ANDALSO",
            &[("a", BasicType::Long), ("b", BasicType::Long)],
            BasicType::Long,
        );
        // _ORELSE returns second argument only if first is false
        self.register_builtin_function(
            "_ORELSE",
            &[("a", BasicType::Long), ("b", BasicType::Long)],
            BasicType::Long,
        );

        // _FREETIMER - free a timer resource
        self.register_builtin_function("_FREETIMER", &[], BasicType::Long);

        // Console mode functions
        self.register_builtin_function("_CONSOLEINPUT", &[], BasicType::Long);
        self.register_builtin_function("_ECHO", &[("text", BasicType::String)], BasicType::Long);

        // Mouse extended
        self.register_builtin_function("_MOUSEHIDDEN", &[], BasicType::Integer);

        // Clipboard extended - get image from clipboard
        self.register_builtin_function("_CLIPBOARDIMAGE", &[], BasicType::Long);

        // Device input functions (gamepad/joystick)
        self.register_builtin_function("_DEVICES", &[], BasicType::Long);
        self.register_builtin_function("_DEVICE$", &[("n", BasicType::Long)], BasicType::String);
        self.register_builtin_function("_DEVICEINPUT", &[], BasicType::Long);
        self.register_builtin_function(
            "_LASTAXIS",
            &[("device", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_LASTBUTTON",
            &[("device", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_LASTWHEEL",
            &[("device", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_AXIS",
            &[("device", BasicType::Long), ("axis", BasicType::Long)],
            BasicType::Single,
        );
        self.register_builtin_function(
            "_BUTTON",
            &[("device", BasicType::Long), ("button", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_BUTTONCHANGE",
            &[("device", BasicType::Long), ("button", BasicType::Long)],
            BasicType::Integer,
        );
        self.register_builtin_function(
            "_WHEEL",
            &[("device", BasicType::Long), ("wheel", BasicType::Long)],
            BasicType::Single,
        );

        // Drag and drop functions
        self.register_builtin_function("_TOTALDROPPEDFILES", &[], BasicType::Long);
        self.register_builtin_function(
            "_DROPPEDFILE",
            &[("index", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_DROPPEDFILE$",
            &[("index", BasicType::Long)],
            BasicType::String,
        );

        // Resize event functions
        self.register_builtin_function("_RESIZE", &[], BasicType::Integer);
        self.register_builtin_function("_RESIZEWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_RESIZEHEIGHT", &[], BasicType::Long);
        self.register_builtin_function("_SCALEDWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_SCALEDHEIGHT", &[], BasicType::Long);

        // Dialog functions
        self.register_builtin_function_with_optionals(
            "_COLORCHOOSERDIALOG",
            &[
                ("initial_color", BasicType::Long, true),
                ("title", BasicType::String, true),
            ],
            BasicType::Long,
        );

        // HSB color functions (Hue-Saturation-Brightness)
        self.register_builtin_function(
            "_HSB32",
            &[
                ("hue", BasicType::Single),
                ("saturation", BasicType::Single),
                ("brightness", BasicType::Single),
            ],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_HSBA32",
            &[
                ("hue", BasicType::Single),
                ("saturation", BasicType::Single),
                ("brightness", BasicType::Single),
                ("alpha", BasicType::Single),
            ],
            BasicType::Long,
        );
        self.register_builtin_function("_HUE32", &[("color", BasicType::Long)], BasicType::Single);
        self.register_builtin_function(
            "_SATURATION32",
            &[("color", BasicType::Long)],
            BasicType::Single,
        );
        self.register_builtin_function(
            "_BRIGHTNESS32",
            &[("color", BasicType::Long)],
            BasicType::Single,
        );

        // Networking functions
        self.register_builtin_function(
            "_CONNECTIONADDRESS",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_CONNECTIONADDRESS$",
            &[("handle", BasicType::Long)],
            BasicType::String,
        );

        // File I/O extended
        self.register_builtin_function(
            "_FILES$",
            &[("spec", BasicType::String)],
            BasicType::String,
        );
        self.register_builtin_function(
            "_EMBEDDED$",
            &[("name", BasicType::String)],
            BasicType::String,
        );

        // Device input extended
        self.register_builtin_function("_LASTHANDLER", &[], BasicType::Long);

        // Sound creation
        self.register_builtin_function(
            "_SNDNEW",
            &[
                ("frames", BasicType::Long),
                ("channels", BasicType::Long),
                ("bits", BasicType::Long),
            ],
            BasicType::Long,
        );

        // Error handling extended
        self.register_builtin_function("_INCLERRORFILE$", &[], BasicType::String);
        self.register_builtin_function("_INCLERRORLINE", &[], BasicType::Long);
        self.register_builtin_function("_STATUSCODE", &[], BasicType::Long);

        // Unicode mapping - dual purpose:
        // As statement: _MAPUNICODE unicode&, ascii% (sets mapping, returns 0)
        // As function: _MAPUNICODE(ascii%) (gets mapping, returns unicode)
        self.register_builtin_function_with_optionals(
            "_MAPUNICODE",
            &[
                ("unicode_or_ascii", BasicType::Long, false),
                ("ascii", BasicType::Long, true),
            ],
            BasicType::Long,
        );

        // Unicode font functions
        self.register_builtin_function(
            "_UCHARPOS",
            &[("text", BasicType::String), ("pos", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_UFONTHEIGHT",
            &[("handle", BasicType::Long)],
            BasicType::Long,
        );
        self.register_builtin_function(
            "_UPRINTWIDTH",
            &[("text", BasicType::String)],
            BasicType::Long,
        );

        // Graphics rendering mode constants (return constant values)
        self.register_builtin_function("_SMOOTH", &[], BasicType::Long);
        self.register_builtin_function("_SMOOTHSHRUNK", &[], BasicType::Long);
        self.register_builtin_function("_SMOOTHSTRETCHED", &[], BasicType::Long);
        self.register_builtin_function("_HARDWARE", &[], BasicType::Long);
        self.register_builtin_function("_HARDWARE1", &[], BasicType::Long);
        self.register_builtin_function("_SOFTWARE", &[], BasicType::Long);
        self.register_builtin_function("_STRETCH", &[], BasicType::Long);
        self.register_builtin_function("_SEAMLESS", &[], BasicType::Long);
        self.register_builtin_function("_SQUAREPIXELS", &[], BasicType::Long);
        self.register_builtin_function("_BEHIND", &[], BasicType::Long);

        // Graphics direction constants
        self.register_builtin_function("_ANTICLOCKWISE", &[], BasicType::Long);
        self.register_builtin_function("_CLOCKWISE", &[], BasicType::Long);

        // Print mode constants
        self.register_builtin_function("_KEEPBACKGROUND", &[], BasicType::Long);
        self.register_builtin_function("_FILLBACKGROUND", &[], BasicType::Long);
        self.register_builtin_function("_ONLYBACKGROUND", &[], BasicType::Long);

        // Alignment constant
        self.register_builtin_function("_MIDDLE", &[], BasicType::Long);

        // Auto display constant
        self.register_builtin_function("_AUTO", &[], BasicType::Long);

        // Type/mode keyword constants
        self.register_builtin_function("_ALL", &[], BasicType::Long);
        self.register_builtin_function("_BLINK", &[], BasicType::Long);
        self.register_builtin_function("_OFF", &[], BasicType::Long);
        self.register_builtin_function("_ONLY", &[], BasicType::Long);

        // Sound/network mode constants
        self.register_builtin_function("_WAVE", &[], BasicType::Long);
        self.register_builtin_function("_DONTWAIT", &[], BasicType::Long);

        // Console functions
        self.register_builtin_function("_CONSOLETITLE$", &[], BasicType::String);
        self.register_builtin_function("_SCREENBUFFER", &[], BasicType::Long);
        self.register_builtin_function("_SCINKEY$", &[], BasicType::String);

        // Debug/assertion functions
        self.register_builtin_function("_ASSERTERROR$", &[], BasicType::String);
        self.register_builtin_function("_GLCOMPAT", &[], BasicType::Long);

        // Display extended
        self.register_builtin_function("_FULLSCREENSMOOTH", &[], BasicType::Long);
        self.register_builtin_function("_DISPLAYWIDTH", &[], BasicType::Long);
        self.register_builtin_function("_DISPLAYHEIGHT", &[], BasicType::Long);

        // Date/time functions
        self.register_builtin_function("_YEAR", &[], BasicType::Long);
        self.register_builtin_function("_MONTH", &[], BasicType::Long);
        self.register_builtin_function("_DAY", &[], BasicType::Long);
        self.register_builtin_function("_HOUR", &[], BasicType::Long);
        self.register_builtin_function("_MINUTE", &[], BasicType::Long);
        self.register_builtin_function("_SECOND", &[], BasicType::Long);
        self.register_builtin_function("_WEEKDAY", &[], BasicType::Long);

        // Register built-in subs
        self.register_builtin_subs();
    }

    /// Registers built-in SUBs (statements with no return value).
    fn register_builtin_subs(&mut self) {
        // Drag and drop control
        // _ACCEPTFILEDROP can be called with 0 or 1 argument as a statement:
        // _ACCEPTFILEDROP - enable file drop with default settings
        // _ACCEPTFILEDROP ON/OFF - explicitly enable/disable
        self.register_builtin_sub_with_optionals(
            "_ACCEPTFILEDROP",
            &[("enable", BasicType::Integer, true)],
        );
        self.register_builtin_sub("_FINISHDROP", &[]);

        // Console mode statements
        self.register_builtin_sub("_CONSOLECURSOR", &[("visible", BasicType::Integer)]);
        self.register_builtin_sub(
            "_CONSOLEFONT",
            &[("font", BasicType::String), ("size", BasicType::Long)],
        );
        self.register_builtin_sub("_CONTROLCHR", &[("mode", BasicType::Integer)]);

        // Graphics alpha/blending
        self.register_builtin_sub(
            "_SETALPHA",
            &[
                ("alpha", BasicType::Long),
                ("color1", BasicType::Long),
                ("color2", BasicType::Long),
            ],
        );
        self.register_builtin_sub(
            "_COPYPALETTE",
            &[
                ("srcHandle", BasicType::Long),
                ("destHandle", BasicType::Long),
            ],
        );
        self.register_builtin_sub("_BLEND", &[("handle", BasicType::Long)]);
        self.register_builtin_sub("_DONTBLEND", &[("handle", BasicType::Long)]);
        self.register_builtin_sub(
            "_CLEARCOLOR",
            &[("color", BasicType::Long), ("handle", BasicType::Long)],
        );
        self.register_builtin_sub("_DEPTHBUFFER", &[("mode", BasicType::Integer)]);
        self.register_builtin_sub(
            "_DISPLAYORDER",
            &[
                ("layer1", BasicType::Long),
                ("layer2", BasicType::Long),
                ("layer3", BasicType::Long),
                ("layer4", BasicType::Long),
            ],
        );

        // Triangle mapping for texture rendering
        self.register_builtin_sub(
            "_MAPTRIANGLE",
            &[
                ("sx1", BasicType::Single),
                ("sy1", BasicType::Single),
                ("sx2", BasicType::Single),
                ("sy2", BasicType::Single),
                ("sx3", BasicType::Single),
                ("sy3", BasicType::Single),
                ("dx1", BasicType::Single),
                ("dy1", BasicType::Single),
                ("dx2", BasicType::Single),
                ("dy2", BasicType::Single),
                ("dx3", BasicType::Single),
                ("dy3", BasicType::Single),
            ],
        );

        // Sound extended statement
        self.register_builtin_sub(
            "_SNDLIMIT",
            &[("handle", BasicType::Long), ("seconds", BasicType::Single)],
        );

        // Icon statement (set window icon)
        self.register_builtin_sub("_ICON", &[("handle", BasicType::Long)]);

        // Window visibility
        self.register_builtin_sub("_HIDE", &[]);
        self.register_builtin_sub("_SHOW", &[]);
        self.register_builtin_sub("_ONTOP", &[("mode", BasicType::Integer)]);

        // Print mode
        self.register_builtin_sub("_PRINTMODE", &[("mode", BasicType::Integer)]);

        // Unicode print statement (stub for QB64PE parity)
        self.register_builtin_sub(
            "_UPRINTSTRING",
            &[
                ("x", BasicType::Long),
                ("y", BasicType::Long),
                ("text", BasicType::String),
            ],
        );

        // Console statements
        self.register_builtin_sub("_ECHO", &[("text", BasicType::String)]);
        self.register_builtin_sub("_CONSOLETITLE", &[("title", BasicType::String)]);

        // Clipboard write statement
        self.register_builtin_sub("_CLIPBOARD", &[("text", BasicType::String)]);

        // Delay/timing statements
        self.register_builtin_sub("_DELAY", &[("seconds", BasicType::Single)]);

        // Memory statements
        self.register_builtin_sub(
            "_MEMPUT",
            &[
                ("block", BasicType::Offset),
                ("offset", BasicType::Offset),
                ("value", BasicType::Long),
            ],
        );
        self.register_builtin_sub(
            "_MEMFILL",
            &[
                ("block", BasicType::Offset),
                ("offset", BasicType::Offset),
                ("size", BasicType::Long),
                ("value", BasicType::Long),
            ],
        );
        self.register_builtin_sub(
            "_MEMCOPY",
            &[
                ("src", BasicType::Offset),
                ("srcoff", BasicType::Offset),
                ("bytes", BasicType::Long),
                ("dst", BasicType::Offset),
                ("dstoff", BasicType::Offset),
            ],
        );
        self.register_builtin_sub("_MEMFREE", &[("block", BasicType::Offset)]);

        // Window icon statement
        self.register_builtin_sub("_SCREENICON", &[]);

        // Logging statements
        self.register_builtin_sub("_LOGTRACE", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGINFO", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGWARN", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGERROR", &[("message", BasicType::String)]);
        self.register_builtin_sub("_LOGMINLEVEL", &[("level", BasicType::Long)]);

        // Graphics extended statements
        self.register_builtin_sub(
            "_SAVEIMAGE",
            &[("filename", BasicType::String), ("handle", BasicType::Long)],
        );
        self.register_builtin_sub("_SCREENPRINT", &[("text", BasicType::String)]);
        self.register_builtin_sub("_PRINTIMAGE", &[("handle", BasicType::Long)]);
        self.register_builtin_sub("_GLRENDER", &[("mode", BasicType::Long)]);

        // Resource cleanup
        self.register_builtin_sub("_CLEAR", &[("handle", BasicType::Long)]);

        // Toggle statement
        self.register_builtin_sub("_TOGGLE", &[("handle", BasicType::Long)]);

        // Sound extended statements
        self.register_builtin_sub(
            "_SNDRAWBATCH",
            &[
                ("handle", BasicType::Long),
                ("data", BasicType::Long),
                ("frames", BasicType::Long),
            ],
        );
        self.register_builtin_sub("_MIDISOUNDBANK", &[("filename", BasicType::String)]);

        // Device handler statement
        self.register_builtin_sub("_NEWHANDLER", &[("handler", BasicType::Long)]);
    }

    /// Registers a single built-in function.
    fn register_builtin_function(
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
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in function with optional parameters.
    /// Parameters are specified as (name, type, is_optional).
    fn register_builtin_function_with_optionals(
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
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in SUB (procedure with no return value).
    fn register_builtin_sub(&mut self, name: &str, params: &[(&str, BasicType)]) {
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
            return_type: None, // SUBs have no return type
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers a built-in SUB with optional parameters.
    fn register_builtin_sub_with_optionals(
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
            return_type: None, // SUBs have no return type
            span: crate::ast::Span::new(0, 0),
            is_static: false,
        };
        let _ = self.symbols.define_procedure(entry);
    }

    /// Registers built-in constants (_TRUE, _FALSE, etc.).
    ///
    /// In QB64, _TRUE is -1 and _FALSE is 0 (following BASIC tradition where
    /// boolean true is all bits set, i.e., -1 in two's complement).
    fn register_builtin_constants(&mut self) {
        // _TRUE = -1 (all bits set, standard BASIC convention)
        let true_symbol = Symbol {
            name: "_TRUE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(-1),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(true_symbol);

        // _FALSE = 0
        let false_symbol = Symbol {
            name: "_FALSE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(0),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(false_symbol);

        // _NONE = 0 (null/none value for handles, modes, etc.)
        let none_symbol = Symbol {
            name: "_NONE".to_string(),
            kind: SymbolKind::Constant {
                value: ConstValue::Integer(0),
            },
            basic_type: BasicType::Long,
            span: crate::ast::Span::new(0, 0),
            is_mutable: false,
        };
        let _ = self.symbols.define_symbol(none_symbol);

        // Register other constant categories
        self.register_character_constants();
        self.register_string_character_constants();
        self.register_error_constants();
        self.register_platform_constants();
        self.register_keyboard_constants();
    }

    /// Registers ASCII character constants (CHR$(0) through CHR$(31) names).
    fn register_character_constants(&mut self) {
        // Helper to register a character constant
        let mut define_char = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        // Control characters
        define_char("_NUL", 0); // Null
        define_char("_SOH", 1); // Start of Heading
        define_char("_STX", 2); // Start of Text
        define_char("_ETX", 3); // End of Text
        define_char("_EOT", 4); // End of Transmission
        define_char("_ENQ", 5); // Enquiry
        define_char("_ACK", 6); // Acknowledge
        define_char("_BEL", 7); // Bell
        define_char("_BS", 8); // Backspace
        define_char("_HT", 9); // Horizontal Tab
        define_char("_TAB", 9); // Tab (alias)
        define_char("_LF", 10); // Line Feed
        define_char("_VT", 11); // Vertical Tab
        define_char("_FF", 12); // Form Feed
        define_char("_CR", 13); // Carriage Return
        define_char("_SO", 14); // Shift Out
        define_char("_SI", 15); // Shift In
        define_char("_DLE", 16); // Data Link Escape
        define_char("_DC1", 17); // Device Control 1
        define_char("_DC2", 18); // Device Control 2
        define_char("_DC3", 19); // Device Control 3
        define_char("_DC4", 20); // Device Control 4
        define_char("_NAK", 21); // Negative Acknowledge
        define_char("_SYN", 22); // Synchronous Idle
        define_char("_ETB", 23); // End of Trans. Block
        define_char("_CAN", 24); // Cancel
        define_char("_EM", 25); // End of Medium
        define_char("_SUB", 26); // Substitute
        define_char("_ESC", 27); // Escape
        define_char("_FS", 28); // File Separator
        define_char("_GS", 29); // Group Separator
        define_char("_RS", 30); // Record Separator
        define_char("_US", 31); // Unit Separator
        define_char("_DEL", 127); // Delete
    }

    /// Registers _CHR_* string constants (actual character strings, not ASCII codes).
    ///
    /// These are QB64 extensions that provide string constants for common characters.
    /// Unlike the _CR, _LF constants (which are ASCII codes), these are actual
    /// single-character strings.
    fn register_string_character_constants(&mut self) {
        // Helper to register a string character constant
        let mut define_str_char = |name: &str, value: &str| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::String(value.to_string()),
                },
                basic_type: BasicType::String,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        // Control characters as strings
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
        define_str_char("_CHR_TAB", "\t"); // Alias
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

        // Common printable characters
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

        // Common string constants
        define_str_char("_STR_EMPTY", "");
        define_str_char("_STR_CRLF", "\r\n");
        define_str_char("_STR_LF", "\n");
        define_str_char("_STR_CR", "\r");
    }

    /// Registers error code constants (ERR_* values).
    fn register_error_constants(&mut self) {
        // Helper to register an error constant
        let mut define_error = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        // QB45 compatible error codes
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

        // File I/O error codes
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
    ///
    /// Also registers common aliases used in $IF conditions:
    /// - `_WIN` - alias for `_WINDOWS`
    /// - `_MAC` - alias for `_MACOSX`
    /// - `_64BIT` - TRUE (-1) on 64-bit platforms
    /// - `_32BIT` - TRUE (-1) on 32-bit platforms
    fn register_platform_constants(&mut self) {
        // Helper to register a platform constant
        let mut define_platform = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        // Platform constants (use actual values based on current platform)
        #[cfg(target_os = "windows")]
        {
            define_platform("_WINDOWS", -1); // TRUE
            define_platform("_WIN", -1); // Alias
            define_platform("_LINUX", 0);
            define_platform("_MACOSX", 0);
            define_platform("_MAC", 0); // Alias
        }

        #[cfg(target_os = "linux")]
        {
            define_platform("_WINDOWS", 0);
            define_platform("_WIN", 0); // Alias
            define_platform("_LINUX", -1); // TRUE
            define_platform("_MACOSX", 0);
            define_platform("_MAC", 0); // Alias
        }

        #[cfg(target_os = "macos")]
        {
            define_platform("_WINDOWS", 0);
            define_platform("_WIN", 0); // Alias
            define_platform("_LINUX", 0);
            define_platform("_MACOSX", -1); // TRUE
            define_platform("_MAC", -1); // Alias
        }

        #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
        {
            define_platform("_WINDOWS", 0);
            define_platform("_WIN", 0); // Alias
            define_platform("_LINUX", 0);
            define_platform("_MACOSX", 0);
            define_platform("_MAC", 0); // Alias
        }

        // Architecture constants (based on pointer size)
        #[cfg(target_pointer_width = "64")]
        {
            define_platform("_64BIT", -1); // TRUE
            define_platform("_32BIT", 0);
        }

        #[cfg(target_pointer_width = "32")]
        {
            define_platform("_64BIT", 0);
            define_platform("_32BIT", -1); // TRUE
        }

        #[cfg(not(any(target_pointer_width = "64", target_pointer_width = "32")))]
        {
            // Rare case: neither 32 nor 64 bit
            define_platform("_64BIT", 0);
            define_platform("_32BIT", 0);
        }
    }

    /// Registers keyboard scan code constants (_KEY_*).
    fn register_keyboard_constants(&mut self) {
        // Helper to register a keyboard constant
        let mut define_key = |name: &str, value: i64| {
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Constant {
                    value: ConstValue::Integer(value),
                },
                basic_type: BasicType::Long,
                span: crate::ast::Span::new(0, 0),
                is_mutable: false,
            };
            let _ = self.symbols.define_symbol(symbol);
        };

        // Keyboard constants (SDL2 key codes, matching QB64)
        // Function keys
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

        // Navigation keys
        define_key("_KEY_HOME", 18176);
        define_key("_KEY_END", 20224);
        define_key("_KEY_PAGEUP", 18688);
        define_key("_KEY_PAGEDOWN", 20736);
        define_key("_KEY_INSERT", 20992);
        define_key("_KEY_DELETE", 21248);

        // Arrow keys
        define_key("_KEY_UP", 18432);
        define_key("_KEY_DOWN", 20480);
        define_key("_KEY_LEFT", 19200);
        define_key("_KEY_RIGHT", 19712);

        // Modifier keys
        define_key("_KEY_LSHIFT", 100304);
        define_key("_KEY_RSHIFT", 100303);
        define_key("_KEY_LCTRL", 100306);
        define_key("_KEY_RCTRL", 100305);
        define_key("_KEY_LALT", 100308);
        define_key("_KEY_RALT", 100307);
        define_key("_KEY_CAPSLOCK", 100301);
        define_key("_KEY_NUMLOCK", 100300);
        define_key("_KEY_SCROLLLOCK", 100302);

        // Print Screen and Pause
        define_key("_KEY_PRINT", 100316);
        define_key("_KEY_PAUSE", 100319);
    }
}
