//! I/O, console, timer, and environment built-in function registration.
//!
//! Registers TAB, SPC, POS, CSRLIN, SCREEN, EOF, LOF, LOC, SEEK, FREEFILE,
//! INKEY$, INPUT$, keyboard (_KEYHIT, _KEYDOWN, etc.), error (ERR, ERL),
//! environment (ENVIRON$, COMMAND$, _CWD$, etc.), and timer/date (TIMER,
//! DATE$, TIME$, _DATE$, _TIME$).

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

/// Registers I/O and environment built-in functions.
pub(super) fn register_io_builtins(analyzer: &mut SemanticAnalyzer) {
    // Timer/Date
    analyzer.register_builtin_function_with_optionals(
        "TIMER",
        &[("accuracy", BasicType::Single, true)],
        BasicType::Single,
    );
    analyzer.register_builtin_function("DATE$", &[], BasicType::String);
    analyzer.register_builtin_function("TIME$", &[], BasicType::String);

    // Print formatting
    analyzer.register_builtin_function("TAB", &[("n", BasicType::Long)], BasicType::String);
    analyzer.register_builtin_function("SPC", &[("n", BasicType::Long)], BasicType::String);
    analyzer.register_builtin_function("POS", &[("n", BasicType::Long)], BasicType::Integer);
    analyzer.register_builtin_function("CSRLIN", &[], BasicType::Integer);

    // SCREEN function
    analyzer.register_builtin_function_with_optionals(
        "SCREEN",
        &[
            ("row", BasicType::Integer, false),
            ("col", BasicType::Integer, false),
            ("flag", BasicType::Integer, true),
        ],
        BasicType::Integer,
    );

    // File I/O functions
    analyzer.register_builtin_function("EOF", &[("fnum", BasicType::Integer)], BasicType::Integer);
    analyzer.register_builtin_function("LOF", &[("fnum", BasicType::Integer)], BasicType::Long);
    analyzer.register_builtin_function("LOC", &[("fnum", BasicType::Integer)], BasicType::Long);
    analyzer.register_builtin_function("SEEK", &[("fnum", BasicType::Integer)], BasicType::Long);
    analyzer.register_builtin_function("FREEFILE", &[], BasicType::Integer);

    // Keyboard input
    analyzer.register_builtin_function("INKEY$", &[], BasicType::String);
    analyzer.register_builtin_function_with_optionals(
        "INPUT$",
        &[
            ("n", BasicType::Integer, false),
            ("filenum", BasicType::Integer, true),
        ],
        BasicType::String,
    );

    // QB64 keyboard extensions
    analyzer.register_builtin_function("_KEYHIT", &[], BasicType::Long);
    analyzer.register_builtin_function("_KEYDOWN", &[("code", BasicType::Long)], BasicType::Long);
    analyzer.register_builtin_function("_CINP", &[], BasicType::Long);
    analyzer.register_builtin_function("_CAPSLOCK", &[], BasicType::Long);
    analyzer.register_builtin_function("_NUMLOCK", &[], BasicType::Long);
    analyzer.register_builtin_function("_SCROLLLOCK", &[], BasicType::Long);

    // Error handling
    analyzer.register_builtin_function("ERR", &[], BasicType::Integer);
    analyzer.register_builtin_function("ERL", &[], BasicType::Integer);
    analyzer.register_builtin_function("_ERRORLINE", &[], BasicType::Long);
    analyzer.register_builtin_function("_ERRORMESSAGE$", &[], BasicType::String);

    // Utility
    analyzer.register_builtin_function("_COMMANDCOUNT", &[], BasicType::Long);
    analyzer.register_builtin_function("_ENVIRONCOUNT", &[], BasicType::Long);

    // Environment
    analyzer.register_builtin_function(
        "ENVIRON$",
        &[("var_or_index", BasicType::Unknown)],
        BasicType::String,
    );
    analyzer.register_builtin_function_with_optionals(
        "COMMAND$",
        &[("index", BasicType::Long, true)],
        BasicType::String,
    );
    analyzer.register_builtin_function("_CWD$", &[], BasicType::String);
    analyzer.register_builtin_function("_OS$", &[], BasicType::String);
    analyzer.register_builtin_function("_STARTDIR$", &[], BasicType::String);
    analyzer.register_builtin_function("Version$", &[], BasicType::String);

    // Phase 2: QB64 Date/Time enhancements
    analyzer.register_builtin_function("_DATE$", &[], BasicType::String);
    analyzer.register_builtin_function("_TIME$", &[], BasicType::String);
}
