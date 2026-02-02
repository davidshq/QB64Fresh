//! Miscellaneous built-in function registration.
//!
//! Registers bitwise (_SHL, _SHR, etc.), _IIF/_IIF$, array (LBOUND, UBOUND),
//! memory (_MEMNEW, _MEMFREE, etc.), system (_FILEEXISTS, SHELL, etc.),
//! mouse (_MOUSEX, _MOUSEY, etc.), and clipboard (_CLIPBOARD$).

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

/// Registers miscellaneous built-in functions.
pub(super) fn register_misc_builtins(analyzer: &mut SemanticAnalyzer) {
    // Bitwise operations
    analyzer.register_builtin_function(
        "_SHL",
        &[("value", BasicType::Long), ("bits", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_SHR",
        &[("value", BasicType::Long), ("bits", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_ROL",
        &[("value", BasicType::Long), ("bits", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_ROR",
        &[("value", BasicType::Long), ("bits", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_READBIT",
        &[("value", BasicType::Long), ("bit", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_SETBIT",
        &[("value", BasicType::Long), ("bit", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_RESETBIT",
        &[("value", BasicType::Long), ("bit", BasicType::Long)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_TOGGLEBIT",
        &[("value", BasicType::Long), ("bit", BasicType::Long)],
        BasicType::Long,
    );

    // Inline conditional
    analyzer.register_builtin_function(
        "_IIF",
        &[
            ("cond", BasicType::Long),
            ("true_val", BasicType::Double),
            ("false_val", BasicType::Double),
        ],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_IIF$",
        &[
            ("cond", BasicType::Long),
            ("true_val", BasicType::String),
            ("false_val", BasicType::String),
        ],
        BasicType::String,
    );

    // Array functions
    analyzer.register_builtin_function_with_optionals(
        "LBOUND",
        &[
            ("arr", BasicType::Unknown, false),
            ("dimension", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function_with_optionals(
        "UBOUND",
        &[
            ("arr", BasicType::Unknown, false),
            ("dimension", BasicType::Long, true),
        ],
        BasicType::Long,
    );

    // Phase 2: Memory operations
    analyzer.register_builtin_function("_MEMNEW", &[("size", BasicType::Offset)], BasicType::Mem);
    analyzer.register_builtin_function("_MEMFREE", &[("mem", BasicType::Mem)], BasicType::Void);
    analyzer.register_builtin_function(
        "_MEMGET",
        &[("mem", BasicType::Mem), ("offset", BasicType::Offset)],
        BasicType::Unknown,
    );
    analyzer.register_builtin_function(
        "_MEMPUT",
        &[
            ("mem", BasicType::Mem),
            ("offset", BasicType::Offset),
            ("value", BasicType::Unknown),
        ],
        BasicType::Void,
    );
    analyzer.register_builtin_function(
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
    analyzer.register_builtin_function(
        "_MEMFILL",
        &[
            ("mem", BasicType::Mem),
            ("offset", BasicType::Offset),
            ("size", BasicType::Offset),
            ("value", BasicType::Unknown),
        ],
        BasicType::Void,
    );
    analyzer.register_builtin_function(
        "_OFFSET",
        &[("variable", BasicType::Unknown)],
        BasicType::Offset,
    );
    analyzer.register_builtin_function("_MEM", &[("variable", BasicType::Unknown)], BasicType::Mem);

    // Phase 5: System Integration
    analyzer.register_builtin_function(
        "_FILEEXISTS",
        &[("path", BasicType::String)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function(
        "_DIREXISTS",
        &[("path", BasicType::String)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function("_DIR$", &[("spec", BasicType::String)], BasicType::String);
    analyzer.register_builtin_function("SHELL", &[("command", BasicType::String)], BasicType::Long);
    analyzer.register_builtin_function(
        "_SHELLHIDE",
        &[("command", BasicType::String)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_READFILE$",
        &[("path", BasicType::String)],
        BasicType::String,
    );
    analyzer.register_builtin_sub(
        "_WRITEFILE",
        &[("path", BasicType::String), ("content", BasicType::String)],
    );

    // Phase 5: Mouse Input
    analyzer.register_builtin_function("_MOUSEX", &[], BasicType::Integer);
    analyzer.register_builtin_function("_MOUSEY", &[], BasicType::Integer);
    analyzer.register_builtin_function(
        "_MOUSEBUTTON",
        &[("button", BasicType::Integer)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function("_MOUSEINPUT", &[], BasicType::Integer);
    analyzer.register_builtin_function("_MOUSEMOVEMENTX", &[], BasicType::Integer);
    analyzer.register_builtin_function("_MOUSEMOVEMENTY", &[], BasicType::Integer);
    analyzer.register_builtin_function("_MOUSEWHEEL", &[], BasicType::Integer);

    // Phase 5: Clipboard
    analyzer.register_builtin_function("_CLIPBOARD$", &[], BasicType::String);
}
