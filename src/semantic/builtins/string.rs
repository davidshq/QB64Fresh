//! String and conversion built-in function registration.
//!
//! Registers LEN, CHR$, ASC, LEFT$, RIGHT$, MID$, INSTR, UCASE$, LCASE$,
//! LTRIM$, RTRIM$, TRIM$, STR$, VAL, STRING$, SPACE$, _STRCMP, _STRICMP,
//! _INSTRREV, _TRIM$, MKI$/MKL$/MKS$/MKD$, CVI/CVL/CVS/CVD, HEX$, OCT$, _BIN$, _TOSTR$.

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

/// Registers string-related and string-conversion built-in functions.
pub(super) fn register_string_builtins(analyzer: &mut SemanticAnalyzer) {
    // LEN() can return the length of a string OR the size of a UDT/fixed-length type
    analyzer.register_builtin_function("LEN", &[("s", BasicType::Unknown)], BasicType::Long);
    analyzer.register_builtin_function("CHR$", &[("n", BasicType::Long)], BasicType::String);
    analyzer.register_builtin_function_with_optionals(
        "ASC",
        &[
            ("s", BasicType::String, false),
            ("position", BasicType::Long, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "LEFT$",
        &[("s", BasicType::String), ("n", BasicType::Long)],
        BasicType::String,
    );
    analyzer.register_builtin_function(
        "RIGHT$",
        &[("s", BasicType::String), ("n", BasicType::Long)],
        BasicType::String,
    );
    analyzer.register_builtin_function_with_optionals(
        "MID$",
        &[
            ("s", BasicType::String, false),
            ("start", BasicType::Long, false),
            ("len", BasicType::Long, true),
        ],
        BasicType::String,
    );
    analyzer.register_builtin_function_with_optionals(
        "INSTR",
        &[
            ("start_or_string", BasicType::Unknown, false),
            ("string_or_find", BasicType::String, false),
            ("find", BasicType::String, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function("UCASE$", &[("s", BasicType::String)], BasicType::String);
    analyzer.register_builtin_function("LCASE$", &[("s", BasicType::String)], BasicType::String);
    analyzer.register_builtin_function("LTRIM$", &[("s", BasicType::String)], BasicType::String);
    analyzer.register_builtin_function("RTRIM$", &[("s", BasicType::String)], BasicType::String);
    analyzer.register_builtin_function("TRIM$", &[("s", BasicType::String)], BasicType::String);
    analyzer.register_builtin_function("STR$", &[("n", BasicType::Double)], BasicType::String);
    analyzer.register_builtin_function("VAL", &[("s", BasicType::String)], BasicType::Double);
    analyzer.register_builtin_function(
        "STRING$",
        &[("n", BasicType::Long), ("c", BasicType::Unknown)],
        BasicType::String,
    );
    analyzer.register_builtin_function("SPACE$", &[("n", BasicType::Long)], BasicType::String);

    // String comparison
    analyzer.register_builtin_function(
        "_STRCMP",
        &[("a", BasicType::String), ("b", BasicType::String)],
        BasicType::Long,
    );
    analyzer.register_builtin_function(
        "_STRICMP",
        &[("a", BasicType::String), ("b", BasicType::String)],
        BasicType::Long,
    );

    // Phase 2: String Enhancements
    analyzer.register_builtin_function_with_optionals(
        "_INSTRREV",
        &[
            ("start_or_source", BasicType::Unknown, false),
            ("source_or_search", BasicType::String, false),
            ("search", BasicType::String, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_TRIM$", &[("s", BasicType::String)], BasicType::String);

    // Binary string packing/unpacking
    analyzer.register_builtin_function("MKI$", &[("n", BasicType::Integer)], BasicType::String);
    analyzer.register_builtin_function("MKL$", &[("n", BasicType::Long)], BasicType::String);
    analyzer.register_builtin_function("MKS$", &[("n", BasicType::Single)], BasicType::String);
    analyzer.register_builtin_function("MKD$", &[("n", BasicType::Double)], BasicType::String);
    analyzer.register_builtin_function("CVI", &[("s", BasicType::String)], BasicType::Integer);
    analyzer.register_builtin_function("CVL", &[("s", BasicType::String)], BasicType::Long);
    analyzer.register_builtin_function("CVS", &[("s", BasicType::String)], BasicType::Single);
    analyzer.register_builtin_function("CVD", &[("s", BasicType::String)], BasicType::Double);

    // Type conversion (HEX$/OCT$/_BIN$)
    analyzer.register_builtin_function("HEX$", &[("n", BasicType::Unknown)], BasicType::String);
    analyzer.register_builtin_function("OCT$", &[("n", BasicType::Unknown)], BasicType::String);
    analyzer.register_builtin_function("_BIN$", &[("n", BasicType::Unknown)], BasicType::String);
    analyzer.register_builtin_function("_TOSTR$", &[("n", BasicType::Double)], BasicType::String);
}
