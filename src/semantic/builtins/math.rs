//! Math and numeric built-in function registration.
//!
//! Registers ABS, SGN, INT, FIX, CINT, CLNG, CSNG, CDBL, SQR, LOG, EXP,
//! SIN, COS, TAN, ATN, RND, _PI, _ASIN, _ACOS, _ATAN2, _HYPOT, _CEIL,
//! _ROUND, _MIN, _MAX, _CLAMP, hyperbolic and reciprocal trig, angle
//! conversions, and _NEGATE.

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

/// Registers math-related built-in functions.
pub(super) fn register_math_builtins(analyzer: &mut SemanticAnalyzer) {
    analyzer.register_builtin_function("ABS", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("SGN", &[("n", BasicType::Double)], BasicType::Integer);
    analyzer.register_builtin_function("INT", &[("n", BasicType::Double)], BasicType::Long);
    analyzer.register_builtin_function("FIX", &[("n", BasicType::Double)], BasicType::Long);
    analyzer.register_builtin_function("CINT", &[("n", BasicType::Double)], BasicType::Integer);
    analyzer.register_builtin_function("CLNG", &[("n", BasicType::Double)], BasicType::Long);
    analyzer.register_builtin_function("CSNG", &[("n", BasicType::Double)], BasicType::Single);
    analyzer.register_builtin_function("CDBL", &[("n", BasicType::Single)], BasicType::Double);
    analyzer.register_builtin_function("SQR", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("LOG", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("EXP", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("SIN", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("COS", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("TAN", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("ATN", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function_with_optionals(
        "RND",
        &[("n", BasicType::Single, true)],
        BasicType::Single,
    );

    // QB64 extended math
    analyzer.register_builtin_function("_PI", &[], BasicType::Double);
    analyzer.register_builtin_function("_ASIN", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ACOS", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function(
        "_ATAN2",
        &[("y", BasicType::Double), ("x", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_HYPOT",
        &[("x", BasicType::Double), ("y", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function("_CEIL", &[("n", BasicType::Double)], BasicType::Long);
    analyzer.register_builtin_function("_ROUND", &[("n", BasicType::Double)], BasicType::Long);
    analyzer.register_builtin_function(
        "_MIN",
        &[("a", BasicType::Double), ("b", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_MAX",
        &[("a", BasicType::Double), ("b", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_CLAMP",
        &[
            ("value", BasicType::Double),
            ("min", BasicType::Double),
            ("max", BasicType::Double),
        ],
        BasicType::Double,
    );

    // Hyperbolic
    analyzer.register_builtin_function("_SINH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_COSH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_TANH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ASINH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ACOSH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ATANH", &[("n", BasicType::Double)], BasicType::Double);

    // Reciprocal trig
    analyzer.register_builtin_function("_SEC", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_CSC", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_COT", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_SECH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_CSCH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_COTH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ARCSEC", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ARCCSC", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ARCCOT", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ARCSECH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ARCCSCH", &[("n", BasicType::Double)], BasicType::Double);
    analyzer.register_builtin_function("_ARCCOTH", &[("n", BasicType::Double)], BasicType::Double);

    // Angle conversions
    analyzer.register_builtin_function(
        "_D2R",
        &[("degrees", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_R2D",
        &[("radians", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_D2G",
        &[("degrees", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_G2D",
        &[("gradians", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_G2R",
        &[("gradians", BasicType::Double)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_R2G",
        &[("radians", BasicType::Double)],
        BasicType::Double,
    );

    analyzer.register_builtin_function("_NEGATE", &[("n", BasicType::Double)], BasicType::Double);
}
