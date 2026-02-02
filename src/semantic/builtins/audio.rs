//! Audio built-in function registration.
//!
//! Registers _SNDOPEN, _SNDOPENRAW, _SNDCOPY, _SNDPLAYING, _SNDPAUSED,
//! _SNDGETPOS, _SNDLEN, _SNDRATE, _SNDRAWLEN, _SNDRAWDONE.

use crate::semantic::types::BasicType;
use crate::semantic::SemanticAnalyzer;

/// Registers audio-related built-in functions.
pub(super) fn register_audio_builtins(analyzer: &mut SemanticAnalyzer) {
    analyzer.register_builtin_function_with_optionals(
        "_SNDOPEN",
        &[
            ("file", BasicType::String, false),
            ("mode", BasicType::String, true),
        ],
        BasicType::Long,
    );
    analyzer.register_builtin_function("_SNDOPENRAW", &[], BasicType::Long);
    analyzer.register_builtin_function("_SNDCOPY", &[("handle", BasicType::Long)], BasicType::Long);
    analyzer.register_builtin_function(
        "_SNDPLAYING",
        &[("handle", BasicType::Long)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function(
        "_SNDPAUSED",
        &[("handle", BasicType::Long)],
        BasicType::Integer,
    );
    analyzer.register_builtin_function(
        "_SNDGETPOS",
        &[("handle", BasicType::Long)],
        BasicType::Double,
    );
    analyzer.register_builtin_function(
        "_SNDLEN",
        &[("handle", BasicType::Long)],
        BasicType::Double,
    );
    analyzer.register_builtin_function("_SNDRATE", &[("handle", BasicType::Long)], BasicType::Long);
    analyzer.register_builtin_function("_SNDRAWLEN", &[], BasicType::Double);
    analyzer.register_builtin_function("_SNDRAWDONE", &[], BasicType::Integer);
}
