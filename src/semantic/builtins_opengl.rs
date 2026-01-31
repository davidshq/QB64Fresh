//! OpenGL (_GL*) built-in procedures, functions, and constants.
//!
//! This module registers QB64pe-style OpenGL built-ins so that programs
//! using SUB _GL and _GL* commands resolve correctly. _GL* commands are
//! only valid inside SUB _GL (enforced in the semantic checker).
//!
//! GL constants (590) and _GL* functions (335 from gl.h) are generated from
//! QB64pe's gl.h by `scripts/gl_parse_header.py`. _GLUPERSPECTIVE (GLU) is
//! hand-added; _GLRENDER and _GLCOMPAT are registered in builtins.rs.

use super::SemanticAnalyzer;
use super::types::BasicType;

include!("builtins_opengl_constants.rs");

impl SemanticAnalyzer {
    /// Registers OpenGL built-in subs, functions, and constants.
    ///
    /// Called from `register_builtins()` so that _GL* names resolve. The
    /// semantic checker enforces that _GL* (except _GLRENDER and _GLCOMPAT)
    /// are only used inside SUB _GL.
    pub(super) fn register_builtins_opengl(&mut self) {
        // --- GL constants (590 from gl.h) + _GL_* aliases (QB64pe allows both) ---
        for (name, value) in GL_CONSTANTS {
            self.register_builtin_constant(name, *value);
            self.register_builtin_constant(&format!("_{}", name), *value);
        }

        // --- OpenGL functions/subs from gl.h (335) ---
        super::builtins_opengl_functions::register_gl_functions(self);

        // --- GLU (single convenience function; not in gl.h) ---
        self.register_builtin_sub(
            "_GLUPERSPECTIVE",
            &[
                ("fovy", BasicType::Single),
                ("aspect", BasicType::Single),
                ("zNear", BasicType::Single),
                ("zFar", BasicType::Single),
            ],
        );
    }
}
