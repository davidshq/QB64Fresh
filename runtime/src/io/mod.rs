//! I/O module for QB64Fresh Runtime
//!
//! This module is split into focused submodules:
//! - [`print`] - PRINT functions
//! - [`input`] - INPUT functions and keyboard handling
//! - [`file`] - File I/O operations

pub mod file;
pub mod input;
pub mod print;

// Re-export all public items for backward compatibility
pub use file::*;
pub use input::*;
pub use print::*;
