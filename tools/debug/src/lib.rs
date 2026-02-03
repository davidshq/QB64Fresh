//! # QB64Fresh Debugger
//!
//! AST-level debugger and Debug Adapter Protocol (DAP) implementation for
//! QB64Fresh BASIC programs. Supports breakpoints (config/TOML), CLI, and
//! stub DAP server; full runtime integration (variable access, execution
//! control) is planned.
//!
//! ## Modules
//!
//! - [`config`] - TOML/config and breakpoint settings
//! - [`dap`] - DAP message types and capabilities
//! - [`frames`] - Call stack frame structures
//! - [`protocol`] - JSON-RPC envelope types
//! - [`server`] - DAP server loop (stdio)
//! - [`sources`] - Multi-file source management
//! - [`symbols`] - Debug symbol extraction from AST
//! - [`values`] - Variable value representation for DAP
//! - [`watch`] - Watch expression parsing

mod config;
mod dap;
mod error;
mod frames;
mod protocol;
mod server;
mod sources;
mod symbols;
mod values;
mod watch;

pub use config::{BreakpointsConfig, DebugConfig, LaunchConfig, ServerConfig};
pub use dap::{default_capabilities, Capabilities, InitializeRequest, DAP_VERSION};
pub use error::{DebugError, DebugResult};
pub use frames::StackFrame;
pub use protocol::{RpcError, RpcRequest, RpcResponse};
pub use server::DapServer;
pub use sources::SourceManager;
pub use symbols::{extract_symbols, DebugSymbols};
pub use values::DebugValue;
pub use watch::{parse_watch, WatchExpr};
