//! Debug Adapter Protocol (DAP) types and message handling.
//!
//! Types and (stub) handling for the DAP JSON-RPC protocol used by
//! IDEs (e.g. VS Code) to communicate with the debugger.

use serde::{Deserialize, Serialize};

/// DAP protocol version we advertise.
pub const DAP_VERSION: &str = "1.0.0";

/// Initialize request (sent by client at startup).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeRequest {
    /// Client name (e.g. "vscode").
    pub client_name: Option<String>,
    /// Client ID (e.g. "vscode").
    pub client_id: Option<String>,
    /// Adapter ID (e.g. "qb64fresh").
    pub adapter_id: Option<String>,
}

/// Initialize response (capabilities we support).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Capabilities {
    /// Supports configurationDoneRequest.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_configuration_done_request: Option<bool>,
    /// Supports setBreakpointsRequest.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_set_breakpoints_request: Option<bool>,
    /// Supports breakpoint locations.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_breakpoint_locations_request: Option<bool>,
}

/// Stub: build capabilities for initialize response.
pub fn default_capabilities() -> Capabilities {
    Capabilities {
        supports_configuration_done_request: Some(true),
        supports_set_breakpoints_request: Some(true),
        supports_breakpoint_locations_request: Some(false),
    }
}
