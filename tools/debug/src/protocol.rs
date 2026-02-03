//! DAP JSON-RPC protocol helpers.
//!
//! Request/response envelope types and serialization for the
//! Debug Adapter Protocol over stdio or TCP.

use serde::{Deserialize, Serialize};

/// JSON-RPC 2.0 request envelope (from client).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpcRequest {
    /// JSON-RPC version.
    pub jsonrpc: String,

    /// Method name (e.g. "initialize", "launch").
    pub method: String,

    /// Optional parameters (method-specific).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<serde_json::Value>,

    /// Request ID for matching response.
    pub id: Option<serde_json::Value>,
}

/// JSON-RPC 2.0 response envelope (to client).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpcResponse {
    /// JSON-RPC version.
    pub jsonrpc: String,

    /// Result payload (when success).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,

    /// Error payload (when failure).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<RpcError>,

    /// Request ID matching the request.
    pub id: Option<serde_json::Value>,
}

/// JSON-RPC error object.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpcError {
    /// Error code (e.g. -32600 for Invalid Request).
    pub code: i32,

    /// Human-readable message.
    pub message: String,

    /// Optional additional data.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,
}
