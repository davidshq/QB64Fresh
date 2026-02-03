//! DAP server implementation (stub).
//!
//! Listens for DAP requests over stdio (or TCP) and dispatches to handlers.
//! Full implementation requires runtime integration (breakpoints, variable access).

use crate::config::DebugConfig;
use crate::protocol::{RpcRequest, RpcResponse};
use crate::DebugResult;
use std::io::{BufRead, BufReader, Write};

/// DAP server state.
pub struct DapServer {
    /// Loaded config (breakpoints, launch options).
    pub config: DebugConfig,
}

impl DapServer {
    /// Create a new server with the given config.
    pub fn new(config: DebugConfig) -> Self {
        Self { config }
    }

    /// Run the server loop reading JSON-RPC from stdin and writing to stdout.
    /// Stub: responds to initialize and returns "not implemented" for others.
    pub fn run_stdio(&mut self) -> DebugResult<()> {
        let stdin = std::io::stdin();
        let mut stdout = std::io::stdout();
        let reader = BufReader::new(stdin);

        for line in reader.lines() {
            let line = line.map_err(|e| crate::DebugError::ProtocolError {
                message: format!("stdin read error: {}", e),
            })?;
            if line.is_empty() {
                continue;
            }
            let request: RpcRequest =
                serde_json::from_str(&line).map_err(|e| crate::DebugError::ProtocolError {
                    message: format!("invalid JSON-RPC: {}", e),
                })?;
            let response = self.handle_request(request)?;
            let out =
                serde_json::to_string(&response).map_err(|e| crate::DebugError::ProtocolError {
                    message: format!("serialize response: {}", e),
                })?;
            writeln!(stdout, "{}", out).map_err(|e| crate::DebugError::ReadError {
                path: std::path::PathBuf::from("<stdout>"),
                source: e,
            })?;
            stdout.flush().map_err(|e| crate::DebugError::ReadError {
                path: std::path::PathBuf::from("<stdout>"),
                source: e,
            })?;
        }
        Ok(())
    }

    /// Handle a single JSON-RPC request (stub implementation).
    fn handle_request(&mut self, request: RpcRequest) -> DebugResult<RpcResponse> {
        let id = request.id.clone();
        let result = match request.method.as_str() {
            "initialize" => {
                let body = serde_json::json!({
                    "supportsConfigurationDoneRequest": true,
                    "supportsSetBreakpointsRequest": true,
                });
                Some(body)
            }
            _ => {
                return Err(crate::DebugError::NotImplemented {
                    message: format!("DAP method '{}' not yet implemented", request.method),
                });
            }
        };
        Ok(RpcResponse {
            jsonrpc: "2.0".to_string(),
            result,
            error: None,
            id,
        })
    }
}
