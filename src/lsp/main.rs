//! QB64Fresh Language Server - Entry Point
//!
//! This is the main entry point for the LSP server. It sets up the
//! tower-lsp service and runs it over stdio.
//!
//! # Usage
//!
//! The LSP server is typically started by an editor/IDE:
//!
//! ```bash
//! qb64fresh-lsp
//! ```
//!
//! For debugging, you can run with logging:
//!
//! ```bash
//! RUST_LOG=debug qb64fresh-lsp 2>lsp.log
//! ```

use tower_lsp::{LspService, Server};

// Import from the library crate
use qb64fresh::lsp::QbLanguageServer;

#[tokio::main]
async fn main() {
    // Initialize logging (writes to stderr so it doesn't interfere with LSP)
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .target(env_logger::Target::Stderr)
        .init();

    log::info!(
        "Starting QB64Fresh LSP server v{}",
        env!("CARGO_PKG_VERSION")
    );

    // Create the LSP service
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(QbLanguageServer::new);

    // Run the server
    Server::new(stdin, stdout, socket).serve(service).await;
}
