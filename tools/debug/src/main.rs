//! QB64Fresh debugger CLI and DAP server entry point.
//!
//! Run as a DAP server (stdio) for IDE integration, or use CLI commands
//! for breakpoints and stub execution.

use clap::{Parser, Subcommand};
use qb64fresh_debug::{DapServer, DebugConfig};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "qb64fresh-debug")]
#[command(about = "Debug adapter and CLI for QB64Fresh BASIC programs", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Config file path (default: .qb64debug or qb64debug.toml in current dir).
    #[arg(short, long, global = true)]
    config: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run the DAP server over stdio (for IDE debug adapter).
    Server,

    /// Print extracted symbols (procedures, globals) for a .bas file.
    Symbols {
        /// Path to the .bas file.
        file: PathBuf,
    },

    /// Validate config file and print breakpoints.
    Check,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let config_path = cli.config.or_else(|| {
        [PathBuf::from(".qb64debug"), PathBuf::from("qb64debug.toml")]
            .into_iter()
            .find(|p| p.exists())
    });

    let config = config_path
        .as_ref()
        .map(|p| DebugConfig::load(p))
        .transpose()?
        .unwrap_or_default();

    match cli.command {
        None | Some(Commands::Server) => {
            let mut server = DapServer::new(config);
            server.run_stdio()?;
        }
        Some(Commands::Symbols { file }) => {
            let source = std::fs::read_to_string(&file).map_err(|e| {
                qb64fresh_debug::DebugError::ReadError {
                    path: file.clone(),
                    source: e,
                }
            })?;
            let symbols = qb64fresh_debug::extract_symbols(&source, Some(&file))?;
            println!("Procedures: {:?}", symbols.procedures);
            println!("Globals: {:?}", symbols.globals);
        }
        Some(Commands::Check) => match config_path {
            Some(p) => println!(
                "Config: {} (breakpoints: {:?})",
                p.display(),
                config.breakpoints
            ),
            None => println!("No config file found; using defaults."),
        },
    }

    Ok(())
}
