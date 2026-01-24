//! QB64Fresh Debugger CLI
//!
//! A command-line tool for debugging QB64/QBasic BASIC programs.
//!
//! ## Usage
//!
//! ```bash
//! # Debug a program interactively
//! qb64fresh-debug myprogram.bas
//!
//! # Debug with breakpoint set at startup
//! qb64fresh-debug --break main myprogram.bas
//!
//! # Start in DAP mode for IDE integration
//! qb64fresh-debug --dap --port 4711
//!
//! # Run with verbose output
//! qb64fresh-debug --verbose myprogram.bas
//! ```

use clap::{Parser, ValueEnum};
use qb64fresh_debug::{
    Breakpoint, BreakpointKind, DebugConfig, Debugger, ExecutionState, Verbosity,
};
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::process::ExitCode;

/// QB64Fresh debugger - debug BASIC programs interactively
#[derive(Parser, Debug)]
#[command(name = "qb64fresh-debug")]
#[command(author, version, about, long_about = None)]
struct Args {
    /// BASIC source file to debug
    #[arg(required_unless_present_any = ["dap", "list_commands"])]
    file: Option<PathBuf>,

    /// Arguments to pass to the program
    #[arg(trailing_var_arg = true)]
    program_args: Vec<String>,

    /// Set initial breakpoint (line number or function name)
    #[arg(long = "break", short = 'b', value_name = "LOCATION")]
    breakpoints: Vec<String>,

    /// Break on program entry
    #[arg(long)]
    break_on_entry: bool,

    /// Break on errors
    #[arg(long, default_value = "true")]
    break_on_error: bool,

    /// Start in DAP (Debug Adapter Protocol) mode
    #[arg(long)]
    dap: bool,

    /// Port for DAP mode
    #[arg(long, default_value = "4711")]
    port: u16,

    /// Output verbosity
    #[arg(long, value_enum, default_value = "normal")]
    verbosity: VerbosityArg,

    /// Configuration file path
    #[arg(long, value_name = "FILE")]
    config: Option<PathBuf>,

    /// List available debugger commands
    #[arg(long)]
    list_commands: bool,

    /// Source file search paths
    #[arg(long = "source-path", short = 'I', value_name = "PATH")]
    source_paths: Vec<PathBuf>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum VerbosityArg {
    /// Minimal output
    Quiet,
    /// Normal output
    Normal,
    /// Detailed output
    Verbose,
    /// Trace all operations
    Trace,
}

impl From<VerbosityArg> for Verbosity {
    fn from(v: VerbosityArg) -> Self {
        match v {
            VerbosityArg::Quiet => Verbosity::Quiet,
            VerbosityArg::Normal => Verbosity::Normal,
            VerbosityArg::Verbose => Verbosity::Verbose,
            VerbosityArg::Trace => Verbosity::Trace,
        }
    }
}

fn main() -> ExitCode {
    let args = Args::parse();

    // Handle --list-commands
    if args.list_commands {
        return list_commands();
    }

    // Handle DAP mode
    if args.dap {
        return run_dap_mode(args.port);
    }

    // Build configuration
    let mut config = if let Some(config_path) = &args.config {
        match DebugConfig::from_file(config_path) {
            Ok(cfg) => cfg,
            Err(e) => {
                eprintln!("Error loading config: {}", e);
                return ExitCode::FAILURE;
            }
        }
    } else {
        DebugConfig::discover().unwrap_or_default()
    };

    // Apply command-line overrides
    config.verbosity = args.verbosity.into();
    config.break_on_entry = args.break_on_entry;
    config.break_on_error = args.break_on_error;
    config.target_args = args.program_args;

    for path in args.source_paths {
        config.add_source_path(path);
    }

    // Create debugger
    let mut debugger = Debugger::new(config);

    // Load the source file
    let file = args.file.expect("File is required in interactive mode");
    if let Err(e) = debugger.load_source(&file) {
        eprintln!("Error loading source: {}", e);
        return ExitCode::FAILURE;
    }

    // Set initial breakpoints from command line
    for bp_spec in &args.breakpoints {
        if let Err(e) = parse_and_add_breakpoint(&mut debugger, bp_spec, &file) {
            eprintln!("Warning: Could not set breakpoint '{}': {}", bp_spec, e);
        }
    }

    // Run interactive debugger
    run_interactive(&mut debugger, &file)
}

fn parse_and_add_breakpoint(
    debugger: &mut Debugger,
    spec: &str,
    default_file: &PathBuf,
) -> Result<u32, String> {
    // Try to parse as line number first
    if let Ok(line) = spec.parse::<usize>() {
        return debugger
            .add_line_breakpoint(default_file, line)
            .map_err(|e| e.to_string());
    }

    // Try file:line format
    if let Some((file, line_str)) = spec.split_once(':') {
        if let Ok(line) = line_str.parse::<usize>() {
            return debugger
                .add_line_breakpoint(file, line)
                .map_err(|e| e.to_string());
        }
    }

    // Treat as function name
    Ok(debugger.add_function_breakpoint(spec))
}

fn run_interactive(debugger: &mut Debugger, main_file: &PathBuf) -> ExitCode {
    println!("QB64Fresh Debugger v{}", env!("CARGO_PKG_VERSION"));
    println!("Loaded: {}", main_file.display());
    println!("Type 'help' for available commands.\n");

    // Show initial breakpoints
    if !debugger.breakpoints().is_empty() {
        println!("Initial breakpoints:");
        for bp in debugger.breakpoints() {
            println!("  {}", format_breakpoint(bp));
        }
        println!();
    }

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        // Print prompt
        let prompt = match debugger.state() {
            ExecutionState::NotStarted => "(debug) ",
            ExecutionState::Running => "(running) ",
            ExecutionState::Paused => "(paused) ",
            ExecutionState::Stepping => "(step) ",
            ExecutionState::Completed => "(done) ",
            ExecutionState::Error => "(error) ",
        };

        print!("{}", prompt);
        stdout.flush().unwrap();

        // Read command
        let mut line = String::new();
        if stdin.lock().read_line(&mut line).is_err() {
            break;
        }

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Parse and execute command
        let parts: Vec<&str> = line.split_whitespace().collect();
        let command = parts[0].to_lowercase();
        let args = &parts[1..];

        match command.as_str() {
            "help" | "h" | "?" => print_help(),

            "run" | "r" => {
                println!("Starting program...");
                match debugger.run() {
                    Ok(state) => println!("State: {:?}", state),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }

            "continue" | "c" => {
                println!("Continuing...");
                match debugger.run() {
                    Ok(state) => println!("State: {:?}", state),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }

            "step" | "s" | "next" | "n" => match debugger.step_over() {
                Ok(_) => {
                    if let Some(loc) = debugger.current_location() {
                        println!("At {}:{}:{}", loc.file.display(), loc.line, loc.column);
                    }
                }
                Err(e) => eprintln!("Error: {}", e),
            },

            "stepin" | "si" => match debugger.step_into() {
                Ok(_) => {
                    if let Some(loc) = debugger.current_location() {
                        println!("At {}:{}:{}", loc.file.display(), loc.line, loc.column);
                    }
                }
                Err(e) => eprintln!("Error: {}", e),
            },

            "stepout" | "so" | "finish" => match debugger.step_out() {
                Ok(_) => {
                    if let Some(loc) = debugger.current_location() {
                        println!("At {}:{}:{}", loc.file.display(), loc.line, loc.column);
                    }
                }
                Err(e) => eprintln!("Error: {}", e),
            },

            "break" | "b" => {
                if args.is_empty() {
                    // List breakpoints
                    if debugger.breakpoints().is_empty() {
                        println!("No breakpoints set.");
                    } else {
                        println!("Breakpoints:");
                        for bp in debugger.breakpoints() {
                            println!("  {}", format_breakpoint(bp));
                        }
                    }
                } else {
                    // Add breakpoint
                    match parse_and_add_breakpoint(debugger, args[0], main_file) {
                        Ok(id) => println!("Breakpoint {} set.", id),
                        Err(e) => eprintln!("Error: {}", e),
                    }
                }
            }

            "delete" | "d" | "clear" => {
                if args.is_empty() {
                    debugger.clear_breakpoints();
                    println!("All breakpoints cleared.");
                } else if let Ok(id) = args[0].parse::<u32>() {
                    if debugger.remove_breakpoint(id) {
                        println!("Breakpoint {} removed.", id);
                    } else {
                        eprintln!("No breakpoint with ID {}.", id);
                    }
                } else {
                    eprintln!("Invalid breakpoint ID.");
                }
            }

            "enable" => {
                if let Some(id_str) = args.first() {
                    if let Ok(id) = id_str.parse::<u32>() {
                        if debugger.enable_breakpoint(id) {
                            println!("Breakpoint {} enabled.", id);
                        } else {
                            eprintln!("No breakpoint with ID {}.", id);
                        }
                    } else {
                        eprintln!("Invalid breakpoint ID.");
                    }
                } else {
                    eprintln!("Usage: enable <breakpoint-id>");
                }
            }

            "disable" => {
                if let Some(id_str) = args.first() {
                    if let Ok(id) = id_str.parse::<u32>() {
                        if debugger.disable_breakpoint(id) {
                            println!("Breakpoint {} disabled.", id);
                        } else {
                            eprintln!("No breakpoint with ID {}.", id);
                        }
                    } else {
                        eprintln!("Invalid breakpoint ID.");
                    }
                } else {
                    eprintln!("Usage: disable <breakpoint-id>");
                }
            }

            "list" | "l" => {
                // List source code around current location or specified line
                let line = if let Some(line_str) = args.first() {
                    line_str.parse::<usize>().unwrap_or(1)
                } else if let Some(loc) = debugger.current_location() {
                    loc.line
                } else {
                    1
                };

                if let Some(source) = debugger.get_source(main_file) {
                    let start = line.saturating_sub(5);
                    let end = line + 5;

                    for n in start..=end {
                        if let Some(src_line) = source.get_line(n) {
                            let marker = if n == line { ">" } else { " " };
                            let bp_marker = if has_breakpoint_at(debugger, main_file, n) {
                                "*"
                            } else {
                                " "
                            };
                            println!("{}{} {:4}: {}", marker, bp_marker, n, src_line);
                        }
                    }
                } else {
                    eprintln!("Source not loaded.");
                }
            }

            "info" | "i" => {
                if args.is_empty() {
                    println!("Usage: info <what>");
                    println!("  info breakpoints  - List all breakpoints");
                    println!("  info sources      - List loaded source files");
                } else {
                    match args[0] {
                        "breakpoints" | "b" => {
                            if debugger.breakpoints().is_empty() {
                                println!("No breakpoints.");
                            } else {
                                for bp in debugger.breakpoints() {
                                    println!("  {}", format_breakpoint(bp));
                                }
                            }
                        }
                        "sources" | "s" => {
                            for source in debugger.sources() {
                                let lines = source.source.lines().count();
                                println!("  {} ({} lines)", source.path.display(), lines);
                            }
                        }
                        _ => eprintln!("Unknown info type: {}", args[0]),
                    }
                }
            }

            "quit" | "q" | "exit" => {
                println!("Goodbye!");
                break;
            }

            _ => {
                eprintln!(
                    "Unknown command: '{}'. Type 'help' for available commands.",
                    command
                );
            }
        }
    }

    ExitCode::SUCCESS
}

fn has_breakpoint_at(debugger: &Debugger, file: &PathBuf, line: usize) -> bool {
    debugger.breakpoints().iter().any(|bp| {
        matches!(&bp.kind, BreakpointKind::Line { file: f, line: l } if f == file && *l == line)
    })
}

fn format_breakpoint(bp: &Breakpoint) -> String {
    let status = if bp.enabled { "enabled" } else { "disabled" };
    let location = match &bp.kind {
        BreakpointKind::Line { file, line } => format!("{}:{}", file.display(), line),
        BreakpointKind::Function { name } => format!("function {}", name),
        BreakpointKind::Label { name } => format!("label {}", name),
        BreakpointKind::Conditional {
            file,
            line,
            condition,
        } => {
            format!("{}:{} when {}", file.display(), line, condition)
        }
    };

    format!("[{}] {} ({})", bp.id, location, status)
}

fn print_help() {
    println!("Available commands:");
    println!();
    println!("  Execution:");
    println!("    run, r              Start/restart program execution");
    println!("    continue, c         Continue execution until next breakpoint");
    println!("    step, s, next, n    Step to next statement (step over)");
    println!("    stepin, si          Step into function/sub call");
    println!("    stepout, so, finish Step out of current function/sub");
    println!();
    println!("  Breakpoints:");
    println!("    break, b [loc]      Set breakpoint or list all breakpoints");
    println!("                        Location: line number, file:line, or function name");
    println!("    delete, d [id]      Delete breakpoint (all if no ID given)");
    println!("    enable <id>         Enable a breakpoint");
    println!("    disable <id>        Disable a breakpoint");
    println!();
    println!("  Information:");
    println!("    list, l [line]      Show source code around line");
    println!("    info breakpoints    List all breakpoints");
    println!("    info sources        List loaded source files");
    println!();
    println!("  Other:");
    println!("    help, h, ?          Show this help");
    println!("    quit, q, exit       Exit the debugger");
    println!();
}

fn run_dap_mode(_port: u16) -> ExitCode {
    // Note: The port is ignored for now; we use stdio mode for DAP
    // which is the standard way VS Code connects to debug adapters.
    eprintln!("Starting DAP server in stdio mode...");

    let mut server = qb64fresh_debug::DapServer::new();
    match server.run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("DAP server error: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn list_commands() -> ExitCode {
    print_help();
    ExitCode::SUCCESS
}
