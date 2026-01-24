//! Debug Protocol for QB64Fresh Debugger.
//!
//! This module defines the communication protocol between the debugger
//! and the debugee (compiled BASIC program with debug hooks).
//!
//! ## Protocol Overview
//!
//! Communication uses named pipes with simple text-based messages.
//! Each message is a single line terminated by `\n`.
//!
//! ### Commands (Debugger → Debugee)
//!
//! - `CONTINUE` - Resume execution
//! - `STEP_INTO` - Step to next statement (entering calls)
//! - `STEP_OVER` - Step to next statement (skipping calls)
//! - `STEP_OUT` - Run until current procedure returns
//! - `PAUSE` - Pause execution
//! - `TERMINATE` - Stop the program
//! - `BP_ADD <line>` - Add breakpoint at line
//! - `BP_REMOVE <line>` - Remove breakpoint at line
//!
//! ### Events (Debugee → Debugger)
//!
//! - `READY` - Program started, waiting for commands
//! - `STOPPED <reason> <line> <file>` - Execution paused
//! - `TERMINATED` - Program ended
//! - `ENTER <proc> <line>` - Entered procedure
//! - `EXIT <proc>` - Exited procedure
//! - `VAR <name> <type> <value>` - Variable value response
//! - `LOCATION <line> <file> <proc>` - Current location

use serde::{Deserialize, Serialize};
use std::fmt;
use std::io::{BufRead, Write};
use std::str::FromStr;

/// Commands sent from the debugger to the debugee.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DebugCommand {
    /// Resume normal execution.
    Continue,
    /// Step to the next statement, entering procedure calls.
    StepInto,
    /// Step to the next statement, treating procedure calls as single steps.
    StepOver,
    /// Continue execution until the current procedure returns.
    StepOut,
    /// Pause execution at the next opportunity.
    Pause,
    /// Terminate the program immediately.
    Terminate,
    /// Add a breakpoint at the specified line.
    AddBreakpoint { line: u32 },
    /// Remove the breakpoint at the specified line.
    RemoveBreakpoint { line: u32 },
    /// Request the value of a variable.
    GetVariable { name: String, frame: u32 },
    /// Request current location information.
    GetLocation,
}

impl fmt::Display for DebugCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DebugCommand::Continue => write!(f, "CONTINUE"),
            DebugCommand::StepInto => write!(f, "STEP_INTO"),
            DebugCommand::StepOver => write!(f, "STEP_OVER"),
            DebugCommand::StepOut => write!(f, "STEP_OUT"),
            DebugCommand::Pause => write!(f, "PAUSE"),
            DebugCommand::Terminate => write!(f, "TERMINATE"),
            DebugCommand::AddBreakpoint { line } => write!(f, "BP_ADD {}", line),
            DebugCommand::RemoveBreakpoint { line } => write!(f, "BP_REMOVE {}", line),
            DebugCommand::GetVariable { name, frame } => write!(f, "GET_VAR {} {}", name, frame),
            DebugCommand::GetLocation => write!(f, "GET_LOCATION"),
        }
    }
}

impl FromStr for DebugCommand {
    type Err = ProtocolError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let parts: Vec<&str> = s.splitn(3, ' ').collect();

        match parts.first().map(|s| *s) {
            Some("CONTINUE") => Ok(DebugCommand::Continue),
            Some("STEP_INTO") => Ok(DebugCommand::StepInto),
            Some("STEP_OVER") => Ok(DebugCommand::StepOver),
            Some("STEP_OUT") => Ok(DebugCommand::StepOut),
            Some("PAUSE") => Ok(DebugCommand::Pause),
            Some("TERMINATE") => Ok(DebugCommand::Terminate),
            Some("BP_ADD") => {
                let line = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("line".into()))?
                    .parse()
                    .map_err(|_| ProtocolError::InvalidArgument("line".into()))?;
                Ok(DebugCommand::AddBreakpoint { line })
            }
            Some("BP_REMOVE") => {
                let line = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("line".into()))?
                    .parse()
                    .map_err(|_| ProtocolError::InvalidArgument("line".into()))?;
                Ok(DebugCommand::RemoveBreakpoint { line })
            }
            Some("GET_VAR") => {
                let name = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("name".into()))?
                    .to_string();
                let frame = parts.get(2).unwrap_or(&"0").parse().unwrap_or(0);
                Ok(DebugCommand::GetVariable { name, frame })
            }
            Some("GET_LOCATION") => Ok(DebugCommand::GetLocation),
            _ => Err(ProtocolError::UnknownCommand(s.to_string())),
        }
    }
}

/// Events sent from the debugee to the debugger.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DebugEvent {
    /// Program started and is ready for debugging.
    Ready,
    /// Execution has stopped.
    Stopped {
        /// Reason for stopping.
        reason: StopReason,
        /// Current line number (byte offset for now).
        line: u32,
        /// Current source file.
        file: String,
    },
    /// Program has terminated.
    Terminated,
    /// Entered a procedure.
    ProcedureEnter {
        /// Procedure name.
        name: String,
        /// Line number of entry.
        line: u32,
    },
    /// Exited a procedure.
    ProcedureExit {
        /// Procedure name.
        name: String,
    },
    /// Variable value response.
    VariableValue {
        /// Variable name.
        name: String,
        /// Variable type.
        var_type: String,
        /// Value as string.
        value: String,
    },
    /// Current location information.
    Location {
        /// Current line number.
        line: u32,
        /// Current source file.
        file: String,
        /// Current procedure (or "main").
        procedure: String,
    },
    /// Output from the program.
    Output {
        /// Output text.
        text: String,
    },
    /// Error message from the debugee.
    Error {
        /// Error message.
        message: String,
    },
}

impl fmt::Display for DebugEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DebugEvent::Ready => write!(f, "READY"),
            DebugEvent::Stopped { reason, line, file } => {
                write!(f, "STOPPED {} {} {}", reason, line, file)
            }
            DebugEvent::Terminated => write!(f, "TERMINATED"),
            DebugEvent::ProcedureEnter { name, line } => write!(f, "ENTER {} {}", name, line),
            DebugEvent::ProcedureExit { name } => write!(f, "EXIT {}", name),
            DebugEvent::VariableValue {
                name,
                var_type,
                value,
            } => write!(f, "VAR {} {} {}", name, var_type, value),
            DebugEvent::Location {
                line,
                file,
                procedure,
            } => write!(f, "LOCATION {} {} {}", line, file, procedure),
            DebugEvent::Output { text } => write!(f, "OUTPUT {}", text),
            DebugEvent::Error { message } => write!(f, "ERROR {}", message),
        }
    }
}

impl FromStr for DebugEvent {
    type Err = ProtocolError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let parts: Vec<&str> = s.splitn(4, ' ').collect();

        match parts.first().map(|s| *s) {
            Some("READY") => Ok(DebugEvent::Ready),
            Some("TERMINATED") => Ok(DebugEvent::Terminated),
            Some("STOPPED") => {
                let reason = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("reason".into()))?
                    .parse()?;
                let line = parts
                    .get(2)
                    .ok_or_else(|| ProtocolError::MissingArgument("line".into()))?
                    .parse()
                    .map_err(|_| ProtocolError::InvalidArgument("line".into()))?;
                let file = parts.get(3).unwrap_or(&"").to_string();
                Ok(DebugEvent::Stopped { reason, line, file })
            }
            Some("ENTER") => {
                let name = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("name".into()))?
                    .to_string();
                let line = parts
                    .get(2)
                    .ok_or_else(|| ProtocolError::MissingArgument("line".into()))?
                    .parse()
                    .map_err(|_| ProtocolError::InvalidArgument("line".into()))?;
                Ok(DebugEvent::ProcedureEnter { name, line })
            }
            Some("EXIT") => {
                let name = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("name".into()))?
                    .to_string();
                Ok(DebugEvent::ProcedureExit { name })
            }
            Some("VAR") => {
                let name = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("name".into()))?
                    .to_string();
                let var_type = parts
                    .get(2)
                    .ok_or_else(|| ProtocolError::MissingArgument("type".into()))?
                    .to_string();
                let value = parts.get(3).unwrap_or(&"").to_string();
                Ok(DebugEvent::VariableValue {
                    name,
                    var_type,
                    value,
                })
            }
            Some("LOCATION") => {
                let line = parts
                    .get(1)
                    .ok_or_else(|| ProtocolError::MissingArgument("line".into()))?
                    .parse()
                    .map_err(|_| ProtocolError::InvalidArgument("line".into()))?;
                let file = parts.get(2).unwrap_or(&"").to_string();
                let procedure = parts.get(3).unwrap_or(&"main").to_string();
                Ok(DebugEvent::Location {
                    line,
                    file,
                    procedure,
                })
            }
            Some("OUTPUT") => {
                let text = if parts.len() > 1 {
                    parts[1..].join(" ")
                } else {
                    String::new()
                };
                Ok(DebugEvent::Output { text })
            }
            Some("ERROR") => {
                let message = if parts.len() > 1 {
                    parts[1..].join(" ")
                } else {
                    String::new()
                };
                Ok(DebugEvent::Error { message })
            }
            _ => Err(ProtocolError::UnknownEvent(s.to_string())),
        }
    }
}

/// Reason why execution stopped.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StopReason {
    /// Hit a breakpoint.
    Breakpoint,
    /// Completed a step operation.
    Step,
    /// Paused by user request.
    Pause,
    /// Program entry point.
    Entry,
    /// Runtime exception occurred.
    Exception,
}

impl fmt::Display for StopReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StopReason::Breakpoint => write!(f, "breakpoint"),
            StopReason::Step => write!(f, "step"),
            StopReason::Pause => write!(f, "pause"),
            StopReason::Entry => write!(f, "entry"),
            StopReason::Exception => write!(f, "exception"),
        }
    }
}

impl FromStr for StopReason {
    type Err = ProtocolError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "breakpoint" => Ok(StopReason::Breakpoint),
            "step" => Ok(StopReason::Step),
            "pause" => Ok(StopReason::Pause),
            "entry" => Ok(StopReason::Entry),
            "exception" => Ok(StopReason::Exception),
            _ => Err(ProtocolError::InvalidArgument(s.to_string())),
        }
    }
}

/// Protocol errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProtocolError {
    /// Unknown command received.
    UnknownCommand(String),
    /// Unknown event received.
    UnknownEvent(String),
    /// Missing required argument.
    MissingArgument(String),
    /// Invalid argument value.
    InvalidArgument(String),
    /// I/O error.
    IoError(String),
}

impl fmt::Display for ProtocolError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProtocolError::UnknownCommand(cmd) => write!(f, "Unknown command: {}", cmd),
            ProtocolError::UnknownEvent(event) => write!(f, "Unknown event: {}", event),
            ProtocolError::MissingArgument(arg) => write!(f, "Missing argument: {}", arg),
            ProtocolError::InvalidArgument(arg) => write!(f, "Invalid argument: {}", arg),
            ProtocolError::IoError(msg) => write!(f, "I/O error: {}", msg),
        }
    }
}

impl std::error::Error for ProtocolError {}

impl From<std::io::Error> for ProtocolError {
    fn from(e: std::io::Error) -> Self {
        ProtocolError::IoError(e.to_string())
    }
}

/// A connection to a debugee process.
///
/// Handles sending commands and receiving events over a named pipe.
pub struct DebugConnection<R: BufRead, W: Write> {
    reader: R,
    writer: W,
}

impl<R: BufRead, W: Write> DebugConnection<R, W> {
    /// Creates a new debug connection.
    pub fn new(reader: R, writer: W) -> Self {
        Self { reader, writer }
    }

    /// Sends a command to the debugee.
    pub fn send_command(&mut self, cmd: &DebugCommand) -> Result<(), ProtocolError> {
        writeln!(self.writer, "{}", cmd)?;
        self.writer.flush()?;
        Ok(())
    }

    /// Receives an event from the debugee.
    ///
    /// This blocks until an event is received or an error occurs.
    pub fn receive_event(&mut self) -> Result<DebugEvent, ProtocolError> {
        let mut line = String::new();
        self.reader.read_line(&mut line)?;
        line.parse()
    }

    /// Tries to receive an event without blocking.
    ///
    /// Returns `None` if no event is available.
    pub fn try_receive_event(&mut self) -> Result<Option<DebugEvent>, ProtocolError> {
        // Note: This requires the reader to support non-blocking reads.
        // For simplicity, we'll just do a blocking read here.
        // A real implementation would use poll/select or async I/O.
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(0) => Ok(None), // EOF
            Ok(_) => Ok(Some(line.parse()?)),
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(None),
            Err(e) => Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_roundtrip() {
        let commands = vec![
            DebugCommand::Continue,
            DebugCommand::StepInto,
            DebugCommand::StepOver,
            DebugCommand::StepOut,
            DebugCommand::Pause,
            DebugCommand::Terminate,
            DebugCommand::AddBreakpoint { line: 42 },
            DebugCommand::RemoveBreakpoint { line: 100 },
        ];

        for cmd in commands {
            let s = cmd.to_string();
            let parsed: DebugCommand = s.parse().unwrap();
            assert_eq!(cmd, parsed);
        }
    }

    #[test]
    fn test_event_roundtrip() {
        let events = vec![
            DebugEvent::Ready,
            DebugEvent::Terminated,
            DebugEvent::Stopped {
                reason: StopReason::Breakpoint,
                line: 10,
                file: "test.bas".to_string(),
            },
            DebugEvent::ProcedureEnter {
                name: "MySub".to_string(),
                line: 20,
            },
            DebugEvent::ProcedureExit {
                name: "MySub".to_string(),
            },
            DebugEvent::VariableValue {
                name: "x".to_string(),
                var_type: "int".to_string(),
                value: "42".to_string(),
            },
        ];

        for event in events {
            let s = event.to_string();
            let parsed: DebugEvent = s.parse().unwrap();
            assert_eq!(event, parsed);
        }
    }

    #[test]
    fn test_stop_reason_roundtrip() {
        let reasons = vec![
            StopReason::Breakpoint,
            StopReason::Step,
            StopReason::Pause,
            StopReason::Entry,
            StopReason::Exception,
        ];

        for reason in reasons {
            let s = reason.to_string();
            let parsed: StopReason = s.parse().unwrap();
            assert_eq!(reason, parsed);
        }
    }

    #[test]
    fn test_parse_stopped_event() {
        let event: DebugEvent = "STOPPED breakpoint 42 test.bas".parse().unwrap();
        assert_eq!(
            event,
            DebugEvent::Stopped {
                reason: StopReason::Breakpoint,
                line: 42,
                file: "test.bas".to_string(),
            }
        );
    }

    #[test]
    fn test_parse_var_event() {
        let event: DebugEvent = "VAR count int 123".parse().unwrap();
        assert_eq!(
            event,
            DebugEvent::VariableValue {
                name: "count".to_string(),
                var_type: "int".to_string(),
                value: "123".to_string(),
            }
        );
    }
}
