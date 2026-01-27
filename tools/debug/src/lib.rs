//! QB64Fresh Debugger Library
//!
//! This library provides debugging capabilities for QB64/QBasic programs.
//! It can be used both as a library and through the `qb64fresh-debug` CLI tool.
//!
//! ## Features
//!
//! - **Breakpoints**: Set line, function, label, and conditional breakpoints
//! - **Stepping**: Step over, step into, step out, and continue execution
//! - **Inspection**: View variables, call stack, and program state
//! - **Source mapping**: Map compiled code back to BASIC source lines
//!
//! ## Example
//!
//! ```no_run
//! use qb64fresh_debug::{Debugger, DebugConfig};
//!
//! let config = DebugConfig::interactive();
//! let mut debugger = Debugger::new(config);
//!
//! // Load a program
//! debugger.load_source("myprogram.bas").unwrap();
//!
//! // Set a breakpoint
//! debugger.add_line_breakpoint("myprogram.bas", 10).unwrap();
//!
//! // Start debugging
//! debugger.run().unwrap();
//! ```

mod config;
pub mod dap;
mod error;
pub mod frames;
pub mod protocol;
pub mod server;
pub mod sources;
pub mod symbols;
pub mod values;
pub mod watch;

pub use config::{Breakpoint, BreakpointKind, DebugConfig, Verbosity};
pub use error::{DebugError, DebugResult};
pub use frames::{
    AccessPathElement, CallStack, FrameId, ScopeInfo, StackFrame, VariableCategory, VariableGroup,
    VariableReference,
};
pub use protocol::{DebugCommand, DebugConnection, DebugEvent, ProtocolError, StopReason};
pub use server::{DapServer, DapServerError};
pub use sources::{IncludeInfo, SourceFileInfo, SourceManager, SourcePosition};
pub use symbols::{
    DebugArrayDimension, DebugLabel, DebugParameter, DebugProcedure, DebugScope, DebugScopeId,
    DebugScopeKind, DebugSymbols, DebugType, DebugTypeMember, DebugUserType, DebugVariable,
    DebugVariableKind,
};
pub use values::{
    ArrayBounds, ArrayValue, DebugValue, DisplayFormat, NumberFormat, UdtValue, VariableInfo,
};
pub use watch::{
    IndexExpr, Watch, WatchExpression, WatchFormat, WatchId, WatchManager, WatchResult,
};

use qb64fresh::ast::{Program, Span, Statement, StatementKind};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use std::collections::HashMap;
use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};

/// Source location information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    /// File path.
    pub file: PathBuf,
    /// Line number (1-indexed).
    pub line: usize,
    /// Column number (1-indexed).
    pub column: usize,
}

impl SourceLocation {
    /// Creates a new source location.
    pub fn new(file: PathBuf, line: usize, column: usize) -> Self {
        Self { file, line, column }
    }

    /// Creates a source location from a span and source text.
    pub fn from_span(file: PathBuf, source: &str, span: &Span) -> Self {
        let (line, column) = offset_to_line_col(source, span.start);
        Self { file, line, column }
    }
}

/// Debug execution state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionState {
    /// Program not yet started.
    NotStarted,
    /// Program is running.
    Running,
    /// Program is paused at a breakpoint.
    Paused,
    /// Program is stepping.
    Stepping,
    /// Program has completed normally.
    Completed,
    /// Program terminated with an error.
    Error,
}

/// Information about a loaded source file.
#[derive(Debug)]
pub struct SourceFile {
    /// File path.
    pub path: PathBuf,
    /// Source code content.
    pub source: String,
    /// Parsed AST (if parsing succeeded).
    pub ast: Option<Program>,
    /// Line-to-statement mapping for breakpoint validation.
    pub line_map: HashMap<usize, Vec<Span>>,
}

impl SourceFile {
    /// Creates a new source file from path and content.
    pub fn new(path: PathBuf, source: String) -> Self {
        Self {
            path,
            source,
            ast: None,
            line_map: HashMap::new(),
        }
    }

    /// Parses the source file and builds the line map.
    pub fn parse(&mut self) -> DebugResult<()> {
        let tokens = lex(&self.source);
        let mut parser = Parser::new(&tokens);

        let program = parser.parse().map_err(|errors| {
            let messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
            DebugError::ParseError {
                message: messages.join("; "),
            }
        })?;

        // Build line map from statements
        self.build_line_map(&program);
        self.ast = Some(program);

        Ok(())
    }

    /// Builds a mapping from line numbers to statement spans.
    fn build_line_map(&mut self, program: &Program) {
        self.line_map.clear();

        for statement in &program.statements {
            self.add_statement_to_line_map(statement);
        }
    }

    /// Recursively adds a statement and its children to the line map.
    fn add_statement_to_line_map(&mut self, statement: &Statement) {
        let (line, _) = offset_to_line_col(&self.source, statement.span.start);
        self.line_map
            .entry(line)
            .or_insert_with(Vec::new)
            .push(statement.span.clone());

        // Handle nested statements (IF blocks, FOR loops, etc.)
        match &statement.kind {
            StatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                for stmt in then_branch {
                    self.add_statement_to_line_map(stmt);
                }
                for (_, stmts) in elseif_branches {
                    for stmt in stmts {
                        self.add_statement_to_line_map(stmt);
                    }
                }
                if let Some(stmts) = else_branch {
                    for stmt in stmts {
                        self.add_statement_to_line_map(stmt);
                    }
                }
            }
            StatementKind::For { body, .. } => {
                for stmt in body {
                    self.add_statement_to_line_map(stmt);
                }
            }
            StatementKind::While { body, .. } => {
                for stmt in body {
                    self.add_statement_to_line_map(stmt);
                }
            }
            StatementKind::DoLoop { body, .. } => {
                for stmt in body {
                    self.add_statement_to_line_map(stmt);
                }
            }
            StatementKind::SelectCase {
                cases, case_else, ..
            }
            | StatementKind::SelectEveryCase {
                cases, case_else, ..
            } => {
                for case in cases {
                    for stmt in &case.body {
                        self.add_statement_to_line_map(stmt);
                    }
                }
                if let Some(stmts) = case_else {
                    for stmt in stmts {
                        self.add_statement_to_line_map(stmt);
                    }
                }
            }
            StatementKind::SubDefinition { body, .. }
            | StatementKind::FunctionDefinition { body, .. } => {
                for stmt in body {
                    self.add_statement_to_line_map(stmt);
                }
            }
            _ => {}
        }
    }

    /// Checks if a line is a valid breakpoint location.
    pub fn is_valid_breakpoint_line(&self, line: usize) -> bool {
        self.line_map.contains_key(&line)
    }

    /// Gets the source line at the given line number (1-indexed).
    pub fn get_line(&self, line: usize) -> Option<&str> {
        self.source.lines().nth(line.saturating_sub(1))
    }
}

/// The main debugger interface.
///
/// The `Debugger` manages debug sessions, breakpoints, and program execution.
pub struct Debugger {
    config: DebugConfig,
    /// Loaded source files.
    sources: HashMap<PathBuf, SourceFile>,
    /// Active breakpoints.
    breakpoints: Vec<Breakpoint>,
    /// Next breakpoint ID.
    next_breakpoint_id: u32,
    /// Current execution state.
    state: ExecutionState,
    /// Current source location (when paused).
    current_location: Option<SourceLocation>,
    /// Optional connection to the debuggee runtime.
    ///
    /// When `None`, execution control methods will return errors indicating
    /// that runtime integration is required.
    connection: Option<Box<dyn Connection>>,
}

/// Trait for debug connections to enable runtime integration.
///
/// This trait abstracts over the concrete `DebugConnection` type to allow
/// the debugger to work with different connection implementations (real runtime,
/// mock for testing, etc.).
pub trait Connection: Send + Sync {
    /// Sends a command to the debuggee.
    fn send_command(&mut self, cmd: &DebugCommand) -> Result<(), ProtocolError>;

    /// Receives an event from the debuggee (blocking).
    fn receive_event(&mut self) -> Result<DebugEvent, ProtocolError>;

    /// Tries to receive an event without blocking.
    fn try_receive_event(&mut self) -> Result<Option<DebugEvent>, ProtocolError>;
}

// Implement Connection for DebugConnection
impl<R: BufRead + Send + Sync, W: Write + Send + Sync> Connection for DebugConnection<R, W> {
    fn send_command(&mut self, cmd: &DebugCommand) -> Result<(), ProtocolError> {
        DebugConnection::send_command(self, cmd)
    }

    fn receive_event(&mut self) -> Result<DebugEvent, ProtocolError> {
        DebugConnection::receive_event(self)
    }

    fn try_receive_event(&mut self) -> Result<Option<DebugEvent>, ProtocolError> {
        DebugConnection::try_receive_event(self)
    }
}

impl Debugger {
    /// Creates a new debugger with the given configuration.
    pub fn new(config: DebugConfig) -> Self {
        let breakpoints = config.breakpoints.clone();
        let next_id = breakpoints.iter().map(|b| b.id).max().unwrap_or(0) + 1;

        Self {
            config,
            sources: HashMap::new(),
            breakpoints,
            next_breakpoint_id: next_id,
            state: ExecutionState::NotStarted,
            current_location: None,
            connection: None,
        }
    }

    /// Creates a debugger with default configuration.
    pub fn default_config() -> Self {
        Self::new(DebugConfig::default())
    }

    /// Creates a debugger configured for interactive use.
    pub fn interactive() -> Self {
        Self::new(DebugConfig::interactive())
    }

    /// Returns a reference to the current configuration.
    pub fn config(&self) -> &DebugConfig {
        &self.config
    }

    /// Returns a mutable reference to the configuration.
    pub fn config_mut(&mut self) -> &mut DebugConfig {
        &mut self.config
    }

    /// Returns the current execution state.
    pub fn state(&self) -> ExecutionState {
        self.state
    }

    /// Returns the current source location (if paused).
    pub fn current_location(&self) -> Option<&SourceLocation> {
        self.current_location.as_ref()
    }

    /// Attaches a connection to the debuggee runtime.
    ///
    /// This must be called before using execution control methods.
    pub fn attach_connection<C: Connection + 'static>(&mut self, connection: C) {
        self.connection = Some(Box::new(connection));
    }

    /// Detaches the current connection.
    pub fn detach_connection(&mut self) {
        self.connection = None;
    }

    /// Returns whether a connection to the runtime is attached.
    pub fn is_connected(&self) -> bool {
        self.connection.is_some()
    }

    /// Loads a source file for debugging.
    pub fn load_source<P: AsRef<Path>>(&mut self, path: P) -> DebugResult<()> {
        let path = path.as_ref().to_path_buf();
        let source = std::fs::read_to_string(&path).map_err(|e| DebugError::ReadError {
            path: path.clone(),
            source: e,
        })?;

        let mut source_file = SourceFile::new(path.clone(), source);
        source_file.parse()?;

        self.sources.insert(path, source_file);
        Ok(())
    }

    /// Loads source code directly (for testing or in-memory debugging).
    pub fn load_source_string(&mut self, name: &str, source: String) -> DebugResult<()> {
        let path = PathBuf::from(name);
        let mut source_file = SourceFile::new(path.clone(), source);
        source_file.parse()?;

        self.sources.insert(path, source_file);
        Ok(())
    }

    /// Returns the loaded source files.
    pub fn sources(&self) -> impl Iterator<Item = &SourceFile> {
        self.sources.values()
    }

    /// Gets a specific source file.
    pub fn get_source<P: AsRef<Path>>(&self, path: P) -> Option<&SourceFile> {
        self.sources.get(path.as_ref())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Breakpoint Management
    // ─────────────────────────────────────────────────────────────────────────

    /// Adds a line breakpoint.
    pub fn add_line_breakpoint<P: AsRef<Path>>(
        &mut self,
        file: P,
        line: usize,
    ) -> DebugResult<u32> {
        let path = file.as_ref().to_path_buf();

        // Validate the breakpoint location if source is loaded
        if let Some(source) = self.sources.get(&path) {
            if !source.is_valid_breakpoint_line(line) {
                return Err(DebugError::InvalidBreakpointLocation { file: path, line });
            }
        }

        let id = self.next_breakpoint_id;
        self.next_breakpoint_id += 1;

        let breakpoint = Breakpoint::line(id, path, line);
        self.breakpoints.push(breakpoint);

        Ok(id)
    }

    /// Adds a function breakpoint.
    pub fn add_function_breakpoint(&mut self, name: &str) -> u32 {
        let id = self.next_breakpoint_id;
        self.next_breakpoint_id += 1;

        let breakpoint = Breakpoint::function(id, name.to_string());
        self.breakpoints.push(breakpoint);

        id
    }

    /// Adds a label breakpoint.
    pub fn add_label_breakpoint(&mut self, name: &str) -> u32 {
        let id = self.next_breakpoint_id;
        self.next_breakpoint_id += 1;

        let breakpoint = Breakpoint::label(id, name.to_string());
        self.breakpoints.push(breakpoint);

        id
    }

    /// Adds a conditional breakpoint.
    pub fn add_conditional_breakpoint<P: AsRef<Path>>(
        &mut self,
        file: P,
        line: usize,
        condition: &str,
    ) -> DebugResult<u32> {
        let path = file.as_ref().to_path_buf();

        // Validate the breakpoint location if source is loaded
        if let Some(source) = self.sources.get(&path) {
            if !source.is_valid_breakpoint_line(line) {
                return Err(DebugError::InvalidBreakpointLocation { file: path, line });
            }
        }

        let id = self.next_breakpoint_id;
        self.next_breakpoint_id += 1;

        let breakpoint = Breakpoint::conditional(id, path, line, condition.to_string());
        self.breakpoints.push(breakpoint);

        Ok(id)
    }

    /// Removes a breakpoint by ID.
    pub fn remove_breakpoint(&mut self, id: u32) -> bool {
        if let Some(pos) = self.breakpoints.iter().position(|b| b.id == id) {
            self.breakpoints.remove(pos);
            true
        } else {
            false
        }
    }

    /// Enables a breakpoint by ID.
    pub fn enable_breakpoint(&mut self, id: u32) -> bool {
        if let Some(bp) = self.breakpoints.iter_mut().find(|b| b.id == id) {
            bp.enabled = true;
            true
        } else {
            false
        }
    }

    /// Disables a breakpoint by ID.
    pub fn disable_breakpoint(&mut self, id: u32) -> bool {
        if let Some(bp) = self.breakpoints.iter_mut().find(|b| b.id == id) {
            bp.enabled = false;
            true
        } else {
            false
        }
    }

    /// Returns all breakpoints.
    pub fn breakpoints(&self) -> &[Breakpoint] {
        &self.breakpoints
    }

    /// Clears all breakpoints.
    pub fn clear_breakpoints(&mut self) {
        self.breakpoints.clear();
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Execution Control
    // ─────────────────────────────────────────────────────────────────────────

    /// Starts or continues program execution.
    ///
    /// # Errors
    ///
    /// - Returns `ConnectionError` if no runtime connection is attached
    /// - Returns `SessionError` if the current state doesn't allow running
    ///   (e.g., already running or completed)
    pub fn run(&mut self) -> DebugResult<ExecutionState> {
        // Validate state transition
        match self.state {
            ExecutionState::Running => {
                return Err(DebugError::SessionError {
                    message: "Program is already running".to_string(),
                });
            }
            ExecutionState::Completed | ExecutionState::Error => {
                return Err(DebugError::SessionError {
                    message: format!("Cannot run program in {:?} state", self.state),
                });
            }
            _ => {} // Valid states: NotStarted, Paused, Stepping
        }

        // Send command to runtime if connected
        if let Some(conn) = &mut self.connection {
            conn.send_command(&DebugCommand::Continue).map_err(|e| {
                DebugError::ConnectionError {
                    message: format!("Failed to send CONTINUE command: {}", e),
                }
            })?;
        } else {
            return Err(DebugError::ConnectionError {
                message: "No runtime connection attached. Call attach_connection() first."
                    .to_string(),
            });
        }

        self.state = ExecutionState::Running;
        Ok(self.state)
    }

    /// Pauses program execution.
    ///
    /// # Errors
    ///
    /// - Returns `ConnectionError` if no runtime connection is attached
    /// - Returns `SessionError` if the program is not running
    pub fn pause(&mut self) -> DebugResult<ExecutionState> {
        // Validate state transition
        if self.state != ExecutionState::Running {
            return Err(DebugError::SessionError {
                message: format!(
                    "Cannot pause program in {:?} state (must be Running)",
                    self.state
                ),
            });
        }

        // Send command to runtime if connected
        if let Some(conn) = &mut self.connection {
            conn.send_command(&DebugCommand::Pause)
                .map_err(|e| DebugError::ConnectionError {
                    message: format!("Failed to send PAUSE command: {}", e),
                })?;
        } else {
            return Err(DebugError::ConnectionError {
                message: "No runtime connection attached. Call attach_connection() first."
                    .to_string(),
            });
        }

        self.state = ExecutionState::Paused;
        Ok(self.state)
    }

    /// Steps to the next statement (step over).
    ///
    /// This executes the current statement and stops at the next statement
    /// in the same scope, treating procedure calls as single steps.
    ///
    /// # Errors
    ///
    /// - Returns `ConnectionError` if no runtime connection is attached
    /// - Returns `SessionError` if the program is not in a pausable state
    ///
    /// # Note
    ///
    /// Stepping from `NotStarted` state is allowed, but typically the runtime
    /// should send a `Ready` event first (which transitions to `Paused` state).
    /// The runtime will handle the case where the program isn't ready yet.
    pub fn step_over(&mut self) -> DebugResult<ExecutionState> {
        // Validate state transition
        match self.state {
            ExecutionState::NotStarted | ExecutionState::Paused | ExecutionState::Stepping => {}
            ExecutionState::Running => {
                return Err(DebugError::SessionError {
                    message: "Cannot step while running. Pause first.".to_string(),
                });
            }
            ExecutionState::Completed | ExecutionState::Error => {
                return Err(DebugError::SessionError {
                    message: format!("Cannot step program in {:?} state", self.state),
                });
            }
        }

        // Send command to runtime if connected
        if let Some(conn) = &mut self.connection {
            conn.send_command(&DebugCommand::StepOver).map_err(|e| {
                DebugError::ConnectionError {
                    message: format!("Failed to send STEP_OVER command: {}", e),
                }
            })?;
        } else {
            return Err(DebugError::ConnectionError {
                message: "No runtime connection attached. Call attach_connection() first."
                    .to_string(),
            });
        }

        self.state = ExecutionState::Stepping;
        Ok(self.state)
    }

    /// Steps into a function/sub call.
    ///
    /// This executes the current statement and stops at the first statement
    /// inside any called procedure.
    ///
    /// # Errors
    ///
    /// - Returns `ConnectionError` if no runtime connection is attached
    /// - Returns `SessionError` if the program is not in a pausable state
    ///
    /// # Note
    ///
    /// Stepping from `NotStarted` state is allowed, but typically the runtime
    /// should send a `Ready` event first (which transitions to `Paused` state).
    /// The runtime will handle the case where the program isn't ready yet.
    pub fn step_into(&mut self) -> DebugResult<ExecutionState> {
        // Validate state transition
        match self.state {
            ExecutionState::NotStarted | ExecutionState::Paused | ExecutionState::Stepping => {}
            ExecutionState::Running => {
                return Err(DebugError::SessionError {
                    message: "Cannot step while running. Pause first.".to_string(),
                });
            }
            ExecutionState::Completed | ExecutionState::Error => {
                return Err(DebugError::SessionError {
                    message: format!("Cannot step program in {:?} state", self.state),
                });
            }
        }

        // Send command to runtime if connected
        if let Some(conn) = &mut self.connection {
            conn.send_command(&DebugCommand::StepInto).map_err(|e| {
                DebugError::ConnectionError {
                    message: format!("Failed to send STEP_INTO command: {}", e),
                }
            })?;
        } else {
            return Err(DebugError::ConnectionError {
                message: "No runtime connection attached. Call attach_connection() first."
                    .to_string(),
            });
        }

        self.state = ExecutionState::Stepping;
        Ok(self.state)
    }

    /// Steps out of the current function/sub.
    ///
    /// This continues execution until the current procedure returns,
    /// then stops at the statement after the call.
    ///
    /// # Errors
    ///
    /// - Returns `ConnectionError` if no runtime connection is attached
    /// - Returns `SessionError` if the program is not in a pausable state
    ///
    /// # Note
    ///
    /// Stepping from `NotStarted` state is allowed, but typically the runtime
    /// should send a `Ready` event first (which transitions to `Paused` state).
    /// The runtime will handle the case where the program isn't ready yet.
    pub fn step_out(&mut self) -> DebugResult<ExecutionState> {
        // Validate state transition
        match self.state {
            ExecutionState::NotStarted | ExecutionState::Paused | ExecutionState::Stepping => {}
            ExecutionState::Running => {
                return Err(DebugError::SessionError {
                    message: "Cannot step while running. Pause first.".to_string(),
                });
            }
            ExecutionState::Completed | ExecutionState::Error => {
                return Err(DebugError::SessionError {
                    message: format!("Cannot step program in {:?} state", self.state),
                });
            }
        }

        // Send command to runtime if connected
        if let Some(conn) = &mut self.connection {
            conn.send_command(&DebugCommand::StepOut)
                .map_err(|e| DebugError::ConnectionError {
                    message: format!("Failed to send STEP_OUT command: {}", e),
                })?;
        } else {
            return Err(DebugError::ConnectionError {
                message: "No runtime connection attached. Call attach_connection() first."
                    .to_string(),
            });
        }

        self.state = ExecutionState::Stepping;
        Ok(self.state)
    }

    /// Stops debugging and terminates the program.
    ///
    /// If no connection is attached, this simply updates the internal state
    /// to `Completed` (useful if the program never started).
    ///
    /// # Errors
    ///
    /// - Returns `ConnectionError` if a connection exists but the terminate command fails
    pub fn stop(&mut self) -> DebugResult<ExecutionState> {
        // Send command to runtime if connected
        if let Some(conn) = &mut self.connection {
            conn.send_command(&DebugCommand::Terminate).map_err(|e| {
                DebugError::ConnectionError {
                    message: format!("Failed to send TERMINATE command: {}", e),
                }
            })?;
        } else {
            // If not connected, we can still update state (program might not have started)
            self.state = ExecutionState::Completed;
            self.current_location = None;
            return Ok(self.state);
        }

        self.state = ExecutionState::Completed;
        self.current_location = None;
        Ok(self.state)
    }

    /// Processes events from the debuggee runtime.
    ///
    /// This should be called periodically (e.g., in an event loop) to handle
    /// events like breakpoint hits, step completions, and program termination.
    ///
    /// # Returns
    ///
    /// Returns `Ok(Some(event))` if an event was received, `Ok(None)` if no
    /// event is available, or an error if communication failed.
    pub fn process_events(&mut self) -> Result<Option<DebugEvent>, DebugError> {
        let conn = match &mut self.connection {
            Some(c) => c,
            None => return Ok(None), // No connection, no events
        };

        match conn.try_receive_event() {
            Ok(Some(event)) => {
                self.handle_event(event.clone())?;
                Ok(Some(event))
            }
            Ok(None) => Ok(None),
            Err(e) => Err(DebugError::ConnectionError {
                message: format!("Failed to receive event: {}", e),
            }),
        }
    }

    /// Handles a debug event from the runtime.
    ///
    /// Updates internal state based on the event (e.g., updates current location
    /// when stopped, transitions to Completed on termination).
    fn handle_event(&mut self, event: DebugEvent) -> DebugResult<()> {
        match event {
            DebugEvent::Ready => {
                self.state = ExecutionState::Paused;
            }
            DebugEvent::Stopped {
                reason: _,
                line,
                file,
            } => {
                self.state = ExecutionState::Paused;
                self.current_location = Some(SourceLocation::new(
                    PathBuf::from(file),
                    line as usize,
                    1, // Column will be updated when we have more precise location info
                ));
            }
            DebugEvent::Terminated => {
                self.state = ExecutionState::Completed;
                self.current_location = None;
            }
            DebugEvent::Location {
                line,
                file,
                procedure: _,
            } => {
                self.current_location =
                    Some(SourceLocation::new(PathBuf::from(file), line as usize, 1));
            }
            DebugEvent::Error { message: _ } => {
                // Runtime error occurred - transition to Error state
                self.state = ExecutionState::Error;
            }
            _ => {
                // Other events (ProcedureEnter, ProcedureExit, VariableValue, Output)
                // don't change execution state, just provide information
            }
        }
        Ok(())
    }
}

impl Default for Debugger {
    fn default() -> Self {
        Self::default_config()
    }
}

/// Converts a byte offset to (line, column) coordinates.
fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debugger_creation() {
        let debugger = Debugger::default();
        assert_eq!(debugger.state(), ExecutionState::NotStarted);
        assert!(debugger.breakpoints().is_empty());
    }

    #[test]
    fn test_load_source_string() {
        let mut debugger = Debugger::default();
        let source = r#"
DIM x AS INTEGER
x = 5
PRINT x
"#;
        let result = debugger.load_source_string("test.bas", source.to_string());
        assert!(result.is_ok());
        assert!(debugger.get_source("test.bas").is_some());
    }

    #[test]
    fn test_add_breakpoint() {
        let mut debugger = Debugger::default();
        let source = r#"
DIM x AS INTEGER
x = 5
PRINT x
"#;
        debugger
            .load_source_string("test.bas", source.to_string())
            .unwrap();

        // Line 3 should be valid (x = 5)
        let id = debugger.add_line_breakpoint("test.bas", 3);
        assert!(id.is_ok());

        assert_eq!(debugger.breakpoints().len(), 1);
    }

    #[test]
    fn test_remove_breakpoint() {
        let mut debugger = Debugger::default();
        let id = debugger.add_function_breakpoint("MySub");

        assert_eq!(debugger.breakpoints().len(), 1);
        assert!(debugger.remove_breakpoint(id));
        assert!(debugger.breakpoints().is_empty());
    }

    #[test]
    fn test_enable_disable_breakpoint() {
        let mut debugger = Debugger::default();
        let id = debugger.add_function_breakpoint("MySub");

        assert!(debugger.breakpoints()[0].enabled);
        debugger.disable_breakpoint(id);
        assert!(!debugger.breakpoints()[0].enabled);
        debugger.enable_breakpoint(id);
        assert!(debugger.breakpoints()[0].enabled);
    }

    #[test]
    fn test_offset_to_line_col() {
        let source = "line1\nline2\nline3";
        assert_eq!(offset_to_line_col(source, 0), (1, 1));
        assert_eq!(offset_to_line_col(source, 6), (2, 1));
        assert_eq!(offset_to_line_col(source, 8), (2, 3));
    }

    #[test]
    fn test_source_location_from_span() {
        let source = "DIM x AS INTEGER\nPRINT x";
        let span = Span::new(17, 24, 2); // "PRINT x"
        let loc = SourceLocation::from_span(PathBuf::from("test.bas"), source, &span);

        assert_eq!(loc.line, 2);
        assert_eq!(loc.column, 1);
    }
}
