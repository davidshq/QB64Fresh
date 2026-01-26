//! DAP Server implementation for QB64Fresh Debugger.
//!
//! This module implements a Debug Adapter Protocol server that allows
//! VS Code and other DAP-compatible IDEs to debug QB64Fresh programs.
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────┐     ┌──────────────────┐     ┌─────────────────────┐
//! │ VS Code /   │ DAP │ DapServer        │pipe │ Compiled program    │
//! │ Cursor      │◄───►│ (this module)    │◄───►│ + debug hooks       │
//! └─────────────┘     └──────────────────┘     └─────────────────────┘
//! stdin/stdout              │
//!                  ┌────────┼────────┐
//!                  │        │        │
//!                  ▼        ▼        ▼
//!             symbols.rs  frames.rs  values.rs
//! ```

use crate::dap::{
    Capabilities, DapBreakpoint, DapStackFrame, DapVariable, Event, Request, Response, Source,
    StoppedEventBody,
};
use crate::protocol::{DebugCommand, DebugEvent, ProtocolError, StopReason};
use crate::symbols::DebugSymbols;
use crate::DebugError;

use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;

/// A stack frame entry tracked from ENTER/EXIT events.
#[derive(Debug, Clone)]
struct TrackedStackFrame {
    /// Procedure name.
    name: String,
    /// Entry line number.
    line: u32,
    /// Variables captured for this frame.
    variables: Vec<TrackedVariable>,
}

/// A variable value received from the debugee.
#[derive(Debug, Clone)]
struct TrackedVariable {
    /// Variable name.
    name: String,
    /// Type string (int, float, string).
    var_type: String,
    /// Value as string.
    value: String,
}

/// DAP server for QB64Fresh debugging.
pub struct DapServer {
    /// Debug symbols extracted from the program.
    symbols: Option<DebugSymbols>,
    /// Source files being debugged.
    source_files: HashMap<PathBuf, String>,
    /// Breakpoints set by the IDE, keyed by source path.
    breakpoints: HashMap<PathBuf, Vec<u32>>,
    /// Spawned debugee process.
    debugee: Option<Child>,
    /// Named pipe path for communication.
    pipe_path: Option<PathBuf>,
    /// Sequence number for DAP messages.
    seq: i64,
    /// Whether configuration is complete.
    configured: bool,
    /// Whether the debugee has started.
    started: bool,
    /// Program path to debug.
    program_path: Option<PathBuf>,
    /// Arguments to pass to the program.
    program_args: Vec<String>,
    /// Sender for events from the debugee.
    event_sender: Option<Sender<DebugEvent>>,
    /// Receiver for events from the debugee.
    event_receiver: Option<Receiver<DebugEvent>>,
    /// Command sender to send commands to pipe writer thread.
    command_sender: Option<Sender<DebugCommand>>,
    /// Call stack tracked from ENTER/EXIT events.
    call_stack: Vec<TrackedStackFrame>,
    /// Current stopped location (line, file).
    current_location: Option<(u32, String)>,
    /// Cached variables for the current scope.
    cached_variables: Vec<TrackedVariable>,
    /// Pending variable request (waiting for response).
    pending_var_request: Option<String>,
}

impl Default for DapServer {
    fn default() -> Self {
        Self::new()
    }
}

impl DapServer {
    /// Creates a new DAP server.
    pub fn new() -> Self {
        Self {
            symbols: None,
            source_files: HashMap::new(),
            breakpoints: HashMap::new(),
            debugee: None,
            pipe_path: None,
            seq: 0,
            configured: false,
            started: false,
            program_path: None,
            program_args: Vec::new(),
            event_sender: None,
            event_receiver: None,
            command_sender: None,
            call_stack: Vec::new(),
            current_location: None,
            cached_variables: Vec::new(),
            pending_var_request: None,
        }
    }

    /// Runs the DAP server, reading from stdin and writing to stdout.
    pub fn run(&mut self) -> Result<(), DapServerError> {
        let stdin = io::stdin();
        let stdout = io::stdout();
        let mut reader = BufReader::new(stdin.lock());
        let mut writer = stdout.lock();

        loop {
            // Read DAP message
            match self.read_message(&mut reader) {
                Ok(Some(request)) => {
                    // Process the request
                    let response = self.handle_request(request)?;
                    self.send_message(&mut writer, &response)?;

                    // Check for events from debugee
                    self.process_debugee_events(&mut writer)?;
                }
                Ok(None) => {
                    // EOF - client disconnected
                    break;
                }
                Err(e) => {
                    eprintln!("Error reading DAP message: {}", e);
                    break;
                }
            }
        }

        // Clean up
        self.terminate_debugee();
        Ok(())
    }

    /// Reads a DAP message from the input.
    fn read_message<R: BufRead>(&self, reader: &mut R) -> Result<Option<Request>, DapServerError> {
        // DAP uses HTTP-like headers followed by JSON content
        // Content-Length: <length>\r\n
        // \r\n
        // <json>

        let mut content_length: Option<usize> = None;
        let mut line = String::new();

        // Read headers
        loop {
            line.clear();
            let bytes_read = reader.read_line(&mut line)?;
            if bytes_read == 0 {
                return Ok(None); // EOF
            }

            let line = line.trim();
            if line.is_empty() {
                break; // End of headers
            }

            if let Some(length_str) = line.strip_prefix("Content-Length: ") {
                content_length =
                    Some(length_str.parse().map_err(|_| {
                        DapServerError::InvalidMessage("Bad Content-Length".into())
                    })?);
            }
        }

        let length = content_length
            .ok_or_else(|| DapServerError::InvalidMessage("Missing Content-Length".into()))?;

        // Read content
        let mut content = vec![0u8; length];
        reader.read_exact(&mut content)?;

        let json_str = String::from_utf8(content)
            .map_err(|_| DapServerError::InvalidMessage("Invalid UTF-8".into()))?;

        let request: Request = serde_json::from_str(&json_str)
            .map_err(|e| DapServerError::InvalidMessage(format!("JSON parse error: {}", e)))?;

        Ok(Some(request))
    }

    /// Sends a DAP message to the output.
    fn send_message<W: Write>(
        &mut self,
        writer: &mut W,
        response: &Response,
    ) -> Result<(), DapServerError> {
        let json = serde_json::to_string(response)
            .map_err(|e| DapServerError::InvalidMessage(format!("JSON serialize error: {}", e)))?;

        write!(writer, "Content-Length: {}\r\n\r\n{}", json.len(), json)?;
        writer.flush()?;
        Ok(())
    }

    /// Sends a DAP event to the output.
    fn send_event<W: Write>(&mut self, writer: &mut W, event: Event) -> Result<(), DapServerError> {
        self.seq += 1;
        let event_with_seq = Event {
            seq: self.seq,
            ..event
        };

        let json = serde_json::to_string(&event_with_seq)
            .map_err(|e| DapServerError::InvalidMessage(format!("JSON serialize error: {}", e)))?;

        write!(writer, "Content-Length: {}\r\n\r\n{}", json.len(), json)?;
        writer.flush()?;
        Ok(())
    }

    /// Handles a DAP request and returns a response.
    fn handle_request(&mut self, request: Request) -> Result<Response, DapServerError> {
        let command = request.command.as_str();
        let request_seq = request.seq;

        match command {
            "initialize" => self.handle_initialize(request_seq),
            "launch" => self.handle_launch(request_seq, request.arguments),
            "attach" => self.handle_attach(request_seq, request.arguments),
            "configurationDone" => self.handle_configuration_done(request_seq),
            "setBreakpoints" => self.handle_set_breakpoints(request_seq, request.arguments),
            "setFunctionBreakpoints" => {
                self.handle_set_function_breakpoints(request_seq, request.arguments)
            }
            "threads" => self.handle_threads(request_seq),
            "stackTrace" => self.handle_stack_trace(request_seq, request.arguments),
            "scopes" => self.handle_scopes(request_seq, request.arguments),
            "variables" => self.handle_variables(request_seq, request.arguments),
            "continue" => self.handle_continue(request_seq, request.arguments),
            "next" => self.handle_next(request_seq, request.arguments),
            "stepIn" => self.handle_step_in(request_seq, request.arguments),
            "stepOut" => self.handle_step_out(request_seq, request.arguments),
            "pause" => self.handle_pause(request_seq, request.arguments),
            "evaluate" => self.handle_evaluate(request_seq, request.arguments),
            "disconnect" => self.handle_disconnect(request_seq, request.arguments),
            "terminate" => self.handle_terminate(request_seq),
            _ => Ok(Response::error(
                request_seq,
                command,
                &format!("Unsupported command: {}", command),
            )),
        }
    }

    fn handle_initialize(&mut self, request_seq: i64) -> Result<Response, DapServerError> {
        let capabilities = Capabilities::qb64fresh();
        let body = serde_json::to_value(capabilities)
            .map_err(|e| DapServerError::InvalidMessage(e.to_string()))?;
        Ok(Response::success(request_seq, "initialize", Some(body)))
    }

    fn handle_launch(
        &mut self,
        request_seq: i64,
        arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        let args = arguments.unwrap_or_default();

        // Get program path
        let program = args
            .get("program")
            .and_then(|v| v.as_str())
            .ok_or_else(|| DapServerError::InvalidMessage("Missing 'program' argument".into()))?;

        self.program_path = Some(PathBuf::from(program));

        // Get program arguments
        if let Some(prog_args) = args.get("args").and_then(|v| v.as_array()) {
            self.program_args = prog_args
                .iter()
                .filter_map(|v| v.as_str())
                .map(String::from)
                .collect();
        }

        // Create named pipe for communication
        #[cfg(unix)]
        {
            let pipe_path =
                std::env::temp_dir().join(format!("qb64fresh_debug_{}", std::process::id()));
            if pipe_path.exists() {
                std::fs::remove_file(&pipe_path)?;
            }
            // Create FIFO using mkfifo command (avoids libc dependency)
            let status = std::process::Command::new("mkfifo")
                .arg(&pipe_path)
                .status();

            if let Err(e) = status {
                eprintln!("Warning: Failed to create debug pipe: {}", e);
                // Continue anyway - debugger will work without pipe
            }
            self.pipe_path = Some(pipe_path);
        }

        #[cfg(windows)]
        {
            // Windows named pipes use a different naming convention
            let pipe_path = PathBuf::from(format!(
                "\\\\.\\pipe\\qb64fresh_debug_{}",
                std::process::id()
            ));
            self.pipe_path = Some(pipe_path);
        }

        Ok(Response::success(request_seq, "launch", None))
    }

    fn handle_attach(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        // Attach mode not yet implemented
        Ok(Response::error(
            request_seq,
            "attach",
            "Attach mode not implemented. Use launch instead.",
        ))
    }

    fn handle_configuration_done(&mut self, request_seq: i64) -> Result<Response, DapServerError> {
        self.configured = true;

        // Start the debugee
        if let (Some(program), Some(pipe_path)) = (&self.program_path, &self.pipe_path) {
            // Set up communication channels
            let (event_tx, event_rx) = mpsc::channel::<DebugEvent>();
            let (cmd_tx, cmd_rx) = mpsc::channel::<DebugCommand>();

            self.event_receiver = Some(event_rx);
            self.command_sender = Some(cmd_tx);

            // Clone pipe_path for the threads
            let reader_pipe_path = pipe_path.clone();
            let writer_pipe_path = pipe_path.clone();

            // Spawn pipe reader thread
            thread::spawn(move || {
                Self::pipe_reader_thread(reader_pipe_path, event_tx);
            });

            // Spawn pipe writer thread
            thread::spawn(move || {
                Self::pipe_writer_thread(writer_pipe_path, cmd_rx);
            });

            // Start the debugee process
            let mut cmd = Command::new(program);
            cmd.args(&self.program_args)
                .env("QB64FRESH_DEBUG_PIPE", pipe_path)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped());

            match cmd.spawn() {
                Ok(child) => {
                    self.debugee = Some(child);
                    self.started = true;

                    // Initialize call stack with main
                    self.call_stack.push(TrackedStackFrame {
                        name: "main".to_string(),
                        line: 1,
                        variables: Vec::new(),
                    });
                }
                Err(e) => {
                    return Ok(Response::error(
                        request_seq,
                        "configurationDone",
                        &format!("Failed to start debugee: {}", e),
                    ));
                }
            }
        }

        Ok(Response::success(request_seq, "configurationDone", None))
    }

    /// Background thread that reads events from the debugee pipe.
    fn pipe_reader_thread(pipe_path: PathBuf, event_tx: Sender<DebugEvent>) {
        #[cfg(unix)]
        {
            use std::fs::OpenOptions;
            use std::io::BufReader;

            // Wait for the pipe to be available (debugee needs to open it first)
            for _ in 0..50 {
                if pipe_path.exists() {
                    break;
                }
                thread::sleep(std::time::Duration::from_millis(100));
            }

            // Open pipe for reading
            match OpenOptions::new().read(true).open(&pipe_path) {
                Ok(file) => {
                    let reader = BufReader::new(file);
                    for line_result in reader.lines() {
                        match line_result {
                            Ok(line) => {
                                if let Ok(event) = line.parse::<DebugEvent>() {
                                    if event_tx.send(event).is_err() {
                                        break; // Receiver dropped
                                    }
                                }
                            }
                            Err(_) => break,
                        }
                    }
                }
                Err(e) => {
                    eprintln!("DEBUG: Failed to open pipe for reading: {}", e);
                }
            }
        }

        #[cfg(windows)]
        {
            // Windows named pipe reading
            use std::fs::OpenOptions;
            use std::io::BufReader;

            // Wait for pipe to be created
            for _ in 0..50 {
                match OpenOptions::new().read(true).open(&pipe_path) {
                    Ok(file) => {
                        let reader = BufReader::new(file);
                        for line_result in reader.lines() {
                            match line_result {
                                Ok(line) => {
                                    if let Ok(event) = line.parse::<DebugEvent>() {
                                        if event_tx.send(event).is_err() {
                                            return; // Receiver dropped
                                        }
                                    }
                                }
                                Err(_) => return,
                            }
                        }
                        return;
                    }
                    Err(_) => {
                        thread::sleep(std::time::Duration::from_millis(100));
                    }
                }
            }
            eprintln!("DEBUG: Failed to open pipe for reading after timeout");
        }
    }

    /// Background thread that writes commands to the debugee pipe.
    fn pipe_writer_thread(pipe_path: PathBuf, cmd_rx: Receiver<DebugCommand>) {
        #[cfg(unix)]
        {
            use std::fs::OpenOptions;

            // Wait for the pipe to be available
            for _ in 0..50 {
                if pipe_path.exists() {
                    break;
                }
                thread::sleep(std::time::Duration::from_millis(100));
            }

            // Open pipe for writing
            match OpenOptions::new().write(true).open(&pipe_path) {
                Ok(mut file) => {
                    while let Ok(cmd) = cmd_rx.recv() {
                        let msg = format!("{}\n", cmd);
                        if file.write_all(msg.as_bytes()).is_err() {
                            break;
                        }
                        let _ = file.flush();
                    }
                }
                Err(e) => {
                    eprintln!("DEBUG: Failed to open pipe for writing: {}", e);
                }
            }
        }

        #[cfg(windows)]
        {
            use std::fs::OpenOptions;

            // Wait and open pipe for writing
            for _ in 0..50 {
                match OpenOptions::new().write(true).open(&pipe_path) {
                    Ok(mut file) => {
                        while let Ok(cmd) = cmd_rx.recv() {
                            let msg = format!("{}\n", cmd);
                            if file.write_all(msg.as_bytes()).is_err() {
                                return;
                            }
                            let _ = file.flush();
                        }
                        return;
                    }
                    Err(_) => {
                        thread::sleep(std::time::Duration::from_millis(100));
                    }
                }
            }
            eprintln!("DEBUG: Failed to open pipe for writing after timeout");
        }
    }

    fn handle_set_breakpoints(
        &mut self,
        request_seq: i64,
        arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        let args = arguments.unwrap_or_default();

        let source_path = args
            .get("source")
            .and_then(|s| s.get("path"))
            .and_then(|p| p.as_str())
            .map(PathBuf::from);

        let breakpoint_lines: Vec<u32> = args
            .get("breakpoints")
            .and_then(|b| b.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|bp| bp.get("line").and_then(|l| l.as_u64()))
                    .map(|l| l as u32)
                    .collect()
            })
            .unwrap_or_default();

        // Store breakpoints
        if let Some(path) = &source_path {
            // Collect old breakpoints to remove (to avoid borrow conflict)
            let old_lines: Vec<u32> = self
                .breakpoints
                .get(path)
                .map(|v| v.clone())
                .unwrap_or_default();

            // Send commands to debugee to update breakpoints
            // First, remove old breakpoints
            for line in old_lines {
                self.send_to_debugee(DebugCommand::RemoveBreakpoint { line });
            }

            // Add new breakpoints
            for &line in &breakpoint_lines {
                self.send_to_debugee(DebugCommand::AddBreakpoint { line });
            }

            self.breakpoints
                .insert(path.clone(), breakpoint_lines.clone());
        }

        // Build response with verified breakpoints
        let verified_breakpoints: Vec<DapBreakpoint> = breakpoint_lines
            .iter()
            .map(|&line| DapBreakpoint {
                id: Some(line as i64),
                verified: true,
                message: None,
                source: source_path.as_ref().map(|p| Source {
                    name: p.file_name().map(|s| s.to_string_lossy().to_string()),
                    path: Some(p.to_string_lossy().to_string()),
                    source_reference: None,
                    presentation_hint: None,
                    origin: None,
                    sources: None,
                }),
                line: Some(line as i64),
                column: None,
                end_line: None,
                end_column: None,
            })
            .collect();

        let body = serde_json::json!({
            "breakpoints": verified_breakpoints
        });

        Ok(Response::success(request_seq, "setBreakpoints", Some(body)))
    }

    fn handle_set_function_breakpoints(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        // Function breakpoints not yet implemented
        let body = serde_json::json!({
            "breakpoints": []
        });
        Ok(Response::success(
            request_seq,
            "setFunctionBreakpoints",
            Some(body),
        ))
    }

    fn handle_threads(&mut self, request_seq: i64) -> Result<Response, DapServerError> {
        // BASIC is single-threaded, so we always report a single main thread
        let body = serde_json::json!({
            "threads": [{
                "id": 1,
                "name": "Main Thread"
            }]
        });
        Ok(Response::success(request_seq, "threads", Some(body)))
    }

    fn handle_stack_trace(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        // Build stack frames from tracked call stack (most recent first)
        let frames: Vec<DapStackFrame> = self
            .call_stack
            .iter()
            .rev()
            .enumerate()
            .map(|(i, frame)| {
                // Get source file - use current location file or program path
                let source_path = self
                    .current_location
                    .as_ref()
                    .map(|(_, f)| PathBuf::from(f))
                    .or_else(|| self.program_path.clone());

                DapStackFrame {
                    id: (i + 1) as i64,
                    name: frame.name.clone(),
                    source: source_path.as_ref().map(|p| Source {
                        name: p.file_name().map(|s| s.to_string_lossy().to_string()),
                        path: Some(p.to_string_lossy().to_string()),
                        source_reference: None,
                        presentation_hint: None,
                        origin: None,
                        sources: None,
                    }),
                    line: frame.line as i64,
                    column: 1,
                    end_line: None,
                    end_column: None,
                    module_id: None,
                    presentation_hint: None,
                }
            })
            .collect();

        let body = serde_json::json!({
            "stackFrames": frames,
            "totalFrames": frames.len()
        });

        Ok(Response::success(request_seq, "stackTrace", Some(body)))
    }

    fn handle_scopes(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        // Return local and global scopes
        let body = serde_json::json!({
            "scopes": [
                {
                    "name": "Local",
                    "variablesReference": 1,
                    "expensive": false
                },
                {
                    "name": "Global",
                    "variablesReference": 2,
                    "expensive": false
                }
            ]
        });

        Ok(Response::success(request_seq, "scopes", Some(body)))
    }

    fn handle_variables(
        &mut self,
        request_seq: i64,
        arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        let args = arguments.unwrap_or_default();
        let _variables_ref = args
            .get("variablesReference")
            .and_then(|v| v.as_i64())
            .unwrap_or(0);

        // Convert cached variables to DAP format
        let variables: Vec<DapVariable> = self
            .cached_variables
            .iter()
            .map(|v| {
                let type_str = match v.var_type.as_str() {
                    "int" => "Integer",
                    "float" => "Double",
                    "string" => "String",
                    _ => &v.var_type,
                };

                DapVariable {
                    name: v.name.clone(),
                    value: v.value.clone(),
                    var_type: Some(type_str.to_string()),
                    presentation_hint: None,
                    evaluate_name: Some(v.name.clone()),
                    variables_reference: 0, // No nested variables for simple types
                    named_variables: None,
                    indexed_variables: None,
                    memory_reference: None,
                }
            })
            .collect();

        let body = serde_json::json!({
            "variables": variables
        });

        Ok(Response::success(request_seq, "variables", Some(body)))
    }

    fn handle_continue(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        self.send_to_debugee(DebugCommand::Continue);
        let body = serde_json::json!({
            "allThreadsContinued": true
        });
        Ok(Response::success(request_seq, "continue", Some(body)))
    }

    fn handle_next(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        self.send_to_debugee(DebugCommand::StepOver);
        Ok(Response::success(request_seq, "next", None))
    }

    fn handle_step_in(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        self.send_to_debugee(DebugCommand::StepInto);
        Ok(Response::success(request_seq, "stepIn", None))
    }

    fn handle_step_out(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        self.send_to_debugee(DebugCommand::StepOut);
        Ok(Response::success(request_seq, "stepOut", None))
    }

    fn handle_pause(
        &mut self,
        request_seq: i64,
        _arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        self.send_to_debugee(DebugCommand::Pause);
        Ok(Response::success(request_seq, "pause", None))
    }

    fn handle_evaluate(
        &mut self,
        request_seq: i64,
        arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        let args = arguments.unwrap_or_default();
        let expression = args
            .get("expression")
            .and_then(|e| e.as_str())
            .unwrap_or("");

        // TODO: Implement expression evaluation
        let body = serde_json::json!({
            "result": format!("Cannot evaluate: {}", expression),
            "variablesReference": 0
        });

        Ok(Response::success(request_seq, "evaluate", Some(body)))
    }

    fn handle_disconnect(
        &mut self,
        request_seq: i64,
        arguments: Option<serde_json::Value>,
    ) -> Result<Response, DapServerError> {
        let args = arguments.unwrap_or_default();
        let terminate = args
            .get("terminateDebuggee")
            .and_then(|t| t.as_bool())
            .unwrap_or(true);

        if terminate {
            self.terminate_debugee();
        }

        Ok(Response::success(request_seq, "disconnect", None))
    }

    fn handle_terminate(&mut self, request_seq: i64) -> Result<Response, DapServerError> {
        self.send_to_debugee(DebugCommand::Terminate);
        self.terminate_debugee();
        Ok(Response::success(request_seq, "terminate", None))
    }

    /// Sends a command to the debugee process.
    fn send_to_debugee(&mut self, command: DebugCommand) {
        if let Some(sender) = &self.command_sender {
            if let Err(e) = sender.send(command.clone()) {
                eprintln!("DEBUG: Failed to send command: {}", e);
            }
        } else {
            // Fallback for when pipe isn't set up yet
            eprintln!("DEBUG: Would send command (pipe not ready): {}", command);
        }
    }

    /// Processes events from the debugee and sends them to the IDE.
    fn process_debugee_events<W: Write>(&mut self, writer: &mut W) -> Result<(), DapServerError> {
        // Collect events first to avoid borrow conflict
        let events: Vec<DebugEvent> = if let Some(receiver) = &self.event_receiver {
            receiver.try_iter().collect()
        } else {
            Vec::new()
        };

        // Now process collected events
        for event in events {
            match event {
                DebugEvent::Ready => {
                    self.send_event(writer, Event::new("initialized", None))?;
                }
                DebugEvent::Stopped { reason, line, file } => {
                    // Update current location
                    self.current_location = Some((line, file.clone()));

                    // Update the top frame's line
                    if let Some(frame) = self.call_stack.last_mut() {
                        frame.line = line;
                    }

                    let dap_reason = match reason {
                        StopReason::Breakpoint => "breakpoint",
                        StopReason::Step => "step",
                        StopReason::Pause => "pause",
                        StopReason::Entry => "entry",
                        StopReason::Exception => "exception",
                    };
                    let body = StoppedEventBody {
                        reason: dap_reason.to_string(),
                        description: None,
                        thread_id: Some(1),
                        preserve_focus_hint: None,
                        text: None,
                        all_threads_stopped: Some(true),
                        hit_breakpoint_ids: None,
                    };
                    let body_value = serde_json::to_value(body)
                        .map_err(|e| DapServerError::InvalidMessage(e.to_string()))?;
                    self.send_event(writer, Event::new("stopped", Some(body_value)))?;
                }
                DebugEvent::Terminated => {
                    self.send_event(writer, Event::new("terminated", None))?;
                }
                DebugEvent::ProcedureEnter { name, line } => {
                    // Push a new frame onto the call stack
                    self.call_stack.push(TrackedStackFrame {
                        name,
                        line,
                        variables: Vec::new(),
                    });
                }
                DebugEvent::ProcedureExit { name } => {
                    // Pop frame from call stack (if it matches)
                    if let Some(frame) = self.call_stack.last() {
                        if frame.name == name {
                            self.call_stack.pop();
                        }
                    }
                }
                DebugEvent::VariableValue {
                    name,
                    var_type,
                    value,
                } => {
                    // Cache the variable value
                    self.cached_variables.push(TrackedVariable {
                        name,
                        var_type,
                        value,
                    });
                }
                DebugEvent::Location {
                    line,
                    file,
                    procedure,
                } => {
                    // Update current location
                    self.current_location = Some((line, file));

                    // Update top frame name if it changed
                    if let Some(frame) = self.call_stack.last_mut() {
                        frame.name = procedure;
                        frame.line = line;
                    }
                }
                DebugEvent::Output { text } => {
                    // Forward program output to the IDE
                    let body = serde_json::json!({
                        "category": "stdout",
                        "output": text
                    });
                    self.send_event(writer, Event::new("output", Some(body)))?;
                }
                DebugEvent::Error { message } => {
                    // Forward error to the IDE
                    let body = serde_json::json!({
                        "category": "stderr",
                        "output": format!("Error: {}\n", message)
                    });
                    self.send_event(writer, Event::new("output", Some(body)))?;
                }
            }
        }
        Ok(())
    }

    /// Terminates the debugee process.
    fn terminate_debugee(&mut self) {
        if let Some(mut child) = self.debugee.take() {
            let _ = child.kill();
            let _ = child.wait();
        }

        // Clean up pipe
        #[cfg(unix)]
        if let Some(pipe_path) = &self.pipe_path {
            let _ = std::fs::remove_file(pipe_path);
        }
        self.pipe_path = None;
    }
}

/// Errors that can occur in the DAP server.
#[derive(Debug)]
pub enum DapServerError {
    /// I/O error.
    Io(io::Error),
    /// Invalid DAP message.
    InvalidMessage(String),
    /// Protocol error.
    Protocol(ProtocolError),
    /// Debug error.
    Debug(DebugError),
}

impl std::fmt::Display for DapServerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DapServerError::Io(e) => write!(f, "I/O error: {}", e),
            DapServerError::InvalidMessage(msg) => write!(f, "Invalid message: {}", msg),
            DapServerError::Protocol(e) => write!(f, "Protocol error: {}", e),
            DapServerError::Debug(e) => write!(f, "Debug error: {}", e),
        }
    }
}

impl std::error::Error for DapServerError {}

impl From<io::Error> for DapServerError {
    fn from(e: io::Error) -> Self {
        DapServerError::Io(e)
    }
}

impl From<ProtocolError> for DapServerError {
    fn from(e: ProtocolError) -> Self {
        DapServerError::Protocol(e)
    }
}

impl From<DebugError> for DapServerError {
    fn from(e: DebugError) -> Self {
        DapServerError::Debug(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_server_creation() {
        let server = DapServer::new();
        assert!(!server.configured);
        assert!(!server.started);
        assert!(server.debugee.is_none());
    }
}
