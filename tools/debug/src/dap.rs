//! Debug Adapter Protocol (DAP) types for IDE integration.
//!
//! This module implements the Debug Adapter Protocol, a JSON-based protocol
//! used by VS Code and other IDEs for debugger integration. DAP provides:
//!
//! - **Standardized communication**: Between IDE and debugger
//! - **Event-driven updates**: Breakpoint hits, program state changes
//! - **Variable inspection**: Hierarchical variable view
//! - **Source mapping**: Navigate between compiled code and source
//!
//! Reference: <https://microsoft.github.io/debug-adapter-protocol/>
//!
//! Note: This module defines the protocol types. A full DAP server would
//! also need async networking code to handle the communication.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ─────────────────────────────────────────────────────────────────────────────
// Protocol Message Types
// ─────────────────────────────────────────────────────────────────────────────

/// Base type for all DAP messages.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum DapMessage {
    /// Request message from client to adapter.
    #[serde(rename = "request")]
    Request(Request),
    /// Response message from adapter to client.
    #[serde(rename = "response")]
    Response(Response),
    /// Event message from adapter to client.
    #[serde(rename = "event")]
    Event(Event),
}

/// A request from the client (IDE) to the debug adapter.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    /// Sequence number (used to match responses).
    pub seq: i64,
    /// The command to execute.
    pub command: String,
    /// Command-specific arguments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arguments: Option<serde_json::Value>,
}

/// A response from the debug adapter to the client.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    /// Sequence number of this response.
    pub seq: i64,
    /// Sequence number of the request this responds to.
    pub request_seq: i64,
    /// The command this is a response to.
    pub command: String,
    /// Whether the request was successful.
    pub success: bool,
    /// Error message (if success is false).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    /// Command-specific result.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<serde_json::Value>,
}

impl Response {
    /// Creates a successful response.
    pub fn success(request_seq: i64, command: &str, body: Option<serde_json::Value>) -> Self {
        Self {
            seq: 0, // Will be set by sender
            request_seq,
            command: command.to_string(),
            success: true,
            message: None,
            body,
        }
    }

    /// Creates an error response.
    pub fn error(request_seq: i64, command: &str, message: &str) -> Self {
        Self {
            seq: 0,
            request_seq,
            command: command.to_string(),
            success: false,
            message: Some(message.to_string()),
            body: None,
        }
    }
}

/// An event from the debug adapter to the client.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    /// Sequence number.
    pub seq: i64,
    /// The event type.
    pub event: String,
    /// Event-specific body.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<serde_json::Value>,
}

impl Event {
    /// Creates a new event.
    pub fn new(event: &str, body: Option<serde_json::Value>) -> Self {
        Self {
            seq: 0,
            event: event.to_string(),
            body,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// DAP Types
// ─────────────────────────────────────────────────────────────────────────────

/// Capabilities of the debug adapter.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Capabilities {
    /// Supports configuration done request.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_configuration_done_request: Option<bool>,
    /// Supports function breakpoints.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_function_breakpoints: Option<bool>,
    /// Supports conditional breakpoints.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_conditional_breakpoints: Option<bool>,
    /// Supports hit condition on breakpoints.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_hit_conditional_breakpoints: Option<bool>,
    /// Supports evaluate for hover.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_evaluate_for_hovers: Option<bool>,
    /// Exception breakpoint filters.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exception_breakpoint_filters: Option<Vec<ExceptionBreakpointsFilter>>,
    /// Supports step back.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_step_back: Option<bool>,
    /// Supports setting variable value.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_set_variable: Option<bool>,
    /// Supports restart frame.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_restart_frame: Option<bool>,
    /// Supports goto targets.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_goto_targets_request: Option<bool>,
    /// Supports step in targets.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_step_in_targets_request: Option<bool>,
    /// Supports completions.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_completions_request: Option<bool>,
    /// Supports modules.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_modules_request: Option<bool>,
    /// Supports terminate.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_terminate_request: Option<bool>,
    /// Supports terminate threads.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_terminate_threads_request: Option<bool>,
    /// Supports loaded sources.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_loaded_sources_request: Option<bool>,
    /// Supports data breakpoints.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_data_breakpoints: Option<bool>,
    /// Supports read memory.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_read_memory_request: Option<bool>,
    /// Supports disassemble.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_disassemble_request: Option<bool>,
}

impl Capabilities {
    /// Creates QB64Fresh debugger capabilities.
    pub fn qb64fresh() -> Self {
        Self {
            supports_configuration_done_request: Some(true),
            supports_function_breakpoints: Some(true),
            supports_conditional_breakpoints: Some(true),
            supports_hit_conditional_breakpoints: Some(true),
            supports_evaluate_for_hovers: Some(true),
            supports_set_variable: Some(true),
            supports_terminate_request: Some(true),
            supports_loaded_sources_request: Some(true),
            exception_breakpoint_filters: Some(vec![ExceptionBreakpointsFilter {
                filter: "runtime".to_string(),
                label: "Runtime Errors".to_string(),
                description: Some("Break on runtime errors".to_string()),
                default: Some(true),
                supports_condition: Some(false),
                condition_description: None,
            }]),
            ..Default::default()
        }
    }
}

/// An exception breakpoints filter.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExceptionBreakpointsFilter {
    /// Filter ID.
    pub filter: String,
    /// Human-readable label.
    pub label: String,
    /// Description.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Default enabled state.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default: Option<bool>,
    /// Whether condition is supported.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_condition: Option<bool>,
    /// Condition description.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub condition_description: Option<String>,
}

/// A source file reference.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Source {
    /// Source name (short).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Full path to source.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    /// Source reference (for synthetic sources).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_reference: Option<i64>,
    /// Presentation hint.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub presentation_hint: Option<String>,
    /// Origin description.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub origin: Option<String>,
    /// Additional sources.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sources: Option<Vec<Source>>,
}

impl Source {
    /// Creates a source from a file path.
    pub fn from_path(path: &str) -> Self {
        Self {
            name: std::path::Path::new(path)
                .file_name()
                .map(|s| s.to_string_lossy().into_owned()),
            path: Some(path.to_string()),
            source_reference: None,
            presentation_hint: None,
            origin: None,
            sources: None,
        }
    }
}

/// A breakpoint in DAP format.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DapBreakpoint {
    /// Breakpoint ID.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<i64>,
    /// Whether the breakpoint is verified.
    pub verified: bool,
    /// Message about breakpoint state.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    /// Source location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    /// Line number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<i64>,
    /// Column number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub column: Option<i64>,
    /// End line.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_line: Option<i64>,
    /// End column.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_column: Option<i64>,
}

/// A stack frame in DAP format.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DapStackFrame {
    /// Frame ID.
    pub id: i64,
    /// Frame name.
    pub name: String,
    /// Source location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    /// Line number.
    pub line: i64,
    /// Column number.
    pub column: i64,
    /// End line.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_line: Option<i64>,
    /// End column.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_column: Option<i64>,
    /// Module ID.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub module_id: Option<serde_json::Value>,
    /// Presentation hint.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub presentation_hint: Option<String>,
}

/// A scope in DAP format (for variable grouping).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DapScope {
    /// Scope name.
    pub name: String,
    /// Presentation hint.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub presentation_hint: Option<String>,
    /// Variables reference (handle for fetching variables).
    pub variables_reference: i64,
    /// Named variables count.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub named_variables: Option<i64>,
    /// Indexed variables count.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed_variables: Option<i64>,
    /// Whether variables are expensive to fetch.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expensive: Option<bool>,
    /// Source location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    /// Line number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<i64>,
    /// Column number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub column: Option<i64>,
    /// End line.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_line: Option<i64>,
    /// End column.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_column: Option<i64>,
}

/// A variable in DAP format.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DapVariable {
    /// Variable name.
    pub name: String,
    /// Variable value as string.
    pub value: String,
    /// Variable type.
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub var_type: Option<String>,
    /// Presentation hint.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub presentation_hint: Option<VariablePresentationHint>,
    /// Evaluate name (for watch).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub evaluate_name: Option<String>,
    /// Variables reference (for structured types).
    pub variables_reference: i64,
    /// Named variables count.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub named_variables: Option<i64>,
    /// Indexed variables count (for arrays).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed_variables: Option<i64>,
    /// Memory reference (for memory view).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory_reference: Option<String>,
}

/// Presentation hints for variables.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VariablePresentationHint {
    /// Kind of variable.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,
    /// Attributes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attributes: Option<Vec<String>>,
    /// Visibility.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visibility: Option<String>,
}

/// A thread in DAP format.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DapThread {
    /// Thread ID.
    pub id: i64,
    /// Thread name.
    pub name: String,
}

// ─────────────────────────────────────────────────────────────────────────────
// Stop Reasons (Events)
// ─────────────────────────────────────────────────────────────────────────────

/// Reason why execution stopped.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum StopReason {
    /// Hit a breakpoint.
    Breakpoint,
    /// Step completed.
    Step,
    /// Exception occurred.
    Exception,
    /// Pause requested.
    Pause,
    /// Entry point.
    Entry,
    /// Goto completed.
    Goto,
    /// Function breakpoint hit.
    FunctionBreakpoint,
    /// Data breakpoint hit.
    DataBreakpoint,
}

/// Body of a stopped event.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StoppedEventBody {
    /// Reason for stopping.
    pub reason: String,
    /// Description.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Thread that stopped.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thread_id: Option<i64>,
    /// Preserve focus hint.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub preserve_focus_hint: Option<bool>,
    /// Additional text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    /// All threads stopped.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub all_threads_stopped: Option<bool>,
    /// Hit breakpoint IDs.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hit_breakpoint_ids: Option<Vec<i64>>,
}

impl StoppedEventBody {
    /// Creates a stopped event for a breakpoint hit.
    pub fn breakpoint(thread_id: i64, breakpoint_ids: Vec<i64>) -> Self {
        Self {
            reason: "breakpoint".to_string(),
            description: Some("Breakpoint hit".to_string()),
            thread_id: Some(thread_id),
            preserve_focus_hint: None,
            text: None,
            all_threads_stopped: Some(true),
            hit_breakpoint_ids: Some(breakpoint_ids),
        }
    }

    /// Creates a stopped event for a step completion.
    pub fn step(thread_id: i64) -> Self {
        Self {
            reason: "step".to_string(),
            description: Some("Step completed".to_string()),
            thread_id: Some(thread_id),
            preserve_focus_hint: None,
            text: None,
            all_threads_stopped: Some(true),
            hit_breakpoint_ids: None,
        }
    }

    /// Creates a stopped event for a pause.
    pub fn pause(thread_id: i64) -> Self {
        Self {
            reason: "pause".to_string(),
            description: Some("Paused".to_string()),
            thread_id: Some(thread_id),
            preserve_focus_hint: None,
            text: None,
            all_threads_stopped: Some(true),
            hit_breakpoint_ids: None,
        }
    }

    /// Creates a stopped event for entry.
    pub fn entry(thread_id: i64) -> Self {
        Self {
            reason: "entry".to_string(),
            description: Some("Stopped on entry".to_string()),
            thread_id: Some(thread_id),
            preserve_focus_hint: None,
            text: None,
            all_threads_stopped: Some(true),
            hit_breakpoint_ids: None,
        }
    }

    /// Creates a stopped event for an exception.
    pub fn exception(thread_id: i64, description: &str) -> Self {
        Self {
            reason: "exception".to_string(),
            description: Some(description.to_string()),
            thread_id: Some(thread_id),
            preserve_focus_hint: None,
            text: Some(description.to_string()),
            all_threads_stopped: Some(true),
            hit_breakpoint_ids: None,
        }
    }
}

/// Body of an output event.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OutputEventBody {
    /// Output category.
    pub category: Option<String>,
    /// Output text.
    pub output: String,
    /// Output group.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,
    /// Variables reference.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub variables_reference: Option<i64>,
    /// Source location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    /// Line number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<i64>,
    /// Column number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub column: Option<i64>,
}

impl OutputEventBody {
    /// Creates a console output event.
    pub fn console(text: &str) -> Self {
        Self {
            category: Some("console".to_string()),
            output: text.to_string(),
            group: None,
            variables_reference: None,
            source: None,
            line: None,
            column: None,
        }
    }

    /// Creates a stdout output event.
    pub fn stdout(text: &str) -> Self {
        Self {
            category: Some("stdout".to_string()),
            output: text.to_string(),
            group: None,
            variables_reference: None,
            source: None,
            line: None,
            column: None,
        }
    }

    /// Creates a stderr output event.
    pub fn stderr(text: &str) -> Self {
        Self {
            category: Some("stderr".to_string()),
            output: text.to_string(),
            group: None,
            variables_reference: None,
            source: None,
            line: None,
            column: None,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Request Arguments
// ─────────────────────────────────────────────────────────────────────────────

/// Arguments for the initialize request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeRequestArguments {
    /// Client ID.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_id: Option<String>,
    /// Client name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_name: Option<String>,
    /// Adapter ID.
    pub adapter_id: String,
    /// Locale.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub locale: Option<String>,
    /// Lines start at 1.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lines_start_at1: Option<bool>,
    /// Columns start at 1.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub columns_start_at1: Option<bool>,
    /// Path format.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path_format: Option<String>,
    /// Supports variable type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_variable_type: Option<bool>,
    /// Supports variable paging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_variable_paging: Option<bool>,
    /// Supports run in terminal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_run_in_terminal_request: Option<bool>,
    /// Supports memory references.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supports_memory_references: Option<bool>,
}

/// Arguments for the launch request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LaunchRequestArguments {
    /// Don't debug, just run.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub no_debug: Option<bool>,
    /// Program to launch.
    pub program: String,
    /// Arguments to pass to the program.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub args: Option<Vec<String>>,
    /// Working directory.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cwd: Option<String>,
    /// Environment variables.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub env: Option<HashMap<String, String>>,
    /// Stop on entry.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stop_on_entry: Option<bool>,
}

/// Arguments for set breakpoints request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetBreakpointsArguments {
    /// Source file.
    pub source: Source,
    /// Breakpoints to set.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub breakpoints: Option<Vec<SourceBreakpoint>>,
    /// Lines (deprecated, use breakpoints).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lines: Option<Vec<i64>>,
    /// Source modified flag.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_modified: Option<bool>,
}

/// A source breakpoint.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceBreakpoint {
    /// Line number.
    pub line: i64,
    /// Column number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub column: Option<i64>,
    /// Condition expression.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub condition: Option<String>,
    /// Hit condition.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hit_condition: Option<String>,
    /// Log message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub log_message: Option<String>,
}

/// Arguments for the stackTrace request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StackTraceArguments {
    /// Thread ID.
    pub thread_id: i64,
    /// Start frame index.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_frame: Option<i64>,
    /// Number of frames to return.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub levels: Option<i64>,
    /// Format options.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<StackFrameFormat>,
}

/// Stack frame format options.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StackFrameFormat {
    /// Include parameters.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<bool>,
    /// Include parameter types.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameter_types: Option<bool>,
    /// Include parameter names.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameter_names: Option<bool>,
    /// Include parameter values.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameter_values: Option<bool>,
    /// Include line numbers.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<bool>,
    /// Include module.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub module: Option<bool>,
    /// Include hex values.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_all: Option<bool>,
}

/// Arguments for the scopes request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ScopesArguments {
    /// Frame ID.
    pub frame_id: i64,
}

/// Arguments for the variables request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VariablesArguments {
    /// Variables reference (from scope or parent variable).
    pub variables_reference: i64,
    /// Filter (indexed or named).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter: Option<String>,
    /// Start index for paging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start: Option<i64>,
    /// Number of variables to return.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub count: Option<i64>,
    /// Format options.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<ValueFormat>,
}

/// Value format options.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ValueFormat {
    /// Use hex format.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hex: Option<bool>,
}

/// Arguments for the evaluate request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EvaluateArguments {
    /// Expression to evaluate.
    pub expression: String,
    /// Frame ID for context.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub frame_id: Option<i64>,
    /// Context (watch, repl, hover).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<String>,
    /// Format options.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<ValueFormat>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capabilities_serialization() {
        let caps = Capabilities::qb64fresh();
        let json = serde_json::to_string(&caps).unwrap();
        assert!(json.contains("supportsConfigurationDoneRequest"));
        assert!(json.contains("supportsFunctionBreakpoints"));
    }

    #[test]
    fn test_source_from_path() {
        let source = Source::from_path("/home/user/test.bas");
        assert_eq!(source.name, Some("test.bas".to_string()));
        assert_eq!(source.path, Some("/home/user/test.bas".to_string()));
    }

    #[test]
    fn test_stopped_event() {
        let body = StoppedEventBody::breakpoint(1, vec![1, 2]);
        assert_eq!(body.reason, "breakpoint");
        assert_eq!(body.thread_id, Some(1));
        assert_eq!(body.hit_breakpoint_ids, Some(vec![1, 2]));
    }

    #[test]
    fn test_output_event() {
        let body = OutputEventBody::stdout("Hello, World!");
        assert_eq!(body.category, Some("stdout".to_string()));
        assert_eq!(body.output, "Hello, World!");
    }

    #[test]
    fn test_response_creation() {
        let success = Response::success(1, "stackTrace", None);
        assert!(success.success);
        assert_eq!(success.request_seq, 1);

        let error = Response::error(2, "evaluate", "Cannot evaluate expression");
        assert!(!error.success);
        assert_eq!(
            error.message,
            Some("Cannot evaluate expression".to_string())
        );
    }
}
