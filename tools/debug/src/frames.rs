//! Call stack frame structures for the debugger.
//!
//! This module defines the representation of call stack frames during debugging.
//! It provides:
//!
//! - **Stack frames**: Represent each active function/sub call
//! - **Call stack**: The complete list of frames from current location to main
//! - **Frame navigation**: Move between frames to inspect local variables
//!
//! Note: This module defines the *representation* of stack frames. The actual
//! capture of frames from a running program requires runtime integration.

use crate::symbols::DebugScopeId;
use crate::values::VariableInfo;
use crate::SourceLocation;

/// A unique identifier for a stack frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FrameId(pub usize);

impl FrameId {
    /// The frame ID for the main program.
    pub const MAIN: FrameId = FrameId(0);
}

/// A single stack frame representing an active procedure call.
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// Unique frame identifier (0 = innermost/current frame).
    pub id: FrameId,
    /// Frame name (procedure name, or "<main>" for module level).
    pub name: String,
    /// Source location where execution is paused in this frame.
    pub location: SourceLocation,
    /// The scope ID for this frame's local variables.
    pub scope_id: DebugScopeId,
    /// Local variables in this frame (populated on demand).
    pub locals: Vec<VariableInfo>,
    /// Arguments passed to this frame (for SUB/FUNCTION).
    pub arguments: Vec<VariableInfo>,
    /// Return address (line to return to in caller).
    pub return_location: Option<SourceLocation>,
    /// Module name/file containing this frame.
    pub module: String,
}

impl StackFrame {
    /// Creates a new stack frame.
    pub fn new(
        id: FrameId,
        name: impl Into<String>,
        location: SourceLocation,
        scope_id: DebugScopeId,
    ) -> Self {
        Self {
            id,
            name: name.into(),
            location: location.clone(),
            scope_id,
            locals: Vec::new(),
            arguments: Vec::new(),
            return_location: None,
            module: location.file.display().to_string(),
        }
    }

    /// Creates a frame for the main program.
    pub fn main(location: SourceLocation) -> Self {
        Self::new(FrameId::MAIN, "<main>", location, DebugScopeId::GLOBAL)
    }

    /// Returns true if this is the main program frame.
    pub fn is_main(&self) -> bool {
        self.name == "<main>"
    }

    /// Returns the function/sub name, or None if this is the main frame.
    pub fn procedure_name(&self) -> Option<&str> {
        if self.is_main() {
            None
        } else {
            Some(&self.name)
        }
    }

    /// Formats the frame for display in a stack trace.
    pub fn format_trace(&self) -> String {
        format!(
            "#{} {} at {}:{}",
            self.id.0,
            self.name,
            self.location.file.display(),
            self.location.line
        )
    }

    /// Formats a detailed view of the frame.
    pub fn format_detail(&self) -> String {
        let mut result = self.format_trace();

        if !self.arguments.is_empty() {
            result.push_str("\n  Arguments:");
            for arg in &self.arguments {
                result.push_str(&format!("\n    {}", arg));
            }
        }

        if !self.locals.is_empty() {
            result.push_str("\n  Locals:");
            for local in &self.locals {
                result.push_str(&format!("\n    {}", local));
            }
        }

        result
    }
}

/// The complete call stack.
#[derive(Debug, Clone)]
pub struct CallStack {
    /// Stack frames, from innermost (current) to outermost (main).
    frames: Vec<StackFrame>,
    /// Currently selected frame for inspection.
    selected_frame: FrameId,
}

impl CallStack {
    /// Creates a new empty call stack.
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            selected_frame: FrameId(0),
        }
    }

    /// Creates a call stack with a single main frame.
    pub fn with_main(location: SourceLocation) -> Self {
        let mut stack = Self::new();
        stack.push(StackFrame::main(location));
        stack
    }

    /// Returns the number of frames in the stack.
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Returns true if the stack is empty.
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Pushes a new frame onto the stack (entering a procedure).
    pub fn push(&mut self, mut frame: StackFrame) {
        frame.id = FrameId(self.frames.len());
        self.frames.push(frame);
    }

    /// Pops the topmost frame from the stack (returning from a procedure).
    pub fn pop(&mut self) -> Option<StackFrame> {
        let frame = self.frames.pop();

        // Adjust selected frame if necessary
        if !self.frames.is_empty() && self.selected_frame.0 >= self.frames.len() {
            self.selected_frame = FrameId(self.frames.len() - 1);
        }

        frame
    }

    /// Returns the current (innermost) frame.
    pub fn current(&self) -> Option<&StackFrame> {
        self.frames.last()
    }

    /// Returns a mutable reference to the current frame.
    pub fn current_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames.last_mut()
    }

    /// Returns the currently selected frame for inspection.
    pub fn selected(&self) -> Option<&StackFrame> {
        self.frames
            .get(self.frames.len().saturating_sub(1 + self.selected_frame.0))
    }

    /// Returns a mutable reference to the selected frame.
    pub fn selected_mut(&mut self) -> Option<&mut StackFrame> {
        let idx = self.frames.len().saturating_sub(1 + self.selected_frame.0);
        self.frames.get_mut(idx)
    }

    /// Selects a frame by ID for inspection.
    pub fn select(&mut self, frame_id: FrameId) -> bool {
        if frame_id.0 < self.frames.len() {
            self.selected_frame = frame_id;
            true
        } else {
            false
        }
    }

    /// Moves selection up the stack (toward caller).
    pub fn select_up(&mut self) -> bool {
        if self.selected_frame.0 + 1 < self.frames.len() {
            self.selected_frame = FrameId(self.selected_frame.0 + 1);
            true
        } else {
            false
        }
    }

    /// Moves selection down the stack (toward callee).
    pub fn select_down(&mut self) -> bool {
        if self.selected_frame.0 > 0 {
            self.selected_frame = FrameId(self.selected_frame.0 - 1);
            true
        } else {
            false
        }
    }

    /// Returns the selected frame ID.
    pub fn selected_id(&self) -> FrameId {
        self.selected_frame
    }

    /// Returns an iterator over all frames from innermost to outermost.
    pub fn iter(&self) -> impl Iterator<Item = &StackFrame> {
        self.frames.iter().rev()
    }

    /// Returns all frames as a slice (outermost to innermost order).
    pub fn frames(&self) -> &[StackFrame] {
        &self.frames
    }

    /// Returns the frame at a specific ID.
    pub fn get(&self, id: FrameId) -> Option<&StackFrame> {
        self.frames.get(self.frames.len().saturating_sub(1 + id.0))
    }

    /// Returns a mutable reference to the frame at a specific ID.
    pub fn get_mut(&mut self, id: FrameId) -> Option<&mut StackFrame> {
        let idx = self.frames.len().saturating_sub(1 + id.0);
        self.frames.get_mut(idx)
    }

    /// Formats the entire stack trace.
    ///
    /// Frame #0 is the innermost (current) frame, higher numbers are callers.
    pub fn format_trace(&self) -> String {
        self.iter()
            .enumerate()
            .map(|(i, f)| {
                format!(
                    "#{} {} at {}:{}",
                    i,
                    f.name,
                    f.location.file.display(),
                    f.location.line
                )
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Clears all frames from the stack.
    pub fn clear(&mut self) {
        self.frames.clear();
        self.selected_frame = FrameId(0);
    }
}

impl Default for CallStack {
    fn default() -> Self {
        Self::new()
    }
}

/// Scope information associated with a stack frame.
///
/// This provides additional context about the variables available
/// in each scope level.
#[derive(Debug, Clone)]
pub struct ScopeInfo {
    /// Scope name (procedure name or "Global").
    pub name: String,
    /// Number of variables in this scope.
    pub variable_count: usize,
    /// Named references (for $INCLUDE files).
    pub named_references: Vec<String>,
}

/// A variable reference that can span multiple scopes.
///
/// Used for evaluating watch expressions that may reference
/// variables in different scopes.
#[derive(Debug, Clone)]
pub struct VariableReference {
    /// The variable name.
    pub name: String,
    /// The frame where this variable is found.
    pub frame_id: FrameId,
    /// Whether this is a global variable.
    pub is_global: bool,
    /// For array/UDT access, the path of member accesses.
    pub access_path: Vec<AccessPathElement>,
}

/// An element in a variable access path.
#[derive(Debug, Clone)]
pub enum AccessPathElement {
    /// Array index access: `arr(i, j)`.
    ArrayIndex(Vec<i64>),
    /// UDT member access: `.member`.
    Member(String),
}

impl VariableReference {
    /// Creates a simple variable reference.
    pub fn simple(name: impl Into<String>, frame_id: FrameId, is_global: bool) -> Self {
        Self {
            name: name.into(),
            frame_id,
            is_global,
            access_path: Vec::new(),
        }
    }

    /// Adds an array index to the access path.
    pub fn with_index(mut self, indices: Vec<i64>) -> Self {
        self.access_path
            .push(AccessPathElement::ArrayIndex(indices));
        self
    }

    /// Adds a member access to the access path.
    pub fn with_member(mut self, member: impl Into<String>) -> Self {
        self.access_path
            .push(AccessPathElement::Member(member.into()));
        self
    }

    /// Formats the reference as a string (for display).
    pub fn format(&self) -> String {
        let mut result = self.name.clone();

        for element in &self.access_path {
            match element {
                AccessPathElement::ArrayIndex(indices) => {
                    let idx_str: Vec<String> = indices.iter().map(|i| i.to_string()).collect();
                    result.push_str(&format!("({})", idx_str.join(", ")));
                }
                AccessPathElement::Member(member) => {
                    result.push('.');
                    result.push_str(member);
                }
            }
        }

        result
    }
}

/// Variable categories for grouping in the debugger UI.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariableCategory {
    /// Local variables in the current scope.
    Locals,
    /// Function/sub arguments.
    Arguments,
    /// Global (module-level) variables.
    Globals,
    /// User-defined watch expressions.
    Watch,
    /// Return values from function calls.
    Return,
}

impl VariableCategory {
    /// Returns the display name for this category.
    pub fn display_name(&self) -> &'static str {
        match self {
            VariableCategory::Locals => "Locals",
            VariableCategory::Arguments => "Arguments",
            VariableCategory::Globals => "Globals",
            VariableCategory::Watch => "Watch",
            VariableCategory::Return => "Return",
        }
    }
}

/// A group of variables organized by category.
#[derive(Debug, Clone)]
pub struct VariableGroup {
    /// The category of variables in this group.
    pub category: VariableCategory,
    /// The variables in this group.
    pub variables: Vec<VariableInfo>,
}

impl VariableGroup {
    /// Creates a new variable group.
    pub fn new(category: VariableCategory) -> Self {
        Self {
            category,
            variables: Vec::new(),
        }
    }

    /// Adds a variable to the group.
    pub fn add(&mut self, var: VariableInfo) {
        self.variables.push(var);
    }

    /// Returns the number of variables in the group.
    pub fn len(&self) -> usize {
        self.variables.len()
    }

    /// Returns true if the group is empty.
    pub fn is_empty(&self) -> bool {
        self.variables.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn make_location(file: &str, line: usize) -> SourceLocation {
        SourceLocation::new(PathBuf::from(file), line, 1)
    }

    #[test]
    fn test_stack_frame_creation() {
        let loc = make_location("test.bas", 10);
        let frame = StackFrame::new(FrameId(0), "MySub", loc, DebugScopeId(1));

        assert_eq!(frame.name, "MySub");
        assert_eq!(frame.location.line, 10);
        assert!(!frame.is_main());
    }

    #[test]
    fn test_call_stack_push_pop() {
        let mut stack = CallStack::new();
        assert!(stack.is_empty());

        let main_loc = make_location("test.bas", 1);
        stack.push(StackFrame::main(main_loc));
        assert_eq!(stack.depth(), 1);

        let sub_loc = make_location("test.bas", 20);
        stack.push(StackFrame::new(
            FrameId(0),
            "MySub",
            sub_loc,
            DebugScopeId(1),
        ));
        assert_eq!(stack.depth(), 2);

        // Current frame should be MySub
        assert_eq!(stack.current().unwrap().name, "MySub");

        // Pop returns to main
        let popped = stack.pop().unwrap();
        assert_eq!(popped.name, "MySub");
        assert_eq!(stack.current().unwrap().name, "<main>");
    }

    #[test]
    fn test_frame_selection() {
        let mut stack = CallStack::new();
        stack.push(StackFrame::main(make_location("test.bas", 1)));
        stack.push(StackFrame::new(
            FrameId(0),
            "Sub1",
            make_location("test.bas", 10),
            DebugScopeId(1),
        ));
        stack.push(StackFrame::new(
            FrameId(0),
            "Sub2",
            make_location("test.bas", 20),
            DebugScopeId(2),
        ));

        // Initially at frame 0 (innermost = Sub2)
        assert_eq!(stack.selected().unwrap().name, "Sub2");

        // Move up to Sub1
        assert!(stack.select_up());
        assert_eq!(stack.selected().unwrap().name, "Sub1");

        // Move up to main
        assert!(stack.select_up());
        assert_eq!(stack.selected().unwrap().name, "<main>");

        // Can't go further up
        assert!(!stack.select_up());

        // Move back down
        assert!(stack.select_down());
        assert_eq!(stack.selected().unwrap().name, "Sub1");
    }

    #[test]
    fn test_variable_reference() {
        let var_ref = VariableReference::simple("arr", FrameId(0), false)
            .with_index(vec![1, 2])
            .with_member("x");

        assert_eq!(var_ref.format(), "arr(1, 2).x");
    }

    #[test]
    fn test_stack_trace_format() {
        let mut stack = CallStack::new();
        stack.push(StackFrame::main(make_location("test.bas", 1)));
        stack.push(StackFrame::new(
            FrameId(0),
            "MySub",
            make_location("test.bas", 15),
            DebugScopeId(1),
        ));

        let trace = stack.format_trace();
        assert!(trace.contains("#0 MySub"));
        assert!(trace.contains("#1 <main>"));
    }
}
