//! Mock graphics backend for testing.
//!
//! This backend records all operations without actually rendering anything.
//! Useful for headless testing and verifying graphics operations.

use super::{GraphicsBackend, GraphicsError};

/// A no-op graphics backend used for testing.
///
/// Records all operations but produces no visible output.
/// Useful for:
/// - Unit testing graphics-heavy BASIC programs
/// - Continuous integration environments without a display
/// - Debugging graphics operations
#[derive(Debug, Clone)]
pub struct MockBackend {
    initialized: bool,
    width: u32,
    height: u32,
    fg_color: u32,
    bg_color: u32,
    cursor_row: u32,
    cursor_col: u32,
    operations: Vec<MockOperation>,
}

/// Recorded graphics operation.
#[derive(Debug, Clone, PartialEq)]
pub enum MockOperation {
    Initialize(u32, u32),
    Shutdown,
    Cls,
    SetColor(u32, u32),
    Locate(u32, u32),
    Print(String),
    Pset(i32, i32, u32),
    Point(i32, i32),
    Line(i32, i32, i32, i32, u32, bool),
    Circle(i32, i32, i32, u32, bool),
    Paint(i32, i32, u32, Option<u32>),
    Display,
    PollEvents,
}

impl MockBackend {
    /// Create a new mock backend.
    pub fn new() -> Self {
        Self {
            initialized: false,
            width: 0,
            height: 0,
            fg_color: 7, // Default white
            bg_color: 0, // Default black
            cursor_row: 1,
            cursor_col: 1,
            operations: Vec::new(),
        }
    }

    /// Get the recorded operations.
    pub fn operations(&self) -> &[MockOperation] {
        &self.operations
    }

    /// Clear the recorded operations.
    pub fn clear_operations(&mut self) {
        self.operations.clear();
    }

    /// Get a reference to the operations vector.
    pub fn operations_mut(&mut self) -> &mut Vec<MockOperation> {
        &mut self.operations
    }

    /// Check if a specific operation was recorded.
    pub fn has_operation(&self, op: &MockOperation) -> bool {
        self.operations
            .iter()
            .any(|recorded| std::mem::discriminant(recorded) == std::mem::discriminant(op))
    }

    /// Count how many times a specific operation type was recorded.
    pub fn operation_count(&self, op_type: &MockOperation) -> usize {
        self.operations
            .iter()
            .filter(|recorded| std::mem::discriminant(*recorded) == std::mem::discriminant(op_type))
            .count()
    }
}

impl Default for MockBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl GraphicsBackend for MockBackend {
    fn initialize(&mut self, width: u32, height: u32) -> Result<(), GraphicsError> {
        if self.initialized {
            return Err(GraphicsError::already_initialized());
        }

        self.initialized = true;
        self.width = width;
        self.height = height;
        self.operations
            .push(MockOperation::Initialize(width, height));
        Ok(())
    }

    fn shutdown(&mut self) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Ok(()); // Idempotent
        }

        self.initialized = false;
        self.operations.push(MockOperation::Shutdown);
        Ok(())
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn cls(&mut self) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations.push(MockOperation::Cls);
        Ok(())
    }

    fn set_color(&mut self, foreground: u32, background: u32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.fg_color = foreground;
        self.bg_color = background;
        self.operations
            .push(MockOperation::SetColor(foreground, background));
        Ok(())
    }

    fn get_foreground_color(&self) -> u32 {
        self.fg_color
    }

    fn get_background_color(&self) -> u32 {
        self.bg_color
    }

    fn locate(&mut self, row: u32, col: u32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.cursor_row = row;
        self.cursor_col = col;
        self.operations.push(MockOperation::Locate(row, col));
        Ok(())
    }

    fn get_cursor_position(&self) -> (u32, u32) {
        (self.cursor_row, self.cursor_col)
    }

    fn print(&mut self, text: &str) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations.push(MockOperation::Print(text.to_string()));
        Ok(())
    }

    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations.push(MockOperation::Pset(x, y, color));
        Ok(())
    }

    fn point(&self, _x: i32, _y: i32) -> Result<u32, GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        // Mock always returns the color we last set
        Ok(self.fg_color)
    }

    fn line(
        &mut self,
        x1: i32,
        y1: i32,
        x2: i32,
        y2: i32,
        color: u32,
        filled: bool,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations
            .push(MockOperation::Line(x1, y1, x2, y2, color, filled));
        Ok(())
    }

    fn circle(
        &mut self,
        x: i32,
        y: i32,
        radius: i32,
        color: u32,
        filled: bool,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations
            .push(MockOperation::Circle(x, y, radius, color, filled));
        Ok(())
    }

    fn paint(
        &mut self,
        x: i32,
        y: i32,
        color: u32,
        boundary_color: Option<u32>,
    ) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations
            .push(MockOperation::Paint(x, y, color, boundary_color));
        Ok(())
    }

    fn display(&mut self) -> Result<(), GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations.push(MockOperation::Display);
        Ok(())
    }

    fn poll_events(&mut self) -> Result<bool, GraphicsError> {
        if !self.initialized {
            return Err(GraphicsError::not_initialized());
        }
        self.operations.push(MockOperation::PollEvents);
        Ok(true)
    }

    fn get_screen_size(&self) -> (u32, u32) {
        (self.width, self.height)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_initialization() {
        let mut backend = MockBackend::new();
        assert!(!backend.is_initialized());

        backend.initialize(320, 200).unwrap();
        assert!(backend.is_initialized());
        assert_eq!(backend.get_screen_size(), (320, 200));
    }

    #[test]
    fn test_mock_records_operations() {
        let mut backend = MockBackend::new();
        backend.initialize(320, 200).unwrap();
        backend.set_color(15, 0).unwrap();
        backend.pset(100, 100, 15).unwrap();
        backend.display().unwrap();

        assert_eq!(backend.operations.len(), 4); // init, set_color, pset, display
    }

    #[test]
    fn test_mock_rejects_uninitialized_operations() {
        let mut backend = MockBackend::new();

        assert!(backend.cls().is_err());
        assert!(backend.pset(0, 0, 0).is_err());
        assert!(backend.display().is_err());
    }

    #[test]
    fn test_operation_counting() {
        let mut backend = MockBackend::new();
        backend.initialize(320, 200).unwrap();

        backend.pset(10, 10, 15).unwrap();
        backend.pset(20, 20, 15).unwrap();
        backend.pset(30, 30, 15).unwrap();

        assert_eq!(backend.operation_count(&MockOperation::Pset(0, 0, 0)), 3);
    }
}
