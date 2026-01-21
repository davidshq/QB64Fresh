# QB64Fresh Graphics System

This document covers the complete graphics system architecture, implementation, and usage.

---

## Overview

The graphics system uses a trait-based backend abstraction, following the same pattern as the code generation system. This allows SDL2 as the primary implementation while enabling alternative backends (native APIs, WebAssembly, etc.) without changing the compiler.

```
TypedProgram
    ↓
CodeGenerator (C Backend)
    ↓
Generated C Code (with graphics API calls)
    ↓
Graphics Abstraction Layer (trait-based)
    ├── SDL2Backend (current implementation)
    ├── NativeBackend (future: Win32, Xlib, Cocoa)
    ├── WebAssemblyBackend (future: Canvas/WebGL)
    └── MockBackend (testing)
    ↓
System-Specific Graphics Library
```

---

## Architecture

### Design Principles

1. **Single Responsibility**: Graphics backends handle only rendering, not language semantics
2. **Zero Cost Abstraction**: Backend selection happens at compile-time via feature flags
3. **C FFI Compatibility**: Generated C code calls a stable C API
4. **Gradual Implementation**: Start with SDL2, add complexity only when needed
5. **Testability**: Mock backend enables headless testing

### Architecture Layers

```
┌─────────────────────────────────────────────────────────────┐
│ Generated C Code                                            │
│ SCREEN 13, PSET (100,100), 15, etc.                        │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ C FFI Layer (qb64_gfx_*)                                    │
│ qb64_gfx_init(), qb64_gfx_pset(), etc.                      │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ GraphicsBackend Trait (Rust)                                │
│ initialize(), pset(), circle(), etc.                        │
└────────────────────┬────────────────────────────────────────┘
                     │
          ┌──────────┼──────────────────────┐
          ▼          ▼                       ▼
    ┌──────────┐ ┌──────────┐        ┌─────────────┐
    │ SDL2     │ │ Mock     │        │ Future:     │
    │ Backend  │ │ Backend  │        │ - Native    │
    │          │ │ (testing)│        │ - WASM      │
    └──────────┘ └──────────┘        │ - Others    │
                                     └─────────────┘
```

### Layer 1: Graphics API Trait (Rust)

Defined in `runtime/src/graphics/mod.rs`:

```rust
pub trait GraphicsBackend {
    // Initialization
    fn initialize(&mut self, width: u32, height: u32) -> Result<(), GraphicsError>;
    fn shutdown(&mut self) -> Result<(), GraphicsError>;

    // Screen operations
    fn cls(&mut self, color: u32) -> Result<(), GraphicsError>;
    fn set_color(&mut self, fg: u32, bg: u32) -> Result<(), GraphicsError>;
    fn locate(&mut self, row: u32, col: u32) -> Result<(), GraphicsError>;

    // Drawing primitives
    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<(), GraphicsError>;
    fn line(&mut self, x1: i32, y1: i32, x2: i32, y2: i32, color: u32) -> Result<(), GraphicsError>;
    fn circle(&mut self, x: i32, y: i32, radius: i32, color: u32) -> Result<(), GraphicsError>;
    // ... more drawing functions

    // Display operations
    fn display(&mut self) -> Result<(), GraphicsError>;
    fn poll_events(&mut self) -> Result<bool, GraphicsError>; // false = quit requested
}
```

### Layer 2: C FFI Wrapper

`runtime/include/qb64fresh_gfx.h` exposes a stable C API:

```c
// Initialization
int qb64_gfx_init(uint32_t width, uint32_t height);
int qb64_gfx_shutdown(void);

// Drawing
int qb64_gfx_pset(int32_t x, int32_t y, uint32_t color);
int qb64_gfx_line(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color);
int qb64_gfx_circle(int32_t x, int32_t y, int32_t radius, uint32_t color);
// ... more
```

### Layer 3: Backend Implementations

| Backend | Location | Status |
|---------|----------|--------|
| **SDL2Backend** | `runtime/src/graphics/sdl2.rs` | ✅ Complete |
| **MockBackend** | `runtime/src/graphics/mock.rs` | ✅ Complete |
| **NativeBackend** | `runtime/src/graphics/native/` | Future |
| **WebAssemblyBackend** | `runtime/src/graphics/wasm.rs` | Future |

### Layer 4: Generated C Code

Code generator emits calls to the stable C FFI:

```c
// Generated from: SCREEN 13
qb64_gfx_init(320, 200);

// Generated from: PSET (100, 100), 15
qb64_gfx_pset(100, 100, 15);

// Generated from: LINE (0,0) - (100,100)
qb64_gfx_line(0, 0, 100, 100, 15);
```

---

## Building with Different Backends

### SDL2 (Default)

```bash
cargo build --release
# or explicitly:
cargo build --release --features graphics-sdl2
```

### Mock Backend (Testing, No Display Required)

```bash
cargo build --lib --no-default-features --features graphics-mock
cargo test --lib
```

### Feature Flags

```toml
[features]
default = ["graphics-sdl2"]
graphics-sdl2 = ["sdl2"]
graphics-mock = []
graphics-native = []      # Future
graphics-wasm = ["web-sys", "wasm-bindgen"]  # Future
```

---

## Usage Examples

### From Rust (Internal)

```rust
use qb64fresh_rt::graphics::{GraphicsBackend, SDL2Backend};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut backend = SDL2Backend::new();

    // Initialize a 320x200 screen
    backend.initialize(320, 200)?;

    // Set colors (foreground=white, background=black)
    backend.set_color(15, 0)?;

    // Clear the screen
    backend.cls()?;

    // Plot some pixels
    backend.pset(100, 100, 15)?;

    // Draw a line
    backend.line(0, 0, 319, 199, 12, false)?;

    // Update display
    backend.display()?;

    // Event loop
    while backend.poll_events()? {
        backend.display()?;
    }

    backend.shutdown()?;
    Ok(())
}
```

### Testing with Mock Backend

```rust
#[cfg(test)]
mod tests {
    use qb64fresh_rt::graphics::mock::MockBackend;
    use qb64fresh_rt::graphics::GraphicsBackend;

    #[test]
    fn test_graphics_sequence() -> Result<(), Box<dyn std::error::Error>> {
        let mut backend = MockBackend::new();

        backend.initialize(320, 200)?;
        backend.set_color(15, 0)?;
        backend.pset(100, 100, 15)?;
        backend.display()?;

        // Verify operations were recorded
        assert_eq!(backend.operations().len(), 4);
        assert!(backend.has_operation(&MockOperation::Pset(100, 100, 15)));

        backend.shutdown()?;
        Ok(())
    }
}
```

---

## Adding a New Backend

### 1. Create the Backend Module

```rust
// runtime/src/graphics/native_win32.rs

use super::{GraphicsBackend, GraphicsError};

pub struct Win32Backend {
    // Windows-specific fields
}

impl GraphicsBackend for Win32Backend {
    // Implement all required methods...
}
```

### 2. Update Feature Flags

```toml
# runtime/Cargo.toml
[features]
graphics-win32 = ["windows"]
```

### 3. Conditional Compilation

```rust
// runtime/src/graphics/mod.rs

#[cfg(feature = "graphics-win32")]
pub mod native_win32;
```

### 4. Build

```bash
cargo build --no-default-features --features graphics-win32
```

---

## Error Handling

Graphics operations return `Result<T, GraphicsError>`:

```rust
match backend.pset(x, y, color) {
    Ok(()) => println!("Pixel plotted"),
    Err(e) if e.kind() == &GraphicsErrorKind::NotInitialized => {
        eprintln!("Call initialize() first");
    }
    Err(e) if e.kind() == &GraphicsErrorKind::InvalidArgument => {
        eprintln!("Invalid coordinates or color: {}", e.message());
    }
    Err(e) => eprintln!("Graphics error: {}", e),
}
```

---

## Comparison with QB64PE

| Aspect | QB64PE | QB64Fresh |
|--------|--------|-----------|
| **Graphics API** | SDL2 (hardcoded) | Trait-based (pluggable) |
| **Native Support** | Limited (Windows mainly) | Easy to add later |
| **WebAssembly** | Not supported | Planned support |
| **Testing** | Requires display | Can use mock backend |
| **Code Reuse** | Graphics logic in C | Reusable Rust trait |
| **Backend Flexibility** | SDL2 only | Feature-based selection |

---

## Implementation Files

| File | Purpose |
|------|---------|
| `runtime/src/graphics/mod.rs` | Core trait and initialization |
| `runtime/src/graphics/error.rs` | Error types |
| `runtime/src/graphics/sdl2.rs` | SDL2 backend |
| `runtime/src/graphics/mock.rs` | Mock backend for testing |
| `runtime/src/graphics/font.rs` | Font rendering (8x8 bitmap) |
| `runtime/src/graphics_ffi.rs` | C FFI bindings |

---

*Last updated: 2026-01-21*