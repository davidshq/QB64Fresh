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
│ C FFI Layer (qb_gfx_*)                                      │
│ qb_gfx_init(), qb_gfx_pset(), etc.                         │
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
    fn cls(&mut self) -> Result<(), GraphicsError>;
    fn set_color(&mut self, fg: u32, bg: u32) -> Result<(), GraphicsError>;
    fn locate(&mut self, row: u32, col: u32) -> Result<(), GraphicsError>;
    fn print(&mut self, text: &str) -> Result<(), GraphicsError>;

    // Drawing primitives
    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<(), GraphicsError>;
    fn point(&self, x: i32, y: i32) -> Result<u32, GraphicsError>;
    fn line(&mut self, x1: i32, y1: i32, x2: i32, y2: i32, color: u32, filled: bool) -> Result<(), GraphicsError>;
    fn circle(&mut self, x: i32, y: i32, radius: i32, color: u32, filled: bool) -> Result<(), GraphicsError>;
    fn paint(&mut self, x: i32, y: i32, color: u32, boundary_color: Option<u32>) -> Result<(), GraphicsError>;
    
    // STEP variants (relative coordinates)
    fn pset_step(&mut self, x: i32, y: i32, color: u32, step: bool) -> Result<(), GraphicsError>;
    fn line_step(&mut self, x1: i32, y1: i32, x2: i32, y2: i32, color: u32, filled: bool, step1: bool, step2: bool) -> Result<(), GraphicsError>;
    fn circle_step(&mut self, x: i32, y: i32, radius: i32, color: u32, filled: bool, step: bool) -> Result<(), GraphicsError>;
    fn paint_step(&mut self, x: i32, y: i32, color: u32, boundary_color: Option<u32>, step: bool) -> Result<(), GraphicsError>;

    // Display operations
    fn display(&mut self) -> Result<(), GraphicsError>;
    fn poll_events(&mut self) -> Result<bool, GraphicsError>; // false = quit requested
    
    // Extended features
    fn set_view(&mut self, screen: bool, x1: i32, y1: i32, x2: i32, y2: i32, fill_color: Option<u32>, border_color: Option<u32>) -> Result<(), GraphicsError>;
    fn set_window(&mut self, screen: bool, x1: f64, y1: f64, x2: f64, y2: f64) -> Result<(), GraphicsError>;
    fn draw(&mut self, commands: &str) -> Result<(), GraphicsError>;
    fn set_palette(&mut self, index: i32, color: u32) -> Result<(), GraphicsError>;
    
    // Image buffers (QB64 extensions)
    fn new_image(&mut self, width: i32, height: i32, mode: i32) -> i32;
    fn load_image(&mut self, filename: &str, mode: i32) -> i32;
    fn put_image(&mut self, dest_x1: i32, dest_y1: i32, dest_x2: i32, dest_y2: i32, src_handle: i32, dest_handle: i32) -> Result<(), GraphicsError>;
    
    // Mouse input
    fn get_mouse_x(&self) -> i32;
    fn get_mouse_y(&self) -> i32;
    fn get_mouse_button(&self, button: u32) -> bool;
    
    // Clipboard
    fn get_clipboard(&self) -> Option<String>;
    fn set_clipboard(&mut self, text: &str);
    
    // Font support
    fn load_font(&mut self, path: &str, size: u16) -> i64;
    fn set_font(&mut self, handle: i64) -> i64;
    // ... and many more methods
}
```

### Layer 2: C FFI Wrapper

The C FFI layer is defined in `runtime/src/graphics_ffi.rs` and provides functions that can be called from generated C code. These functions use the `qb_gfx_*` prefix and are exported via `#[no_mangle]`:

```c
// Initialization
int qb_gfx_init(uint32_t width, uint32_t height);
int qb_gfx_shutdown(void);

// Screen operations
int qb_gfx_cls(void);
int qb_gfx_color(uint32_t foreground, uint32_t background);
int qb_gfx_locate(uint32_t row, uint32_t col);

// Drawing primitives
int qb_gfx_pset(int32_t x, int32_t y, uint32_t color);
int qb_gfx_pset_step(int32_t x, int32_t y, uint32_t color, int step);
uint32_t qb_gfx_point(int32_t x, int32_t y);
int qb_gfx_line(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color);
int qb_gfx_line_step(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int step1, int step2);
int qb_gfx_circle(int32_t x, int32_t y, int32_t radius, uint32_t color, int filled);
int qb_gfx_circle_step(int32_t x, int32_t y, int32_t radius, uint32_t color, int filled, int step);
int qb_gfx_paint(int32_t x, int32_t y, uint32_t color, int32_t boundary_color);
int qb_gfx_paint_step(int32_t x, int32_t y, uint32_t color, int32_t boundary_color, int step);

// Display
int qb_gfx_display(void);
int qb_gfx_poll_events(void);

// Color utilities
uint32_t qb_rgb(uint32_t r, uint32_t g, uint32_t b);
uint32_t qb_rgba(uint32_t r, uint32_t g, uint32_t b, uint32_t a);

// Extended features
int qb_gfx_view(int screen, int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t fill, uint32_t border);
int qb_gfx_window(int screen, double x1, double y1, double x2, double y2);
int qb_gfx_palette(int32_t index, uint32_t color);

// Image buffers
int32_t qb_gfx_newimage(int32_t width, int32_t height, int32_t mode);
int qb_gfx_putimage(int32_t dest_x1, int32_t dest_y1, int32_t dest_x2, int32_t dest_y2, int32_t src_handle, int32_t dest_handle);

// Mouse input
int32_t qb_mouse_x(void);
int32_t qb_mouse_y(void);
int32_t qb_mouse_button(int32_t button);

// Clipboard
QbString* qb_clipboard_get(void);
void qb_clipboard_set(const char* text);

// Font support
int64_t qb_font(int64_t handle);
int64_t qb_freefont(int64_t handle);
// ... and many more functions
```

**Note:** The actual function declarations are embedded in the generated C code via the inline runtime mode. See `src/codegen/c_backend/runtime.rs` for the complete list.

### Layer 3: Backend Implementations

| Backend | Location | Status |
|---------|----------|--------|
| **SDL2Backend** | `runtime/src/graphics/sdl2.rs` | ✅ Complete |
| **MockBackend** | `runtime/src/graphics/mock.rs` | ✅ Complete |
| **NativeBackend** | `runtime/src/graphics/native/` | Future |
| **WebAssemblyBackend** | `runtime/src/graphics/wasm.rs` | Future |

### Layer 4: Generated C Code

Code generator emits calls to the stable C FFI. Examples from `src/codegen/c_backend/stmt.rs`:

```c
// Generated from: SCREEN 13
qb_gfx_screen((int32_t)13, (int32_t)0, (int32_t)0, (int32_t)0);

// Generated from: PSET (100, 100), 15
qb_gfx_pset_step((int32_t)100, (int32_t)100, (uint32_t)15, 0);

// Generated from: PSET STEP (10, 20), 15
qb_gfx_pset_step((int32_t)10, (int32_t)20, (uint32_t)15, 1);

// Generated from: LINE (0,0) - (100,100), 15
qb_gfx_line_step((int32_t)0, (int32_t)0, (int32_t)100, (int32_t)100, (uint32_t)15, 0, 0);

// Generated from: CIRCLE (160, 100), 50, 15
qb_gfx_circle_step((int32_t)160, (int32_t)100, (int32_t)50, (uint32_t)15, 0, 0);

// Generated from: PAINT (100, 100), 15, 0
qb_gfx_paint_step((int32_t)100, (int32_t)100, (uint32_t)15, (int32_t)0, 0);
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

## Runtime Modes

The compiler supports two runtime modes for graphics:

### Inline Runtime (Default)

```bash
cargo run --bin qb64fresh -- program.bas --emit-c
# or explicitly:
cargo run --bin qb64fresh -- program.bas --emit-c --runtime inline
```

The inline runtime embeds all graphics stub functions directly in the generated C code. Graphics calls become no-ops with warning messages. This is useful for:
- Quick testing without graphics dependencies
- Text-only programs
- Systems without SDL2

### External Runtime

```bash
cargo run --bin qb64fresh -- program.bas --emit-c --runtime external
```

The external runtime links against `libqb64fresh_rt.a`, providing full graphics support via the Rust runtime library. Generated code includes `#include "qb64fresh_rt.h"`.

**Building with External Runtime:**

```bash
# 1. Build the runtime library
cd runtime
cargo build --release --features graphics-sdl2

# 2. Compile BASIC program to C
cd ..
cargo run --bin qb64fresh -- program.bas --emit-c --runtime external

# 3. Compile and link
gcc -I runtime/include program.c \
    -L target/release -lqb64fresh_rt \
    $(pkg-config --libs sdl2) \
    -lasound -lwayland-client -lm -lpthread -ldl \
    -o program
```

**Header File:** `runtime/include/qb64fresh_rt.h`

The header declares all FFI functions available in the external runtime:
- Graphics functions: `qb_gfx_screen`, `qb_gfx_pset`, `qb_gfx_line_step`, etc.
- Color utilities: `qb_rgb`, `qb_rgba`, `qb_rgb32`, `qb_rgba32`
- Mouse input: `qb_mouse_x`, `qb_mouse_y`, `qb_mouse_button`, etc.
- Clipboard: `qb_clipboard_get`, `qb_clipboard_set`
- Fonts: `qb_loadfont`, `qb_font`, `qb_freefont`
- Window control: `qb_fullscreen`, `qb_screenmove`, `qb_screenshow`, `qb_screenhide`
- Alpha blending: `qb_blend`, `qb_dontblend`, `qb_clearcolor`, `qb_clearcolor_none`
- Initialization: `qb_init_args`, `qb_init_startdir`, `_qb_init_palette`

**Compatibility macros** bridge inline/external naming conventions:
```c
#define qb__rgb32(r, g, b) qb_rgb(r, g, b)
#define qb__rgb32_4(r, g, b, a) qb_rgba(r, g, b, a)
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

    // Clear the screen (uses current background color)
    backend.cls()?;

    // Plot some pixels
    backend.pset(100, 100, 15)?;

    // Draw a line
    backend.line(0, 0, 319, 199, 12, false)?;

    // Draw a filled circle
    backend.circle(160, 100, 50, 10, true)?;

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

## Implementation Status

Based on ADR-0006, the following features are **complete**:

| Component | Status | Notes |
|-----------|--------|-------|
| GraphicsBackend trait | ✅ Complete | ~40 methods covering all BASIC graphics operations |
| Mock backend | ✅ Complete | Enables headless testing without display |
| SDL2 backend | ✅ Complete | Full implementation (~2100 lines) |
| C FFI layer | ✅ Complete | All graphics operations exposed via `qb_gfx_*` functions |
| Code generation | ✅ Complete | All graphics statements generate appropriate C calls |
| Mouse input | ✅ Complete | Position, buttons, movement, wheel support |
| Clipboard | ✅ Complete | Get/set text from system clipboard |
| Font rendering (8x8 bitmap) | ✅ Complete | Built-in font for text mode |
| TrueType font support (_LOADFONT) | ✅ Complete | Optional feature via SDL2_ttf |
| Text output (PRINT, LOCATE) | ✅ Complete | Text mode and graphics mode text |
| Image buffers (_NEWIMAGE, _LOADIMAGE, _PUTIMAGE) | ✅ Complete | QB64 extended graphics support |
| VIEW/WINDOW coordinate systems | ✅ Complete | Viewport and world coordinate mapping |
| DRAW turtle graphics | ✅ Complete | Full DRAW command string parsing |
| Palette (PALETTE statement) | ✅ Complete | 256-color palette support |
| GET/PUT array operations | ✅ Complete | Pixel array read/write |
| STEP mode for all primitives | ✅ Complete | Relative coordinate support |
| Window control | ✅ Complete | `_FULLSCREEN`, `_SCREENMOVE`, `_SCREENSHOW`, `_SCREENHIDE` |
| Alpha blending | ✅ Complete | `_BLEND`, `_DONTBLEND`, `_CLEARCOLOR` |

## Implementation Files

| File | Purpose | Lines |
|------|---------|-------|
| `runtime/src/graphics/mod.rs` | Core trait definition (~620 lines) | Defines `GraphicsBackend` trait with all methods |
| `runtime/src/graphics/error.rs` | Error types | `GraphicsError` and `GraphicsErrorKind` |
| `runtime/src/graphics/sdl2.rs` | SDL2 backend implementation | ~2100 lines, full feature implementation |
| `runtime/src/graphics/mock.rs` | Mock backend for testing | Records operations for verification |
| `runtime/src/graphics/font.rs` | Font rendering | 8x8 bitmap font and TrueType support |
| `runtime/src/graphics_ffi.rs` | C FFI bindings | ~1600 lines, exports `qb_gfx_*` functions |
| `src/codegen/c_backend/stmt.rs` | Graphics code generation | Emits C calls for all graphics statements |
| `src/parser/graphics.rs` | Graphics statement parsing | Parses SCREEN, PSET, LINE, CIRCLE, etc. |

## Supported Graphics Statements

The following BASIC graphics statements are fully supported:

### Screen Management
- `SCREEN` - Set screen mode and page configuration
- `SCREEN mode, , active_page, visual_page` - Set drawing and display pages for double buffering
- `PCOPY source, dest` - Copy page contents between screen pages
- `WIDTH` - Set text mode dimensions
- `CLS` - Clear screen
- `COLOR` - Set foreground/background colors
- `LOCATE` - Position text cursor
- `VIEW` - Set viewport for graphics
- `WINDOW` - Set world coordinate system
- `PALETTE` - Set palette entries
- `_COPYPALETTE` - Copy palette between images

### Window Control (QB64 Extensions)
- `_FULLSCREEN` - Set/get fullscreen mode (0=windowed, 1=fullscreen, 2=desktop)
- `_SCREENMOVE` - Move window to position
- `_SCREENSHOW` - Show window
- `_SCREENHIDE` - Hide window

### Drawing Primitives
- `PSET` / `PSET STEP` - Plot pixel
- `POINT` - Get pixel color
- `LINE` / `LINE STEP` - Draw line or box
- `CIRCLE` / `CIRCLE STEP` - Draw circle (filled or outline)
- `PAINT` / `PAINT STEP` - Flood fill
- `DRAW` - Turtle graphics commands

### 3D Graphics (QB64 Extensions)
- `_MAPTRIANGLE` - Software texture mapping rasterizer for 3D graphics
  - Maps texture coordinates from source image to destination triangle
  - Supports perspective-correct interpolation
  - Used for 3D rendering without OpenGL

### Image Buffers (QB64 Extensions)
- `_NEWIMAGE` - Create new image buffer
- `_LOADIMAGE` - Load image from file
- `_PUTIMAGE` - Copy image to screen or buffer
- `_FREEIMAGE` - Release image buffer
- `_SOURCE` / `_DEST` - Set source/destination buffer
- `_AUTODISPLAY` - Enable/disable auto-display
- `_DISPLAYORDER` - Control rendering layer order (hardware, software, text layers)

### Alpha Blending (QB64 Extensions)
- `_BLEND` - Enable alpha blending for image (uses alpha channel during `_PUTIMAGE`)
- `_DONTBLEND` - Disable alpha blending (direct pixel copy)
- `_CLEARCOLOR` - Set transparency key (pixels matching color are skipped during copy)

### Pixel Arrays
- `GET` - Read pixels to array
- `PUT` - Write pixels from array

### Mouse Input
- `_MOUSEX`, `_MOUSEY` - Get mouse position
- `_MOUSEBUTTON` - Check button state
- `_MOUSEINPUT` - Poll for mouse events
- `_MOUSEMOVEMENTX`, `_MOUSEMOVEMENTY` - Get movement delta
- `_MOUSEWHEEL` - Get wheel delta
- `_MOUSEHIDE`, `_MOUSESHOW` - Control cursor visibility
- `_MOUSEMOVE` - Move cursor programmatically

### Clipboard
- `_CLIPBOARD$` - Get/set clipboard text

### Fonts
- `_LOADFONT` - Load TrueType font
- `_LOADFONT(path$, size, "UNICODE")` - Load TrueType font with Unicode support
- `_FONT` - Set current font
- `_FREEFONT` - Release font resources
- `_FONTHEIGHT`, `_FONTWIDTH` - Get font dimensions
- `_PRINTWIDTH` - Get text width in pixels

### Unicode Text Rendering (QB64 Extensions)
- `_UPRINTSTRING(x, y, text$)` - Render Unicode text at position
- `_UPRINTWIDTH(text$)` - Get Unicode text width in pixels
- `_UFONTHEIGHT` - Get Unicode font height
- `_ULINESPACING` - Get Unicode line spacing
- `_UCHARPOS(text$, pos)` - Get character X position within string
- `_MAPUNICODE` - CP437 to Unicode mapping table (256 codepoints)

---

*Last updated: 2026-01-24*