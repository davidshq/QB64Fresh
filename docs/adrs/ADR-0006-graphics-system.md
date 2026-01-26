# ADR-0006: Graphics System Architecture

## Status

**Accepted** - January 18, 2026

## Context

QB64Fresh needs a graphics system to support BASIC's visual programming capabilities:
- Classic QBasic screen modes (SCREEN 0-13)
- QB64 extended graphics (_NEWIMAGE, _LOADIMAGE, _PUTIMAGE)
- Drawing primitives (PSET, LINE, CIRCLE, PAINT, DRAW)
- Text rendering with cursor positioning (LOCATE, PRINT)
- Input handling (mouse, keyboard)
- Modern features (32-bit color, alpha blending, image buffers)

Key considerations:
- Must support headless/CI testing without display
- Should allow future backends (native APIs, WebAssembly)
- Need consistent behavior across platforms
- Must integrate cleanly with the C code generation approach
- QB64pe uses SDL2 extensively - proven approach

## Decision

**We chose a trait-based pluggable backend architecture with SDL2 as the primary implementation**.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Generated C Code                                            │
│ SCREEN 13, PSET (100,100), 15, etc.                        │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ C FFI Layer (qb_gfx_*)                                      │
│ qb_gfx_init(), qb_gfx_pset(), etc.                          │
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

### GraphicsBackend Trait

The core abstraction (~40 methods) covering:

```rust
pub trait GraphicsBackend {
    // Initialization
    fn initialize(&mut self, width: u32, height: u32) -> Result<(), GraphicsError>;
    fn shutdown(&mut self) -> Result<(), GraphicsError>;

    // Drawing primitives
    fn cls(&mut self) -> Result<(), GraphicsError>;
    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<(), GraphicsError>;
    fn line(&mut self, x1: i32, y1: i32, x2: i32, y2: i32, color: u32, boxed: bool) -> Result<(), GraphicsError>;
    fn circle(&mut self, x: i32, y: i32, radius: i32, color: u32, filled: bool) -> Result<(), GraphicsError>;
    fn paint(&mut self, x: i32, y: i32, color: u32, boundary: Option<u32>) -> Result<(), GraphicsError>;

    // Image buffers (QB64 extensions)
    fn new_image(&mut self, width: i32, height: i32, mode: i32) -> i32;
    fn load_image(&mut self, filename: &str, mode: i32) -> i32;
    fn put_image(&mut self, dx1: i32, dy1: i32, dx2: i32, dy2: i32, src: i32, dest: i32) -> Result<(), GraphicsError>;

    // Mouse input
    fn get_mouse_x(&self) -> i32;
    fn get_mouse_y(&self) -> i32;
    fn get_mouse_button(&self, button: u32) -> bool;

    // ... additional methods
}
```

### Backend Selection via Feature Flags

```toml
[features]
default = ["graphics-sdl2"]
graphics-sdl2 = ["sdl2"]
graphics-mock = []  # For testing
```

### C FFI Layer

Generated C code calls functions like:
- `qb_gfx_init(width, height)` - Initialize graphics
- `qb_gfx_pset(x, y, color)` - Plot pixel
- `qb_gfx_circle(x, y, radius, color, filled)` - Draw circle
- `qb_rgb(r, g, b)` - Create ARGB color value

### Rationale

1. **SDL2 as primary**: Proven by QB64pe, cross-platform, well-maintained
2. **Trait abstraction**: Enables testing without display, future backends
3. **Mock backend**: CI/headless testing without X11/Wayland/Windows
4. **Feature flags**: Compile-time backend selection, smaller binaries
5. **Global instance**: Required for C FFI integration from generated code
6. **ARGB color format**: Standard format, matches QB64pe

### Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| **winit + softbuffer** | More complex for classic screen modes |
| **Hardcoded SDL2** | No testing flexibility, no future options |
| **minifb** | Less mature, fewer features than SDL2 |
| **wgpu** | Overkill for 2D BASIC graphics |

## Consequences

### Positive

- Headless testing via mock backend
- Clean separation of concerns
- Easy to add new backends (WebAssembly, native APIs)
- Consistent API across platforms
- Can swap backends without changing compiler code
- Feature flags optimize binary size

### Negative

- Global mutable state (required for C FFI)
- Trait overhead (minimal, one vtable lookup)
- SDL2 dependency for production builds
- Not thread-safe (single-threaded graphics assumed)

### Implementation Status

| Component | Status |
|-----------|--------|
| GraphicsBackend trait | Complete |
| Mock backend | Complete |
| SDL2 backend | Complete |
| C FFI layer | Complete |
| Code generation | Complete |
| Mouse input | Complete |
| Clipboard | Complete |
| Font rendering (8x8 bitmap) | Complete |
| TrueType font support (_LOADFONT) | Complete (optional feature) |
| Text output (PRINT, LOCATE) | Complete |
| Image buffers (_NEWIMAGE, _LOADIMAGE, _PUTIMAGE) | Complete |
| VIEW/WINDOW coordinate systems | Complete |
| DRAW turtle graphics | Complete |
| Palette (PALETTE statement) | Complete |

### Files

- `runtime/src/graphics/mod.rs` - Core trait, initialization
- `runtime/src/graphics/error.rs` - Error types
- `runtime/src/graphics/mock.rs` - Mock backend for testing
- `runtime/src/graphics/sdl2.rs` - SDL2 backend (~2100 lines)
- `runtime/src/graphics/font.rs` - Font rendering and text output
- `runtime/src/graphics_ffi.rs` - C FFI functions

**Detailed Documentation:** See [docs/GRAPHICS.md](../GRAPHICS.md) for comprehensive architecture details, implementation status, supported statements, usage instructions, and runtime modes.
