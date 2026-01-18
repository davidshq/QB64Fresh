# QB64Fresh Graphics Architecture

## Overview

The graphics system is designed with a trait-based backend abstraction, following the same pattern as the code generation system. This allows SDL2 to be the initial implementation while maintaining the ability to swap in alternative graphics backends (native APIs, WebAssembly, etc.) without changing the compiler.

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

## Design Principles

1. **Single Responsibility**: Graphics backends handle only rendering, not language semantics
2. **Zero Cost Abstraction**: Backend selection happens at compile-time via feature flags
3. **C FFI Compatibility**: Generated C code calls a stable C API
4. **Gradual Implementation**: Start with SDL2, add complexity only when needed
5. **Testability**: Mock backend enables headless testing

## Architecture Layers

### Layer 1: Graphics API Trait (Rust)
Defined in `runtime/src/graphics/mod.rs`

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
`runtime/include/qb64fresh_gfx.h` exposes a stable C API for generated code:

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

#### SDL2Backend (current)
- `runtime/src/graphics/sdl2.rs`
- Implements `GraphicsBackend` trait
- Wraps `sdl2` crate

#### NativeBackend (future)
- `runtime/src/graphics/native/mod.rs`
  - `runtime/src/graphics/native/win32.rs`
  - `runtime/src/graphics/native/xlib.rs`
  - `runtime/src/graphics/native/cocoa.rs`

#### WebAssemblyBackend (future)
- `runtime/src/graphics/wasm.rs`
- Wraps `web-sys` and Canvas API

#### MockBackend (testing)
- `runtime/src/graphics/mock.rs`
- Records all operations for verification
- No actual rendering

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

## Implementation Strategy

### Phase 1: Setup (SDL2)
1. Create `runtime/src/graphics/mod.rs` with trait definition
2. Create `runtime/src/graphics/sdl2.rs` with SDL2 implementation
3. Create `runtime/include/qb64fresh_gfx.h` with C FFI
4. Create `runtime/src/graphics_ffi.rs` for FFI glue code
5. Create `runtime/src/graphics/mock.rs` for testing
6. Update `runtime/Cargo.toml` with SDL2 optional dependency

### Phase 2: Code Generation Integration
1. Extend C backend to emit graphics FFI calls
2. Update semantic analysis to recognize graphics statements
3. Add graphics statements to code generation

### Phase 3: Future Backends
- Switch backends by changing feature flag in `Cargo.toml`
- No changes needed to compiler or generated code
- Compiler remains backend-agnostic

## Feature Flags

```toml
[features]
default = ["graphics-sdl2"]
graphics-sdl2 = ["sdl2"]
graphics-native = []
graphics-wasm = ["web-sys", "wasm-bindgen"]
# Can have multiple enabled for cross-compilation

# At runtime or build-time, select active backend
```

## Benefits of This Approach

| Benefit | How It's Achieved |
|---------|------------------|
| **Flexibility** | New backends added without touching compiler |
| **Testability** | Mock backend enables unit/integration tests |
| **Performance** | Backends can optimize without API changes |
| **Maintenance** | Each backend isolated in its own module |
| **Learning** | Clear separation of concerns |
| **Future-Proof** | Easy to add native APIs, WebAssembly, etc. |
| **Decoupling** | Compiler knows nothing about graphics implementation |

## Comparison with QB64pe

QB64pe hardcodes SDL2 into the C generation. Our approach is more flexible:

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| Graphics API | SDL2 (hardcoded) | Trait-based (pluggable) |
| Native Support | Limited (Windows mainly) | Easy to add later |
| WebAssembly | Not supported | Planned support |
| Testing | Requires display | Can use mock backend |
| Code Reuse | Graphics logic in C | Reusable Rust trait |

