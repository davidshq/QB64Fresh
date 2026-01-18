# Graphics Backend Usage Examples

This document demonstrates how to use the pluggable graphics backend system in QB64Fresh.

## Building with Different Backends

### With SDL2 (Default)
```bash
cd QB64Fresh
cargo build --release
```

### For Testing (Mock Backend, No Display Required)
```bash
cargo build --lib --no-default-features --features graphics-mock
cargo test --lib
```

## Using the Graphics Backend from Rust

### Example: Direct Backend Usage (Internal)

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
    backend.pset(101, 101, 14)?;
    
    // Draw a line
    backend.line(0, 0, 319, 199, 12, false)?;
    
    // Update display
    backend.display()?;
    
    // Poll for events (returns false if window should close)
    while backend.poll_events()? {
        // Do some drawing
        backend.pset(150, 150, 11)?;
        backend.display()?;
    }
    
    // Cleanup
    backend.shutdown()?;
    Ok(())
}
```

## Using the Graphics Backend from C

The graphics backend exposes a C FFI layer (to be implemented):

```c
// qb64fresh_gfx.h

// Initialization
int qb64_gfx_init(uint32_t width, uint32_t height);
int qb64_gfx_shutdown(void);

// Drawing
int qb64_gfx_pset(int32_t x, int32_t y, uint32_t color);
int qb64_gfx_line(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color);
int qb64_gfx_circle(int32_t x, int32_t y, int32_t radius, uint32_t color);
int qb64_gfx_cls(void);

// Display
int qb64_gfx_display(void);
int qb64_gfx_poll_events(void);
```

Generated QB64 code would call:

```c
// Generated from: SCREEN 13, 0, 0, 0
qb64_gfx_init(320, 200);

// Generated from: PSET (100, 100), 15
qb64_gfx_pset(100, 100, 15);

// Generated from: LINE (0,0) - (100,100)
qb64_gfx_line(0, 0, 100, 100, 15);

// Generated from: DISPLAY
qb64_gfx_display();
```

## Testing with the Mock Backend

The mock backend is perfect for testing graphics-heavy programs without a display:

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
        
        // Check that pset was called
        assert!(backend.has_operation(&MockOperation::Pset(100, 100, 15)));
        
        // Count how many times display was called
        assert_eq!(backend.operation_count(&MockOperation::Display), 1);
        
        backend.shutdown()?;
        Ok(())
    }
}
```

## Adding a New Backend

To add a new backend (e.g., native Windows GDI):

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

### 2. Update the Feature System

```toml
# runtime/Cargo.toml
[features]
default = ["graphics-sdl2"]
graphics-sdl2 = ["sdl2"]
graphics-win32 = ["windows"]  # New backend
```

### 3. Conditional Compilation

```rust
// runtime/src/graphics/mod.rs

#[cfg(feature = "graphics-win32")]
pub mod native_win32;

pub fn init_graphics(width: u32, height: u32) -> Result<(), GraphicsError> {
    #[cfg(feature = "graphics-sdl2")]
    { /* SDL2 init */ }
    
    #[cfg(feature = "graphics-win32")]
    { /* Win32 init */ }
    
    #[cfg(not(any(feature = "graphics-sdl2", feature = "graphics-win32")))]
    { /* Mock backend fallback */ }
}
```

### 4. Build with New Backend

```bash
cargo build --no-default-features --features graphics-win32
```

## Architecture Benefits

| Scenario | Benefit |
|----------|---------|
| **Development** | Use SDL2 for cross-platform dev |
| **CI/Testing** | Use mock backend without display |
| **Native Performance** | Switch to platform-specific backend |
| **Cross-compilation** | Compile for Windows from Linux using native backend |
| **WebAssembly** | Add WASM backend later for in-browser QB64 |

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

## Performance Considerations

1. **Batching**: Call `begin_batch()` and `end_batch()` for multiple draw operations
2. **Frame Rate**: Use `poll_events()` loop timing for 60 FPS target
3. **Caching**: SDL2 backend can cache color conversions for frequently used colors
4. **Hardware Acceleration**: SDL2 backend can be extended with GPU-accelerated drawing

## Future Extensions

Planned features that the abstraction supports:

- [ ] Image loading and blitting (`_PUTIMAGE`, `_LOADIMAGE`)
- [ ] Font rendering (`_PRINTSTRING`)
- [ ] Sprite support (multiple images)
- [ ] Hardware acceleration options
- [ ] WebAssembly Canvas and WebGL
- [ ] Native platform APIs (GDI+, Quartz, X11)
- [ ] 3D graphics (via OpenGL/Vulkan backend)
