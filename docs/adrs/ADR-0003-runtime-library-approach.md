# ADR-0003: Runtime Library Approach

## Status

**Accepted** - January 16, 2026

## Context

QB64 requires extensive runtime support for graphics, audio, file I/O, and other operations. QB64pe uses a custom C++ runtime (~31,000 lines) with various third-party dependencies.

Key considerations:
- Need to provide QB64-compatible runtime behavior
- Should leverage modern, maintained libraries
- Must support cross-platform operation
- Should avoid inheriting technical debt

## Decision

**We chose a hybrid Rust runtime leveraging established crates**.

### Architecture

Write runtime "glue" in Rust and leverage battle-tested crates:

- **Graphics/Windowing**: `sdl2` or `winit` + `pixels`/`softbuffer`
- **Audio**: `rodio` (built on `cpal`)
- **Image loading**: `image` crate
- **Font rendering**: `fontdue` or `rusttype`

### Rationale

1. **Avoid technical debt**: Don't inherit QB64pe's 20+ year old C++ codebase
2. **Leverage expertise**: Established crates are battle-tested and optimized
3. **Unified codebase**: Everything in Rust makes maintenance easier
4. **Safety**: Rust's safety guarantees extend to runtime code
5. **Modern dependencies**: Active maintenance and security updates

### Implementation Approach

- Define Rust traits for runtime capabilities (similar to codegen backend)
- Implement using chosen crates
- Test against QB64pe's test suite for compatibility
- Document any intentional deviations

### Alternatives Considered

- **Port QB64pe C++ runtime**: Inherits technical debt, hard to maintain
- **Pure C runtime**: Manual memory management, less safe
- **Minimal runtime + system calls**: Would need to reimplement too much
- **SDL2 exclusively**: Matches QB64pe but limits flexibility

## Consequences

### Positive

- Modern, maintained dependencies
- Memory safety in runtime code
- Unified Rust tooling and build system
- Easier to contribute (one language)
- Better error handling and diagnostics
- Potential performance improvements

### Negative

- May have subtle behavioral differences from QB64pe
- Need to carefully map QB64 semantics to library capabilities
- Graphics modes and SCREEN compatibility require careful design
- Some QB64pe programs might need minor adjustments
- Testing burden to ensure compatibility
- Learning curve for understanding crate APIs

### Migration Path

When compatibility issues arise:
1. Document the difference
2. Assess if it affects real-world programs
3. If critical, add compatibility layer
4. If minor, document as intentional deviation
