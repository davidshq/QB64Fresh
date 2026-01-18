# ADR-0004: Build System and Tooling

## Status

**Accepted** - January 16, 2026

## Context

The project needs a build system to manage dependencies, compilation, testing, and tooling. QB64pe uses custom shell scripts and makefiles, which can be fragile and platform-specific.

Key considerations:
- Must integrate with chosen implementation language
- Should support multi-crate workspace (compiler + runtime + LSP)
- Need consistent experience across platforms
- Should include testing and quality tools

## Decision

**We chose Cargo as the primary build system, with auxiliary orchestration tools**.

### Architecture

- **Cargo**: Rust's standard build system for core functionality
  - Dependency management
  - Compilation
  - Testing
  - Documentation generation
- **Additional tooling**:
  - `just` or `make` for orchestrating multi-step builds
  - GitHub Actions for CI/CD
  - `cargo fmt` for code formatting
  - `cargo clippy` for linting

### Rationale

1. **Standard Rust tooling**: Cargo is the de facto standard
2. **Excellent dependency management**: Handles transitive dependencies, versions
3. **Integrated testing**: Built-in `cargo test` with parallel execution
4. **Workspace support**: Manages multiple related crates
5. **Documentation generation**: `cargo doc` produces browsable API docs
6. **Quality tools**: `fmt` and `clippy` enforce consistency and best practices
7. **Cross-platform**: Works identically on Windows, Linux, macOS

### Alternatives Considered

- **CMake**: Complex, more suited to C/C++ projects
- **Bazel**: Powerful but overkill, steep learning curve
- **Custom scripts**: Fragile, hard to maintain across platforms
- **Make**: Platform differences, less integrated with Rust ecosystem

## Consequences

### Positive

- Standard tooling reduces friction for contributors
- Cargo handles dependency resolution automatically
- Built-in test runner with parallel execution
- `cargo fmt` ensures consistent code style
- `cargo clippy` catches common mistakes
- Workspace feature manages related crates cleanly
- GitHub Actions has excellent Rust support
- Documentation generated automatically

### Negative

- Cargo can be slow for large projects (caching helps)
- Some complex build steps need auxiliary tools
- Compile times can be long in debug mode
- Less control than lower-level build systems
- Learning curve for contributors new to Rust/Cargo

### Usage Patterns

```bash
# Development workflow
cargo build              # Compile
cargo test               # Run tests
cargo clippy             # Lint
cargo fmt                # Format code

# Release workflow
cargo build --release    # Optimized build
cargo doc --no-deps      # Generate docs

# Workspace operations
cargo test --workspace   # Test all crates
cargo build -p qb64fresh # Build specific crate
```
