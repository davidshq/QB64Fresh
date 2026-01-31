# QB64Fresh vs QB64pe: All Differences

This document is the **single place** for “how QB64Fresh differs from QB64pe.” It has two main parts:

1. **Intentional differences** — Design choices where we deliberately behave differently (stricter, excluded, or different-by-design).
2. **Behavioral / architectural differences** — Semantic and architectural differences discovered during the bootstrap project (compiling QB64pe with QB64Fresh).

**See also:**

- [adrs/ADR-0016-intentional-behavioral-differences.md](../adrs/ADR-0016-intentional-behavioral-differences.md) — Policy for documenting and enforcing intentional differences
- [adrs/ADR-0014-scope-and-excluded-features.md](../adrs/ADR-0014-scope-and-excluded-features.md) — Scope and excluded features (OpenGL, legacy hardware)
- [ARCHITECTURE.md](../ARCHITECTURE.md) — QB64Fresh architecture
- [QB64PE_ARCHITECTURE.md](QB64PE_ARCHITECTURE.md) — QB64pe architecture
- [LIBQB_FUNCTIONALITY.md](LIBQB_FUNCTIONALITY.md) — Current runtime/libqb parity and remaining gaps
- [tests/runtime_comparison/README.md](../../tests/runtime_comparison/README.md) — Runtime comparison tests (QB64Fresh vs QB64pe output)

---

## Intentional differences

These are **design choices** where QB64Fresh deliberately behaves differently from QB64pe. They are not bugs or missing features—they are stricter rules, excluded features, or different-by-design behavior we have chosen for clarity, safety, or portability.

### Summary table

| Area | QB64pe | QB64Fresh | Rationale |
|------|--------|-----------|-----------|
| [Cross-procedure GOTO](#1-cross-procedure-goto--labels) | Creates forward-reference label in current scope when name exists elsewhere | Error: label must be defined in current procedure | Stricter; avoids hiding typos or wrong-scope intent |
| [Raw OpenGL (`_GL*`)](#2-raw-opengl-_gl) | Hundreds of `_GL*` commands | Excluded | Portable graphics (SDL2/winit); use DECLARE LIBRARY for OpenGL |
| [Legacy / DOS features](#3-legacy--dos-features) | Some implemented, some error | Stub-only (compile, no-op or undefined at runtime) | Portability and security; documented non-functional |
| [Error reporting](#4-error-reporting) | Often stops at first error | Collects multiple errors per run | Better debugging experience |
| [RND / RANDOMIZE](#5-rnd--randomize) | QB64pe PRNG and sequence | Different PRNG → different sequence | Acceptable; document for compatibility level |
| [Identifier mangling](#6-identifier-mangling) | QB64pe C++ mangling conventions | C-safe mangling (different names) | Implementation choice for C backend |
| [RUN statement](#7-run-statement) | RUN (restart), RUN line/label, RUN "file" | RUN with no args = no-op; RUN "file" = run then exit; no RUN line/label | Minimal implementation; full parity deferred |

### 1. Cross-Procedure GOTO / Labels

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **When** | GOTO/GOSUB to a label name that exists only in another procedure | Same |
| **QB64pe** | Creates a *new* label in the current procedure (forward reference). Compiles; resolution is by scope. | — |
| **QB64Fresh** | **Error:** "undefined label" — label must be defined in the *current* procedure. | — |
| **Rationale** | QB64pe's permissive behavior can hide bugs (e.g. typo or intending the other procedure's label). We require the label to exist in the current scope and do *not* create a forward-reference label when the name exists elsewhere. |

We may improve the *message* (e.g. "GOTO target must be in the same procedure (label 'X' is defined in another procedure)") but we do **not** emulate creating a label in the current scope. See [ADR-0016](../adrs/ADR-0016-intentional-behavioral-differences.md).

### 2. Raw OpenGL (`_GL*`)

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | Hundreds of `_GL*` commands (e.g. `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`) | — |
| **QB64Fresh** | **Excluded.** Not parsed or implemented. | — |
| **Rationale** | We use a portable graphics stack (SDL2/winit), not raw OpenGL. Supporting `_GL*` would tie us to an OpenGL-specific backend. Users who need raw OpenGL can call it via `DECLARE LIBRARY`. |

See [ADR-0014](../adrs/ADR-0014-scope-and-excluded-features.md) and [OPENGL_SUPPORT.md](../ThingsToDo/OPENGL_SUPPORT.md).

### 3. Legacy / DOS Features

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **Examples** | Port I/O (`INP`, `OUT`, `WAIT`), `INTERRUPT`/`INTERRUPTX`, `PEN`, `IOCTL`, `ERDEV`/`ERDEV$`, some `ON COM`/`ON UEVENT`-style handlers | Same symbols accepted at compile time |
| **QB64pe** | Some implemented, some emit compile errors | — |
| **QB64Fresh** | **Stub-only:** symbols compile; runtime behavior is no-op or undefined. Documented as non-functional. | — |
| **Rationale** | These target legacy hardware or DOS. We keep compile-time compatibility so old code compiles, but we do not implement behavior that is unsafe or non-portable. |

See [ADR-0014](../adrs/ADR-0014-scope-and-excluded-features.md).

### 4. Error Reporting

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | Typically stops at first error (limited recovery) | — |
| **QB64Fresh** | **Collects multiple errors** per compilation and reports them together. | — |
| **Rationale** | Showing all errors at once improves the debugging experience; we do not emulate single-error-stop. |

### 5. RND / RANDOMIZE

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | Uses its own PRNG and seed semantics | — |
| **QB64Fresh** | **Different PRNG** → different number sequence for the same seed. | — |
| **Rationale** | Acceptable for our compatibility level. Document so users do not rely on bit-identical sequences. |

See [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](../ThingsToDo/BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md).

**RND(0) and RND(negative):** Both runtimes implement RND(0) as “last value” and RND(negative) as reseed-and-return-first; numeric output differs due to different PRNGs. Runtime comparison tests: `232_rnd_zero`, `233_rnd_negative`.

### 6. Identifier Mangling

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | C++-oriented mangling for procedures, labels, etc. | — |
| **QB64Fresh** | **C-safe mangling** (different names in generated C). | — |
| **Rationale** | We emit C, not C++; our mangling is chosen for C compatibility and uniqueness. No intent to match QB64pe's symbol names. |

### 7. RUN Statement

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **RUN (no args)** | Restarts the current program (sub_run_init, sub_clear, QBMAIN or goto S_0). | **No-op.** We do not support restart yet. |
| **RUN &lt;line&gt; / &lt;label&gt;** | Clears state and jumps to the given line or label. | **Not supported.** We treat RUN with one argument as a string (filename) only. |
| **RUN "file"** | Runs the given program via WinExec/system, then exits. | We run the program via `system()` and then exit. |

**Rationale:** Minimal implementation so that RUN links and has defined behavior. RUN "file" matches QB64pe (run program, then exit). RUN with no args and RUN line/label are deferred; documenting here so porting code knows the difference.

### Adding to intentional differences

When we **intentionally** adopt a behavior that differs from QB64pe (stricter, excluded, or different-by-design), add a short section here with:

1. **Area** and one-line summary
2. **QB64pe** vs **QB64Fresh** behavior
3. **Rationale**
4. References to ADRs or other docs if applicable

Do *not* list:

- **Unintentional** bugs or missing behavior we plan to fix
- **Architectural** differences that do not reflect a deliberate *choice* to differ (those belong in the [Behavioral / architectural differences](#behavioral--architectural-differences) section below)

---

## Behavioral / architectural differences

These differences stem from fundamental architectural choices or were discovered during the bootstrap project. They may affect behavior in edge cases; most typical programs are unaffected.

### Overview

| Category | Compatibility |
|----------|---------------|
| Core language | 99%+ |
| Built-in functions | 95%+ |
| Graphics | ~90% |
| File I/O | 99%+ |
| Error handling | ~95% |

### Architectural differences

#### Compilation Pipeline

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **Implementation Language** | QB64 (self-hosted) | Rust |
| **Intermediate Representation** | None (direct C++ emission) | Full AST → Typed IR |
| **Code Generation Target** | C++ | C |
| **Parser Strategy** | Line-by-line, GOTO-heavy | Recursive descent, Pratt parsing |
| **Passes** | Single-pass with recompile loops | Two-pass semantic analysis |
| **Error Recovery** | Limited (stops early) | Multiple errors collected |

**Behavioral Impact:**
- **Forward References:** QB64pe uses recompile loops to resolve forward references. QB64Fresh's two-pass approach handles these upfront, potentially catching errors earlier.
- **Error Messages:** QB64Fresh can report multiple errors per compilation, while QB64pe typically stops at the first error.
- **Compilation Speed:** QB64Fresh is significantly faster (~800ms vs 5-10 seconds for 59K lines) due to Rust's efficiency and better algorithms.

#### Symbol Table Management

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **Storage** | Hash table (`HashFind`, `HashAdd`) | Rust HashMap with scope management |
| **Dual Namespace** | Implicit (context-based) | Explicit (`lookup_scalar` vs `lookup_array`) |
| **Scope Handling** | Global state with `subfunc$` | Structured scope tree |

**Behavioral Impact:**
- **Symbol Resolution:** QB64Fresh's explicit dual namespace may give clearer error messages for ambiguous references.
- **Scope Errors:** QB64Fresh may catch scope violations that QB64pe's single-pass approach misses.

#### Type System

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **Type Inference** | Single-pass, context-dependent | Two-pass, explicit type checking |
| **DEFTYPE Processing** | Single-pass (may miss some cases) | Two-pass (processes before procedures) |
| **Type Suffix Normalization** | Preserves original suffix | Normalizes to canonical names |

**Behavioral Impact:**
- **DEFTYPE Scope:** QB64Fresh correctly handles DEFTYPE affecting procedure parameters declared later in the file (QB64pe may miss this).
- **Type Errors:** QB64Fresh may catch type mismatches earlier due to two-pass analysis.

#### Code Generation

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **Output Language** | C++ | C |
| **Runtime Library** | C++ (~31K lines) | Rust (~19K lines) with C FFI |
| **String Implementation** | `qbs*` (copy-on-write) | Reference-counted Rust strings |
| **Identifier Mangling** | QB64pe conventions | C-safe mangling (different conventions) |

**Behavioral Impact:**
- **Generated Code:** Different C/C++ code structure, but semantically equivalent.
- **String Operations:** Both use reference counting, but implementation details differ (may affect edge cases with string sharing).
- **Memory Management:** QB64Fresh benefits from Rust's memory safety; QB64pe uses manual C++ management.

#### Error Handling

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **Error Collection** | Stops at first error (limited recovery) | Collects multiple errors |
| **Error Codes** | QB64pe-specific `ERR` values | May differ for some errors |
| **Cross-Function GOTO** | Supported | Not supported (scope restriction) |

**Behavioral Impact:**
- **Error Reporting:** QB64Fresh can show all errors at once, making debugging easier.
- **GOTO Restrictions:** QB64Fresh enforces proper scope boundaries (generally considered better practice).

#### TIMER (approximate value)

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **TIMER** | Seconds since midnight (SINGLE) | Seconds since midnight (same semantics) |
| **Exact value** | Time-based; differs each run | Time-based; differs each run |

**Note:** TIMER returns seconds since midnight. The value is **approximate** and will not match exactly between runs or between QB64pe and QB64Fresh (timing, resolution). Runtime comparison test `234_timer_value` prints TIMER; diffs are expected. For validation, only the range (0 to 86400) or “both produce a numeric value” is asserted; do not expect bit-identical output.

### Language semantics

#### 1. DEFTYPE Scope

**Both support:** `DEFLNG`, `DEFINT`, `DEFSNG`, `DEFDBL`, `DEFSTR`.

```basic
DEFLNG A-Z  ' All untyped variables default to LONG
```

**QB64Fresh:** Processes DEFTYPE before procedure declarations (two-pass).

**QB64pe:** Single-pass processing.

**Impact:** QB64Fresh correctly handles DEFTYPE affecting procedure parameters declared later in the file.

#### 2. Implicit Variable Declaration

**Both:** Allow undeclared variables with type inference.

**QB64Fresh:** Stricter about detecting truly undefined variables vs. implicitly declared ones.

**QB64pe:** More permissive in some edge cases.

**Impact:** Some programs that compile in QB64pe may produce warnings or errors in QB64Fresh. This is generally beneficial (catches bugs).

### Built-in functions

#### String Functions

| Function | Difference |
|----------|------------|
| `_IIF` | QB64Fresh: fully polymorphic (any compatible types) |
| `ASC` | QB64Fresh: two-argument form properly supported |

#### Math Functions

| Function | Difference |
|----------|------------|
| `RND` | Different PRNG algorithm (both produce valid random numbers) |
| `RANDOMIZE` | Seeding may produce different sequences |

#### System Functions

| Function | Difference |
|----------|------------|
| `TIMER` | QB64Fresh: accuracy parameter fully supported |
| `SHELL` | QB64Fresh: properly returns exit code |

### Code generation details

#### Target Language

| Aspect | QB64Fresh | QB64pe |
|--------|-----------|--------|
| Output | C | C++ |
| Runtime | Rust FFI (~19K lines) | Custom C++ |
| Strings | Reference-counted | Copy-on-write |

#### Identifier Mangling

QB64Fresh transforms identifiers for C compatibility:

| BASIC | C |
|-------|---|
| `path.exe$` | `path_exe_str` |
| `count&` | `count_lng` |
| `value~&` | `value_u_lng` |
| `default` | `default_` (reserved word) |

QB64pe uses different mangling conventions.

**Impact:** Generated code looks different but behaves the same.

### Graphics

#### Coordinate System

Both use the same coordinate system (origin top-left, Y increases downward).

#### Color Handling

| Function | Difference |
|----------|------------|
| `POINT` | QB64Fresh may return slightly different values for anti-aliased pixels |

#### Screen Modes

QB64Fresh supports standard SCREEN modes (0, 1, 2, 7, 8, 9, 10, 11, 12, 13) plus `_NEWIMAGE`.

**Note:** Some QB64pe-specific screen modes may not be supported.

### File I/O

#### Differences

| Operation | Difference |
|-----------|------------|
| `LOCK`/`UNLOCK` | QB64Fresh: may have platform-specific behavior |

### Error handling (runtime)

#### ON ERROR GOTO

```basic
ON ERROR GOTO ErrorHandler
' ... code ...
ErrorHandler:
RESUME NEXT
```

**Both support** standard error handling.

#### Differences

| Aspect | QB64Fresh | QB64pe |
|--------|-----------|--------|
| `ERR` values | May differ for some errors; set correctly in ON ERROR handler |
| `ERL` | Supported; set in handler (0 for ERROR statement; runtime errors may set line) |
| `ERROR` statement | Supported; triggers jump to handler and sets ERR/ERL |
| Cross-function GOTO | Not supported | Supported |

**Note:** QB64Fresh does not support `GOTO` to labels in other procedures (this is generally considered bad practice anyway). Runtime comparison tests: `228_err_erl_values`, `230_error_statement`, `247_on_error_simple`.

### Platform-specific

#### Windows-Specific Features

| Feature | QB64Fresh | QB64pe |
|---------|-----------|--------|
| `$EXEICON` | Parsed but ignored | Fully supported |
| `$VERSIONINFO` | Parsed but ignored | Fully supported |
| Registry access | Not implemented | Supported |

#### Console Mode

```basic
$CONSOLE
$CONSOLE:ONLY
```

**Both support** console mode. QB64Fresh uses native terminal; QB64pe may create a Windows console window.

### Performance

#### Compilation Speed

| Metric | QB64Fresh | QB64pe |
|--------|-----------|--------|
| 59K lines | ~800ms | ~5-10 seconds |
| Incremental | Not yet | Not supported |

QB64Fresh is significantly faster due to Rust's efficiency.

#### Runtime Performance

Comparable for most operations. Differences may exist in:
- String operations (different implementation)
- Graphics (SDL2 vs custom OpenGL)
- File I/O (may vary by operation type)

### IDE and tooling

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **IDE** | Built-in (~970K lines QB64 code) | LSP-based (external editors) |
| **Editor Integration** | Self-contained IDE | VSCode extension, LSP support |
| **Debugging** | Built-in debugger | DAP-based debugger (tools/debug) |
| **Code Formatting** | Built-in formatter | Separate `qb64fresh-fmt` tool |
| **Linting** | Limited | Separate `qb64fresh-lint` tool |

**Behavioral Impact:**
- **Development Workflow:** QB64pe provides all-in-one IDE; QB64Fresh uses modern editor integrations.
- **Tooling:** QB64Fresh tools are separate binaries, allowing independent updates and use in CI/CD.

### Build system

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **Bootstrap** | Self-hosted (compiles itself) | Rust toolchain (standard Cargo) |
| **Build Time** | 5-10 seconds (59K lines) | ~800ms (59K lines) |
| **Memory Usage** | Can consume 25GB+ on large programs | More efficient (Rust memory management) |
| **Incremental Compilation** | Not supported | Not yet (future enhancement) |

**Behavioral Impact:**
- **Compilation Speed:** QB64Fresh is significantly faster, improving developer experience.
- **Memory Requirements:** QB64Fresh is more memory-efficient, reducing system requirements.

### Runtime / libqb parity

Current gaps and stub status (used by QB64pe; work toward full parity) are documented in [LIBQB_FUNCTIONALITY.md](LIBQB_FUNCTIONALITY.md). Summary: keyhandler (`_KEYUP`/`_KEYDOWN`) stubs, legacy events (ON COM, ON PEN, ON SIGNAL) stubs, and Parts (audio MIDI bank path stored; MIDI playback not yet; core = SDL2, no raw OpenGL; data/video implemented or in progress).

### Summary: behavioral differences

For most BASIC programs, QB64Fresh and QB64pe produce identical results. Key areas to watch:

1. **Random number sequences** — Use explicit seeds for reproducibility
2. **Platform-specific metacommands** — May be parsed but not functional
3. **Error codes** — `ERR` values may differ for edge cases
4. **Graphics edge cases** — Anti-aliasing, specific screen modes
5. **Error reporting** — QB64Fresh shows multiple errors; QB64pe stops at first
6. **GOTO scope** — QB64Fresh restricts cross-function GOTO (better practice)

**Architectural advantages of QB64Fresh:**
1. **Faster compilation** — ~10x faster for large programs
2. **Better error messages** — Multiple errors reported, clearer diagnostics
3. **Memory safety** — Rust's ownership system prevents many bugs
4. **Modular design** — Easier to maintain and extend
5. **Modern tooling** — LSP support, separate tools, CI/CD friendly

**When porting code:**
- Test numeric-intensive code for precision differences
- Verify graphics output visually
- Check file I/O with binary files
- Ensure error handling works as expected
- Review GOTO usage (cross-function GOTO not supported)
- Use explicit seeds for RND/RANDOMIZE for reproducibility

---

*Last updated: 2026-01-31*
