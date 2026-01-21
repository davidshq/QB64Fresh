# ADR-0012: Preprocessor Architecture

## Status

**Accepted** - January 20, 2026

## Context

QB64 supports preprocessor directives that must be handled before parsing:
- `$INCLUDE: 'filename'` - Include another source file
- `$IF`, `$ELSE`, `$END IF` - Conditional compilation (future)
- `$LET` - Preprocessor variable definition (future)
- `$CHECKING:OFF/ON` - Bounds checking control (future)

Key considerations:
- `$INCLUDE` must work before lexing (included content needs tokenization)
- Circular includes must be detected
- Error messages must show include stack for debugging
- Design must accommodate future directives

## Decision

**We chose a text-based preprocessor that operates on raw source before lexing**.

### Pipeline Position

```
┌─────────────────────────────────────────────────────────────┐
│ Source File                                                 │
│ main.bas                                                    │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Preprocessor                                                │
│ - Expands $INCLUDE directives                               │
│ - Returns combined source as single string                  │
└────────────────────┬────────────────────────────────────────┘
                     │ Expanded source
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Lexer                                                       │
│ - Tokenizes the expanded source                             │
└────────────────────┬────────────────────────────────────────┘
                     │ Vec<Token>
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Parser                                                      │
│ - Handles $IF/$LET as tokens (token-level directives)       │
└─────────────────────────────────────────────────────────────┘
```

### Core API

```rust
/// Preprocesses source code, expanding $INCLUDE directives.
///
/// # Arguments
/// * `source` - The source code to preprocess
/// * `base_path` - Base directory for resolving relative includes
///
/// # Returns
/// The preprocessed source with all includes expanded, or an error.
pub fn preprocess(source: &str, base_path: &Path) -> Result<String, PreprocessorError>
```

### Error Types

```rust
pub enum PreprocessorError {
    /// Include file not found.
    FileNotFound {
        path: String,
        from_file: PathBuf,
    },

    /// Error reading an include file.
    ReadError {
        path: PathBuf,
        message: String,
    },

    /// Circular include detected.
    CircularInclude {
        path: PathBuf,
        include_stack: Vec<PathBuf>,  // Shows the full cycle
    },

    /// Maximum include depth exceeded.
    MaxDepthExceeded {
        max_depth: usize,
        path: PathBuf,
    },
}
```

### Cycle Detection

The preprocessor tracks two things:
1. **include_stack**: Currently active includes (for error messages)
2. **visited**: Set of canonical paths (for cycle detection)

```rust
struct PreprocessContext {
    include_stack: Vec<PathBuf>,
    visited: HashSet<PathBuf>,  // Canonicalized paths
    current_depth: usize,
}
```

Detection happens before reading:
```rust
let canonical = path.canonicalize()?;
if ctx.visited.contains(&canonical) {
    return Err(PreprocessorError::CircularInclude {
        path: canonical,
        include_stack: ctx.include_stack.clone(),
    });
}
```

### Include Resolution

Include paths are resolved relative to the including file:

```basic
' In /project/main.bas
$INCLUDE: 'lib/utils.bas'      ' Resolves to /project/lib/utils.bas

' In /project/lib/utils.bas
$INCLUDE: '../common.bas'      ' Resolves to /project/common.bas
$INCLUDE: 'helpers.bas'        ' Resolves to /project/lib/helpers.bas
```

### Safety Limits

```rust
const MAX_INCLUDE_DEPTH: usize = 64;
```

Prevents stack overflow from deeply nested (but non-circular) includes.

### Design: Why Pre-Lexer?

**Alternative considered:** Parse `$INCLUDE` as a token and handle in parser.

**Why we chose pre-lexer:**
1. Included content needs full lexing/parsing
2. An include in the middle of a statement must be seamless
3. Simpler mental model: one string goes to lexer
4. QB64pe does it this way

**Tradeoff:** Line number mapping becomes complex (addressed below).

### Line Number Handling

Currently, line numbers in errors refer to the **expanded** source. Future work:

```rust
// Future: Source map for include tracking
struct SourceMap {
    // Maps byte offset in expanded source to (file, line)
    entries: Vec<SourceMapEntry>,
}

struct SourceMapEntry {
    expanded_offset: usize,
    original_file: PathBuf,
    original_line: u32,
}
```

### Rationale

1. **Pre-lexer processing**: Natural position for text inclusion
2. **Recursive with depth limit**: Handles complex project structures safely
3. **Canonical path comparison**: Handles symlinks, `..`, different path forms
4. **Include stack in errors**: Helps users trace include chains
5. **Simple API**: Single function, returns string or error

### Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| **Token-level $INCLUDE** | Complex parser changes, edge cases mid-statement |
| **Separate compilation units** | Different semantics, breaks compatibility |
| **Lazy/streaming includes** | Over-complicated for BASIC file sizes |
| **No include support** | QB64 compatibility requires it |

## Consequences

### Positive

- Simple, predictable behavior
- Matches QB64pe semantics
- Cycle detection prevents infinite loops
- Depth limit prevents stack overflow
- Clean error messages with include stack

### Negative

- Line numbers in expanded source (not original files)
- Entire file must fit in memory (fine for BASIC programs)
- No incremental processing (re-expand on any change)
- Include search paths not yet configurable

### Implementation Status

| Component | Status |
|-----------|--------|
| $INCLUDE expansion | Complete |
| Cycle detection | Complete |
| Depth limit | Complete |
| Error messages with stack | Complete |
| Source map for line numbers | Planned |
| $IF/$ELSE conditional | Planned (lexer-level) |
| $LET preprocessor vars | Planned (lexer-level) |
| Include search paths | Not started |

### Files

- `src/preprocessor.rs` - Preprocessor implementation (~200 lines)
- `src/main.rs` - Calls `preprocess()` before lexing
- `src/lsp/mod.rs` - Also uses preprocessor for LSP analysis

### Future Work

1. **Source maps**: Track original file/line through expansion
2. **Include search paths**: `-I` flag for additional include directories
3. **$IF conditionals**: Conditional compilation (in lexer/parser, not preprocessor)
4. **$LET variables**: Preprocessor variable definitions
5. **Caching**: Don't re-read unchanged include files
