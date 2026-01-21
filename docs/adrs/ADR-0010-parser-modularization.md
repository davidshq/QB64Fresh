# ADR-0010: Parser Modularization Strategy

## Status

**Accepted** - January 20, 2026

## Context

BASIC is a large language with many statement types spanning different domains:
- Core statements (LET, DIM, PRINT, INPUT)
- Control flow (IF, FOR, WHILE, DO, SELECT)
- Procedures (SUB, FUNCTION, TYPE)
- Graphics (SCREEN, LINE, CIRCLE, PSET, PAINT)
- Audio (BEEP, SOUND, PLAY, _SNDOPEN)
- File I/O (OPEN, CLOSE, GET, PUT, SEEK)
- System operations (SHELL, KILL, NAME, MKDIR)
- Preprocessor directives ($IF, $LET, $INCLUDE)

A monolithic parser file would be thousands of lines and difficult to navigate.

Key considerations:
- Parser must remain maintainable as language coverage expands
- Related statements should be grouped for easy reference
- Each module should be independently understandable
- We chose hand-written parser over parser generators (like chumsky) for control and error messages

## Decision

**We chose "vertical slicing" - organizing parser modules by feature domain rather than by grammar production type**.

### Module Structure

```
src/parser/
├── mod.rs            # Entry point, Parser struct, program-level parsing
├── tokens.rs         # Token navigation utilities (peek, advance, expect)
├── error.rs          # ParseError types with source spans
├── expressions.rs    # Pratt parser for all expressions
├── statements.rs     # Core statement parsing, dispatch to specialists
├── control_flow.rs   # IF, FOR, WHILE, DO, SELECT CASE
├── procedures.rs     # SUB, FUNCTION, TYPE definitions
├── directives.rs     # $IF, $LET, $CHECKING, $INCLUDE
├── graphics.rs       # SCREEN, LINE, CIRCLE, PSET, PAINT, etc.
├── audio.rs          # BEEP, SOUND, PLAY, _SND* functions
├── file_io.rs        # OPEN, CLOSE, GET, PUT, SEEK, etc.
└── system.rs         # SHELL, KILL, NAME, MKDIR, CHDIR, etc.
```

### How It Works

1. **`mod.rs`** creates the `Parser` struct and handles program-level parsing
2. **`statements.rs`** contains the main `parse_statement()` dispatch
3. Domain modules export `impl Parser` methods like `parse_screen_statement()`
4. Statement dispatch calls the appropriate specialist module

```rust
// In statements.rs
fn parse_statement(&mut self) -> Result<Statement, ()> {
    match self.peek_kind() {
        Some(TokenKind::Screen) => self.parse_screen_statement(),  // → graphics.rs
        Some(TokenKind::Beep) => self.parse_beep_statement(),      // → audio.rs
        Some(TokenKind::Open) => self.parse_open_statement(),      // → file_io.rs
        Some(TokenKind::If) => self.parse_if_statement(),          // → control_flow.rs
        // ...
    }
}
```

### Expression Parsing

Expressions use a **Pratt parser** (operator precedence parsing) in `expressions.rs`:

```rust
pub fn parse_expression(&mut self) -> Result<Expr, ()> {
    self.parse_expression_with_precedence(0)
}

fn parse_expression_with_precedence(&mut self, min_precedence: u8) -> Result<Expr, ()> {
    let mut left = self.parse_prefix()?;
    while let Some(op) = self.peek_infix_op() {
        if precedence(op) < min_precedence { break; }
        left = self.parse_infix(left, op)?;
    }
    Ok(left)
}
```

### Token Navigation (`tokens.rs`)

Shared utilities used by all parser modules:

```rust
impl Parser<'_> {
    pub fn peek(&self) -> Option<&Token>
    pub fn peek_kind(&self) -> Option<&TokenKind>
    pub fn peek_ahead(&self, n: usize) -> Option<&Token>
    pub fn advance(&mut self) -> Option<&Token>
    pub fn match_token(&mut self, kind: &TokenKind) -> bool
    pub fn expect(&mut self, kind: &TokenKind, context: &str) -> Result<&Token, ()>
    pub fn skip_to_end_of_line(&mut self)
}
```

### Rationale

1. **Domain cohesion**: All graphics parsing in one file aids comprehension
2. **Parallel development**: Different developers can work on different modules
3. **QB64pe reference**: Original compiler organizes runtime by domain - we mirror this
4. **Educational**: A learner studying audio parsing reads one focused file
5. **Extensibility**: Adding a new domain means adding a new file, not modifying a giant switch

### Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| **Monolithic parser** | Would be 10,000+ lines, hard to navigate |
| **Grammar-based splits** (stmt vs expr) | Scatters related code (e.g., SCREEN parsing separate from PSET) |
| **Parser generator (chumsky)** | Less control over error messages, learning curve |
| **One file per statement** | Too granular, overhead of many tiny files |

## Consequences

### Positive

- Each module fits in one screen session (~300-800 lines)
- Easy to find relevant code (graphics issue → `graphics.rs`)
- Parallel development without merge conflicts
- Clear ownership for maintainers
- Matches mental model of "BASIC has graphics commands, audio commands, etc."

### Negative

- Some code duplication for common patterns (could extract more utilities)
- Cross-domain statements need care (e.g., INPUT # uses both IO and core)
- Module dependencies must be managed (all import from `tokens.rs`)
- New contributors must learn the split

### Module Statistics (as of January 2026)

| Module | Lines | Statements Handled |
|--------|-------|-------------------|
| `mod.rs` | ~200 | Program structure, line labels |
| `statements.rs` | ~600 | Core: LET, DIM, PRINT, INPUT, etc. |
| `expressions.rs` | ~400 | All expressions (Pratt parser) |
| `control_flow.rs` | ~500 | IF, FOR, WHILE, DO, SELECT |
| `procedures.rs` | ~400 | SUB, FUNCTION, TYPE, DECLARE |
| `graphics.rs` | ~800 | 20+ graphics statements |
| `audio.rs` | ~300 | BEEP, SOUND, PLAY, _SND* |
| `file_io.rs` | ~450 | OPEN, CLOSE, GET, PUT, etc. |
| `system.rs` | ~200 | SHELL, KILL, NAME, etc. |
| `directives.rs` | ~300 | $IF, $LET, $CHECKING |
| `tokens.rs` | ~200 | Token navigation utilities |
| `error.rs` | ~150 | ParseError enum |

### Adding a New Statement

1. Add token to `src/lexer/token.rs` if needed
2. Add AST node to `src/ast/stmt.rs`
3. Identify the domain (graphics? audio? core?)
4. Add parsing method to the appropriate module
5. Add dispatch case in `statements.rs`
6. Add typed IR node to `src/semantic/typed_ir.rs`
7. Add semantic checking
8. Add code generation
