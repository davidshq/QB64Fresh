# Parser Implementation Plan

**Created:** 2026-01-16
**Status:** Planning
**Milestone:** Phase 3.2 - Parser Implementation

---

## Overview

The parser transforms a stream of tokens (from the lexer) into an Abstract Syntax Tree (AST). This is the second major phase of the compiler pipeline.

```
Source (.bas) → [Lexer] → Tokens → [Parser] → AST → Semantic Analysis → ...
                              ↑
                          WE ARE HERE
```

### Goals

1. **Correctness** - Parse valid BASIC programs into accurate ASTs
2. **Error recovery** - Don't bail on first error; collect multiple diagnostics
3. **Educational** - Clear code demonstrating parsing techniques
4. **Incremental** - Build up capability step by step

---

## Architecture

### Module Structure

```
src/
├── ast/
│   ├── mod.rs          # Module root, re-exports
│   ├── expr.rs         # Expression AST nodes
│   ├── stmt.rs         # Statement AST nodes
│   ├── types.rs        # Type representations
│   └── visitor.rs      # Visitor trait for AST traversal (optional, later)
│
├── parser/
│   ├── mod.rs          # Parser struct, main entry point
│   ├── expr.rs         # Expression parsing (Pratt/precedence climbing)
│   ├── stmt.rs         # Statement parsing
│   └── error.rs        # Parse errors with source locations
```

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Parsing technique | Pratt parsing (precedence climbing) | Elegant for expressions, easy to extend |
| Error handling | Collect errors, continue parsing | Better UX than stopping at first error |
| AST ownership | Owned nodes (no lifetimes) | Simpler; AST outlives source for later phases |
| Location tracking | Span on every node | Required for good error messages |

---

## Phase 1: Foundation (Minimal AST + Parser Shell)

### Step 1.1: Create AST Module Structure

Create `src/ast/mod.rs` with basic types:

```rust
// src/ast/mod.rs
mod expr;
mod stmt;

pub use expr::*;
pub use stmt::*;

/// Source location information
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// A complete BASIC program
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
```

### Step 1.2: Define Expression AST

Start minimal, expand later:

```rust
// src/ast/expr.rs

/// Expression node with source location
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    // Literals
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // Identifiers
    Identifier(String),

    // Binary operations: left op right
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },

    // Unary operations: op expr
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    // Parenthesized expression
    Grouped(Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, IntDiv, Mod, Pow,
    // Comparison
    Eq, NotEq, Lt, LtEq, Gt, GtEq,
    // Logical
    And, Or, Xor,
    // String
    Concat,  // + for strings, but semantically different
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,    // -x
    Not,    // NOT x
}
```

### Step 1.3: Define Statement AST

Start with essentials:

```rust
// src/ast/stmt.rs

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    // PRINT expr1, expr2, ...
    Print {
        values: Vec<Expr>,
        newline: bool,  // trailing semicolon suppresses newline
    },

    // LET x = expr  (LET is optional in BASIC)
    Let {
        name: String,
        value: Expr,
    },

    // DIM name AS type
    Dim {
        name: String,
        type_spec: Option<TypeSpec>,
    },

    // IF condition THEN ... [ELSE ...] END IF
    If {
        condition: Expr,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },

    // FOR var = start TO end [STEP step] ... NEXT
    For {
        variable: String,
        start: Expr,
        end: Expr,
        step: Option<Expr>,
        body: Vec<Statement>,
    },

    // WHILE condition ... WEND
    While {
        condition: Expr,
        body: Vec<Statement>,
    },

    // Expression as statement (function calls, etc.)
    Expression(Expr),

    // END
    End,

    // Comment (preserved for potential formatting tools)
    Comment(String),
}

#[derive(Debug, Clone)]
pub enum TypeSpec {
    Integer,
    Long,
    Single,
    Double,
    String,
    // QB64 extensions (later)
}
```

### Step 1.4: Create Parser Shell

```rust
// src/parser/mod.rs
mod expr;
mod stmt;
mod error;

pub use error::ParseError;

use crate::lexer::{Token, TokenKind};
use crate::ast::{Program, Statement, Expr, Span};

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    current: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Program, Vec<ParseError>> {
        let statements = self.parse_program();

        if self.errors.is_empty() {
            Ok(Program { statements })
        } else {
            Err(self.errors)
        }
    }

    // Token navigation helpers
    fn peek(&self) -> Option<&Token<'a>> { ... }
    fn advance(&mut self) -> Option<&Token<'a>> { ... }
    fn check(&self, kind: TokenKind) -> bool { ... }
    fn consume(&mut self, kind: TokenKind, msg: &str) -> Result<&Token<'a>, ()> { ... }
    fn is_at_end(&self) -> bool { ... }
}
```

### Step 1.5: Create Parse Error Type

```rust
// src/parser/error.rs
use crate::ast::Span;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token at {span:?}: expected {expected}, found {found}")]
    UnexpectedToken {
        span: Span,
        expected: String,
        found: String,
    },

    #[error("Unexpected end of file")]
    UnexpectedEof,

    #[error("Invalid expression at {span:?}")]
    InvalidExpression { span: Span },
}
```

---

## Phase 2: Expression Parsing (Pratt Parser)

### Step 2.1: Implement Precedence Table

```rust
// src/parser/expr.rs

/// Operator precedence levels (higher = binds tighter)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    None = 0,
    Or = 1,          // OR, XOR
    And = 2,         // AND
    Not = 3,         // NOT (unary)
    Comparison = 4,  // =, <>, <, <=, >, >=
    Additive = 5,    // +, -
    Multiplicative = 6, // *, /, \, MOD
    Unary = 7,       // - (unary)
    Power = 8,       // ^
    Primary = 9,     // literals, identifiers, (grouped)
}

fn get_precedence(kind: &TokenKind) -> Precedence {
    match kind {
        TokenKind::Or | TokenKind::Xor => Precedence::Or,
        TokenKind::And => Precedence::And,
        TokenKind::Equals | TokenKind::NotEquals |
        TokenKind::LessThan | TokenKind::LessEquals |
        TokenKind::GreaterThan | TokenKind::GreaterEquals => Precedence::Comparison,
        TokenKind::Plus | TokenKind::Minus => Precedence::Additive,
        TokenKind::Star | TokenKind::Slash |
        TokenKind::Backslash | TokenKind::Mod => Precedence::Multiplicative,
        TokenKind::Caret => Precedence::Power,
        _ => Precedence::None,
    }
}
```

### Step 2.2: Implement Pratt Parser Core

```rust
impl<'a> Parser<'a> {
    /// Parse expression with given minimum precedence
    pub fn parse_expression(&mut self, min_precedence: Precedence) -> Result<Expr, ()> {
        // Parse prefix (literals, identifiers, unary ops, grouped)
        let mut left = self.parse_prefix()?;

        // Parse infix operators while they have sufficient precedence
        while let Some(token) = self.peek() {
            let precedence = get_precedence(&token.kind);
            if precedence <= min_precedence {
                break;
            }

            left = self.parse_infix(left, precedence)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ()> {
        let token = self.advance().ok_or(())?;

        match &token.kind {
            TokenKind::IntegerLiteral => { /* parse integer */ }
            TokenKind::FloatLiteral => { /* parse float */ }
            TokenKind::StringLiteral => { /* parse string */ }
            TokenKind::Identifier => { /* parse identifier */ }
            TokenKind::LeftParen => { /* parse grouped */ }
            TokenKind::Minus => { /* parse unary minus */ }
            TokenKind::Not => { /* parse NOT */ }
            _ => { /* error: expected expression */ }
        }
    }

    fn parse_infix(&mut self, left: Expr, precedence: Precedence) -> Result<Expr, ()> {
        let op_token = self.advance().ok_or(())?;
        let op = token_to_binary_op(&op_token.kind)?;

        // For right-associative operators (^), use precedence - 1
        let right_precedence = if op == BinaryOp::Pow {
            Precedence::from(precedence as u8 - 1)
        } else {
            precedence
        };

        let right = self.parse_expression(right_precedence)?;

        Ok(Expr {
            span: Span { start: left.span.start, end: right.span.end },
            kind: ExprKind::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
        })
    }
}
```

### Step 2.3: Test Expression Parsing

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_precedence_mul_before_add() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let ast = parse_expr("1 + 2 * 3");
        assert_matches!(ast.kind, ExprKind::Binary { op: BinaryOp::Add, .. });
    }

    #[test]
    fn test_precedence_power_right_associative() {
        // 2 ^ 3 ^ 4 should parse as 2 ^ (3 ^ 4)
        let ast = parse_expr("2 ^ 3 ^ 4");
        // Verify right associativity
    }

    #[test]
    fn test_parentheses_override() {
        // (1 + 2) * 3 should parse as (1 + 2) * 3
        let ast = parse_expr("(1 + 2) * 3");
        assert_matches!(ast.kind, ExprKind::Binary { op: BinaryOp::Mul, .. });
    }
}
```

---

## Phase 3: Statement Parsing

### Step 3.1: Implement Statement Dispatcher

```rust
impl<'a> Parser<'a> {
    fn parse_statement(&mut self) -> Result<Statement, ()> {
        // Skip any leading newlines
        self.skip_newlines();

        let token = self.peek().ok_or(())?;

        match &token.kind {
            TokenKind::Print => self.parse_print(),
            TokenKind::Let => self.parse_let(),
            TokenKind::Dim => self.parse_dim(),
            TokenKind::If => self.parse_if(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::End => self.parse_end(),
            TokenKind::Comment => self.parse_comment(),
            TokenKind::Identifier => {
                // Could be: assignment (x = 5) or call (MySub arg1, arg2)
                self.parse_identifier_statement()
            }
            _ => {
                self.error("Expected statement");
                Err(())
            }
        }
    }
}
```

### Step 3.2: Implement Individual Statement Parsers

Each statement type gets its own parsing function:

- `parse_print()` - Handle PRINT with multiple expressions, semicolons
- `parse_let()` - Handle LET (optional keyword) assignment
- `parse_dim()` - Handle DIM with optional AS type
- `parse_if()` - Handle single-line and multi-line IF
- `parse_for()` - Handle FOR...NEXT loops
- `parse_while()` - Handle WHILE...WEND loops

### Step 3.3: Handle BASIC-Specific Quirks

BASIC has several parsing challenges:

1. **Line-oriented syntax** - Newlines are significant
2. **Optional LET** - `x = 5` is same as `LET x = 5`
3. **Single-line IF** - `IF x > 0 THEN PRINT x` vs multi-line
4. **PRINT formatting** - Semicolons, commas have special meaning
5. **Statement separators** - Colon `:` allows multiple statements per line

---

## Phase 4: Integration & Testing

### Step 4.1: Update lib.rs

```rust
pub mod ast;
pub mod parser;
pub mod lexer;
```

### Step 4.2: Update CLI

Add `--ast` flag to dump parsed AST:

```rust
if args.ast {
    let tokens = lex(&source);
    let ast = Parser::new(&tokens).parse()?;
    println!("{:#?}", ast);
}
```

### Step 4.3: Create Integration Tests

```
tests/
├── parser/
│   ├── expressions.rs    # Expression parsing tests
│   ├── statements.rs     # Statement parsing tests
│   └── programs.rs       # Full program parsing tests
```

### Step 4.4: Test Against Real BASIC Programs

Use simple programs from `QB64pe/tests/qbasic_testcases/qb45com/` as test cases.

---

## Implementation Order (Checklist)

### Foundation
- [ ] Create `src/ast/mod.rs` with Span, Program
- [ ] Create `src/ast/expr.rs` with Expr, ExprKind, BinaryOp, UnaryOp
- [ ] Create `src/ast/stmt.rs` with Statement, StatementKind
- [ ] Create `src/parser/mod.rs` with Parser struct
- [ ] Create `src/parser/error.rs` with ParseError
- [ ] Update `src/lib.rs` to export new modules

### Expression Parsing
- [ ] Implement token navigation helpers (peek, advance, check, consume)
- [ ] Implement precedence table
- [ ] Implement `parse_prefix()` for literals and identifiers
- [ ] Implement `parse_prefix()` for unary operators
- [ ] Implement `parse_prefix()` for parenthesized expressions
- [ ] Implement `parse_infix()` for binary operators
- [ ] Implement `parse_expression()` with precedence climbing
- [ ] Write expression parsing tests

### Statement Parsing
- [ ] Implement `parse_statement()` dispatcher
- [ ] Implement `parse_print()`
- [ ] Implement `parse_let()` (assignment)
- [ ] Implement `parse_if()` (single-line first)
- [ ] Implement `parse_if()` (multi-line)
- [ ] Implement `parse_for()`
- [ ] Implement `parse_while()`
- [ ] Implement `parse_dim()`
- [ ] Write statement parsing tests

### Integration
- [ ] Implement `parse_program()` to parse full files
- [ ] Add `--ast` flag to CLI
- [ ] Write integration tests with real BASIC programs
- [ ] Update documentation

---

## References

- [Pratt Parsing (Matklad)](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) - Excellent explanation
- [Crafting Interpreters - Parsing Expressions](https://craftinginterpreters.com/parsing-expressions.html)
- QB64 Wiki for BASIC syntax reference

---

## Success Criteria

Parser is complete when:

1. ✓ Can parse this program correctly:
```basic
' Hello World with variables
DIM x AS INTEGER
x = 5
PRINT "Value is:"; x
IF x > 0 THEN
    PRINT "Positive"
ELSE
    PRINT "Not positive"
END IF
FOR i = 1 TO 10
    PRINT i
NEXT
END
```

2. ✓ Produces helpful error messages for invalid input
3. ✓ All tests pass
4. ✓ Documentation updated

---

*Last updated: 2026-01-16*
