# Problem: Parser Token API Mismatch

**Date Started:** 2026-01-16
**Date Resolved:** 2026-01-16
**Related Session:** [session-002_parser-implementation.md](../2026-01-16_session-002_parser-implementation.md)
**Status:** Resolved

---

## Problem Statement

The parser was implemented (~1600 lines) but failed to compile with 34+ errors. The parser was written assuming a different Token API than what actually exists in the lexer.

## Initial Symptoms

Compilation failed with multiple error types:

```
error[E0107]: struct takes 0 lifetime arguments but 1 lifetime argument was supplied
   --> src/parser/mod.rs:48:20
    |
 48 |     tokens: &'a [Token<'a>],
    |                    ^^^^^--- help: remove these generics
    |                    |
    |                    expected 0 lifetime arguments

error[E0500]: closure requires unique access to `self.errors` but it is already borrowed
   --> src/parser/mod.rs:256:44
    |
256 |         let token = self.peek().ok_or_else(|| {
    |                     ----        ---------- ^^ closure construction occurs here
    |                     |           |
    |                     |           first borrow later used by call
    |                     borrow occurs here
257 |             self.errors.push(ParseError::eof("expression"));
    |             ----------- second borrow occurs due to use of `self.errors` in closure

error[E0507]: cannot move out of `token.span` which is behind a shared reference
   --> src/parser/mod.rs:169:58
    |
169 |                 (format!("{:?}", token.kind), token.span.into())
    |                                               ^^^^^^^^^^ move occurs because `token.span`
    |                                               has type `Range<usize>`, which does not
    |                                               implement the `Copy` trait
```

## Investigation Path

### Attempt 1: Fix lifetime parameters

**Hypothesis:** The Token struct has a lifetime parameter that we need to match.

**What we tried:**
- Read the actual Token definition in `src/lexer/token.rs`

**Result:** Discovered Token has no lifetime - it owns its data:
```rust
pub struct Token {
    pub kind: TokenKind,
    pub span: std::ops::Range<usize>,  // Owned, not borrowed
    pub text: String,                   // Owned String, not &str
}
```

**Learning:** The parser was written assuming `Token<'a>` with borrowed `&'a str` for text, but the actual implementation owns all data. Need to change all `Token<'a>` to `Token`.

---

### Attempt 2: Remove lifetime parameters

**Hypothesis:** Simply removing `<'a>` from Token references will fix the lifetime errors.

**What we tried:**
- Changed `Parser<'a>` struct to use `&'a [Token]` instead of `&'a [Token<'a>]`
- Used replace_all to change all `&Token<'a>` to `&Token`

**Result:** Lifetime errors fixed, but still had borrow checker errors with closures and span conversion errors.

**Learning:** Fixing one class of errors revealed the deeper issues with the borrow checker and type assumptions.

---

### Attempt 3: Fix borrow checker issues with closures

**Hypothesis:** The `.ok_or_else(|| { self.errors.push(...) })` pattern creates a closure that borrows `self.errors` while `self` is already borrowed by the method call.

**What we tried:**
- Restructured from:
```rust
let token = self.peek().ok_or_else(|| {
    self.errors.push(ParseError::eof("expression"));
})?;
```
To:
```rust
let token = match self.peek() {
    Some(t) => t,
    None => {
        self.errors.push(ParseError::eof("expression"));
        return Err(());
    }
};
```

**Result:** Fixed the basic cases, but more complex cases still had issues where we needed token data AND self.errors.

**Learning:** The closure pattern is elegant but incompatible with Rust's borrow rules when you need to access multiple parts of self. Explicit match is verbose but works.

---

### Attempt 4: Extract values before borrowing self again

**Hypothesis:** For cases where we need token data for error messages, we can clone/extract the needed values first, releasing the borrow on self.

**What we tried:**
```rust
let op_token = match self.advance() {
    Some(t) => t,
    None => { ... }
};

// Extract values we need before borrowing self again
let op_kind = op_token.kind.clone();
let op_span: Span = op_token.span.clone().into();

// Now we can use self.errors because op_token borrow is released
let op = match Self::token_to_binary_op(&op_kind) {
    Some(o) => o,
    None => {
        self.errors.push(ParseError::syntax(
            format!("expected operator, found {:?}", op_kind),
            op_span,
        ));
        return Err(());
    }
};
```

**Result:** This pattern worked for all remaining borrow checker issues.

**Learning:** Clone early, borrow late. Extract what you need from borrowed data before you need to borrow self again.

---

### Attempt 5: Fix Range<usize> span conversion

**Hypothesis:** `Range<usize>` is not Copy, so `token.span.into()` tries to move out of a borrow.

**What we tried:**
- Changed `token.span.into()` to `token.span.clone().into()`

**Result:** All span conversion errors fixed.

**Learning:** `Range<T>` is not Copy even when T is Copy. Always clone ranges before converting.

---

## Resolution

**Root Cause:** Multiple API assumptions were wrong:
1. Token doesn't have a lifetime parameter (owns its data)
2. Closure-based error handling conflicts with Rust's borrow rules
3. `Range<usize>` is not Copy

**Solution:** Applied these patterns throughout the parser:
1. Use `&[Token]` instead of `&[Token<'a>]`
2. Use explicit `match` instead of `.ok_or_else()` closures
3. Extract needed values (clone) before accessing `self.errors`
4. Clone spans before converting: `token.span.clone().into()`

**Why it worked:** These changes align with Rust's ownership model - we're explicit about what we borrow and when, avoiding conflicting borrows.

---

## Key Takeaways

1. **Always read the actual API before writing dependent code** - A quick `Read` of token.rs would have revealed the ownership model
2. **The "extract then use" pattern** is essential for Rust error handling when you need data for error messages
3. **`Range<T>` quirk** - Even with `T: Copy`, Range itself is not Copy
4. **Closure borrowing** - Closures capture their environment; if that conflicts with an existing borrow, use explicit control flow instead

## References

- `src/lexer/token.rs` - Actual Token definition
- `src/parser/mod.rs` - Parser implementation with fixes
- Rust borrow checker documentation on closure captures
