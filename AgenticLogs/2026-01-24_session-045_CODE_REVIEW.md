# Code Review: Windows Path Normalization (Session 045)

## Scope

All code touched for the "Windows-specific path handling" feature:  
`system.rs`, `legacy.rs`, `graphics.rs`, `runtime/io.rs`, and (for dependency/order) `file.rs`.

---

## 1. Bugs

### 1.1 Pre-existing: `file.rs` `qb_file_open` – NULL from `_qb_normalize_path`

**Location:** `src/codegen/c_backend/runtime/file.rs` (unchanged by this feature)

```c
char* normalized = _qb_normalize_path(filename);
_qb_files[fnum] = fopen(normalized, mode);  // UB if normalized == NULL
free(normalized);
```

`_qb_normalize_path` returns `NULL` if `path` is `NULL` or `malloc` fails.  
`fopen(NULL, mode)` is undefined behavior; `free(NULL)` is fine.

**Fix:** Only call `fopen` when `normalized` is non-NULL, and always `free(normalized)`:

```c
char* normalized = _qb_normalize_path(filename);
if (normalized) { _qb_files[fnum] = fopen(normalized, mode); }
free(normalized);
```

---

### 1.2 Pre-existing: `qb_rmdir` missing from inline C runtime

**Location:** `src/codegen/c_backend/runtime/system.rs`

`qb_rmdir` is emitted by the stmt layer and implemented in the Rust runtime, but **not** in the inline C runtime. With `--runtime inline`, any use of `RMDIR` leads to an undefined reference at link time.

**Fix:** Add `qb_rmdir` to `system.rs`, with the same pattern as `qb_mkdir`/`qb_file_kill` (on `#ifndef _WIN32`: strdup → normalize → `rmdir`/`remove`-style API if available, or `rmdir` on Unix → free). `remove()` in C typically only does files; for directories we need `rmdir`. On Windows `_rmdir`; on Unix `rmdir` from `<unistd.h>`. System already includes conditionally; we need to emit a `qb_rmdir` that uses `rmdir` (and on Windows `_rmdir` via an appropriate define or include).

---

### 1.3 Pre-existing: `qb_writefile` / `qb_readfile` – no NULL check on `content` / path

**Locations:**  
- Inline C: `graphics.rs` `qb_writefile` uses `content->data` and `content->len` without checking `content`.  
- Same for `qb_readfile` and `path` in some code paths; we at least check `path` and `path->data` at the top.

For `qb_writefile`, if `content` is NULL, `content->data` / `content->len` is UB.  
**Fix (suggested):** In both inline C and, if used, any Rust equivalent, guard:  
`if (!content || !content->data) return;` before using `content`.

---

## 2. Bad practices / quality

### 2.1 `_qb_normalize_path` only in `file.rs`; `legacy` and `graphics` depend on emission order

**Location:** `file.rs` defines `static … _qb_normalize_path(…)` under `#ifndef _WIN32`.  
`legacy.rs` and `graphics.rs` call `_qb_normalize_path` only inside `#ifndef _WIN32`.

**Emission order** (from `runtime/mod.rs`):  
`system` → … → `file` → … → `graphics` → `legacy`.

So `_qb_normalize_path` is always defined before any use. This is fine but **fragile**: reordering or splitting emitted C could break. A short comment in `legacy` and `graphics` that `_qb_normalize_path` is provided by `file` would help.

---

### 2.2 Inline C vs Rust return convention

- **Inline C:** `qb_file_kill`, `qb_mkdir`, `qb_chdir`, `qb_file_rename` return **0** on success and **-1** on failure.
- **Rust runtime:** same functions return **0** on success and **1** on failure.

The BASIC codegen does not use these return values, and inline vs external runtime are not mixed in one build, so behavior is consistent for BASIC. The split is a **documentation/maintainability** issue and can confuse C/FFI contracts. Worth documenting and, longer term, aligning (e.g. all C 0/-1 or all 0/1 and a single doc).

---

### 2.3 `normalize_path_for_fs` lifetime in Rust

The helper returns `Cow<str>`. The `mismatched_lifetime_syntaxes` (or similar) lint about elided vs explicit lifetime can be resolved by using `Cow<'_, str>` in the signature. Cosmetic but cleans up a warning.

---

## 3. Possible loss of functionality

### 3.1 `create_dir` vs `create_dir_all`

Rust `qb_mkdir` uses `std::fs::create_dir`, which creates a single directory.  
`MKDIR "a\b\c"` normalized to `"a/b/c"` will fail if `a` or `a/b` do not exist.  

This matches the previous behavior and is consistent with typical BASIC `MKDIR` semantics (one level). **No change suggested** unless you explicitly want `create_dir_all` and to document that.

---

### 3.2 UNC and “double” path edge cases

- Paths like `\\server\share\file` on non-Windows become `//server/share/file`. Behavior is platform-dependent; we do not special-case.
- Consecutive backslashes `\` after normalization can become `//`. On typical Unix systems this is collapsed to a single `/`, so **no functionality lost** in normal use.

---

## 4. What was done well

- **Null checks:**  
  - Inline C: `strdup` failure in `qb_chdir`, `qb_mkdir`, `qb_file_kill` returns -1; `qb_file_rename` checks `!o || !n` and frees both before returning -1.  
  - All `_qb_normalize_path` uses in `legacy` and `graphics` are behind `if (n) { … }` before `fopen` and `free(n)`, so no `fopen(NULL, …)`.
- **free(NULL):** In `qb_file_rename`, `free(o); free(n);` is safe when one of `o`/`n` is NULL.
- **Platform split:** Normalization only on `#ifndef _WIN32` (C) and `#[cfg(not(target_os = "windows"))]` (Rust); no unnecessary work on Windows.
- **Rust `Cow`:** `normalize_path_for_fs` avoids allocation when the path has no backslash; temporaries are correctly bound when passing into `Path::new(…)` to satisfy the borrow checker.
- **Emission order:** `file` before `graphics` and `legacy` ensures `_qb_normalize_path` is defined before use.

---

## 5. Recommended immediate fixes

1. **`file.rs`:** Guard `fopen` on `_qb_normalize_path` result:
   - `if (normalized) { _qb_files[fnum] = fopen(normalized, mode); }`
   - then `free(normalized);`
2. **`system.rs`:** Add `qb_rmdir` with the same pattern as `qb_mkdir`/`qb_file_kill` (normalize on non-Windows, `rmdir`/`_rmdir`), so `--runtime inline` + `RMDIR` link.
3. **`graphics.rs` `qb_writefile`:** Add `if (!content || !content->data) return;` before `fwrite(content->data, …)`.
4. **Rust:** Use `Cow<'_, str>` for `normalize_path_for_fs` to clear the lifetime warning.

---

## 6. Optional / follow-up

- **`qb_readfile`:** We already guard `path` and `path->data`; consider the same for any `content`-like buffer if it can be NULL.
- **Return convention:** Document C (0/-1) vs Rust (0/1) for these helpers and consider unifying in a later change.
- **Comments:** In `legacy` and `graphics`, add a one-liner that `_qb_normalize_path` is defined in the file I/O runtime (e.g. in `file`).

---

## 7. Fixes applied (post-review)

- **file.rs:** `fopen(normalized, mode)` only when `normalized` is non-NULL; `free(normalized)` always.
- **system.rs:** `qb_rmdir` added with same pattern as `qb_mkdir` (normalize on non-Windows; `_rmdir` on Windows, `rmdir` on Unix).
- **graphics.rs qb_writefile:** Guard `!content || !content->data` before using `content`.
- **runtime/io.rs:** `normalize_path_for_fs` return type set to `Cow<'_, str>` to satisfy lifetime lint.
