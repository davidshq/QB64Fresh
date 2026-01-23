# QB64pe Bootstrap: Remaining Work

*Updated: 2026-01-23*

This document tracks what remains to complete the QB64pe bootstrap project. For full history, see [BOOTSTRAP_PLAN_FULL.md](../archive/BOOTSTRAP_PLAN_FULL.md).

---

## Current Status

**Phases A-D Complete:**
- ✅ Analysis complete (gap identification)
- ✅ Implementation complete (992 → 0 semantic errors)
- ✅ Code generation complete (0 GCC errors, ~86K lines of C)
- ✅ Linking complete (2.1MB executable builds and runs)

**What Works:**
- QB64pe executable builds without errors
- Executable starts without crashing
- Clean exit (waiting for graphical initialization)

---

## Remaining Work

### Phase E: Validation & Documentation

**Objective:** Verify correctness and document the achievement

**Tasks:**
1. [x] Create test suite for QB64Fresh-compiled QB64pe
   - `tests/bootstrap_tests.rs` - compilation & regression tests
   - `scripts/test-bootstrap.sh` - helper script
2. [x] Document the achievement
   - `docs/BOOTSTRAP_ACHIEVEMENT.md` - technical summary
3. [x] Document behavioral differences
   - `docs/BEHAVIORAL_DIFFERENCES.md` - QB64Fresh vs QB64pe semantics
4. [x] Write migration/compatibility notes
   - Updated `docs/MIGRATION_GUIDE.md` with bootstrap validation
5. [x] Update project README
   - Added bootstrap section and metrics

### Runtime Stubs → Real Implementations

The current executable uses stub functions. For full functionality, these need real implementations:

**Critical for QB64pe to actually work:**
- ✅ `qb_console()` - Returns console handle (fixed)
- ✅ `qb_dir_exists()` - Check if directory exists (implemented)
- ✅ `qb_fullpath()` - Get full path of file (implemented)
- ✅ **Array scoping bug** - FIXED! Arrays in main now use globals for cross-function sharing
  - `menu$`, `menuDesc$`, etc. now allocate to global (not shadowing local)
  - Executable runs past menu initialization without crashing
- Graphics initialization (QB64pe expects graphical mode)

**Nice to have (warnings only):**
- Clean up `char*` vs `qb_string*` type warnings (~100+ occurrences)
- Implement remaining `_KEY_*` constants

---

## Success Criteria

### Milestone 5: Validation Success (PENDING)
- [ ] QB64Fresh-compiled QB64pe passes subset of QB64pe's test suite
- [ ] Behavior matches original QB64pe for core functionality
- [ ] Process is documented and reproducible

---

## Notes

- The QB64pe IDE component requires graphics support (SDL2)
- Compiler-only mode (`-c`) still initializes the IDE, doesn't help avoid crashes
- Real runtime implementations needed for QB64pe to compile BASIC programs

### Array Scoping Issue - FIXED (2026-01-23)

The issue was array scoping. In QB64, when you use `menu$(m, i)` in main without explicit
DIM, it creates a module-level array accessible to called subroutines.

**Before fix:**
```c
// Global (line 2934)
qb_string** menu_str = NULL;

// Local in main - SHADOWED global
qb_string** menu_str = malloc(sizeof(qb_string*) * (12) * (21));

// In subroutine - uses GLOBAL (NULL!) → CRASH
menu_str[...] = qb_string_new("File");
```

**After fix:**
```c
// Global (line 2934)
qb_string** menu_str = NULL;

// In main - allocates to GLOBAL (no redeclaration)
menu_str = malloc(sizeof(qb_string*) * (12) * (21));

// In subroutine - uses GLOBAL (allocated!) → WORKS
menu_str[...] = qb_string_new("File");
```

**Fix implemented in:**
- `implicit_vars.rs`: Added `is_main_program` parameter to distinguish main from SUB/FUNCTION
- `stmt.rs`: `emit_dim` checks `current_proc.is_none()` for main context
- Key insight: DIM in main uses globals (for cross-function sharing), DIM in SUB/FUNCTION
  creates locals (even if a global with the same name exists)
