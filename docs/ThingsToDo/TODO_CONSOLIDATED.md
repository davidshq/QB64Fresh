# QB64Fresh Consolidated TODO

**Last Updated:** 2026-01-28 (Session 079)

---

## Medium-term Priorities

### 3. Stream Code Generation ⚠️ **MEMORY OPTIMIZATION**

**Issue:** Codegen accumulates entire C output in `String` (114K lines for QB64pe uses significant memory)  
**Fix:** Use `Write` trait, stream directly to file  
**Effort:** 1 week  
**Priority:** Medium

---

### 4. Runtime Mode Abstraction

**Issue:** Inline vs External modes handled with if/else  
**Consider:** Trait-based abstraction (or document current approach if YAGNI)  
**Effort:** 1-2 weeks  
**Priority:** Low-Medium

---

## Long-term Priorities

### 6. Phase 6: Optimization

**Remaining:**
- [ ] Dead code elimination (2-3 sessions)
- [ ] Loop optimization (2-3 sessions)
- [ ] Inline small functions (2-3 sessions)

---

### 7. Distribution and Installer

**Phases:**
1. **Minimal:** Build binaries (Windows x64, macOS x64+arm64, Linux x64), publish on GitHub Releases
2. **Runtime:** Ship `libqb64fresh_rt.a`/`.lib` and header, document `--runtime external`
3. **Installers:** Windows (NSIS/Inno), macOS (.pkg/.dmg), Linux (.deb/.rpm)
4. **Polish:** Optional tools (`qb64fresh-fmt`, `qb64fresh-lint`), `qb64fresh doctor`, code signing

---

## Decision Matrix

| Priority | Task | Impact | Effort | Recommended? |
|----------|------|--------|--------|--------------|
| High | Full bootstrap execution testing | High | 1-2 weeks | ✅ **YES** |
| Medium-term | Stream codegen | Medium | 1 week | ⚠️ If memory issue |
| Low-medium | Runtime mode abstraction | Low | 1-2 weeks | ⚠️ If needed |

---

## Related Documentation

**Planning:** [ARCHITECTURAL_REVIEW.md](docs/ARCHITECTURAL_REVIEW.md), [TODO-completed.md](docs/archive/TODO-completed.md)  
**Status:** [PARTIAL_IMPLEMENTATIONS.md](PARTIAL_IMPLEMENTATIONS.md), [BOOTSTRAP_VALIDATION.md](../archive/BOOTSTRAP_VALIDATION.md)

*This file consolidates content from TODO.md, TODO_ITEMS.md, and various planning documents.*
