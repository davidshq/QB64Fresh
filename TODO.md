# QB64Fresh TODO

*Last updated: 2026-01-26*

A prioritized roadmap for QB64Fresh development. For completed features, see [TODO-completed.md](docs/archive/TODO-completed.md).

---
- All Phase 1-5, 7 items have been completed — see [TODO-completed.md](docs/archive/TODO-completed.md).
- Phase 5: All C Library Integration items completed — see [TODO-completed.md](docs/archive/TODO-completed.md).
- Phase 5: Joystick/gamepad full in external runtime — see [TODO-completed.md](docs/archive/TODO-completed.md).
- Phase 7: All metacommands have been implemented — see [TODO-completed.md](docs/archive/TODO-completed.md).
- Runtime header: `qb_chdir`, `qb_mkdir`, `qb_rmdir`, `qb_dir_exists`, and `qb_net_*` are in `qb64fresh_rt.h`.

---

## Phase 6: Tooling & Ecosystem

### Debugging (`tools/debug`) ✅ Complete
Debugger fully implemented with 50+ tests passing. See [ADR-0013](docs/adrs/ADR-0013-debugger-architecture.md) for architecture details.

**Completed:**
- [x] Debug infrastructure (symbols, values, frames, dap, sources, watch)
- [x] Runtime state capture (debug info in generated C via `--debug` flag)
- [x] Live breakpoint execution (runtime hooks with `qb_dbg_line()`)
- [x] Variable value reading (DAP server pipe communication)
- [x] Step execution (step into/over/out via `qb_dbg_enter_proc()`/`qb_dbg_exit_proc()`)
- [x] DAP server for VS Code/Cursor integration
- [x] Named pipe IPC for debugger ↔ debugee communication

### Optimization
- [ ] Dead code elimination *(Medium - 2-3 sessions)*
- [ ] Loop optimization *(Medium - 2-3 sessions)*
- [ ] Inline small functions *(Medium - 2-3 sessions)*
---

## Phase 8: VSCode Extension Enhancements

These can be worked on independently of the compiler/runtime:

- [x] Debugger support (DAP) - *Runtime integration complete (Phase 6), VSCode extension needs `launch.json` configuration*
