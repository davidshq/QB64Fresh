# Session 109 — libqb-common.h Platform and Types

**Date:** 2026-01-31

## Summary

Implemented libqb-common.h–style platform and type macros/constants in QB64Fresh runtime header so generated or external code can use the same names as QB64pe.

## Changes

- **runtime/include/qb64fresh_rt.h**
  - Added a “Platform and Types” block at the top (after includes, before `extern "C"`):
    - **OS:** `QB64_WINDOWS`, `QB64_LINUX`, `QB64_MACOSX`, `QB64_UNIX`
    - **FS:** `QB64_BACKSLASH_FILESYSTEM`
    - **Compiler:** `QB64_MICROSOFT`, `QB64_GCC`, `QB64_MINGW`
    - **Arch:** `QB64_32`, `QB64_64`, `QB64_NOT_X86`, `QB64_ARM`
    - **Constants:** `QB_FALSE` (0), `QB_TRUE` (-1)
    - **Helper:** `_countof(Array_)` (only if not already defined)
  - Logic mirrors QB64pe `internal/c/libqb/include/libqb-common.h`; C-only `_countof` macro (no C++ template).

- **docs/QB64pe/LIBQB_FUNCTIONALITY.md**
  - Section 39 (libqb-common.h) updated from 🟡 to 🟢 with short note on location and C-only `_countof`.

## Decisions

- Define macros in the main runtime header so any C/C++ that includes `qb64fresh_rt.h` gets libqb-compatible names without a separate header.
- Use `#ifndef` for `QB_FALSE`, `QB_TRUE`, and `_countof` to avoid redefinition if another header provides them.
