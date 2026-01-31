# Session 116 — image.h inline helpers and mem.h lock API (libqb)

**Date:** 2026-01-31

## Goal

Implement LIBQB_FUNCTIONALITY.md sections 20 and 23:

- **20. image.h** — Add inline helpers: `image_get_bgra_*`, `image_set_bgra_alpha`, `image_make_bgra`, `image_scale_*`, `image_swap_red_blue`, `image_clamp_color_component`, `image_calculate_rgb_distance`, `image_get_color_delta`.
- **23. mem.h** — Add structures `mem_block`, `mem_lock`; constants `MEM_TYPE_*`, `INVALID_MEM_LOCK`; lock lifecycle `new_mem_lock`, `free_mem_lock`; lock globals `mem_lock_id`, `mem_lock_tmp`, `mem_lock_base`.

## Approach

- **Image helpers:** Pure C static inline in `runtime/include/qb64fresh_rt.h` (external runtime) and in codegen inline runtime `src/codegen/c_backend/runtime/graphics.rs`. Use `qb_image_*` prefix for consistency.
- **Mem lock:** Add libqb-compatible types and declarations to the header; implement lock pool and `new_mem_lock`/`free_mem_lock` in Rust (`runtime/src/memory.rs` or new module) and export globals.

## Status

- [x] Image inline helpers in qb64fresh_rt.h
- [x] Image inline helpers in codegen graphics.rs
- [x] mem_block, mem_lock, MEM_TYPE_*, INVALID_MEM_LOCK in header
- [x] new_mem_lock, free_mem_lock, globals in header + Rust implementation
- [x] Update LIBQB_FUNCTIONALITY.md and LIBQB_FUNCTIONALITY_COMPLETED.md

## Summary

- **image.h:** Added `qb_image_get_bgra_*`, `qb_image_set_bgra_alpha`, `qb_image_make_bgra`, `qb_image_scale_*`, `qb_image_swap_red_blue`, `qb_image_clamp_color_component`, `qb_image_calculate_rgb_distance`, `qb_image_get_color_delta` to `runtime/include/qb64fresh_rt.h` (static inline) and to codegen inline runtime in `src/codegen/c_backend/runtime/graphics.rs`.
- **mem.h:** Added `mem_block`, `mem_lock`, `MEM_TYPE_*`, `INVALID_MEM_LOCK`, `new_mem_lock`, `free_mem_lock`, and globals `mem_lock_id`, `mem_lock_tmp`, `mem_lock_base` to the header; implemented lock pool and FFI in `runtime/src/mem_lock.rs`.
