# Session 108 — _WAVE, _SNDNEW, _MIDISOUNDBANK (audio.h)

**Date:** 2026-01-31

## Summary

Implemented the three audio.h items from LIBQB_FUNCTIONALITY.md §29:

- **sub__wave** → `qb_wave()` — _WAVE (waveform constant / wave output device)
- **func__sndnew** → `qb_sndnew(...)` — _SNDNEW (create new sound buffer)
- **sub__midisoundbank** → `qb_midisoundbank(...)` — _MIDISOUNDBANK (set MIDI sound bank path)

## Changes

1. **Inline runtime** (`src/codegen/c_backend/runtime/audio.rs`)
   - Added `qb_wave(void)` returning 0 (default device/constant).
   - Added `qb_sndnew(frames, channels, bits)` returning -1 (stub; warns and returns invalid handle).
   - Added `qb_midisoundbank(qb_string* filename)` no-op stub with audio warn.
   - Updated module doc to list _WAVE, _SNDNEW, _MIDISOUNDBANK.

2. **C header** (`runtime/include/qb64fresh_rt.h`)
   - Declared `int32_t qb_wave(void);`
   - Declared `int32_t qb_sndnew(int32_t frames, int32_t channels, int32_t bits);`
   - Declared `void qb_midisoundbank(const QbString* filename);`

3. **External runtime** (`runtime/src/audio_ffi.rs`)
   - `qb_wave()` → returns 0.
   - `qb_sndnew(frames, channels, bits)` → returns -1 (stub).
   - `qb_midisoundbank(filename)` → no-op (stub); accepts `*const QbString`.

4. **Docs** (`docs/QB64pe/LIBQB_FUNCTIONALITY.md`)
   - §29 table: all three entries set to 🟢 with short notes (function form / stubs).

## Verification

- Integration tests: `sndnew_function`, `midisoundbank_statement`, `wave_function` all pass (generated C contains the expected calls).
- Compiler and codegen already mapped _WAVE → qb_wave, _SNDNEW → qb_sndnew, _MIDISOUNDBANK → qb_midisoundbank; no codegen changes required.

## Notes

- _WAVE SUB form (voice, waveDefinition, frameCount) is not implemented; only the function form returning the constant/device is.
- _SNDNEW and _MIDISOUNDBANK are stubs: they allow programs to compile and link; full behavior would require backend support (sound buffer creation, MIDI soundfont loading).
