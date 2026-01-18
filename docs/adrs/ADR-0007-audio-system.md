# ADR-0007: Audio System Architecture

## Status

**Accepted** - January 18, 2026

## Context

QB64Fresh needs an audio system supporting:
- Classic QBasic sounds (BEEP, SOUND, PLAY with MML)
- QB64 extended audio (_SNDOPEN, _SNDPLAY, _SNDLOOP, etc.)
- Multiple simultaneous sounds
- Volume and balance control
- Sound file formats (WAV, MP3, OGG)
- Raw audio synthesis (_SNDRAW, _SNDOPENRAW)

Key considerations:
- Must support headless/CI testing without audio hardware
- Should follow same pluggable pattern as graphics
- Need cross-platform compatibility
- QB64pe uses OpenAL + custom decoders - complex but proven
- Modern alternatives exist (rodio, SDL2_mixer)

## Decision

**We chose a trait-based pluggable backend architecture mirroring the graphics system, with rodio as the planned primary implementation**.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Generated C Code                                            │
│ BEEP, SOUND 440, 18, PLAY "CDEFGAB", _SNDPLAY(h)           │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ C FFI Layer (qb_* functions)                                │
│ qb_beep(), qb_sound(), qb_play(), qb_sndopen(), etc.        │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ AudioBackend Trait (Rust)                                   │
│ beep(), sound(), play(), snd_open(), snd_play(), etc.       │
└────────────────────┬────────────────────────────────────────┘
                     │
          ┌──────────┼──────────────────────┐
          ▼          ▼                       ▼
    ┌──────────┐ ┌──────────┐        ┌─────────────┐
    │ Rodio    │ │ Mock     │        │ Future:     │
    │ Backend  │ │ Backend  │        │ - SDL2_mixer│
    │          │ │ (testing)│        │ - Web Audio │
    └──────────┘ └──────────┘        └─────────────┘
```

### AudioBackend Trait

```rust
pub trait AudioBackend: Send {
    // Classic BASIC sound
    fn beep(&mut self) -> Result<(), AudioError>;
    fn sound(&mut self, frequency: f64, duration: f64) -> Result<(), AudioError>;
    fn play(&mut self, mml_commands: &str) -> Result<(), AudioError>;

    // QB64 sound file operations
    fn snd_open(&mut self, filename: &str) -> i32;
    fn snd_close(&mut self, handle: i32) -> Result<(), AudioError>;
    fn snd_play(&mut self, handle: i32) -> Result<(), AudioError>;
    fn snd_stop(&mut self, handle: i32) -> Result<(), AudioError>;
    fn snd_pause(&mut self, handle: i32) -> Result<(), AudioError>;
    fn snd_resume(&mut self, handle: i32) -> Result<(), AudioError>;
    fn snd_loop(&mut self, handle: i32) -> Result<(), AudioError>;

    // Volume and position control
    fn snd_vol(&mut self, handle: i32, volume: f64) -> Result<(), AudioError>;
    fn snd_bal(&mut self, handle: i32, balance: f64) -> Result<(), AudioError>;
    fn snd_len(&self, handle: i32) -> f64;
    fn snd_getpos(&self, handle: i32) -> f64;
    fn snd_setpos(&mut self, handle: i32, position: f64) -> Result<(), AudioError>;

    // Status queries
    fn snd_playing(&self, handle: i32) -> bool;
    fn snd_paused(&self, handle: i32) -> bool;
    fn snd_rate(&self) -> i32;

    // Raw audio synthesis
    fn snd_openraw(&mut self) -> i32;
    fn snd_raw(&mut self, sample: f64) -> Result<(), AudioError>;
    fn snd_raw_stereo(&mut self, left: f64, right: f64) -> Result<(), AudioError>;
    fn snd_rawlen(&self) -> f64;
}
```

### Sound Handle System

- Handles are positive integers starting at 1
- Handle 0 is reserved for the screen/default output
- Negative values indicate errors
- Handles track loaded sounds for playback control

### MML (Music Macro Language) Support

The PLAY command interprets MML strings:
- Notes: `CDEFGAB` with sharps `+`/`#` and flats `-`
- Octaves: `O0-O6`, `<` (down), `>` (up)
- Duration: `L1-L64` (whole to 64th note)
- Tempo: `T32-T255` (beats per minute)
- Pause: `P1-P64` (rest)
- Volume: `V0-V15`

### Rationale

1. **Trait-based like graphics**: Consistent architecture, testing support
2. **rodio over SDL2_mixer**: Pure Rust, simpler integration, good codec support
3. **Handle-based API**: Matches QB64 semantics, enables multiple sounds
4. **Auto-initialization**: Audio starts on first use, no explicit init required
5. **Mock backend**: Enables CI testing, records operations for verification

### Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| **SDL2_mixer** | Another C dependency, less Rustic |
| **OpenAL** | Complex, overkill for BASIC programs |
| **cpal directly** | Too low-level, need decoders anyway |
| **Hardcoded implementation** | No testing flexibility |

## Consequences

### Positive

- Headless testing via mock backend
- Consistent with graphics architecture
- Cross-platform via rodio
- Multiple simultaneous sounds supported
- MML support for classic PLAY command
- Raw audio synthesis for advanced users

### Negative

- MML parser needed (non-trivial)
- Handle management adds complexity
- Global mutable state (required for C FFI)
- Some QB64 audio features very complex (3D positional audio)

### Implementation Status

| Component | Status |
|-----------|--------|
| AudioBackend trait | Complete |
| Mock backend | Complete |
| Rodio backend | Stubbed |
| C FFI layer | Complete |
| Code generation | Partial |
| BEEP/SOUND | Ready |
| PLAY (MML) | Ready (needs MML parser) |
| _SND* functions | Ready |
| Raw audio | Ready |

### Files

- `runtime/src/audio/mod.rs` - Core trait, initialization
- `runtime/src/audio/error.rs` - Error types
- `runtime/src/audio/mock.rs` - Mock backend for testing
- `runtime/src/audio_ffi.rs` - C FFI functions (~30 functions)
