# Session 123: MIDI playback (soundfont + MIDI→PCM)

**Date:** 2026-01-31

## Goal

When playing MIDI (e.g. `_SNDPLAYFILE` with a .mid file or PLAY with MB), load the stored soundfont and synthesize MIDI to PCM. QB64pe uses TinySoundFont (tsf) + foo_midi + libmidi; for Rust we use **rustysynth** (SoundFont + `MidiFileSequencer`).

## Decisions

- **Library:** rustysynth (pure Rust, MIT, MidiFileSequencer + SoundFont + Synthesizer). xsynth is LGPL and aimed at Black MIDI.
- **Integration:** Optional feature `audio-midi`; when enabled, `snd_open` / `snd_playfile` detect `.mid` and render via soundfont to PCM, then play as normal stereo PCM (same handle/play API).
- **Soundfont:** Required for MIDI. Path set by `_MIDISOUNDBANK`; if not set and user opens a .mid file, open fails with a clear error.
- **PCM storage:** SoundHandle extended to support either encoded bytes (WAV/OGG/etc.) or pre-rendered PCM (for MIDI) so rodio gets a single unified path.

## Implementation

- Add `rustysynth` under feature `audio-midi`.
- Add `midi` module in runtime (load soundfont, load MIDI, render to float stereo PCM).
- In rodio_backend: `SoundSource::Encoded(Vec<u8>)` | `SoundSource::Pcm { samples, sample_rate }`; when opening a file, if extension is .mid and soundfont path set, render MIDI to PCM and store as Pcm; otherwise decode as before.
- Implement rodio `Source` for PCM buffer (stereo interleaved f32) for playback.

## Status

Done. MIDI playback implemented: `runtime/src/audio/midi.rs` (rustysynth render), rodio_backend extended with `SoundSource::Pcm` and `PcmBufferSource`; `snd_open` / `snd_playfile` detect `.mid` and synthesize when `_MIDISOUNDBANK` is set. Default feature `audio-midi` enables rustysynth.
