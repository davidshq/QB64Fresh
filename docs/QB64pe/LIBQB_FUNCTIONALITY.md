# libqb Functionality Reference

## Used by QB64pe itself

**Remaining gaps (used by QB64pe; work toward full parity):**

| Section | Used by QB64pe | QB64Fresh gap |
|---------|----------------|---------------|
| **21. keyhandler** | Key state via inkey/keyhit; `keyheld`-style use in IDE | keydown_vk / keyup_vk stub 🟢 (no-op; BASIC _KEYUP/_KEYDOWN statement → qb_keyup_vk/qb_keydown_vk) |
| **Legacy events** | ON COM, ON PEN, ON SIGNAL (parser/codegen + runtime) | 🟢 Stub in both inline and external runtime (qb_on_com, qb_on_pen, qb_com_control, qb_pen_control, qb_on_signal, qb_signal_control); warn once, no-op. |
| **41. Parts** | audio, core, data, video (see §41 below) | audio 🟢 (rodio + MIDI rustysynth); core 🔴 (SDL2, no raw OpenGL); data 🟢; video 🟢 |

---

## 41. Parts (Subsystems) — Remaining gaps

| Part | Role | QB64Fresh |
|------|------|-----------|
| **audio** | miniaudio, MIDI/mod/OPL/etc.; implements audio.h API | 🟢 (rodio; MIDI bank path ✅; MIDI playback ✅ rustysynth) |
| **core** | OpenGL/GLUT (freeglut, GLEW); gl_helper_code.h for GL wrappers | 🔴 (SDL2, no raw OpenGL). Optional design: [OPENGL_GLUT_DESIGN.md](../ThingsToDo/OPENGL_GLUT_DESIGN.md) |
| **data** | compression (miniz), encoding (modp_b64) used by libqb compression/encoding | 🟢 Encoding §7, compression §33 in runtime (external rt; inline stubs for deflate/inflate) |
| **video** | font (FreeType), image (stb, jo_gif, nanosvg, qoi, etc.); implements font.h and image.h | 🟢 font.h §37, image.h §20 in runtime (SDL2/FreeType, load/save image) |

**MIDI bank:** (1) **Store path** ✅ — `qb_midisoundbank(filename)` now persists the soundfont path; `AudioBackend::set_midi_sound_bank_path(Option<&str>)` is implemented for Rodio and Mock. (2) **MIDI playback** ✅ — when playing MIDI (e.g. `_SNDPLAYFILE` with a .mid file or PLAY with MB), load the stored soundfont and synthesize MIDI to PCM via **rustysynth** (SoundFont + `MidiFileSequencer`); PCM is played through rodio. Feature `audio-midi` (default on); if no soundfont is set, opening a .mid file returns handle -7. (3) **AudioBackend** — trait has “set MIDI sound bank path” ✅; "play MIDI file using current bank" ✅ (rodio + rustysynth render MIDI to PCM on open/play).