# OpenGL Support (Optional)

QB64Fresh supports QB64pe-style OpenGL (`_GL*` and `_GLU*`) as an **optional** feature. Programs that do not use OpenGL have no OpenGL dependency or code size cost.

## Enabling OpenGL

**In your BASIC program:** Use `SUB _GL` and any `_GL*` / `_GLU*` commands. The compiler detects OpenGL usage and enables the OpenGL layer automatically.

**Override flags:**
- `--opengl` — Enable OpenGL built-ins and codegen even if the source does not use `_GL*` (e.g. for libraries).
- `--no-opengl` — Disable OpenGL even if the source uses `_GL*` (e.g. to force compilation without GL).

**Runtime:** The runtime library must be built with the `opengl` feature and linked against system OpenGL (e.g. `libGL`, `libGLU`). When you build the runtime with `opengl`, `_GLCOMPAT` returns 1 and `_GLRENDER` is available; otherwise they are stubs.

## Rules (QB64pe parity)

- **`_GL*` / `_GLU*` only inside `SUB _GL`:** Using an OpenGL command outside `SUB _GL` is a semantic error (e.g. error 270).
- **`_GLRENDER`:** Controls whether OpenGL draws behind, on top of, or instead of the 2D layer (`_BEHIND`, `_ONTOP`, `_ONLY`). When the runtime is built with `opengl` and `graphics-sdl2`, calling `_GLRENDER` with a non-off mode creates an OpenGL context (lazily) and the display loop invokes `SUB _GL` each frame with `sub_gl_called` set. Currently when _GLRENDER is active the frame is GL-only; 2D+GL composition (BEHIND/ONTOP) is planned.
- **`_GLCOMPAT`:** Returns whether OpenGL is available (1 when runtime is built with `opengl`, 0 otherwise).

## Building with OpenGL

**Compiler:** No extra step; OpenGL is enabled by use or by `--opengl`.

**Runtime (for linking generated C):** Build with both `graphics-sdl2` and `opengl` so the display loop can create a GL context and call `SUB _GL` each frame:

```bash
cd runtime
cargo build --release --features "graphics-sdl2,opengl"
```

Then compile your generated C with `-DQB64FRESH_OPENGL` and link against the runtime and OpenGL (e.g. `-lqb64fresh_rt -lGL -lGLU` and system libs as in the main docs). The generated C registers `qb_sub__gl` with the runtime at startup so `_DISPLAY` / the main loop invokes it when `_GLRENDER` is active.

## Design and scope

OpenGL is implemented as a modular, optional layer. See [ThingsToDo/OPENGL_GLUT_DESIGN.md](ThingsToDo/OPENGL_GLUT_DESIGN.md) for architecture, phases, and file layout. Only the OpenGL/GLU surface that QB64pe implements is supported—no extra APIs or newer GL versions.
