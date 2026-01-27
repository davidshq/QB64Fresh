//! Graphics stubs for the inline runtime.
//!
//! This module contains the `emit_graphics_stubs` function which generates C code
//! for graphics operations when using the inline runtime mode. These stubs allow
//! programs that use graphics commands to compile without requiring the external
//! runtime library, though actual graphics functionality is not available.
//!
//! For full graphics support, use `--runtime external` and link with `libqb64fresh_rt`.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits graphics operation stubs for the inline runtime.
///
/// These stubs allow programs that use graphics commands to compile even when
/// using the inline runtime. They print a warning message on first use and
/// return safe default values.
pub(super) fn emit_graphics_stubs(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Graphics Stubs (Inline Runtime) */")?;
    writeln_code!(
        output,
        "/* For full graphics support, use --runtime external and link with libqb64fresh_rt */"
    )?;
    writeln_code!(output)?;

    // Warning flag and frame counter for preventing infinite loops
    writeln_code!(output, "static int _qb_gfx_warned = 0;")?;
    writeln_code!(output, "static int _qb_gfx_frame_count = 0;")?;
    writeln_code!(
        output,
        "static int _qb_gfx_max_frames = 1000; /* Prevent infinite loops in stub mode */"
    )?;
    writeln_code!(output)?;
    writeln_code!(output, "static void _qb_gfx_warn(void) {{")?;
    writeln_code!(output, "    if (!_qb_gfx_warned) {{")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"Warning: Graphics functions require external runtime. Use --runtime external\\n\");"
    )?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"         Programs with game loops will exit after %d frames in stub mode.\\n\", _qb_gfx_max_frames);"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "        _qb_gfx_warned = 1;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Environment variable to control max frames (for testing)
    writeln_code!(output, "static void _qb_gfx_init_max_frames(void) {{")?;
    writeln_code!(output, "    static int initialized = 0;")?;
    writeln_code!(output, "    if (!initialized) {{")?;
    writeln_code!(
        output,
        "        const char* env = getenv(\"QB64FRESH_MAX_FRAMES\");"
    )?;
    writeln_code!(output, "        if (env) _qb_gfx_max_frames = atoi(env);")?;
    writeln_code!(
        output,
        "        if (_qb_gfx_max_frames <= 0) _qb_gfx_max_frames = 1000;"
    )?;
    writeln_code!(output, "        initialized = 1;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Initialization - SCREEN statement
    writeln_code!(output, "int qb_gfx_init(int32_t mode) {{")?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(output, "    _qb_gfx_init_max_frames();")?;
    writeln_code!(
        output,
        "    _qb_gfx_frame_count = 0; /* Reset frame counter on new SCREEN */"
    )?;
    writeln_code!(output, "    (void)mode;")?;
    writeln_code!(
        output,
        "    return 0; /* Success - stub mode accepts any screen mode */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "int qb_gfx_shutdown(void) {{ return 0; }}")?;
    writeln_code!(output)?;

    // Basic graphics operations
    // CLS increments frame counter to help with termination in stub mode
    writeln_code!(
        output,
        "int qb_gfx_cls(void) {{ _qb_gfx_frame_count++; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_color(uint32_t fg, uint32_t bg) {{ (void)fg; (void)bg; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_locate(int32_t row, int32_t col) {{ (void)row; (void)col; return 0; }}"
    )?;
    writeln_code!(output)?;

    // Drawing
    writeln_code!(
        output,
        "int qb_gfx_pset(int32_t x, int32_t y, uint32_t color) {{ (void)x; (void)y; (void)color; return 0; }}"
    )?;
    writeln_code!(
        output,
        "uint32_t qb_gfx_point(int32_t x, int32_t y) {{ (void)x; (void)y; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_line(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color) {{ (void)x1; (void)y1; (void)x2; (void)y2; (void)color; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_line_step(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int step1, int step2, uint16_t style) {{ (void)x1; (void)y1; (void)x2; (void)y2; (void)color; (void)step1; (void)step2; (void)style; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_box(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int filled) {{ (void)x1; (void)y1; (void)x2; (void)y2; (void)color; (void)filled; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_box_step(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int filled, int step1, int step2, uint16_t style) {{ (void)x1; (void)y1; (void)x2; (void)y2; (void)color; (void)filled; (void)step1; (void)step2; (void)style; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_circle(int32_t x, int32_t y, int32_t radius, uint32_t color, int filled) {{ (void)x; (void)y; (void)radius; (void)color; (void)filled; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_paint(int32_t x, int32_t y, uint32_t color, uint32_t boundary) {{ (void)x; (void)y; (void)color; (void)boundary; return 0; }}"
    )?;
    writeln_code!(output)?;

    // Display
    // Note: Frame counter is checked here to prevent infinite loops even if
    // programs only call DISPLAY without checking _SCREENEXISTS or _POLLEVENTS
    writeln_code!(output, "int qb_gfx_display(void) {{")?;
    writeln_code!(output, "    _qb_gfx_init_max_frames();")?;
    writeln_code!(output, "    _qb_gfx_frame_count++;")?;
    writeln_code!(
        output,
        "    if (_qb_gfx_frame_count >= _qb_gfx_max_frames) {{"
    )?;
    writeln_code!(
        output,
        "        if (_qb_gfx_frame_count == _qb_gfx_max_frames) {{"
    )?;
    writeln_code!(
        output,
        "            fprintf(stderr, \"Note: Stub graphics reached %d frames in DISPLAY, exiting to prevent infinite loop.\\n\", _qb_gfx_max_frames);"
    )?;
    writeln_code!(
        output,
        "            fprintf(stderr, \"      Set QB64FRESH_MAX_FRAMES environment variable to change limit.\\n\");"
    )?;
    writeln_code!(output, "            fflush(stderr);")?;
    writeln_code!(
        output,
        "            _qb_gfx_frame_count++; /* Only print once */"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(
        output,
        "        exit(0); /* Exit to prevent infinite loop */"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // poll_events returns 1 (window open) until max frames reached, then 0 (window closed)
    // This prevents infinite loops in game loops when using stub mode
    writeln_code!(output, "int qb_gfx_poll_events(void) {{")?;
    writeln_code!(output, "    _qb_gfx_init_max_frames();")?;
    writeln_code!(
        output,
        "    if (_qb_gfx_frame_count >= _qb_gfx_max_frames) {{"
    )?;
    writeln_code!(
        output,
        "        if (_qb_gfx_frame_count == _qb_gfx_max_frames) {{"
    )?;
    writeln_code!(
        output,
        "            fprintf(stderr, \"Note: Stub graphics reached %d frames, signaling window close.\\n\", _qb_gfx_max_frames);"
    )?;
    writeln_code!(
        output,
        "            fprintf(stderr, \"      Set QB64FRESH_MAX_FRAMES environment variable to change limit.\\n\");"
    )?;
    writeln_code!(output, "            fflush(stderr);")?;
    writeln_code!(
        output,
        "            _qb_gfx_frame_count++; /* Only print once */"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        return 0; /* Signal window closed */")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return 1; /* Window still open */")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "uint32_t qb_gfx_width(void) {{ return 80; }}")?;
    writeln_code!(output, "uint32_t qb_gfx_height(void) {{ return 25; }}")?;
    writeln_code!(output)?;

    // Palette and page copy
    writeln_code!(
        output,
        "int qb_gfx_palette(int32_t attr, uint32_t color) {{ (void)attr; (void)color; return 0; }}"
    )?;
    writeln_code!(output, "int qb_gfx_palette_reset(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "int qb_gfx_pcopy(int32_t src, int32_t dst) {{ (void)src; (void)dst; return 0; }}"
    )?;
    // Page control for double buffering
    writeln_code!(
        output,
        "int qb_gfx_set_active_page(int32_t page) {{ (void)page; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_set_visual_page(int32_t page) {{ (void)page; return 0; }}"
    )?;
    writeln_code!(
        output,
        "void qb_gfx_get_pages(int32_t* active, int32_t* visual) {{ if (active) *active = 0; if (visual) *visual = 0; }}"
    )?;
    // PMAP: coordinate mapping (stub returns coordinate unchanged)
    writeln_code!(
        output,
        "double qb_gfx_pmap(double coord, int32_t func_code) {{ (void)func_code; return coord; }}"
    )?;
    // Note: POINT function is already defined above as qb_gfx_point(x, y) -> uint32_t
    writeln_code!(output)?;

    // Extended graphics
    writeln_code!(
        output,
        "int qb_gfx_set_width(uint32_t cols, uint32_t rows) {{ (void)cols; (void)rows; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_view(int screen, int32_t x1, int32_t y1, int32_t x2, int32_t y2, int32_t fill, int32_t border) {{ (void)screen; (void)x1; (void)y1; (void)x2; (void)y2; (void)fill; (void)border; return 0; }}"
    )?;
    writeln_code!(output, "int qb_gfx_view_reset(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "int qb_gfx_window(int screen, double x1, double y1, double x2, double y2) {{ (void)screen; (void)x1; (void)y1; (void)x2; (void)y2; return 0; }}"
    )?;
    writeln_code!(output, "int qb_gfx_window_reset(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "int qb_gfx_draw(const char* cmd) {{ (void)cmd; return 0; }}"
    )?;
    writeln_code!(output)?;

    // Image operations
    writeln_code!(
        output,
        "int32_t qb_gfx_newimage(int32_t w, int32_t h, int32_t mode) {{ (void)w; (void)h; (void)mode; return -1; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_gfx_loadimage(const char* fn, int32_t mode) {{ (void)fn; (void)mode; return -1; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_gfx_copyimage(int32_t src, int32_t mode) {{ (void)src; (void)mode; return -1; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_freeimage(int32_t h) {{ (void)h; return 0; }}"
    )?;
    // _PUTIMAGE functions with scale_mode parameter:
    // scale_mode: 0 = default, 1 = smooth (bilinear), 2 = stretch (nearest-neighbor)
    writeln_code!(
        output,
        "int qb_gfx_putimage_simple(int32_t src, int32_t dst, int scale_mode) {{ (void)src; (void)dst; (void)scale_mode; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_putimage(int32_t dx1, int32_t dy1, int32_t dx2, int32_t dy2, int32_t src, int32_t dst, int scale_mode) {{ (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)src; (void)dst; (void)scale_mode; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_putimage_full(int32_t dx1, int32_t dy1, int32_t dx2, int32_t dy2, int32_t src, int32_t dst, int32_t sx1, int32_t sy1, int32_t sx2, int32_t sy2, int scale_mode) {{ (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)src; (void)dst; (void)sx1; (void)sy1; (void)sx2; (void)sy2; (void)scale_mode; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_source(int32_t h) {{ (void)h; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_dest(int32_t h) {{ (void)h; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_printstring(int32_t x, int32_t y, const char* text) {{ (void)x; (void)y; (void)text; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int qb_gfx_autodisplay(int enabled) {{ (void)enabled; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_gfx_image_width(int32_t h) {{ (void)h; return 0; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_gfx_image_height(int32_t h) {{ (void)h; return 0; }}"
    )?;
    writeln_code!(output)?;

    // Color creation functions (QB64)
    // _RGB32 creates 32-bit ARGB color - multiple variants for different arg counts
    writeln_code!(
        output,
        "uint32_t qb__rgb32(int32_t r, int32_t g, int32_t b) {{ return 0xFF000000u | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF); }}"
    )?;
    // 4-arg version: either (r,g,b,a) or (gray,gray,gray,alpha) - same implementation
    writeln_code!(
        output,
        "uint32_t qb__rgb32_4(int32_t r, int32_t g, int32_t b, int32_t a) {{ return ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF); }}"
    )?;
    writeln_code!(
        output,
        "uint32_t qb__rgba32(int32_t r, int32_t g, int32_t b, int32_t a) {{ return ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF); }}"
    )?;
    // _RGB/_RGBA for paletted modes (stub - returns index 0)
    writeln_code!(
        output,
        "uint32_t qb__rgb(int32_t r, int32_t g, int32_t b, int32_t mode) {{ (void)r; (void)g; (void)b; (void)mode; return 0; }}"
    )?;
    writeln_code!(
        output,
        "uint32_t qb__rgba(int32_t r, int32_t g, int32_t b, int32_t a, int32_t mode) {{ (void)r; (void)g; (void)b; (void)a; (void)mode; return 0; }}"
    )?;
    writeln_code!(output)?;

    // Color component extraction (QB64)
    // 32-bit mode extraction - works on ARGB format
    writeln_code!(
        output,
        "int32_t qb_red32(uint32_t c) {{ return (c >> 16) & 0xFF; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_green32(uint32_t c) {{ return (c >> 8) & 0xFF; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_blue32(uint32_t c) {{ return c & 0xFF; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_alpha32(uint32_t c) {{ return (c >> 24) & 0xFF; }}"
    )?;
    // Paletted mode extraction (stub - returns 0)
    writeln_code!(
        output,
        "int32_t qb_red(uint32_t c, int32_t mode) {{ (void)mode; return (c >> 16) & 0xFF; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_green(uint32_t c, int32_t mode) {{ (void)mode; return (c >> 8) & 0xFF; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_blue(uint32_t c, int32_t mode) {{ (void)mode; return c & 0xFF; }}"
    )?;
    writeln_code!(
        output,
        "int32_t qb_alpha(uint32_t c, int32_t mode) {{ (void)mode; return (c >> 24) & 0xFF; }}"
    )?;
    writeln_code!(output)?;

    // Legacy file open (for compatibility)
    // Signature matches generated code: (fnum, mode_char, filename_char)
    // mode_char is "I" (input), "O" (output), "A" (append), "B" (binary), "R" (random)
    writeln_code!(
        output,
        "void qb_file_open_legacy(int32_t fnum, const char* mode_char, const char* fname) {{"
    )?;
    writeln_code!(output, "    const char* fmode = \"r\";")?;
    writeln_code!(output, "    if (mode_char && mode_char[0]) {{")?;
    writeln_code!(output, "        switch(mode_char[0]) {{")?;
    writeln_code!(
        output,
        "            case 'I': case 'i': fmode = \"r\"; break;"
    )?;
    writeln_code!(
        output,
        "            case 'O': case 'o': fmode = \"w\"; break;"
    )?;
    writeln_code!(
        output,
        "            case 'A': case 'a': fmode = \"a\"; break;"
    )?;
    writeln_code!(
        output,
        "            case 'B': case 'b': fmode = \"r+b\"; break;"
    )?;
    writeln_code!(
        output,
        "            case 'R': case 'r': fmode = \"r+b\"; break;"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    FILE* f = fopen(fname, fmode);")?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    if (!f) {{")?;
    writeln_code!(output, "        char* n = _qb_normalize_path(fname);")?;
    writeln_code!(output, "        if (n) {{ f = fopen(n, fmode); free(n); }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(
        output,
        "    /* For binary/random mode, create file if it doesn't exist */"
    )?;
    writeln_code!(
        output,
        "    if (!f && mode_char && (mode_char[0] == 'B' || mode_char[0] == 'b' || mode_char[0] == 'R' || mode_char[0] == 'r')) {{"
    )?;
    writeln_code!(output, "        f = fopen(fname, \"w+b\");")?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "        if (!f) {{")?;
    writeln_code!(output, "            char* n = _qb_normalize_path(fname);")?;
    writeln_code!(
        output,
        "            if (n) {{ f = fopen(n, \"w+b\"); free(n); }}"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    _qb_file_set(fnum, f);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // VIEW PRINT - text viewport
    writeln_code!(output, "static int32_t _qb_view_print_top = 1;")?;
    writeln_code!(output, "static int32_t _qb_view_print_bottom = 25;")?;
    writeln_code!(
        output,
        "void qb_view_print(int32_t top, int32_t bottom) {{ _qb_view_print_top = top; _qb_view_print_bottom = bottom; }}"
    )?;
    writeln_code!(
        output,
        "void qb_view_print_reset(void) {{ _qb_view_print_top = 1; _qb_view_print_bottom = 25; }}"
    )?;
    writeln_code!(output)?;

    // GET/PUT graphics arrays - action constants
    writeln_code!(output, "#define QB_PUT_XOR 0")?;
    writeln_code!(output, "#define QB_PUT_PSET 1")?;
    writeln_code!(output, "#define QB_PUT_PRESET 2")?;
    writeln_code!(output, "#define QB_PUT_AND 3")?;
    writeln_code!(output, "#define QB_PUT_OR 4")?;
    writeln_code!(output)?;

    // GET - capture screen region to array
    // Array format: first 4 bytes = width (16-bit) + height (16-bit), rest = pixel data (32-bit ARGB)
    writeln_code!(
        output,
        "int qb_gfx_get(int32_t x1, int32_t y1, int32_t x2, int32_t y2, void* arr) {{"
    )?;
    writeln_code!(output, "    if (!arr) return 1;")?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(output, "    /* Normalize coordinates */")?;
    writeln_code!(output, "    int32_t left = x1 <= x2 ? x1 : x2;")?;
    writeln_code!(output, "    int32_t right = x1 > x2 ? x1 : x2;")?;
    writeln_code!(output, "    int32_t top = y1 <= y2 ? y1 : y2;")?;
    writeln_code!(output, "    int32_t bottom = y1 > y2 ? y1 : y2;")?;
    writeln_code!(output, "    uint16_t width = (uint16_t)(right - left + 1);")?;
    writeln_code!(
        output,
        "    uint16_t height = (uint16_t)(bottom - top + 1);"
    )?;
    writeln_code!(output, "    /* Write header */")?;
    writeln_code!(output, "    uint16_t* header = (uint16_t*)arr;")?;
    writeln_code!(output, "    header[0] = width;")?;
    writeln_code!(output, "    header[1] = height;")?;
    writeln_code!(
        output,
        "    /* In standalone mode, fill with zeros (no real backend) */"
    )?;
    writeln_code!(
        output,
        "    uint32_t* pixels = (uint32_t*)((uint8_t*)arr + 4);"
    )?;
    writeln_code!(
        output,
        "    for (int i = 0; i < width * height; i++) pixels[i] = 0;"
    )?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "int qb_gfx_get_step(int32_t x1, int32_t y1, int32_t w, int32_t h, void* arr) {{"
    )?;
    writeln_code!(output, "    /* STEP variant: w,h are relative offsets */")?;
    writeln_code!(
        output,
        "    return qb_gfx_get(x1, y1, x1 + w, y1 + h, arr);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // PUT - draw array contents to screen
    writeln_code!(
        output,
        "int qb_gfx_put(int32_t x, int32_t y, void* arr, int action, int clip, int32_t trans_color) {{"
    )?;
    writeln_code!(output, "    if (!arr) return 1;")?;
    writeln_code!(
        output,
        "    (void)x; (void)y; (void)action; (void)clip; (void)trans_color;"
    )?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(output, "    /* Read header for validation */")?;
    writeln_code!(output, "    uint16_t* header = (uint16_t*)arr;")?;
    writeln_code!(output, "    uint16_t width = header[0];")?;
    writeln_code!(output, "    uint16_t height = header[1];")?;
    writeln_code!(output, "    (void)width; (void)height;")?;
    writeln_code!(
        output,
        "    /* In standalone mode, no-op (no real backend) */"
    )?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "int qb_gfx_put_step(int32_t x, int32_t y, void* arr, int action, int clip, int32_t trans_color) {{"
    )?;
    writeln_code!(
        output,
        "    /* STEP variant: x,y are relative to last graphics position */"
    )?;
    writeln_code!(
        output,
        "    return qb_gfx_put(x, y, arr, action, clip, trans_color);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Color helpers
    writeln_code!(
        output,
        "uint32_t qb_rgb(uint32_t r, uint32_t g, uint32_t b) {{ return 0xFF000000 | ((r & 255) << 16) | ((g & 255) << 8) | (b & 255); }}"
    )?;
    writeln_code!(
        output,
        "uint32_t qb_rgba(uint32_t r, uint32_t g, uint32_t b, uint32_t a) {{ return ((a & 255) << 24) | ((r & 255) << 16) | ((g & 255) << 8) | (b & 255); }}"
    )?;
    writeln_code!(
        output,
        "uint32_t qb_rgb32(uint32_t r, uint32_t g, uint32_t b) {{ return qb_rgb(r, g, b); }}"
    )?;
    writeln_code!(
        output,
        "uint32_t qb_rgba32(uint32_t r, uint32_t g, uint32_t b, uint32_t a) {{ return qb_rgba(r, g, b, a); }}"
    )?;
    writeln_code!(output)?;

    // Mouse stubs
    writeln_code!(output, "int32_t qb_mouse_x(void) {{ return 0; }}")?;
    writeln_code!(output, "int32_t qb_mouse_y(void) {{ return 0; }}")?;
    writeln_code!(
        output,
        "int32_t qb_mouse_button(int32_t b) {{ (void)b; return 0; }}"
    )?;
    writeln_code!(output, "int32_t qb_mouse_input(void) {{ return 0; }}")?;
    writeln_code!(output, "int32_t qb_mouse_movement_x(void) {{ return 0; }}")?;
    writeln_code!(output, "int32_t qb_mouse_movement_y(void) {{ return 0; }}")?;
    writeln_code!(output, "int32_t qb_mouse_wheel(void) {{ return 0; }}")?;
    writeln_code!(output, "void qb_mouse_hide(void) {{ }}")?;
    writeln_code!(output, "void qb_mouse_show(void) {{ }}")?;
    writeln_code!(
        output,
        "void qb_mouse_move(int32_t x, int32_t y) {{ (void)x; (void)y; }}"
    )?;
    writeln_code!(output)?;

    // Clipboard stubs (return empty string)
    writeln_code!(
        output,
        "qb_string* qb_clipboard_get(void) {{ return qb_string_new(\"\"); }}"
    )?;
    writeln_code!(
        output,
        "void qb_clipboard_set(const char* text) {{ (void)text; }}"
    )?;
    writeln_code!(output)?;

    // File content helpers
    writeln_code!(output, "/* File Content Helpers */")?;
    writeln_code!(output)?;

    // _READFILE$ - read entire file into string
    writeln_code!(output, "qb_string* qb_readfile(qb_string* path) {{")?;
    writeln_code!(
        output,
        "    if (!path || !path->data) return qb_string_new(\"\");"
    )?;
    writeln_code!(output, "    FILE* f = fopen(path->data, \"rb\");")?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    if (!f) {{")?;
    writeln_code!(output, "        char* n = _qb_normalize_path(path->data);")?;
    writeln_code!(
        output,
        "        if (n) {{ f = fopen(n, \"rb\"); free(n); }}"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    if (!f) return qb_string_new(\"\");")?;
    writeln_code!(output, "    fseek(f, 0, SEEK_END);")?;
    writeln_code!(output, "    long size = ftell(f);")?;
    writeln_code!(output, "    fseek(f, 0, SEEK_SET);")?;
    writeln_code!(output, "    char* buf = (char*)malloc(size + 1);")?;
    writeln_code!(
        output,
        "    if (!buf) {{ fclose(f); return qb_string_new(\"\"); }}"
    )?;
    writeln_code!(output, "    fread(buf, 1, size, f);")?;
    writeln_code!(output, "    buf[size] = '\\0';")?;
    writeln_code!(output, "    fclose(f);")?;
    writeln_code!(output, "    qb_string* result = qb_string_new(buf);")?;
    writeln_code!(output, "    free(buf);")?;
    writeln_code!(output, "    return result;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _WRITEFILE - write string to file
    writeln_code!(
        output,
        "void qb_writefile(qb_string* path, qb_string* content) {{"
    )?;
    writeln_code!(
        output,
        "    if (!path || !path->data || !content || !content->data) return;"
    )?;
    writeln_code!(output, "    FILE* f = fopen(path->data, \"wb\");")?;
    writeln_code!(output, "#ifndef _WIN32")?;
    writeln_code!(output, "    if (!f) {{")?;
    writeln_code!(output, "        char* n = _qb_normalize_path(path->data);")?;
    writeln_code!(
        output,
        "        if (n) {{ f = fopen(n, \"wb\"); free(n); }}"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "    if (!f) return;")?;
    writeln_code!(output, "    fwrite(content->data, 1, content->len, f);")?;
    writeln_code!(output, "    fclose(f);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Utility functions
    writeln_code!(output, "/* Utility Functions */")?;
    writeln_code!(output)?;

    // _COMMANDCOUNT - returns number of command line arguments
    // Uses _qb_argc/argv defined earlier for COMMAND$
    writeln_code!(
        output,
        "int64_t qb_commandcount(void) {{ return _qb_argc - 1; }}"
    )?;
    writeln_code!(output)?;

    // _ENVIRONCOUNT - returns number of environment variables
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "int64_t qb_environcount(void) {{")?;
    writeln_code!(output, "    int count = 0;")?;
    writeln_code!(output, "    char* env = GetEnvironmentStringsA();")?;
    writeln_code!(output, "    if (env) {{")?;
    writeln_code!(output, "        char* p = env;")?;
    writeln_code!(
        output,
        "        while (*p) {{ count++; p += strlen(p) + 1; }}"
    )?;
    writeln_code!(output, "        FreeEnvironmentStringsA(env);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return count;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "extern char** environ;")?;
    writeln_code!(output, "int64_t qb_environcount(void) {{")?;
    writeln_code!(output, "    int count = 0;")?;
    writeln_code!(output, "    if (environ) {{")?;
    writeln_code!(output, "        while (environ[count]) count++;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return count;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // Font stubs
    writeln_code!(output, "/* Font Stubs */")?;
    writeln_code!(output, "static int64_t _qb_current_font = 0;")?;
    // qb_font_get - returns current font handle (for _FONT pseudo-variable)
    writeln_code!(
        output,
        "int64_t qb_font_get(void) {{ return _qb_current_font; }}"
    )?;
    writeln_code!(
        output,
        "int64_t qb_loadfont(qb_string* file, int64_t size) {{ _qb_gfx_warn(); (void)file; (void)size; return 0; }}"
    )?;
    writeln_code!(output, "int64_t qb_font(int64_t handle) {{")?;
    writeln_code!(output, "    int64_t prev = _qb_current_font;")?;
    writeln_code!(output, "    _qb_current_font = handle;")?;
    writeln_code!(output, "    return prev;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(
        output,
        "int64_t qb_freefont(int64_t handle) {{ (void)handle; return 0; }}"
    )?;
    writeln_code!(output, "int64_t qb_fontheight(void) {{ return 16; }}")?;
    writeln_code!(output, "int64_t qb_fontwidth(void) {{ return 8; }}")?;

    // _PRINTWIDTH function - returns pixel width of a string
    writeln_code!(output, "int64_t qb_printwidth(qb_string* text) {{")?;
    writeln_code!(
        output,
        "    if (text == NULL || text->data == NULL) return 0;"
    )?;
    writeln_code!(output, "    return (int64_t)text->len * qb_fontwidth();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Unicode font functions
    // In inline mode, these provide basic functionality using the built-in font.
    // For full Unicode/FreeType support, use --runtime external.
    writeln_code!(output, "/* Unicode Font Functions */")?;
    writeln_code!(
        output,
        "/* For full Unicode rendering, use --runtime external with FreeType */"
    )?;
    writeln_code!(output)?;

    // Font loading option flags
    writeln_code!(
        output,
        "#define QB_FONT_DONTBLEND   8   /* No anti-aliasing */"
    )?;
    writeln_code!(
        output,
        "#define QB_FONT_MONOSPACE   16  /* Force monospace */"
    )?;
    writeln_code!(output, "#define QB_FONT_UNICODE     32  /* UTF-8 mode */")?;
    writeln_code!(
        output,
        "#define QB_FONT_AUTOMONO    64  /* Auto-detect mono */"
    )?;
    writeln_code!(output)?;

    // _UPRINTSTRING - print Unicode text at pixel position
    // Falls back to regular print (ASCII subset only in inline mode)
    writeln_code!(
        output,
        "void qb_uprintstring(int64_t x, int64_t y, qb_string* text) {{"
    )?;
    writeln_code!(
        output,
        "    if (text == NULL || text->data == NULL) return;"
    )?;
    writeln_code!(
        output,
        "    /* Inline mode: render using built-in 8x8 font (ASCII only) */"
    )?;
    writeln_code!(
        output,
        "    qb_gfx_printstring((int32_t)x, (int32_t)y, text->data);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _UPRINTWIDTH - returns pixel width of Unicode text
    writeln_code!(output, "int64_t qb_uprintwidth(qb_string* text) {{")?;
    writeln_code!(
        output,
        "    /* In inline mode, use byte-based width (8 pixels/char) */"
    )?;
    writeln_code!(output, "    return qb_printwidth(text);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _UCHARPOS - returns x position of character at index
    writeln_code!(
        output,
        "int64_t qb_ucharpos(qb_string* text, int64_t pos) {{"
    )?;
    writeln_code!(output, "    (void)text;")?;
    writeln_code!(
        output,
        "    /* In inline mode, assume 8 pixels per character */"
    )?;
    writeln_code!(output, "    if (pos < 1) return -1;")?;
    writeln_code!(output, "    return (pos - 1) * qb_fontwidth();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _UFONTHEIGHT - returns Unicode font height
    writeln_code!(output, "int64_t qb_ufontheight(int64_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(
        output,
        "    /* Returns current font height (16 for built-in font) */"
    )?;
    writeln_code!(output, "    return qb_fontheight();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _ULINESPACING - returns Unicode line spacing
    writeln_code!(output, "int64_t qb_ulinespacing(void) {{")?;
    writeln_code!(
        output,
        "    /* Returns current font height as line spacing */"
    )?;
    writeln_code!(output, "    return qb_fontheight();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Desktop/Window functions
    writeln_code!(output, "/* Desktop/Window Functions */")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(
        output,
        "int64_t qb_desktopwidth(void) {{ return GetSystemMetrics(SM_CXSCREEN); }}"
    )?;
    writeln_code!(
        output,
        "int64_t qb_desktopheight(void) {{ return GetSystemMetrics(SM_CYSCREEN); }}"
    )?;
    writeln_code!(output, "#else")?;
    writeln_code!(
        output,
        "int64_t qb_desktopwidth(void) {{ return 1920; }}  /* Stub - requires X11/Wayland */"
    )?;
    writeln_code!(
        output,
        "int64_t qb_desktopheight(void) {{ return 1080; }} /* Stub - requires X11/Wayland */"
    )?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    writeln_code!(output, "int64_t qb_screenx(void) {{ return 0; }}")?;
    writeln_code!(output, "int64_t qb_screeny(void) {{ return 0; }}")?;
    writeln_code!(output, "static char _qb_window_title[256] = \"QB64Fresh\";")?;
    writeln_code!(
        output,
        "qb_string* qb_title_get(void) {{ return qb_string_new(_qb_window_title); }}"
    )?;
    writeln_code!(
        output,
        "void qb_title_set(qb_string* title) {{ if (title && title->data) strncpy(_qb_window_title, title->data, 255); }}"
    )?;
    // _WINDOWHANDLE - Windows only, returns HWND
    writeln_code!(output, "int64_t qb_windowhandle(void) {{")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    return (int64_t)GetActiveWindow();")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "int64_t qb_windowhasfocus(void) {{ return -1; }}")?;
    writeln_code!(output)?;

    // _SCREENEXISTS - returns -1 (true) if window exists, 0 (false) if closed
    // In stub mode, this respects the frame limit to prevent infinite loops
    writeln_code!(output, "int64_t qb_screenexists(void) {{")?;
    writeln_code!(output, "    _qb_gfx_init_max_frames();")?;
    writeln_code!(
        output,
        "    if (_qb_gfx_frame_count >= _qb_gfx_max_frames) {{"
    )?;
    writeln_code!(
        output,
        "        if (_qb_gfx_frame_count == _qb_gfx_max_frames) {{"
    )?;
    writeln_code!(
        output,
        "            fprintf(stderr, \"Note: Stub graphics reached %d frames, _SCREENEXISTS returning FALSE.\\n\", _qb_gfx_max_frames);"
    )?;
    writeln_code!(
        output,
        "            fprintf(stderr, \"      Set QB64FRESH_MAX_FRAMES environment variable to change limit.\\n\");"
    )?;
    writeln_code!(output, "            fflush(stderr);")?;
    writeln_code!(
        output,
        "            _qb_gfx_frame_count++; /* Only print once */"
    )?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        return 0; /* Window closed */")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(
        output,
        "    return -1; /* Window exists (QB64 uses -1 for TRUE) */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Window control functions
    writeln_code!(output, "/* Window Control Functions */")?;
    writeln_code!(output, "static int _qb_screen_visible = 1;")?;
    writeln_code!(output, "static int _qb_fullscreen_mode = 0;")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_screenmove(int32_t x, int32_t y) {{")?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(output, "    (void)x; (void)y;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_screenhide(void) {{")?;
    writeln_code!(output, "    _qb_screen_visible = 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_screenshow(void) {{")?;
    writeln_code!(output, "    _qb_screen_visible = 1;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _FULLSCREEN statement - sets mode, returns previous mode
    writeln_code!(output, "int32_t qb_fullscreen(int32_t mode) {{")?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(output, "    int32_t prev = _qb_fullscreen_mode;")?;
    writeln_code!(output, "    _qb_fullscreen_mode = mode;")?;
    writeln_code!(output, "    return prev;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _FULLSCREEN function - get current mode
    writeln_code!(output, "int32_t qb_fullscreen_get(void) {{")?;
    writeln_code!(output, "    return _qb_fullscreen_mode;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SCREENCLICK x, y [, button] - Windows only, simulates mouse click on desktop
    writeln_code!(
        output,
        "void qb_screenclick(int32_t x, int32_t y, int32_t button) {{"
    )?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    INPUT input;")?;
    writeln_code!(output, "    HWND hwnd = GetDesktopWindow();")?;
    writeln_code!(output, "    RECT rect;")?;
    writeln_code!(output, "    GetWindowRect(hwnd, &rect);")?;
    writeln_code!(
        output,
        "    double fx = 65535.0 / (double)(rect.right - rect.left);"
    )?;
    writeln_code!(
        output,
        "    double fy = 65535.0 / (double)(rect.bottom - rect.top);"
    )?;
    writeln_code!(output, "    ZeroMemory(&input, sizeof(INPUT));")?;
    writeln_code!(output, "    input.type = INPUT_MOUSE;")?;
    writeln_code!(
        output,
        "    input.mi.dwFlags = MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE;"
    )?;
    writeln_code!(output, "    input.mi.dx = (LONG)(x * fx);")?;
    writeln_code!(output, "    input.mi.dy = (LONG)(y * fy);")?;
    writeln_code!(output, "    SendInput(1, &input, sizeof(INPUT));")?;
    writeln_code!(output, "    /* Button down */")?;
    writeln_code!(
        output,
        "    input.mi.dwFlags = (button == 2) ? MOUSEEVENTF_RIGHTDOWN : MOUSEEVENTF_LEFTDOWN;"
    )?;
    writeln_code!(output, "    SendInput(1, &input, sizeof(INPUT));")?;
    writeln_code!(output, "    /* Button up */")?;
    writeln_code!(
        output,
        "    input.mi.dwFlags = (button == 2) ? MOUSEEVENTF_RIGHTUP : MOUSEEVENTF_LEFTUP;"
    )?;
    writeln_code!(output, "    SendInput(1, &input, sizeof(INPUT));")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    (void)x; (void)y; (void)button;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SCREENPRINT text$ - Windows only, simulates keyboard input to focused window
    writeln_code!(output, "void qb_screenprint(qb_string* text) {{")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    if (!text || !text->data) return;")?;
    writeln_code!(output, "    INPUT input;")?;
    writeln_code!(output, "    for (size_t i = 0; i < text->len; i++) {{")?;
    writeln_code!(output, "        char c = text->data[i];")?;
    writeln_code!(output, "        SHORT vk = VkKeyScanA(c);")?;
    writeln_code!(output, "        if (vk == -1) continue;")?;
    writeln_code!(
        output,
        "        BYTE scancode = (BYTE)MapVirtualKeyA(vk & 0xFF, MAPVK_VK_TO_VSC);"
    )?;
    writeln_code!(output, "        int shift = (vk >> 8) & 1;")?;
    writeln_code!(output, "        /* Shift down if needed */")?;
    writeln_code!(output, "        if (shift) {{")?;
    writeln_code!(output, "            ZeroMemory(&input, sizeof(INPUT));")?;
    writeln_code!(output, "            input.type = INPUT_KEYBOARD;")?;
    writeln_code!(output, "            input.ki.wVk = VK_SHIFT;")?;
    writeln_code!(output, "            SendInput(1, &input, sizeof(INPUT));")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "        /* Key down */")?;
    writeln_code!(output, "        ZeroMemory(&input, sizeof(INPUT));")?;
    writeln_code!(output, "        input.type = INPUT_KEYBOARD;")?;
    writeln_code!(output, "        input.ki.wVk = vk & 0xFF;")?;
    writeln_code!(output, "        input.ki.wScan = scancode;")?;
    writeln_code!(output, "        SendInput(1, &input, sizeof(INPUT));")?;
    writeln_code!(output, "        /* Key up */")?;
    writeln_code!(output, "        input.ki.dwFlags = KEYEVENTF_KEYUP;")?;
    writeln_code!(output, "        SendInput(1, &input, sizeof(INPUT));")?;
    writeln_code!(output, "        /* Shift up if needed */")?;
    writeln_code!(output, "        if (shift) {{")?;
    writeln_code!(output, "            ZeroMemory(&input, sizeof(INPUT));")?;
    writeln_code!(output, "            input.type = INPUT_KEYBOARD;")?;
    writeln_code!(output, "            input.ki.wVk = VK_SHIFT;")?;
    writeln_code!(output, "            input.ki.dwFlags = KEYEVENTF_KEYUP;")?;
    writeln_code!(output, "            SendInput(1, &input, sizeof(INPUT));")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    (void)text;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SCREENIMAGE([x1, y1, x2, y2]) - Windows only, captures desktop screenshot
    // If all coordinates are 0, captures full screen. Otherwise captures rectangle.
    writeln_code!(
        output,
        "int32_t qb_screenimage(int32_t x1, int32_t y1, int32_t x2, int32_t y2) {{"
    )?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    HWND hwnd = GetDesktopWindow();")?;
    writeln_code!(output, "    RECT rect;")?;
    writeln_code!(output, "    GetWindowRect(hwnd, &rect);")?;
    writeln_code!(output, "    int w, h;")?;
    writeln_code!(output, "    /* If all coords are 0, capture full screen */")?;
    writeln_code!(
        output,
        "    if (x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0) {{"
    )?;
    writeln_code!(output, "        w = rect.right; h = rect.bottom;")?;
    writeln_code!(output, "    }} else {{")?;
    writeln_code!(output, "        if (x1 < 0) x1 = 0;")?;
    writeln_code!(output, "        if (y1 < 0) y1 = 0;")?;
    writeln_code!(
        output,
        "        if (x2 > rect.right - 1) x2 = rect.right - 1;"
    )?;
    writeln_code!(
        output,
        "        if (y2 > rect.bottom - 1) y2 = rect.bottom - 1;"
    )?;
    writeln_code!(output, "        w = x2 - x1 + 1;")?;
    writeln_code!(output, "        h = y2 - y1 + 1;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    if (w <= 0 || h <= 0) return -1;")?;
    writeln_code!(output, "    HDC hdc = GetDC(NULL);")?;
    writeln_code!(output, "    HDC hdc2 = CreateCompatibleDC(hdc);")?;
    writeln_code!(
        output,
        "    HBITMAP bitmap = CreateCompatibleBitmap(hdc, w, h);"
    )?;
    writeln_code!(output, "    SelectObject(hdc2, bitmap);")?;
    writeln_code!(
        output,
        "    BitBlt(hdc2, 0, 0, w, h, hdc, x1, y1, SRCCOPY);"
    )?;
    writeln_code!(output, "    /* Create image and copy pixels */")?;
    writeln_code!(output, "    int32_t img = qb_gfx_newimage(w, h, 32);")?;
    writeln_code!(output, "    if (img > 0) {{")?;
    writeln_code!(output, "        BITMAPINFOHEADER bi;")?;
    writeln_code!(output, "        bi.biSize = sizeof(BITMAPINFOHEADER);")?;
    writeln_code!(output, "        bi.biWidth = w;")?;
    writeln_code!(output, "        bi.biHeight = -h; /* Top-down */")?;
    writeln_code!(output, "        bi.biPlanes = 1;")?;
    writeln_code!(output, "        bi.biBitCount = 32;")?;
    writeln_code!(output, "        bi.biCompression = BI_RGB;")?;
    writeln_code!(output, "        bi.biSizeImage = 0;")?;
    writeln_code!(output, "        bi.biXPelsPerMeter = 0;")?;
    writeln_code!(output, "        bi.biYPelsPerMeter = 0;")?;
    writeln_code!(output, "        bi.biClrUsed = 0;")?;
    writeln_code!(output, "        bi.biClrImportant = 0;")?;
    writeln_code!(
        output,
        "        /* Would need _qb_images array access to copy pixels */"
    )?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    DeleteObject(bitmap);")?;
    writeln_code!(output, "    DeleteDC(hdc2);")?;
    writeln_code!(output, "    ReleaseDC(NULL, hdc);")?;
    writeln_code!(output, "    return img;")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    (void)x1; (void)y1; (void)x2; (void)y2;")?;
    writeln_code!(output, "    return -1; /* Not supported on non-Windows */")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Alpha blending functions
    writeln_code!(output, "/* Alpha Blending Functions */")?;
    writeln_code!(
        output,
        "static int _qb_blend_enabled[256] = {{0}};  /* Per-image blend state */"
    )?;
    writeln_code!(
        output,
        "static int32_t _qb_clear_color[256] = {{0}};  /* Per-image clear color */"
    )?;
    writeln_code!(
        output,
        "static int _qb_clear_color_set[256] = {{0}};  /* Whether clear color is set */"
    )?;
    writeln_code!(output)?;

    // Initialize all images to blend enabled
    writeln_code!(output, "static void _qb_init_blend(void) {{")?;
    writeln_code!(output, "    static int initialized = 0;")?;
    writeln_code!(output, "    if (!initialized) {{")?;
    writeln_code!(
        output,
        "        for (int i = 0; i < 256; i++) _qb_blend_enabled[i] = 1;"
    )?;
    writeln_code!(output, "        initialized = 1;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_blend(int32_t handle) {{")?;
    writeln_code!(output, "    _qb_init_blend();")?;
    writeln_code!(
        output,
        "    if (handle >= 0 && handle < 256) _qb_blend_enabled[handle] = 1;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_dontblend(int32_t handle) {{")?;
    writeln_code!(output, "    _qb_init_blend();")?;
    writeln_code!(
        output,
        "    if (handle >= 0 && handle < 256) _qb_blend_enabled[handle] = 0;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_clearcolor(uint32_t color, int32_t handle) {{"
    )?;
    writeln_code!(output, "    if (handle >= 0 && handle < 256) {{")?;
    writeln_code!(output, "        _qb_clear_color[handle] = (int32_t)color;")?;
    writeln_code!(output, "        _qb_clear_color_set[handle] = 1;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "void qb_clearcolor_none(int32_t handle) {{")?;
    writeln_code!(
        output,
        "    if (handle >= 0 && handle < 256) _qb_clear_color_set[handle] = 0;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(output, "int64_t qb_clearcolor_get(int32_t handle) {{")?;
    writeln_code!(
        output,
        "    if (handle >= 0 && handle < 256 && _qb_clear_color_set[handle])"
    )?;
    writeln_code!(
        output,
        "        return (int64_t)(uint32_t)_qb_clear_color[handle];"
    )?;
    writeln_code!(output, "    return -1;  /* No clear color set */")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Palette operations
    writeln_code!(output, "/* Palette Operations */")?;
    writeln_code!(
        output,
        "static uint32_t _qb_palettes[256][256];  /* Per-image palettes */"
    )?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_copypalette(int32_t src_handle, int32_t dest_handle) {{"
    )?;
    writeln_code!(
        output,
        "    if (src_handle < 0 || src_handle >= 256) return;"
    )?;
    writeln_code!(
        output,
        "    if (dest_handle < 0 || dest_handle >= 256) return;"
    )?;
    writeln_code!(
        output,
        "    memcpy(_qb_palettes[dest_handle], _qb_palettes[src_handle], sizeof(_qb_palettes[0]));"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Display layer ordering
    writeln_code!(output, "/* Display Layer Ordering */")?;
    writeln_code!(
        output,
        "static int32_t _qb_display_order[4] = {{1, 2, 3, 4}};  /* Layer order */"
    )?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_displayorder(int32_t layer1, int32_t layer2, int32_t layer3, int32_t layer4) {{"
    )?;
    writeln_code!(output, "    _qb_display_order[0] = layer1;")?;
    writeln_code!(output, "    _qb_display_order[1] = layer2;")?;
    writeln_code!(output, "    _qb_display_order[2] = layer3;")?;
    writeln_code!(output, "    _qb_display_order[3] = layer4;")?;
    writeln_code!(
        output,
        "    /* Note: Layer ordering affects compositing in full runtime */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // Dialog boxes
    writeln_code!(output, "/* Dialog Box Stubs */")?;
    writeln_code!(
        output,
        "/* Full implementations require platform-specific GUI libraries */"
    )?;
    writeln_code!(output)?;

    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(
        output,
        "int64_t qb_messagebox(qb_string* title, qb_string* msg) {{"
    )?;
    writeln_code!(
        output,
        "    return MessageBoxA(NULL, msg ? msg->data : \"\", title ? title->data : \"\", MB_OK);"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(
        output,
        "int64_t qb_messagebox(qb_string* title, qb_string* msg) {{"
    )?;
    writeln_code!(
        output,
        "    printf(\"[%s] %s\\n\", title ? title->data : \"\", msg ? msg->data : \"\");"
    )?;
    writeln_code!(output, "    return 1;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "qb_string* qb_inputbox(qb_string* prompt, qb_string* title) {{"
    )?;
    writeln_code!(output, "    (void)title;")?;
    writeln_code!(output, "    char buf[1024];")?;
    writeln_code!(output, "    printf(\"%s \", prompt ? prompt->data : \"\");")?;
    writeln_code!(output, "    if (fgets(buf, sizeof(buf), stdin)) {{")?;
    writeln_code!(output, "        size_t len = strlen(buf);")?;
    writeln_code!(
        output,
        "        if (len > 0 && buf[len-1] == '\\n') buf[len-1] = '\\0';"
    )?;
    writeln_code!(output, "        return qb_string_new(buf);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    return qb_string_new(\"\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // File dialogs return empty string (no GUI support in console mode)
    writeln_code!(
        output,
        "qb_string* qb_openfiledialog(qb_string* title, qb_string* filter) {{"
    )?;
    writeln_code!(output, "    (void)title; (void)filter;")?;
    writeln_code!(
        output,
        "    fprintf(stderr, \"Note: _OPENFILEDIALOG$ requires external runtime for GUI support\\n\");"
    )?;
    writeln_code!(output, "    fflush(stderr);")?;
    writeln_code!(output, "    return qb_string_new(\"\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "qb_string* qb_savefiledialog(qb_string* title, qb_string* filter) {{"
    )?;
    writeln_code!(output, "    (void)title; (void)filter;")?;
    writeln_code!(
        output,
        "    fprintf(stderr, \"Note: _SAVEFILEDIALOG$ requires external runtime for GUI support\\n\");"
    )?;
    writeln_code!(output, "    fflush(stderr);")?;
    writeln_code!(output, "    return qb_string_new(\"\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "qb_string* qb_selectfolderdialog(qb_string* title) {{"
    )?;
    writeln_code!(output, "    (void)title;")?;
    writeln_code!(
        output,
        "    fprintf(stderr, \"Note: _SELECTFOLDERDIALOG$ requires external runtime for GUI support\\n\");"
    )?;
    writeln_code!(output, "    fflush(stderr);")?;
    writeln_code!(output, "    return qb_string_new(\"\");")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MAPTRIANGLE - Triangle texture mapping (stub for inline runtime)
    writeln_code!(output, "/* Triangle Mapping */")?;
    writeln_code!(
        output,
        "void qb_maptriangle(double sx1, double sy1, double sx2, double sy2, double sx3, double sy3,"
    )?;
    writeln_code!(
        output,
        "                    double dx1, double dy1, double dx2, double dy2, double dx3, double dy3) {{"
    )?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(
        output,
        "    (void)sx1; (void)sy1; (void)sx2; (void)sy2; (void)sx3; (void)sy3;"
    )?;
    writeln_code!(
        output,
        "    (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)dx3; (void)dy3;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    writeln_code!(
        output,
        "void qb_maptriangle_ex(double sx1, double sy1, double sx2, double sy2, double sx3, double sy3,"
    )?;
    writeln_code!(
        output,
        "                       double dx1, double dy1, double dx2, double dy2, double dx3, double dy3,"
    )?;
    writeln_code!(
        output,
        "                       int32_t src_handle, int32_t dest_handle, int32_t smooth, int32_t seamless) {{"
    )?;
    writeln_code!(output, "    _qb_gfx_warn();")?;
    writeln_code!(
        output,
        "    (void)sx1; (void)sy1; (void)sx2; (void)sy2; (void)sx3; (void)sy3;"
    )?;
    writeln_code!(
        output,
        "    (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)dx3; (void)dy3;"
    )?;
    writeln_code!(
        output,
        "    (void)src_handle; (void)dest_handle; (void)smooth; (void)seamless;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _GLRENDER, _GLCOMPAT - OpenGL stubs (no-op; raw _GL* excluded per ADR-0014)
    writeln_code!(output, "/* OpenGL stubs */")?;
    writeln_code!(output, "void qb_glrender(int32_t mode) {{ (void)mode; }}")?;
    writeln_code!(output, "int32_t qb_glcompat(void) {{ return 0; }}")?;
    writeln_code!(output)?;
    Ok(())
}
