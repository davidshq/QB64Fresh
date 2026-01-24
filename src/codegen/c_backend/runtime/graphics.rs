//! Graphics stubs for the inline runtime.
//!
//! This module contains the `emit_graphics_stubs` function which generates C code
//! for graphics operations when using the inline runtime mode. These stubs allow
//! programs that use graphics commands to compile without requiring the external
//! runtime library, though actual graphics functionality is not available.
//!
//! For full graphics support, use `--runtime external` and link with `libqb64fresh_rt`.

use std::fmt::Write;

/// Emits graphics operation stubs for the inline runtime.
///
/// These stubs allow programs that use graphics commands to compile even when
/// using the inline runtime. They print a warning message on first use and
/// return safe default values.
pub(super) fn emit_graphics_stubs(output: &mut String) {
    writeln!(output, "/* Graphics Stubs (Inline Runtime) */").unwrap();
    writeln!(
        output,
        "/* For full graphics support, use --runtime external and link with libqb64fresh_rt */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Warning flag
    writeln!(output, "static int _qb_gfx_warned = 0;").unwrap();
    writeln!(output, "static void _qb_gfx_warn(void) {{").unwrap();
    writeln!(output, "    if (!_qb_gfx_warned) {{").unwrap();
    writeln!(output, "        fprintf(stderr, \"Warning: Graphics functions require external runtime. Use --runtime external\\n\");").unwrap();
    writeln!(output, "        _qb_gfx_warned = 1;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Initialization
    writeln!(
        output,
        "int qb_gfx_init(int32_t mode) {{ _qb_gfx_warn(); (void)mode; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int qb_gfx_shutdown(void) {{ return 0; }}").unwrap();
    writeln!(output).unwrap();

    // Basic graphics operations
    writeln!(output, "int qb_gfx_cls(void) {{ return 0; }}").unwrap();
    writeln!(
        output,
        "int qb_gfx_color(uint32_t fg, uint32_t bg) {{ (void)fg; (void)bg; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int qb_gfx_locate(int32_t row, int32_t col) {{ (void)row; (void)col; return 0; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Drawing
    writeln!(output, "int qb_gfx_pset(int32_t x, int32_t y, uint32_t color) {{ (void)x; (void)y; (void)color; return 0; }}").unwrap();
    writeln!(
        output,
        "uint32_t qb_gfx_point(int32_t x, int32_t y) {{ (void)x; (void)y; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int qb_gfx_line(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color) {{ (void)x1; (void)y1; (void)x2; (void)y2; (void)color; return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_box(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int filled) {{ (void)x1; (void)y1; (void)x2; (void)y2; (void)color; (void)filled; return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_circle(int32_t x, int32_t y, int32_t radius, uint32_t color, int filled) {{ (void)x; (void)y; (void)radius; (void)color; (void)filled; return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_paint(int32_t x, int32_t y, uint32_t color, uint32_t boundary) {{ (void)x; (void)y; (void)color; (void)boundary; return 0; }}").unwrap();
    writeln!(output).unwrap();

    // Display
    writeln!(output, "int qb_gfx_display(void) {{ return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_poll_events(void) {{ return 1; }}").unwrap();
    writeln!(output, "uint32_t qb_gfx_width(void) {{ return 80; }}").unwrap();
    writeln!(output, "uint32_t qb_gfx_height(void) {{ return 25; }}").unwrap();
    writeln!(output).unwrap();

    // Palette and page copy
    writeln!(
        output,
        "int qb_gfx_palette(int32_t attr, uint32_t color) {{ (void)attr; (void)color; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int qb_gfx_palette_reset(void) {{ return 0; }}").unwrap();
    writeln!(
        output,
        "int qb_gfx_pcopy(int32_t src, int32_t dst) {{ (void)src; (void)dst; return 0; }}"
    )
    .unwrap();
    // Page control for double buffering
    writeln!(
        output,
        "int qb_gfx_set_active_page(int32_t page) {{ (void)page; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int qb_gfx_set_visual_page(int32_t page) {{ (void)page; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "void qb_gfx_get_pages(int32_t* active, int32_t* visual) {{ if (active) *active = 0; if (visual) *visual = 0; }}"
    )
    .unwrap();
    // PMAP: coordinate mapping (stub returns coordinate unchanged)
    writeln!(
        output,
        "double qb_gfx_pmap(double coord, int32_t func_code) {{ (void)func_code; return coord; }}"
    )
    .unwrap();
    // Note: POINT function is already defined above as qb_gfx_point(x, y) -> uint32_t
    writeln!(output).unwrap();

    // Extended graphics
    writeln!(
        output,
        "int qb_gfx_set_width(uint32_t cols, uint32_t rows) {{ (void)cols; (void)rows; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int qb_gfx_view(int screen, int32_t x1, int32_t y1, int32_t x2, int32_t y2, int32_t fill, int32_t border) {{ (void)screen; (void)x1; (void)y1; (void)x2; (void)y2; (void)fill; (void)border; return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_view_reset(void) {{ return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_window(int screen, double x1, double y1, double x2, double y2) {{ (void)screen; (void)x1; (void)y1; (void)x2; (void)y2; return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_window_reset(void) {{ return 0; }}").unwrap();
    writeln!(
        output,
        "int qb_gfx_draw(const char* cmd) {{ (void)cmd; return 0; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Image operations
    writeln!(output, "int32_t qb_gfx_newimage(int32_t w, int32_t h, int32_t mode) {{ (void)w; (void)h; (void)mode; return -1; }}").unwrap();
    writeln!(output, "int32_t qb_gfx_loadimage(const char* fn, int32_t mode) {{ (void)fn; (void)mode; return -1; }}").unwrap();
    writeln!(output, "int32_t qb_gfx_copyimage(int32_t src, int32_t mode) {{ (void)src; (void)mode; return -1; }}").unwrap();
    writeln!(
        output,
        "int qb_gfx_freeimage(int32_t h) {{ (void)h; return 0; }}"
    )
    .unwrap();
    // _PUTIMAGE functions with scale_mode parameter:
    // scale_mode: 0 = default, 1 = smooth (bilinear), 2 = stretch (nearest-neighbor)
    writeln!(
        output,
        "int qb_gfx_putimage_simple(int32_t src, int32_t dst, int scale_mode) {{ (void)src; (void)dst; (void)scale_mode; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int qb_gfx_putimage(int32_t dx1, int32_t dy1, int32_t dx2, int32_t dy2, int32_t src, int32_t dst, int scale_mode) {{ (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)src; (void)dst; (void)scale_mode; return 0; }}").unwrap();
    writeln!(output, "int qb_gfx_putimage_full(int32_t dx1, int32_t dy1, int32_t dx2, int32_t dy2, int32_t src, int32_t dst, int32_t sx1, int32_t sy1, int32_t sx2, int32_t sy2, int scale_mode) {{ (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)src; (void)dst; (void)sx1; (void)sy1; (void)sx2; (void)sy2; (void)scale_mode; return 0; }}").unwrap();
    writeln!(
        output,
        "int qb_gfx_source(int32_t h) {{ (void)h; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int qb_gfx_dest(int32_t h) {{ (void)h; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int qb_gfx_printstring(int32_t x, int32_t y, const char* text) {{ (void)x; (void)y; (void)text; return 0; }}").unwrap();
    writeln!(
        output,
        "int qb_gfx_autodisplay(int enabled) {{ (void)enabled; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_gfx_image_width(int32_t h) {{ (void)h; return 0; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_gfx_image_height(int32_t h) {{ (void)h; return 0; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Color creation functions (QB64)
    // _RGB32 creates 32-bit ARGB color - multiple variants for different arg counts
    writeln!(output, "uint32_t qb__rgb32(int32_t r, int32_t g, int32_t b) {{ return 0xFF000000u | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF); }}").unwrap();
    // 4-arg version: either (r,g,b,a) or (gray,gray,gray,alpha) - same implementation
    writeln!(output, "uint32_t qb__rgb32_4(int32_t r, int32_t g, int32_t b, int32_t a) {{ return ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF); }}").unwrap();
    writeln!(output, "uint32_t qb__rgba32(int32_t r, int32_t g, int32_t b, int32_t a) {{ return ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF); }}").unwrap();
    // _RGB/_RGBA for paletted modes (stub - returns index 0)
    writeln!(output, "uint32_t qb__rgb(int32_t r, int32_t g, int32_t b, int32_t mode) {{ (void)r; (void)g; (void)b; (void)mode; return 0; }}").unwrap();
    writeln!(output, "uint32_t qb__rgba(int32_t r, int32_t g, int32_t b, int32_t a, int32_t mode) {{ (void)r; (void)g; (void)b; (void)a; (void)mode; return 0; }}").unwrap();
    writeln!(output).unwrap();

    // Color component extraction (QB64)
    // 32-bit mode extraction - works on ARGB format
    writeln!(
        output,
        "int32_t qb_red32(uint32_t c) {{ return (c >> 16) & 0xFF; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_green32(uint32_t c) {{ return (c >> 8) & 0xFF; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_blue32(uint32_t c) {{ return c & 0xFF; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_alpha32(uint32_t c) {{ return (c >> 24) & 0xFF; }}"
    )
    .unwrap();
    // Paletted mode extraction (stub - returns 0)
    writeln!(
        output,
        "int32_t qb_red(uint32_t c, int32_t mode) {{ (void)mode; return (c >> 16) & 0xFF; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_green(uint32_t c, int32_t mode) {{ (void)mode; return (c >> 8) & 0xFF; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_blue(uint32_t c, int32_t mode) {{ (void)mode; return c & 0xFF; }}"
    )
    .unwrap();
    writeln!(
        output,
        "int32_t qb_alpha(uint32_t c, int32_t mode) {{ (void)mode; return (c >> 24) & 0xFF; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Legacy file open (for compatibility)
    // Signature matches generated code: (fnum, mode_char, filename_char)
    // mode_char is "I" (input), "O" (output), "A" (append), "B" (binary), "R" (random)
    writeln!(
        output,
        "void qb_file_open_legacy(int32_t fnum, const char* mode_char, const char* fname) {{"
    )
    .unwrap();
    writeln!(output, "    const char* fmode = \"r\";").unwrap();
    writeln!(output, "    if (mode_char && mode_char[0]) {{").unwrap();
    writeln!(output, "        switch(mode_char[0]) {{").unwrap();
    writeln!(
        output,
        "            case 'I': case 'i': fmode = \"r\"; break;"
    )
    .unwrap();
    writeln!(
        output,
        "            case 'O': case 'o': fmode = \"w\"; break;"
    )
    .unwrap();
    writeln!(
        output,
        "            case 'A': case 'a': fmode = \"a\"; break;"
    )
    .unwrap();
    writeln!(
        output,
        "            case 'B': case 'b': fmode = \"r+b\"; break;"
    )
    .unwrap();
    writeln!(
        output,
        "            case 'R': case 'r': fmode = \"r+b\"; break;"
    )
    .unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    FILE* f = fopen(fname, fmode);").unwrap();
    writeln!(
        output,
        "    /* For binary/random mode, create file if it doesn't exist */"
    )
    .unwrap();
    writeln!(output, "    if (!f && mode_char && (mode_char[0] == 'B' || mode_char[0] == 'b' || mode_char[0] == 'R' || mode_char[0] == 'r'))").unwrap();
    writeln!(output, "        f = fopen(fname, \"w+b\");").unwrap();
    writeln!(output, "    _qb_file_set(fnum, f);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // VIEW PRINT - text viewport
    writeln!(output, "static int32_t _qb_view_print_top = 1;").unwrap();
    writeln!(output, "static int32_t _qb_view_print_bottom = 25;").unwrap();
    writeln!(
        output,
        "void qb_view_print(int32_t top, int32_t bottom) {{ _qb_view_print_top = top; _qb_view_print_bottom = bottom; }}"
    )
    .unwrap();
    writeln!(
        output,
        "void qb_view_print_reset(void) {{ _qb_view_print_top = 1; _qb_view_print_bottom = 25; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // GET/PUT graphics arrays - action constants
    writeln!(output, "#define QB_PUT_XOR 0").unwrap();
    writeln!(output, "#define QB_PUT_PSET 1").unwrap();
    writeln!(output, "#define QB_PUT_PRESET 2").unwrap();
    writeln!(output, "#define QB_PUT_AND 3").unwrap();
    writeln!(output, "#define QB_PUT_OR 4").unwrap();
    writeln!(output).unwrap();

    // GET - capture screen region to array
    // Array format: first 4 bytes = width (16-bit) + height (16-bit), rest = pixel data (32-bit ARGB)
    writeln!(
        output,
        "int qb_gfx_get(int32_t x1, int32_t y1, int32_t x2, int32_t y2, void* arr) {{"
    )
    .unwrap();
    writeln!(output, "    if (!arr) return 1;").unwrap();
    writeln!(output, "    _qb_gfx_warn();").unwrap();
    writeln!(output, "    /* Normalize coordinates */").unwrap();
    writeln!(output, "    int32_t left = x1 <= x2 ? x1 : x2;").unwrap();
    writeln!(output, "    int32_t right = x1 > x2 ? x1 : x2;").unwrap();
    writeln!(output, "    int32_t top = y1 <= y2 ? y1 : y2;").unwrap();
    writeln!(output, "    int32_t bottom = y1 > y2 ? y1 : y2;").unwrap();
    writeln!(output, "    uint16_t width = (uint16_t)(right - left + 1);").unwrap();
    writeln!(
        output,
        "    uint16_t height = (uint16_t)(bottom - top + 1);"
    )
    .unwrap();
    writeln!(output, "    /* Write header */").unwrap();
    writeln!(output, "    uint16_t* header = (uint16_t*)arr;").unwrap();
    writeln!(output, "    header[0] = width;").unwrap();
    writeln!(output, "    header[1] = height;").unwrap();
    writeln!(
        output,
        "    /* In standalone mode, fill with zeros (no real backend) */"
    )
    .unwrap();
    writeln!(
        output,
        "    uint32_t* pixels = (uint32_t*)((uint8_t*)arr + 4);"
    )
    .unwrap();
    writeln!(
        output,
        "    for (int i = 0; i < width * height; i++) pixels[i] = 0;"
    )
    .unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "int qb_gfx_get_step(int32_t x1, int32_t y1, int32_t w, int32_t h, void* arr) {{"
    )
    .unwrap();
    writeln!(output, "    /* STEP variant: w,h are relative offsets */").unwrap();
    writeln!(
        output,
        "    return qb_gfx_get(x1, y1, x1 + w, y1 + h, arr);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // PUT - draw array contents to screen
    writeln!(output, "int qb_gfx_put(int32_t x, int32_t y, void* arr, int action, int clip, int32_t trans_color) {{").unwrap();
    writeln!(output, "    if (!arr) return 1;").unwrap();
    writeln!(
        output,
        "    (void)x; (void)y; (void)action; (void)clip; (void)trans_color;"
    )
    .unwrap();
    writeln!(output, "    _qb_gfx_warn();").unwrap();
    writeln!(output, "    /* Read header for validation */").unwrap();
    writeln!(output, "    uint16_t* header = (uint16_t*)arr;").unwrap();
    writeln!(output, "    uint16_t width = header[0];").unwrap();
    writeln!(output, "    uint16_t height = header[1];").unwrap();
    writeln!(output, "    (void)width; (void)height;").unwrap();
    writeln!(
        output,
        "    /* In standalone mode, no-op (no real backend) */"
    )
    .unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int qb_gfx_put_step(int32_t x, int32_t y, void* arr, int action, int clip, int32_t trans_color) {{").unwrap();
    writeln!(
        output,
        "    /* STEP variant: x,y are relative to last graphics position */"
    )
    .unwrap();
    writeln!(
        output,
        "    return qb_gfx_put(x, y, arr, action, clip, trans_color);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Color helpers
    writeln!(output, "uint32_t qb_rgb(uint32_t r, uint32_t g, uint32_t b) {{ return 0xFF000000 | ((r & 255) << 16) | ((g & 255) << 8) | (b & 255); }}").unwrap();
    writeln!(output, "uint32_t qb_rgba(uint32_t r, uint32_t g, uint32_t b, uint32_t a) {{ return ((a & 255) << 24) | ((r & 255) << 16) | ((g & 255) << 8) | (b & 255); }}").unwrap();
    writeln!(
        output,
        "uint32_t qb_rgb32(uint32_t r, uint32_t g, uint32_t b) {{ return qb_rgb(r, g, b); }}"
    )
    .unwrap();
    writeln!(output, "uint32_t qb_rgba32(uint32_t r, uint32_t g, uint32_t b, uint32_t a) {{ return qb_rgba(r, g, b, a); }}").unwrap();
    writeln!(output).unwrap();

    // Mouse stubs
    writeln!(output, "int32_t qb_mouse_x(void) {{ return 0; }}").unwrap();
    writeln!(output, "int32_t qb_mouse_y(void) {{ return 0; }}").unwrap();
    writeln!(
        output,
        "int32_t qb_mouse_button(int32_t b) {{ (void)b; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int32_t qb_mouse_input(void) {{ return 0; }}").unwrap();
    writeln!(output, "int32_t qb_mouse_movement_x(void) {{ return 0; }}").unwrap();
    writeln!(output, "int32_t qb_mouse_movement_y(void) {{ return 0; }}").unwrap();
    writeln!(output, "int32_t qb_mouse_wheel(void) {{ return 0; }}").unwrap();
    writeln!(output, "void qb_mouse_hide(void) {{ }}").unwrap();
    writeln!(output, "void qb_mouse_show(void) {{ }}").unwrap();
    writeln!(
        output,
        "void qb_mouse_move(int32_t x, int32_t y) {{ (void)x; (void)y; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Clipboard stubs (return empty string)
    writeln!(
        output,
        "qb_string* qb_clipboard_get(void) {{ return qb_string_new(\"\"); }}"
    )
    .unwrap();
    writeln!(
        output,
        "void qb_clipboard_set(const char* text) {{ (void)text; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // File content helpers
    writeln!(output, "/* File Content Helpers */").unwrap();
    writeln!(output).unwrap();

    // _READFILE$ - read entire file into string
    writeln!(output, "qb_string* qb_readfile(qb_string* path) {{").unwrap();
    writeln!(output, "    FILE* f = fopen(path->data, \"rb\");").unwrap();
    writeln!(output, "    if (!f) return qb_string_new(\"\");").unwrap();
    writeln!(output, "    fseek(f, 0, SEEK_END);").unwrap();
    writeln!(output, "    long size = ftell(f);").unwrap();
    writeln!(output, "    fseek(f, 0, SEEK_SET);").unwrap();
    writeln!(output, "    char* buf = (char*)malloc(size + 1);").unwrap();
    writeln!(
        output,
        "    if (!buf) {{ fclose(f); return qb_string_new(\"\"); }}"
    )
    .unwrap();
    writeln!(output, "    fread(buf, 1, size, f);").unwrap();
    writeln!(output, "    buf[size] = '\\0';").unwrap();
    writeln!(output, "    fclose(f);").unwrap();
    writeln!(output, "    qb_string* result = qb_string_new(buf);").unwrap();
    writeln!(output, "    free(buf);").unwrap();
    writeln!(output, "    return result;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _WRITEFILE - write string to file
    writeln!(
        output,
        "void qb_writefile(qb_string* path, qb_string* content) {{"
    )
    .unwrap();
    writeln!(output, "    FILE* f = fopen(path->data, \"wb\");").unwrap();
    writeln!(output, "    if (!f) return;").unwrap();
    writeln!(output, "    fwrite(content->data, 1, content->len, f);").unwrap();
    writeln!(output, "    fclose(f);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Utility functions
    writeln!(output, "/* Utility Functions */").unwrap();
    writeln!(output).unwrap();

    // _COMMANDCOUNT - returns number of command line arguments
    // Uses _qb_argc/argv defined earlier for COMMAND$
    writeln!(
        output,
        "int64_t qb_commandcount(void) {{ return _qb_argc - 1; }}"
    )
    .unwrap();
    writeln!(output).unwrap();

    // _ENVIRONCOUNT - returns number of environment variables
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "int64_t qb_environcount(void) {{").unwrap();
    writeln!(output, "    int count = 0;").unwrap();
    writeln!(output, "    char* env = GetEnvironmentStringsA();").unwrap();
    writeln!(output, "    if (env) {{").unwrap();
    writeln!(output, "        char* p = env;").unwrap();
    writeln!(
        output,
        "        while (*p) {{ count++; p += strlen(p) + 1; }}"
    )
    .unwrap();
    writeln!(output, "        FreeEnvironmentStringsA(env);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return count;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "extern char** environ;").unwrap();
    writeln!(output, "int64_t qb_environcount(void) {{").unwrap();
    writeln!(output, "    int count = 0;").unwrap();
    writeln!(output, "    if (environ) {{").unwrap();
    writeln!(output, "        while (environ[count]) count++;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return count;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // Font stubs
    writeln!(output, "/* Font Stubs */").unwrap();
    writeln!(output, "static int64_t _qb_current_font = 0;").unwrap();
    // qb_font_get - returns current font handle (for _FONT pseudo-variable)
    writeln!(
        output,
        "int64_t qb_font_get(void) {{ return _qb_current_font; }}"
    )
    .unwrap();
    writeln!(output, "int64_t qb_loadfont(qb_string* file, int64_t size) {{ _qb_gfx_warn(); (void)file; (void)size; return 0; }}").unwrap();
    writeln!(output, "int64_t qb_font(int64_t handle) {{").unwrap();
    writeln!(output, "    int64_t prev = _qb_current_font;").unwrap();
    writeln!(output, "    _qb_current_font = handle;").unwrap();
    writeln!(output, "    return prev;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(
        output,
        "int64_t qb_freefont(int64_t handle) {{ (void)handle; return 0; }}"
    )
    .unwrap();
    writeln!(output, "int64_t qb_fontheight(void) {{ return 16; }}").unwrap();
    writeln!(output, "int64_t qb_fontwidth(void) {{ return 8; }}").unwrap();

    // _PRINTWIDTH function - returns pixel width of a string
    writeln!(output, "int64_t qb_printwidth(qb_string* text) {{").unwrap();
    writeln!(
        output,
        "    if (text == NULL || text->data == NULL) return 0;"
    )
    .unwrap();
    writeln!(output, "    return (int64_t)text->len * qb_fontwidth();").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Desktop/Window functions
    writeln!(output, "/* Desktop/Window Functions */").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(
        output,
        "int64_t qb_desktopwidth(void) {{ return GetSystemMetrics(SM_CXSCREEN); }}"
    )
    .unwrap();
    writeln!(
        output,
        "int64_t qb_desktopheight(void) {{ return GetSystemMetrics(SM_CYSCREEN); }}"
    )
    .unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(
        output,
        "int64_t qb_desktopwidth(void) {{ return 1920; }}  /* Stub - requires X11/Wayland */"
    )
    .unwrap();
    writeln!(
        output,
        "int64_t qb_desktopheight(void) {{ return 1080; }} /* Stub - requires X11/Wayland */"
    )
    .unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int64_t qb_screenx(void) {{ return 0; }}").unwrap();
    writeln!(output, "int64_t qb_screeny(void) {{ return 0; }}").unwrap();
    writeln!(output, "static char _qb_window_title[256] = \"QB64Fresh\";").unwrap();
    writeln!(
        output,
        "qb_string* qb_title_get(void) {{ return qb_string_new(_qb_window_title); }}"
    )
    .unwrap();
    writeln!(output, "void qb_title_set(qb_string* title) {{ if (title && title->data) strncpy(_qb_window_title, title->data, 255); }}").unwrap();
    // _WINDOWHANDLE - Windows only, returns HWND
    writeln!(output, "int64_t qb_windowhandle(void) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    return (int64_t)GetActiveWindow();").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int64_t qb_windowhasfocus(void) {{ return -1; }}").unwrap();
    writeln!(output).unwrap();

    // Window control functions
    writeln!(output, "/* Window Control Functions */").unwrap();
    writeln!(output, "static int _qb_screen_visible = 1;").unwrap();
    writeln!(output, "static int _qb_fullscreen_mode = 0;").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_screenmove(int32_t x, int32_t y) {{").unwrap();
    writeln!(output, "    _qb_gfx_warn();").unwrap();
    writeln!(output, "    (void)x; (void)y;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_screenhide(void) {{").unwrap();
    writeln!(output, "    _qb_screen_visible = 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_screenshow(void) {{").unwrap();
    writeln!(output, "    _qb_screen_visible = 1;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _FULLSCREEN statement - sets mode, returns previous mode
    writeln!(output, "int32_t qb_fullscreen(int32_t mode) {{").unwrap();
    writeln!(output, "    _qb_gfx_warn();").unwrap();
    writeln!(output, "    int32_t prev = _qb_fullscreen_mode;").unwrap();
    writeln!(output, "    _qb_fullscreen_mode = mode;").unwrap();
    writeln!(output, "    return prev;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _FULLSCREEN function - get current mode
    writeln!(output, "int32_t qb_fullscreen_get(void) {{").unwrap();
    writeln!(output, "    return _qb_fullscreen_mode;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SCREENCLICK x, y [, button] - Windows only, simulates mouse click on desktop
    writeln!(
        output,
        "void qb_screenclick(int32_t x, int32_t y, int32_t button) {{"
    )
    .unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    INPUT input;").unwrap();
    writeln!(output, "    HWND hwnd = GetDesktopWindow();").unwrap();
    writeln!(output, "    RECT rect;").unwrap();
    writeln!(output, "    GetWindowRect(hwnd, &rect);").unwrap();
    writeln!(
        output,
        "    double fx = 65535.0 / (double)(rect.right - rect.left);"
    )
    .unwrap();
    writeln!(
        output,
        "    double fy = 65535.0 / (double)(rect.bottom - rect.top);"
    )
    .unwrap();
    writeln!(output, "    ZeroMemory(&input, sizeof(INPUT));").unwrap();
    writeln!(output, "    input.type = INPUT_MOUSE;").unwrap();
    writeln!(
        output,
        "    input.mi.dwFlags = MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE;"
    )
    .unwrap();
    writeln!(output, "    input.mi.dx = (LONG)(x * fx);").unwrap();
    writeln!(output, "    input.mi.dy = (LONG)(y * fy);").unwrap();
    writeln!(output, "    SendInput(1, &input, sizeof(INPUT));").unwrap();
    writeln!(output, "    /* Button down */").unwrap();
    writeln!(
        output,
        "    input.mi.dwFlags = (button == 2) ? MOUSEEVENTF_RIGHTDOWN : MOUSEEVENTF_LEFTDOWN;"
    )
    .unwrap();
    writeln!(output, "    SendInput(1, &input, sizeof(INPUT));").unwrap();
    writeln!(output, "    /* Button up */").unwrap();
    writeln!(
        output,
        "    input.mi.dwFlags = (button == 2) ? MOUSEEVENTF_RIGHTUP : MOUSEEVENTF_LEFTUP;"
    )
    .unwrap();
    writeln!(output, "    SendInput(1, &input, sizeof(INPUT));").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    (void)x; (void)y; (void)button;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SCREENPRINT text$ - Windows only, simulates keyboard input to focused window
    writeln!(output, "void qb_screenprint(qb_string* text) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    if (!text || !text->data) return;").unwrap();
    writeln!(output, "    INPUT input;").unwrap();
    writeln!(output, "    for (size_t i = 0; i < text->len; i++) {{").unwrap();
    writeln!(output, "        char c = text->data[i];").unwrap();
    writeln!(output, "        SHORT vk = VkKeyScanA(c);").unwrap();
    writeln!(output, "        if (vk == -1) continue;").unwrap();
    writeln!(
        output,
        "        BYTE scancode = (BYTE)MapVirtualKeyA(vk & 0xFF, MAPVK_VK_TO_VSC);"
    )
    .unwrap();
    writeln!(output, "        int shift = (vk >> 8) & 1;").unwrap();
    writeln!(output, "        /* Shift down if needed */").unwrap();
    writeln!(output, "        if (shift) {{").unwrap();
    writeln!(output, "            ZeroMemory(&input, sizeof(INPUT));").unwrap();
    writeln!(output, "            input.type = INPUT_KEYBOARD;").unwrap();
    writeln!(output, "            input.ki.wVk = VK_SHIFT;").unwrap();
    writeln!(output, "            SendInput(1, &input, sizeof(INPUT));").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "        /* Key down */").unwrap();
    writeln!(output, "        ZeroMemory(&input, sizeof(INPUT));").unwrap();
    writeln!(output, "        input.type = INPUT_KEYBOARD;").unwrap();
    writeln!(output, "        input.ki.wVk = vk & 0xFF;").unwrap();
    writeln!(output, "        input.ki.wScan = scancode;").unwrap();
    writeln!(output, "        SendInput(1, &input, sizeof(INPUT));").unwrap();
    writeln!(output, "        /* Key up */").unwrap();
    writeln!(output, "        input.ki.dwFlags = KEYEVENTF_KEYUP;").unwrap();
    writeln!(output, "        SendInput(1, &input, sizeof(INPUT));").unwrap();
    writeln!(output, "        /* Shift up if needed */").unwrap();
    writeln!(output, "        if (shift) {{").unwrap();
    writeln!(output, "            ZeroMemory(&input, sizeof(INPUT));").unwrap();
    writeln!(output, "            input.type = INPUT_KEYBOARD;").unwrap();
    writeln!(output, "            input.ki.wVk = VK_SHIFT;").unwrap();
    writeln!(output, "            input.ki.dwFlags = KEYEVENTF_KEYUP;").unwrap();
    writeln!(output, "            SendInput(1, &input, sizeof(INPUT));").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    (void)text;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SCREENIMAGE([x1, y1, x2, y2]) - Windows only, captures desktop screenshot
    // If all coordinates are 0, captures full screen. Otherwise captures rectangle.
    writeln!(
        output,
        "int32_t qb_screenimage(int32_t x1, int32_t y1, int32_t x2, int32_t y2) {{"
    )
    .unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    HWND hwnd = GetDesktopWindow();").unwrap();
    writeln!(output, "    RECT rect;").unwrap();
    writeln!(output, "    GetWindowRect(hwnd, &rect);").unwrap();
    writeln!(output, "    int w, h;").unwrap();
    writeln!(output, "    /* If all coords are 0, capture full screen */").unwrap();
    writeln!(
        output,
        "    if (x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0) {{"
    )
    .unwrap();
    writeln!(output, "        w = rect.right; h = rect.bottom;").unwrap();
    writeln!(output, "    }} else {{").unwrap();
    writeln!(output, "        if (x1 < 0) x1 = 0;").unwrap();
    writeln!(output, "        if (y1 < 0) y1 = 0;").unwrap();
    writeln!(
        output,
        "        if (x2 > rect.right - 1) x2 = rect.right - 1;"
    )
    .unwrap();
    writeln!(
        output,
        "        if (y2 > rect.bottom - 1) y2 = rect.bottom - 1;"
    )
    .unwrap();
    writeln!(output, "        w = x2 - x1 + 1;").unwrap();
    writeln!(output, "        h = y2 - y1 + 1;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    if (w <= 0 || h <= 0) return -1;").unwrap();
    writeln!(output, "    HDC hdc = GetDC(NULL);").unwrap();
    writeln!(output, "    HDC hdc2 = CreateCompatibleDC(hdc);").unwrap();
    writeln!(
        output,
        "    HBITMAP bitmap = CreateCompatibleBitmap(hdc, w, h);"
    )
    .unwrap();
    writeln!(output, "    SelectObject(hdc2, bitmap);").unwrap();
    writeln!(
        output,
        "    BitBlt(hdc2, 0, 0, w, h, hdc, x1, y1, SRCCOPY);"
    )
    .unwrap();
    writeln!(output, "    /* Create image and copy pixels */").unwrap();
    writeln!(output, "    int32_t img = qb_gfx_newimage(w, h, 32);").unwrap();
    writeln!(output, "    if (img > 0) {{").unwrap();
    writeln!(output, "        BITMAPINFOHEADER bi;").unwrap();
    writeln!(output, "        bi.biSize = sizeof(BITMAPINFOHEADER);").unwrap();
    writeln!(output, "        bi.biWidth = w;").unwrap();
    writeln!(output, "        bi.biHeight = -h; /* Top-down */").unwrap();
    writeln!(output, "        bi.biPlanes = 1;").unwrap();
    writeln!(output, "        bi.biBitCount = 32;").unwrap();
    writeln!(output, "        bi.biCompression = BI_RGB;").unwrap();
    writeln!(output, "        bi.biSizeImage = 0;").unwrap();
    writeln!(output, "        bi.biXPelsPerMeter = 0;").unwrap();
    writeln!(output, "        bi.biYPelsPerMeter = 0;").unwrap();
    writeln!(output, "        bi.biClrUsed = 0;").unwrap();
    writeln!(output, "        bi.biClrImportant = 0;").unwrap();
    writeln!(
        output,
        "        /* Would need _qb_images array access to copy pixels */"
    )
    .unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    DeleteObject(bitmap);").unwrap();
    writeln!(output, "    DeleteDC(hdc2);").unwrap();
    writeln!(output, "    ReleaseDC(NULL, hdc);").unwrap();
    writeln!(output, "    return img;").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    (void)x1; (void)y1; (void)x2; (void)y2;").unwrap();
    writeln!(output, "    return -1; /* Not supported on non-Windows */").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Alpha blending functions
    writeln!(output, "/* Alpha Blending Functions */").unwrap();
    writeln!(
        output,
        "static int _qb_blend_enabled[256] = {{0}};  /* Per-image blend state */"
    )
    .unwrap();
    writeln!(
        output,
        "static int32_t _qb_clear_color[256] = {{0}};  /* Per-image clear color */"
    )
    .unwrap();
    writeln!(
        output,
        "static int _qb_clear_color_set[256] = {{0}};  /* Whether clear color is set */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Initialize all images to blend enabled
    writeln!(output, "static void _qb_init_blend(void) {{").unwrap();
    writeln!(output, "    static int initialized = 0;").unwrap();
    writeln!(output, "    if (!initialized) {{").unwrap();
    writeln!(
        output,
        "        for (int i = 0; i < 256; i++) _qb_blend_enabled[i] = 1;"
    )
    .unwrap();
    writeln!(output, "        initialized = 1;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_blend(int32_t handle) {{").unwrap();
    writeln!(output, "    _qb_init_blend();").unwrap();
    writeln!(
        output,
        "    if (handle >= 0 && handle < 256) _qb_blend_enabled[handle] = 1;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_dontblend(int32_t handle) {{").unwrap();
    writeln!(output, "    _qb_init_blend();").unwrap();
    writeln!(
        output,
        "    if (handle >= 0 && handle < 256) _qb_blend_enabled[handle] = 0;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_clearcolor(uint32_t color, int32_t handle) {{"
    )
    .unwrap();
    writeln!(output, "    if (handle >= 0 && handle < 256) {{").unwrap();
    writeln!(output, "        _qb_clear_color[handle] = (int32_t)color;").unwrap();
    writeln!(output, "        _qb_clear_color_set[handle] = 1;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_clearcolor_none(int32_t handle) {{").unwrap();
    writeln!(
        output,
        "    if (handle >= 0 && handle < 256) _qb_clear_color_set[handle] = 0;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "int64_t qb_clearcolor_get(int32_t handle) {{").unwrap();
    writeln!(
        output,
        "    if (handle >= 0 && handle < 256 && _qb_clear_color_set[handle])"
    )
    .unwrap();
    writeln!(
        output,
        "        return (int64_t)(uint32_t)_qb_clear_color[handle];"
    )
    .unwrap();
    writeln!(output, "    return -1;  /* No clear color set */").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Palette operations
    writeln!(output, "/* Palette Operations */").unwrap();
    writeln!(
        output,
        "static uint32_t _qb_palettes[256][256];  /* Per-image palettes */"
    )
    .unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_copypalette(int32_t src_handle, int32_t dest_handle) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    if (src_handle < 0 || src_handle >= 256) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    if (dest_handle < 0 || dest_handle >= 256) return;"
    )
    .unwrap();
    writeln!(
        output,
        "    memcpy(_qb_palettes[dest_handle], _qb_palettes[src_handle], sizeof(_qb_palettes[0]));"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Display layer ordering
    writeln!(output, "/* Display Layer Ordering */").unwrap();
    writeln!(
        output,
        "static int32_t _qb_display_order[4] = {{1, 2, 3, 4}};  /* Layer order */"
    )
    .unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "void qb_displayorder(int32_t layer1, int32_t layer2, int32_t layer3, int32_t layer4) {{"
    )
    .unwrap();
    writeln!(output, "    _qb_display_order[0] = layer1;").unwrap();
    writeln!(output, "    _qb_display_order[1] = layer2;").unwrap();
    writeln!(output, "    _qb_display_order[2] = layer3;").unwrap();
    writeln!(output, "    _qb_display_order[3] = layer4;").unwrap();
    writeln!(
        output,
        "    /* Note: Layer ordering affects compositing in full runtime */"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // Dialog boxes
    writeln!(output, "/* Dialog Box Stubs */").unwrap();
    writeln!(
        output,
        "/* Full implementations require platform-specific GUI libraries */"
    )
    .unwrap();
    writeln!(output).unwrap();

    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(
        output,
        "int64_t qb_messagebox(qb_string* title, qb_string* msg) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    return MessageBoxA(NULL, msg ? msg->data : \"\", title ? title->data : \"\", MB_OK);"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(
        output,
        "int64_t qb_messagebox(qb_string* title, qb_string* msg) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    printf(\"[%s] %s\\n\", title ? title->data : \"\", msg ? msg->data : \"\");"
    )
    .unwrap();
    writeln!(output, "    return 1;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "qb_string* qb_inputbox(qb_string* prompt, qb_string* title) {{"
    )
    .unwrap();
    writeln!(output, "    (void)title;").unwrap();
    writeln!(output, "    char buf[1024];").unwrap();
    writeln!(output, "    printf(\"%s \", prompt ? prompt->data : \"\");").unwrap();
    writeln!(output, "    if (fgets(buf, sizeof(buf), stdin)) {{").unwrap();
    writeln!(output, "        size_t len = strlen(buf);").unwrap();
    writeln!(
        output,
        "        if (len > 0 && buf[len-1] == '\\n') buf[len-1] = '\\0';"
    )
    .unwrap();
    writeln!(output, "        return qb_string_new(buf);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    return qb_string_new(\"\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // File dialogs return empty string (no GUI support in console mode)
    writeln!(
        output,
        "qb_string* qb_openfiledialog(qb_string* title, qb_string* filter) {{"
    )
    .unwrap();
    writeln!(output, "    (void)title; (void)filter;").unwrap();
    writeln!(output, "    fprintf(stderr, \"Note: _OPENFILEDIALOG$ requires external runtime for GUI support\\n\");").unwrap();
    writeln!(output, "    return qb_string_new(\"\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "qb_string* qb_savefiledialog(qb_string* title, qb_string* filter) {{"
    )
    .unwrap();
    writeln!(output, "    (void)title; (void)filter;").unwrap();
    writeln!(output, "    fprintf(stderr, \"Note: _SAVEFILEDIALOG$ requires external runtime for GUI support\\n\");").unwrap();
    writeln!(output, "    return qb_string_new(\"\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(
        output,
        "qb_string* qb_selectfolderdialog(qb_string* title) {{"
    )
    .unwrap();
    writeln!(output, "    (void)title;").unwrap();
    writeln!(output, "    fprintf(stderr, \"Note: _SELECTFOLDERDIALOG$ requires external runtime for GUI support\\n\");").unwrap();
    writeln!(output, "    return qb_string_new(\"\");").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _MAPTRIANGLE - Triangle texture mapping (stub for inline runtime)
    writeln!(output, "/* Triangle Mapping */").unwrap();
    writeln!(output, "void qb_maptriangle(double sx1, double sy1, double sx2, double sy2, double sx3, double sy3,").unwrap();
    writeln!(output, "                    double dx1, double dy1, double dx2, double dy2, double dx3, double dy3) {{").unwrap();
    writeln!(output, "    _qb_gfx_warn();").unwrap();
    writeln!(
        output,
        "    (void)sx1; (void)sy1; (void)sx2; (void)sy2; (void)sx3; (void)sy3;"
    )
    .unwrap();
    writeln!(
        output,
        "    (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)dx3; (void)dy3;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    writeln!(output, "void qb_maptriangle_ex(double sx1, double sy1, double sx2, double sy2, double sx3, double sy3,").unwrap();
    writeln!(output, "                       double dx1, double dy1, double dx2, double dy2, double dx3, double dy3,").unwrap();
    writeln!(output, "                       int32_t src_handle, int32_t dest_handle, int32_t smooth, int32_t seamless) {{").unwrap();
    writeln!(output, "    _qb_gfx_warn();").unwrap();
    writeln!(
        output,
        "    (void)sx1; (void)sy1; (void)sx2; (void)sy2; (void)sx3; (void)sy3;"
    )
    .unwrap();
    writeln!(
        output,
        "    (void)dx1; (void)dy1; (void)dx2; (void)dy2; (void)dx3; (void)dy3;"
    )
    .unwrap();
    writeln!(
        output,
        "    (void)src_handle; (void)dest_handle; (void)smooth; (void)seamless;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
