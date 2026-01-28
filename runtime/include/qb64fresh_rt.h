/*
 * QB64Fresh Runtime Library - C Header
 *
 * This header declares the C interface to the QB64Fresh runtime library.
 * Include this in generated C code and link against libqb64fresh_rt.a
 *
 * Usage:
 *   gcc -I path/to/include program.c -L path/to/lib -lqb64fresh_rt -o program
 */

#ifndef QB64FRESH_RT_H
#define QB64FRESH_RT_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * String Type
 * ============================================================================
 * QbString is an opaque pointer to a reference-counted string.
 * Always use qb_string_release() when done with a string.
 */

typedef struct QbString QbString;

/* String creation */
QbString* qb_string_new(const char* s);
QbString* qb_string_empty(void);
QbString* qb_string_from_bytes(const uint8_t* data, size_t len);

/* Reference counting */
QbString* qb_string_retain(QbString* s);
void qb_string_release(QbString* s);

/* String access */
size_t qb_string_len(const QbString* s);
const char* qb_string_data(const QbString* s);

/* String operations */
QbString* qb_string_concat(const QbString* a, const QbString* b);
int qb_string_compare(const QbString* a, const QbString* b);

/* BASIC string functions */
QbString* qb_chr(int32_t code);
int32_t qb_asc(const QbString* s);
QbString* qb_left(const QbString* s, int32_t n);
QbString* qb_right(const QbString* s, int32_t n);
QbString* qb_mid(const QbString* s, int32_t start, int32_t length);
int32_t qb_instr(int32_t start, const QbString* haystack, const QbString* needle);
QbString* qb_ucase(const QbString* s);
QbString* qb_lcase(const QbString* s);
QbString* qb_ltrim(const QbString* s);
QbString* qb_rtrim(const QbString* s);
QbString* qb_space(int32_t n);
QbString* qb_string_fill(int32_t n, int32_t char_code);

/* String/number conversion */
QbString* qb_str_int(int64_t n);
QbString* qb_str_float(double n);
double qb_val(const QbString* s);

/* _TOSTR$, _IIF, _IIF$ (for --runtime external) */
QbString* qb_tostr(double n);
double qb_iif(int64_t cond, double true_val, double false_val);
QbString* qb_iif_str(int64_t cond, QbString* true_val, QbString* false_val);

/* String conversion: HEX$, OCT$, _BIN$, TRIM$, _INSTRREV */
QbString* qb_hex(int64_t n);
QbString* qb_oct(int64_t n);
QbString* qb_bin(int64_t n);
QbString* qb_trim(const QbString* s);
int32_t qb_instrrev(const QbString* source, const QbString* search);
int32_t qb_instrrev3(const QbString* s, const QbString* sub, int32_t start);

/* String conversion from C string (for fixed-length strings) */
QbString* qb_str_from_c(const char* s);

/* ============================================================================
 * Memory Operations (_MEMNEW, _MEMFREE, _MEMGET, _MEMPUT, _MEMCOPY, _MEMFILL,
 *                    _MEM, _MEMEXISTS, _MEMELEMENT, _MEMIMAGE, _MEMSOUND, _OFFSET)
 * ============================================================================
 * Requires <stdint.h> (included at top) for intptr_t.
 */
typedef struct qb_mem {
    void* offset;
    intptr_t size;
    intptr_t type;
    intptr_t elementsize;
    int32_t image;
    int32_t sound;
} qb_mem;

qb_mem qb_memnew(intptr_t size);
void qb_memfree(qb_mem* m);
int64_t qb_memget(qb_mem m, intptr_t byteoffset);
void qb_memput(qb_mem m, intptr_t byteoffset, int64_t value);
void qb_memcopy(qb_mem src, intptr_t src_offset, intptr_t bytes, qb_mem dest, intptr_t dest_offset);
void qb_memfill(qb_mem m, intptr_t byteoffset, intptr_t bytes, int32_t value);
intptr_t qb_offset(void* ptr);
qb_mem qb_mem_of(void* ptr, intptr_t size);
int32_t qb_memexists(qb_mem m);
qb_mem qb_memelement(qb_mem m, intptr_t index);
qb_mem qb_memimage(int32_t handle);
qb_mem qb_memsound(int32_t handle);

/* ============================================================================
 * I/O Functions
 * ============================================================================ */

/* Print functions */
void qb_print_int(int64_t n);
void qb_print_float(double n);
void qb_print_string(const QbString* s);
void qb_print_newline(void);
void qb_print_tab(void);
void qb_print_space(void);
void qb_print_flush(void);

/* Input functions */
void qb_input_string(const char* prompt, QbString** var, int same_line);
void qb_input_int(const char* prompt, int32_t* var, int same_line);
void qb_input_long(const char* prompt, int64_t* var);
void qb_input_float(const char* prompt, double* var, int same_line);
void qb_input_single(const char* prompt, float* var);
void qb_line_input(const char* prompt, QbString** var);

/* Console functions */
void qb_cls(void);
void qb_locate(int32_t row, int32_t col);
void qb_color(int32_t foreground, int32_t background);
void qb_color_reset(void);
void qb_echo(const QbString* text);

/* Keyboard */
QbString* qb_inkey(void);
int64_t qb_keyhit(void);
int32_t qb_keydown(int64_t keycode);
void qb_keyclear(void);
void qb_keyboard_shutdown(void);

/* ============================================================================
 * Math Functions
 * ============================================================================ */

/* Basic math */
int64_t qb_abs_int(int64_t n);
double qb_abs_float(double n);
int32_t qb_sgn_int(int64_t n);
int32_t qb_sgn_float(double n);
double qb_int(double n);
double qb_fix(double n);
int16_t qb_cint(double n);
int32_t qb_clng(double n);

/* Trigonometric functions */
double qb_sin(double n);
double qb_cos(double n);
double qb_tan(double n);
double qb_atn(double n);
double qb_asin(double n);
double qb_acos(double n);
double qb_sinh(double n);
double qb_cosh(double n);
double qb_tanh(double n);

/* Exponential and logarithmic */
double qb_sqr(double n);
double qb_log(double n);
double qb_log10(double n);
double qb_exp(double n);
double qb_pow(double base, double exp);
double qb_pow_int(double base, int32_t exp);

/* Random numbers */
void qb_randomize(double seed);
void qb_randomize_timer(void);
double qb_rnd(double n);

/* Conversions */
double qb_d2r(double degrees);
double qb_r2d(double radians);

/* Constants */
double qb_pi(void);
double qb_e(void);

/* Min/Max */
int64_t qb_min_int(int64_t a, int64_t b);
int64_t qb_max_int(int64_t a, int64_t b);
double qb_min_float(double a, double b);
double qb_max_float(double a, double b);

/* Timer */
double qb_timer(void);
void qb_sleep(double seconds);
void qb_delay(double seconds);

/* Date/Time functions */
QbString* qb_date(void);        /* DATE$ - returns MM-DD-YYYY format */
QbString* qb_time(void);        /* TIME$ - returns HH:MM:SS format */
QbString* qb_date64(void);      /* _DATE$ - returns YYYY-MM-DD format */
QbString* qb_time64(void);      /* _TIME$ - returns HH:MM:SS format */
QbString* qb_cwd(void);         /* _CWD$ - current working directory */
QbString* qb_startdir(void);    /* _STARTDIR$ - program start directory */
QbString* qb_os(void);          /* _OS$ - operating system string */

/* ============================================================================
 * File I/O Functions
 * ============================================================================ */

/* File open/close */
void qb_file_open(int32_t fnum, const char* filename, const char* mode);
void qb_file_open_str(int32_t fnum, const QbString* filename, const char* mode);  /* Helper: qb_file_open with QbString* */
void qb_file_set_reclen(int32_t fnum, int32_t len);
void qb_file_close(int32_t fnum);
void qb_file_close_all(void);

/* Sequential output (PRINT #) */
void qb_file_print_int(int32_t fnum, int64_t val);
void qb_file_print_float(int32_t fnum, double val);
void qb_file_print_string(int32_t fnum, QbString* s);
void qb_file_print_newline(int32_t fnum);
void qb_file_print_tab(int32_t fnum);

/* Sequential output (WRITE #) */
void qb_file_write_string(int32_t fnum, QbString* s);
void qb_file_write_number(int32_t fnum, double val);
void qb_file_write_char(int32_t fnum, char c);

/* Sequential input (INPUT #) */
void qb_file_input_string(int32_t fnum, QbString** s);
void qb_file_input_int(int32_t fnum, int32_t* val);
void qb_file_input_float(int32_t fnum, double* val);
void qb_file_line_input(int32_t fnum, QbString** s);

/* Random/binary access */
void qb_file_seek(int32_t fnum, int64_t pos);
void qb_file_seek_record(int32_t fnum, int64_t rec);
void qb_file_get(int32_t fnum, void* data, size_t size);
void qb_file_get_string(int32_t fnum, QbString* s);
void qb_file_put(int32_t fnum, const void* data, size_t size);
void qb_file_put_string(int32_t fnum, const QbString* s);

/* File status functions */
int32_t qb_eof(int32_t fnum);
int64_t qb_lof(int32_t fnum);
int64_t qb_loc(int32_t fnum);
int32_t qb_freefile(void);

/* System/shell functions */
int32_t qb_shell(const char* command);  /* SHELL statement - execute command */

/* FIELD statement support */
void qb_field_start(int32_t fnum);
void qb_field_add(int32_t width, QbString** var);
void qb_lset(QbString** var, QbString* value);
void qb_rset(QbString** var, QbString* value);

/* File system operations */
int32_t qb_file_kill(const char* filename);
int32_t qb_file_rename(const char* old_name, const char* new_name);
int32_t qb_file_exists(const QbString* path);  /* _FILEEXISTS - accepts QbString* */

/* Directory operations (CHDIR, MKDIR, RMDIR, _DIREXISTS, DIR$) */
int32_t qb_chdir(const char* path);
int32_t qb_mkdir(const char* path);
int32_t qb_rmdir(const char* path);
int32_t qb_dir_exists(const QbString* path);
QbString* qb_dir(const QbString* spec);  /* DIR$ - directory listing function */

/* Environment operations (ENVIRON statement) */
void qb_sub_environ(QbString* env);

/* ============================================================================
 * Networking (_OPENHOST, _OPENCONNECTION, _OPENCLIENT, _CONNECTED, _CLOSEHOST,
 *             GET #/PUT #, EOF, LOF on network handles)
 * ============================================================================ */

int64_t qb_net_openhost(int64_t port);
int64_t qb_net_openconnection(int64_t host_handle);
int64_t qb_net_openclient(const char* connection_string);
int32_t qb_net_connected(int64_t handle);
void qb_net_close(int64_t handle);
size_t qb_net_get(int64_t handle, uint8_t* data, size_t size);
size_t qb_net_put(int64_t handle, const uint8_t* data, size_t size);
size_t qb_net_get_string(int64_t handle, QbString** s);
size_t qb_net_put_string(int64_t handle, const QbString* s);
int32_t qb_net_eof(int64_t handle);
int64_t qb_net_lof(int64_t handle);

/* ============================================================================
 * Dialog Functions
 * ============================================================================ */

/* File dialogs */
QbString* qb_openfiledialog(const char* title, const char* initial_dir, const char* filter);
QbString* qb_savefiledialog(const char* title, const char* initial_dir, const char* default_name, const char* filter);
QbString* qb_selectfolderdialog(const char* title, const char* initial_dir);

/* Message box button types */
#define QB_MB_OK              0
#define QB_MB_OKCANCEL        1
#define QB_MB_ABORTRETRYIGNORE 2
#define QB_MB_YESNOCANCEL     3
#define QB_MB_YESNO           4
#define QB_MB_RETRYCANCEL     5

/* Message box return values */
#define QB_MBRET_OK           1
#define QB_MBRET_CANCEL       2
#define QB_MBRET_ABORT        3
#define QB_MBRET_RETRY        4
#define QB_MBRET_IGNORE       5
#define QB_MBRET_YES          6
#define QB_MBRET_NO           7

/* Message box */
int32_t qb_messagebox_ex(const char* title, const char* message, int32_t buttons);

/* ============================================================================
 * Joystick/Gamepad Functions
 * ============================================================================ */

/* Classic BASIC joystick functions */
int32_t qb_stick(int32_t axis);
int32_t qb_strig(int32_t button);
int32_t qb_strig2(int32_t button, int32_t controller);  /* QB64 extension */

/* STRIG event handling */
void qb_on_strig(int32_t button_num, uint32_t event_id);  /* ON STRIG statement */
void qb_strig_control(int32_t button_num, int32_t mode);   /* STRIG(n) ON|OFF|STOP */
uint32_t qb_strig_check_event(void);  /* Check for pending STRIG event, returns event_id or 0 */
void qb_strig_event_done(void);  /* Mark STRIG event as handled */

/* QB64 input device extensions */
int32_t qb_devices(void);
double qb_axis(int32_t device, int32_t axis);
int32_t qb_button(int32_t device, int32_t button);

/* ============================================================================
 * Runtime Control
 * ============================================================================ */

void qb_runtime_init(void);
void qb_runtime_shutdown(void);
void qb_end(int32_t exit_code);
void qb_stop(void);

/* Initialization functions called at program start */
void qb_init_args(int argc, char** argv);
void qb_init_startdir(void);
void _qb_init_palette(void);

/* Error handling functions */
int32_t qb_err_code(void);          /* ERR function - returns error code */
int32_t qb_err_line(void);          /* ERL function - returns error line */
int64_t qb_errorline(void);         /* _ERRORLINE - returns error line as 64-bit */
QbString* qb_errormessage(void);   /* _ERRORMESSAGE$ - returns error message */
int32_t qb_inclerrorline(void);    /* _INCLERRORLINE - error line in include file */
QbString* qb_inclerrorfile(void);  /* _INCLERRORFILE$ - include file with error */

/* Compatibility macros for inline runtime naming conventions */
#define qb__rgb32(r, g, b) qb_rgb(r, g, b)
#define qb__rgb32_4(r, g, b, a) qb_rgba(r, g, b, a)
#define qb__rgba32(r, g, b, a) qb_rgba(r, g, b, a)

/* ============================================================================
 * Graphics Functions
 * ============================================================================ */

/* Initialization and shutdown */
int qb_gfx_init(uint32_t width, uint32_t height);
int qb_gfx_shutdown(void);
int qb_gfx_screen(int32_t mode, int32_t color_switch, int32_t active_page, int32_t visual_page);

/* Screen operations */
int qb_gfx_cls(void);
int qb_gfx_color(uint32_t foreground, uint32_t background);
uint32_t qb_gfx_get_foreground(void);
uint32_t qb_gfx_get_background(void);
int qb_gfx_locate(uint32_t row, uint32_t col);
uint32_t qb_gfx_csrlin(void);
uint32_t qb_gfx_pos(void);
int qb_gfx_print(const char* text);
int qb_gfx_display(void);
int qb_gfx_autodisplay(int enabled);
uint32_t qb_gfx_width(void);
uint32_t qb_gfx_height(void);

/* Drawing primitives */
int qb_gfx_pset(int32_t x, int32_t y, uint32_t color);
int qb_gfx_pset_step(int32_t x, int32_t y, uint32_t color, int step);
uint32_t qb_gfx_point(int32_t x, int32_t y);
int qb_gfx_line(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color);
int qb_gfx_line_step(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int step1, int step2, uint16_t style);
int qb_gfx_box(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int filled);
int qb_gfx_box_step(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint32_t color, int filled, int step1, int step2, uint16_t style);
int qb_gfx_circle(int32_t x, int32_t y, int32_t radius, uint32_t color, int filled);
int qb_gfx_circle_step(int32_t x, int32_t y, int32_t radius, uint32_t color, int filled, int step);
int qb_gfx_paint(int32_t x, int32_t y, uint32_t color, int32_t boundary_color);
int qb_gfx_paint_step(int32_t x, int32_t y, uint32_t color, int32_t boundary_color, int step);
int qb_gfx_draw(const char* commands);

/* Palette functions */
int qb_gfx_palette(int32_t index, uint32_t color);
int qb_gfx_palette_reset(void);
uint32_t qb_gfx_palette_get(int32_t index);

/* _PALETTECOLOR - get/set palette entry */
/* Function form: _PALETTECOLOR(attribute%[, imgHandle&]) - returns color */
/* Statement form: _PALETTECOLOR attribute%, color&[, imgHandle&] - sets color */
int32_t qb_palettecolor(int32_t attribute, int32_t color_or_handle, int32_t handle);
int32_t qb_palettecolor_get(int32_t attribute, int32_t handle);

/* Color functions */
uint32_t qb_rgb(uint32_t r, uint32_t g, uint32_t b);
uint32_t qb_rgb32(uint32_t r, uint32_t g, uint32_t b);
uint32_t qb_rgba(uint32_t r, uint32_t g, uint32_t b, uint32_t a);
uint32_t qb_rgba32(uint32_t r, uint32_t g, uint32_t b, uint32_t a);

/* Viewport and coordinate mapping */
int qb_gfx_view(int screen, int32_t x1, int32_t y1, int32_t x2, int32_t y2, int32_t fill_color, int32_t border_color);
int qb_gfx_view_reset(void);
int qb_gfx_window(int screen, double x1, double y1, double x2, double y2);
int qb_gfx_window_reset(void);
double qb_gfx_pmap(double coord, int32_t func_code);
int qb_gfx_set_width(uint32_t columns, uint32_t rows);
int qb_gfx_pcopy(int32_t src, int32_t dst);

/* Event handling */
int qb_gfx_poll_events(void);

/* Image functions */
int32_t qb_gfx_newimage(int32_t width, int32_t height, int32_t mode);
int32_t qb_gfx_loadimage(const char* filename, int32_t mode);
int qb_gfx_freeimage(int32_t handle);
int qb_gfx_putimage_simple(int32_t src_handle, int32_t dest_handle);
int qb_gfx_putimage(int32_t dx1, int32_t dy1, int32_t dx2, int32_t dy2, int32_t src_handle, int32_t dest_handle);
int qb_gfx_putimage_full(int32_t dx1, int32_t dy1, int32_t dx2, int32_t dy2, int32_t src_handle, int32_t dest_handle, int32_t sx1, int32_t sy1, int32_t sx2, int32_t sy2);
int qb_gfx_source(int32_t handle);
int qb_gfx_dest(int32_t handle);
int qb_gfx_printstring(int32_t x, int32_t y, const char* text);
int32_t qb_gfx_image_width(int32_t handle);
int32_t qb_gfx_image_height(int32_t handle);

/* GET/PUT graphics array operations */
int qb_gfx_get(int32_t x1, int32_t y1, int32_t x2, int32_t y2, uint8_t* arr);
int qb_gfx_get_step(int32_t x1, int32_t y1, int32_t w, int32_t h, uint8_t* arr);
int qb_gfx_put(int32_t x, int32_t y, const uint8_t* arr, int action, int clip, int32_t trans_color);
int qb_gfx_put_step(int32_t x, int32_t y, const uint8_t* arr, int action, int clip, int32_t trans_color);

/* PUT action constants */
#define QB_PUT_XOR      0
#define QB_PUT_PSET     1
#define QB_PUT_PRESET   2
#define QB_PUT_AND      3
#define QB_PUT_OR       4

/* Mouse input functions */
int32_t qb_mouse_x(void);
int32_t qb_mouse_y(void);
int32_t qb_mouse_button(int32_t button);
int32_t qb_mouse_input(void);
int32_t qb_mouse_movement_x(void);
int32_t qb_mouse_movement_y(void);
int32_t qb_mouse_wheel(void);
void qb_mouse_hide(void);
void qb_mouse_show(void);
void qb_mouse_move(int32_t x, int32_t y);

/* Clipboard functions */
QbString* qb_clipboard_get(void);
void qb_clipboard_set(const char* text);

/* Font functions */
int64_t qb_loadfont(const char* path, int32_t size, int32_t options);
int64_t qb_loadfont_qb(const QbString* path, int64_t size, int32_t options);
int64_t qb_font(int64_t handle);
void qb_freefont(int64_t handle);
int64_t qb_fontheight(void);
int64_t qb_fontwidth(void);
int64_t qb_printwidth(const QbString* text);
int64_t qb_font_get(void);

/* Unicode font functions (FreeType-based) */
void qb_uprintstring(int64_t x, int64_t y, const QbString* text);
int64_t qb_uprintwidth(const QbString* text);
int64_t qb_ufontheight(int64_t handle);
int64_t qb_ulinespacing(void);
int64_t qb_ucharpos(const QbString* text, int64_t pos);

/* Font loading option flags */
#define QB_FONT_DONTBLEND   8   /* No anti-aliasing (1-bit rendering) */
#define QB_FONT_MONOSPACE   16  /* Force monospace width */
#define QB_FONT_UNICODE     32  /* UTF-8 input mode */
#define QB_FONT_AUTOMONO    64  /* Auto-detect monospace */

/* Window control functions */
int32_t qb_fullscreen(int32_t mode);
int32_t qb_fullscreen_get(void);
void qb_screenmove(int32_t x, int32_t y);
void qb_screenshow(void);
void qb_screenhide(void);
void qb_sub__title(const QbString* title);  /* _TITLE statement */
int32_t qb_icon(void);                   /* _ICON function (get current icon) */
int32_t qb_icon1(int32_t handle);        /* _ICON statement (set icon from handle) */

/* Windows-only desktop functions */
int64_t qb_windowhandle(void);       /* Returns HWND on Windows, 0 on other platforms */
void qb_screenclick(int32_t x, int32_t y, int32_t button);  /* Simulate mouse click */
void qb_screenprint(const char* text);  /* Simulate keyboard input */
int32_t qb_screenimage(int32_t x1, int32_t y1, int32_t x2, int32_t y2);  /* Desktop screenshot */

/* Alpha blending functions */
void qb_blend(int32_t handle);
void qb_dontblend(int32_t handle);
void qb_clearcolor(uint32_t color, int32_t handle);
void qb_clearcolor_none(int32_t handle);
int64_t qb_clearcolor_get(int32_t handle);

/* Palette operations */
void qb_copypalette(int32_t src_handle, int32_t dest_handle);

/* Display layer ordering */
void qb_displayorder(int32_t layer1, int32_t layer2, int32_t layer3, int32_t layer4);

/* Triangle mapping (_MAPTRIANGLE) */
void qb_maptriangle(double sx1, double sy1, double sx2, double sy2, double sx3, double sy3,
                    double dx1, double dy1, double dx2, double dy2, double dx3, double dy3);
void qb_maptriangle_ex(double sx1, double sy1, double sx2, double sy2, double sx3, double sy3,
                       double dx1, double dy1, double dx2, double dy2, double dx3, double dy3,
                       int32_t src_handle, int32_t dest_handle, int32_t smooth, int32_t seamless);

/* OpenGL stubs (_GLRENDER, _GLCOMPAT) - no-op; raw _GL* excluded per ADR-0014 */
void qb_glrender(int32_t mode);
int32_t qb_glcompat(void);

/* ============================================================================
 * Audio Functions
 * ============================================================================ */

/* Initialization and shutdown */
int qb_audio_init(void);
int qb_audio_shutdown(void);

/* Classic BASIC sound */
int qb_beep(void);
int qb_sound(double frequency, double duration);
int qb_play(const char* commands);

/* QB64 _SND* functions - file operations */
int32_t qb_sndopen(const char* filename);
int qb_sndclose(int32_t handle);
int qb_sndplay(int32_t handle);
int qb_sndstop(int32_t handle);
int qb_sndpause(int32_t handle);
int qb_sndresume(int32_t handle);
int qb_sndloop(int32_t handle);

/* Sound control */
int qb_sndvol(int32_t handle, double volume);
int qb_sndbal(int32_t handle, double balance);

/* Sound queries */
double qb_sndlen(int32_t handle);
double qb_sndgetpos(int32_t handle);
int qb_sndsetpos(int32_t handle, double position);
int qb_sndplaying(int32_t handle);
int qb_sndpaused(int32_t handle);
int32_t qb_sndrate(void);

/* Sound copying and direct playback */
int32_t qb_sndcopy(int32_t handle);
int qb_sndplayfile(const char* filename, int sync);
int qb_sndplaycopy(int32_t handle);

/* Raw audio synthesis */
int32_t qb_sndopenraw(void);
int qb_sndraw(double sample);
int qb_sndraw_stereo(double left, double right);
double qb_sndrawlen(void);

/* ============================================================================
 * System Interrupt Emulation
 * ============================================================================ */

/* INTERRUPT/INTERRUPTX - DOS interrupt emulation (INT 0x33 mouse supported)
 *
 * RegType structure (16 bytes): AX, BX, CX, DX, BP, SI, DI, FLAGS
 * RegTypeX structure (20 bytes): AX, BX, CX, DX, BP, SI, DI, FLAGS, DS, ES
 *
 * Supported: INT 0x33 (mouse) with subfunctions:
 *   AX=0: Check mouse installed -> AX=0xFFFF, BX=2
 *   AX=1: Show cursor
 *   AX=2: Hide cursor
 *   AX=3: Get position -> BX=buttons, CX=X, DX=Y
 */
void qb_interrupt(int32_t int_num, const int16_t* in_regs, int16_t* out_regs);
void qb_interruptx(int32_t int_num, const int16_t* in_regs, int16_t* out_regs);

#ifdef __cplusplus
}
#endif

#endif /* QB64FRESH_RT_H */
