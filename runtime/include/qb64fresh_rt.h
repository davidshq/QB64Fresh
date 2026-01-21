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
void qb_input_string(const char* prompt, QbString** var);
void qb_input_int(const char* prompt, int32_t* var);
void qb_input_long(const char* prompt, int64_t* var);
void qb_input_float(const char* prompt, double* var);
void qb_input_single(const char* prompt, float* var);
void qb_line_input(const char* prompt, QbString** var);

/* Console functions */
void qb_cls(void);
void qb_locate(int32_t row, int32_t col);
void qb_color(int32_t foreground, int32_t background);
void qb_color_reset(void);

/* Keyboard */
QbString* qb_inkey(void);

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

/* ============================================================================
 * File I/O Functions
 * ============================================================================ */

/* File open/close */
void qb_file_open(int32_t fnum, const char* filename, const char* mode);
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
void qb_file_put(int32_t fnum, const void* data, size_t size);

/* File status functions */
int32_t qb_eof(int32_t fnum);
int64_t qb_lof(int32_t fnum);
int64_t qb_loc(int32_t fnum);
int32_t qb_freefile(void);

/* FIELD statement support */
void qb_field_start(int32_t fnum);
void qb_field_add(int32_t width, QbString** var);
void qb_lset(QbString** var, QbString* value);
void qb_rset(QbString** var, QbString* value);

/* File system operations */
int32_t qb_file_kill(const char* filename);
int32_t qb_file_rename(const char* old_name, const char* new_name);
int32_t qb_file_exists(const char* path);

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

#ifdef __cplusplus
}
#endif

#endif /* QB64FRESH_RT_H */
