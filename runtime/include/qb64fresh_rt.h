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
