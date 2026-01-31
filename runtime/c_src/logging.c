/*
 * Variadic wrappers for libqb_log / libqb_log_qb64 when using external runtime.
 * The Rust library provides libqb_log_init, qb_log_message, libqb_log_qbs.
 * This file provides libqb_log and libqb_log_qb64 (printf-style) by formatting
 * and calling qb_log_message.
 *
 * When linking with external runtime, compile and link this file:
 *   gcc -I runtime/include program.c runtime/c_src/logging.c -L. -lqb64fresh_rt -o program
 */

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "qb64fresh_rt.h"

#define LOG_BUF_SIZE 1024

void libqb_log(int32_t lvl, int32_t scope, const char *file, const char *func, int line,
               const char *fmt, ...) {
    char buf[LOG_BUF_SIZE];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt ? fmt : "", ap);
    va_end(ap);
    qb_log_message(lvl, scope, file, func, line, buf);
}

void libqb_log_qb64(int32_t lvl, int32_t scope, const char *file, const char *func, int line,
                    const char *fmt, ...) {
    char buf[LOG_BUF_SIZE];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt ? fmt : "", ap);
    va_end(ap);
    qb_log_message(lvl, scope, file, func, line, buf);
}
