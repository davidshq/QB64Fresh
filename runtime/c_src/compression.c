/*
 * QB64Fresh inline runtime: real _DEFLATE$/_INFLATE$ using zlib.
 *
 * Compile with -DQB64FRESH_COMPRESSION_EXTERNAL and link this file with -lz
 * so that the emitted C does not define qb_deflate/qb_inflate (stubs are
 * disabled) and these implementations are used instead.
 *
 * Build example:
 *   gcc -c -I../include -DQB64FRESH_COMPRESSION_EXTERNAL compression.c -o compression.o
 *   gcc -o prog prog.c compression.o -I../include -DQB64FRESH_COMPRESSION_EXTERNAL -lz -lm
 *
 * Requires: zlib (compress2, uncompress). Raw DEFLATE (no zlib wrapper) to match
 * runtime miniz_oxide behavior; zlib's compress2 uses zlib wrapper by default.
 * We use compress2(..., Z_NO_COMPRESSION) for raw deflate or compress2 with
 * Z_DEFAULT_COMPRESSION and document that output format may differ from
 * miniz_oxide raw deflate. For compatibility we use raw deflate via deflateInit2
 * with -MAX_WBITS so no zlib header/footer.
 */

#include "../include/qb64fresh_rt.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef QB64FRESH_COMPRESSION_EXTERNAL
#include <zlib.h>

#ifndef ZLIB_CONST
# define ZLIB_CONST
#endif

#define QB_INFLATE_MAX_OUT (64 * 1024 * 1024) /* 64 MiB limit like runtime */

QbString* qb_deflate(const QbString* data) {
    if (!data) return qb_string_empty();
    size_t len = qb_string_len(data);
    if (len == 0) return qb_string_empty();
    const uint8_t* src = (const uint8_t*)qb_string_data(data);
    if (!src) return qb_string_empty();

    z_stream strm;
    memset(&strm, 0, sizeof(strm));
    if (deflateInit2(&strm, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY) != Z_OK)
        return qb_string_empty();

    strm.next_in = (ZLIB_CONST Bytef*)src;
    strm.avail_in = (uInt)(len > 0xFFFFFFFFu ? 0xFFFFFFFFu : len);

    size_t cap = len + (len >> 4) + 64;
    if (cap < 256) cap = 256;
    Bytef* buf = (Bytef*)malloc(cap);
    if (!buf) { deflateEnd(&strm); return qb_string_empty(); }

    strm.next_out = buf;
    strm.avail_out = (uInt)(cap > 0xFFFFFFFFu ? 0xFFFFFFFFu : cap);

    for (;;) {
        int ret = deflate(&strm, Z_FINISH);
        if (ret == Z_STREAM_END) break;
        /* Need more output space: grow buffer and continue until Z_STREAM_END */
        if (ret == Z_BUF_ERROR || (ret == Z_OK && strm.avail_out == 0)) {
            size_t used = (size_t)(strm.next_out - buf);
            size_t add = (cap >= 4096u) ? cap : 4096u;
            Bytef* new_buf = (Bytef*)realloc(buf, cap + add);
            if (!new_buf) { deflateEnd(&strm); free(buf); return qb_string_empty(); }
            buf = new_buf;
            strm.next_out = buf + used;
            strm.avail_out = (uInt)(add > 0xFFFFFFFFu ? 0xFFFFFFFFu : add);
            cap += add;
            continue;
        }
        deflateEnd(&strm);
        free(buf);
        return qb_string_empty();
    }
    deflateEnd(&strm);
    size_t out_len = (size_t)(strm.next_out - buf);
    QbString* s = qb_string_from_bytes(buf, out_len);
    free(buf);
    return s;
}

QbString* qb_inflate(const QbString* data) {
    if (!data) return qb_string_empty();
    size_t len = qb_string_len(data);
    if (len == 0) return qb_string_empty();
    const uint8_t* src = (const uint8_t*)qb_string_data(data);
    if (!src) return qb_string_empty();

    z_stream strm;
    memset(&strm, 0, sizeof(strm));
    if (inflateInit2(&strm, -15) != Z_OK) return qb_string_empty();

    strm.next_in = (ZLIB_CONST Bytef*)src;
    strm.avail_in = (uInt)(len > 0xFFFFFFFFu ? 0xFFFFFFFFu : len);

    size_t cap = len * 4;
    if (cap > QB_INFLATE_MAX_OUT) cap = QB_INFLATE_MAX_OUT;
    if (cap < 256) cap = 256;
    Bytef* buf = (Bytef*)malloc(cap);
    if (!buf) { inflateEnd(&strm); return qb_string_empty(); }

    strm.next_out = buf;
    strm.avail_out = (uInt)(cap > 0xFFFFFFFFu ? 0xFFFFFFFFu : cap);

    for (;;) {
        int ret = inflate(&strm, Z_NO_FLUSH);
        if (ret == Z_STREAM_END || ret == Z_OK) {
            if (ret == Z_STREAM_END) break;
            if (strm.avail_out == 0) {
                size_t used = (size_t)(strm.next_out - buf);
                if (used >= QB_INFLATE_MAX_OUT) {
                    inflateEnd(&strm);
                    free(buf);
                    return qb_string_empty(); /* output limit exceeded, match runtime */
                }
                size_t add = cap;
                if (add > QB_INFLATE_MAX_OUT - used) add = QB_INFLATE_MAX_OUT - used;
                if (add == 0) {
                    inflateEnd(&strm);
                    free(buf);
                    return qb_string_empty();
                }
                cap = used + add;
                Bytef* new_buf = (Bytef*)realloc(buf, cap);
                if (!new_buf) {
                    inflateEnd(&strm);
                    free(buf);
                    return qb_string_empty();
                }
                buf = new_buf;
                strm.next_out = buf + used;
                strm.avail_out = (uInt)(add > 0xFFFFFFFFu ? 0xFFFFFFFFu : add);
            }
        } else {
            inflateEnd(&strm);
            free(buf);
            return qb_string_empty();
        }
    }
    inflateEnd(&strm);
    size_t out_len = (size_t)(strm.next_out - buf);
    QbString* s = qb_string_from_bytes(buf, out_len);
    free(buf);
    return s;
}

#endif /* QB64FRESH_COMPRESSION_EXTERNAL */
