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
#include <stdlib.h>
#include <math.h>

/* ============================================================================
 * Platform and Types (libqb-common.h compatible)
 * ============================================================================
 * OS: QB64_WINDOWS, QB64_LINUX, QB64_MACOSX, QB64_UNIX.
 * FS: QB64_BACKSLASH_FILESYSTEM.
 * Compiler: QB64_MICROSOFT, QB64_GCC, QB64_MINGW.
 * Arch: QB64_32, QB64_64, QB64_NOT_X86, QB64_ARM.
 * Constants: QB_FALSE (0), QB_TRUE (-1). Helper: _countof(array).
 */
#if defined(_WIN32) || defined(WIN32)
#  define QB64_WINDOWS
#  ifndef _WIN32_WINNT
#    define _WIN32_WINNT 0x0600
#    define WINVER 0x0600
#  endif
#  define QB64_BACKSLASH_FILESYSTEM
#  if defined(_MSC_VER)
#    define QB64_MICROSOFT
#  else
#    define QB64_GCC
#    define QB64_MINGW
#  endif
#elif defined(__APPLE__)
#  define QB64_MACOSX
#  define QB64_UNIX
#  define QB64_GCC
#elif defined(__linux__)
#  define QB64_LINUX
#  define QB64_UNIX
#  define QB64_GCC
#else
#  error "QB64Fresh runtime: unknown system; edit qb64fresh_rt.h"
#endif

#if defined(_WIN64) || defined(__x86_64__) || defined(__ppc64__) || defined(QB64_MACOSX) || defined(__aarch64__)
#  define QB64_64
#else
#  define QB64_32
#endif

#if !defined(i386) && !defined(__i386__) && !defined(__x86_64__)
#  define QB64_NOT_X86
#  if defined(__arm__) || defined(__aarch64__) || defined(_M_ARM64)
#    define QB64_ARM
#  endif
#endif

#ifndef QB_FALSE
#  define QB_FALSE 0
#endif
#ifndef QB_TRUE
#  define QB_TRUE (-1)
#endif

#ifndef _countof
#  define _countof(Array_) (sizeof(Array_) / sizeof((Array_)[0]))
#endif

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
QbString* qb_hex_float(double n);
QbString* qb_oct_float(double n);
QbString* qb_bin_float(double n);
QbString* qb_trim(const QbString* s);
int32_t qb_instrrev(const QbString* source, const QbString* search);
int32_t qb_instrrev3(const QbString* s, const QbString* sub, int32_t start);

/* Base64 encoding/decoding (_BASE64ENCODE$, _BASE64DECODE$) */
QbString* qb_base64encode(const QbString* data);
QbString* qb_base64decode(const QbString* data);

/* Hashing (_ADLER32, _CRC32, _MD5$) */
uint32_t qb_adler32(const QbString* data);
uint32_t qb_crc32(const QbString* data);
QbString* qb_md5(const QbString* data);

/* Compression (_DEFLATE$, _INFLATE$) — stubs return empty string */
QbString* qb_deflate(const QbString* data);
QbString* qb_inflate(const QbString* data);

/* String conversion from C string (for fixed-length strings) */
QbString* qb_str_from_c(const char* s);

/* qbs.h equivalents (LIBQB compatibility) */
QbString* qb_string_new_cmem(int32_t size);
QbString* qb_string_new_fixed(const uint8_t* ptr, uint32_t size);
void qb_string_set_size(QbString** target, int32_t newlength);

/* QB64pe-specific string processing functions */
QbString* qb_removestringenclosingpair_str(QbString** text_ref, QbString** pair_ref);

/* ============================================================================
 * libqb qbs.h compatibility — wrapper types for old code
 * ============================================================================
 * QB64Fresh uses opaque QbString* internally. These structs match QB64pe
 * libqb/include/qbs.h layout so code that expects qbs/qbs_field can compile
 * and run. Use qbs_from_qb_string() to wrap QbString* as qbs*; qbs_free()
 * releases the underlying QbString. Reading q->chr and q->len is supported;
 * modifying through q->chr is undefined (QbString is ref-counted and may move).
 */
struct qbs_field {
    int32_t fileno;
    int64_t fileid;
    int64_t size;
    int64_t offset;
};

struct qbs {
    uint8_t *chr;
    int32_t len;
    uint8_t in_cmem;
    uint16_t *cmem_descriptor;
    uint16_t cmem_descriptor_offset;
    uint32_t listi;
    uint8_t tmp;
    uint32_t tmplisti;
    uint8_t fixed;
    uint8_t readonly;
    struct qbs_field *field;
};

/* Wrap QbString* as qbs*; chr/len point into the string. Call qbs_free when done. */
struct qbs *qbs_from_qb_string(QbString *s);
void qbs_free(struct qbs *q);
/* Get QbString* from a qbs* created by qbs_from_qb_string (retains; caller must release). */
QbString *qb_string_from_qbs(struct qbs *q);
static inline int32_t qbs_len(struct qbs *str) { return str ? str->len : 0; }

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
 * libqb mem.h compatibility — _MEM lock lifecycle and globals
 * Structures and constants match QB64pe internal/c/libqb/include/mem.h.
 * Use qb_mem* for normal _MEM; use these when porting code that uses
 * mem_block/mem_lock/new_mem_lock/free_mem_lock.
 * ============================================================================ */
#define INVALID_MEM_LOCK 1073741821

#define MEM_TYPE_NOSECURITY 0
#define MEM_TYPE_MALLOC      1
#define MEM_TYPE_IMAGE       2
#define MEM_TYPE_SUBFUNC     3
#define MEM_TYPE_ARRAY       4
#define MEM_TYPE_SOUND       5

struct mem_block {
    intptr_t offset;
    intptr_t size;
    int64_t lock_id;
    intptr_t lock_offset;
    intptr_t type;
    intptr_t elementsize;
    int32_t image;
    int32_t sound;
};

struct mem_lock {
    int64_t id;
    int32_t type;
    void *offset;
};

extern uint64_t mem_lock_id;
extern struct mem_lock *mem_lock_tmp;
extern struct mem_lock *mem_lock_base;

void new_mem_lock(void);
void free_mem_lock(struct mem_lock *lock);

/* ============================================================================
 * Array Bounds Registry (LBOUND, UBOUND)
 * ============================================================================
 * Tracks lower/upper bounds per dimension for DIM'd arrays.
 * Generated code calls qb_array_register / qb_array_register_md on DIM,
 * qb_array_update on REDIM, and qb_array_erase on ERASE.
 */
void qb_array_register(void* ptr, int32_t lower, int32_t upper);
void qb_array_register_md(void* ptr, int32_t num_dims, int32_t* lowers, int32_t* uppers);
void qb_array_update(void* old_ptr, void* new_ptr, int32_t lower, int32_t upper);
int32_t qb_lbound(void* arr);
int32_t qb_lbound2(void* arr, int32_t dim);
int32_t qb_ubound(void* arr);
int32_t qb_ubound2(void* arr, int32_t dim);
void qb_array_erase(void* arr);

/* ============================================================================
 * Bit Operations (libqb bitops.h compatibility)
 * ============================================================================
 * Bit field get/set in byte buffers. Used by TYPE bit fields and DECLARE LIBRARY.
 * intptr_t from <stdint.h> (included at top).
 */
uint64_t qb_getubits(uint32_t bsize, uint8_t *base, intptr_t i);
int64_t qb_getbits(uint32_t bsize, uint8_t *base, intptr_t i);
void qb_setbits(uint32_t bsize, uint8_t *base, intptr_t i, int64_t val);

/* ============================================================================
 * Generic Buffer (libqb_buffer — QB64pe buffer.h compatibility)
 * ============================================================================
 * FIFO byte buffer: init once, write to append, read to consume.
 * Used by HTTP and other libqb code. Structure layout matches QB64pe.
 */
struct libqb_buffer_entry {
    size_t length;
    char *data;
    struct libqb_buffer_entry *next;
};

struct libqb_buffer {
    size_t total_length;
    size_t cur_entry_offset;
    struct libqb_buffer_entry *head;
    struct libqb_buffer_entry **tail;
};

void libqb_buffer_init(struct libqb_buffer *buffer);
void libqb_buffer_clear(struct libqb_buffer *buffer);
size_t libqb_buffer_length(struct libqb_buffer *buffer);
size_t libqb_buffer_read(struct libqb_buffer *buffer, char *out, size_t length);
void libqb_buffer_write(struct libqb_buffer *buffer, const char *in, size_t length);

/* ============================================================================
 * Mutex (libqb mutex.h compatibility)
 * ============================================================================
 * Opaque mutex for thread-safe access. Create with libqb_mutex_new(), destroy
 * with libqb_mutex_free(). Lock/unlock with libqb_mutex_lock/unlock.
 * In C, use LIBQB_MUTEX_GUARD(m) for scope-based lock; in C++, a class that
 * locks in ctor and unlocks in dtor can wrap these calls.
 */
struct libqb_mutex;

struct libqb_mutex *libqb_mutex_new(void);
void libqb_mutex_free(struct libqb_mutex *m);
void libqb_mutex_lock(struct libqb_mutex *m);
void libqb_mutex_unlock(struct libqb_mutex *m);

/* RAII-style scope lock for C: lock at start of block, unlock at end. */
#define LIBQB_MUTEX_GUARD(m) \
    for (int _qb_guard_done = (libqb_mutex_lock(m), 0); !_qb_guard_done; _qb_guard_done = 1, libqb_mutex_unlock(m))

#ifdef __cplusplus
}
/* C++ RAII guard (usable when header is included from C++) */
class libqb_mutex_guard {
public:
    explicit libqb_mutex_guard(struct libqb_mutex *mtx) : lock_(mtx) {
        libqb_mutex_lock(lock_);
    }
    ~libqb_mutex_guard() {
        libqb_mutex_unlock(lock_);
    }
    libqb_mutex_guard(const libqb_mutex_guard &) = delete;
    libqb_mutex_guard &operator=(const libqb_mutex_guard &) = delete;
private:
    struct libqb_mutex *lock_;
};
extern "C" {
#endif

/* ============================================================================
 * Condition Variable (libqb condvar.h compatibility)
 * ============================================================================
 * Opaque condition variable for wait/signal. Create with libqb_condvar_new(),
 * destroy with libqb_condvar_free(). Use with libqb_mutex: lock mutex, check
 * predicate, call libqb_condvar_wait(cond, mutex) in a loop, then
 * libqb_condvar_signal or libqb_condvar_broadcast when condition becomes true.
 */
struct libqb_condvar;

struct libqb_condvar *libqb_condvar_new(void);
void libqb_condvar_free(struct libqb_condvar *c);
void libqb_condvar_wait(struct libqb_condvar *c, struct libqb_mutex *mutex);
void libqb_condvar_signal(struct libqb_condvar *c);
void libqb_condvar_broadcast(struct libqb_condvar *c);

/* ============================================================================
 * Completion (libqb completion.h — one-shot thread sync)
 * ============================================================================
 * One-shot signal: wait until finish is called, then never block again.
 * Caller allocates struct completion (stack or heap), calls completion_init,
 * then completion_wait / completion_finish from different threads; finally
 * completion_clear to release mutex and condvar.
 */
struct completion {
    int finished;
    struct libqb_mutex *mutex;
    struct libqb_condvar *var;
};

void completion_init(struct completion *comp);
void completion_clear(struct completion *comp);
void completion_wait(struct completion *comp);
void completion_finish(struct completion *comp);

/* ============================================================================
 * Handle List (qblist.h — thread-safe list for handles)
 * ============================================================================
 * Index 0 unused; indices are 1-based. Each slot stores [user_data | index].
 */
struct list;
struct list *list_new(intptr_t structure_size);
struct list *list_new_threadsafe(intptr_t structure_size);
void list_destroy(struct list *L);
intptr_t list_add(struct list *L);
intptr_t list_remove(struct list *L, intptr_t i);  /* -1 success, 0 failure */
void *list_get(struct list *L, intptr_t i);
intptr_t list_get_index(struct list *L, void *structure);

/* ============================================================================
 * Path Utilities (LIBQB filepath.h compatibility)
 * ============================================================================
 * Caller must qb_string_release() any returned QbString*.
 */
QbString* filepath_get_filename(const char* path);
QbString* filepath_get_extension(const char* path);
int32_t filepath_has_extension(const char* path, const char* extension);
const char* filepath_fix_directory(char* path);
QbString* filepath_fix_directory_copy(const char* path);
void filepath_split(const char* path, QbString** dir_out, QbString** file_out);
QbString* filepath_join(const char* directory, const char* filename);

/* ============================================================================
 * ParseNum UDT (for QB64pe compatibility)
 * ============================================================================
 * Used internally by QB64pe for constant evaluation and parsing.
 * This struct holds a parsed numeric/string value during evaluation.
 */
typedef struct qbt_ParseNum {
    long double f;        /* _FLOAT - floating point value */
    int64_t i;            /* _INTEGER64 - signed integer value */
    uint64_t ui;          /* _UNSIGNED _INTEGER64 - unsigned integer value */
    QbString* s;          /* STRING - string value */
    int32_t typ;          /* LONG - type indicator */
} qbt_ParseNum;

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
void qb_keydown_vk(uint32_t vk);   /* _KEYDOWN (simulate key) — stub */
void qb_keyup_vk(uint32_t vk);    /* _KEYUP (simulate key) — stub */

/* Key code bases (libqb keyhandler.h compatibility)
 * QBK: 200000 = numpad/scroll/insert mode codes
 * VK:  100000 = virtual key offset for _KEYHIT
 * UC:  1073741824 = Unicode code point offset
 */
#define QBK 200000
#define VK  100000
#define UC  1073741824

/* QBVK_* — Virtual key codes (match libqb keyhandler.h for _KEYDOWN/_KEYHIT) */
#define QBVK_UNKNOWN    0
#define QBVK_BACKSPACE  8
#define QBVK_TAB        9
#define QBVK_CLEAR      12
#define QBVK_RETURN     13
#define QBVK_PAUSE      19
#define QBVK_ESCAPE     27
#define QBVK_SPACE      32
#define QBVK_0          48
#define QBVK_1          49
#define QBVK_2          50
#define QBVK_3          51
#define QBVK_4          52
#define QBVK_5          53
#define QBVK_6          54
#define QBVK_7          55
#define QBVK_8          56
#define QBVK_9          57
#define QBVK_a          97
#define QBVK_b          98
#define QBVK_c          99
#define QBVK_d          100
#define QBVK_e          101
#define QBVK_f          102
#define QBVK_g          103
#define QBVK_h          104
#define QBVK_i          105
#define QBVK_j          106
#define QBVK_k          107
#define QBVK_l          108
#define QBVK_m          109
#define QBVK_n          110
#define QBVK_o          111
#define QBVK_p          112
#define QBVK_q          113
#define QBVK_r          114
#define QBVK_s          115
#define QBVK_t          116
#define QBVK_u          117
#define QBVK_v          118
#define QBVK_w          119
#define QBVK_x          120
#define QBVK_y          121
#define QBVK_z          122
#define QBVK_DELETE     127
#define QBVK_KP0        256
#define QBVK_KP1        257
#define QBVK_KP2        258
#define QBVK_KP3        259
#define QBVK_KP4        260
#define QBVK_KP5        261
#define QBVK_KP6        262
#define QBVK_KP7        263
#define QBVK_KP8        264
#define QBVK_KP9        265
#define QBVK_KP_PERIOD  266
#define QBVK_KP_DIVIDE  267
#define QBVK_KP_MULTIPLY 268
#define QBVK_KP_MINUS   269
#define QBVK_KP_PLUS    270
#define QBVK_KP_ENTER   271
#define QBVK_UP         273
#define QBVK_DOWN       274
#define QBVK_RIGHT      275
#define QBVK_LEFT       276
#define QBVK_INSERT     277
#define QBVK_HOME       278
#define QBVK_END        279
#define QBVK_PAGEUP     280
#define QBVK_PAGEDOWN   281
#define QBVK_F1         282
#define QBVK_F2         283
#define QBVK_F3         284
#define QBVK_F4         285
#define QBVK_F5         286
#define QBVK_F6         287
#define QBVK_F7         288
#define QBVK_F8         289
#define QBVK_F9         290
#define QBVK_F10        291
#define QBVK_F11        292
#define QBVK_F12        293
#define QBVK_NUMLOCK    300
#define QBVK_CAPSLOCK   301
#define QBVK_SCROLLOCK  302
#define QBVK_RSHIFT     303
#define QBVK_LSHIFT     304
#define QBVK_RCTRL      305
#define QBVK_LCTRL      306
#define QBVK_RALT       307
#define QBVK_LALT       308
#define QBVK_LSUPER     311
#define QBVK_RSUPER     312
#define QBVK_MENU       319

/* KMOD_* — Key modifiers (libqb keyhandler.h) */
#define KMOD_NONE   0x0000
#define KMOD_LSHIFT 0x0001
#define KMOD_RSHIFT 0x0002
#define KMOD_LCTRL  0x0040
#define KMOD_RCTRL  0x0080
#define KMOD_LALT   0x0100
#define KMOD_RALT   0x0200
#define KMOD_LMETA  0x0400
#define KMOD_RMETA  0x0800
#define KMOD_NUM    0x1000
#define KMOD_CAPS   0x2000
#define KMOD_MODE   0x4000
#define KMOD_CTRL   (KMOD_LCTRL | KMOD_RCTRL)
#define KMOD_SHIFT  (KMOD_LSHIFT | KMOD_RSHIFT)
#define KMOD_ALT    (KMOD_LALT | KMOD_RALT)
#define KMOD_META   (KMOD_LMETA | KMOD_RMETA)

/* Console/display (libqb.h compatibility) */
void qb_printimage(int32_t i);           /* _PRINTIMAGE - print image to console; stub */
void validatepage(int32_t n);            /* Validate graphics page; stub */
void qb_view_print(int32_t top, int32_t bottom);
void qb_view_print_reset(void);
void qbg_sub_view_print(int32_t topline, int32_t bottomline, int32_t passed);  /* VIEW PRINT */
void makefit(QbString* text);            /* Fit text to width before printing */
void lprint_makefit(QbString* text);     /* LPRINT width fit; stub */

/* Keyboard port 0x60 queue (libqb port60h_event / port60h_events) */
extern uint8_t port60h_event[256];
extern int32_t port60h_events;

/* Window/control state (libqb) */
extern int32_t window_exists;
extern int32_t no_control_characters2;

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
void qb_fpu_reinit(void);  /* Reset FPU rounding mode (libqb fpu_reinit) — stub */

/* Rounding / type conversion (rounding.h compatibility) */
int64_t qb_qbr(double n);           /* Round to int64 (FPU-style) */
float qb_csng_float(double n);      /* CSNG from float; error 6 on overflow */
float qb_csng_double(double n);     /* CSNG from double; error 6 on overflow */
double qb_cdbl_float(double n);     /* CDBL from float */
int64_t qb_round_double(double n);  /* _ROUND (double) */
int64_t qb_round_float(double n);    /* _ROUND (float) */

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
double qb_d2g(double degrees);
double qb_g2d(double gradians);
double qb_g2r(double gradians);
double qb_r2g(double radians);

/* Extended math (sec/csc/cot, arc*, clamp) — Used by QB64pe */
double qb_sec(double n);
double qb_csc(double n);
double qb_cot(double n);
double qb_sech(double n);
double qb_csch(double n);
double qb_coth(double n);
double qb_arcsec(double n);
double qb_arccsc(double n);
double qb_arccot(double n);
double qb_arcsech(double n);
double qb_arccsch(double n);
double qb_arccoth(double n);
double qb_clamp(double x, double min_val, double max_val);

/* Constants */
double qb_pi(void);
double qb_e(void);

/* Power-of-2 (libqb extended_math.h — Math_IsPowerOf2, RoundUp/DownToPowerOf2) */
int qb_math_is_power_of_2_u32(uint32_t n);
int qb_math_is_power_of_2_u64(uint64_t n);
uint32_t qb_math_round_up_to_power_of_2_u32(uint32_t n);
uint64_t qb_math_round_up_to_power_of_2_u64(uint64_t n);
uint32_t qb_math_round_down_to_power_of_2_u32(uint32_t n);
uint64_t qb_math_round_down_to_power_of_2_u64(uint64_t n);

/* Min/Max */
int64_t qb_min_int(int64_t a, int64_t b);
int64_t qb_max_int(int64_t a, int64_t b);
double qb_min_float(double a, double b);
double qb_max_float(double a, double b);

/* Timer */
double qb_timer(void);
void qb_sleep(double seconds);
void qb_delay(double seconds);

/* Bit manipulation */
int64_t qb_readbit(int64_t value, int64_t bit);
int64_t qb_setbit(int64_t value, int64_t bit);
int64_t qb_resetbit(int64_t value, int64_t bit);
int64_t qb_togglebit(int64_t value, int64_t bit);
int64_t qb_rol(int64_t value, int64_t bits);
int64_t qb_ror(int64_t value, int64_t bits);

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

/* OPEN access mode (ACCESS READ / WRITE / READ WRITE) */
#define QB_FILE_ACCESS_DEFAULT 0
#define QB_FILE_ACCESS_READ    1
#define QB_FILE_ACCESS_WRITE   2
#define QB_FILE_ACCESS_READ_WRITE 3

/* OPEN lock mode (SHARED, LOCK READ/WRITE/READ WRITE, ONLY) */
#define QB_FILE_LOCK_DEFAULT    0
#define QB_FILE_LOCK_SHARED     1
#define QB_FILE_LOCK_READ       2
#define QB_FILE_LOCK_WRITE      3
#define QB_FILE_LOCK_READ_WRITE 4
#define QB_FILE_LOCK_ONLY      5

/* File open/close. access/lock: use QB_FILE_ACCESS_* and QB_FILE_LOCK_* (0 = default). */
void qb_file_open(int32_t fnum, const char* filename, const char* mode, int32_t access, int32_t lock);
void qb_file_open_str(int32_t fnum, const QbString* filename, const char* mode, int32_t access, int32_t lock);
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
int64_t qb_seek(int32_t fnum);
int32_t qb_freefile(void);

/* File locking (LOCK/UNLOCK statements) */
/* Lock/unlock a file or file range. start=-1, end=-1 means lock entire file. */
/* Returns: 0=success, -2=invalid handle, -4=illegal function call, -7=permission denied, -9=access error */
int32_t qb_file_lock(int32_t fnum, int64_t start, int64_t end);
int32_t qb_file_unlock(int32_t fnum, int64_t start, int64_t end);

/* ============================================================================
 * libqb gfs.h compatibility — wrapper struct for old code
 * ============================================================================
 * QB64Fresh uses opaque file handles internally. This struct matches QB64pe
 * libqb/include/gfs.h layout so code that expects gfs_file_struct can
 * compile and run. gfs_get_file_struct(fileno) fills and returns a pointer
 * to the struct for that fileno (NULL if invalid/closed). Platform-specific
 * handles are void* (file_handle, file_handle_o, win_handle).
 */
struct gfs_file_struct {
    int64_t id;
    uint8_t open;
    uint8_t read;
    uint8_t write;
    uint8_t lock_read;
    uint8_t lock_write;
    int64_t pos;
    uint8_t eof_reached;
    uint8_t eof_passed;
    int32_t fileno;
    uint8_t type;
    int64_t record_length;
    uint8_t *field_buffer;
    struct qbs **field_strings;
    int32_t field_strings_n;
    int64_t column;
    void *file_handle;
    void *file_handle_o;
    void *win_handle;
    uint8_t com_port;
    int32_t com_baud_rate;
    int8_t com_parity;
    int8_t com_data_bits_per_byte;
    int8_t com_stop_bits;
    int8_t com_bin_asc;
    int8_t com_asc_lf;
    int8_t com_rs;
    int32_t com_cd_x;
    int32_t com_cs_x;
    int32_t com_ds_x;
    int32_t com_op_x;
    uint8_t scrn;
};

struct gfs_file_struct *gfs_get_file_struct(int32_t fileno);
int32_t gfs_get_fileno(int32_t file_number);

/* Memory (QB4.5 compatibility) */
int64_t qb_fre(int64_t n);  /* FRE(n) - approximate free memory; n=0 far heap, n=-1 string space */

/* System/shell (libqb shell.h compatibility) */
extern int32_t shell_call_in_progress;  /* 1 while SHELL is active, 0 otherwise */
int32_t qb_shell(const char* command);  /* SHELL statement - execute command */
int32_t qb_shellhide(const QbString* cmd);  /* _SHELLHIDE(command$) - execute without console */

/* ============================================================================
 * Thread (libqb thread.h compatibility)
 * ============================================================================
 * Opaque thread handle. Create with libqb_thread_new(), start with
 * libqb_thread_start(), join with libqb_thread_join(), then free with libqb_thread_free().
 */
struct libqb_thread;
struct libqb_thread *libqb_thread_new(void);
void libqb_thread_free(struct libqb_thread *t);
void libqb_thread_start(struct libqb_thread *t, void (*start_func)(void *), void *arg);
void libqb_thread_join(struct libqb_thread *t);

/* Logging (Used by QB64pe) — simple wrappers */
void qb_logtrace(const QbString* msg);
void qb_loginfo(const QbString* msg);
void qb_logwarn(const QbString* msg);
void qb_logerror(const QbString* msg);
void qb_logminlevel(int64_t level);

/* ============================================================================
 * Scoped Logging (libqb logging.h compatibility)
 * ============================================================================
 * loglevel: 0=Trace, 1=Information, 2=Warning, 3=Error
 * logscope: 0=Runtime, 1=QB64, 2=Libqb, 3=Audio, 4=Image
 *
 * When using external runtime (--runtime external), variadic libqb_log/libqb_log_qb64
 * are implemented in runtime/c_src/logging.c; compile and link that file with your program.
 */
#define QB_LOGLEVEL_TRACE        0
#define QB_LOGLEVEL_INFORMATION  1
#define QB_LOGLEVEL_WARNING      2
#define QB_LOGLEVEL_ERROR        3
#define QB_LOGSCOPE_RUNTIME      0
#define QB_LOGSCOPE_QB64         1
#define QB_LOGSCOPE_LIBQB        2
#define QB_LOGSCOPE_AUDIO        3
#define QB_LOGSCOPE_IMAGE        4

void libqb_log_init(void);
void libqb_log(int32_t lvl, int32_t scope, const char* file, const char* func, int line, const char* fmt, ...);
void libqb_log_qb64(int32_t lvl, int32_t scope, const char* file, const char* func, int line, const char* fmt, ...);
void libqb_log_qbs(int32_t lvl, int32_t scope, const char* file, const char* func, int line, const QbString* str);
void qb_log_message(int32_t lvl, int32_t scope, const char* file, const char* func, int line, const char* message);
int32_t qb_log_set_min_level(int32_t level);
int32_t qb_log_get_min_level(void);

#define libqb_log_with_scope_trace(scope, fmt, ...) \
    libqb_log(QB_LOGLEVEL_TRACE, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)
#define libqb_log_with_scope_info(scope, fmt, ...) \
    libqb_log(QB_LOGLEVEL_INFORMATION, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)
#define libqb_log_with_scope_warn(scope, fmt, ...) \
    libqb_log(QB_LOGLEVEL_WARNING, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)
#define libqb_log_with_scope_error(scope, fmt, ...) \
    libqb_log(QB_LOGLEVEL_ERROR, (scope), __FILE__, __func__, __LINE__, fmt, ## __VA_ARGS__)

#define libqb_log_trace(...) \
    libqb_log_with_scope_trace(QB_LOGSCOPE_LIBQB, __VA_ARGS__)
#define libqb_log_info(...) \
    libqb_log_with_scope_info(QB_LOGSCOPE_LIBQB, __VA_ARGS__)
#define libqb_log_warn(...) \
    libqb_log_with_scope_warn(QB_LOGSCOPE_LIBQB, __VA_ARGS__)
#define libqb_log_error(...) \
    libqb_log_with_scope_error(QB_LOGSCOPE_LIBQB, __VA_ARGS__)

/* FIELD statement support */
void qb_field_start(int32_t fnum);
void qb_field_add(int32_t width, QbString** var);
void qb_field_free(QbString* str);   /* libqb field_free: no-op (no per-string field in QB64Fresh) */
void qb_field_update(int32_t fileno); /* libqb field_update: sync field buffer to vars (no-op if no var list) */
void qb_lset(QbString** var, QbString* value);
void qb_rset(QbString** var, QbString* value);

/* libqb file-fields.h — Free field buffer for a string; sync field buffer to file.
 * field_free(s): disconnects string from field buffer (no-op if not tracked).
 * field_update(fileno): syncs field buffer for file (stub until full field tracking).
 */
void field_free(QbString* s);
void field_update(int32_t fileno);

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
QbString* qb_files_str(const QbString* spec);  /* _FILES$ - same as qb_dir */
void qb_files(const QbString* spec);  /* FILES statement - print directory listing to console */

/* Environment operations (ENVIRON statement, ENVIRON$, _ENVIRONCOUNT) */
void qb_sub_environ(QbString* env);
int64_t qb_environcount(void);
QbString* qb_environ(const QbString* name);
QbString* qb_environ_by_index(int64_t index);  /* ENVIRON$(n) - nth env var, 1-based, "NAME=VALUE" */

/* ============================================================================
 * Networking (_OPENHOST, _OPENCONNECTION, _OPENCLIENT, _CONNECTED, _CLOSEHOST,
 *             GET #/PUT #, EOF, LOF on network handles)
 * ============================================================================ */

int64_t qb_net_openhost(const char* connection_string);
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
 * HTTP Client (libqb_http_* — OPEN URL, GET bytes, Content-Length, etc.)
 * ============================================================================ */

void libqb_http_init(void);
void libqb_http_stop(void);

/* All return 0 on success, negative on error (or invalid handle). */

int libqb_http_open(const char* url, int handle);
int libqb_http_close(int handle);
int libqb_http_connected(int handle);

int libqb_http_get_length(int handle, size_t* length);
int libqb_http_get_content_length(int handle, uint64_t* length);
int libqb_http_get_status_code(int handle);  /* Returns status code or -1 */

/* Effective URL after redirects; valid until handle closed. NULL if invalid. */
const char* libqb_http_get_url(int handle);

int libqb_http_get(int handle, char* buf, size_t* length);
int libqb_http_get_fixed(int handle, char* buf, size_t length);

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

/* Notify popup (toast / action center); icon_type: "info" | "warning" | "error" */
void qb_notifypopup(const char* title, const char* message, const char* icon_type);

/* Input box; returns user text or empty string if cancelled. NULL default_input = password-style. */
QbString* qb_inputbox(const char* title, const char* message, const char* default_input);

/* Color chooser; returns 0xAARRGGBB or 0 if cancelled */
uint32_t qb_colorchooserdialog(const char* title, uint32_t default_rgb);

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
 * game_controller.h — Gamepad / Keyboard / Mouse Devices (libqb compatibility)
 * ============================================================================
 * Structures: device_struct, onstrig_struct.
 * Constants: QUEUED_EVENTS_LIMIT, DEVICETYPE_CONTROLLER, DEVICETYPE_KEYBOARD, DEVICETYPE_MOUSE.
 * Globals: device_last, device_max, devices, onstrig, onstrig_inprogress.
 */
#define QUEUED_EVENTS_LIMIT 1024
#define DEVICETYPE_CONTROLLER 1
#define DEVICETYPE_KEYBOARD 2
#define DEVICETYPE_MOUSE 3

struct device_struct {
    int32_t used;
    int32_t type;
    char *name;
    int32_t connected;
    int32_t lastbutton;
    int32_t lastaxis;
    int32_t lastwheel;
    int32_t max_events;
    int32_t queued_events;
    uint8_t *events;
    int32_t event_size;
    uint8_t STRIG_button_pressed[256];
    void *handle_pointer;
    int64_t handle_int;
    const char *description;
    int64_t product_id;
    int64_t vendor_id;
    int32_t buttons;
    int32_t axes;
    int32_t balls;
    int32_t hats;
};

struct onstrig_struct {
    uint32_t id;
    int64_t pass;
    uint8_t active;
    uint8_t state;
};

extern int32_t device_last;
extern int32_t device_max;
extern struct device_struct *devices;
extern struct onstrig_struct *onstrig;
extern int32_t onstrig_inprogress;

uint8_t getDeviceEventButtonValue(struct device_struct *device, int32_t eventIndex, int32_t objectIndex);
void setDeviceEventButtonValue(struct device_struct *device, int32_t eventIndex, int32_t objectIndex, uint8_t value);
float getDeviceEventAxisValue(struct device_struct *device, int32_t eventIndex, int32_t objectIndex);
void setDeviceEventAxisValue(struct device_struct *device, int32_t eventIndex, int32_t objectIndex, float value);
float getDeviceEventWheelValue(struct device_struct *device, int32_t eventIndex, int32_t objectIndex);
void setDeviceEventWheelValue(struct device_struct *device, int32_t eventIndex, int32_t objectIndex, float value);
void setupDevice(struct device_struct *device);
int32_t createDeviceEvent(struct device_struct *device);
void commitDeviceEvent(struct device_struct *device);

/* ============================================================================
 * Runtime Control
 * ============================================================================ */

void qb_runtime_init(void);
void qb_runtime_shutdown(void);
void qb_end(int32_t exit_code);
void qb_stop(void);
void qb_run(QbString* path);

/* Set to 1 when user closes window (X). Program checks "if (stop_program) end();" */
extern uint8_t stop_program;

/* Initialization functions called at program start */
/* qb_init_args is defined in generated C to set _qb_argc/_qb_argv for COMMAND$ */
void qb_init_startdir(void);
void _qb_init_palette(void);

/* Error handling functions (Option B: error-pending and RESUME support) */
uint32_t qb_error_pending(void);    /* Non-zero if error pending (e.g. after failed OPEN) */
void qb_set_error(uint32_t code, int32_t line);  /* Set pending error from runtime */
void qb_clear_error(void);         /* Clear pending (e.g. RESUME NEXT) */
void qb_commit_error(void);        /* Copy pending to ERR/ERL then clear; call before goto handler */
void qb_fix_error(void);           /* Process error state: if no handler, report and exit(1) */

int32_t qb_err_code(void);          /* ERR function - returns error code */
int32_t qb_err_line(void);          /* ERL function - returns error line */
int64_t qb_errorline(void);         /* _ERRORLINE - returns error line as 64-bit */
QbString* qb_errormessage(void);   /* _ERRORMESSAGE$ - returns error message */
int32_t qb_inclerrorline(void);    /* _INCLERRORLINE - error line in include file */
QbString* qb_inclerrorfile(void);  /* _INCLERRORFILE$ - include file with error */

/* Error handling state (libqb error_handle.h compatibility) */
uint32_t qb_error_handling_get(void);   /* Non-zero while in error handler */
void qb_error_handling_set(uint32_t v);
uint32_t qb_error_retry_get(void);      /* Set by RESUME (retry), cleared after retry */
void qb_error_retry_set(uint32_t v);
QbString* qb_error_handler_history_get(void);  /* Error handler call history string */
void qb_error_handler_history_set(QbString* s);

/* QB_ERROR_* constants (1-76, 256-260, 270-271, 300-315, 502-518) */
#define QB_ERROR_NEXT_WITHOUT_FOR 1
#define QB_ERROR_SYNTAX_ERROR 2
#define QB_ERROR_RETURN_WITHOUT_GOSUB 3
#define QB_ERROR_OUT_OF_DATA 4
#define QB_ERROR_ILLEGAL_FUNCTION_CALL 5
#define QB_ERROR_OVERFLOW 6
#define QB_ERROR_OUT_OF_MEMORY 7
#define QB_ERROR_LABEL_NOT_DEFINED 8
#define QB_ERROR_SUBSCRIPT_OUT_OF_RANGE 9
#define QB_ERROR_DUPLICATE_DEFINITION 10
#define QB_ERROR_DIVISION_BY_ZERO 11
#define QB_ERROR_ILLEGAL_IN_DIRECT_MODE 12
#define QB_ERROR_TYPE_MISMATCH 13
#define QB_ERROR_OUT_OF_STRING_SPACE 14
#define QB_ERROR_STRING_FORMULA_TOO_COMPLEX 16
#define QB_ERROR_CANNOT_CONTINUE 17
#define QB_ERROR_FUNCTION_NOT_DEFINED 18
#define QB_ERROR_NO_RESUME 19
#define QB_ERROR_RESUME_WITHOUT_ERROR 20
#define QB_ERROR_DEVICE_TIMEOUT 24
#define QB_ERROR_DEVICE_FAULT 25
#define QB_ERROR_FOR_WITHOUT_NEXT 26
#define QB_ERROR_OUT_OF_PAPER 27
#define QB_ERROR_WHILE_WITHOUT_WEND 29
#define QB_ERROR_WEND_WITHOUT_WHILE 30
#define QB_ERROR_DUPLICATE_LABEL 33
#define QB_ERROR_SUBPROGRAM_NOT_DEFINED 35
#define QB_ERROR_ARGUMENT_COUNT_MISMATCH 37
#define QB_ERROR_ARRAY_NOT_DEFINED 38
#define QB_ERROR_VARIABLE_REQUIRED 40
#define QB_ERROR_FIELD_OVERFLOW 50
#define QB_ERROR_INTERNAL_ERROR 51
#define QB_ERROR_BAD_FILE_NAME_OR_NUMBER 52
#define QB_ERROR_FILE_NOT_FOUND 53
#define QB_ERROR_BAD_FILE_MODE 54
#define QB_ERROR_FILE_ALREADY_OPEN 55
#define QB_ERROR_FIELD_STATEMENT_ACTIVE 56
#define QB_ERROR_DEVICE_IO_ERROR 57
#define QB_ERROR_FILE_ALREADY_EXISTS 58
#define QB_ERROR_BAD_RECORD_LENGTH 59
#define QB_ERROR_DISK_FULL 61
#define QB_ERROR_INPUT_PAST_END_OF_FILE 62
#define QB_ERROR_BAD_RECORD_NUMBER 63
#define QB_ERROR_BAD_FILE_NAME 64
#define QB_ERROR_TOO_MANY_FILES 67
#define QB_ERROR_DEVICE_UNAVAILABLE 68
#define QB_ERROR_COMMUNICATION_BUFFER_OVERFLOW 69
#define QB_ERROR_PERMISSION_DENIED 70
#define QB_ERROR_DISK_NOT_READY 71
#define QB_ERROR_DISK_MEDIA_ERROR 72
#define QB_ERROR_FEATURE_UNAVAILABLE 73
#define QB_ERROR_RENAME_ACROSS_DISKS 74
#define QB_ERROR_PATH_FILE_ACCESS_ERROR 75
#define QB_ERROR_PATH_NOT_FOUND 76
#define QB_ERROR_OUT_OF_STACK_SPACE 256
#define QB_ERROR_OUT_OF_MEMORY_FATAL 257
#define QB_ERROR_INVALID_HANDLE 258
#define QB_ERROR_CANNOT_FIND_DYNAMIC_LIBRARY_FILE 259
#define QB_ERROR_FUNCTION_NOT_FOUND_IN_DYNAMIC_LIBRARY 260
#define QB_ERROR_FUNCTION_NOT_FOUND_IN_DYNAMIC_LIBRARY_261 261
#define QB_ERROR_GL_COMMAND_OUTSIDE_SUB_GL_SCOPE 270
#define QB_ERROR_END_SYSTEM_IN_SUB_GL_SCOPE 271
#define QB_ERROR_MEMORY_REGION_OUT_OF_RANGE 300
#define QB_ERROR_INVALID_SIZE 301
#define QB_ERROR_SOURCE_MEMORY_REGION_OUT_OF_RANGE 302
#define QB_ERROR_DESTINATION_MEMORY_REGION_OUT_OF_RANGE 303
#define QB_ERROR_BOTH_MEMORY_REGIONS_OUT_OF_RANGE 304
#define QB_ERROR_SOURCE_MEMORY_FREED 305
#define QB_ERROR_DESTINATION_MEMORY_FREED 306
#define QB_ERROR_MEMORY_ALREADY_FREED 307
#define QB_ERROR_MEMORY_HAS_BEEN_FREED 308
#define QB_ERROR_MEMORY_NOT_INITIALIZED 309
#define QB_ERROR_SOURCE_MEMORY_NOT_INITIALIZED 310
#define QB_ERROR_DESTINATION_MEMORY_NOT_INITIALIZED 311
#define QB_ERROR_BOTH_MEMORY_NOT_INITIALIZED 312
#define QB_ERROR_BOTH_MEMORY_FREED 313
#define QB_ERROR_ASSERT_FAILED 314
#define QB_ERROR_ASSERT_FAILED_WITH_DESCRIPTION 315
#define QB_ERROR_OUT_OF_MEMORY_FATAL_502 502
#define QB_ERROR_OUT_OF_MEMORY_FATAL_503 503
#define QB_ERROR_OUT_OF_MEMORY_FATAL_504 504
#define QB_ERROR_OUT_OF_MEMORY_FATAL_505 505
#define QB_ERROR_OUT_OF_MEMORY_FATAL_506 506
#define QB_ERROR_OUT_OF_MEMORY_FATAL_507 507
#define QB_ERROR_OUT_OF_MEMORY_FATAL_508 508
#define QB_ERROR_OUT_OF_MEMORY_FATAL_509 509
#define QB_ERROR_OUT_OF_MEMORY_FATAL_510 510
#define QB_ERROR_OUT_OF_MEMORY_FATAL_511 511
#define QB_ERROR_OUT_OF_MEMORY_FATAL_512 512
#define QB_ERROR_OUT_OF_MEMORY_FATAL_513 513
#define QB_ERROR_OUT_OF_MEMORY_FATAL_514 514
#define QB_ERROR_OUT_OF_MEMORY_FATAL_515 515
#define QB_ERROR_OUT_OF_MEMORY_FATAL_516 516
#define QB_ERROR_OUT_OF_MEMORY_FATAL_517 517
#define QB_ERROR_OUT_OF_MEMORY_FATAL_518 518

/* Debug event hooks (Option B: evnt for IDE/debugger) */
extern uint32_t qbevent;           /* 0 = no debug; non-zero = call qb_evnt at statement boundaries */
void qb_evnt(uint32_t line, uint32_t incline, const char* incfile);

/* Event type constants (libqb event.h compatibility) */
#define QB64_EVENT_CLOSE 1
#define QB64_EVENT_KEY 2
#define QB64_EVENT_RELATIVE_MOUSE_MOVEMENT 3
#define QB64_EVENT_FILE_DROP 4

/* Custom event callback (libqb qb64_custom_event). Stub returns 0. */
int qb64_custom_event(int event, int v1, int v2, int v3, int v4, int v5, int v6, int v7, int v8, void* p1, void* p2);

/* Compatibility macros for inline runtime naming conventions */
#define qb__rgb32(r, g, b) qb_rgb(r, g, b)
#define qb__rgb32_4(r, g, b, a) qb_rgba(r, g, b, a)
#define qb__rgba32(r, g, b, a) qb_rgba(r, g, b, a)

/* ============================================================================
 * Graphics state structures (libqb graphics.h compatibility)
 * ============================================================================
 * Types and constants for code that references libqb graphics structures.
 * Full screen/image/draw API is in Graphics Functions below.
 */
struct qb_img_struct {
    void* lock_offset;
    int64_t lock_id;
    uint8_t valid;
    uint8_t text;
    uint8_t console;
    uint16_t width, height;
    uint8_t bytes_per_pixel;
    uint8_t bits_per_pixel;
    uint32_t mask;
    uint16_t compatible_mode;
    uint32_t color, background_color, draw_color;
    uint32_t font;
    int16_t top_row, bottom_row;
    int16_t cursor_x, cursor_y;
    uint8_t cursor_show, cursor_firstvalue, cursor_lastvalue;
    union { uint8_t* offset; uint32_t* offset32; };
    uint32_t flags;
    uint32_t* pal;
    int32_t transparent_color;
    uint8_t alpha_disabled;
    uint8_t holding_cursor;
    uint8_t print_mode;
    uint8_t apm_p1;
    int32_t view_x1, view_y1, view_x2, view_y2;
    int32_t view_offset_x, view_offset_y;
    float x, y;
    uint8_t clipping_or_scaling;
    float scaling_x, scaling_y, scaling_offset_x, scaling_offset_y;
    float window_x1, window_y1, window_x2, window_y2;
    double draw_ta;
    double draw_scale;
    uint8_t apm_p2;
};
struct qb_hsb_color { double h; double s; double b; };
struct qb_rgb_color { double r; double g; double b; };
struct qb_render_state_dest { int32_t ignore; };
struct qb_render_state_source { int32_t smooth_stretched; int32_t smooth_shrunk; int32_t texture_wrap; int32_t PO2_fix; };
struct qb_render_state_global {
    struct qb_render_state_dest* dest;
    struct qb_render_state_source* source;
    int32_t dest_handle;
    int32_t source_handle;
    int32_t view_mode;
    int32_t use_alpha;
    int32_t depthbuffer_mode;
    int32_t cull_mode;
};
#define QB_VIEW_MODE__UNKNOWN 0
#define QB_VIEW_MODE__2D 1
#define QB_VIEW_MODE__3D 2
#define QB_VIEW_MODE__RESET 3
#define QB_ALPHA_MODE__UNKNOWN (-1)
#define QB_ALPHA_MODE__DONT_BLEND 0
#define QB_ALPHA_MODE__BLEND 1
#define QB_DEPTHBUFFER_MODE__UNKNOWN (-1)
#define QB_DEPTHBUFFER_MODE__OFF 0
#define QB_DEPTHBUFFER_MODE__ON 1
#define QB_DEPTHBUFFER_MODE__LOCKED 2
#define QB_DEPTHBUFFER_MODE__CLEAR 3
#define QB_CULL_MODE__UNKNOWN (-1)
#define QB_CULL_MODE__NONE 0
#define QB_CULL_MODE__CLOCKWISE_ONLY 1
#define QB_CULL_MODE__ANTICLOCKWISE_ONLY 2
#define QB_INVALID_HARDWARE_HANDLE (-1)
struct qb_hardware_img_struct {
    int32_t w, h;
    int32_t texture_handle;
    int32_t dest_context_handle;
    int32_t depthbuffer_handle;
    int32_t pending_commands;
    int32_t remove;
    uint32_t* software_pixel_buffer;
    int32_t alpha_disabled;
    int32_t depthbuffer_mode;
    int32_t valid;
    struct qb_render_state_source source_state;
    struct qb_render_state_dest dest_state;
    int32_t PO2_w, PO2_h;
};
struct qb_hardware_graphics_command_struct {
    int64_t order;
    int32_t next_command;
    int64_t command;
    union { int32_t option; int32_t src_img; };
    union { int32_t dst_img; int32_t target; };
    float src_x1, src_y1, src_x2, src_y2, src_x3, src_y3;
    float dst_x1, dst_y1, dst_z1, dst_x2, dst_y2, dst_z2, dst_x3, dst_y3, dst_z3;
    int32_t smooth;
    int32_t cull_mode;
    int32_t depthbuffer_mode;
    int32_t use_alpha;
    int32_t remove;
};
#define QB_HARDWARE_GRAPHICS_COMMAND__PUTIMAGE 1
#define QB_HARDWARE_GRAPHICS_COMMAND__FREEIMAGE_REQUEST 2
#define QB_HARDWARE_GRAPHICS_COMMAND__FREEIMAGE 3
#define QB_HARDWARE_GRAPHICS_COMMAND__MAPTRIANGLE 4
#define QB_HARDWARE_GRAPHICS_COMMAND__MAPTRIANGLE3D 5
#define QB_HARDWARE_GRAPHICS_COMMAND__CLEAR_DEPTHBUFFER 6

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

/* Image color helpers (BGRA, scale, clamp, distance — libqb image.h compatibility).
 * Color format: 32-bit BGRA (0xAABBGGRR). Use for load/save image and color math. */
static inline uint8_t qb_image_get_bgra_red(uint32_t c) {
    return (uint8_t)((c >> 16) & 0xFFu);
}
static inline uint8_t qb_image_get_bgra_green(uint32_t c) {
    return (uint8_t)((c >> 8) & 0xFFu);
}
static inline uint8_t qb_image_get_bgra_blue(uint32_t c) {
    return (uint8_t)(c & 0xFFu);
}
static inline uint8_t qb_image_get_bgra_alpha(uint32_t c) {
    return (uint8_t)(c >> 24);
}
static inline uint32_t qb_image_get_bgra_bgr(uint32_t c) {
    return c & 0xFFFFFFu;
}
static inline uint32_t qb_image_set_bgra_alpha(uint32_t c, uint8_t a) {
    return (c & 0xFFFFFFu) | ((uint32_t)a << 24);
}
static inline uint32_t qb_image_make_bgra(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    return (uint32_t)b | ((uint32_t)g << 8) | ((uint32_t)r << 16) | ((uint32_t)a << 24);
}
static inline int qb_image_scale_5bits_to_8bits(int v) {
    return (v << 3) | (v >> 2);
}
static inline int qb_image_scale_6bits_to_8bits(int v) {
    return (v << 2) | (v >> 4);
}
static inline uint32_t qb_image_swap_red_blue(uint32_t clr) {
    return (clr & 0xFF00FF00u) | ((clr & 0x00FF0000u) >> 16) | ((clr & 0x000000FFu) << 16);
}
static inline uint8_t qb_image_clamp_color_component(int n) {
    if (n < 0) return 0;
    if (n > 255) return 255;
    return (uint8_t)n;
}
static inline float qb_image_calculate_rgb_distance(uint8_t r1, uint8_t g1, uint8_t b1,
    uint8_t r2, uint8_t g2, uint8_t b2) {
    float dr = (float)r2 - (float)r1, dg = (float)g2 - (float)g1, db = (float)b2 - (float)b1;
    return sqrtf(dr*dr + dg*dg + db*db);
}
static inline uint32_t qb_image_get_color_delta(uint8_t r1, uint8_t g1, uint8_t b1,
    uint8_t r2, uint8_t g2, uint8_t b2) {
    return (uint32_t)(abs((long)r1 - (long)r2) + abs((long)g1 - (long)g2) + abs((long)b1 - (long)b2));
}

/* HSB color functions (_HSB32, _HSBA32, _HUE32, _SATURATION32, _BRIGHTNESS32) */
/* Hue 0-360, Saturation/Brightness 0-100; color format &HAARRGGBB */
uint32_t qb_hsb32(float hue, float sat, float bri);
uint32_t qb_hsba32(float hue, float sat, float bri, float alpha);
float qb_hue32(uint32_t color);
float qb_saturation32(uint32_t color);
float qb_brightness32(uint32_t color);

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

/* _SAVEIMAGE filename$, handle - save image to file */
void qb_saveimage(QbString* path, int32_t handle);

/* _DEPTHBUFFER mode - depth buffer for 3D (stub: no-op) */
void qb_depthbuffer(int32_t mode);

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
int32_t qb_clipboardimage(void);
void qb_clipboardimage_set(int32_t handle);

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

/* font.h — Font (FreeType) — libqb-compatible API */
#define INVALID_FONT_HANDLE 0
#define FONT_LOAD_DONTBLEND 8
#define FONT_LOAD_MONOSPACE 16
#define FONT_LOAD_UNICODE 32
#define FONT_LOAD_AUTOMONO 64
#define FONT_RENDER_MONOCHROME 1

/* CP437 to UTF-16 BMP (index = byte 0..255, value = Unicode code point) */
extern uint16_t codepage437_to_unicode16[256];

/* Load font file into memory. Caller must free() the returned buffer. */
uint8_t* FontLoadFileToMemory(const char* file_path_name, int32_t* out_bytes);

/* Render UTF-32 codepoints to alpha buffer. *out_data is malloc'd; caller must free(). */
int32_t FontRenderTextUTF32(int32_t fh, const uint32_t* codepoint, int32_t codepoints, int32_t options, uint8_t** out_data, int32_t* out_x, int32_t* out_y);

/* Render ASCII/CP437 bytes to alpha buffer (converts to UTF-32 then renders). */
int32_t FontRenderTextASCII(int32_t fh, const uint8_t* codepoint, int32_t codepoints, int32_t options, uint8_t** out_data, int32_t* out_x, int32_t* out_y);

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

/* OpenGL _GLRENDER mode constants (_BEHIND, _ONTOP, _ONLY, _ONLYBACKGROUND) */
int32_t qb_behind(void);
int32_t qb_ontop(void);
int32_t qb_only(void);
int32_t qb_onlybackground(void);

/* OpenGL (_GLRENDER, _GLCOMPAT); when QB64FRESH_OPENGL, call_gl* and sub_gl_called are available */
void qb_glrender(int32_t mode);
int32_t qb_glcompat(void);

#ifdef QB64FRESH_OPENGL
/* Set by runtime when invoking SUB _GL; call_gl* wrappers check this (error 270 if used outside SUB _GL) */
extern int sub_gl_called;

/* Register SUB _GL callback. Call at startup with &qb_sub__gl so the runtime invokes it each frame when _GLRENDER is active. */
void qb_gl_register_sub_gl(void (*cb)(void));

/* OpenGL wrappers (implemented in gl_wrappers.c when runtime built with opengl feature) */
void call_glBegin(uint32_t mode);
void call_glEnd(void);
void call_glVertex3f(float x, float y, float z);
void call_glClear(uint32_t mask);
void call_glClearColor(float r, float g, float b, float a);
void call_glColor3f(float r, float g, float b);
void call_glColor4f(float r, float g, float b, float a);
void call_glMatrixMode(uint32_t mode);
void call_glLoadIdentity(void);
void call_glEnable(uint32_t cap);
void call_glDisable(uint32_t cap);
void call_glBlendFunc(uint32_t sfactor, uint32_t dfactor);
void call_glDepthMask(uint32_t flag);
void call_glAlphaFunc(uint32_t func, float ref);
void call_glTexParameteri(uint32_t target, uint32_t pname, uint32_t param);
void call_glBindTexture(uint32_t target, uint32_t texture);
void call_glTexCoord2f(float s, float t);
void call_glTranslatef(float x, float y, float z);
void call_glRotatef(float angle, float x, float y, float z);
void call_glTexImage2D(uint32_t target, int32_t level, int32_t internalformat, int32_t width, int32_t height, int32_t border, uint32_t format, uint32_t type, const void* pixels);
void call_glGenTextures(int32_t n, void* textures);
void call_gluPerspective(double fovy, double aspect, double zNear, double zFar);
#endif

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

/* _WAVE - waveform type constant / wave output device (returns 0 = default) */
int32_t qb_wave(void);

/* _SNDNEW - create new sound buffer; returns handle or -1 */
int32_t qb_sndnew(int32_t frames, int32_t channels, int32_t bits);

/* _MIDISOUNDBANK - set MIDI sound bank (soundfont) file path */
void qb_midisoundbank(const QbString* filename);

/* ============================================================================
 * libqb event.h — Event type constants and custom event callback
 * ============================================================================
 * Used by graphics/window code to report close, key, relative mouse, file drop.
 * qb64_custom_event returns 0 if event was handled, -1 if unknown/unhandled.
 */
#define QB64_EVENT_CLOSE 1
#define QB64_EVENT_KEY 2
#define QB64_EVENT_RELATIVE_MOUSE_MOVEMENT 3
#define QB64_EVENT_FILE_DROP 4

/* Custom event callback (libqb qb64_custom_event).
 * event: one of QB64_EVENT_*; v1..v8 and p1, p2 are event-specific.
 * Returns: 0 = handled, -1 = unknown or unhandled.
 */
int qb64_custom_event(int event, int v1, int v2, int v3, int v4, int v5, int v6, int v7, int v8, void* p1, void* p2);

/* Set by qb64_custom_event(QB64_EVENT_CLOSE,...). Main loop should check and exit. */
extern int32_t qb64_exit_requested;

/* ============================================================================
 * Event Trapping (ON KEY, ON TIMER, ON UEVENT)
 * ============================================================================ */

/* Event handler registration
 * These functions register label addresses (computed goto targets) for event handlers.
 * The generated C code checks for events and jumps to these labels.
 */

/* ON KEY(n) GOSUB - Register keyboard event handler
 * key_num: Key number (1-31, or 0 to disable)
 * target: Label address (computed goto target)
 */
void qb_on_key(int32_t key_num, void* target);

/* KEY(n) ON/OFF/STOP - Control key event trapping
 * key_num: Key number
 * mode: 0 = OFF, 1 = ON, 2 = STOP (suspended)
 */
void qb_key_control(int32_t key_num, int mode);

/* ON TIMER(n) GOSUB - Register timer event handler
 * interval: Timer interval in seconds
 * target: Label address (computed goto target)
 */
void qb_on_timer(float interval, void* target);

/* TIMER ON/OFF/STOP - Control timer event trapping
 * mode: 0 = OFF, 1 = ON, 2 = STOP (suspended)
 */
void qb_timer_control(int mode);

/* ON UEVENT GOSUB - Register user event handler
 * target: Label address (computed goto target)
 */
void qb_on_uevent(void* target);

/* UEVENT ON/OFF/STOP - Control user event trapping
 * mode: 0 = OFF, 1 = ON, 2 = STOP (suspended)
 */
void qb_uevent_control(int mode);

/* UEVENT - Trigger a user event */
void qb_uevent_trigger(void);

/* Event checking functions (called from generated C code)
 * These return event flags that the C code checks before jumping to handlers.
 */

/* Check for pending key event
 * Returns: Key number (1-31) if event pending, 0 if none
 */
int32_t qb_check_key_event(void);

/* Check for pending timer event
 * Returns: 1 if timer event pending, 0 if none
 */
int qb_check_timer_event(void);

/* Check for pending user event
 * Returns: 1 if user event pending, 0 if none
 */
int qb_check_uevent(void);

/* Get handler label for events (for computed goto)
 * These return the label address to jump to, or NULL if no handler.
 * WARNING: These are unsafe - only use with computed goto in generated C code.
 */

/* Get handler label for key event
 * key_num: Key number
 * Returns: Label address or NULL
 */
void* qb_get_key_handler(int32_t key_num);

/* Get handler label for timer event
 * Returns: Label address or NULL
 */
void* qb_get_timer_handler(void);

/* Get handler label for user event
 * Returns: Label address or NULL
 */
void* qb_get_uevent_handler(void);

/* Clear all event handlers (called on RUN) */
void qb_events_clear_all(void);

/* Legacy event stubs (ON COM, ON PEN, ON SIGNAL) — not implemented; warn once, no-op.
 * Provided so generated code linking against libqb64fresh_rt (--runtime external)
 * resolves these symbols. Inline runtime emits equivalent C stubs. */

/* ON COM(n) GOSUB — serial port event trapping (stub) */
void qb_on_com(int32_t port_num, void* target);
/* COM(n) ON/OFF/STOP — control serial port event trapping (stub) */
void qb_com_control(int32_t port_num, int mode);

/* ON PEN GOSUB — light pen event trapping (stub) */
void qb_on_pen(void* target);
/* PEN ON/OFF/STOP — control light pen event trapping (stub) */
void qb_pen_control(int mode);

/* ON SIGNAL(n) GOSUB — BASIC signal trapping (stub) */
void qb_on_signal(int32_t signal_num, void* target);
/* SIGNAL(n) ON/OFF/STOP — control signal trapping (stub) */
void qb_signal_control(int32_t signal_num, int mode);

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
