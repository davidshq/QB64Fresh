# libqb Functionality — Completed (🟢)

Items below are implemented in QB64Fresh (🟢 or 🟢 stub). See [LIBQB_FUNCTIONALITY.md](LIBQB_FUNCTIONALITY.md) for partial (🟡) and not implemented (🔴).

---

## 1. Main libqb.h (Console / Display)


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__source()` | Current source (image/page) | 🟢 |
| `func_pos(int32)` | Cursor position (POS) | 🟢 |
| `func_timer(double, int32_t)` | Timer (TIMER) | 🟢 |
| `func__newimage(int32 x, y, bpp, passed)` | Create new image | 🟢 |
| `display()` | Refresh display | 🟢 |
| `sub__dest(int32)` | Set destination | 🟢 |
| `sub__source(int32)` | Set source | 🟢 |
| `func__printwidth(qbs*, int32, int32)` | Print width | 🟢 |
| `sub_clsDest(...)` | CLS to destination | 🟢 |
| `sub_cls(...)` | CLS | 🟢 |
| `qbs_print(qbs*, int32)` | Print string | 🟢 |
| `func__copyimage(int32, int32, int32)` | Copy image | 🟢 |
| `sub__freeimage(int32, int32)` | Free image | 🟢 |
| `func__dest()` | Current destination | 🟢 |
| `func__display()` | Current display handle | 🟢 |
| `func_space(int32_t)` | SPACE$ | 🟢 |
| `qbg_sub_window(float, float, float, float, int32)` | WINDOW | 🟢 |
| `autodisplay` | Auto-display flag | 🟢 |
| `keyhit[8192]`, `keyhit_nextfree`, `keyhit_next` | Key hit ring buffer | 🟢 |
| `sub__printimage(int32)` | Print image to console | 🟢 (stub: `qb_printimage`) |
| `validatepage(int32)` | Validate current page | 🟢 (stub) |
| `qbg_sub_view_print(...)` | VIEW PRINT | 🟢 (calls `qb_view_print` / reset) |
| `makefit(qbs*)` | Fit text to width | 🟢 (inline: wrap; external: stub) |
| `port60h_event[]`, `port60h_events` | Keyboard port events | 🟢 (stub; INP(0x60) reads queue) |
| `window_exists`, `no_control_characters2` | Window/control state | 🟢 (stubs: 1, 0) |

## 2. cmem.h — Conventional Memory

| Symbol | Description | QB64Fresh |
|--------|-------------|-----------|
| `cmem[1114099]` | Conventional memory block (DBLOCK) | 🟢 |
| `dblock` | Required for Play() | 🟢 |

## 3. qbs.h — QB64 String Type and String Ops

**Structures:** QB64Fresh provides **wrapper types** `struct qbs_field` and `struct qbs` with the same layout as QB64pe so old code can use them. Under the hood we use opaque `QbString*`. Use `qbs_from_qb_string(QbString*)` to wrap QbString* as qbs*; `qbs_free(qbs*)` releases; `qb_string_from_qbs(qbs*)` returns the underlying QbString* (retained). Old code that reads `q->chr` / `q->len` works. See `runtime/include/qb64fresh_rt.h` and `runtime/src/qbs_compat.rs`.

| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `qbs_new(int32_t, uint8_t)` | New string | 🟢 |
| `qbs_new_txt(const char*)` | From C string | 🟢 |
| `qbs_new_txt_len(const char*, int32_t)` | From C string with length | 🟢 |
| `qbs_add`, `qbs_set` | Concatenate / assign | 🟢 |
| `qbs_free(qbs*)` | Free string | 🟢 |
| `qbs_val<T>(qbs*)` | Parse value from string | 🟢 |
| `qbs_str(...)` | STR$ (multiple overloads: int/float/double, etc.) | 🟢 |
| `qbs__tostr(...)` | _TOSTR$ (with digits/passed) | 🟢 |
| `func_chr(int32_t)` | CHR$ | 🟢 |
| `qbs_ucase`, `qbs_lcase` | UCase$, LCASE$ | 🟢 |
| `qbs_left`, `qbs_right` | LEFT$, RIGHT$ | 🟢 |
| `qbs_equal`, `qbs_notequal`, `qbs_greaterthan`, `qbs_lessthan`, `qbs_lessorequal`, `qbs_greaterorequal` | String compare | 🟢 |
| `qbs_asc(qbs*, uint32_t)` / `qbs_asc(qbs*)` | ASC | 🟢 |
| `qbs_len(qbs*)` | LEN (inline) | 🟢 |
| `sub_lset`, `sub_rset` | LSET, RSET | 🟢 |
| `func_space(int32_t)` | SPACE$ | 🟢 |
| `func_string(int32_t, int32_t)` | STRING$ | 🟢 |
| `func_instr(...)` | INSTR | 🟢 |
| `func__instrrev(...)` | _INSTRREV | 🟢 |
| `sub_mid`, `func_mid` | MID$ (sub and function) | 🟢 |
| `qbs_ltrim`, `qbs_rtrim`, `qbs__trim` | _LTRIM$, _RTRIM$, _TRIM$ | 🟢 |
| `func__str_nc_compare`, `func__str_compare` | String compare (case) | 🟢 |
| `qbs_new_cmem(...)` | In conventional memory | 🟢 `qb_string_new_cmem(int32_t size)` |
| `qbs_new_fixed(uint8_t*, uint32_t, uint8_t)` | Fixed-length string | 🟢 `qb_string_new_fixed(const uint8_t* ptr, uint32_t size)` |
| `set_qbs_size(...)` | Set string size (vWatch) | 🟢 `qb_string_set_size(QbString** target, int32_t newlength)` |
| `qbs_tmp_list`, `qbs_tmp_list_lasti`, `qbs_tmp_list_nexti` | Temp string list | 🟢 Equivalent: `_qbs_tmp_pool`, `_qbs_tmp_next`, overflow array |
| `qbs_cleanup<T>(uint32_t, T)` | Cleanup temp strings | 🟢 `qbs_cleanup(uint64_t base, int dummy)` in generated code; pool is inline |
| `qbs_cmem_sp` | cmem string pool | 🟢 N/A — no cmem string pool in same form; cmem via `qb_string_new_cmem` |

## 4. buffer.h — Generic Buffer

| Function | Description | QB64Fresh |
|----------|-------------|-----------|
| `libqb_buffer_init` | Initialize buffer | 🟢 |
| `libqb_buffer_clear` | Clear/free buffer | 🟢 |
| `libqb_buffer_length` | Current length | 🟢 |
| `libqb_buffer_read` | Read (and consume) bytes | 🟢 |
| `libqb_buffer_write` | Append bytes | 🟢 |

## 5. command.h — Command Line


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func_command_str` | Command string | 🟢 |
| `func_command(int32_t, int32_t)` | COMMAND$ (by index) | 🟢 |
| `func__commandcount()` | _COMMANDCOUNT | 🟢 |
| `command_initialize(argc, argv)` | Init from main args | 🟢 |

## 6. datetime.h — Time and Delay


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `clock_init()` | Init tick clock (Linux/Windows) | 🟢 |
| `GetTicks()` | Tick count | 🟢 |
| `func_timer(double, int32_t)` | TIMER | 🟢 |
| `sub__delay(double)` | _DELAY | 🟢 |
| `sub__limit(double)` | _LIMIT (FPS) | 🟢 |
| `Sleep(uint32_t)` | Sleep ms (non-Windows) | 🟢 |
| `func_time()`, `sub_time(qbs*)` | TIME$, TIME statement | 🟢 |
| `func_date()`, `sub_date(qbs*)` | DATE$, DATE statement | 🟢 |

## 7. encoding.h — Base64


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__base64encode(qbs*)` | _BASE64ENCODE | 🟢 |
| `func__base64decode(qbs*)` | _BASE64DECODE | 🟢 |

## 8. environ.h — Environment


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__environcount()` | _ENVIRONCOUNT | 🟢 |
| `func_environ(qbs*)` / `func_environ(int32_t)` | ENVIRON$ (by name or index) | 🟢 |
| `sub_environ(qbs*)` | ENVIRON statement | 🟢 |

## 9. error_handle.h — Errors and RESUME


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `error(int32_t)` | Raise error | 🟢 |
| `fix_error()` | Process error state (unhandled: report and exit) | 🟢 `qb_fix_error()` |
| `error_handler_history`, `error_handling`, `error_retry` | Error handling state | 🟢 `qb_error_handler_history_get/set`, `qb_error_handling_get/set`, `qb_error_retry_get/set` |
| `QB_ERROR_*` | Error code constants (1–76, 256–260, 270–271, 300–315, 502–518) | 🟢 In header and inline runtime |
| `new_error`, `error_err`, `error_occurred`, `error_goto_line` | Error state | 🟢 |
| `is_error_pending()` | Check if error pending | 🟢 |
| `clear_error()` | Clear error | 🟢 |
| `get_error_erl()`, `get_error_err()` | ERDEV/ERL/ERR helpers | 🟢 |
| `func__errorline()`, `func__inclerrorline()`, `func__inclerrorfile()` | Error location | 🟢 |
| `func__errormessage(int32_t, int32_t)` | _ERRORMESSAGE | 🟢 |
| `error_set_line(...)` | Set error line info | 🟢 |

## 10. event.h — Events


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `evnt(...)` | Process events (C++ overload with defaults) | 🟢 |
| `new_error`, `qbevent` | Error/event state | 🟢 |
| `QB64_EVENT_CLOSE`, `QB64_EVENT_KEY`, `QB64_EVENT_RELATIVE_MOUSE_MOVEMENT`, `QB64_EVENT_FILE_DROP` | Event type constants | 🟢 `qb64fresh_rt.h` |
| `qb64_custom_event(...)` | Custom event callback | 🟢 `qb64_custom_event`; CLOSE sets `qb64_exit_requested` |

## 11. file-fields.h — FIELD (Random-Access Files)


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `lrset_field(qbs*)` | LSET/RSET for field | 🟢 |
| `field_new(int32_t)` | New field set for file | 🟢 |
| `field_add(qbs*, int64_t)` | Add field to file | 🟢 |
| `field_get(int32_t, int64_t, int32_t)` | Get field | 🟢 |
| `field_put(int32_t, int64_t, int32_t)` | Put field | 🟢 |
| `field_free(qbs*)` | Free field buffer | 🟢 `field_free(QbString*)` (stub: no-op; no per-string field tracking) |
| `field_update(int32_t)` | Sync field to file | 🟢 `field_update(int32_t)` (stub: no-op until field-variable tracking) |

## 13. filesystem.h — Directories and Files


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `FS_SaveStartDirectory()` | Save startup directory | 🟢 |
| `FS_DirectoryExists`, `FS_FileExists` | Existence checks | 🟢 |
| `func__cwd()` | _CWD$ | 🟢 |
| `func__dir(qbs*)` | _DIR$ (directory listing context) | 🟢 |
| `func__direxists(qbs*)` | _DIREXISTS | 🟢 |
| `func__fileexists(qbs*)` | _FILEEXISTS | 🟢 |
| `func__startdir()` | _STARTDIR$ | 🟢 |
| `sub_chdir(qbs*)` | CHDIR | 🟢 |
| `func__files(qbs*, int32_t)` | _FILES$ (file spec, one entry per call) | 🟢 |
| `func__fullpath(qbs*)` | _FULLPATH$ | 🟢 |
| `sub_files(qbs*, int32_t)` | FILES statement | 🟢 |
| `sub_kill(qbs*)` | KILL | 🟢 |
| `sub_mkdir(qbs*)` | MKDIR | 🟢 |
| `sub_name(qbs* old, qbs* new)` | NAME | 🟢 |
| `sub_rmdir(qbs*)` | RMDIR | 🟢 |

## 12. filepath.h — Path Utilities (C++)

| Function | Description | QB64Fresh |
|----------|-------------|-----------|
| `filepath_get_filename(const char*)` | Filename part of path | 🟢 |
| `filepath_get_extension(const char*)` | Extension part | 🟢 |
| `filepath_has_extension(const char*, const char*)` | Match extension | 🟢 |
| `filepath_fix_directory(char*)` | Normalize path separators (in-place) | 🟢 |
| `filepath_fix_directory_copy(const char*)` | Normalize path (returns new QbString*) | 🟢 |
| `filepath_split(path, QbString** dir, QbString** file)` | Split path into dir + filename | 🟢 |
| `filepath_join(const char* dir, const char* file)` | Join directory and filename | 🟢 |

## 14. gfs.h — Generic File System (OPEN/CLOSE/GET/PUT/LOF/SEEK/LOCK)


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `gfs_eof_passed`, `gfs_eof_reached` | EOF state | 🟢 |
| `gfs_getpos`, `gfs_setpos` | Position | 🟢 |
| `gfs_fileno_valid`, `gfs_fileno_freefile`, `gfs_fileno_use` | File number ↔ GFS index | 🟢 |
| `gfs_open`, `gfs_close` | Open/close | 🟢 |
| `gfs_lof` | LOF | 🟢 |
| `gfs_write`, `gfs_read` | Write/read bytes | 🟢 |
| `gfs_read_bytes()` | Last read size | 🟢 `qb_gfs_read_bytes()` |
| `gfs_get_fileno`, `gfs_get_file_struct` | By fileno | 🟢 `gfs_get_fileno`, `gfs_get_file_struct` (struct filled from internal state); `qb_gfs_get_fileno`, `qb_gfs_get_file_struct` aliases |
| `gfs_lock`, `gfs_unlock` | Lock region | 🟢 |
| `gfs_close_all_files` | Close all | 🟢 |

**Structures:** `gfs_file_struct` (file handle, mode, position, FIELD, COM, SCRN). QB64Fresh provides a **wrapper struct** with the same layout as QB64pe. `gfs_get_file_struct(fileno)` fills and returns a pointer to the struct for that fileno (NULL if invalid/closed). `gfs_get_fileno(file_number)` returns the file number. Under the hood we use opaque file handles; the struct is filled on demand from internal state. See `runtime/include/qb64fresh_rt.h` and `runtime/src/io/file.rs`.

## 15. graphics.h — Graphics State and 3D Helpers

**Structures (libqb compatibility):** `qb_img_struct`, `qb_hsb_color`, `qb_rgb_color`, `qb_render_state_*`, `qb_hardware_img_struct`, `qb_hardware_graphics_command_struct`; constants `QB_VIEW_MODE_*`, `QB_ALPHA_MODE_*`, `QB_DEPTHBUFFER_MODE_*`, `QB_CULL_MODE_*`, `QB_HARDWARE_GRAPHICS_COMMAND_*` in `qb64fresh_rt.h`.

| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__hsb32`, `func__hsba32` | HSB → _RGB32 | 🟢 |
| `func__hue32`, `func__sat32`, `func__bri32` | _RGB32 → hue/sat/bri | 🟢 |
| `sub__depthbuffer(...)` | _DEPTHBUFFER | 🟢 (stub) |
| `sub__maptriangle(...)` | _MAPTRIANGLE / _MAPTRIANGLE3D | 🟢 |

## 16. gui.h — Dialogs (tinyfiledialogs)


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__guiMessageBox(...)` / `sub__guiMessageBox(...)` | _GUIMESSAGEBOX | 🟢 |
| `func__guiSelectFolderDialog(...)` | _GUISELECTFOLDERDIALOG | 🟢 |
| `func__guiOpenFileDialog(...)` | _GUIOPENFILEDIALOG | 🟢 |
| `func__guiSaveFileDialog(...)` | _GUISAVEFILEDIALOG | 🟢 |
| `sub__guiNotifyPopup(...)` | _GUINOTIFYPOPUP / _NOTIFYPOPUP | 🟢 |
| `func__guiInputBox(...)` | _GUIINPUTBOX / _INPUTBOX$ | 🟢 |
| `func__guiColorChooserDialog(...)` | _GUICOLORCHOOSERDIALOG | 🟢 |
| `gui_alert(...)` | C alert (equivalent: qb_messagebox_ex) | 🟢 |

## 17. hashing.h — Checksums and Hash


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__adler32(qbs*)` | _ADLER32 | 🟢 |
| `func__crc32(qbs*)` | _CRC32 | 🟢 |
| `func__md5(qbs*)` | _MD5$ | 🟢 |

## 18. hexoctbin.h — Radix String Conversion


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__bin(int64_t, int32_t)` | _BIN$ | 🟢 |
| `func__bin_float(long double)` | _BIN$ (float) | 🟢 |
| `func_oct(int64_t, int32_t)` | OCT$ | 🟢 |
| `func_oct_float(long double)` | OCT$ (float) | 🟢 |
| `func_hex(int64_t, int32_t)` | HEX$ | 🟢 |
| `func_hex_float(long double)` | HEX$ (float) | 🟢 |

## 19. http.h — HTTP Client (libcurl)

| Function | Description | QB64Fresh |
|----------|-------------|-----------|
| `libqb_http_init()`, `libqb_http_stop()` | Init/stop HTTP | 🟢 |
| `libqb_http_open(url, handle)` | Open URL (blocking GET, reqwest) | 🟢 |
| `libqb_http_close(handle)` | Close handle | 🟢 |
| `libqb_http_connected(handle)` | Connected? (1/0/-1) | 🟢 |
| `libqb_http_get_length(handle, size_t*)` | Bytes available | 🟢 |
| `libqb_http_get_content_length(handle, uint64_t*)` | Content-Length header | 🟢 |
| `libqb_http_get_status_code(handle)` | HTTP status | 🟢 |
| `libqb_http_get_url(handle)` | Effective URL (valid until close) | 🟢 |
| `libqb_http_get(handle, buf, size_t*)` | Read bytes | 🟢 |
| `libqb_http_get_fixed(handle, buf, size_t)` | Read exact bytes | 🟢 |

## 20. image.h — Load/Save Images and Color


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__loadimage(qbs*, int32_t, qbs*, int32_t)` | _LOADIMAGE | 🟢 |
| `sub__saveimage(qbs*, int32_t, qbs*, int32_t)` | _SAVEIMAGE | 🟢 |
| `func__rgb32(...)` (several overloads) | _RGB32 | 🟢 |
| `func__rgba32(...)` | _RGBA32 | 🟢 |
| `func__alpha32`, `func__red32`, `func__green32`, `func__blue32` | _ALPHA32, _RED32, _GREEN32, _BLUE32 | 🟢 |
| **Inline helpers** | BGRA get/set, scale, swap, clamp, distance | 🟢 |
| `qb_image_get_bgra_red/green/blue/alpha/bgr` | Extract BGRA components from uint32 | 🟢 |
| `qb_image_set_bgra_alpha`, `qb_image_make_bgra` | Set alpha, build BGRA from r,g,b,a | 🟢 |
| `qb_image_scale_5bits_to_8bits`, `qb_image_scale_6bits_to_8bits` | Scale bit depth | 🟢 |
| `qb_image_swap_red_blue`, `qb_image_clamp_color_component` | Swap R/B, clamp 0–255 | 🟢 |
| `qb_image_calculate_rgb_distance`, `qb_image_get_color_delta` | Euclidean distance, sum of abs diffs | 🟢 |

## 21. keyhandler.h — Keyboard State

| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `keyheld(uint32_t)` | Key currently held | 🟢 (`qb_keydown`) |
| `QBK`, `VK`, `UC` | Key code bases (200000, 100000, 1073741824) | 🟢 In `qb64fresh_rt.h` |
| `QBVK_*` | Virtual key codes (QBVK_ESCAPE, QBVK_F1–F12, arrows, etc.) | 🟢 In `qb64fresh_rt.h` |
| `KMOD_*` | Key modifiers (KMOD_SHIFT, KMOD_CTRL, KMOD_ALT, etc.) | 🟢 In `qb64fresh_rt.h` |
| `keydown_vk` / `keyup_vk` | Simulate key down/up | 🟢 Stub (no-op); BASIC `_KEYUP`/`_KEYDOWN` statement → `qb_keyup_vk`/`qb_keydown_vk` |

## 22. logging.h — Scoped Logging

| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `loglevel`, `logscope` | Level and scope enums | 🟢 `QB_LOGLEVEL_*`, `QB_LOGSCOPE_*` |
| `libqb_log_init`, `libqb_log`, `libqb_log_qb64`, `libqb_log_qbs` | Scoped log (file, func, line) | 🟢 |
| `func__logminlevel()` | Minimum log level (1–4) | 🟢 `qb_logminlevel` |
| `sub__logtrace/info/warn/error` | QB64 (file, func, line, qbs) | 🟢 `qb_logtrace` etc. |
| `libqb_log_trace/info/warn/error` | Libqb scope macros | 🟢 |

## 23. mem.h — _MEM (Safe Memory Blocks)

QB64Fresh provides both `qb_mem` (primary API) and libqb-compatible `mem_block`/`mem_lock` with lock lifecycle and globals for porting code that uses them.

| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| **Structures** | `mem_block`, `mem_lock`; `MEM_TYPE_*`, `INVALID_MEM_LOCK` | 🟢 In `qb64fresh_rt.h` |
| `new_mem_lock()`, `free_mem_lock(struct mem_lock*)` | Lock lifecycle | 🟢 Runtime `mem_lock.rs` |
| `mem_lock_id`, `mem_lock_tmp`, `mem_lock_base` | Lock globals | 🟢 Exported from runtime |
| `func__memexists(void*)` | _MEMEXISTS | 🟢 `qb_memexists` |
| `sub__memfill*` | _MEMFILL (multiple types) | 🟢 `qb_memfill` |
| `func__memget(mem_block*, intptr_t, intptr_t)` | _MEMGET | 🟢 `qb_memget` |
| `func__mem(...)` | _MEM (block from offset/size/type) | 🟢 `qb_mem_of` |
| `func__mem_at_offset(...)` | _MEM at _OFFSET | 🟢 |
| `func__memnew(intptr_t)` | _MEMNEW | 🟢 `qb_memnew` |
| `sub__memfree(void*)` | _MEMFREE | 🟢 `qb_memfree` |
| `sub__memcopy(...)` | _MEMCOPY | 🟢 `qb_memcopy` |

## 24. qblist.h — Handle List (Thread-Safe List)

| Function | Description | QB64Fresh |
|----------|-------------|-----------|
| `list_new(intptr_t)` | New list | 🟢 |
| `list_new_threadsafe(intptr_t)` | New list with mutex | 🟢 |
| `list_destroy(list*)` | Destroy | 🟢 |
| `list_add(list*)` | Add entry | 🟢 |
| `list_remove(list*, intptr_t)` | Remove by index | 🟢 |
| `list_get(list*, intptr_t)` | Get pointer by index | 🟢 |
| `list_get_index(list*, void*)` | Get index from pointer | 🟢 |

## 25. qbmath.h — Math (with Error Handling)


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func_log(double)` | LOG (error 5 if ≤0) | 🟢 |
| `func_fix_double`, `func_fix_float` | FIX (truncate toward zero) | 🟢 |
| `func_exp_single`, `func_exp_float` | EXP (overflow error 6) | 🟢 |
| `func_sqr(double)` | SQR (error 5 if &lt;0) | 🟢 |
| `pow2(long double, long double)` | Power (negative base + non-integer exponent → error 5) | 🟢 |
| `func_abs<T>` | ABS | 🟢 |
| `func_sgn<T>` | SGN | 🟢 |

## 26. rounding.h — Type Conversion and Rounding


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `qbr_longdouble_to_uint64`, `qbr_float_to_long`, `qbr_double_to_long` | Float → int round | 🟢 |
| `fpu_reinit()` | Reset FPU rounding mode | 🟢 |
| `func_cint_*` | CINT (various types) | 🟢 |
| `func_clng_*` | CLNG (various types) | 🟢 |

## 27. shell.h — SHELL


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func_shell(qbs*)` | SHELL (function) | 🟢 |
| `func__shellhide(qbs*)` | _SHELLHIDE | 🟢 |
| `sub_shell`, `sub_shell2`, `sub_shell3`, `sub_shell4` | SHELL statement variants | 🟢 |
| `shell_call_in_progress` | Shell active flag (1 while SHELL is active) | 🟢 |

## 28. thread.h — Threads

| Type / function | Description | QB64Fresh |
|-----------------|-------------|-----------|
| `libqb_thread` | Opaque thread handle | 🟢 |
| `libqb_thread_new()`, `libqb_thread_free()` | Create/destroy | 🟢 |
| `libqb_thread_start(thread, void (*)(void*), void*)` | Start thread | 🟢 |
| `libqb_thread_join(thread)` | Join thread | 🟢 |

## 29. audio.h — Beep, PLAY, SOUND, _SND*


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `sub_beep()` | BEEP | 🟢 |
| `func_play(uint32_t, int32_t)` | PLAY (function) | 🟢 |
| `sub_play(...)` | PLAY statement | 🟢 |
| `sub_sound(...)` | SOUND | 🟢 |
| `func__sndrate()` | _SNDRATE | 🟢 |
| `func__sndopen`, `sub__sndclose`, `func__sndcopy` | _SNDOPEN / _SNDCLOSE / _SNDCOPY | 🟢 |
| `sub__sndplay`, `sub__sndplaycopy`, `sub__sndplayfile` | _SNDPLAY / _SNDPLAYCOPY / _SNDPLAYFILE | 🟢 |
| `sub__sndpause`, `func__sndplaying`, `func__sndpaused` | _SNDPAUSE / _SNDPLAYING / _SNDPAUSED | 🟢 |
| `sub__sndvol`, `sub__sndloop`, `sub__sndbal` | _SNDVOL / _SNDLOOP / _SNDBAL | 🟢 |
| `func__sndlen`, `func__sndgetpos`, `sub__sndsetpos`, `sub__sndlimit`, `sub__sndstop` | _SNDLEN / _SNDGETPOS / _SNDSETPOS / _SNDLIMIT / _SNDSTOP | 🟢 |
| `func__sndopenraw`, `sub__sndraw`, `sub__sndrawbatch`, `sub__sndrawdone` | _SNDOPENRAW / _SNDRAW / _SNDRAWBATCH / _SNDRAWDONE | 🟢 |
| `func__sndrawlen` | _SNDRAWLEN | 🟢 |
| `func__memsound` | _MEMSOUND | 🟢 |
| `snd_update()` | Audio update (internal) | 🟢 |
| `sub__wave(...)` / func form | _WAVE (function: device/constant) | 🟢 |
| `func__sndnew` | _SNDNEW | 🟢 (stub) |
| `sub__midisoundbank` | _MIDISOUNDBANK | 🟢 (stub) |

## 30. bitops.h — Bit Operations


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__rol<T>`, `func__ror<T>` | _ROL, _ROR | 🟢 |
| `func__shl`, `func__shr` | _SHL, _SHR | 🟢 |
| `func__readbit`, `func__setbit`, `func__resetbit`, `func__togglebit` | _READBIT / _SETBIT / _RESETBIT / _TOGGLEBIT | 🟢 |
| `getubits`, `getbits`, `setbits` | Bit field get/set (qb_getubits, qb_getbits, qb_setbits) | 🟢 |

## 31. clipboard.h — Clipboard


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__clipboard()` | _CLIPBOARD$ (get) | 🟢 |
| `sub__clipboard(const qbs*)` | _CLIPBOARD (set) | 🟢 |
| `func__clipboardimage()` | _CLIPBOARDIMAGE (get) | 🟢 |
| `sub__clipboardimage(int32_t)` | _CLIPBOARDIMAGE (set from image handle) | 🟢 |

## 33. compression.h — Deflate/Inflate


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func__deflate(qbs*, int32_t level, int32_t passed)` | _DEFLATE | 🟢 |
| `func__inflate(qbs*, int64_t, int32_t)` | _INFLATE | 🟢 |

## 34. condvar.h — Condition Variable

| Type / function | Description | QB64Fresh |
|-----------------|-------------|-----------|
| `libqb_condvar` | Opaque condvar | 🟢 |
| `libqb_condvar_new()`, `libqb_condvar_free()` | Create/destroy | 🟢 |
| `libqb_condvar_wait(condvar, mutex)` | Wait | 🟢 |
| `libqb_condvar_signal`, `libqb_condvar_broadcast` | Signal one / all | 🟢 |

## 32. completion.h — One-Shot Completion (Thread Sync)

| Type / function | Description | QB64Fresh |
|-----------------|-------------|-----------|
| `struct completion` | finished, mutex, var | 🟢 |
| `completion_init`, `completion_clear` | Init/clear | 🟢 |
| `completion_wait` | Block until finished | 🟢 |
| `completion_finish` | Signal finished | 🟢 |

## 35. extended_math.h — Angles, Pi, Trig, Clamp, Power-of-2


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `func_deg2rad`, `func_rad2deg` | Degree ↔ radian | 🟢 |
| `func_deg2grad`, `func_grad2deg`, `func_rad2grad`, `func_grad2rad` | Degree ↔ grad | 🟢 |
| `func_pi(double, int32_t)` | PI (optional multiplier) | 🟢 |
| `func_arcsec`, `func_arccsc`, `func_arccot` | Arcsec, arccsc, arccot | 🟢 |
| `func_sech`, `func_csch`, `func_coth` | Hyperbolic sec/csc/cot | 🟢 |
| `func_sec`, `func_csc`, `func_cot` | Sec, csc, cot | 🟢 |
| `func_clamp<T>(value, limit1, limit2)` | Clamp (handles reversed limits) | 🟢 |
| `Math_IsPowerOf2<T>` | Power-of-2 test | 🟢 `qb_math_is_power_of_2_u32/u64` |
| `Math_RoundUpToPowerOf2<T>`, `Math_RoundDownToPowerOf2<T>` | Round to power of 2 | 🟢 `qb_math_round_up/down_to_power_of_2_u32/u64` |

## 36. mutex.h — Mutex

| Type / function | Description | QB64Fresh |
|-----------------|-------------|-----------|
| `libqb_mutex` | Opaque mutex | 🟢 |
| `libqb_mutex_new()`, `libqb_mutex_free()` | Create/destroy | 🟢 |
| `libqb_mutex_lock`, `libqb_mutex_unlock` | Lock/unlock | 🟢 |
| `libqb_mutex_guard` | RAII lock guard (C: LIBQB_MUTEX_GUARD macro; C++: class) | 🟢 |

## 37. font.h — Font (FreeType)

**Constants:** `INVALID_FONT_HANDLE`, `FONT_LOAD_*`, `FONT_RENDER_*`. **Symbol:** `codepage437_to_unicode16[]`.

| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `INVALID_FONT_HANDLE`, `FONT_LOAD_*`, `FONT_RENDER_*` | Font constants | 🟢 (in qb64fresh_rt.h) |
| `codepage437_to_unicode16[256]` | CP437 → UTF-16 BMP | 🟢 (cp437.rs) |
| `FontLoadFileToMemory` | Load font file to memory (caller free()) | 🟢 (font_ffi.rs; search paths) |
| `FontRenderTextUTF32` | Render UTF-32 codepoints to alpha buffer | 🟢 (font_ffi + font_manager) |
| `FontRenderTextASCII` | Render ASCII/CP437 to alpha buffer | 🟢 (font_ffi; converts via CP437) |
| `FontLoad`, `FontFree` | Load/free font | 🟢 |
| `FontWidth` | Font width | 🟢 |
| `FontPrintWidthUTF32`, `FontPrintWidthASCII` | Measure width | 🟢 |
| `func__UFontHeight` | _FONTHEIGHT (UTF) | 🟢 |
| `func__UPrintWidth` | _PRINTWIDTH (UTF) | 🟢 |
| `func__ULineSpacing` | _LINESPACING | 🟢 |
| `sub__UPrintString` | _PRINTSTRING (UTF) | 🟢 |
| `func__UCharPos` | _CHARPOS (UTF) | 🟢 |

## 38. game_controller.h — Gamepad / Keyboard / Mouse Devices


| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `QB64_GAMEPAD_INIT`, `QB64_GAMEPAD_POLL`, `QB64_GAMEPAD_SHUTDOWN` | Init/poll/shutdown | 🟢 |

## 39. libqb-common.h — Platform and Types

**Macros:** `QB64_WINDOWS`, `QB64_LINUX`, `QB64_MACOSX`, `QB64_UNIX`, `QB64_BACKSLASH_FILESYSTEM`, `QB64_MICROSOFT`, `QB64_GCC`, `QB64_MINGW`, `QB64_32`, `QB64_64`, `QB64_NOT_X86`, `QB64_ARM`. **Constants:** `QB_FALSE`, `QB_TRUE`. **Helper:** `_countof`.

**QB64Fresh:** 🟢 Same macro/constant names in `runtime/include/qb64fresh_rt.h` (platform block at top); C-only `_countof` macro.

## 40. Legacy event stubs (ON COM, ON PEN, ON SIGNAL)

| Function / symbol | Description | QB64Fresh |
|-------------------|-------------|-----------|
| `qb_on_com(int32_t, void*)` | ON COM(n) GOSUB — serial port event | 🟢 Stub (warn once, no-op); inline C + external runtime |
| `qb_com_control(int32_t, int)` | COM(n) ON/OFF/STOP | 🟢 Stub |
| `qb_on_pen(void*)` | ON PEN GOSUB — light pen event | 🟢 Stub (warn once, no-op) |
| `qb_pen_control(int)` | PEN ON/OFF/STOP | 🟢 Stub |
| `qb_on_signal(int32_t, void*)` | ON SIGNAL(n) GOSUB — BASIC signal | 🟢 Stub (warn once, no-op) |
| `qb_signal_control(int32_t, int)` | SIGNAL(n) ON/OFF/STOP | 🟢 Stub |

**QB64Fresh:** Inline runtime emits C stubs in `src/codegen/c_backend/runtime/legacy.rs`. External runtime exports the same symbols from `runtime/src/events.rs` and `runtime/include/qb64fresh_rt.h`.

## 41. Parts (Subsystems)


| Part | Role | QB64Fresh |
|------|------|-----------|
| **gui** | tinyfiledialogs; implements gui.h (file/folder/messagebox) | 🟢 |
| **os** | clipboard (clip); implements clipboard.h (text only) | 🟢 |
| **input** | game_controller (libstem_gamepad); implements game_controller.h | 🟢 |
| **network** | TCP/UDP (many .c/.h files) | 🟢 |

## 42. Other Headers (Reference — No Extra API Listed Here)

- **qbs-mk-cv.h** — qbs/condvar glue.
- **mac-mouse-support.h** — macOS mouse support (included from libqb.cpp).
- **glut-thread.h** — GLUT/OpenGL thread (included from libqb.cpp).

