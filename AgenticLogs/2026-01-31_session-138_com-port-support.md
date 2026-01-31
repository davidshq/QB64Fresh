# Session 138: COM Port Support (Serial)

**Date:** 2026-01-31

## Summary

Added **serial COM port support** so that `OPEN "COM1:9600,N,8,1" AS #n` (or with `FOR OUTPUT`/`FOR INPUT`) opens a real serial port. PRINT # and INPUT # work unchanged on the opened file number.

## Implementation

- **Codegen** (`src/codegen/c_backend/runtime/file.rs`):
  - In `qb_file_open`, detect filename starting with `COM` (case-insensitive) followed by digits and `:`.
  - Parse `COMn:baud,parity,data,stop` (parity N/E/O, data 5–8, stop 1–2). COM10+ supported.
  - New static helper `_qb_serial_open(port_num, baud, parity_char, data_bits, stop_bits)` returns `FILE*`:
    - **Windows:** CreateFile(`\\.\COMn` for port > 9), SetCommState (DCB: baud, parity, data, stop), `_open_osfhandle` + `_fdopen` so fread/fwrite work.
    - **Unix:** open(`/dev/ttyS{port-1}`), termios (cfsetispeed/cfsetospeed, CS5–CS8, PARENB, CSTOPB), fdopen.
  - No change to close path: `fclose()` on the `FILE*` from serial open closes the handle.
- **Includes:** `<termios.h>` on Unix; `<fcntl.h>` on Windows for `_O_RDWR|_O_BINARY`.
- **Tests:** `tests/integration_tests.rs` — new `file_io::open_com_port` test; golden files updated.

## Notes

- **ON COM(n) GOSUB** and **COM(n) ON/OFF/STOP** remain stubbed (event trapping); only OPEN COM is implemented.
- QB45 syntax `OPEN "COM1:9600,N,8,1" AS #1` is supported when combined with FOR (e.g. `OPEN "COM1:9600,N,8,1" FOR OUTPUT AS #1`); parser requires FOR.

## Review fixes (same session)

- **Windows `_fdopen` failure:** If `_fdopen(fd, "rb+")` failed, the CRT fd (and underlying handle) was leaked. Now we assign to `FILE* f`, check `!f`, and on failure call `_close(fd)` before returning NULL.
- **Port number overflow/cap:** Parsing `COMn:` with many digits could overflow `port_num`. Parsing now stops when `port_num > 25 || (port_num == 25 && d > 6)` so the value never exceeds 256. Call site also requires `port_num > 0 && port_num <= 256` before calling `_qb_serial_open`.
- **Windows COM path:** Confirmed generated C string `"\\\\\\.\\\\COM%d"` is correct (emits `\\.\COMn` for COM10+).

## Files changed

- `src/codegen/c_backend/runtime/file.rs` — COM detection, `_qb_serial_open`, includes; review fixes above.
- `tests/integration_tests.rs` — `open_com_port` test.
- `tests/golden/*.golden` — regenerated (inline runtime now includes COM support).
