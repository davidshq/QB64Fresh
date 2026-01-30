# Runtime Comparison Tests (QB64Fresh vs QB64pe)

**133+ minimal BASIC programs** used to compare runtime behavior between QB64Fresh and QB64pe. See [DIFFERENCES.md](DIFFERENCES.md) for known output differences and [MORNING_REVIEW.md](MORNING_REVIEW.md) for tasks needing user intervention.

## Run all tests

From QB64Fresh repo root:

```bash
cd tests/runtime_comparison
./run_comparison.sh
```

- **QB64Fresh:** Always run (compile with `--runtime inline`, gcc -lm, run). Results in `results/fresh/<name>.txt`.
- **QB64pe:** Run only if `../QB64pe/qb64pe` exists; uses 4GB ulimit. Results in `results/qb64pe/<name>.txt`.
- **QB64Fresh only (faster):** `RUN_PE=0 ./run_comparison.sh`

## Test index (sample)

| File | Purpose |
|------|--------|
| `01_string_ops.bas` | String concat, LEN, MID$, LEFT$, RIGHT$, comparison |
| `02_lbound_ubound.bas` | LBOUND/UBOUND with `DIM arr(1 TO 5)` |
| `03_on_error_resume_next.bas` | ON ERROR GOTO, ERROR 5, ERR/ERL, RESUME NEXT |
| `04_gosub_return.bas` | GOSUB/RETURN stack |
| `05_file_io.bas` | OPEN, PRINT #, CLOSE, LINE INPUT #, KILL |
| `06_*`–`125_*` | Math, string, control flow, SUB/FUNCTION, TYPE, file I/O, etc. |

All use `$CONSOLE:ONLY` so QB64pe produces console output. Five tests currently fail under QB64Fresh (compile/link); see DIFFERENCES.md.

## Single-file run

**QB64Fresh (inline runtime):**
```bash
qb64fresh tests/runtime_comparison/01_string_ops.bas --emit-c --runtime inline -o out.c
gcc -o out out.c -lm
./out
```

**QB64pe (from QB64pe repo):**
```bash
cd /path/to/QB64pe
./qb64pe -x /path/to/QB64Fresh/tests/runtime_comparison/01_string_ops.bas -o out
./out
```
