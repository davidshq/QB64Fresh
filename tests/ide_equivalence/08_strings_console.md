# Portion 8: Strings and console

**Criterion:** Editor text and search behave; no encoding crashes.

**Repro:** Use existing runtime comparison string tests; golden `.out` outputs.

- **Location:** [tests/runtime_comparison/](../../runtime_comparison/)
- **Examples:** `01_string_ops.bas`, `12_chr_asc.bas`, `50_mid_two_args.bas`, `73_string_compare.bas`, `258_string_builtin.bas`, and other string/console tests.
- **Run:** `./tests/runtime_comparison/run_comparison.sh` (or run individual .bas with QB64Fresh and QB64pe, compare output).
- **Automation:** Full (golden output compare via `diff_results.sh`).

No separate minimal repro in this directory; portion 8 is covered by the runtime_comparison suite.
