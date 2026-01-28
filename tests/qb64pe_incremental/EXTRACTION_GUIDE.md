# Extracting Sections from qb64pe.bas

## Problem

Some QB64pe modules (like built-in functions) require infrastructure from the main compiler file (`qb64pe.bas`). We can't test them in isolation, but we can extract specific sections.

## Solution

Use the `extract-qb64pe-section.sh` script to extract sections from qb64pe.bas and create test files.

## Usage

```bash
# Extract a section
./scripts/extract-qb64pe-section.sh <section_name> <start_line> <end_line>

# Example: Extract idstruct TYPE definition
./scripts/extract-qb64pe-section.sh idstruct_type 596 656
```

## Key Sections to Extract

### For Built-in Functions Testing

1. **idstruct TYPE** (lines 596-642)
   ```bash
   ./scripts/extract-qb64pe-section.sh idstruct_type 596 642
   ```
   - Defines the identifier structure used by the compiler
   - Needed for `clearid` and `regid` SUBs

2. **clearid SUB** (line 14476+)
   ```bash
   # First, find the end of the SUB
   grep -n "^SUB clearid\|^END SUB" ../../../QB64pe/source/qb64pe.bas | head -2
   # Then extract
   ./scripts/extract-qb64pe-section.sh clearid_sub 14476 <end_line>
   ```
   - Resets the id structure to defaults
   - Needed for built-in function registration

3. **regid SUB** (line 21849+)
   ```bash
   # First, find the end of the SUB
   grep -n "^SUB regid\|^END SUB" ../../../QB64pe/source/qb64pe.bas | head -2
   # Then extract
   ./scripts/extract-qb64pe-section.sh regid_sub 21849 <end_line>
   ```
   - Registers a function/subroutine in the symbol table
   - Needed for built-in function registration

4. **ids() array initialization** (around line 644-656)
   ```bash
   ./scripts/extract-qb64pe-section.sh ids_init 644 656
   ```
   - Initializes the symbol table array
   - Needed for built-in function registration

## Workflow

1. **Identify needed sections** - Check what's missing from error messages
2. **Find line numbers** - Use grep to find SUB/TYPE definitions
3. **Extract sections** - Use the script to create test files
4. **Include in test** - Add `$INCLUDE` directives to your test file
5. **Test incrementally** - Fix errors, add more sections as needed

## Example: Building Phase 4 Test

```bash
# Extract all needed sections
./scripts/extract-qb64pe-section.sh idstruct_type 596 642
./scripts/extract-qb64pe-section.sh ids_init 644 656
./scripts/extract-qb64pe-section.sh clearid_sub 14476 14500  # adjust end line
./scripts/extract-qb64pe-section.sh regid_sub 21849 21900    # adjust end line

# Then in 04_core_compiler_minimal.bas, add:
'$INCLUDE:'sections/idstruct_type.bas'
'$INCLUDE:'sections/ids_init.bas'
'$INCLUDE:'sections/clearid_sub.bas'
'$INCLUDE:'sections/regid_sub.bas'
```

## Finding Line Numbers

```bash
# Find TYPE definitions
grep -n "^TYPE " ../../../QB64pe/source/qb64pe.bas

# Find SUB definitions
grep -n "^SUB " ../../../QB64pe/source/qb64pe.bas | head -20

# Find FUNCTION definitions
grep -n "^FUNCTION " ../../../QB64pe/source/qb64pe.bas | head -20

# Find specific item
grep -n "idstruct\|clearid\|regid" ../../../QB64pe/source/qb64pe.bas
```

## Notes

- Extracted sections are saved to `tests/qb64pe_incremental/sections/`
- Each section includes necessary includes and minimal test code
- You may need to extract dependencies of extracted sections
- Some sections may have circular dependencies - extract carefully
