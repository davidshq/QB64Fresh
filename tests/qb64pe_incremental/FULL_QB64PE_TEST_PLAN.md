# Full QB64pe Testing Plan

## Goal
Test the entire QB64pe compiler incrementally, identifying what works and what needs fixes.

## Strategy

### Phase 1: Test All Isolated Components ✅
- [x] Core infrastructure (global includes)
- [x] Hash utility
- [x] Type utility
- [ ] Const eval utility (partial - needs work)
- [ ] Elements utility
- [ ] Other utilities

### Phase 2: Extract and Test Core Compiler Sections
1. Extract TYPE definitions
2. Extract SUB/FUNCTION definitions
3. Extract initialization code
4. Test each section incrementally

### Phase 3: Test Built-in Functions
1. Test with minimal compiler infrastructure
2. Identify missing dependencies
3. Add dependencies incrementally

### Phase 4: Test Main Compiler Logic
1. Extract compiler main loop sections
2. Test parsing logic
3. Test code generation logic

### Phase 5: Full QB64pe Test
1. Test complete qb64pe.bas
2. Document all errors
3. Prioritize fixes

## Execution Plan

### Step 1: Identify All Major Sections
- Scan qb64pe.bas for TYPE definitions
- Scan for SUB/FUNCTION definitions
- Identify logical sections

### Step 2: Extract and Test Each Section
- Use extract-qb64pe-section.sh
- Test each section
- Document dependencies

### Step 3: Build Up Incrementally
- Start with working sections
- Add dependencies one by one
- Fix errors as we go

### Step 4: Test Full Compiler
- Run full qb64pe.bas
- Collect all errors
- Create prioritized fix list

## Expected Results

- List of all sections that compile successfully
- List of sections with errors (and what's missing)
- Dependency map
- Prioritized fix list
