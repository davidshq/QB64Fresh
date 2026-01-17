Generate or run tests for code that has been recently changed or is currently selected.

## Tasks

1. **Identify what needs testing**: Look at recent changes via `git diff` or the selected code
2. **Find existing tests**: Search for related test files that may need updating
3. **Determine test approach**:
   - If tests exist: Run them and verify they still pass, update if needed
   - If no tests exist: Determine if tests are warranted (not everything needs a test)
4. **Write/update tests** focusing on:
   - Happy path scenarios
   - Edge cases and boundary conditions
   - Error conditions
   - Any regression scenarios based on the changes made
5. **Run the tests** and report results

## Guidelines

- Match the existing test style and framework used in the project
- Keep tests focused and readable - one logical assertion per test when practical
- Use descriptive test names that explain the scenario
- Don't over-test trivial code (getters, simple assignments)
- Mock external dependencies appropriately

If you can't find an existing test framework or pattern, ask before creating new test infrastructure.
