Analyze the selected code or recently changed files and suggest refactoring opportunities.

## Tasks

1. **Understand the code**: Read and comprehend what the code does before suggesting changes
2. **Identify refactoring opportunities**:
   - Code duplication (DRY violations)
   - Long functions/methods that should be split
   - Complex conditionals that could be simplified
   - Poor naming that obscures intent
   - Inappropriate coupling between components
   - Mixed levels of abstraction
   - Dead code that can be removed
3. **Prioritize suggestions**: Focus on impactful improvements, not nitpicks
4. **Propose specific changes**: Show before/after or describe concrete steps

## Guidelines

- **Don't refactor for refactoring's sake** - there should be a clear benefit
- **Preserve behavior** - refactoring should not change what the code does
- **Consider the blast radius** - prefer smaller, safer changes
- **Respect existing patterns** - work within the project's established style
- **YAGNI applies** - don't add abstraction for hypothetical future needs

## Output

Present suggestions as a prioritized list:
1. **High value**: Clear improvements to readability, maintainability, or correctness
2. **Medium value**: Nice-to-have cleanups
3. **Optional**: Minor style preferences (mention but don't push)

Ask before making changes - present the analysis first and let the user decide what to proceed with.
