Document the code that has been recently changed, selected, or specified by the user.

## Tasks

1. **Identify what needs documentation**: Recent changes, selected code, or user-specified files/functions
2. **Assess current documentation**: Check what already exists and what's missing
3. **Add appropriate documentation**:

### In-Code Documentation
- Function/method docstrings explaining purpose, parameters, return values
- Complex logic explanations where the "why" isn't obvious
- TODO/FIXME comments for known limitations
- Type hints/annotations if the language supports them

### External Documentation (when appropriate)
- README updates if functionality or usage has changed
- API documentation for public interfaces
- Architecture notes for significant design decisions

## Guidelines

- Document the "why" not the "what" - code shows what, comments explain why
- Keep docs close to the code they describe
- Use the documentation style already established in the project
- Don't over-document obvious code
- Update existing docs rather than adding redundant new ones
- If adding to a docs/ folder, follow existing structure

Ask if unclear whether external documentation is needed or where it should go.
