All of the following should be done from the perspective of a pragmatic software engineer. We are writing high quality software but we aren't perfectionists.

Review the code that has been recently changed or edited. Look for:

1. **Bugs and Errors**
2. **Lost Functionality**: Features or functions that may have been accidentally removed or broken
3. **Security Issues**
4. **Bad Practices**: Code smells, anti-patterns, violations of DRY/SOLID, YAGNI
5. **Performance Issues**
6. **Error Handling**
7. **Testing**:
  - Does this code need a test? Does a test need to be updated?
  - Have we run existing tests that relate to the functionality we've changed if needed?
8. **Documentation**:
  - Does the code have adequate documentation? Do any other docs need to be updated?
9. **Dependencies**:
  - If you've added dependencies, are they the best option? Are we using the right version (usually the latest stable)?

You can use `git` commands to see what has changed. If there is more than 2 items to change create an md with the items and place in a docs/ThingsToDo folder. Include a severity/priority for each item.