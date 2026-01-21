- [ ] **GOSUB uses GCC computed goto extension** *(Medium - 2-3 sessions for MSVC alternative)*
      The GOSUB/RETURN implementation uses GCC's computed goto extension (`&&label` for label
      addresses, `goto *ptr` for indirect jumps). This works with GCC and Clang but NOT MSVC.
      For MSVC support, would need a switch-based dispatch table alternative.
      Low priority since most users compile with GCC/MinGW.
