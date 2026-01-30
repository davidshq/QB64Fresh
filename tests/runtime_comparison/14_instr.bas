$CONSOLE:ONLY
' Runtime comparison: INSTR
PRINT "instr hello ll:"; INSTR("hello", "ll")
PRINT "instr abc x:"; INSTR("abc", "x")
PRINT "instr 3 hello ll:"; INSTR(3, "hello", "ll")
PRINT "instr 1 a a:"; INSTR("a", "a")
END
