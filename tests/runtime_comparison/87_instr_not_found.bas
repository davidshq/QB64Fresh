$CONSOLE:ONLY
' Runtime comparison: INSTR when not found
PRINT "instr x in abc:"; INSTR("abc", "x")
PRINT "instr 2 abc x:"; INSTR(2, "abc", "x")
END
