$CONSOLE:ONLY
' String built-ins: LEN, LEFT$, RIGHT$, MID$, INSTR
a$ = "abcdef"
PRINT LEN(a$); LEFT$(a$, 2); RIGHT$(a$, 2); MID$(a$, 2, 3)
PRINT INSTR(a$, "cd"); INSTR(2, a$, "b")
END
