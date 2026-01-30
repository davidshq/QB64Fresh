$CONSOLE:ONLY
' Minimal ROT13 on "ab" (inspired by qbasic_testcases/misc/rot13.bas)
z$ = "ab"
FOR n = 1 TO LEN(z$)
  a$ = RIGHT$(LEFT$(z$, n), 1)
  c = ASC(UCASE$(a$))
  IF c > 64 AND c < 91 THEN b$ = CHR$((c - 65 + 13) MOD 26 + 65) ELSE b$ = a$
  PRINT b$;
NEXT n
PRINT
END
