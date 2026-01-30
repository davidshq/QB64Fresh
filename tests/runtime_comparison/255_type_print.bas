$CONSOLE:ONLY
' TYPE with PRINT fields
TYPE T
  a AS LONG
  b AS DOUBLE
END TYPE
DIM u AS T
u.a = 7
u.b = 2.5
PRINT u.a; u.b
END
