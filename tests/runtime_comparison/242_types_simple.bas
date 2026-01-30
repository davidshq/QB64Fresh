$CONSOLE:ONLY
' Runtime comparison: simple TYPE (LONG, DOUBLE, STRING) - no _BYTE/_UNSIGNED
TYPE num_t
  i AS LONG
  d AS DOUBLE
  s AS STRING
END TYPE
DIM n AS num_t
n.i = -12345
n.d = 3.14
n.s = "hello"
PRINT n.i; n.d; n.s
DIM n2 AS num_t
n2 = n
PRINT n2.i; n2.d; n2.s
END
