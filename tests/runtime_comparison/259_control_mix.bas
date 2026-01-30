$CONSOLE:ONLY
' Mix: WHILE, DO LOOP, EXIT
n = 0
WHILE n < 2
  n = n + 1
  PRINT "w"; n;
WEND
PRINT
DO
  n = n - 1
  PRINT "d"; n;
  IF n <= 0 THEN EXIT DO
LOOP
PRINT
END
