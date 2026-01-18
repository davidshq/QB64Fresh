' Test DO LOOP variants
DIM n AS LONG
n = 0

' DO WHILE at start
DO WHILE n < 3
    n = n + 1
LOOP

' DO UNTIL at end
DO
    n = n - 1
LOOP UNTIL n = 0

PRINT n
END
