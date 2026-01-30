$CONSOLE:ONLY
' Runtime comparison: CONST with basic math (subset of QB64pe const/expression; no unsigned/EQV/IMP)
CONST cadd = 2 + 3
CONST csub = 2 - 3
CONST cmul = 7 * 20
CONST cdiv = 6 / 3
CONST cidiv = 7 \ 3
CONST cmod = 20 MOD 3
CONST cpow = 3 ^ 4
CONST cneg = -20
CONST ceq = 2 = 2
CONST cgt = 3 > 2
CONST cstr = "foobar"
PRINT cadd; csub; cmul; cdiv; cidiv; cmod; cpow; cneg
PRINT ceq; cgt
PRINT cstr
END
