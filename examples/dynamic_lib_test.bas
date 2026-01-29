' Test DECLARE DYNAMIC LIBRARY - runtime loading (dlopen/LoadLibrary)
DECLARE DYNAMIC LIBRARY "mylib"
    FUNCTION MyFunc (x AS LONG, y AS LONG) AS LONG
    SUB MySub (msg AS STRING)
END DECLARE

PRINT "Before call"
' Calls use qb_dyn_MyFunc and qb_dyn_MySub (function pointers)
PRINT MyFunc(1, 2)
MySub "hello"
PRINT "Done"
