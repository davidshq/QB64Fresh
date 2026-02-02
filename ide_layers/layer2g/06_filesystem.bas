$CONSOLE:ONLY
' IDE Equivalence 6: Filesystem
' Minimal repro: _CWD$, _DIR$, _FILEEXISTS, CHDIR (optional).
' Criterion: Open/Save dialogs and paths correct.
DIM cwd AS STRING
DIM d AS STRING
cwd = _CWD$
PRINT "cwd: "; cwd
d = _DIR$(".")
PRINT "dir: "; LEFT$(d, 80)
PRINT "fileexists(06_filesystem.bas): "; _FILEEXISTS("06_filesystem.bas")
END
