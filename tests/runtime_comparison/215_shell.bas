$CONSOLE:ONLY
' Runtime comparison 215: SHELL runs command and returns
' Use echo (Unix) / cmd (Windows); output may vary by OS but "before" and "end" are comparable
PRINT "before"
SHELL "echo after"
PRINT "end"
END
