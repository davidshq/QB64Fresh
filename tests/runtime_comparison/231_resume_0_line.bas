$CONSOLE:ONLY
' Runtime comparison 231: RESUME 0 (retry) and RESUME <label>
' Part 1: RESUME 0 retries the line that caused the error.
' Part 2: RESUME after_label jumps to the given label.
' Note: With inline runtime, RESUME 0 may not retry (_qb_error_line not set); see DIFFERENCES if results differ from QB64pe.
ON ERROR GOTO handler
PRINT "part1_start"
flag = 0
IF flag = 0 THEN ERROR 5
PRINT "part1_after_resume0"
GOTO part2
handler:
PRINT "handler"
IF flag = 0 THEN flag = 1: RESUME 0
RESUME after_label
part2:
PRINT "part2_trigger"
ERROR 6
PRINT "part2_after_error"
END
after_label:
PRINT "after_label"
END
