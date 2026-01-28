'==============================================================================
' Test: Utilities - Constant Evaluation
' Purpose: Verify const_eval utility module compiles
' Expected: ~1s compilation time
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Elements utility (needed by const_eval for pushelement, getelements$)
$INCLUDE:'../../../QB64pe/source/utilities/elements.bas'

' Constant evaluation utility (header first, then implementation)
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bi'
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bas'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Constant evaluation utility test"
