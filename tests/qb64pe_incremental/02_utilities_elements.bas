'==============================================================================
' Test: Utilities - Element Parsing
' Purpose: Verify elements.bas (token/element parsing) compiles
' Expected: ~1s compilation time
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes (elements.bas uses sp, sp2, sp3 from constants.bas)
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Element parsing utility (no .bi - implementation only)
$INCLUDE:'../../../QB64pe/source/utilities/elements.bas'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Elements utility test"
