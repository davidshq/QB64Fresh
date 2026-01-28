'==============================================================================
' Test: Utilities - Type System
' Purpose: Verify type system utility compiles
' Expected: ~1s compilation time
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Type system utility (header)
$INCLUDE:'../../../QB64pe/source/utilities/type.bi'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Type system utility test"
