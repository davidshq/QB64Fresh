'==============================================================================
' Test: Utilities - Hash Table
' Purpose: Verify hash table utility compiles
' Expected: ~1s compilation time
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Hash table utility (header first, then implementation)
$INCLUDE:'../../../QB64pe/source/utilities/hash.bi'
$INCLUDE:'../../../QB64pe/source/utilities/hash.bas'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Hash table utility test"
