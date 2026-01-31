'==============================================================================
' Test: Utilities - Constant Evaluation
' Purpose: Verify const_eval utility module compiles
' Expected: ~0.5s compilation time
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Elements utility (needed by const_eval for pushelement, getelements$)
$INCLUDE:'../../../QB64pe/source/utilities/elements.bas'

' Give_Error (needed by type.bas and const_eval)
$INCLUDE:'../../../QB64pe/source/utilities/give_error.bi'
$INCLUDE:'../../../QB64pe/source/utilities/give_error.bas'
' Simple buffer (needed by type.bas for WriteBufLine)
$INCLUDE:'../../../QB64pe/source/utilities/s-buffer/simplebuffer.bi'
$INCLUDE:'../../../QB64pe/source/utilities/s-buffer/simplebuffer.bm'
' Hash and type utilities (needed by const_eval for HashFindRev, tryRemoveSymbol$)
$INCLUDE:'../../../QB64pe/source/utilities/hash.bi'
$INCLUDE:'../../../QB64pe/source/utilities/hash.bas'
$INCLUDE:'../../../QB64pe/source/utilities/type.bi'
$INCLUDE:'../../../QB64pe/source/utilities/type.bas'

' Stub const arrays (normally REDIM'd in main compiler; required by const_eval.bas)
REDIM SHARED conststring(1) AS STRING
REDIM SHARED consttype(1) AS LONG
REDIM SHARED constinteger(1) AS _INTEGER64
REDIM SHARED constuinteger(1) AS _UNSIGNED _INTEGER64
REDIM SHARED constfloat(1) AS _FLOAT
REDIM SHARED constsubfunc(1) AS LONG
REDIM SHARED constdefined(1) AS LONG

' Constant evaluation utility (header first, then implementation)
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bi'
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bas'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Constant evaluation utility test"
