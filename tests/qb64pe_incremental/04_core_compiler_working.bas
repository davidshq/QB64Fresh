'==============================================================================
' Test: Core Compiler - Working Infrastructure for Built-in Functions
' Purpose: Test minimal compiler infrastructure needed for built-in functions
' Expected: ~2-5s compilation time
'==============================================================================
' This test includes the minimal infrastructure needed to test built-in
' function registration by extracting key sections from qb64pe.bas.
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Utility headers
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bi'
$INCLUDE:'../../../QB64pe/source/utilities/give_error.bi'
$INCLUDE:'../../../QB64pe/source/utilities/statevars.bi'
$INCLUDE:'../../../QB64pe/source/utilities/type.bi'

' Give_Error implementation
$INCLUDE:'../../../QB64pe/source/utilities/give_error.bas'

' Hash table (needed for symbol table)
$INCLUDE:'../../../QB64pe/source/utilities/hash.bi'
$INCLUDE:'../../../QB64pe/source/utilities/hash.bas'

' Elements utility (needed for const_eval)
$INCLUDE:'../../../QB64pe/source/utilities/elements.bas'

' Constant evaluation (needed for built-in functions)
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bas'

' Extracted sections from qb64pe.bas
$INCLUDE:'sections/idstruct_type.bas'
$INCLUDE:'sections/ids_init.bas'
$INCLUDE:'sections/clearid_sub.bas'
$INCLUDE:'sections/regid_sub.bas'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Core compiler infrastructure test"
PRINT "Includes: idstruct, ids() array, clearid, regid"
