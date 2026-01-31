'==============================================================================
' Test: Built-in Functions
' Purpose: Verify subs_functions module compiles
' Expected: ~5s compilation time
'==============================================================================

DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Utility headers needed by subs_functions
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

' Constant evaluation (needed for Set_ConstFunctions)
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bas'

' Extracted sections from qb64pe.bas (needed for clearid, regid)
$INCLUDE:'sections/idstruct_type.bas'
$INCLUDE:'sections/ids_init.bas'
' Stubs for compiler-internal symbols (validname, tryRemoveSymbol$, AddQuotes$, subfunc)
$INCLUDE:'sections/phase3_stubs.bas'
$INCLUDE:'sections/clearid_sub.bas'
$INCLUDE:'sections/regid_sub.bas'

' Built-in functions (large file - 4,342 lines)
$INCLUDE:'../../../QB64pe/source/subs_functions/subs_functions.bas'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Built-in functions test"
