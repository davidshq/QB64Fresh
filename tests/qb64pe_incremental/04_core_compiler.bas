'==============================================================================
' Test: Core Compiler (No IDE)
' Purpose: Verify main compiler logic without IDE component
' Expected: ~10s compilation time
'==============================================================================
' This test includes all of QB64pe EXCEPT the IDE component (ide_methods.bas
' which is 21K lines). The core compiler is ~3,500 lines without IDE.
'
' This is the most useful test for rapid iteration - it exercises the actual
' compiler logic without the IDE overhead.
'==============================================================================

DEFLNG A-Z
$CONSOLE
$SCREENHIDE

' Core global includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' OpenGL extension (if needed)
$INCLUDE:'../../../QB64pe/source/subs_functions/extensions/opengl/opengl_global.bas'

' Utility headers
$INCLUDE:'../../../QB64pe/source/utilities/ini-manager/ini.bi'
$INCLUDE:'../../../QB64pe/source/utilities/s-buffer/simplebuffer.bi'
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bi'
$INCLUDE:'../../../QB64pe/source/utilities/give_error.bi'
$INCLUDE:'../../../QB64pe/source/utilities/statevars.bi'
$INCLUDE:'../../../QB64pe/source/utilities/type.bi'

' Give_Error implementation
$INCLUDE:'../../../QB64pe/source/utilities/give_error.bas'

' Hash table (needed for symbol table / type system)
$INCLUDE:'../../../QB64pe/source/utilities/hash.bi'
$INCLUDE:'../../../QB64pe/source/utilities/hash.bas'

' Elements utility (needed for const_eval)
$INCLUDE:'../../../QB64pe/source/utilities/elements.bas'

' Constant evaluation (needed by subs_functions)
$INCLUDE:'../../../QB64pe/source/utilities/const_eval.bas'

' Compiler infrastructure: idstruct TYPE, ids array, clearid/regid (same as Phase 3)
$INCLUDE:'sections/idstruct_type.bas'
$INCLUDE:'sections/ids_init.bas'
$INCLUDE:'sections/phase3_stubs.bas'
$INCLUDE:'sections/clearid_sub.bas'
$INCLUDE:'sections/regid_sub.bas'

' NOTE: IDE component is INTENTIONALLY EXCLUDED here
' '$INCLUDE:'ide\ide_global.bas'  <-- Commented out in original too

' Built-in functions (uses regid/clearid and id struct)
$INCLUDE:'../../../QB64pe/source/subs_functions/subs_functions.bas'

' Minimal test code
DIM test AS INTEGER
test = 1

PRINT "Core compiler test (no IDE)"
