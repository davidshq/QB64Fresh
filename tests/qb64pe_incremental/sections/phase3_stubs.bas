'==============================================================================
' Phase 3 stubs: minimal compiler infrastructure for built-in functions test
' Purpose: Declare symbols that subs_functions.bas and regid need but that
'          are defined in qb64pe.bas or other modules we do not include.
' Include: After ids_init.bas, before clearid_sub.bas and subs_functions.bas.
'==============================================================================

' Globals used by regid (defined in qb64pe.bas) - stubs for Phase 3 isolation
DIM SHARED subfunc AS STRING * 256
DIM SHARED subfuncn AS LONG

'-----------------------------------------------------------------------------
' validname (defined in qb64pe.bas) - stub for Phase 3 isolation
'-----------------------------------------------------------------------------
FUNCTION validname (a$)
    validname = 1
END FUNCTION

'-----------------------------------------------------------------------------
' tryRemoveSymbol$ (defined in utilities/type.bas) - stub for Phase 3 isolation
'-----------------------------------------------------------------------------
FUNCTION tryRemoveSymbol$ (varname$)
    tryRemoveSymbol$ = ""
END FUNCTION

'-----------------------------------------------------------------------------
' AddQuotes$ (defined in utilities/strings.bas) - stub for Phase 3 isolation
'-----------------------------------------------------------------------------
FUNCTION AddQuotes$ (s$)
    AddQuotes$ = CHR$(34) + s$ + CHR$(34)
END FUNCTION
