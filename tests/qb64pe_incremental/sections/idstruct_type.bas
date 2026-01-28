'==============================================================================
' Extracted Section: idstruct_type
' Source: qb64pe.bas lines 596-642
'==============================================================================
' NOTE: This is a raw extraction. Include this file AFTER setting up:
'   - Core includes (version, settings, constants)
'   - Required utility headers
'   - TYPE definitions this section depends on
'==============================================================================

TYPE idstruct

    n AS STRING * 256 'name
    cn AS STRING * 256 'case sensitive version of n

    arraytype AS LONG 'similar to t
    arrayelements AS INTEGER
    staticarray AS INTEGER 'set for arrays declared in the main module with static elements

    mayhave AS STRING * 8 'mayhave and musthave are exclusive of each other
    musthave AS STRING * 8
    t AS LONG 'type

    tsize AS LONG


    subfunc AS INTEGER 'if function=1, sub=2 (max 100 arguments)
    Dependency AS INTEGER
    internal_subfunc AS INTEGER

    callname AS STRING * 256
    ccall AS INTEGER
    overloaded AS _BYTE
    args AS INTEGER
    minargs AS INTEGER
    arg AS STRING * 400 'similar to t
    argsize AS STRING * 400 'similar to tsize (used for fixed length strings)
    specialformat AS STRING * 256
    secondargmustbe AS STRING * 256
    secondargcantbe AS STRING * 256
    ret AS LONG 'the value it returns if it is a function (again like t)

    insubfunc AS STRING * 256
    insubfuncn AS LONG

    share AS INTEGER
    nele AS STRING * 100
    nelereq AS STRING * 100
    linkid AS LONG
    linkarg AS INTEGER
    staticscope AS INTEGER
    'For variables which are arguments passed to a sub/function
    sfid AS LONG 'id number of variable's parent sub/function
    sfarg AS INTEGER 'argument/parameter # within call (1=first)

    hr_syntax AS STRING
END TYPE
