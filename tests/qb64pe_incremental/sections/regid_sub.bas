'==============================================================================
' Extracted Section: regid_sub
' Source: qb64pe.bas lines 21849-22081
'==============================================================================
' NOTE: This is a raw extraction. Include this file AFTER setting up:
'   - Core includes (version, settings, constants)
'   - Required utility headers
'   - TYPE definitions this section depends on
'==============================================================================

SUB regid
    idn = idn + 1

    IF idn > ids_max THEN
        ids_max = ids_max * 2
        REDIM _PRESERVE ids(1 TO ids_max) AS idstruct
        REDIM _PRESERVE cmemlist(1 TO ids_max + 1) AS INTEGER
        REDIM _PRESERVE sfcmemargs(1 TO ids_max + 1) AS STRING * 100
        REDIM _PRESERVE arrayelementslist(1 TO ids_max + 1) AS INTEGER
    END IF

    n$ = RTRIM$(id.n)

    IF reginternalsubfunc = 0 THEN
        autoIncForceUScore = 1
        IF validname(n$) = 0 THEN Give_Error "Invalid name": EXIT SUB
    END IF

    'register case sensitive name if none given
    IF ASC(id.cn) = 32 THEN
        n$ = RTRIM$(id.n)
        id.n = UCASE$(n$)
        id.cn = n$
    END IF

    id.insubfunc = subfunc
    id.insubfuncn = subfuncn

    'note: cannot be STATIC and SHARED at the same time
    IF dimshared THEN
        id.share = dimshared
    ELSE
        IF dimstatic THEN id.staticscope = 1
    END IF

    ids(idn) = id

    currentid = idn

    'prepare hash flags and check for conflicts
    hashflags = 1

    'sub/function?
    'Note: QBASIC does not allow: Internal type names (INTEGER,LONG,...)
    IF id.subfunc THEN
        ids(currentid).internal_subfunc = reginternalsubfunc
        IF id.subfunc = 1 THEN hashflags = hashflags + HASHFLAG_FUNCTION ELSE hashflags = hashflags + HASHFLAG_SUB
        IF reginternalsubfunc = 0 THEN 'allow internal definition of subs/functions without checks
            hashchkflags = HASHFLAG_RESERVED + HASHFLAG_CONSTANT
            IF id.subfunc = 1 THEN hashchkflags = hashchkflags + HASHFLAG_FUNCTION ELSE hashchkflags = hashchkflags + HASHFLAG_SUB
            hashres = HashFind(n$, hashchkflags, hashresflags, hashresref)
            DO WHILE hashres
                IF hashres THEN
                    'Note: Numeric sub/function names like 'mid' do not clash with Internal string sub/function names
                    '      like 'MID$' because MID$ always requires a '$'. For user defined string sub/function names
                    '      the '$' would be optional so the rule should not be applied there.
                    allow = 0
                    IF hashresflags AND (HASHFLAG_FUNCTION + HASHFLAG_SUB) THEN
                        IF RTRIM$(ids(hashresref).musthave) = "$" THEN
                            IF INSTR(ids(currentid).mayhave, "$") = 0 THEN allow = 1
                        END IF
                    END IF
                    IF allow = 0 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                END IF 'hashres
                IF hashres <> 1 THEN hashres = HashFindCont(hashresflags, hashresref) ELSE hashres = 0
            LOOP
            IF idemode THEN
                IF INSTR(listOfCustomKeywords$, "@" + UCASE$(n$) + "@") = 0 THEN
                    listOfCustomKeywords$ = listOfCustomKeywords$ + "@" + UCASE$(n$) + "@"
                END IF
            END IF
        END IF 'reginternalsubfunc = 0
    END IF

    'variable?
    IF id.t THEN
        hashflags = hashflags + HASHFLAG_VARIABLE
        IF reginternalvariable = 0 THEN
            allow = 0
            var_recheck:
            IF ASC(id.musthave) = 32 THEN astype2 = 1 '"AS type" declaration?
            scope2 = subfuncn
            hashchkflags = HASHFLAG_RESERVED + HASHFLAG_SUB + HASHFLAG_FUNCTION + HASHFLAG_CONSTANT + HASHFLAG_VARIABLE
            hashres = HashFind(n$, hashchkflags, hashresflags, hashresref)
            DO WHILE hashres

                'conflict with reserved word?
                IF hashresflags AND HASHFLAG_RESERVED THEN
                    musthave$ = RTRIM$(id.musthave)
                    IF INSTR(musthave$, "$") THEN
                        'All reserved words can be used as variables in QBASIC if "$" is appended to the variable name!
                        '(allow)
                    ELSE
                        Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'Conflicts with reserved word
                    END IF
                END IF 'HASHFLAG_RESERVED

                'conflict with sub/function?
                IF hashresflags AND (HASHFLAG_FUNCTION + HASHFLAG_SUB) THEN
                    IF ids(hashresref).internal_subfunc = 0 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'QBASIC doesn't allow a variable of the same name as a user-defined sub/func
                    IF RTRIM$(id.n) = "WIDTH" AND ids(hashresref).subfunc = 2 THEN GOTO varname_exception
                    musthave$ = RTRIM$(id.musthave)
                    IF LEN(musthave$) = 0 THEN
                        IF RTRIM$(ids(hashresref).musthave) = "$" THEN
                            'a sub/func requiring "$" can co-exist with implicit numeric variables
                            IF INSTR(id.mayhave, "$") THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                        ELSE
                            Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'Implicitly defined variables cannot conflict with sub/func names
                        END IF
                    END IF 'len(musthave$)=0
                    IF INSTR(musthave$, "$") THEN
                        IF RTRIM$(ids(hashresref).musthave) = "$" THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'A sub/function name already exists as a string
                        '(allow)
                    ELSE
                        IF RTRIM$(ids(hashresref).musthave) <> "$" THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'A non-"$" sub/func name already exists with this name
                    END IF
                END IF 'HASHFLAG_FUNCTION + HASHFLAG_SUB

                'conflict with constant?
                IF hashresflags AND HASHFLAG_CONSTANT THEN
                    scope1 = constsubfunc(hashresref)
                    IF (scope1 = 0 AND AllowLocalName = 0) OR scope1 = scope2 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                END IF

                'conflict with variable?
                IF hashresflags AND HASHFLAG_VARIABLE THEN
                    astype1 = 0: IF ASC(ids(hashresref).musthave) = 32 THEN astype1 = 1
                    scope1 = ids(hashresref).insubfuncn
                    IF astype1 = 1 AND astype2 = 1 THEN
                        IF scope1 = scope2 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                    END IF
                    'same type?
                    IF id.t = ids(hashresref).t THEN
                        IF id.tsize = ids(hashresref).tsize THEN
                            IF scope1 = scope2 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                        END IF
                    END IF
                    'will astype'd fixed STRING-variable mask a non-fixed string?
                    IF id.t AND ISFIXEDLENGTH THEN
                        IF astype2 = 1 THEN
                            IF ids(hashresref).t AND ISSTRING THEN
                                IF (ids(hashresref).t AND ISFIXEDLENGTH) = 0 THEN
                                    IF scope1 = scope2 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                                END IF
                            END IF
                        END IF
                    END IF
                END IF

                varname_exception:
                IF hashres <> 1 THEN hashres = HashFindCont(hashresflags, hashresref) ELSE hashres = 0
            LOOP
        END IF 'reginternalvariable
    END IF 'variable

    'array?
    IF id.arraytype THEN
        hashflags = hashflags + HASHFLAG_ARRAY
        allow = 0
        ary_recheck:
        scope2 = subfuncn
        IF ASC(id.musthave) = 32 THEN astype2 = 1 '"AS type" declaration?
        hashchkflags = HASHFLAG_RESERVED + HASHFLAG_SUB + HASHFLAG_FUNCTION + HASHFLAG_ARRAY
        hashres = HashFind(n$, hashchkflags, hashresflags, hashresref)
        DO WHILE hashres

            'conflict with reserved word?
            IF hashresflags AND HASHFLAG_RESERVED THEN
                musthave$ = RTRIM$(id.musthave)
                IF INSTR(musthave$, "$") THEN
                    'All reserved words can be used as variables in QBASIC if "$" is appended to the variable name!
                    '(allow)
                ELSE
                    Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'Conflicts with reserved word
                END IF
            END IF 'HASHFLAG_RESERVED

            'conflict with sub/function?
            IF hashresflags AND (HASHFLAG_FUNCTION + HASHFLAG_SUB) THEN
                IF ids(hashresref).internal_subfunc = 0 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'QBASIC doesn't allow a variable of the same name as a user-defined sub/func
                IF RTRIM$(id.n) = "WIDTH" AND ids(hashresref).subfunc = 2 THEN GOTO arrayname_exception
                musthave$ = RTRIM$(id.musthave)

                IF LEN(musthave$) = 0 THEN
                    IF RTRIM$(ids(hashresref).musthave) = "$" THEN
                        'a sub/func requiring "$" can co-exist with implicit numeric variables
                        IF INSTR(id.mayhave, "$") THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                    ELSE
                        Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'Implicitly defined variables cannot conflict with sub/func names
                    END IF
                END IF 'len(musthave$)=0
                IF INSTR(musthave$, "$") THEN
                    IF RTRIM$(ids(hashresref).musthave) = "$" THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'A sub/function name already exists as a string
                    '(allow)
                ELSE
                    IF RTRIM$(ids(hashresref).musthave) <> "$" THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB 'A non-"$" sub/func name already exists with this name
                END IF
            END IF 'HASHFLAG_FUNCTION + HASHFLAG_SUB

            'conflict with array?
            IF hashresflags AND HASHFLAG_ARRAY THEN
                astype1 = 0: IF ASC(ids(hashresref).musthave) = 32 THEN astype1 = 1
                scope1 = ids(hashresref).insubfuncn
                IF astype1 = 1 AND astype2 = 1 THEN
                    IF scope1 = scope2 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                END IF
                'same type?
                IF id.arraytype = ids(hashresref).arraytype THEN
                    IF id.tsize = ids(hashresref).tsize THEN
                        IF scope1 = scope2 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                    END IF
                END IF
                'will astype'd fixed STRING-variable mask a non-fixed string?
                IF id.arraytype AND ISFIXEDLENGTH THEN
                    IF astype2 = 1 THEN
                        IF ids(hashresref).arraytype AND ISSTRING THEN
                            IF (ids(hashresref).arraytype AND ISFIXEDLENGTH) = 0 THEN
                                IF scope1 = scope2 THEN Give_Error "Name already in use (" + n$ + ")": EXIT SUB
                            END IF
                        END IF
                    END IF
                END IF
            END IF

            arrayname_exception:
            IF hashres <> 1 THEN hashres = HashFindCont(hashresflags, hashresref) ELSE hashres = 0
        LOOP
    END IF 'array

    'add it to the hash table
    HashAdd n$, hashflags, currentid

END SUB
