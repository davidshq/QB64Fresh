//! Built-in function signature information for LSP features.
//!
//! This module provides signature information for QB64 built-in functions,
//! used for hover documentation and signature help in the language server.

/// Built-in function signature information.
pub(crate) struct FunctionSignature {
    pub label: &'static str,
    pub doc: &'static str,
    pub params: &'static [&'static str],
}

/// Gets the signature for a built-in function.
pub(crate) fn get_builtin_signature(name: &str) -> Option<FunctionSignature> {
    match name {
        // String functions
        "LEFT$" => Some(FunctionSignature {
            label: "LEFT$(string$, n%)",
            doc: "Returns the leftmost n characters of a string.",
            params: &["string$", "n%"],
        }),
        "RIGHT$" => Some(FunctionSignature {
            label: "RIGHT$(string$, n%)",
            doc: "Returns the rightmost n characters of a string.",
            params: &["string$", "n%"],
        }),
        "MID$" => Some(FunctionSignature {
            label: "MID$(string$, start%[, length%])",
            doc: "Returns a substring starting at position start. Length is optional.",
            params: &["string$", "start%", "length%"],
        }),
        "INSTR" => Some(FunctionSignature {
            label: "INSTR([start%,] string$, search$)",
            doc: "Returns the position of search$ within string$. Start position is optional.",
            params: &["start%", "string$", "search$"],
        }),
        "LEN" => Some(FunctionSignature {
            label: "LEN(string$)",
            doc: "Returns the length of a string in bytes.",
            params: &["string$"],
        }),
        "CHR$" => Some(FunctionSignature {
            label: "CHR$(code%)",
            doc: "Returns the character for an ASCII code (0-255).",
            params: &["code%"],
        }),
        "ASC" => Some(FunctionSignature {
            label: "ASC(string$[, position%])",
            doc: "Returns the ASCII code of a character. Position defaults to 1.",
            params: &["string$", "position%"],
        }),
        "UCASE$" => Some(FunctionSignature {
            label: "UCASE$(string$)",
            doc: "Converts a string to uppercase.",
            params: &["string$"],
        }),
        "LCASE$" => Some(FunctionSignature {
            label: "LCASE$(string$)",
            doc: "Converts a string to lowercase.",
            params: &["string$"],
        }),
        "LTRIM$" => Some(FunctionSignature {
            label: "LTRIM$(string$)",
            doc: "Removes leading spaces from a string.",
            params: &["string$"],
        }),
        "RTRIM$" => Some(FunctionSignature {
            label: "RTRIM$(string$)",
            doc: "Removes trailing spaces from a string.",
            params: &["string$"],
        }),
        "_TRIM$" => Some(FunctionSignature {
            label: "_TRIM$(string$)",
            doc: "Removes both leading and trailing spaces from a string.",
            params: &["string$"],
        }),
        "STRING$" => Some(FunctionSignature {
            label: "STRING$(n%, char)",
            doc: "Returns a string of n copies of a character.",
            params: &["n%", "char"],
        }),
        "SPACE$" => Some(FunctionSignature {
            label: "SPACE$(n%)",
            doc: "Returns a string of n spaces.",
            params: &["n%"],
        }),
        "STR$" => Some(FunctionSignature {
            label: "STR$(number)",
            doc: "Converts a number to its string representation.",
            params: &["number"],
        }),
        "VAL" => Some(FunctionSignature {
            label: "VAL(string$)",
            doc: "Converts a string to a numeric value.",
            params: &["string$"],
        }),

        // Math functions
        "ABS" => Some(FunctionSignature {
            label: "ABS(number)",
            doc: "Returns the absolute value of a number.",
            params: &["number"],
        }),
        "SGN" => Some(FunctionSignature {
            label: "SGN(number)",
            doc: "Returns -1, 0, or 1 indicating the sign of a number.",
            params: &["number"],
        }),
        "INT" => Some(FunctionSignature {
            label: "INT(number)",
            doc: "Truncates a number toward negative infinity.",
            params: &["number"],
        }),
        "FIX" => Some(FunctionSignature {
            label: "FIX(number)",
            doc: "Truncates a number toward zero.",
            params: &["number"],
        }),
        "CINT" => Some(FunctionSignature {
            label: "CINT(number)",
            doc: "Converts a number to INTEGER (rounds to nearest).",
            params: &["number"],
        }),
        "CLNG" => Some(FunctionSignature {
            label: "CLNG(number)",
            doc: "Converts a number to LONG (rounds to nearest).",
            params: &["number"],
        }),
        "CSNG" => Some(FunctionSignature {
            label: "CSNG(number)",
            doc: "Converts a number to SINGLE precision.",
            params: &["number"],
        }),
        "CDBL" => Some(FunctionSignature {
            label: "CDBL(number)",
            doc: "Converts a number to DOUBLE precision.",
            params: &["number"],
        }),
        "SQR" => Some(FunctionSignature {
            label: "SQR(number)",
            doc: "Returns the square root of a number.",
            params: &["number"],
        }),
        "LOG" => Some(FunctionSignature {
            label: "LOG(number)",
            doc: "Returns the natural logarithm (base e) of a number.",
            params: &["number"],
        }),
        "EXP" => Some(FunctionSignature {
            label: "EXP(power)",
            doc: "Returns e raised to a power.",
            params: &["power"],
        }),
        "SIN" => Some(FunctionSignature {
            label: "SIN(radians)",
            doc: "Returns the sine of an angle in radians.",
            params: &["radians"],
        }),
        "COS" => Some(FunctionSignature {
            label: "COS(radians)",
            doc: "Returns the cosine of an angle in radians.",
            params: &["radians"],
        }),
        "TAN" => Some(FunctionSignature {
            label: "TAN(radians)",
            doc: "Returns the tangent of an angle in radians.",
            params: &["radians"],
        }),
        "ATN" => Some(FunctionSignature {
            label: "ATN(number)",
            doc: "Returns the arctangent of a number in radians.",
            params: &["number"],
        }),
        "RND" => Some(FunctionSignature {
            label: "RND[(n)]",
            doc: "Returns a random number between 0 and 1.",
            params: &["n"],
        }),

        // Array functions
        "LBOUND" => Some(FunctionSignature {
            label: "LBOUND(array[, dimension%])",
            doc: "Returns the lower bound of an array dimension.",
            params: &["array", "dimension%"],
        }),
        "UBOUND" => Some(FunctionSignature {
            label: "UBOUND(array[, dimension%])",
            doc: "Returns the upper bound of an array dimension.",
            params: &["array", "dimension%"],
        }),

        // Screen/graphics functions
        "POINT" => Some(FunctionSignature {
            label: "POINT(x%, y%)",
            doc: "Returns the color of a pixel at the specified coordinates.",
            params: &["x%", "y%"],
        }),
        "_RGB" => Some(FunctionSignature {
            label: "_RGB(red%, green%, blue%[, imageHandle&])",
            doc: "Returns a 32-bit color value from RGB components.",
            params: &["red%", "green%", "blue%", "imageHandle&"],
        }),
        "_RGBA" => Some(FunctionSignature {
            label: "_RGBA(red%, green%, blue%, alpha%[, imageHandle&])",
            doc: "Returns a 32-bit color value from RGBA components.",
            params: &["red%", "green%", "blue%", "alpha%", "imageHandle&"],
        }),
        "_RGB32" => Some(FunctionSignature {
            label: "_RGB32(red%, green%, blue%[, alpha%])",
            doc: "Returns a 32-bit color value. Alpha defaults to 255.",
            params: &["red%", "green%", "blue%", "alpha%"],
        }),
        "_RED" | "_RED32" => Some(FunctionSignature {
            label: "_RED(color&)",
            doc: "Extracts the red component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_GREEN" | "_GREEN32" => Some(FunctionSignature {
            label: "_GREEN(color&)",
            doc: "Extracts the green component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_BLUE" | "_BLUE32" => Some(FunctionSignature {
            label: "_BLUE(color&)",
            doc: "Extracts the blue component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_ALPHA" | "_ALPHA32" => Some(FunctionSignature {
            label: "_ALPHA(color&)",
            doc: "Extracts the alpha component (0-255) from a color value.",
            params: &["color&"],
        }),
        "_NEWIMAGE" => Some(FunctionSignature {
            label: "_NEWIMAGE(width%, height%[, mode%])",
            doc: "Creates a new image. Mode: 0=current, 32=32-bit, 256=256-color.",
            params: &["width%", "height%", "mode%"],
        }),
        "_LOADIMAGE" => Some(FunctionSignature {
            label: "_LOADIMAGE(filename$[, mode%])",
            doc: "Loads an image file. Supports BMP, PNG, JPG, GIF.",
            params: &["filename$", "mode%"],
        }),
        "_PUTIMAGE" => Some(FunctionSignature {
            label: "_PUTIMAGE([dest], [source][, destHandle&][, srcHandle&])",
            doc: "Copies image data. Coordinates are (x1,y1)-(x2,y2).",
            params: &["dest", "source", "destHandle&", "srcHandle&"],
        }),
        "_WIDTH" => Some(FunctionSignature {
            label: "_WIDTH[(imageHandle&)]",
            doc: "Returns the width of an image or the current screen.",
            params: &["imageHandle&"],
        }),
        "_HEIGHT" => Some(FunctionSignature {
            label: "_HEIGHT[(imageHandle&)]",
            doc: "Returns the height of an image or the current screen.",
            params: &["imageHandle&"],
        }),

        // Input functions
        "INKEY$" => Some(FunctionSignature {
            label: "INKEY$",
            doc: "Returns a character from the keyboard buffer without waiting.",
            params: &[],
        }),
        "_KEYHIT" => Some(FunctionSignature {
            label: "_KEYHIT",
            doc: "Returns the keycode of a pressed key, including extended keys.",
            params: &[],
        }),
        "_KEYDOWN" => Some(FunctionSignature {
            label: "_KEYDOWN(keycode&)",
            doc: "Returns -1 if a key is currently held down, 0 otherwise.",
            params: &["keycode&"],
        }),
        "_MOUSEX" => Some(FunctionSignature {
            label: "_MOUSEX",
            doc: "Returns the current mouse X coordinate.",
            params: &[],
        }),
        "_MOUSEY" => Some(FunctionSignature {
            label: "_MOUSEY",
            doc: "Returns the current mouse Y coordinate.",
            params: &[],
        }),
        "_MOUSEBUTTON" => Some(FunctionSignature {
            label: "_MOUSEBUTTON(button%)",
            doc: "Returns -1 if mouse button is pressed. 1=left, 2=right, 3=middle.",
            params: &["button%"],
        }),

        // File functions
        "FREEFILE" => Some(FunctionSignature {
            label: "FREEFILE",
            doc: "Returns the next available file number.",
            params: &[],
        }),
        "EOF" => Some(FunctionSignature {
            label: "EOF(fileNumber%)",
            doc: "Returns -1 if at end of file, 0 otherwise.",
            params: &["fileNumber%"],
        }),
        "LOF" => Some(FunctionSignature {
            label: "LOF(fileNumber%)",
            doc: "Returns the length of an open file in bytes.",
            params: &["fileNumber%"],
        }),
        "LOC" => Some(FunctionSignature {
            label: "LOC(fileNumber%)",
            doc: "Returns the current position in an open file.",
            params: &["fileNumber%"],
        }),
        "_FILEEXISTS" => Some(FunctionSignature {
            label: "_FILEEXISTS(filename$)",
            doc: "Returns -1 if file exists, 0 otherwise.",
            params: &["filename$"],
        }),
        "_DIREXISTS" => Some(FunctionSignature {
            label: "_DIREXISTS(path$)",
            doc: "Returns -1 if directory exists, 0 otherwise.",
            params: &["path$"],
        }),

        // System/misc functions
        "TIMER" => Some(FunctionSignature {
            label: "TIMER[(accuracy!)]",
            doc: "Returns seconds since midnight. Optional accuracy parameter.",
            params: &["accuracy!"],
        }),
        "_SCREENY" | "_SCREENX" => Some(FunctionSignature {
            label: "_SCREENX / _SCREENY",
            doc: "Returns the screen position of the window.",
            params: &[],
        }),
        "COMMAND$" => Some(FunctionSignature {
            label: "COMMAND$[(n%)]",
            doc: "Returns command line arguments. N specifies which argument.",
            params: &["n%"],
        }),
        "ENVIRON$" => Some(FunctionSignature {
            label: "ENVIRON$(name$)",
            doc: "Returns the value of an environment variable.",
            params: &["name$"],
        }),

        _ => None,
    }
}
