' Minimal OpenGL example for QB64Fresh
' Uses SUB _GL, _GLRENDER, _GLBEGIN/_GLEND, _GLVERTEX3F.
' Requires runtime built with opengl feature and system OpenGL.

SCREEN 12
_GLRENDER _ONTOP

DO
    _DISPLAY
    _LIMIT 60
LOOP UNTIL _KEYDOWN(27)

SUB _GL
    _GLCLEAR _GL_COLOR_BUFFER_BIT
    _GLBEGIN _GL_TRIANGLES
    _GLCOLOR3F 1.0, 0.0, 0.0
    _GLVERTEX3F 0.0, 0.5, 0.0
    _GLCOLOR3F 0.0, 1.0, 0.0
    _GLVERTEX3F -0.5, -0.5, 0.0
    _GLCOLOR3F 0.0, 0.0, 1.0
    _GLVERTEX3F 0.5, -0.5, 0.0
    _GLEND
END SUB
