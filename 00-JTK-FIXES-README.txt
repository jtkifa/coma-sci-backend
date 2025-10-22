

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Error: UndefinedVar: Usage of undefined variable '$LD_LIBRARY_PATH' (line 137)

Solution: In Dockerfile add
  ARG LD_LIBRARY_PATH=""
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Issue: quicklisp yason package (among other things) uses internal
       Lisp float parsing but I prefer more versatile custom parsing.

Solution:       
       substitute jlib/jtk-json in source tree, modify coma-json-server,
       and remove --eval '(ql:quickload :yason)'  from Dockerfile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Issue: moving coma-json-server from its position in astro/COMA-PROJECT/
       to astro/ breaks parallel to canonical source tree.

Soluton:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
