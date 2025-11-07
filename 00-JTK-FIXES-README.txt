

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
       to astro/ breaks parallelism with canonical source tree.

Solution: instead of coma-json-server put COMA-PROJECT into astro
          Can always move it one dir up later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Issue: astorb was a bit of a mess with its loading.

Solution: rewrite the database loading code to be a lot cleaner, with
          better ways of telling it whether it should load database
	  at startup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Issue: building a single app isn't really the normal Lisp thing
       to do

Solution: remove load-all.lisp, and remove app building.
          Change the Dockerfile to run a coma-sci-backend.sh
	  script that in turns runs astro/COMA-PROJECT/Scripts/
