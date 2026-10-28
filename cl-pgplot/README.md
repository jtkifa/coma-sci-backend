# pgplot - Common Lisp front end to Pearson's PGPLOT C/Fortran vintage graphics library

See: http://www.astro.caltech.edu/~tjp/pgplot/

Requires libcpglot.so and libpgplot.so (or .dylib for OSX) to be installed
in standard location accessible to CFFI.

Most functionality of pgplot is supported


A very nice feature of pgplot library is the use of an X11 Motif based server that maintains an image on screen, while not freezing the program like a single threaded GUI program would.



## Requires

* cffi
* waaf-cffi
* ps2pdf   (not needed if ````(pushnew :pgplot-does-pdf cl:*features*)````  is disabled in pgplot.asd; this will disable conversion of PS to PDF outputs using external program ps2pdf)


Example usage, a small subset of possible functions.

````

(asdf:load-system "pgplot")

(deparameter *p* (pgplot:open-device :x11))

(pgplot:box *p*)

(pgplot:connect *p* #(0 1) #(0 1))

(pgplot:erase *p*)


;;;;; Running the demo routines

(load "pgplot-examples.lisp")
;;   OR
(asdf:load-system "pgplot/examples")  

;; run all demos with X11 pgplot output device
(pgplot-examples:run-all-demos)

;; run individual demos, with file outputs
(pgplot-examples::demo-plot-1 :device :ps :filename "demo1.ps")

;; run all demos, creating gif files in a directory DEMO-PLOTS
(pgplot-examples:run-all-demos-to-gif)

````
