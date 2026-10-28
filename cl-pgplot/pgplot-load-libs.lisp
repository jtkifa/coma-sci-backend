
;; routines for loading pgplot libraries for cmucl - would like
;; to put this into into pgplot-cmucl, but cmucl barfs about
;; undefined foreign symbols unless libraries get loaded before
;; they are referenced -- (eval-when (load eval compile)...) does
;; not do the trick, alas

(in-package pgplot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pgplot)

#-linux-raspberry-pi-arm32
(eval-when (:load-toplevel)

  ;; allow environment variable PGPLOT-GIZA to activate :pgplot-giza
  (when (uiop:getenv "PGPLOT_GIZA")
    (pushnew :pgplot-giza *features*))
    
  ;; use giza library instead
  (defparameter *using-pgplot-giza* (find :pgplot-giza *features*))
  
  (cffi:define-foreign-library libcpgplot
    (:darwin (:or "libcpgplot.dylib"))
    (:unix (:or "libcpgplot.so" "libcpgplot.so.5")))
  
  (cffi:define-foreign-library libcpgplot/giza
    (:darwin (:or "libcpgplot_giza.dylib"))
    (:unix (:or "libcpgplot_giza.so")))

  (if *using-pgplot-giza*
      (progn (cffi:use-foreign-library libcpgplot/giza)
	     (setf *using-pgplot-giza* t))
      (cffi:use-foreign-library libcpgplot)))

#+linux-raspberry-pi-arm32 ;; temporary kludge
(eval-when (:load-toplevel)
   (cffi:load-foreign-library "/usr/local/lib/libpgplot.so")
   (cffi:load-foreign-library "/usr/local/lib/libcpgplot.so"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



