#|

Cairo-based rendering backend for fits-stamp.

Replaces PGPLOT with cl-cairo2 for thread-safe, modern rendering.

Usage is similar to fits-stamp but uses Cairo contexts:

  (fits-stamp-cairo:make-stamp
      "./test.fits" :pdf 1000 1000 400
      :output-file "./test.pdf"
      :markers (list (make-instance 'fits-stamp-cairo:crosshairs
                                    :x 1000 :y 1000)))

|# 

(defpackage fits-stamp-cairo
  (:use #:cl)
  (:export
   ;; main entry points
   #:make-stamp
   #:make-pdf-stamp
   #:make-svgz-stamp
   ;; context class (for advanced use)
   #:stamp-context
   ;; markers
   #:crosshairs
   #:arrow
   #:circle
   #:points
   #:compass-rose
   #:orbit-direction-axes
   #:pixel-scale-marker
   #:text-marker
   ;;
   #:fill-orbit-direction-axes-using-comet-elem))
