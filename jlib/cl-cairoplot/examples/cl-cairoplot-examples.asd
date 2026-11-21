;;;; cl-cairoplot-examples.asd — example plots for cl-cairoplot
;;;;
;;;;   (asdf:load-system "cl-cairoplot-examples")
;;;;   (cl-cairoplot-examples:make-examples)   ; writes example-plots/*.pdf
;;;;
;;;; examples.lisp is the framework and shared data; every other file is
;;;; one example, a guide to one feature area.  Files are loaded, and
;;;; examples listed, in this order.

(asdf:defsystem #:cl-cairoplot-examples
  :description "Example plots for cl-cairoplot, one per file"
  :author "JK"
  :license "MIT"
  :depends-on (#:cl-cairoplot)
  :serial t
  :components ((:file "examples")
               (:file "minimal-plot")
               (:file "basic-plot")
               (:file "errorbars-and-legends")
               (:file "histograms")
               (:file "bar-charts")
               (:file "box-and-violin-plots")
               (:file "step-and-area-charts")
               (:file "pie-charts")
               (:file "heatmaps")
               (:file "polar-plots")
               (:file "corner-plot")
               (:file "time-axes")
               (:file "panels")
               (:file "insets")
               (:file "erasing")
               (:file "scatter-by-value")
               (:file "vector-fields")
               (:file "axis-styles")
               (:file "special-axes")
               (:file "sky-projection")
               (:file "sky-reprojection")
               (:file "all-sky-projections")
               (:file "sky-coordinate-frames")
               (:file "tilted-all-sky")
               (:file "rotated-viewport")
               (:file "viewport-targets")
               (:file "coordinate-systems")
               (:file "clipping-regions")
               (:file "shapes-and-rotation")
               (:file "skewed-coordinates")
               (:file "fonts-and-text")
               (:file "markers")
               (:file "astronomical-symbols")
               (:file "images")
               (:file "image-placement")
               (:file "contours")
               (:file "filled-contours-and-density")
               (:file "curved-contour-labels")
               (:file "halos")
               (:file "halo-styles")
               (:file "annotated-image")
               (:file "surfaces")
               (:file "surface-details")
               (:file "colors")))
