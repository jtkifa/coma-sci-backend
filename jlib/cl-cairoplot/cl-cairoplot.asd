;;;; cl-cairoplot.asd — PGPLOT-like plotting with cl-cairo2

(asdf:defsystem #:cl-cairoplot
  :description "Plotting to PDF, PS, SVG, PNG and pixel buffers with cl-cairo2, modeled after cl-pgplot"
  :author "JK"
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-cairo2 #:cl-cairo-hershey-fonts #:cffi #:float-utils
               #:bordeaux-threads)
  :serial t
  :components ((:file "cl-cairoplot-package")
               (:file "colors")
               (:file "colormap-data")
               (:file "colormaps")
               ;; the only file that may refer to cl-cairo2
               (:file "cairo-graphics")
               (:file "fonts")
               ;; viewport first, so its inline transforms are known
               (:file "viewport")
               (:file "plot-device")
               (:file "clipping")
               (:file "drawing")
               (:file "axes")
               (:file "time-axes")
               (:file "markers")
               (:file "image")
               (:file "contours")
               (:file "plot3d")
               (:file "charts")
               (:file "categorical")
               (:file "areas")
               (:file "vectors")
               (:file "pie")
               (:file "heatmap")
               (:file "corner")
               (:file "legend")
               (:file "layout")
               (:file "sky")
               (:file "polar")))

;; Examples: system cl-cairoplot-examples in examples/
;;   (asdf:load-system "cl-cairoplot-examples")
;;   (cl-cairoplot-examples:make-examples)   ; writes example-plots/*.pdf
