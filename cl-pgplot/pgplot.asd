

;; asdf file for pgplot

;; enable pdf plots by using ps2pdf package to convert
;; ps to pdf
(pushnew :pgplot-does-pdf cl:*features*)

(asdf:defsystem pgplot
    :depends-on (waaf-cffi #+pgplot-does-pdf ps2pdf)
    ;;
    :components
    ((:file "pgplot-package")
     (:file "pgplot-ffi" :depends-on ("pgplot-package" "pgplot-load-libs"))
     (:file "pgplot-load-libs" :depends-on ("pgplot-package"))
     (:file "pgplot-fix-ps" :depends-on ("pgplot-package"))
     (:file "pgplot" :depends-on ("pgplot-ffi"))
     (:file "pgplot-extras" :depends-on  ("pgplot"))
     (:file "fancy-plots" :depends-on  ("pgplot"))
     (:file "viridis" :depends-on  ("pgplot"))
     (:file "colormap" :depends-on  ("pgplot" "viridis"))
     ))	
    
(asdf:defsystem pgplot/examples
  :depends-on (pgplot)
  :components
  ((:file "pgplot-examples")))




    
