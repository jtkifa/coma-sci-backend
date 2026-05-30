

;; asdf file for wcs


(asdf:defsystem wcs
    ;; needs jutils.asd to work
    :depends-on ()
    ;;
    :components
  ((:file "wcs-package")
   (:file "wcs-structs" :depends-on ("wcs-package"))
   (:file "pv-corrections" :depends-on ("wcs-package" "wcs-structs"))
   (:file "wcs" :depends-on ("wcs-package" "pv-corrections" "wcs-structs"))
   (:file "wcs-build" :depends-on ("wcs-package" "wcs-structs"))))




    
