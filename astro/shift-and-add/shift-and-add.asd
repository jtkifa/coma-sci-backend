
;; shift and add images using terapix

(asdf:defsystem shift-and-add
  :depends-on (terapix cfitsio slalib-ephem file-io 
	       bordeaux-threads imutils logger instrument-id)
  :components 
  ((:file "shift-and-add-package"
    :depends-on ())
   (:file "saaplan"
    :depends-on ("shift-and-add-package"))
   (:file "utils"
    :depends-on ("shift-and-add-package" "saaplan"))
   (:file "shift-and-add"
    :depends-on ("shift-and-add-package" "utils" "saaplan"))
   (:file "object-locators"
    :depends-on ("shift-and-add-package" "saaplan" "utils"))
   (:file "preprocessor" ;; parent image preprocessor class
    :depends-on ("shift-and-add-package" "saaplan" "utils"))
   (:file "static-sky-subtract"  ;; subclass that actually does something 
    :depends-on ("shift-and-add" "preprocessor" "saaplan" "utils"))
   (:file "weighter" ;; parent image weighter class
    :depends-on ("shift-and-add-package"  "saaplan" "utils"))
   (:file "badpix-masker"  ;; mask from  instrument-id:get-badpix-function-for-fits-file  (if defined)
    :depends-on ("shift-and-add-package" "weighter" "saaplan" "utils"))
   (:file "simple-masker"  ;; mask from detections in individual images
    :depends-on ("shift-and-add-package" "weighter" "saaplan" "utils"))
   (:file "stacked-masker" ;; mask from detections in stack image
    :depends-on ("shift-and-add-package" "simple-masker" "weighter"  "saaplan" "utils"))

   
   (:file "simulated-object"    
    :depends-on ("shift-and-add-package" "preprocessor" "saaplan"))
   ))


