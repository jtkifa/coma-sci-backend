
(asdf:defsystem phot-calib
  :depends-on (astro-catalog terapix imutils astro-obj bordeaux-threads
			     astro-coords three-vector sky-project 
			     phot-transforms bayes-outlier bootstrap 
			     instrument-id
			     cl-cairoplot ;; for diagnostics
			     csv-read)
  :components 
  ((:file "phot-calib-package" :depends-on ())
   (:file "fix-mag-aper" :depends-on ("phot-calib-package"))
   (:file "phot-calib" :depends-on ("phot-calib-package" "fix-mag-aper"))
   (:file "image-image-calib" :depends-on ("phot-calib-package" "phot-calib"))
   (:file "mag-trans" :depends-on ("phot-calib-package"))
   (:file "diagnostics" :depends-on ("phot-calib-package" "phot-calib"))
   ;(:file "validate" :depends-on ("phot-calib-package"))
  ))
