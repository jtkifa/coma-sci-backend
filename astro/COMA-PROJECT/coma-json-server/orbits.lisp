

(in-package coma-json-server)


(def-json-command get-orbit (json-req)
  (with-json-command-setup (json-req)
    (let* ((method (get-param "METHOD" :default "MPC"))
	   (object (get-param "OBJECT" :required t))
	   ;; MJD at which to get orbit (reliable for JPL only)
	   (mjd (get-param "EPOCH-MJD")))

    (cond ((equalp method "MPC")
	   (multiple-value-bind (elem err)
	       ;; don't trust the MJD really
	       (mpc:get-mpc-elements-with-caching object :mjd mjd)
	     (jcom-test-expr (not elem)
			     "RETRIEVAL-ERROR"
			     (format nil "MPC-ERROR: ~A" err))
	     (set-param "ORBIT"
			(comet-elem-to-json
			 elem
			 :extra-fields `(("METHOD" . ,method))))))
      
	  ((equalp method "JPL-HORIZONS")
	   (multiple-value-bind (elem err)
	       (jpl-horizons:get-jpl-horizons-elements-with-caching
		object :mjd mjd :ntries 2)
	      (jcom-test-expr (not elem)
			     "RETRIEVAL-ERROR"
			     (format nil "JPL-ERROR: ~A" err))
	     (set-param "ORBIT"
			(comet-elem-to-json
			 elem
			 :extra-fields `(("METHOD" . ,method))))))
	  (t
	   (setf (json-object-error json-resp)
		 (make-error-object
		  :error "UKNOWN-METHOD"
		  :desc (format nil "UNKNOWN RETRIEVAL METHOD: ~A" method))))))))
	  
