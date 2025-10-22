

(in-package coma-json-server)





#|

{
  "TYPE":"REQUEST",
  "COMMAND":"MAKE-ISOCHRONES-AND-ISODYNES",
  "ID":"123abc",
  "PARAMETERS": {                 
                 // the usual ORBIT object, or strings describing where to obtain the orbit
                 "ORBIT": {ID: .. EPOCH-MJD: .. ...}, // or "JPL-ORBIT" or "MPC-ORBIT"
              
                 // MJD at which to take snapshot
                 "MJD":58849.0,
 
                 // if given will compute pixel coordinates of isochrone using WCS
                 "FITS-FILE": "some.fits",
                 "EXTENSION": 1,
        
                 // if the orbit is determined using MPC-ORBIT or JPL-ORBIT
                 "OBJECT-NAME": "2P",

                 // observatory from which we are drawing isochrones
                 "OBSERVATORY":"568",
 
                 // vector of isochrones requested
                 "ISOCHRONE-REQUESTS":
                 [ {
                    "ID":"Isochrone-1",
                    "DT":-1e6, // dust emitted this many seconds before MJD
                    // dust betas in this isochrone
                    "BETA-VECTOR":[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 0.9, 1.0, 1.1],
                   }, ...
                 ],

                 // vector of isodynes requested
                 "ISODYNE-REQUESTS":
                 [ {
                    "ID":"Isodyne-1",
                    // the single dust beta
                    "BETA":0.1,
                    "DT-VECTOR":[-1e6,-2e6]
                   }, ...
                 ] 

                }
}


|#

(in-package coma-json-server)


(def-json-command make-isochrones-and-isodynes (json-req)
  (with-json-command-setup (json-req)
    (let* ((object-name
	     (get-param "OBJECT-NAME"))
	   (mjd (ignore-errors (* 1d0 (get-param "MJD"))))
	   (json-orbit (get-param "ORBIT"))
	   ;; for WCS only
	   (fits-file (get-param "FITS-FILE"))
	   (extension (get-param "EXTENSION"))
	   (wcs (if fits-file
		    (ignore-errors
		     (cf:read-wcs fits-file :extension extension))))
	   ;;
	   orbit orbit-error
	   (observatory (get-param "OBSERVATORY" :default "geocenter"))
	   ;;
	   (isochrone-request-vector (get-param "ISOCHRONE-REQUESTS"))
	   (isodyne-request-vector   (get-param "ISODYNE-REQUESTS")))

      ;; preserve the source of the orbit error
      (when json-orbit
	(multiple-value-setq (orbit orbit-error)
	  (get-orbit-using-method
	   json-orbit object-name :mjd mjd)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      (when (and json-orbit (not orbit))
	(return-with-error
	 "FAILED-TO-OBTAIN-ORBIT"
	 (format
	  nil
	  "Failed to parse or obtain provided ORBIT=\"~A\" - error is '~A'"
	  json-orbit orbit-error)))

      
      (when (not (slalib-ephem:comet-elem-p orbit))
	(return-with-error
	 "NO-ORBIT-OBTAINED"
	 "An orbit is required for object defun."))
      
      (when (and fits-file (not wcs))
	(return-with-error
	 "FAILED-TO-READ-WCS"
	 (format
	  nil 
	  "Could not get WCS from fits file ~A, extension ~A" fits-file extension)))
      
      (when (and isochrone-request-vector
		 (not (and (vectorp isochrone-request-vector)
			   (plusp (length isochrone-request-vector)))))
	(return-with-error
	 "ISOCHRONE-REQUESTS-INVALID"
	 "ISOCHRONE-REQUESTS not a vector of at least length 1"))
      (when (and isodyne-request-vector
		 (not (and (vectorp isodyne-request-vector)
			   (plusp (length isodyne-request-vector)))))
	(return-with-error
	 "ISODYNE-REQUESTS-INVALID"
	 "ISODYNE-REQUESTS not a vector of at least length 1"))
      
      (when (not (or isochrone-request-vector isodyne-request-vector))
	(return-with-error
	 "NO-ISOCHRONES-OR-ISODYNES-REQUESTED"
	 "Neither ISOCHRONE-REQUEST-VECTOR nor ISODYNE-REQUEST-VECTOR given; nothing to do."))
      
      (flet ((insert-pix-vectors (iso-hash)
	       (when wcs
		 (let* ((ra-vector (gethash "RA-VECTOR" iso-hash))
			(dec-vector (gethash "DEC-VECTOR" iso-hash))
			(n (length ra-vector))
			(xpix-vector (make-array n))
			(ypix-vector (make-array n)))
		   (loop for ra across ra-vector
			 for dec across dec-vector
			 for i from 0
			 do (multiple-value-bind (xpix ypix)
				(wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)
			      (setf (aref xpix-vector i) xpix)
			      (setf (aref ypix-vector i) ypix)))
		   (setf (gethash "XPIX-VECTOR" iso-hash) xpix-vector)
		   (setf (gethash "YPIX-VECTOR" iso-hash) ypix-vector)))))
	
	;; isochrones
	(loop with rvec = isochrone-request-vector
	      with iso-results-list = nil
	      with iso-out = (make-hash-table :test 'equalp)
	      for i from 0
	      for iso-request across rvec
	      when (not (hash-table-p iso-request))
		do (return-with-error
		    "INVALID-ISOCHRONE-REQUEST"
		    "A member of ISOCHRONE-REQUEST-VECTOR is not a JSON structure.")
	      do (let ((id (or (gethash "ID" iso-request) "NONE"))
		       (beta-vec (gethash "BETA-VECTOR" iso-request))
		       (dt  (gethash "DT" iso-request)))
		   (when (or (not (vectorp beta-vec))
			     (not (every 'realp beta-vec))
			     (not (realp dt)))
		     (return-with-error
		      "INVALID-ARGUMENTS-IN-ISOCHRONE-REQUEST"
		      (format nil "Invalid DT=~A BETA-VECTOR=~A in ISOCHRONE-REQUEST[~D] ID=~A"
			      dt beta-vec i id)))
		   (multiple-value-bind (isochrone err)
		       (isochrones-isodynes:build-isochrone 
			orbit mjd dt beta-vec :observatory observatory)
		     (if (not isochrone)
			 (return-with-error
			  "ERROR-IN-BUILD-ISOCHRONE"
			  (format nil "Error in build-isochrone for request ~A, ID=~A: ~A"
				  i id err))
			 (progn ;; success
			   (setf (gethash "ID" iso-out) id)
			   (setf (gethash "DT" iso-out)
				 (aref (isocisod:iso-dt-vec isochrone) 0))
			   (setf (gethash "BETA-VECTOR" iso-out)
				 (isocisod:iso-beta-vec isochrone))
			   (setf (gethash "RA-VECTOR" iso-out)
				 (isocisod:iso-ra-vec isochrone))
			   (setf (gethash "DEC-VECTOR" iso-out)
				 (isocisod:iso-dec-vec isochrone))
			   (setf (gethash "XSKY-VECTOR" iso-out)
				 (isocisod:iso-xsky-vec isochrone))
			   (setf (gethash "YSKY-VECTOR" iso-out)
				 (isocisod:iso-ysky-vec isochrone)))))
		   (insert-pix-vectors iso-out)
		   (push iso-out iso-results-list))
	      finally
		 (set-param "ISOCHRONE-RESULTS" 
			    (nreverse (coerce iso-results-list 'vector))))
	

	;; isodynes
	(loop with rvec = isodyne-request-vector
	      with iso-results-list = nil
			 with iso-out = (make-hash-table :test 'equalp)
	      for i from 0
	      for iso-request across rvec
	      when (not (hash-table-p iso-request))
		do (return-with-error
		    "INVALID-ISODYNE-REQUEST"
		    "A member of ISODYNE-REQUEST-VECTOR is not a JSON structure.")
	      do (let ((id (or (gethash "ID" iso-request) "NONE"))
		       (beta (gethash "BETA" iso-request))
		       (dt-vec  (gethash "DT-VECTOR" iso-request)))
		   (when (or (not (vectorp dt-vec))
					(not (every 'realp dt-vec))
					(not (realp beta)))
		     (return-with-error
		      "INVALID-ARGUMENTS-IN-ISODYNE-REQUEST"
		      (format nil "Invalid DT-VECTOR=~A BETA=~A in ISODYNE-REQUEST[~D] ID=~A"
			      dt-vec beta i id)))
		   (multiple-value-bind (isodyne err)
		       (isochrones-isodynes:build-isodyne  
			orbit mjd dt-vec beta :observatory observatory)
		     (if (not isodyne)
			 (return-with-error
			  "ERROR-IN-BUILD-ISODYNE"
			  (format nil "Error in build-isodyne for request ~A, ID=~A: ~A"
				  i id err))
			 (progn ;; success
			   (setf (gethash "ID" iso-out) id)
			   (setf (gethash "DT-VECTOR" iso-out)
				 (isocisod:iso-dt-vec isodyne))
			   (setf (gethash "BETA" iso-out)
				 (aref (isocisod:iso-beta-vec isodyne) 0))
			   (setf (gethash "RA-VECTOR" iso-out)
				 (isocisod:iso-ra-vec isodyne))
			   (setf (gethash "DEC-VECTOR" iso-out)
				 (isocisod:iso-dec-vec isodyne))
			   (setf (gethash "XSKY-VECTOR" iso-out)
				 (isocisod:iso-xsky-vec isodyne))
			   (setf (gethash "YSKY-VECTOR" iso-out)
				 (isocisod:iso-ysky-vec isodyne)))))
		   (insert-pix-vectors iso-out)
		   (push iso-out iso-results-list))
	      finally
		 (set-param "ISODYNE-RESULTS" 
			    (nreverse (coerce iso-results-list 'vector))))
	))))
				    
				
