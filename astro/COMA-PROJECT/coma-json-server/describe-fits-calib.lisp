#|

Request:

{
  "TYPE":"REQUEST",
  "COMMAND":"DESCRIBE-FITS-CALIB",
  "ID":"123abc",
  "PARAMETERS": {"FITS-FILE":"/dir/sample.fits", 
                 "EXTENSION":1 // if extension is not the default one
                 }
}


Response:

{
  "COMMAND": "DESCRIBE-FITS-CALIB",
  "ID": "123abc",
  "PARAMETERS": {
        "TYPE": "PARAMETERS"
        "CALIB-DESCRIPITON":
            {
              "TYPE": "CALIB-DESCRIPTION",
              "FITS-FILE": "/dir/sample.fits",
              "EXTENSION":1
              "WCS-CALIB": true/false, // is WCS calibrated?
              "WCS-CALIB-SOURCE": "source", // string describing source of WCS calibration - "COMA" if we did it
              "WCS-CALIB-CATALOG": "catalog", // string describing the catalog used
              "WCS: { ... description of WCS fields },
              "PHOTOMETRY-CALIB": true/false, // is photometric zeropoint calibrated?
              "PHOTOMETRY-CALIB-SOURCE": "source", // string describing phot calib - "COMA" if we did it
              "PHOTOMETRY-CALIB-CATALOG": "catalog", // string describing catalog used
              "ZPMAG": xx.xxx, // photometric zeropoint, mag, in ADU units, no exptime adjustment
              "ZPMAGERR": xx.xxx // zeropoint error 
              "FILTER": "RSDSS", // the filter name assumed for calibration
            }
}
|#

(in-package coma-json-server)


(def-json-command describe-fits-calib (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   (inst (or
		  (ignore-errors (instrument-id:identify-instrument fits-file))
		  (return-with-error
		   "COULD-NOT-IDENTIFY-FITS-FILE"
		   (format nil "Could not identify ~A using INSTRUMENT-ID" fits-file))))
	   ;; if extension=NIL then use the default single-image
	   ;; extension, or the first finite image extension
	   (extension  (get-param "EXTENSION")))
      
      
      ;; the work is outsourced to make-describe-fits-calib-object
      (multiple-value-bind (calib-object err err-desc)
	  (make-describe-fits-calib-object fits-file extension :instrument inst)
	(when (not calib-object)
	  (return-with-error err err-desc))
	
	(set-param "CALIB-DESCRIPTION"  calib-object)))))



(defun make-describe-fits-calib-object (fits-file extension &key instrument)
"Make a  CALIB-DESCRIPITON JSON object for a fits file.
Return (VALUES CALIB-OBJ ERROR-NAME ERROR-DESC) where the errors are
returned if CALIB-OBJ is NIL."
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (return-from retblock (values NIL err-name err-desc))))
      (multiple-value-bind (result err)
	  (progn ;ignore-errors
	   (let* ((inst (or instrument (instrument-id:identify-instrument fits-file))))
	     
	     (cfitsio:maybe-with-open-fits-file (fits-file ff)
	       ;; move to correct extension
	       (cond ((typep inst 'instrument-id:onechip)
		      (cf:move-to-extension
		       ff
		       (setf extension ;; kludge
			    (instrument-id:get-image-extension-for-onechip-instrument inst fits-file))))
		     (extension
		      (multiple-value-bind (res %err) (cf:move-to-extension ff extension)
			(when (not res) (return-with-error
					 "ERROR-MOVING-TO-EXTENSION"
					 (format nil "Error moving to extension ~A of ~A: ~A" extension fits-file %err))))))
	       (let* ((dhash (make-hash-table :test 'equalp))
		      (wcs (cf:read-wcs ff))
		      (wcs-catalog (cf:read-fits-header ff "COMA.WCS.CATALOG")) ;; can be NULL
		      (wcs-source  (if wcs-catalog
				       "COMA"
				       (instrument-id:preproc-wcs-origin inst)))
		      (wcs-calib (or wcs-catalog wcs-source))		      
		      (filter (or (cf:read-fits-header ff "COMA.PHOT-CALIB.FILTER")
				  (string
				   (or (instrument-id:get-standard-filter-for-instrument
					inst fits-file)
				       "UNKNOWN"))))		      
		      (zpmag (cf:read-fits-header ff "COMA.PHOT-CALIB.ZPMAG"))
		      (zpmagerr (cf:read-fits-header ff "COMA.PHOT-CALIB.ZPMAGERR"))
		      (phot-calib-catalog (cf:read-fits-header ff "COMA.PHOT-CALIB.CATALOG"))
		      (phot-calib-source (if zpmag
					     "COMA"  ;; we read out COMA header
					     (instrument-id:preproc-phot-calib-origin inst)))
		      (phot-calib nil))

		 (when (not zpmag)
		   (multiple-value-setq (zpmag zpmagerr)
		     (instrument-id:get-calibrated-zeropoint-for-instrument
		      inst fits-file :flux-units :adu :exposure :exptime)))

		 (setf phot-calib (if zpmag t nil))
		      
		      
	       (flet ((setval (key val)
			(setf (gethash key dhash) val)))
		 (setval "TYPE" "CALIB-DESCRIPTION")
		 (setval "FITS-FILE" fits-file)
		 (setval "EXTENSION" extension)
		 (setval "WCS-CALIB" (if wcs-calib 'yason:true 'yason:false))
		 (setval "WCS-CALIB-SOURCE" wcs-source)
		 ;;(setval "WCS-CALIB-CATALOG" wcs-catalog)  ;; make it consistent with calibrate routine
		 (setval "WCS-CATALOG" wcs-catalog)
		 (setval "WCS" (if wcs (hashify-wcs wcs)))
		 (setval "WCS-PIXEL-SCALE" (if wcs (ignore-errors (wcs:get-pixel-scale-for-wcs wcs))))
		 (setval "PHOTOMETRY-CALIB"  (if phot-calib 'yason:true 'yason:false))
		 (setval "PHOTOMETRY-CALIB-SOURCE" phot-calib-source)
		 ;;(setval "PHOTOMETRY-CALIB-CATALOG" phot-calib-catalog ) ;; make it consistent with calibrate routine
		 (setval "PHOT-CATALOG" phot-calib-catalog)
		 (setval "ZPMAG"  zpmag)
		 (setval "ZPMAGERR" zpmagerr)
		 (setval "FILTER" filter))
		 ;; return the json dhash into RESULT
		 dhash))))

	
	;; return the json result or (values NIL err err-desc)
	(if result
	    (return-from retblock result)
	    (return-with-error
	     "INTERNAL-ERROR-IN-MAKE-DESCRIBE-FITS-CALIB-OBJECT"
	     (format nil "Internal error in make-describe-fits-calib-object: <~A>" err)))))))
