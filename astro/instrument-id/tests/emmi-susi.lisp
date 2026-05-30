

(eval-when (:load-toplevel :execute)
  (asdf:load-system "instrument-id"))

(defparameter *data-topdir*
  (or (uiop:getenv "EMMI-SUSI-DIR")
      "/Volumes/12TB/Scratch/HALE_BOPP"))


(defparameter *emmi-susi-raw-dirs*
  (mapcar 'namestring
	  (append
	   (directory (format nil "~A/raw/*EMMI*" *data-topdir*))
	   (directory (format nil "~A/raw/*SUSI*" *data-topdir*)))))

(defun get-inst-type (dir)
  (let ((file-list (mapcar 'namestring
			   (directory
			    (format nil "~A/*.fits" dir)))))
    (instrument-id:identify-instrument (first file-list))))


(defparameter *raw-insts*
  (mapcar 'get-inst-type *emmi-susi-raw-dirs*))

(defun get-deepest-image-for-dir (dir)
  (let ((file-list (mapcar 'namestring
			   (directory
			    (format nil "~A/*.fits" dir)))))
    (loop with etmax = 0
	  with bestfits = nil
	  for fits in file-list
	  for exptime = (instrument-id:get-exptime-for-fits fits)
	  for type = (instrument-id:get-object-type-for-fits fits)
	  when (and (> exptime etmax)
		    (eq type :object))
	    do (setf etmax exptime
		     bestfits fits)
	  finally (return (values bestfits exptime)))))

(defparameter *deepest-raw*
  (mapcar 'get-deepest-image-for-dir *emmi-susi-raw-dirs*))

(defun make-wcs-corrected-version (fits &key (verbose t))
  (let ((newfits (string-utils:replace-substring ".fits" "_WCS.fits" fits)))
    (alexandria:copy-file fits newfits)
    (sb-posix:chmod newfits #o666)
    ;;
    (cf:with-open-fits-file (newfits ff :mode :io)
      (loop for iext from 1 to (cf:fits-file-num-hdus ff)
	    do
	       (cf:move-to-extension ff iext)
	       (when (= (cf:fits-file-current-image-ndims ff) 2)
		 (when verbose
		   (format t "Found image in ~A[~A]" newfits iext))
		 (let ((wcs (instrument-id:get-initial-wcs-for-fits
			     fits ;; use original fits
			     :extension iext)))
		   (cf:write-wcs wcs ff :extension iext)))))))
