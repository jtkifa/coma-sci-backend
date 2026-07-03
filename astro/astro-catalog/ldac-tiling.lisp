;; generate a tiling of ldac-catalogs fits files, that fills a circle or
;; a square


(in-package astro-catalog)


;; return a list of dra,ddec centers for a tiling, so the the
;; inscribed squares of the pointings are a square tesselation,
;; meaning the stop size is 2xsqrt(2) times the catalog radius,
;; or sqrt(2) times the catalog diameter
;;
;; if FIELD-SHAPE is :CIRCLE, truncate the tiling so it fills
;; circular field, otherwise do the whole :SQUARE
(defun %generate-catalog-tiling (field-diam cat-diam
				 &key (field-shape :circle))
  (declare (type (real (0)) field-diam cat-diam)
	   (type (member :circle :square) field-shape))
  (loop
    with stepsize = (* #.(sqrt 2.0) 0.5 cat-diam)
    with tlist = nil
    for ix from 0
    for dx = (* ix stepsize)
    until (>= dx (* 0.5 field-diam))
    do
       (loop
	 for iy from 0
	 for dy = (* iy stepsize)
	 until (>= dy (* 0.5 field-diam))
	 do
	    (loop
	      for sx in (if (plusp ix) '(-1 1) '(1)) ;; don't repeat +-0
	      when 
		;; don't do pointings beyond edge of field unless a square
	      (or (eq field-shape :square)
		  (< (sqrt (+ (* dx dx) (* dy dy)))
		     (* 0.5 (+ field-diam cat-diam))))
	      do
		 (loop for sy in (if (plusp iy) '(-1 1) '(1))  ;; don't repeat +-0
		       do
			  (push (list (* sx dx) (* sy dy)) tlist))))
    finally (return (reverse tlist))))


(defun %make-astref-catalog-filename (ra dec diam/deg catalog-type)
  (format nil "~A_~A~A_~A.cat"
	  catalog-type
	  (ra-dec:deg->hms-string ra :separator-strings #("h" "m" "s")
					 :rounding 0)
	  (ra-dec:deg->dms-string dec :separator-strings #("d" "m" "s")
				      :rounding 0)
	  (format nil "~5,'0darcsec" (round (* 3600 diam/deg)))))


(defun retrieve-ldac-catalog-tiling-for-pointing
   (ra0 dec0 field-diam cat-diam catalog-type target-directory
    &key
      (field-shape :CIRCLE)
      (logger logger:*default-logger*)
      (mjd (astro-time:ut-to-mjd (get-universal-time)))
      (overwrite t))
    "Given a RA0,DEC0,FIELD-DIAM for a field to fill with catalog
pointings, retrieve catalogs of diameter CAT-DIAM and of type
CATALOG-TYPE, placing their FITS LDAC files into DIRECTORY.

If FIELD-SHAPE is :CIRCLE, truncate the tiling so it fills circular
field, otherwise do the whole :SQUARE.

LOGGER is a LOGGER object (NIL,T,STREAM, LOGGER) as defined in LOGGER
package.

MJD is the epoch to which to move stars using proper motions, if present.
It defaults to the moment the function is called.

OVERWRITE determines whether old catalogs are overwritten."
   

 (declare (type (real (0)) field-diam cat-diam)
	  (type (member :circle :square) field-shape))
  
  (setf target-directory
	(namestring
	 (concatenate 'string (string-right-trim "/" target-directory) "/")))

  (ignore-errors
   (ensure-directories-exist (format nil "~A/#DUMMY#" target-directory)))
  
  (when (not (cl-fad:directory-exists-p target-directory))
    (error "Target directory ~A does not exist and cannot be created"
	   target-directory))
  
  (let ((tiling (%generate-catalog-tiling field-diam cat-diam
					  :field-shape field-shape)))
    (loop with n = (length tiling)
	  for i from 1
	  for offset-pair in tiling
	  do
	     #+sbcl (sb-ext:gc :full t) ;; unexplained heap exhaustion
	     (multiple-value-bind (ra dec)
		 (sky-project:tan-de-project 
		  (* 1d0 (first offset-pair))
		  (* 1d0 (second offset-pair))
		  (* 1d0 ra0) (* 1d0 dec0)
		  :units :degrees)
	       ;;
	       (let* ((catalog-name (%make-astref-catalog-filename
				     ra dec cat-diam catalog-type))
		      (fullpath (format nil "~A~A" target-directory catalog-name))
		      (file-exists (probe-file fullpath)))
		 ;;
		 (cond
		   ((or overwrite (not file-exists)) ;; we are retrieving
		    (let ((catalog-object
			    (progn
			      ;;
			      (logger:writelog
			       logger
			       (format nil "Retrieving ~A catalog ~D/~D at ~,3F,~,3F to ~A"
				       catalog-type i n ra dec catalog-name)
			       :log-level 1)
			      ;;
			      (multiple-value-bind (cat err)
				  (ignore-errors
				   (get-catalog-object-of-type 
				    ra dec (* 0.5d0 cat-diam) catalog-type ))
				(when (not cat)
				  (logger:writelog
				     logger
				     (format
				      nil
				      "Failed to retrieve ~A catalog at ~,3F,~,3F to ~A - ERROR ~A"
				      catalog-type ra dec catalog-name err)))
				cat))))
					     
		      ;;
		      (when catalog-object
			(when file-exists
			  (logger:writelog
			   logger
			   (format nil "Overwriting existing catalog ~A"
				   fullpath)))
			(astro-catalog:write-catalog-to-fits-ldac 
			 catalog-object fullpath
			 :mjd mjd
			 :overwrite overwrite))))
		    ;;
		    ((and file-exists (not overwrite)) ;; not retrieving
		     (logger:writelog
		      logger
		      (format
		       nil
		       "Not retrieving ~A catalog ~D/~D at ~,3F,~,3F - File ~A exists and OVERWRITE=FALSE"
		       catalog-type i n ra dec catalog-name )))))))))
			




