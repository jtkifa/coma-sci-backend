

 
(in-package shift-and-add)


;; result structure for stacking
(defstruct stack-result
  error  ;; string representing error
  output-stack  ;; name of output fits file
  accepted-fits-list ;; list of accepted fits inputs
  rejected-fits-list ;; list of rejected fits inputs
  reject-reasons-list) ;; list of reasons for rejection


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note rejected fits and why rejected, in stack-result
(defun add-rejected-fits (fits reason sresult)
  (push (file-io:file-minus-dir fits)
	 (stack-result-rejected-fits-list sresult))
  (push reason
	(stack-result-reject-reasons-list sresult)))

;; note accepted fits in stack-result
(defun add-accepted-fits (fits sresult)
  (push (file-io:file-minus-dir fits)
	(stack-result-accepted-fits-list sresult)))

(defun set-stack-result-error (error-string sresult)
  (setf (stack-result-error sresult) error-string))

;; put in correct order at end
(defun reverse-stack-result (sresult)
  (setf (stack-result-accepted-fits-list sresult)
	(nreverse (stack-result-accepted-fits-list sresult)))
  (setf (stack-result-rejected-fits-list sresult)
	(nreverse (stack-result-rejected-fits-list sresult)))
  (setf (stack-result-reject-reasons-list sresult)
	(nreverse (stack-result-reject-reasons-list sresult))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shift-and-add-fits-list 
    (fits-list 
     &key
       (saaplan *default-saaplan*)
       (directory nil))
  "Shift and add a list of files FITS-LIST into IMAGEOUT-BASE.fits and 
IMAGEOUT.weight.fits.

SAAPLAN is the standard shift-and-add-plan (saaplan) structure.

DIRECTORY is the optional directory to prepend to the files in FITS-LIST.

Returns (VALUES SUCCESS STACK-RESULT)
"

  (when (not (typep (saaplan-locator saaplan) 'locator))
    (error "SAAPLAN-LOCATOR is not a LOCATOR object in ~A" saaplan))
  
  (prog*  ;; for return statement
      ((imageout-base (saaplan-imageout-base saaplan))
       (final-stack-file
	(format nil "~A/~A"
	 (string-right-trim "/" (saaplan-output-directory saaplan))
	 imageout-base))  
       (wcsgood-fits-list nil)
       (headerfile-list nil)
       (shifted-fits-list nil)
       (sresult (make-stack-result))

       ;; convert files to full paths
       (fits-list-full
	(loop for fits in fits-list
	      for fullfits =  (concatenate 'string (or directory "") 
					   (if directory "/" "") fits)
	      ;; we don't expand symlinks, so our work files get created
	      ;; in a directory with links to fits files
	      for namestring 
		= (file-io:full-namestring/no-symlink-expand fullfits)
	      
	      if (not namestring)
		do (saaplan-log-format
		    saaplan
		    "SHIFT-AND-ADD: ERROR ~A not found" fullfits)
		   (add-rejected-fits fullfits "FILE-NOT-FOUND" sresult)
	      else
		collect namestring))
       ;; the files in their new linked location
       (fits-list-linked nil)
       ;; the badpix functions, one for each fits file, extracted at an early
       ;; stage because they may be lost at extraction phase
       (badpix-func-list nil)
       ;; the fits files we end up combining, after filtering out bad ones
       (fits-working-list nil))


     ;; make the output directory
     (when (not (ignore-errors
		 (ensure-directories-exist
		  (concatenate 'string (string-right-trim "/" (saaplan-output-directory saaplan))
			       "/#IGNORE-ME#"))))
       (saaplan-log-format saaplan
			   "SHIFT-AND-ADD: ERROR - could not create or find output directory ~A"
			   (saaplan-output-directory saaplan))
       (set-stack-result-error "CANNOT-OBTAIN-OUTPUT-DIRECTORY" sresult)
       (return (values NIL sresult)))
     
     ;; symlink the files into the output directory.  This may also extract the relevant
     ;; extension, and return a list of badpix-functions (or NIL)
     (multiple-value-setq (fits-list-linked badpix-func-list)
       (symlink-or-copy-files-into-output-directory saaplan fits-list-full))
     
     
     ;; check WCS already in the file
     (loop
       with fits0 = nil 
       for fits in fits-list-linked
       ;; requiring wcsgood messes up stacking of PS1
       ;; for wcsgood = (cf:read-fits-header fits "WCSFITOK")
       for wcs 
	 = (cf:read-wcs
	    fits
	    :extension (instrument-id:get-image-extension-for-onechip-fits fits))
       ;; as'plode the fits name into the full UNIX path
       for fullhead = (concatenate 'string (file-io:file-basename fits) ".head")
       do
	  (if wcs
	      (progn
		;; set first wcs
		(when (not fits0)
		  (setf fits0 fits))
		;; if images are too spread out drop it from the list
		(multiple-value-bind (dist-ok imdist)
		    (if (not (eq fits fits0)) (images-too-far-p fits0 fits saaplan))
		  ;;
		  (if (and (not (eq fits fits0)) ;; not still the first image
			   dist-ok)
		      (progn
			(saaplan-log-format
			 saaplan
			 "SHIFT-AND-ADD: image ~A is too far (~A arcsec) from first usable image ~A - limit is ~A arcsec"
			 fits imdist fits0 (saaplan-max-sky-spread saaplan))
			(add-rejected-fits fits "IMAGE-TOO-FAR-FROM-FIRST" sresult))
		      
		      ;; else it is OK, and we accept it, and make the head file
		      (progn
			(saaplan-log-format 
			 saaplan "SHIFT-AND-ADD: Making head file for ~A using WCS its WCS~%" fits)
			(make-head-file-from-wcs fullhead wcs)
			(push fullhead headerfile-list) ;; for possible deletion
			(push fits wcsgood-fits-list)))))
	      (progn ;; not wcs
		(saaplan-log-format
		 saaplan
		 "SHIFT-AND-ADD: Could not find WCS in ~A.  Ignoring this image."
		 fits)
		(add-rejected-fits  fits "NO-WCS-FOUND" sresult) )))
     
     
     ;;
     ;; now wcsgood-fits-list has the list of successful files
     (setf wcsgood-fits-list (nreverse wcsgood-fits-list)) ;; to original order

     (when (not wcsgood-fits-list)
       (set-stack-result-error "NO-VALID-FILES-LEFT" sresult)
       (return (values NIL sresult))) ;; nothing done
     
     (setf fits-working-list wcsgood-fits-list)

     ;; add all the remaining files to the accepted list
     (mapcar (lambda (fits) (add-accepted-fits fits sresult)) fits-working-list)

     
     ;; run the preproc function, maybe setting fits-working-list
     ;; to new files
     (when (saaplan-image-preproc saaplan)
       (saaplan-log-format saaplan "SHIFT-AND-ADD: Running image preprocessor function on ~A" 
			   fits-working-list)
       (setf fits-working-list
	     (run-image-preproc  (saaplan-image-preproc saaplan)
				 saaplan fits-working-list)))

    ;; make the weight images if desired
    (when (saaplan-image-weighter saaplan)
      (saaplan-log-format saaplan "SHIFT-AND-ADD: Making weight images for ~A" fits-working-list)
      (run-weight-generation (saaplan-image-weighter saaplan) saaplan fits-working-list
			     :badpix-function-list badpix-func-list))

    (saaplan-log-format saaplan "SHIFT-AND-ADD: Creating shifted head files")
    (loop 
       with first-fits = (car fits-working-list)
       for fits in fits-working-list 
       for headerfile = (create-shifted-head-file-relative-fits
			 first-fits fits saaplan
			 :header-suffix "saahead"
			 :where "main-stacker")
       do
	  (push fits shifted-fits-list)
	  (push headerfile headerfile-list))
     
     (setf shifted-fits-list (nreverse shifted-fits-list))

     (if (zerop (length shifted-fits-list ))
	 (saaplan-log-format
	  saaplan "SHIFT-AND-ADD: No good files to swarp. Exiting")

	 (progn
	   (saaplan-log-format 
	    saaplan 
	    "SHIFT-AND-ADD: Running SWARP on ~A ~%   files~%  ~A~%   to make ~A"
	    (length shifted-fits-list)
	    (mapcar 'file-io:file-minus-dir shifted-fits-list)
	    imageout-base)
	   (terapix:run-swarp  
	    shifted-fits-list 
	    final-stack-file  
	    :verbose-type (saaplan-verbose-type saaplan)
	    :output nil
	    :weight-type (if (saaplan-image-weighter saaplan)
			     "MAP_WEIGHT"
			     "NONE")
	    :weight-suffix ".weight.fits"
	    :combine-type (saaplan-swarp-combine-type saaplan)
	    :resampling-type (saaplan-swarp-resampling-type saaplan)
	    :copy-keywords (append (if (saaplan-mjd-keyword saaplan)
				       (list (saaplan-mjd-keyword saaplan)))
				   (instrument-id:get-critical-headers-for-fits
				    (first shifted-fits-list)))
	    :header-suffix "saahead")

	   (when (saaplan-cleanup saaplan)
	     (dolist (hfile headerfile-list)
	       (delete-file hfile)))

	   (setf (stack-result-output-stack sresult)
		 (concatenate 'string final-stack-file ".fits"))

	   (reverse-stack-result sresult) ;; put in correct order
	   
	   (return ;; prog* needs return!
	     (values T sresult)) ;; return STACK-RESULT structure as final value
	   ))))




  
		


    
	
       



(defun images-too-far-p (fits-a fits-b saaplan)
  (flet ((get-image-center (fits)
	   (cf:with-open-fits-file (fits ff)
	     (let* ((n-ext (instrument-id:get-image-extension-for-onechip-fits fits))
		    (wcs   (cf:read-wcs ff :extension n-ext))
		    (naxis1 (cf:read-fits-header ff "NAXIS1" :extension n-ext))
		    (naxis2 (cf:read-fits-header ff "NAXIS2" :extension n-ext)))
	       (wcs:wcs-convert-pix-xy-to-ra-dec wcs (* 0.5d0 naxis1) (* 0.5d0 naxis2))))))
    (multiple-value-bind (ra-a dec-a)
	(get-image-center fits-a)
      (multiple-value-bind (ra-b dec-b)
	  (get-image-center fits-b)
	(let ((dist (astro-coords:sky-angle ra-a dec-a ra-b dec-b :units :arcsec)))
	  (values
	   (>= dist (saaplan-max-sky-spread saaplan))
	   dist))))))
	    
    
						 

;; should this fits file's image extension be extracted, rather than a symlink?
;; YES, if it's not a one-extension trivial fits
(defun %extract-if-fits-should-be-extracted (fits-file output-fits-file saaplan)
  (cf:with-open-fits-file (fits-file ff)
    (let* ((inst (instrument-id:identify-instrument ff))
	   (im-next  (instrument-id:get-image-extension-for-onechip-fits ff :onechip inst))
	   (nexts  (cf:fits-file-num-hdus ff))
	   (headers (instrument-id:get-critical-headers-for-instrument inst fits-file)))
      (cond ((> nexts 2) ;; 2 extensions - need to turn it into one
	     ;; check if not extracting onto itself
	     (when (and (probe-file output-fits-file)
			(equalp (truename fits-file)
				(truename output-fits-file)))
	       (saaplan-log-format
		saaplan
		"SHIFT-AND-ADD: ERROR - %extract-if-fits-should-be-extracted  - cannot destructively extract original file ~A to same path."
		(truename fits-file))
	       (error "%extract-if-fits-should-be-extracted  - cannot destructively extract original file ~A to same path."
		      (truename fits-file)))
	     ;;
	     (cf:extract-fits-extension fits-file output-fits-file
					:extension im-next 
					:overwrite t
					:morekeys (+ 10 (length headers))
					:preserve-primary-extension nil)
	     (cf:with-open-fits-file (output-fits-file ffout :mode :io)
	       (loop for hdr in headers
		     do (multiple-value-bind (value comment hret)
			    (cf:read-fits-header ff hdr :extension 1)
			  (when (not hret) ;; not in primary, so try image ext
			    (multiple-value-setq (value comment hret)
			      (cf:read-fits-header ff hdr :extension im-next)))
			  (when hret
			    (cf:write-fits-header ffout hdr value :comment comment)))))
	     ;; return the output file if we extracted
	     output-fits-file)
	    ;; otherwise, it does not need to be extracted
	    (t
	     nil)))))
			

;; symlink the file 
(defun %symlink-input-fits-file (fits fits-link saaplan)
  (cond
    ;; symlink exists, or is the same as the input, so we just use it
    ((or  (probe-file fits-link)
	  (equalp fits fits-link)))
    ;; else we symlink and add if successful
    (t
     (multiple-value-bind (val err)
	 (ignore-errors (osicat-posix:symlink fits fits-link))
       (if (not val)
	   (saaplan-log-format
	    saaplan
	    "SHIFT-AND-ADD: ERROR - Could not symlink original ~A to link ~A - ~A"
	    fits fits-link err)
	   (error "Fatal error - could not  Could not symlink original ~A to link ~A - ~A"
		  fits fits-link err)
	   ))))
  fits-link)




;; link files into processing directory - but if they are multi-ext, strip out image extension
(defun symlink-or-copy-files-into-output-directory (saaplan fits-working-list
					    &key
					      ;; the subdir in saaplan-output-dir that holds inputs
					      (input-subdir "shift-and-add-input-files"))
  (let ((outdir-full (file-io:full-namestring/no-symlink-expand
		      (saaplan-output-directory saaplan))))
    (saaplan-log-format saaplan "SHIFT-AND-ADD: Linking files into ~A if not there already."
			outdir-full)
    (loop with outlist = nil
	  with badpix-func-outlist = nil
	  for fits in fits-working-list
	  ;; get badpix function NOW
	  for badpix-function = (or (instrument-id:get-badpix-function-for-fits fits)
				    ;; a dummy function that generates permissive weight files
				    (lambda (iy ix) (declare (ignore iy ix)) 0))
	  for fits-base = (file-io:file-minus-dir fits)
	  for fits-link-or-copy = (concatenate 'string outdir-full
				       "/"
				       (string-trim "/" input-subdir)
				       "/" fits-base)
	  do
	     (ensure-directories-exist fits-link-or-copy)
	     (when (not (%extract-if-fits-should-be-extracted  fits fits-link-or-copy saaplan))
	       (%symlink-input-fits-file fits fits-link-or-copy saaplan))
	     (push fits-link-or-copy outlist)
	     (push badpix-function badpix-func-outlist)
	     ;;
	  finally
	     (return (values (reverse outlist)
			     (reverse badpix-func-outlist))))))

						     
	       
#+nil 
(defun symlink-files-into-output-directory (saaplan fits-working-list
					    &key
					      ;; the subdir in saaplan-output-dir that holds inputs
					      (input-subdir "shift-and-add-input-files"))
  (let ((outdir-full (file-io:full-namestring/no-symlink-expand
		      (saaplan-output-directory saaplan))))
    (saaplan-log-format saaplan "SHIFT-AND-ADD: Linking files into ~A if not there already."
			outdir-full)
    (loop with outlist = nil
	  for fits in fits-working-list
	  for fits-base = (file-io:file-minus-dir fits)
	  for fits-link = (concatenate 'string outdir-full
				       "/"
				       (string-trim "/" input-subdir)
				       "/" fits-base)
	  do
	     (ensure-directories-exist fits-link)
	     (cond
	       ;; symlink exists, or is the same as the input, so we just use it
	       ((or  (probe-file fits-link)
		     (equalp fits fits-link))
		(push fits-link outlist))
	       ;; else we symlink and add if successful
	       (t
		(multiple-value-bind (val err)
		    (ignore-errors (osicat-posix:symlink fits fits-link))
		  (if (not val)
		      (saaplan-log-format
		       saaplan
		       "SHIFT-AND-ADD: ERROR - Could not symlink original ~A to link ~A - ~A"
		       fits fits-link err)
		      ;; else file is OK
		      (push fits-link outlist)))))
	       finally
	       (return (reverse outlist)))))
