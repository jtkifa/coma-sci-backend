

(in-package imred)


(defun %build-bias (reduction-plan fits-list bias-fits)

  (imred-log-note
   (format nil "ZEROCOMBINE to make ~A : ~D files - ~{~A ~}"
	   bias-fits
	   (length fits-list) fits-list))

  (on-error-delete-file ;; ensure a bad bias is deleted
      bias-fits
    (cond ((not (or (reduction-plan-trim reduction-plan)
		    (reduction-plan-overscan-subtract reduction-plan)))
	     (zerocombine fits-list bias-fits 
			  :reduction-plan reduction-plan))
	    (t ;; trim and/or overscan-sub
	     (let ((fits-list-trimmed
		     (mapcar (lambda (fits)
			       (modify-fits-name 
				fits 
				:suffix "t" 
				:new-dir (reduction-plan-target-dir reduction-plan)))
			     fits-list)))
	       (loop 
		 for fits in fits-list
		 for fits-trimmed in fits-list-trimmed
		 do
		    (when (not (probe-file fits-trimmed))
		      (trim/os-image fits fits-trimmed reduction-plan)))
	       ;;
	       (zerocombine 
		fits-list-trimmed bias-fits
		:reduction-plan reduction-plan)
	       (when (reduction-plan-delete-intermediate-files reduction-plan)
		 (dolist (fits-trimmed fits-list-trimmed)
		   (delete-file fits-trimmed)))))))
  t) ;; return T on non-error


(defun %build-flat (reduction-plan fits-list bias-fits flat-fits flat-type)
  (on-error-delete-file ;; ensure that a bad flat is deleted
   flat-fits
   (let* ((suffix (if (reduction-plan-trim reduction-plan) "tb" "b"))
	  (fits-list-tb ;; trimmed and/or debiased
	    (mapcar (lambda (fits)
		      (modify-fits-name 
		       fits 
		       :suffix suffix 
		       :new-dir (reduction-plan-target-dir reduction-plan)))
		    fits-list)))
     (loop 
       for fits in fits-list
       for fits-tb in fits-list-tb
       do
	  (when (not (probe-file fits-tb))
	    (ccdproc fits fits-tb
		     ;; if we normalize gain, then the ADU limits on
		     ;; flats in REDUCTION-PLAN won't make sense.
		     :never-normalize-gain t 
		     :reduction-plan reduction-plan
		     :bias bias-fits :flat nil)))
     ;;
     ;; If we are making flats from the object images, clean them up
     ;; first to get rid of bad counts before processing, and cull to
     ;; get correct number (ie, don't combine too many images, just a
     ;; subset of bright ones).  This assumes that the flats were already cleaned
     ;; up using %clean-up-object-flat-list inside function make-fileset-from-fits-list
     (let ((fits-list-tb/clean
	     (if (eq flat-type :object)
		 (%clean-up-object-flat-list reduction-plan fits-list-tb)
		 fits-list-tb)))

       ;; if these are OBJECT flats, then mark those used to construct
       ;; the flat with OBJFLAT header
       (when (eq flat-type :object)       
	 (dolist (fits fits-list-tb/clean)
	   (when (not (eq (instrument-id:get-object-type-for-fits fits) :flat))
	     (cf:write-fits-header 
	      fits "OBJFLAT" t :comment "This object image used to make sky flat"))))
       
	   (imred-log-note
	     (format nil "FLATCOMBINE using flats of type ~A to make ~A : ~D files - ~{~A ~}"
		     (if (eq flat-type :flat) "FLAT"  "OBJECT (darksky)")
	      flat-fits
	      (length fits-list-tb/clean) fits-list-tb/clean))
	   
       (flatcombine fits-list-tb/clean flat-fits
		    :flat-type flat-type
		    :reduction-plan reduction-plan))

		  
     ;;
     (when (reduction-plan-delete-intermediate-files reduction-plan)
       (dolist (fits-tb fits-list-tb)
	 (delete-file fits-tb)))))
  t) ;; return T on non-error

(defun %build-fringe (reduction-plan fits-list fringe-fits)
  (if (not fits-list)
      (imred-log-warn (format nil "Can't build fringe ~A - no files~%" fringe-fits))
      (progn
	(imred-log-note
	 (format nil "Building Fringe ~A using  ~%~%~A~%~%" fringe-fits
		 fits-list))
	(fringecombine fits-list fringe-fits :reduction-plan reduction-plan)))
  t) ;; return T on non-error




;; clean up old calibs because we never want to use them as inputs    
(defun %remove-old-calibs-from-fits-list (fits-list)
  (loop
    for fits-file in fits-list
    when (not (cf:read-fits-header fits-file "IMRED.CAL"))
      collect fits-file))


(defun process-fits-list (fits-list reduction-plan)
  "Fully process a list of fits files according to reduction-plan - output in is
the REDUCTION-PLAN-TARGET-DIR"
 
   (ensure-directories-exist
    (format nil "~A/#ignore#" (reduction-plan-target-dir reduction-plan)))

  (let* ((fits-list-no-cal (%remove-old-calibs-from-fits-list fits-list))
	 (outdir (reduction-plan-target-dir reduction-plan))
	 ;; copy files to preprocced versions if there is a preproc function
	 (fits-list-preproc
	   (when (reduction-plan-input-fits-patch-function reduction-plan)
	     (loop for fits in fits-list-no-cal
		   when (probe-file fits)
		     collect (let ((ppfits (format nil "~A/~Ap.fits"
						   outdir
						   (file-io:file-minus-dir
						    (file-io:file-basename fits)))))
			       (file-io:copy-file fits ppfits :overwrite t)
			       (cf:write-fits-comment
				ppfits
				"IMRED - XXXXp suffix means PREPROCESSED")
			       (funcall
				(reduction-plan-input-fits-patch-function reduction-plan)
				ppfits reduction-plan)
			       ppfits))))
	 ;;
	 (fits-list-used (or fits-list-preproc fits-list-no-cal))
	 (fileset (make-fileset-from-fits-list fits-list-used reduction-plan))
	 (calib-dir ;; dir for bias+flat default to outdir
	   (string-right-trim "/" (or (reduction-plan-calib-dir reduction-plan) outdir )))
	 (bias-fits (format
		     nil "~A/~A~A.fits" 
		     calib-dir
		     (reduction-plan-zero-basename reduction-plan)
		     (funcall (reduction-plan-bias-group-func reduction-plan)
			      (first fits-list-used))))
	 ;; in-out-obj-fits-list is a hash of 
	 ;; KEY=fits-in  VAL=object-fits-out for purpose of
	 ;; fringe correction, so we can get the processed object
	 ;; for each raw input object.
	 (in-out-obj-fits-hash (make-hash-table :test 'equal)))


    (ensure-directories-exist
     (format nil "~A/#ignore#" outdir))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; BIAS
    (when (and (probe-file bias-fits)
	       (reduction-plan-make-zero reduction-plan)
	       (reduction-plan-recreate-zero reduction-plan))
      (backup-file-by-renaming bias-fits))

    (when (and (not (probe-file bias-fits))
	       (reduction-plan-make-zero reduction-plan))
      (when (not (fileset-bias-list fileset))
	(imred-log-error 
	 "Trying to make a combined bias (REDUCTION-PLAN-MAKE-ZERO
 is set) but no found no bias images among input files."
	 :die t)) ;; hopeless without a bias
      (multiple-value-bind (success err)
	  (maybe-ignore-errors
	   (%build-bias reduction-plan (fileset-bias-list fileset) bias-fits))
	(when (not success)
	  (imred-log-error (format nil "%BUILD-BIAS failed with error ~A" err)
			   :die t))))
       
    (if (probe-file bias-fits)
	(push (file-io:file-minus-dir bias-fits) (fileset-output-bias-list fileset))
	(imred-log-error
	 (format nil "BIAS file ~A not found after %BUILD-BIAS. Cannot continue." bias-fits)
	 :die t)) ;; we must ALWAYS die when there is no flat
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; FLATS
    (when (reduction-plan-make-flats reduction-plan)
      (loop 
	for flatset in (fileset-flatsets fileset)
	for flat-fits = (format nil  "~A/~A.fits" calib-dir
				(flatset-name-filesafe (flatset-name flatset)))
	do
	   
	   (when (and (probe-file flat-fits)
		      (reduction-plan-recreate-flats reduction-plan))
	     (backup-file-by-renaming flat-fits))
	   
	   (when (not (probe-file flat-fits))
	     (multiple-value-bind (success err)
		 (ignore-errors
		  (%build-flat reduction-plan
			       (flatset-flat-list flatset)
			       bias-fits flat-fits
			       (flatset-flat-type flatset)))
	       (when (not success)
		 (imred-log-error
		  (format nil "%BUILD-FLAT for FLATSET ~A failed with error: '~A'" 
			  (flatset-name flatset) err)
		  :die nil))))
	   
	   (if (probe-file flat-fits)
	       (push (file-io:file-minus-dir flat-fits) (fileset-output-flat-list fileset))
	       (imred-log-error
		(format nil "FLAT file ~A not found after %BUILD-FLAT." flat-fits)
		;; we don't need to die on a single bad flat - other filters might process
		:die nil)))) ;; FIXME - we need a 'best effort' option in reduction plan?
			       
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; OBJECTS
    (when (reduction-plan-ccdproc-objects reduction-plan)
      (loop 
	for fits in (fileset-object-list fileset)
	for group = (funcall (reduction-plan-flat-group-func reduction-plan)
			     fits)
	for flat-name = (format nil "~A~A" 
				(reduction-plan-flat-basename reduction-plan)
				(flatset-name-filesafe (or group "")))
	for flat-fits = (format nil  "~A/~A.fits" calib-dir flat-name)
	for fits-out = (modify-fits-name 
			fits 
			:new-dir  (reduction-plan-target-dir reduction-plan)
			:suffix (reduction-plan-reduced-suffix reduction-plan))
	do
	   (cond 
	     ;; reduction already exists and we don't want to redo
	     ((and (probe-file fits-out)
		   (not (reduction-plan-recreate-objects reduction-plan)))
	      ;; in this case, just normalize the gain if requested; if gain=1
	      ;; in images, then this does nothing
	      (when (reduction-plan-gain-normalize reduction-plan)
		(normalize-gain-of-reduced-image fits-out
						 :reduction-plan reduction-plan)))
	     ;; do the reduction of fits
	     (t
	      (when (probe-file fits-out)
		(backup-file-by-renaming fits-out))

	      
	      (imred-log-note
	       (format nil "Reducing ~A --> ~A" fits fits-out))
	      
	      (cond
		;; fail if no bias
		((not (probe-file bias-fits))
		 (imred-log-error
		  (format nil "Failed to reduce ~A - missing bias ~A" fits bias-fits)
		  :die nil))
		;; fail if no flat
		((not (probe-file flat-fits))
		 (imred-log-error
		  (format nil "Failed to reduce ~A - missing flat ~A" fits flat-fits)
		  :die nil))
		;; else reduce it
		(t
		 (progn
		   (push fits-out (fileset-output-fits-list fileset))
		   (push fits-out (fileset-output-reduced-obj-list fileset))
		   (setf (gethash fits in-out-obj-fits-hash) fits-out) ;; for fringe cor
		   (multiple-value-bind (success err)
		       (maybe-ignore-errors
			(progn (ccdproc fits fits-out 
					:reduction-plan reduction-plan
					:bias bias-fits
					:flat flat-fits)
			       t))
		     (when (not success)
		       (imred-log-error
			(format nil "CCDPROC of fits ~A failed: ~A" fits err)
			:die nil))))

	      
		 ;; patch up the final fits file
		 (when 
		     (and 
		      (reduction-plan-output-fits-patch-function reduction-plan)
		      (probe-file fits-out))
		   (funcall  
		    (reduction-plan-output-fits-patch-function reduction-plan)
		    fits-out reduction-plan)) ))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; FRINGE CORRECTION
    (when (and (reduction-plan-make-fringes reduction-plan)
	       (fileset-fringesets fileset))
      ;; first make the fringes
      (loop for fringeset in (fileset-fringesets fileset)
	    for fringe-fits = (format nil "~A/~A.fits" calib-dir
				      (fringeset-name-filesafe
				       (fringeset-name fringeset)))
	    do (when (and (probe-file fringe-fits)
			  (reduction-plan-recreate-fringes reduction-plan))
		 (backup-file-by-renaming fringe-fits))
	       ;;
	       (when (not (probe-file fringe-fits))
		 (multiple-value-bind (success err)
		     (maybe-ignore-errors
		      (%build-fringe
		       reduction-plan
		       ;; remove any fits that weren't successfully ccdproced,
		       ;; so give a NIL in the mapping hash table
		       (remove nil
			       (mapcar
				(lambda (orig-fits)
				  (gethash orig-fits in-out-obj-fits-hash))
			       (fringeset-fringe-obj-list fringeset)))
		       fringe-fits))
		   ;;
		   (when (not success)
		     (imred-log-error (format nil "Failed to make fringe ~A with error ~A"
					      fringe-fits
					      err)
				      :die nil)))
	       
		 (when (probe-file fringe-fits)
		   (push fringe-fits (fileset-output-fringecorrected-obj-list fileset))))
	       ;;
	    finally
	       (loop for orig-fits in (fringeset-fringe-obj-list fringeset)
		     for proc-fits = (gethash orig-fits in-out-obj-fits-hash)
		     for defringed-fits
		       = (when proc-fits ;; can fail to proc
			   (modify-fits-name
			    proc-fits 
			    :new-dir nil ;; keep output dir
			    :suffix (reduction-plan-defringed-suffix reduction-plan)))
		     when proc-fits ;; can fail to proc
		     do
			(push defringed-fits (fileset-output-fits-list fileset))
			;; if defringed exists, don't redo unless asked
			(cond 
			  ;; reduction already exists and we don't want to redo
			  ((and (probe-file defringed-fits)
				(not (reduction-plan-recreate-fringes
				      reduction-plan)))
			   t) ;; do nothing
			  ;; else do defringing
			  (t  
			   (when (probe-file defringed-fits)
			     (backup-file-by-renaming defringed-fits))
			    (imred-log-note
			     (format nil "Fringe subtracting ~A --> ~A"
				     proc-fits defringed-fits))
			    (multiple-value-bind (success err)
				(maybe-ignore-errors
				 (progn
				   (fringecor proc-fits defringed-fits fringe-fits
					      :reduction-plan reduction-plan
					      :if-exists :supersede)
				   t))
			      (when (not success)
				(imred-log-error (format
						  nil
						  "FRINGECOR failed for ~A + ~A with error ~A"
						  proc-fits fringe-fits err)
						 :die nil)))
						 
			    ;; now apply patch function to output
			    (when 
				(and 
				 (reduction-plan-output-fits-patch-function
				  reduction-plan)
				 (probe-file defringed-fits))
			      (funcall  
			       (reduction-plan-output-fits-patch-function
				reduction-plan)
			       defringed-fits reduction-plan)))))))

    ;; delete preproc list
    (when (and fits-list-preproc
	       (reduction-plan-delete-intermediate-files reduction-plan))
      (dolist (fits fits-list-preproc)
	(delete-file fits)))
    
    (setf  (fileset-output-fits-list fileset)
	   (nreverse  (fileset-output-fits-list fileset)))
    #+sbcl (sb-ext:gc :full t)
    fileset
    ))

      
