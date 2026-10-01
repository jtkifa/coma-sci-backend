
;; combine images to make a fringe frame

(in-package imred)


(defun %fringe-nuke-outlier-pix (imsec reduction-plan) 
  (declare (type cf:image-section imsec)
	   (type reduction-plan reduction-plan)
	   (optimize speed))
  (when (reduction-plan-fringe-prefilter-input-images reduction-plan)
    (let* ((im (cf:image-section-data imsec))
	   (filtered-im (imutils:copy-image im))
	   (r1 (reduction-plan-fringe-filter-r1 reduction-plan))
	   (r2 (reduction-plan-fringe-filter-r2 reduction-plan))
	   (max-dev (reduction-plan-fringe-filter-max-dev reduction-plan)))
      (declare (type imutils:image im filtered-im)
	       (type single-float r2 r2 max-dev))
      ;; get rid of NaN and Inf and replace with 1.0 (im is normalized)
      (loop for i below (array-total-size filtered-im)
	    for x of-type single-float = (row-major-aref filtered-im i)
	    when (float-utils:single-float-nan-or-infinity-p x)
	      do (setf (row-major-aref filtered-im i) 1.0)) ;; median=1

      ;; create ring-median backd
      (setf filtered-im (imutils:median-ring-filter-image
			 filtered-im :r1 r1 :r2 r2))
      ;; and set anything that deviates from background by max-dev to NaN (which is ignored
      ;; when fringe subtracting)
      (loop with nan = float-utils:*single-float-nan*
	    for i below (array-total-size im)
	    for x of-type single-float = (row-major-aref im i)
	    for xf of-type single-float = (row-major-aref filtered-im i)
	    when (> (abs (- x xf)) max-dev)
	      do (setf (row-major-aref im i) nan)))))
		  
	  
    


;; FIXME - would be better to spread the images out a bit through the night?

;; return those image sections whose median pixel value is
;; within min-counts and max-counts, and rescale the images to have a
;; value of 1.
(defun %fringecombine-imsec-filter-function 
    (imsec-generator reduction-plan median-nsample)
  (let* ((min-counts (reduction-plan-min-fringe-counts reduction-plan))
	 (max-counts (reduction-plan-max-fringe-counts reduction-plan))
	 (nfringe-max (reduction-plan-max-fringe-frames reduction-plan))
	 (logger (reduction-plan-logger reduction-plan))
	 (ext-stuff-list
	   (loop for i from 0
		 for imsec = (funcall imsec-generator i)
		 until (not imsec)
		 for ext = (cf:fits-file-current-hdu-num
			    (cf:image-section-orig-fits-file imsec))
		 for fits = (file-io:file-minus-dir
			     (cf:fits-file-filename
			      (cf:image-section-fits-file imsec)))
		 for median = (multiple-value-bind (med err)
				  (ignore-errors
				   (randomly-sampled-image-median
				    (cf:image-section-data imsec)
				    median-nsample))
				(cond
				  ((not med)
				   (logger:writelog
				    logger
				    (format
				     nil
				     "ERROR: in computing median of input fringe ~A:~A - ~A"
				     fits ext err))
				   nil) ;; return median=nil so it isn't collected
				  (t
				   med)))
		 ;;
		 when median
		   collect (list i median fits ext))))

    (let* ((min-frames (reduction-plan-min-fringe-frames reduction-plan))
	   (accepted-imsecs
	     (loop with ngood = 0
		   ;; sort by randomization field
		   for ext-stuff in ext-stuff-list
		   for i = (first ext-stuff) and med = (second ext-stuff)
		   and fits = (file-io:file-minus-dir (third ext-stuff))
		   and ext = (1- (fourth ext-stuff))
		   for counts-hi = (> med max-counts)
		   for counts-lo = (< med min-counts)
		   until (= ngood nfringe-max)
		   do (when counts-hi
			(logger:writelog
			 logger
			 (format nil "ERROR: Input fringe ~A:~A rejected: counts ~A>~A"
				 fits ext  med max-counts)))
		      (when counts-lo
			(logger:writelog
			 logger
			 (format nil "Input fringe ~A:~A rejected: counts ~A<~A"
				 fits ext med min-counts)))
		      ;;
		   when (not (or counts-lo counts-hi))
		     do (incf ngood)
			(logger:writelog
			 logger
			 (format nil "Input fringe ~A:~A accepted: counts = ~A"
				 fits ext med))
		     and
		       ;; scale the image to a median of 1, and return
		       collect  (let* ((imsec (funcall imsec-generator i))
				       (im (cf:image-section-data imsec)))
				  (imutils:im-scale im (/ 1.0 med) :image-out im)
				  (%fringe-nuke-outlier-pix imsec reduction-plan)
				  imsec))))
      ;; Check minimum frame requirement - return NIL to skip this extension
      (when (< (length accepted-imsecs) min-frames)
	(logger:writelog logger
			 (format nil "Skipping fringe: only ~A valid inputs, need at least ~A for star removal"
				 (length accepted-imsecs) min-frames))
	(return-from %fringecombine-imsec-filter-function nil))
      accepted-imsecs)))
    


 
 
(defun fringecombine (fits-list fits-out
		      &key
			(reduction-plan *default-reduction-plan*)
			(stack-type :median)
			(template-fits nil)
			(if-exists :error)
			(median-nsample 30000))
  "Median combine fringe fields, removing those with a median
that is not in range [MIN-COUNTS, MAX-COUNTS]."

  (declare (type list fits-list)
	   (type (or string pathname) fits-out)
	   (type (unsigned-byte 28) median-nsample))
  
  (let ((minval
	  (float (reduction-plan-min-final-fringe-value reduction-plan) 1.0))
	(maxval
	  (float (reduction-plan-max-final-fringe-value reduction-plan) 1.0)))

    ;;
    ;; fix the final fringe to be within the range specified
    ;; and remove the median value of the fringe field

    ;; FIXME - what about null values in inputs?  can't use them
    ;; to compute median
    
    (flet ((final-array-function (im) 
	     (declare (type (simple-array single-float (* *)) im))
	     (loop
	       with med of-type single-float
		 = (randomly-sampled-image-median im median-nsample)
	       for i of-type fixnum below (array-total-size im)
	       for x of-type single-float = (- (row-major-aref im i)
					       med)
	       if (or (float-nan-p x)
		      (< x minval) (> x maxval))
		 ;; set weird values of fringe frame to zero
		 do (setf (row-major-aref im i) 0.0)
	       else do
		 (setf (row-major-aref im i) x)))	     
	   ;;
	   (imsec-filter-function (imsec-list ext-desc)
	     (declare  (ignore ext-desc)) ;; FIXME - should use the STATSEC in extdesc
	     (%fringecombine-imsec-filter-function 
	      imsec-list 
	      reduction-plan
	      median-nsample)))

      
      ;; stack-images returns NIL if all extensions were skipped
      (when (stack-images fits-list fits-out
			  :reduction-plan reduction-plan
			  :template-fits template-fits
			  :stack-type stack-type
			  :count-header "IMRED.NFRINGE"
			  :if-exists if-exists
			  :final-array-function #'final-array-function
			  :imsec-filter-function #'imsec-filter-function)
	(cf:write-fits-header fits-out "IMRED.CAL" T
			      :comment "This is an IMRED calib file")))))



