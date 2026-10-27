

(in-package instrument-id)
 
  
(defun precess-ra-dec-to-j2000  (equinox ra dec)
  (if (= equinox 2000d0)
      (values ra dec)
      (precess:precess-decimal-deg-radec
       ra dec
       (astro-time:mjd-to-jd (astro-time:decimal-year-to-mjd equinox))
       2451512.5d0)))


;; safely test if a header contains a string; case insensitive
(defun header-contains (header substring)
  (search substring header :test 'equalp))


(defun parse-ccdsum (ccdbin-string &key (return :values))
  "Parse CCDSUM=' 1 2 ' into (VALUES 1 2) or (VECTOR 1 2),
returning NIL on failure."
  (when (stringp ccdbin-string)
    (let ((pair (string-utils:split-string ccdbin-string " ")))
      (when (and (= (length pair) 2)
		 (every 'digit-char-p (first pair))
		 (every 'digit-char-p (second pair)))
	(let ((b1 (parse-integer (first pair)))
	      (b2 (parse-integer (second pair))))
	  
	(cond ((eq return :values)
	       (values b1 b2))
	      ((eq return :vector)
	       (vector b1 b2))))))))
	       
	
(defun number-of-extensions (fits-file)
  (cond ((typep fits-file 'cf:fits-file)
	 (cf:fits-file-num-hdus fits-file))
	(t
	 (cf:with-open-fits-file (fits-file ff)
	   (cf:fits-file-num-hdus ff)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %is-image-reduced-using-common-methods (fits-file)
  (let ((history-headers nil)
	(comment-headers nil)
	(imtype nil)
	(ff nil))
    (labels ((search-in-comment-headers (substring)
	       (setf comment-headers
		     (or comment-headers
			 (mapcar
			  'second
			  (cf:read-fits-header-list ff :extension 1 :keyword-wildcard "COMMENT"))))
	       (loop for header in comment-headers
		     when (search substring header)
		       do (return t)))
	     ;;
	     (search-in-history-headers (substring)
	       (setf history-headers
		     (or history-headers
			 (mapcar
			  'second
			  (cf:read-fits-header-list ff :extension 1 :keyword-wildcard "HISTORY"))))
	       (loop for header in history-headers
		     when (search substring header)
		       do (return t)))
	     ;;
	     (get-imtype ()
	       (setf imtype
		     (or imtype (get-object-type-for-fits ff))))
	     ;;
	     (reduced-with-iraf? ()
	       (let ((ccdproc  (cf:read-fits-header ff "CCDPROC" :extension 1))
		     (irafname (cf:read-fits-header ff "IRAFNAME" :extension 1))
		     (iraf-min (or (cf:read-fits-header ff "IRAF_MIN" :extension 1)
				   (cf:read-fits-header ff "IRAF-MIN" :extension 1))) 
		     (iraf-max (or (cf:read-fits-header ff "IRAF_MAX" :extension 1)
				   (cf:read-fits-header ff "IRAF-MAX" :extension 1)))
		     (iraf-bpx (or (cf:read-fits-header ff "IRAF_BPX" :extension 1)
				   (cf:read-fits-header ff "IRAF-BPX" :extension 1)))
		     (iraf-type (or (cf:read-fits-header ff "IRAFTYPE" :extension 1)
				    (cf:read-fits-header ff "IRAF_TYPE" :extension 1)
				    (cf:read-fits-header ff "IRAF-TYPE" :extension 1))))
		 ;; ccdproc, if present, has to have “CCD processing
		 ;; done” and at least one other IRAF-* header must be
		 ;; present
		 (if (and (or (not ccdproc) ;; sometimes absent
			      (search "CCD processing done" ccdproc))
			  (or irafname iraf-min iraf-max iraf-bpx iraf-type))
		     :iraf)))
	     ;;
	     (reduced-with-imred? () ;; our imred does header-by-header marking
	       (cond ((cf:read-fits-header ff "IMRED.CAL") ;; it's an IMRED calibration field, which counts as reduced
		      :imred-ccdproc)
		     (t
		      (loop
			with imtype = (get-imtype)
			for iext from 1 to  (cf:fits-file-num-hdus ff)
			do (cf:move-to-extension ff iext)
			   (cond ((and (eq imtype :flat)
				       (cf:read-fits-header ff "IMRED.DEBIAS"))
				  (return :imred-ccdproc))
				 ((and (eq imtype :object)
				       (cf:read-fits-header ff "IMRED.FLATTEN"))
				  (return :imred-ccdproc)))))))
	     ;;
	     (reduced-with-elixir? ()
	       (let ((imred-dt (cf:read-fits-header ff "IMRED_DT" :extension 1)))
		 (and imred-dt
		      (search "Elixir" imred-dt)
		      :elixir)))
	     ;;
	     (reduced-with-midas? ()  ;; this one is basically a guess
	       (if (and
		    (equalp (cf:read-fits-header ff "ORIGIN" :extension 1) "ESO-MIDAS")
		    (equalp (cf:read-fits-header ff "MIDASFTP" :extension 1) "IMAGE"))
		   :eso-midas))
	     ;;
	     (reduced-with-theli? () ;; the WHT pipeline
	       (let ((imtype (get-imtype))
		     (is-theli (search-in-history-headers "THELI Pipeline"))
		     (is-bias-subtracted
		       (search-in-history-headers  "image has been bias subtracted"))
		     (is-flattened
		       (search-in-history-headers  "image has been flat-fielded"))
		     (is-trimmed
		       (search-in-history-headers   "image has been trimmed")))
		 (when is-theli
		   (when (or
			  (and (eq imtype :obect) is-flattened)
			  (and (eq imtype :flat) is-bias-subtracted)
			  (and (eq imtype :bias) is-trimmed))
		     :theli-pipeline)))))			  

      (cf:maybe-with-open-fits-file (fits-file %ff :mode :input)
	(setf ff %ff)
	(or (reduced-with-iraf?)
	    (reduced-with-imred?)
	    (reduced-with-elixir?)
	    (reduced-with-midas?)
	    (reduced-with-theli?)
	    
	    )))))
	  
	  
	  
	  
	  
    
