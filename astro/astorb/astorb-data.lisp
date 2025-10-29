#|

Routines for reading and compiling astorb

|#


(in-package astorb)

;; format only if *astorb-quiet* is false
(defun %aformat (&rest args)
  (when (not *astorb-quiet*)
    (apply 'format args)))

;; return a list of astorb.dat.[gz] files, with the latest one first.
;; they must in *astorb-data-dir* and of the form astorb.dat.MJD or astorb.dat.MJD.gz
(defun get-astorb-file-list ()
  (bordeaux-threads:with-recursive-lock-held (*astorb-lock*)
    (mapcar
     ;; add the basenames back because we remove them
     (lambda (basename)
       (format nil "~A/~A" *astorb-data-dir* basename))
     (remove-if ;; remove any .fasl files
      (lambda (file) (search ".fasl" file))
      (sort  (mapcar 'file-io:file-minus-dir ;; look at only the basename, to be safe
		     (mapcar 'namestring
			     (append ;; look for both .dat.NNN  and .dat.NNN.gz where NNN=MJD
				     (directory (format nil "~A/astorb.dat.*" *astorb-data-dir*))
				     (directory (format nil "~A/astorb.dat.*.gz" *astorb-data-dir*)))))
	     'string>)))))


;; describe current mjd file and return MJD of it
(defun describe-astorb-file (filename &key quiet (verbose-stream *astorb-info-output-stream*))
  (let* ((basename (file-io:file-minus-dir filename))
	 (i-mjd-start (or (position-if 'digit-char-p basename)
			  (error "NO MJD found in astorb file ~A~%" filename)))
	 (mjd-elements (numio:parse-float basename
					  :start  i-mjd-start
					  :junk-allowed t))
	 (date (astro-time:mjd-to-ut-string mjd-elements)))
    (when (and verbose-stream (not quiet))
      (%aformat verbose-stream
	      "ASTORB: Using ASTORB-FILE ~A ~% with MJD=~A and UT ~A~%"
	      basename
	      mjd-elements date))
    mjd-elements))







;; make string lowercase and remove non-alpha chars
(defun %scrub-string (string)
  (declare (type  (simple-array character (*)) string)
	   (optimize speed))
  (let* ((nspc (loop
		  with n of-type (unsigned-byte 28) = 0
		  for c of-type base-char across string
		  when (not (and (typep c 'base-char) (alphanumericp c)))
		  do  (incf n) 
		  finally (return n)))
	 (outstring (make-array (- (length string) nspc) :element-type 'base-char)))
    (loop 
       with i of-type (unsigned-byte 28) = 0
       for c across string
       when  (and (typep c 'base-char) (alphanumericp c))
       do 
	 (setf (aref outstring i) (char-downcase (the base-char c)))
	 (incf i))
    outstring))


;; count lines even if file is a gzip file
(defun astorb-count-lines (filename)
  (if (not (string-utils:string-ends-with filename ".gz"))
	(file-io:file-count-lines filename)
      (gzip-stream:with-open-gzip-file (s filename)
	(loop with nret = 0
	      for b = (read-byte s nil nil) ;; buffered reading is no faster
	      until (not b)
	      when (eql b #.(char-code #\lf))
		do (incf nret)
	      finally (return nret)))))


;; open a file either in gzip mode or other mode, in byte mode
(defmacro astorb-with-open-file ((stream-var filename) &body body)
  `(let ((%file ,filename))
     (flet ((%astorb-with-open-file-body (,stream-var)
	      ,@body))
       (if (string-utils:string-ends-with %file ".gz")
	   (gzip-stream:with-open-gzip-file (%stream %file)
	     (%astorb-with-open-file-body %stream))
	   (with-open-file (%stream %file :element-type '(unsigned-byte 8))
	     (%astorb-with-open-file-body %stream))))))


;; read an astorb line from byte stream
(defun astorb-read-line (stream &optional buffer)
  (declare (type stream stream)
	   (type (or null string) buffer))
  (loop with buffer = (or buffer (make-string 2048))
	with nmax = (length buffer)
	for b = (read-byte stream nil nil)
	for i of-type fixnum from 0
	when (not b)
	  do (return nil)
	do (let ((c (code-char b)))
	     (cond ((char= c #\lf)
		    (return buffer)) ;; leave junk on end - doesn't matter
		   ((char= c #\cr)
		    nil)
		   (t
		    (when (= i nmax) (error "Line too long in astorb stream."))
		    (setf (aref buffer i) c))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read an astorb text or text.gz file into an astorb structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; astorb is a real PITA to read because not all fields are present
(defun read-astorb (astorb-file)
  #+sbcl (sb-ext:gc :full t)
  (bordeaux-threads:with-recursive-lock-held (*astorb-lock*)
    ;; the astorb file name has to have the MJD of the elements in it
    (let* ((mjd-elements (describe-astorb-file astorb-file :quiet t)) ;; returns MJD
	   (n (astorb-count-lines astorb-file))
	   (astorb (build-astorb n))
	   (tmpline (make-array 20 :element-type 'character))
	   (buffer (make-string 512))
	   (where nil) ;; what field we're parsing for debug
	   (ncurline 0)) ;; number of current line
      (declare (ignorable where))
      ;; just a bookkeeping measure - not used
      (setf (astorb-epoch-of-elements astorb) mjd-elements)
      (setf (astorb-astorb-file  astorb) astorb-file)
      (astorb-with-open-file (s astorb-file)
	(labels ((subline (line n1 n2) ;; subseq using scratch array
		   (declare (type (simple-array character (*)) line)
			    (type (unsigned-byte 16) n1 n2)
			    (optimize speed))
		   (fill tmpline #\space)
		   (loop 
		     for i of-type fixnum from n1 below n2 
		     for j of-type fixnum from 0
		     do (setf (aref tmpline j) (aref line i)))
		   tmpline)
		 ;; for grab-string use real subseq to avoid clobbering
		 (grab-string (line n1 n2)
		   (declare (type (simple-array character (*)) line)
			    (type (unsigned-byte 16) n1 n2)
			    (optimize speed))			
		   (string-trim #(#\tab #\space) (subseq line n1 n2)))
		 (grab-int (line n1 n2 &optional (default 0))
		   (declare (type (simple-array character (*)) line)
			    (type (unsigned-byte 16) n1 n2)
			    (optimize speed))
		   (or (ignore-errors (parse-integer (subline line n1 n2)))
		       default))
		 (grab-dbl (line n1 n2 &optional (default 0d0))
		   (declare (type (simple-array character (*)) line)
			    (type (unsigned-byte 16) n1 n2)
			    (optimize speed))
		   (or (ignore-errors (numio:parse-float (subline line n1 n2)))
		       default))
		 (grab-float (line n1 n2 &optional (default 0e0))
		   (declare (type (simple-array character (*)) line)
			    (type (unsigned-byte 16) n1 n2)
			    (optimize speed))
		   (or (ignore-errors 
			(float (numio:parse-float (subline line n1 n2)) 1.0))
		       default)))
	  
	  (multiple-value-bind (val err)
	      (ignore-errors
	       (loop
		 for i below n
		 for line of-type simple-string = (or (astorb-read-line s buffer)
						      (error "premature end of astorb-file"))
		 do
		    (setf ncurline (1+ i))
		    (setf where "I")
		    (setf (aref (astorb-astnum astorb) i) (grab-int line 0 6))
		    (setf where "NAME")
		    (setf (aref (astorb-name astorb) i) (grab-string line 7 25))
		    (setf where "SNAME")
		    (setf (aref (astorb-sname astorb) i) 
			  (if (aref (astorb-name astorb) i)
			      (%scrub-string  (aref (astorb-name astorb) i))))
		    (setf where "HMAG")
		    (setf (aref (astorb-hmag astorb) i) (grab-float line 42 47))
		    (setf where "G")
		    (setf (aref (astorb-g astorb) i) (grab-float line 49 53))
		    (setf where "IRAS-KM")
		    (setf (aref (astorb-iras-km astorb) i) (grab-float line 55 64))
		    (setf where "IRAS-CLASS")
		    (setf (aref (astorb-iras-class astorb) i) (grab-float line 65 58))
		    (setf where "CODE1")
		    (setf (aref (astorb-code1 astorb) i) (grab-int line 73 75))
		    (setf where "CODE2")
		    (setf (aref (astorb-code2 astorb) i) (grab-int line 77 79))
		    (setf where "CODE3")
		    (setf (aref (astorb-code3 astorb) i) (grab-int line 81 83))
		    (setf where "CODE4")
		    (setf (aref (astorb-code4 astorb) i) (grab-int line 87 87))
		    (setf where "CODE5")
		    (setf (aref (astorb-code5 astorb) i) (grab-int line 89 91))
		    (setf where "CODE6")
		    (setf (aref (astorb-code6 astorb) i) (grab-int line 93 95))
		    (setf where "ORBARC")
		    (setf (aref (astorb-orbarc astorb) i) (grab-float line 95 100))
		    (setf where "NOBS")
		    (setf (aref (astorb-nobs astorb) i) (grab-int line 101 105))
		    (setf where "EPOCH-OSC")
		    (setf (aref (astorb-epoch-osc astorb) i) (grab-int line 106 114))
		    (setf where "MEAN-ANOM")
		    (setf (aref (astorb-mean-anomaly astorb) i) (grab-dbl line 115 125))
		    (setf where "ARG-PERI")
		    (setf (aref (astorb-arg-peri astorb) i) (grab-dbl line 126 136))
		    (setf where "ANOIDE")
		    (setf (aref (astorb-anode astorb) i) (grab-dbl line 137 147))
		    (setf where "ORBINC")
		    (setf (aref (astorb-orbinc astorb) i) (grab-dbl line 148 157))
		    (setf where "ECC")
		    (setf (aref (astorb-ecc astorb) i) (grab-dbl line 158 168))
		    (setf where "A")
		    (setf (aref (astorb-a astorb) i) (grab-dbl line 170 181))
		    (setf where "ORBIT-DATE")
		    (setf (aref (astorb-orbit-date astorb) i) (grab-int line 182 190))
		 finally (return t))) ;; for ignore-errors
	    (when  (not val)
	      (error "ERROR ~A at line ~A: line is ~A" err ncurline buffer)))
	  ;; force a full GC to compact the memory again
	  #+sbcl (sb-ext:gc :full t)
	  astorb)))))
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fasl saving facility for much faster loading of astorb database
;; we compile the astorb into a lisp file that turns into a fasl file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-fasl-base-from-astorb-file (astorb-filename)
  (format nil "~A-~A" astorb-filename (uiop/os:implementation-identifier)))
(defun make-fasl-filename-from-astorb-file (astorb-filename)
  (format nil "~A.fasl" (make-fasl-base-from-astorb-file astorb-filename)))



;; turn an astorb struct into a fasl file by creating a lisp file and
;; compiling it and optionally loading it
(defun make-astorb-fasl (astorb  &key (delete-lisp-file t)
				 (load-fasl nil))
  (let ((fasl-base (make-fasl-base-from-astorb-file (astorb-astorb-file astorb))))
    (readable-arrays:with-readable-arrays
      (let ((lisp-file (format nil "~A.lisp" fasl-base))
	    (fasl-file (format nil "~A.fasl" fasl-base)))
	(unwind-protect
	     (progn
	       (with-open-file (s lisp-file :direction :output :if-exists :supersede)
		 (write '(in-package astorb) :stream s)
		 (terpri s)
		 (write `(setf *the-astorb*
			       ,(astorb-writer astorb))
			:stream s))
	       (let ((*standard-output* *astorb-info-output-stream*))
		 (compile-file lisp-file :output-file fasl-file
					 :verbose (not *astorb-quiet*))))
	(when delete-lisp-file
	  (delete-file lisp-file)))
	;;
	(when load-fasl
	  (with-astorb-lock
	    (load fasl-file)))
	;;
	#+sbcl (sb-ext:gc :full t)
	fasl-file))))

(defun load-fasl-for-astorb-file (astorb-filename)
  (let ((fasl-filename (make-fasl-filename-from-astorb-file astorb-filename)))
    (when (probe-file fasl-filename)
      (load fasl-filename))))


(defun update-to-latest-astorb (&key
				  (verbose-stream *astorb-info-output-stream*)
				  (set-the-astorb t)
				  (make-fasl t)
				  (load-fasl t)
				  (ntries 3))
  "Refresh the system to the newest astorb from Lowell, and by default compile
a new fasl file and load it."
  (let ((astorb-file
	  (retrieve-newest-astorb-file/iterate
	   :ntries ntries
	   :verbose-stream (and (not *astorb-quiet*) verbose-stream))))
    (let ((astorb (read-astorb astorb-file)))
      (when set-the-astorb
	(with-astorb-lock
	  (setf *the-astorb* astorb)
	  (setf *astorb-file* astorb-file)))
	(when make-fasl
	  ;; if *load-fasl* is true then set-the-astorb will be effectively
	  ;; true
	  (%aformat
	   verbose-stream
	   "Making compiled fasl file for astorb database.  This may take over 5 minutes.")
	  (let ((fasl-file (make-astorb-fasl astorb :load-fasl load-fasl)))
	    (%aformat verbose-stream "Made fasl file ~A~%" fasl-file)
	    fasl-file)))))
	      


(defun read-astorb-on-initialization (&key (verbose-stream *astorb-info-output-stream*))
    (let* ((astorb-file-list (get-astorb-file-list))
	   (astorb-file (first astorb-file-list)))
      (cond (astorb-file
	     (describe-astorb-file astorb-file :verbose-stream verbose-stream)
	     ;; try to load fasl
	     (cond ((load-fasl-for-astorb-file astorb-file)
		    (when (not *the-astorb*)
		      (error "ASTORB: ERROR - variable *THE-ASTORB* is not set after reading astorb fasl."))
		    (%aformat verbose-stream "ASTORB: Read prebuilt fasl file for astorb database.~%"))
		   ;; failed to load fasl so build it
		   (t
		    (%aformat verbose-stream "ASTORB: No prebuilt astorb fasl file found. Building it now.~%")
		    (%aformat verbose-stream "ASTORB:   This may take 5 to 10 minutes, but will be fast~%")
		    (%aformat verbose-stream "ASTORB:   on subsequent loads.~%")
		    (let ((astorb (read-astorb astorb-file)))
		      (with-astorb-lock
			(setf *astorb-file-list* astorb-file-list)
			(setf *astorb-file* astorb-file)
			(setf *the-astorb* astorb)
			(make-astorb-fasl astorb :load-fasl t)))
		    (when (not *the-astorb*)
		      (error "ASTORB: ERROR - variable *THE-ASTORB* is not set after reading astorb fasl.")))))
	    ;; case that we didn't find an astorb file
	    (*read-astorb-on-load*
	     (when (not (update-to-latest-astorb :verbose-stream verbose-stream))
	       (error "ASTORB: failed to download astorb database from web."))
	     (when (not *the-astorb*)
	       (error "ASTORB: ERROR - variable *THE-ASTORB* is not set downloading database and building astorb fasl.")))
	    ;; case that there's no astorb file
	    (t
	     (%aformat verbose-stream "ASTORB: Warning - no astorb database present and not auto-downloading.")))))
	     
		 
	   
	   
  

(eval-when (:load-toplevel)
  (when (not *the-astorb*)
    (read-astorb-on-initialization)))
    
  




