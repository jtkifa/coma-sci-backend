

(in-package imred)


;; convert an extdesc type (a FITS land type) into a 
;; a type of array that is read
(defun convert-fits-type-to-read-type (fits-type)
"Given a FITS-TYPE like :BYTE, :SHORT, :USHORT, :LONG, :FLOAT, :DOUBLE
return a suitable type to read it into, like :SINGLE-FLOAT, :DOUBLE-FLOAT,
:UNSIGNED-BYTE-8, :UNSIGNED-BYTE-16 :SIGNED-BYTE-16, :SIGNED-BYTE-32"
  (cond ((eq fits-type nil) :single-float) ;; default
	((eq fits-type :float) :single-float)
	((eq fits-type :double) :double-float)
	((eq fits-type :byte)  :unsigned-byte-8) ;; no unsigned8 in fits
	((eq fits-type :short) :signed-byte-16)
	((eq fits-type :ushort) :unsigned-byte-16)
	((eq fits-type :long) :signed-byte-32)
	(t (error "Unknown fits-type ~A in convert-fits-type-to-read-type"
		  fits-type))))

(defun copy-headers (ff-in ff-out &key (exclude nil))
  (let ((hlist (cf:read-fits-header-list ff-in)))
    (loop 
       for h in hlist
       for name = (first h) and val = (second h) and comment = (third h)
       when (and (not (member name exclude :test 'equalp))
		 (not (equalp name "END")))
       do (cf:write-fits-header ff-out name val :comment comment))))



	 

(defun randomly-sampled-image-median (im nsamples &key ix0 ix1 iy0 iy1)  
  (declare (type imutils:image im)
	   (type fixnum nsamples)
	   (type (or null fixnum) ix0 ix1 iy0 iy1)
	   (optimize speed))
  ;; create a sub-array with the good pixels
  (let* ((ix0 (or ix0 0))
	 (ix1 (or ix1 (1- (array-dimension im 1))))
	 (iy0 (or iy0 0))
	 (iy1 (or iy1 (1- (array-dimension im 0))))
	 (npix (* (- ix1 ix0 -1) (- iy1 iy0 -1)))
	 (npix-good 0)
	 (v (make-array npix :element-type 'single-float)))
    (declare (type fixnum npix npix-good ix0 ix1 iy0 iy1)
	     (type (simple-array single-float (*)) v))

    (loop for kx of-type fixnum from ix0 to ix1
	  do (loop for ky of-type fixnum from iy0 to iy1
			for x of-type single-float = (aref im ky kx)
		   when (not (or (float-utils:single-float-nan-or-infinity-p x)
				 (= x +invalid-pixel-value+)))
		     do (setf (aref v npix-good) x)
			(incf npix-good)))
    (when (zerop npix-good)
      (error "Zero good (non-NaN, non-Inf, non- +invalid-pixel-value+) pixels in randomly-sampled-image-median"))
    

    (cond
      ;; if the number of samples is big (larger than 1/2 npix-good)
      ;; then just take the brute force median because sampling gives no advantage
      ((>= nsamples (ash npix-good -1))
       (fastmedian:fast-single-float-1d-array-median v npix-good))
      ;; else do random sampling by shuffling array into the bottom NSAMPLES elements
      ;; and then taking the median of this bottom NSAMPLES
      (t
       (loop
	 ;; always use same random seed to get deterministic behavior
	 with vseed = (make-array 1 :element-type '(unsigned-byte 32)
				    :initial-contents '(1))
	 for i of-type fixnum below nsamples
	 for iswap = (+ i (random:pmrann (- npix-good i 1) vseed))
	 do (rotatef (aref v i) (aref v iswap)))
       (fastmedian:fast-single-float-1d-array-median v nsamples)))))
	     
      

 
;; convert a file to a unique-file string, but return NIL if no such file,
;; unless create is set, in which case make a file.  The purpose is to get the
;; full namestring of a file.  When CREATE is set, it makes the file.  If
;; DELETE is set, it then deletes the created file - the purpose of
;; CREATE and DELETE together is to get a successful path for output - yeah, a bit
;; irksome.
(defun fullfile (file &key (create nil) (delete nil))
  (let ((fullname
	  (or 
	   (and (probe-file file)
		(namestring (truename file)))
	   (and create
		(ignore-errors
		 (with-open-file (ff file :direction :output :if-does-not-exist :create)
		   t))
		(namestring (truename file))))))
    (when (and delete fullname) (delete-file fullname))
    fullname))
	

(defun backup-file-by-renaming (file)
  "Back a file up a file by renaming it to file.BAK.n where n is the lowest
free integer"
  (when (not (probe-file file))
    (error "~A does not exist." file))
  (loop
     for i from 1 
     for newfile = (format nil "~A.BAK.~D"
			   (namestring (fullfile file)) i)
     when (not (probe-file newfile))
     do
       (rename-file file newfile)
       (return)))



;; change a fits file to have a new directory and a suffix
;; before the final .fits.  For example 
;; "./foo/bar/bunny.fits" to "/this/that/bunnyX.fits"
(defun modify-fits-name (fits &key new-dir suffix)
  (cond ((and (not new-dir) (not suffix))
	 fits)
	(t
	 (let* ((fullname (namestring fits))
		(new-dir 
		 (if ;; ensure trailing / -- YUCK
		  (and new-dir  
		       (plusp (length new-dir))
		       (not (char=
			     (aref new-dir (1- (length new-dir)))
			     #\/)))
		  (format nil "~A/" new-dir)
		  new-dir))
		(old-dir (file-io:dir-of-file fullname))
		(fullbase (file-io:file-minus-dir fullname))
		(ndot (position #\. fullbase :from-end t))
		(oldsuffix (if ndot (subseq fullbase ndot) ""))
		(prefix (subseq fullbase 0 ndot)))
	   (format nil "~A~A~A~A" 
		   (or new-dir old-dir)
		   prefix (or suffix "") oldsuffix)))))
  

		       

;; parse notation [a:b,c:d] and return #(a b c d)
(defun parse-sec (string &key (name ""))
  (let* (str pcolon1 pcomma pcolon2 nx1 nx2 ny1 ny2)
    (flet ((oopsie ()
	     (error "Bad image section ~A ~A" name string)))
    (setf str (string-trim  "[]" string))
    (setf pcolon1 (position #\:  str))
    (when (not pcolon1) (oopsie))
    (setf pcomma (position #\, str  :start (+ 2 pcolon1)))
    (when (not pcomma) (oopsie))
    (setf pcolon2 (position #\: str :start  (+ 2 pcomma)))
    (when (not pcolon2) (oopsie))

    (setf nx1 
	  (ignore-errors (parse-integer str :start 0 :junk-allowed t)))
    (setf nx2 
	  (ignore-errors (parse-integer str 
					:start (1+ pcolon1)
					:junk-allowed t)))
    (setf ny1
	  (ignore-errors (parse-integer str 
					:start (1+ pcomma)
					:junk-allowed t)))
    (setf ny2
	  (ignore-errors (parse-integer str :start 
					(1+ pcolon2)
					:junk-allowed t)))

    (when (not (and nx1 nx2 ny1 ny2))
      (oopsie))
    (vector nx1 nx2 ny1 ny2))))

(defun sec-to-string (sec)
  (format 
   nil "[~A:~A,~A:~A]" 
   (aref sec 0)(aref sec 1)(aref sec 2)(aref sec 3)))
   
  
(defparameter *default-logger* nil)

;; get the reduction plan logger if an rplan is defined otherwise return *default-logger*
(defmacro %get-reduction-plan-logger (rplan-var)
  `(if ,rplan-var
       (reduction-plan-logger ,rplan-var)
       *default-logger*))

;; log assuming existence of reduction-plan variable
(defmacro imred-log (log-string
		     &key
		       (rplan-var 'reduction-plan)  ;; name of reduction plan var
		       (log-type :log)
		       (log-level 5))
  `(logger:writelog (%get-reduction-plan-logger ,rplan-var)
		    ,log-string
		    :log-type ,log-type
		    :log-level ,log-level))




(defmacro imred-log-note (log-string
			  &key
			    (rplan-var 'reduction-plan))  ;; name of reduction plan var
  `(logger:writelog (%get-reduction-plan-logger ,rplan-var)
		    (concatenate 'string "NOTE: " ,log-string)
		    :log-type :log :log-level 1))


(defmacro imred-log-warn (log-string
			  &key
			    (rplan-var 'reduction-plan))  ;; name of reduction plan var
  `(logger:writelog (%get-reduction-plan-logger ,rplan-var)
		    (concatenate 'string "WARNING: " ,log-string)
		    :log-type :error :log-level 3))

(defmacro imred-log-error
    (log-string
     &key
       (rplan-var 'reduction-plan)  ;; name of reduction plan var
       (die nil)) 
  `(progn (logger:writelog (%get-reduction-plan-logger ,rplan-var)
			   (concatenate 'string "ERROR: " ,log-string)
			   :log-type :error :log-level 5)
	  ,@(when die ;; so that NIL doesn't produce removed code compiler note
	      `((when ,die (error ,log-string))))))



;; macro that renames a file if the BODY gives an error, to avoid
;; leaving corrupted files around 
(defmacro on-error-delete-file (file &body body)
  `(let ((%good-result nil)
	 (%body-value niL))
     (unwind-protect
	  (progn
	    (setf %body-value (progn ,@body))
	    ;; if we got there, there's no error
	    (setf %good-result t)
	    %body-value)
       (progn ;; the unwind-protected form
	 (when (not %good-result)
	   (ignore-errors (delete-file ,file)))))))
	   


(defun compute-image-median-in-statsec (im median-nsample statsec)
  (declare (type imutils:image im)
	   (type (unsigned-byte 30) median-nsample)
	   (type vector statsec))
  (let* ((nx (1- (array-dimension im 1)))
	 (ny (1- (array-dimension im 0)))
	 (ix0 (min nx (max 0 (1- (aref statsec 0)))))
	 (ix1 (min nx (max 0 (1- (aref statsec 1)))))
	 (iy0 (min ny (max 0 (1- (aref statsec 2)))))
	 (iy1 (min ny (max 0 (1- (aref statsec 3))))))
    (when (not (and (<= ix0 ix1)
		    (<= iy0 iy1)))
      (error "Range error in image sampling - trying to sample image of dimensions ~A
with ix0=~A ix1=~A iy0=~A iy1=~A" (array-dimensions im) ix0 ix1 iy0 iy1))

    (randomly-sampled-image-median
     im  median-nsample
     :ix0 ix0 :ix1 ix1
     :iy0 iy0 :iy1 iy1)))


;; run BODY with a temporary file based on FILENAME and EXTRA-PREFIX,
;; bound to FILE-VAR.  Then rename the temporary file to FILE. The
;; purpose is to avoid making FILENAME until we know that the full
;; process has succeded, so that we're not left with a broken output
;; file to confuse future runs.
(defmacro with-temporary-output-file ((filename file-var &key extra-suffix) &body body)
  `(let* ((,file-var (file-io:make-tempfile-name 
		      :prefix (format nil "~A~A" ,filename (or ,extra-suffix ""))))
	  (%return-value% (progn ,@body))) ;; expects FILE-VAR to be bound
     (when (probe-file ,filename) (delete-file ,filename))
     (rename-file ,file-var ,filename)
     %return-value%))
  
  

(defstruct compatible-image-set
  instrument
  chip-id ;; either chip-id for one-chip 
  extension-signature  ;; a vector of #(#(naxis1a naxis2a) ...) for a mosaic
  file-list)

;; generate a vector of #(#(naxis1a naxis2a) ...) for a mosaic
(defun %generate-extension-signature (fits-file)
  (cf:with-open-fits-file (fits-file ff)
    (loop with next = (cf:fits-file-num-hdus ff)
	  with vout = (make-array next)
	  for i from 1 to next
	  do (cf:move-to-extension ff i)
	     (setf (aref vout (1- i))
		   (cf:fits-file-current-image-size ff))
	  finally (return vout))))
	     
	   
   

;; a helper function to divide an image list into compatible blocks
(defun divide-image-list-into-compatible-image-sets (fits-list &key directory)
  "Given a list of fits files FITS-LIST, return a list of structure
COMPATIBLE-IMAGE-SET that breaks the list down into images that can be
processed together, by matching instrument type and CHIP-ID (for
onechip images)

Returns (VALUES COMPATIBLE-IMAGE-SET-LIST REJECTED-FITS-LIST REJECTION-REASONS-LIST)"
  (loop with h = (make-hash-table :test 'equalp)
	with rejected-fits-list = nil
	with dir = (if directory (concatenate 'string (string-right-trim "/" directory)))
	with rejection-reasons = nil
	for fits-orig in fits-list
	;; append directory if specified
	for fits = (if (not directory)
		       fits-orig
		       (concatenate 'string dir "/" fits-orig))
	for inst = (ignore-errors
		    (type-of (instrument-id:identify-instrument fits)))
	for chip-id = (ignore-errors
		       (cond ((typep inst 'instrument-id:multichip)
			      nil)
			     ((typep inst 'instrument-id:onechip)
			      (or (instrument-id:get-chip-id-for-fits fits) 
				  1)))) ;; just default to chip 1 if not defined
	for extension-signature = (ignore-errors (%generate-extension-signature fits))
	do
	   (cond ((not inst)
		  (push fits-orig rejected-fits-list)
		  (push "INSTRUMENT-NOT-IDENTIFIED" rejection-reasons))
		 ((and (not chip-id) (typep inst 'instrument-id:multichip))
		  (push fits-orig rejected-fits-list)
		  (push "CHIP-ID-NOT-COMPUTABLE" rejection-reasons))
		 ((not extension-signature)
		  (push fits-orig rejected-fits-list)
		  (push "EXTENSION-SIGNATURE-NOT-COMPUTABLE" rejection-reasons))
		 (t
		  (let* ((key (list inst chip-id extension-signature))
			 (comset
			   (or (gethash key h)
			       (setf (gethash key h)
				     (make-compatible-image-set
				      :instrument inst
				      :extension-signature extension-signature
				      :chip-id chip-id)))))
		    (push fits-orig (compatible-image-set-file-list comset)))))
	finally
	   (return
	     (values
	      (loop for comset being the hash-value of h
		    do (setf  (compatible-image-set-file-list comset)
			      (nreverse  (compatible-image-set-file-list comset)))
		    collect comset)
	      rejected-fits-list
	      rejection-reasons))))
	   
	   



(defun %get-trimsec-at-extension (fits reduction-plan extension)
  (let ((trimsec (reduction-plan-trimsec reduction-plan)))
    (cond ((stringp trimsec)
	   (cf:parse-image-section-string
	    (or (cf:read-fits-header fits trimsec)
		(error "Could not find TRIMSEC header '~A' in fits file ~A"
		       trimsec fits))))
	  ((vectorp trimsec)
	   trimsec)
	  (t
	   (or
	    (instrument-id:get-trimsec-for-fits fits :extension extension)
	    (error "Could not get TRIMSEC for fits ~A" fits))))))
  

(defun %get-overscans-at-extension (fits reduction-plan extension)
  (let ((overscans (reduction-plan-overscans reduction-plan)))
    (cond ((and overscans (listp overscans))
	   (when (not (every
		       (lambda (os)
			 (and (vectorp os)
			      (= (length os) 4)
			      (every (lambda (n)
				       (and (integerp n)
					    (plusp n)))
				     os)))
		       overscans))
	     (error "Overcans ~A are invalid image sections #(xmin xmax ymin ymax)."
		    overscans))
	   overscans)
	  (t ;; this is allowed to return NIL for 'no overscans'
	    (instrument-id:get-overscans-for-fits fits :extension extension)))))



;; turn off ignore-error forms to allow backtraces to halt at
;; error location
(defvar *disable-ignore-errors* t)
(defmacro maybe-ignore-errors (&body body)
  `(flet ((%%ignore-err-func () ,@body))
     (if *disable-ignore-errors*
	 (%%ignore-err-func)
	 (ignore-errors
	  (%%ignore-err-func)))))
