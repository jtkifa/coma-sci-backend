


;; cfitsio interface
;;
;; some quirks:
;;   1) uses Fortran-style indexing, so a [1:100,1:1000] image fits into
;;      a 1000x100 array
;;   2) should rewrite wcs inside image section to use wcslib
;;   3) need to handle NULL keys to allow comments


(defvar *cfitsio-libraries* nil) ;; actually defined in cfitsio-cmucl.lisp too

(in-package cfitsio)
 
(defstruct fits-file
  (filename nil)
  (fptr 0 :type (unsigned-byte #.(* 8 (cffi:foreign-type-size :pointer))))
  (num-hdus 0 :type (unsigned-byte 32))
  (is-open nil) ;; NIL or :input or :io
  (current-hdu-num 0 :type (unsigned-byte 32))
  (current-hdu-name nil)
  (current-hdu-type nil :type
   (member nil :image :ascii-tbl :binary-tbl :unknown))
  ;; if this is an image, these get set
  (current-image-type nil)
  (current-image-ndims 0 :type (unsigned-byte 20))
  (current-image-size nil) ;; a vector of the sizes of the dimensions
  ;; if this is a table, these get set
  (current-table-nrows nil :type (or null (unsigned-byte 28)))
  (current-table-ncols nil :type (or null (unsigned-byte 28)))
  ;; names of current table columns
  (current-table-columns nil)
  (current-table-types nil)
  (current-table-widths nil)  ;; length of each entry in bytes
  (current-table-repeats nil) ;; the number of repeats, except for string, where
                              ;; it is the total number of chars
  (current-table-nitems/entry nil) ;; the true number of entries, repeats for
                                   ;; all but strings, in which case REPEATS/WIDTH
  (current-table-variable-length-col nil) 
  )

;; FOR MEANING OF REPEAT AND WIDTH -  5.7.2 of cfitsio manual

;; The 'repeat' parameter returns the vector repeat count on the
;; binary table TFORMn keyword value. (ASCII table columns always have
;; repeat = 1). The 'width' parameter returns the width in bytes of a
;; single column element (e.g., a '10D' binary table column will have
;; width = 8, an ASCII table 'F12.2' column will have width = 12, and
;; a binary table'60A' character string column will have width = 60);
;; Note that CFITSIO supports the local convention for specifying
;; arrays of fixed length strings within a binary table character
;; column using the syntax TFORM = 'rAw' where 'r' is the total number
;; of characters (= the width of the column) and 'w' is the width of a
;; unit string within the column. Thus if the column has TFORM =
;; '60A12' then this means that each row of the table contains 5
;; 12-character substrings within the 60-character field, and thus in
;; this case this routine will return typecode = TSTRING, repeat = 60,
;; and width = 12. (The TDIMn keyword may also be used to specify the
;; unit string length; The pair of keywords TFORMn = '60A' and TDIMn =
;; '(12,5)' would have the same effect as TFORMn = '60A12'). The
;; number of substrings in any binary table character string field can
;; be calculated by (repeat/width). A null pointer may be given for
;; any of the output parameters that are not needed.

(defstruct (image-section (:print-function image-section-printer))
  ;; the fits-file that was used to open 
  (fits-file nil :type (or null fits-file))
  ;; a copy of the fits-file used to open, to preserve original state
  (orig-fits-file nil :type (or null fits-file))
  (type nil)
  (ndim 0 :type (unsigned-byte 32))
  (size nil) ;; dimensions, eg #(1024 2048)
  (parent-size nil) ;; dimensions of parent
  (whole nil) ;; is this WHOLE of orignal image?
  (fp #()) (lp #()) ;; vectors representing first and last pixels in parent
  (wcs nil) ;; wcs system if available
  (data nil) ;; the actual data as a multidim array - with Fortran style
             ;; indices, so that a [1:100,1:1000] image has a 1000x100 data
             ;; array
  (headers nil) ;; optional list of '(("key" "val") ... read in at image read time
  )



(defun size-string (fp lp) ;; make "[1:20,2:30,1:1024]" from fp,lp
  (loop with s of-type simple-string = "["
	for first = t then nil
	for n1 across fp
	for n2 across lp
	do
	(setf s (concatenate 'string s
			     (format nil (if first "~d:~d" ",~d:~d")
					       n1 n2)))
	finally (return (concatenate 'string s "]"))))
	

(defun image-section-printer (is stream n)
  (declare (ignore n)
           (type image-section is))
  (format stream
	  "#<cfitsio:image-section img=\"~A\" ~A ~A>"
	  (when (image-section-fits-file is)
	      (fits-file-filename (image-section-fits-file is)))
	  (image-section-type is)
	  (size-string (image-section-fp is) (image-section-lp is)))) 




(defmacro check-if-ff-closed (ff where)
  `(if (not (fits-file-is-open ,ff))
       (error "trying to operate on closed fits-file in ~A" ,where)))


(defunL read-fits-file-hdu (ff &key (throw-error t))
  "set the hdu fields of a fits file, based current value,
using fields EXTNAME and HDUNAME"
  (declare (type fits-file ff)
	   (optimize speed))
  (check-if-ff-closed ff "read-fits-file-hdu")
  (let* ((fptr (fits-file-fptr ff))
	 (hdunum (get-current-extension-for-fits-pointer fptr))
	 (extension (read-header-value-for-pointer
		     fptr "EXTNAME" :string :throw-error throw-error))
	 (hduname (read-header-value-for-pointer
		   fptr "HDUNAME" :string :throw-error throw-error))
	 (hdutype (get-hdu-type-for-fits-pointer
		   fptr :throw-error throw-error))
	 (imtype (when (eq hdutype :image)
		   (get-image-type-for-fits-pointer
		    fptr :throw-error throw-error)))
	 (ndims (when (eq hdutype :image)
		  (get-num-dims-for-fits-pointer
		   fptr :throw-error throw-error)))
	 (imsize (when (and (eq hdutype :image) ndims)
		   (get-image-size-for-fits-pointer
		    fptr ndims :throw-error throw-error)))
	 (nrows (when (or (eq hdutype :ascii-tbl) (eq hdutype :binary-tbl))
		  (get-table-nrows-for-fits-pointer fptr :throw-error throw-error)))
	 (ncols (when (or (eq hdutype :ascii-tbl) (eq hdutype :binary-tbl))
		  (get-table-ncols-for-fits-pointer fptr :throw-error throw-error)))
	 (colnames (when (or (eq hdutype :ascii-tbl) (eq hdutype :binary-tbl))
		     (loop with tmpvec = (make-array ncols)
			   for i below ncols
			   do (setf (aref tmpvec i)
				    (get-table-colname-for-fits-pointer
				     fptr (1+ i) ;; one indexed
				     :throw-error throw-error))
			   finally (return tmpvec))))
	 types widths repeats nitems/entry varlengths)
    ;;
    ;; read info about columns, if this is a table
    (when (or (eq hdutype :ascii-tbl) (eq hdutype :binary-tbl))
      (loop
	 initially
	   (setf types (make-array ncols))
	   (setf widths (make-array ncols))
	   (setf repeats (make-array ncols))
	   (setf nitems/entry (make-array ncols))
	   (setf varlengths (make-array ncols))
	 for i from 1 to ncols
	 for j from 0
	 do
	   (multiple-value-bind (type var repeat width)
	       (get-table-coltype-for-fits-pointer fptr i :throw-error throw-error)
	     (setf (aref types j) type)
	     (setf (aref varlengths j) var)
	     (setf (aref repeats j) repeat)
	     (setf (aref widths j) width)
	     ;; strings are special: repeats is number of chars, not substrings
	     (setf (aref nitems/entry j)
		   (if (eq type :string) (floor repeat width) repeat)))))
    ;;
    (setf (fits-file-current-hdu-num ff) hdunum)
    (setf (fits-file-current-hdu-name ff) (or extension hduname))
    (setf (fits-file-current-hdu-type ff) hdutype)
    ;;
    ;; null out the image/table specific values before setting them
    (setf (fits-file-current-image-type ff) nil)
    (setf (fits-file-current-image-ndims ff) 0)
    (setf (fits-file-current-image-size ff) nil)
    (setf (fits-file-current-table-nrows ff) nil)
    (setf (fits-file-current-table-ncols ff) nil)
    (setf (fits-file-current-table-columns ff) nil)
    (setf (fits-file-current-table-types ff) nil)
    (setf (fits-file-current-table-widths ff) nil)
    (setf (fits-file-current-table-repeats ff) nil)
    (setf (fits-file-current-table-nitems/entry ff) nil)
    (setf (fits-file-current-table-variable-length-col ff) nil)
    
    (when (eq hdutype :image)
      (setf (fits-file-current-image-type ff) imtype)
      (setf (fits-file-current-image-ndims ff) ndims)
      (setf (fits-file-current-image-size ff) imsize))
    ;;
    (when (or (eq hdutype :ascii-tbl) (eq hdutype :binary-tbl))
      (setf (fits-file-current-table-nrows ff) nrows)
      (setf (fits-file-current-table-ncols ff) ncols)
      (setf (fits-file-current-table-columns ff) colnames)
      (setf (fits-file-current-table-types ff) types)
      (setf (fits-file-current-table-repeats ff) repeats)
      (setf (fits-file-current-table-widths ff) widths)
      (setf (fits-file-current-table-nitems/entry ff) nitems/entry)
      (setf (fits-file-current-table-variable-length-col ff) varlengths))
    ff))
      
	

;; try to find filename.gz or filename.fz as specified, if filename
;; does not exist
(defun try-to-find-compressed-file (filename &key (try-fz nil) (try-gz nil))
  (let ((fzfile (when try-fz (concatenate 'string filename ".fz")))
	(gzfile (when try-gz (concatenate 'string filename ".gz")))
	(zfile  (when try-gz (concatenate 'string filename ".Z"))))
    
  (cond ((probe-file filename)
	 filename)
	((and try-fz (probe-file fzfile))
	 fzfile)
	((and try-gz (probe-file gzfile))
	 gzfile)
	((and try-gz (probe-file zfile)) ;; .Z is synonym for .gz
	 zfile))))
			   
  
(defunL open-fits-file (filename &key (mode :input) (throw-error t)
			(try-fz nil) (try-gz nil))
  "Open a fits-file

MODE is either keyword :INPUT or :IO
THROW-ERROR means to throw an error on failure, instead of returning
   (values nil status error-string)
TRY-FZ means to try FILENAME.fz (compressed), if the named file doesn't 
   existe
TRY-GZ means to try FILENAME.gz (as above).
"
  (declare (type (or string pathname) filename )
	   (type (member :input :io) mode)) ;; :io=:output
  (block ret
    ;;
    ;; check for .imh, which can barf with a mem fault that we
    ;; can't reproduce in C
    #+nil
    (when (eql (search ".IMH" (string-upcase filename))
	       (- (length filename) 4))
      (when throw-error
	(error "Cannot safely open .imh file ~A - risks memfault"
	       filename))
      (return-from ret
	(values nil -999 "Cannot safely open .imh file - risks memfault")))
    ;;
    (let* ((ff nil)
	   fptr status err-string
	   (filename-string-raw
	     (if (pathnamep filename) (namestring filename) filename))
	   (filename-string (try-to-find-compressed-file
			     filename-string-raw
			     :try-fz try-fz :try-gz try-gz)))
      
      
      (multiple-value-setq (fptr status err-string)
	(open-fits-file-pointer
	 ;; below, allow any NOT-FOUND error to happen in cfitsio
	 (or filename-string filename-string-raw)
	 :read-only (eq mode :input)
	 :throw-error throw-error))
      (when (not (zerop status))
	(return-from ret (values nil status err-string)))
      ;;
      (setf ff (make-fits-file :filename filename-string
			       :fptr fptr
			       :is-open mode))
      (finalize-fits-file-pointer ff fptr)
      (read-fits-file-hdu ff :throw-error t)
      (setf (fits-file-num-hdus ff)
	    (get-number-of-extensions-for-fits-pointer
	     fptr :throw-error throw-error))
      (values fptr 0 err-string)
      ff)))
			   
(defunL close-fits-file (ff &key (throw-error t))
  "close a fits-file"
  (declare (type fits-file ff))
  (check-if-ff-closed ff "close-fits-file")
  (cancel-finalization ff)
  (setf (fits-file-is-open ff) nil)
  (close-fits-file-pointer (fits-file-fptr ff) :throw-error throw-error))



(defmacro with-open-fits-file
    ((filename ffvar &key (mode :input) (throw-error t)
	       (try-fz nil) (try-gz nil))
   &body body)
  "(WITH-OPEN-FITS-FILE (FILENAME FFVAR &KEY MODE THROW-ERROR) opens
fits file 'filename' in mode 'mode', binds it to ffvar,
executes 'body' and closes the file using an unwind-protect -
returns (values nil status status err-string) if file open fails,
otherwise returns the result of evaulating 'body'

TRY-FZ and TRY-GZ keywords tell routine to try filename.fz or
filename.gz if filename not present, for transparent use of fz files."
  (let ((status-var (gensym "status-"))
	(err-string-var (gensym "err-string-")))
    `(let (,ffvar ,status-var ,err-string-var)
       (unwind-protect
	   (block opener
	     (multiple-value-setq (,ffvar ,status-var ,err-string-var)
	       (open-fits-file ,filename :mode ,mode
					 :throw-error ,throw-error
					 :try-fz ,try-fz :try-gz ,try-gz))
	     (when (not ,ffvar)
	       (return-from opener (values nil ,status-var ,err-string-var)))
	     ,@body)
	 (when (and (fits-file-p ,ffvar)
		    (fits-file-is-open ,ffvar))
	   (close-fits-file ,ffvar :throw-error ,throw-error))))))

(defmacro maybe-with-open-fits-file   ((fits ffvar &key (mode :input) (throw-error t))
				       &body body)
  "(MAYBE-WITH-OPEN-FITS-FILE (FITS FFVAR &KEY MODE THROW-ERROR)   opens FITS
if it is a filename, but if it is a fits file structure merely continues as long
as the MODE is acceptable (:INPUT is OK with already being opened as :IO).

The extension is always reset to the first one with
WITH-FITS-EXTENSION, to ensure similar behavior between fresh open and reuse
of an open FITS-FILE device."
  (let ((fits-var (gensym "fits-"))
	(temporary-ffvar (gensym "ffvar-temp-")))
    `(let ((,fits-var ,fits)
	   ;; create variable ffvar so that (do-maybe-with-open-fits-file-body) sees it
	   (,ffvar nil))
       (flet ((do-maybe-with-open-fits-file-body ()
		(with-fits-extension (,ffvar 1)
		  ,@body)))
	 (cond ((or (typep ,fits-var 'string)
		    (typep ,fits-var 'pathname))
		(with-open-fits-file (,fits-var ,temporary-ffvar :mode ,mode :throw-error ,throw-error)
		  (setf ,ffvar ,temporary-ffvar)
		  (do-maybe-with-open-fits-file-body)))
	       ((typep ,fits-var 'fits-file)
		(cond ((or
			;; if MODE is :INPUT, we accept any previously open mode
			(and (eq ,mode :input)
			     (fits-file-is-open ,fits-var))
			;; if MODE is :IO, then must be open :IO
			(and (eq ,mode :io)
			     (eq (fits-file-is-open ,fits-var) :io)))
		       (setf ,ffvar ,fits-var)
		       (do-maybe-with-open-fits-file-body))
		      (t ;; wrong mode
		       (error "Wrong mode in macro MAYBE-WITH-OPEN-FITS-FILE: requested mode is ~S but existing open mode is ~S"
			      ,mode (fits-file-is-open ,fits-var)))))
	       (t ;; wrong type
		(error "Invalid type ~A for FITS - should be a filename or an open FITS-FILE"
		       (type-of ,fits-var))))))))




(defmacro with-fits-extension ((ff extension) &body body)
  "For a FITS-FILE FF, go go EXTENSION, perform BODY, and go back.
Extension numbers start at 1, being HDU-NUMs.  If EXTENSION is nil,
then do nothing."
  (let ((%current-ext-var (gensym "CURRENT-EXTENSION"))
	(%extvar (gensym "EXTENSION")))
    `(let ((,%current-ext-var (fits-file-current-hdu-num ,ff))
	   (,%extvar ,extension))
       (unwind-protect
	    (progn
	      (when ,%extvar
		;; move to extension, and throw an error if bogus
		(cf:move-to-extension ,ff ,%extvar)
		(when (and (not (symbolp ,%extvar)) ;; like :FIRST-IMAGE
		  	   (not
			    (or (equalp ,%extvar (fits-file-current-hdu-num ,ff))
				(equalp ,%extvar (fits-file-current-hdu-name ,ff)))))
		  (error "Failed to move to extension ~A" ,%extvar)))
		;; and execute body in this extension
		,@body)
	 (when ,%extvar (cf:move-to-extension ,ff ,%current-ext-var))))))
	 
 
(defmacro with-new-fits-file
  ((filename ffvar &key (template nil) (overwrite nil) (throw-error t)
	     (make-primary-headers nil))
     &body body)
  "(with-new-fits-file filename ffvar &key template overwrite
throw-error) creates a new fits file using optional TEMPLATE, binds
it to FFVAR, and closes it using unwind protect after evaluating BODY.
Remember to create SIMPLE=T, BITPIX=Z, NAXIS=X keywords, or add an
image, to make it usable.

If MAKE-PRIMARY-HEADERS keyword is T, then these basic headers for NAXIS=0
are created for a empty primary extension.  This is desirable for most 
cases except when the primary extension is an image with different 
BITPIX and NAXIS."
  (let ((status-var (gensym "status-"))
	(err-string-var (gensym "err-string-")))
    `(let (,ffvar ,status-var ,err-string-var)
      (unwind-protect
	   (block opener
	     (multiple-value-setq (,ffvar ,status-var ,err-string-var)
	       (create-fits-file ,filename :overwrite ,overwrite 
				 :template ,template
				 :throw-error ,throw-error))
	     (when (not ,ffvar)
	       (return-from opener (values nil ,status-var ,err-string-var)))
	     ;;
	     ,(when make-primary-headers ;; expand this part if make-primary-headers not NIL
		`(when ,make-primary-headers
		     (write-fits-header ,ffvar "SIMPLE" t)
		     (write-fits-header ,ffvar "BITPIX" 8)
		     (write-fits-header ,ffvar "NAXIS" 0)))
	     ;;
	     ;; make the fits-file struct sensible for the body - only
	     ;; works if there is something sensible in it
	     ,(when (or make-primary-headers template) ;; epand only if needed
		`(when (or ,make-primary-headers ,template)
		   (read-fits-file-hdu ,ffvar :throw-error ,throw-error)))
	     ;;
	     ,@body)
	(when (and (fits-file-p ,ffvar)
		   (fits-file-is-open ,ffvar))
	  (close-fits-file ,ffvar :throw-error ,throw-error))))))
  



(defun is-float-string (str)
  "return T or NIL if str is a valid representation of a decimal string
FIXME - does not correctly determine overflow, because it looks
only at exponent"
  (declare (type simple-string str)
	   (optimize speed))
  (loop
     ;; with sign of-type (integer -1 1) = 1
     with n-before-dec of-type (unsigned-byte 20) = 0
     with n-after-dec of-type (unsigned-byte 20) = 0
     with have-dec = nil
     with have-exp = nil
     with n-after-exp of-type (unsigned-byte 20) = 0
     with exponent of-type (unsigned-byte 20) = 0
     ;;with exponent-sign of-type (integer -1 1) = 1
     for c of-type base-char across str
     for i of-type (unsigned-byte 20) = 0 then (1+ i)
     do
       (cond ((not have-exp) ;; stuff before exponent
	      (cond ((digit-char-p c)
		     (if have-dec (incf n-after-dec) (incf n-before-dec)))
		    ((member c '(#\e #\E #\d #\D))
		     (setf have-exp t))
		    ((or (char= c #\-) (char= c #\+))
		     (if (> i 0) (return (values nil "misplaced-sign-char")))
		     ;;(setf sign (if (char= c #\-) -1 1))
		     )
		    ((char= c #\.)
		     (when have-dec
		       (return (values nil "repeated-decimal-point"))) 
		     (setf have-dec t))
		    (t ;; invalid char
		     (return (values nil "invalid-char-before-exp")))))
	     (t ;; stuff after exp
	      (cond ((or (char= c #\-) (char= c #\+))
		     (if (> n-after-exp 0)
			 (return (values nil "misplaced sign in exponent")))
		     ;;(setf exponent-sign (if (char= c #\-) -1 1))
		     )
		    ((not (digit-char-p c))
		     (return (values nil "non-digit char after exponent")))
		    (t
		     (setf exponent (+ (* exponent 10)
				   (- (char-int c) #.(char-int #\0))))
		     (if (> exponent 308)
			 (return (values nil "exponent out of range")))
		     (incf n-after-exp)))))
     finally
       (when (and (zerop n-before-dec) (zerop n-after-dec))
     (return (values nil "no mantissa found")))
       (when (and have-exp (zerop n-after-exp))
	 (return (values nil "no digits after exponent found")))
       (return (values t nil))))



(defun is-int-string (s)
  "is this a valid string for representing an integer?"
  (declare (type simple-string s)
	   (optimize speed))
  (loop
   with ndigit of-type (unsigned-byte 28) = 0
   for c of-type character across s
   for i of-type (unsigned-byte 28) from 0
   do
   (if (= i 0) ;; first char can be +/-/digit
       (when (not (or (char= c #\+) (char= c #\-) (digit-char-p c)))
	 (return (values nil "first char not +/-/digit")))
       (when (not (digit-char-p c))
	 (return (values nil "char not a digit"))))
   (when (digit-char-p c) (incf ndigit))
   finally
   (if (zerop ndigit)
       (return (values nil "no digits"))
       (return (values t nil)))))
   
       
;; count headers
(defgeneric get-header-space (ff &key throw-error extension)
  (:documentation "Return (VALUES N-KEYS-EXISTING
N-ROOM-FOR-MORE-KEYS) for a fits file FF. If EXTENSION is set, try
move temporarily to extension (starting at 1) before reading."))

(defmethodL get-header-space ((ff fits-file) &key (throw-error t) extension)
  (check-if-ff-closed ff "get-header-space")
  (with-fits-extension (ff extension)
    (get-header-space-for-pointer (fits-file-fptr ff) 
				  :throw-error throw-error)))


(defmethodL get-header-space ((ff string)  &key (throw-error t) extension)
   (with-open-fits-file 
       (ff fitsfile :mode :input :throw-error throw-error)
     (get-header-space
      fitsfile
      :throw-error throw-error
      :extension extension)))
  

;; figure out the header type of a string
(defun get-header-type (s)
  (declare (type simple-string s))
  (cond ((= (length s) 0) :string)
	;; long integers can cause a problem with data conversion
	((is-int-string s) :integer) 
	((char= (char s 0) #\() :complex)
	((char= (char s 0) #\') :string)
	((member (char-upcase (char s 0)) '(#\T #\F)) :logical)
	((is-float-string s) :double)
	(t :string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric read-fits-headers-as-string (ff &key throw-error extension
					       exclude-comments convert
					       output)
  (:documentation
   "Read a fits header as one long block of text. 

If CONVERT is T (default) use its_convert_hdr2str, else use
fitse_hdr2str.  CONVERT converts headers from tile mode to normal
mode.

OUTPUT can be LISP-STRING, or C-STRING, in which case output is not
deallocated, and must be deallocated with free or FITS_FREE_MEMORY

Returns (VALUES LISP-OR-C-STRING N-HEADERS ERRORCODE ERRORSTRING)"))
 
(defmethodL read-fits-headers-as-string ((ff fits-file)
					    &key (throw-error t) extension
					    (exclude-comments nil) (convert t)
					    (output :lisp-string))
  (check-if-ff-closed ff "read-fits-header")
  (with-fits-extension (ff extension)
    (get-full-headers-as-string-for-pointer (fits-file-fptr ff)
					   :throw-error throw-error
					   :exclude-comments exclude-comments
					   :convert convert
					   :output output)))



(defmethodL read-fits-headers-as-string ((ff string)
					 &key (throw-error t) extension
					 (exclude-comments nil) (convert t)
					 (output :lisp-string))
  (with-open-fits-file 
      (ff fitsfile :mode :input :throw-error throw-error)
    (read-fits-headers-as-string fitsfile
			 :throw-error throw-error
			 :extension extension
			 :exclude-comments exclude-comments
			 :convert convert
			 :output output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric read-fits-header (ff key &key throw-error extension)
  (:documentation
    "Read a fits header, for key of type string or number (position).
decides appropriate return type, and 
returns (values val comment key status err-string)
'keyname' is the name of the key when the key was found, or
NIL when not; it is useful when the value is boolean NIL, or
if key is a number rather than a name.

If EXTENSION is set, try move temporarily to extension (starting at 1)
before reading.")) 

(defmethodL read-fits-header ((ff fits-file) key &key (throw-error t) extension)
  (declare (type fits-file ff) 
	   (type (or string (unsigned-byte 28)) key))
  (check-if-ff-closed ff "read-fits-header")
  (with-fits-extension (ff extension)
    (block ret 
      (let (val-string val comment found status err-string type new-key dummy) 
	;; first get the header as a string, so we can figure 
	;; out its type
	(cond
	  ((stringp key)
	   (multiple-value-setq (val-string comment status err-string) 
	     (get-header-line-value-and-comment-strings-for-pointer 
	      (fits-file-fptr ff) key :throw-error throw-error))
	   (setq new-key key))
	  ((integerp key)
	   (multiple-value-setq (new-key val-string comment status err-string) 
	     (get-header-line-fields-for-keynum-for-pointer 
	      (fits-file-fptr ff) key :throw-error throw-error))
	   ))
	;;
	;; now new-key has true keyname
	;;
	(when (not (zerop status)) ;; header not found 
	  (return-from ret (values nil nil nil status err-string)))
	;;
	(setf type (get-header-type val-string))
	;;
	;; certain special cases don't have values
	(cond ((member new-key '("COMMENT"  "HISTORY") :test 'equalp)
	       (setf val comment
		     found t
		     comment nil
		     status 0))
	      ((equalp new-key "END")
	       (setf val nil 
		     found t
		     comment nil 
		     status 0))
	      ;;
	      ((eq type :integer) ;; special case - sometimes LONGLONG is too small
	       (setf val (parse-integer val-string)) ;; lisp handles big integers
	       ;; read a string into dummy instead of VAL
	       (multiple-value-setq (dummy comment found status err-string)
		 (read-header-value-for-pointer ;; just read it as a string
		  (fits-file-fptr ff) new-key :string :throw-error throw-error)))
	      ;;
	      (t ;; not a comment
	       (multiple-value-setq (val comment found status err-string)
		 (read-header-value-for-pointer
		  (fits-file-fptr ff) new-key type :throw-error throw-error))))
	;;
	(when (or (not (zerop status)) (not found)) ;; not found is superfluous
	  (return-from ret (values nil nil nil status err-string)))
	;; note return of new-key in third field
	(values val comment new-key status err-string)))))
      

(defmethodL read-fits-header ((ff string) key &key (throw-error t) extension)
  (with-open-fits-file 
      (ff fitsfile :mode :input :throw-error throw-error)
    (read-fits-header fitsfile key :throw-error
		      throw-error :extension extension)))
		       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric read-fits-header-list (ff &key keyword-wildcard throw-error
					extension)
  (:documentation
   "Return a list of (keyname value comment) for the current HDU
of a fits file, matching on  :KEYWORD-WILDCARD
returns (values '((key1 val1 comment1)...) success status error-string"))

(defmethodL read-fits-header-list ((ff fits-file) &key
				  (keyword-wildcard "*")
				   (throw-error t)
				   (extension nil))
  (declare (type string keyword-wildcard))
  (with-fits-extension (ff extension)
    (block ret
      ;; rewind to start
      (multiple-value-bind (dum1 dum2 dum3 status err-string)
	  (get-header-line-fields-for-keynum-for-pointer
       (fits-file-fptr ff) 0 :throw-error throw-error)
	(declare (ignorable dum1 dum2 dum3))
      (when (not (zerop status))
	(return-from ret (values nil nil status err-string))))
      ;;
      (loop
	with retlist = nil
	for i of-type (unsigned-byte 28) = 1 then (1+ i)
	do
	   (multiple-value-bind (val comment key status err-string)
	       (read-fits-header ff i :throw-error nil) ;; can't throw error
	     ;; stop when no more keys to read
	     (when (= status +KEY_OUT_BOUNDS+)
	       (return-from ret
		 (values (nreverse retlist) t status err-string)))
	     ;; throw the error we didn't throw above in checking for +KEY_OUT_BOUNDS+
	     ;; except for undefined key
	     (when (not (or (= status +VALUE_UNDEFINED+) ;; can have no value
			    (= status +KEY_NO_EXIST+)))  ;; can have completely blank line
	       (handle-error-in-status status
				       (format nil "READ-FITS-HEADER-LIST - line ~A -" i)
				       throw-error))
	     ;;
	     (when (and key
			(> (length key) 1) ;; make sure key is not just ""
			(not (equalp key "END")) ;; this special one is always present
			(fits-compare-string keyword-wildcard key nil))
	       (push (list key val comment) retlist)))))))

  
(defmethodL read-fits-header-list ((ff string)  &key
				  (keyword-wildcard "*")
				   (throw-error t)
				   (extension nil))
  (with-open-fits-file
   (ff fitsfile :mode :input :throw-error throw-error)
    (read-fits-header-list fitsfile :throw-error throw-error
				    :keyword-wildcard keyword-wildcard
				    :extension extension)))
		       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil ;; old version
(defgeneric delete-fits-header (ff key &key
				   throw-error delete-all)
  (:documentation "delete fits header, accepting wildcards in KEY
If DELETE-ALL is set (default), then iterates until all keys
matching KEY are deleted.  If GO-TO-START is set (default) then
rewind to start of HDU before deleting.
Returns (values success num-deleted last-status last-error-string)
 success is T if all matching keys (including 0 keys) were deleted, and 
 status and err-string are those of the LAST attempt.  Thus the
 status and error string returned will always say that the
 key was not found, unless DELETE-ALL is NIL.
"))


#+nil ;;old version - we find that cfitsio fits header deletion does not
      ;; respect HIERARCH wildcards.  A bug?
(defmethodL delete-fits-header ((ff fits-file) key &key
			       (throw-error t)
			       (delete-all t)
			       (go-to-start t))
  (declare (type fits-file ff)
	   (type string key))
  (check-if-ff-closed ff "delete-fits-header")
  (let (status err-string dum1 dum2 dum3)
    (declare (ignorable dum1 dum2 dum3))
  (block ret
    ;; rewind if asked
    (when go-to-start
      (multiple-value-setq (dum1 dum2 dum3 status err-string)
	(get-header-line-fields-for-keynum-for-pointer
	 (fits-file-fptr ff) 0 :throw-error throw-error))
      (when (not (zerop status))
	(return-from ret (values nil 0 status err-string))))
    ;; iterate
    (loop
     with num-deleted = 0 ;; number of headers deleted
     do
     (multiple-value-bind (success status err-string)
	 (delete-header-for-pointer (fits-file-fptr ff) key
				    :throw-error throw-error)
       (when (or (not success) (= status +KEY_NO_EXIST+) (not delete-all))
	 (return-from ret (values success num-deleted status err-string)))
       (incf num-deleted)
       )))))


(defgeneric delete-fits-header (ff key &key
				   throw-error delete-all extension)
  (:documentation "delete fits header, accepting wildcards in KEY
If DELETE-ALL is set (default), then iterates until all keys
matching KEY are deleted.  

Returns 
  (VALUES SUCCESS NUM-DELETED  KEYS-DELETED 
          LAST-STATUS LAST-ERROR-STRING)
 success is T if all matching keys (including 0 keys) were deleted, and 
 status and err-string are those of the LAST attempt.  Thus the
 status and error string returned will always say that the
 key was not found, unless DELETE-ALL is NIL.

CFITSIO's handling of wildcards in for HIERARCH headers appears
broken, so we first make a wildcard-matching pass to collect headers,
then delete them.
"))

(defmethodL delete-fits-header ((ff fits-file) key &key
			       (throw-error t)
				(delete-all t)
				(extension nil))
  (declare (type fits-file ff)
	   (type string key))
  (check-if-ff-closed ff "delete-fits-header")
  ;; 
  (with-fits-extension (ff extension)
    (let ((keys-to-delete nil)
	  (num-deleted 0)
	  success  status err-string)
      ;; iterate reading headers to get the keys to delete
      (loop
	for i from 1
	do ;; first accumulate which keys to delete
	   (multiple-value-bind (this-key value-str comment-string status err-string)
	       (get-header-line-fields-for-keynum-for-pointer
		(fits-file-fptr ff) i :throw-error throw-error)
	     (declare (ignorable value-str comment-string err-string))
	     (when (= status  +KEY_OUT_BOUNDS+)
	       (return))
	     (when (fits-compare-string key this-key nil)
	       ;; CFITSIO deletion deals poorly with HIERARCH - we decide
	       ;; manually if THIS-KEY is HIERARCH
	       (let ((true-keyname (if (<= (length this-key) 8)
				       this-key
				       (concatenate 'string "HIERARCH " this-key))))
		 (push true-keyname keys-to-delete)
		 (when (not delete-all) (return)) ;; just doing first matching key
		 ))))
      ;;
      ;; then at end delete them, to prevent strange effects in cfitsio
      (loop for true-keyname in (reverse keys-to-delete)
	    do
	       (multiple-value-bind (success-d status-d err-string-d)
		   (delete-header-for-pointer (fits-file-fptr ff) true-keyname
					      :throw-error throw-error)
		 (setf success success-d
		       status status-d
		       err-string err-string-d)
		 (when  (plusp status-d) (return))
		 (incf num-deleted)))
      (values success num-deleted keys-to-delete status err-string))))

     
(defmethodL delete-fits-header ((ff string) key &key
				(throw-error t)
				(delete-all t)
				(extension nil))
				
  (with-open-fits-file
   (ff fitsfile :mode :io :throw-error throw-error)
   (delete-fits-header fitsfile key
		       :throw-error throw-error
		       :delete-all delete-all
		       :extension extension)))




(defgeneric write-fits-header (ff key val &key comment update throw-error extension)
  (:documentation
    "write a fits header VAL for KEY, with optional comment.  If update is T,
then a current header with the same name is updated rather than writing
a new header, if possible.

If EXTENSION is set, then go to this extension temporarily."))

(defmethodL write-fits-header ((ff fits-file) key val &key
			      (comment nil) (update t) (throw-error t) extension)
  (declare (type fits-file ff)
	   (type string key)
	   (type (or real complex string symbol) val) ;; includes T and NIL
	   (type (or null string) comment))
  (check-if-ff-closed ff "write-fits-header")
  ;; treat special cases specially
  (with-fits-extension (ff extension)
    (cond ((equalp key "END")
	   (error "Can't write special END header manually"))
	  ((equalp  key "HISTORY")
	   (write-history-for-pointer (fits-file-fptr ff) val :throw-error throw-error))
	  ((equalp  key "COMMENT")
	   (write-comment-for-pointer (fits-file-fptr ff) val :throw-error throw-error))
	  (t
	   (write-header-value-for-pointer (fits-file-fptr ff) key val comment
					   :update update :throw-error throw-error)))))
  

(defmethodL write-fits-header ((ff string) key val &key
			      (comment nil) (update t) (throw-error t) extension)
  (with-open-fits-file
   (ff fitsfile :mode :io :throw-error throw-error)
   (write-fits-header fitsfile key val :comment comment
				       :update update :throw-error throw-error
				       :extension extension )))






(defgeneric write-fits-comment (ff comment &key throw-error extension)
  (:documentation "Write a fits COMMENT header to a fits file.  COMMENT
is a string or symbol."))

(defmethodL write-fits-comment (ff comment &key (throw-error t) extension)
  (declare (type fits-file ff)
	   (type (or symbol string) comment))
  (check-if-ff-closed ff "write-fits-comment")
  (with-fits-extension (ff extension)
    (write-comment-for-pointer (fits-file-fptr ff) comment :throw-error throw-error)))

(defmethodL write-fits-comment ((ff string) comment &key (throw-error t) extension)
  (with-open-fits-file
      (ff fitsfile :mode :io :throw-error throw-error)
    (write-fits-comment fitsfile comment :throw-error throw-error
			:extension extension)))
  



(defgeneric write-fits-history (ff history &key throw-error extension)
  (:documentation "Write a fits HISTORY header to a fits file.  HISTORY
is a string or symbol."))

(defmethodL write-fits-history (ff history &key (throw-error t) extension)
  (declare (type fits-file ff)
	   (type (or symbol string) history))
  (check-if-ff-closed ff "write-fits-history")
  (with-fits-extension (ff extension)
    (write-history-for-pointer (fits-file-fptr ff) history :throw-error throw-error)))

(defmethodL write-fits-history ((ff string) history &key (throw-error t) extension)
  (with-open-fits-file
      (ff fitsfile :mode :io :throw-error throw-error)
    (write-fits-history fitsfile history :throw-error throw-error
			:extension extension)))
  





(defgeneric move-to-extension (ff extension &key throw-error extver hdu-type)
  (:documentation
   "move to a new extension, either by name or by index
hdu-type is one of :image-hdu :ascii-tbl :binary-tbl :any-hdu, and
is used for named (not numbered) extension.
extver is the version for named versions, and ignored if extver=0
Note that when extension is a number, it is possible for
an error-free return to occur even if extension does not exist.

Some special symbols for EXTENSION are:

   :FIRST-IMAGE - move to first image in the fits file


WARNING - if extension is a number, then it is an HDU-NUM, which
  starts at 1.  However, cfitsio filename extension labeling starts as
  image.fits[0].  To get to image.fits[0], use 
  (move-to-extension  \"image.fits\" 1)."))

(defmethodL move-to-extension ((ff fits-file) extension &key
			       (throw-error t)
			       (extver 0)
			       (relative nil)
			       (hdu-type :any-hdu))
  (declare (type fits-file ff)
	   (type (member :image-hdu :ascii-tbl :binary-tbl :any-hdu)
		 hdu-type)
	   (type (or string (unsigned-byte 10) (member :first-image))
		 extension))
  (check-if-ff-closed ff "move-to-extension")
  (when (and relative (not (integerp extension)))
    (error "If RELATIVE is True then EXTENSION must be an integer"))    
  (flet ((mte (%extension)
	   (multiple-value-bind (result status err-string)
	       (go-to-extension-for-pointer (fits-file-fptr ff) %extension
					    :relative relative
					    :hdu-type hdu-type
					    :extver extver
					    :throw-error throw-error)
	     ;; reset the hdu in the structure
	     (read-fits-file-hdu ff :throw-error throw-error)
	     ;;
	     (values result status err-string))))
    
    (cond ((eq extension :first-image)
	   (loop for i from 1 to (fits-file-num-hdus ff)
		 do (multiple-value-bind (result status err-string)
			(mte i)
		      (when (zerop status)
			(when (and
			       (eq (fits-file-current-hdu-type ff) :image)
			       (plusp (fits-file-current-image-ndims ff)))
			  (return (values result status err-string)))))
		 finally
		    (when throw-error
		      (error "No valid first image extension found."))
		    (return
		      (values nil -1 "No valid image extension found."))))
	  (t
	   (mte extension)))))
				   
			  
			  



	 
		    
	 


(defgeneric delete-current-extension (ff &key throw-error)
  (:documentation "Delete the current FITS extension. If it is a
primary extension, then replace the primary with a minimal fits
header. If this is the last extension, move to previous. Otherwise,
shift all subsequent extensions up and remain at what was the next
extension"))

(defmethodL delete-current-extension ((ff fits-file) &key throw-error)
  (declare (type fits-file ff))
  (check-if-ff-closed ff "delete-current-extension")
  (multiple-value-bind (result status err-string)
      (delete-current-hdu-for-fits-pointer (fits-file-fptr ff)
					   :throw-error throw-error)
    ;; reset the hdu in the structure
    (read-fits-file-hdu ff :throw-error throw-error)
    ;; and the number of hdus
    (setf (fits-file-num-hdus ff)
	  (get-number-of-extensions-for-fits-pointer
	   (fits-file-fptr ff) :throw-error throw-error))
    (values result status err-string)))
      


      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this one needs four methods - a PITA
(defgeneric copy-current-extension (ff-from ff-to 
				    &key throw-error morekeys)
  (:documentation "Copy current fits extension in FF-FROM to the end of
FF-TO, possibly adding MOREKEYS number of slots for more keys.  If
FF-FROM is not an open fits file pointer but is a file path, then open
it and use first extension. If FF-TO is a path, then open it, and
write to end."))

(defmethodL copy-current-extension ((ff-from fits-file) (ff-to fits-file)
				   &key (throw-error) (morekeys 0))
   (declare (type fits-file ff-from ff-to)
	    (type (unsigned-byte 20) morekeys))
  (check-if-ff-closed ff-from "copy-current-extension")
  (check-if-ff-closed ff-to "copy-current-extension")
  (multiple-value-bind (status err-string)
      (copy-hdu-from-fits-pointer-to-fits-pointer 
       (fits-file-fptr ff-from) (fits-file-fptr ff-to) 
       :throw-error throw-error :morekeys morekeys)
    ;; reset the target fits file ff-to
    ;; reset the hdu in the structure
    (read-fits-file-hdu ff-to :throw-error throw-error)
    (setf (fits-file-num-hdus ff-to)
	  (get-number-of-extensions-for-fits-pointer
	   (fits-file-fptr ff-to) :throw-error throw-error))
    (values status err-string)))

(defmethodL copy-current-extension ((ff-from string) (ff-to fits-file)
				   &key (throw-error) (morekeys 0))
  (with-open-fits-file  (ff-from ffff-from :mode :input)
    (copy-current-extension ffff-from ff-to :throw-error throw-error
			    :morekeys morekeys)))

(defmethodL copy-current-extension ((ff-from fits-file) (ff-to string)
				   &key (throw-error) (morekeys 0))
  (with-open-fits-file (ff-to ffff-to :mode :io)
    (copy-current-extension ff-from ffff-to :throw-error throw-error
			    :morekeys morekeys)))

(defmethodL copy-current-extension ((ff-from string) (ff-to string)
				   &key (throw-error) (morekeys 0))
  (with-open-fits-file  (ff-from ffff-from :mode :input)
    (with-open-fits-file (ff-to ffff-to :mode :io)
      (copy-current-extension ffff-from ffff-to :throw-error throw-error
			      :morekeys morekeys))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




    
    






 
;; helper for read-image-section  -
;;   for a type, return the lisp-style array type
(defun get-atype-for-type (type)
  (case type
    (:unsigned-byte-8  '(unsigned-byte 8))
    (:signed-byte-8    '(signed-byte 8))
    (:unsigned-byte-16 '(unsigned-byte 16))
    (:signed-byte-16   '(signed-byte 16))
    (:unsigned-byte-32 '(unsigned-byte 32))
    (:signed-byte-32   '(signed-byte 32))
    ((:float :single-float)     'single-float)
    ((:double :double-float)     'double-float)
    (t (error "Unknown type ~A" type))))


(defun get-zero-for-type (type)
  (case type
    (:unsigned-byte-8  0)
    (:signed-byte-8    0)
    (:unsigned-byte-16 0)
    (:signed-byte-16   0)
    (:unsigned-byte-32 0)
    (:signed-byte-32   0)
    ((:float :single-float)  0.0e0)
    ((:double :double-float) 0.0d0)
    (t (error "Unknown type ~A" type))))





;; same but in add-image-to-fits-file, where the options are limited
(defun get-creation-type-for-type (type)
  (case type 
    (:byte '(unsigned-byte 8))
    (:short '(signed-byte 16))
    (:ushort '(unsigned-byte 16))
    (:long '(unsigned-byte 32))
    (:float      'single-float)
    (:double     'double-float)
    (t (error "Unknown type ~A" type))))

(defun get-zero-for-creation-type (type)
  (case type
    (:byte   0)
    (:short  0)
    (:ushort 0)
    (:long   0)
    (:float  0.0)
    (:double 0d0)
    (t (error "Unknown type ~A" type))))

;; really need to clarify our types
(defun get-read-type-for-creation-type (type)
    (case type
      (:byte   :unsigned-byte-8)
      (:short  :signed-byte-16)
      (:ushort :unsigned-byte-16)
      (:long   :signed-byte-32)
      (:float  :single-float)
      (:double :double-float)
      (t (error "Unknown type ~A" type))))

(defun get-read-type-for-bitpix (bitpix)
  (get-read-type-for-creation-type
   (bitpix-to-type bitpix)))



(defmacro with-bzero-and-bscale ((ff &key (bzero nil) (bscale nil) (throw-error t)) &body body)
  "Macro to use FFPSCL / FITS_SET_BSCALE to use a given BZERO and BSCALE
while performing BODY.  This would typically be used in a read/write operation
to suppress the use of BZERO,BSCALE headers.   The old values are restored."
  `(flet ((do-wbab-body ()
	    ,@body))
     (let ((%bzero ,bzero)
	   (%bscale ,bscale)
	   (%throw-error ,throw-error)
	   (%ff ,ff))
       (cond
	 ;; if neither is set, just run the body
	 ((not (or %bzero %bscale))
	  (do-wbab-body))
	 (t
	  ;; read in initial values
	  (let ((bscale-orig (or (read-fits-header %ff "BSCALE") 1d0))
		(bzero-orig (or (read-fits-header %ff "BZERO") 0d0)))
	    (when (or (not (typep bzero-orig 'double-float))
		      (not (typep bscale-orig 'double-float)))
	      (error "Image BSCALE=~S and BZERO=~S are not double-floats in macro WITH-BZERO-AND-BSCALE"
		     bscale-orig bzero-orig))
	    (unwind-protect
		 (progn
		   (fits-set-bscale (fits-file-fptr %ff)
				    (float bscale 1d0) (float bzero 1d0)
				    :throw-error %throw-error)
		   (do-wbab-body))
	      ;; restore original values
	      (progn
		(fits-set-bscale (fits-file-fptr %ff) bscale-orig bzero-orig :throw-error %throw-error)))))))))
	      
	       
			  
	  
	    

(defgeneric read-image-section (ff &key extension fp lp throw-error
				     read-wcs data-array
				     null-value type headers
				     bzero bscale
				     try-fz try-gz)
  (:documentation
   "read a section from a FITS image.  FP and LP are the first-pixel
and last-pixel, as vectors, or NIL if we should read the
whole image.  TYPE is the type of the output image-section-data array
and is one of
     :unsigned-byte-8   :signed-byte-8
     :unsigned-byte-16  :signed-byte-16
     :unsigned-byte-32  :signed-byte-32
     :single-float      :double-float 
The default is :single-float

FP and LP are FirstPixel and LastPixel (inclusive) arrays.

Note that a potential problem is that FITS file may contain an
additive offset that means (eg) that an 8 bit array may not be
able to contain an 8 bit FITS file; this will cause an error.

Note that cfitsio has Fortran style indices, so that
image[1:100,1:1000] will have an image section with a DATA array
that is a 1000x100 array.

If DATA-ARRAY is an array of the correct type and dimensions, then
reuse it to hold the data, thereby avoiding excessive allocation.

If NULL-VALUE is set, then image pixels with the BLANK header
will be set to this.  It must agree with TYPE, if not NIL.

If READ-WCS is T (the default), try to read a WCS from the current
header. NOTE - The WCS is adjusted for FP.

HEADERS is a list of of headers to read in while reading the image,
from the same extension.

BZERO and BSCALE override the keyword values; useful for forcing
data to be read as it is in the fits array.

TRY-FZ and TRY-GZ try XX.fits.fz and XX.fits.gz (or .Z) if the
 fits file doesn't exist."))



 
(defmethodL read-image-section 
    ((ff fits-file)
     &key extension fp lp (throw-error t)
     (read-wcs t) ;; read wcs if we can
     (data-array nil) ;; reuse this array if possible
     (null-value NIL)
     (type :single-float)
     (headers nil)
     bzero bscale
     try-fz try-gz)
  (declare (type (member :unsigned-byte-8 :signed-byte-8
			 :unsigned-byte-16 :signed-byte-16
			 :unsigned-byte-32 :signed-byte-32
			 :single-float :double-float))
	   (type (or null vector) fp lp)
	   (ignorable try-fz try-gz)) ;; invalid for file that is open
  (with-fits-extension (ff extension)
    (when (> (fits-file-current-image-ndims ff) 9)
      (error "Too many dimensions in fits file"))
    (when (or (not (= (length fp) (length lp)))
	      (> (length fp) 9)
	      (not (every (lambda (n) (and (integerp n) (plusp n))) fp))
	      (not (every (lambda (n) (and (integerp n) (plusp n))) lp)))
      (error "BAD fp=~A and lp=~A - must be equal length arrays of positive
integers of length <= 9" fp lp))
    (when (not (eq (fits-file-current-hdu-type ff) :image))
      (error "Cannot read an image-section for image of type ~A. must be :IMAGE"
	     (fits-file-current-hdu-type ff)))
    (when (and fp (not (= (fits-file-current-image-ndims ff) (length fp))))
      (error "Bad dimensions: image has size ~A but FP=~A"
	     (fits-file-current-image-size ff) fp))
    (when (and null-value (not (float-utils:single-float-nan-or-infinity-p null-value))
	       (zerop null-value))
      (error "NULL-VALUE=~A which is a special case in CFITSIO meaning 
'no null value'.  This is almost certainly not what you want if you 
are specifying NULL-VALUE, so an error is being thrown." null-value))
    ;;
    (let* ((parent-size (copy-seq (fits-file-current-image-size ff)))
	   (ndim (length parent-size))
	   (size (make-array ndim :element-type '(unsigned-byte 32)
				  :initial-element 0))
	   (fp (if fp (map '(simple-array (unsigned-byte 32) (*)) #'identity fp)
		   (make-array ndim :element-type '(unsigned-byte 32)
				    :initial-element 1)))
	   (lp (if lp (map '(simple-array (unsigned-byte 32) (*)) #'identity lp)
		   parent-size))
	   (atype (get-atype-for-type type)) ;; eg '(signed byte 32)
	   (is (make-image-section
		:fits-file ff
		;; make a copy of ff in its current state but
		;; make it unusable 
		:orig-fits-file
		(let ((x (copy-structure ff)))
		  (setf (fits-file-is-open x) nil)
		  x)
		:type type
		:ndim ndim
		:size size
		:parent-size parent-size :fp fp :lp lp)))
      ;;
      (when (and null-value (not (typep null-value atype)))
	(error "NULL-VALUE is not of lisp type ~A, derived from TYPE=~A~%" 
	       atype type))
      ;;
      ;; now check the validity of the first and last pixels ranges, and
      ;; figure out if this represents the whole image
      (loop
	with whole = t
	for n1 across fp
	for n2 across lp
	for np across (fits-file-current-image-size ff)
	for i = 0 then (1+ i)
	do
	   (when (> n1 n2) (error "Invalid pixel range: fp[~d]=~d > lp[~d]=~d"
				  i n1 i n2))
	   (when (or (not (= n1 1))  (not (= n2 np))) (setf whole nil))
	   (setf (aref size i) (1+ (- n2 n1)))
	finally (setf (image-section-whole is) whole))
      ;;
      (setf (image-section-data is)
	    (if (typep data-array 
		       `(simple-array ,atype ,(reverse (coerce size 'list))))
		;; reuse old array if we can
		data-array 
		;; note that we must reverse image dimension for cfitio
		;; fortran-type indexing
		(make-array (reverse (coerce size 'list))
			    :element-type atype
			    #+no-typed-arrays :initial-element
			    #+no-typed-arrays (get-zero-for-type type))))
      ;;
      (with-bzero-and-bscale (ff :bzero bzero :bscale bscale)
	(read-image-cube-for-fits-pointer
	 (fits-file-fptr ff) fp lp (image-section-data is)
	 :null-value null-value
	 #+no-typed-arrays :array-type #+no-typed-arrays atype
	 :throw-error throw-error))

      ;; 
      (when read-wcs
	(let ((wcs (read-wcs ff)))
	  (when wcs
	    (setf (image-section-wcs is) 
		  (%shift-wcs-for-fp wcs fp)))))

      (when headers
	(dolist (header headers) 
	  (multiple-value-bind (val comment key)
	      (read-fits-header ff header)
	    (declare (ignorable comment))
	    (when key
	      (push (list header val) (image-section-headers is))))))

      is)))



(defmethodL read-image-section  
    ((ff string)
     &key
     (extension :first-image) ;; by default, move-to-extension finds 1st image
     fp lp (throw-error t)
     (read-wcs t) ;; read wcs if we can
     (data-array nil) ;; reuse this array if possible
     (null-value NIL)
     (type :single-float)
     (headers nil)
     bzero bscale
     try-fz try-gz)
  (with-open-fits-file
      (ff fitsfile :mode :input :throw-error throw-error
		   :try-fz try-fz :try-gz try-gz)
    (read-image-section fitsfile
			:extension extension :fp fp :lp lp
			:read-wcs read-wcs
			:data-array data-array
			:null-value null-value
			:throw-error throw-error
			:type type
			:bzero bzero
			:bscale bscale
			:headers headers)))

 
(defgeneric write-back-image-section (is &key throw-error)
  (:documentation
   "Write the image section back into its open fits-file.

The CFITSIO documentation says 'A current limitation of the code is
that the data type of the PHDU (principal HDU) cannot be replaced
after the FITS file is created.'  This may mean that other HDUs may be
modified."))

(defmethodL write-back-image-section ((is image-section)
				     &key (throw-error t))
  (let ((ff (image-section-fits-file is))
	(ff-orig (image-section-orig-fits-file is)))
    (when (not (fits-file-is-open ff))
      (error "fits file is closed - cannot write image section"))
    (when (not (eq (fits-file-is-open ff) :io))
      (error "fits file is not open in :IO mode - cannot write image section"))
    (with-fits-extension (ff (fits-file-current-hdu-num ff-orig))
      (write-image-cube-for-fits-pointer   
       (fits-file-fptr ff) (image-section-fp is) (image-section-lp is)
       (image-section-data is) :throw-error throw-error)
      is)))




;; do first-pixel 'fp' and last-pixel 'lp' fit into array of size 'size'?
(defun fp-lp-fit-p (fp lp size)
  (declare (type vector fp lp size))
  (when (not (= (length fp) (length lp) (length size)))
    (error "fp,lp,size of different lengths"))
  (loop for ifp of-type (unsigned-byte 28) across fp
	for ilp of-type (unsigned-byte 28) across lp
	for isz of-type (unsigned-byte 28) across size
	for i of-type (unsigned-byte 28) = 0 then (1+ i)
	do
	(when (> ifp ilp) (error "fp[~D]=~D > lp[~D]=~D" i ifp i ilp))
	(when (> ilp isz) (return nil))
	finally (return t)))

(defgeneric write-image-section (ff is &key throw-error reckless)
  (:documentation
   "Write an image section into a fits file.  If RECKLESS is set,
then don't check that the HDU type, etc match.  This is for use when
creating new files."))

(defmethodL write-image-section ((ff fits-file) (is image-section)
				 &key (throw-error t) reckless
				 extension)
  ;;(format t "write-image-section - Fits file method~%")
  (when (not (fits-file-is-open ff))
    (error "fits file is closed - cannot write image section"))
  (when (not (eq (fits-file-is-open ff) :io))
    (error "fits file is not open in :IO mode - cannot write image section"))

  (when extension (move-to-extension ff extension))
  ;; in reckless mode we just write it without checking for consistency
  (when (not reckless)
    (when (not (eq (fits-file-current-hdu-type ff) :image))
      (when (not (eq (fits-file-is-open ff) :io))
	(error
	 "current hdu is not of type :IMAGE - cannot write image section")))
    (when (not (= (fits-file-current-image-ndims ff)
		  (image-section-ndim is)))
      (error "Number of image dimensions do not match for image section
and fits-file - cannot write image section"))
    (when (not (fp-lp-fit-p (image-section-fp is) (image-section-lp is)
			    (fits-file-current-image-size ff)))
      (error "image section fp=~A lp=~A do not fit into image of size ~A -
cannot write image section"
	     (image-section-fp is) (image-section-lp is)
	     (fits-file-current-image-size ff))))
  ;;
  (write-image-cube-for-fits-pointer   
   (fits-file-fptr ff) (image-section-fp is) (image-section-lp is)
   (image-section-data is) :throw-error throw-error)
  ;;
  is)


(defmethodL write-image-section ((ff string) (is image-section)
				&key (throw-error t) reckless extension)
  ;;(format t "write-image-section - string method~%")
  (with-open-fits-file 
   (ff fits-file :mode :io :throw-error throw-error)
    (write-image-section fits-file is :throw-error throw-error
				      :reckless reckless
				      :extension extension)))




(defunL create-fits-file (filename &key (template nil)
				  (overwrite nil)
				  (close nil)
				  (make-empty-primary nil)
				  (throw-error t))
  "create a new fits file with no content, returns (values (or NIL
fits-file) status error-string) If CLOSE, then close the file upon
creation.  This file cannot be closed without writing further content
to it unless it was created with a TEMPLATE.  Remember to create
SIMPLE=T, NAXIS=X, BITPIX=X keywords, or add an image or table, or do
something equivalent for tables, to make it usable.

If MAKE-EMPTY-PRIMARY is set, then it is turned into a valid NAXIS=0
primary header."
  (declare (type string filename)
	   (type (or null string) template))
  (when (and template (not (probe-file template)))
    (error "Could not find template file ~A" template))
  (when (and (not overwrite) (probe-file filename))
    (error "Would overwrite file ~A and :OVERWRITE is NIL" filename))
  (when (and close (not template))
    (error "Cannot immediately close empty file created without a template."))
  (block ret
    (multiple-value-bind (fptr status err-string)
	(create-fits-file-pointer filename template
				  :overwrite overwrite
				  :throw-error throw-error)
      (when (not fptr) (return-from ret (values nil status err-string)))
      (let ((ff (make-fits-file :filename filename
				:fptr fptr :is-open :io)))
	;; mandatory keyword - routines like FFCRIM should add the
	;; other mandatory keywords in the right place
	(when (and (not template) make-empty-primary)
	  (write-fits-header ff "SIMPLE" T)
	  (write-fits-header ff "BITPIX" 16)
	  (write-fits-header ff "NAXIS" 0))
	
	(when close (close-fits-file ff :throw-error throw-error))
	(values ff nil nil)))))


;; this routine was causing lots of problems, like not creating the
;; extension.  So treat it with suspicion.
(defgeneric add-image-to-fits-file (ff type naxes &key throw-error create-data)
  (:documentation
   "Add an image of TYPE = :byte :short :ushort :long :float :double to an
fits file, becoming the primary extension if the file is empty, or the
next extension if it contains an extension already - NAXES is a vector
giving the dimensions of the image.  If CREATE-DATA is set, then the
data are created as a big array and written with zeros.  If
CREATE-DATA is an array, the values in this array are used instead of
zero.  Note that CFITSIO seems to create the data array of the fits
file on closing the file by default, so using anything but an array
for CREATE-DATA seems pointless.  If CREATE-DATA is T (not an array) then
the write will be in one big chunk, which may cause Lisp to run out
of memory for big images.

IMPORTANT: This must be the first thing done, before writing headers, if this
is to be the primary extension."))

(defmethodL add-image-to-fits-file ((ff fits-file) type naxes
				   &key (throw-error t)
				   (create-data NIL))
  (declare (type (member :byte :short :ushort :long :float :double) type)
	   (type vector naxes))
  (when (not (every (lambda (n)
		      (and (typep n 'unsigned-byte) (<= n  +max-image-dim+)))
		    naxes))
    (error "every element of naxes is not < ~D" +max-image-dim+))
  (when (> (length naxes) +max-image-dims+)
    (error "too many dimensions (~D) in image - only ~D allowed"
	   (length naxes) +max-image-dims+))
  ;; first we move to the last header
  (when (plusp (fits-file-num-hdus ff))
    (move-to-extension ff (fits-file-num-hdus ff)))
  ;;
  (block ret
    ;; create the image
    (multiple-value-bind (success status err-string)
	(create-fits-image-for-fits-file-pointer
	 (fits-file-fptr ff) type naxes :throw-error throw-error)
      (when (not success)
	(return-from ret (values success status err-string))))
    ;; note new number of hdus
    (setf (fits-file-num-hdus ff)
	  (get-number-of-extensions-for-fits-pointer
	   (fits-file-fptr ff) :throw-error throw-error))
    ;; if desired, add blank data too
    ;; FIXME - should write this in small chunks!!
    (when create-data
      (when (and  (arrayp create-data)
		  (not (equalp (reverse (array-dimensions create-data))
			       (coerce naxes 'list))))
	(error "Dimensions of CREATE-DATA array are ~A not equal to naxes ~A - note that FORTRAN style indexing means that order of dimensions is reversed."
	       (array-dimensions create-data) naxes))
      ;;
      (let ((a (if (arrayp create-data)
		   create-data
		   (make-array (coerce naxes 'list)
			       :element-type (get-creation-type-for-type type)
			       :initial-element (get-zero-for-creation-type type))))
	    (fp (make-array (length naxes) :initial-element 1))
	    (lp naxes))
	;; this is NOT obvious - first we re-read the HDU, then
	;; we move to the last extension, then we write the data
	(read-fits-file-hdu ff :throw-error throw-error)
	;;(move-to-extension ff  (min 1 (fits-file-num-hdus ff)))
	(write-image-cube-for-fits-pointer   
	 (fits-file-fptr ff) fp lp a :throw-error throw-error)))
    ;; register it
    (read-fits-file-hdu ff :throw-error throw-error)
    (values t nil nil))) ;; return success  




(defmethodL add-image-to-fits-file ((ff string) type naxes
				   &key (throw-error t) (create-data t))
  (with-open-fits-file
   (ff fitsfile :mode :io :throw-error throw-error)
   (add-image-to-fits-file fitsfile type naxes :throw-error throw-error
			   :create-data create-data)))
		       



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fits table functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defgeneric add-table-to-fits-file (fits-file columns 
					      &key
					      nrows
					      table-type
					      extname
					      tunit-vec
					      throw-error)
  (:documentation 
   "Create a new table extension for fits-file FF. COLUMNS is a vector
like 

 #((name-string-1 :LONG) 
   (name-string-2 :FLOAT)  
   (name-string-3 :DOUBLE-FLOAT)  
   (name-string-4 :SHORT N)       ;; N given for number of items in entry
   (name-string-5 :STRING LEN)    ;; LEN for length of string - MUST be given
   (name-string-6 :STRING LEN N)  ;; LEN is length of string, N is number
                                  ;;     of strings in an entry


TUNIT is an optional vector of strings giving the units, or NIL.
TABLE-TYPE is :BINARY or :ASCII 

Nrows is usually 0, because the
table is expanded as data are written - ASCII TABLES ARE NOT
SUPPORTED AT PRESENT

NOTE for strings - the type 'rAw' means a total string length of r, but
 subdivided into strings of length w, so that ('foo' :STRING 2 5) becomes
 type '10A2'

If this is the first data in the file, it must have SIMPLE=T,
BITPIX=N, NAXIS=0 headers in the primary, or table creation will fail
with mysterious cfitsio errors about missing headers."))
		
(defun typecode-to-tform  (list)
  (let* ((type (second list))
	 (count (or (third list) 1))
	 (string-rep (fourth list)) ;; only if string
	 (typestr (cdr
		   (assoc type
			  '((:string . "A") (:logical . "L") (:bit . "X") 
			    (:byte . "B")   (:short . "I")   (:long . "J")
			    (:long-long . "K") 
			    (:float . "E")  (:double-float . "D") 
			    (:complex . "C") (:double-complex . "M"))))))
    (when (not typestr)
      (error "Unknown table type symbol ~A~%" type))
    (when (and (not (eq type :string)) string-rep)
      (error "Only a STRING can have a repeat value and a length"))
    ;; according to https://heasarc.gsfc.nasa.gov/docs/software/fitsio/quick/node10.html
    ;;   "rA" is a char string; "rAw" is an array of strings, each of length w
    (cond ((and (eq type :string) string-rep)
	   (format nil "~DA~D" (* count string-rep) count))
	  (t
	   (format nil "~D~A" count typestr)))))
	  

(defmethodL add-table-to-fits-file ((ff fits-file) columns
				   &key
				   (nrows 0)
				   (table-type :binary)
				   extname 
				   tunit-vec
				   (throw-error t))
  (declare (type vector columns)
	   (type unsigned-byte nrows)
	   (type (member :binary :ascii) table-type)
	   (type (or null string) extname)
	   (type (or null vector) tunit-vec))
  (when (eq table-type :ascii)
    (error "creation of ASCII tables is not supported"))
  (when (and (not tunit-vec)
	     (not (every 'stringp tunit-vec)))
    (error "TUNIT-VEC must be NULL or a vector of strings"))


  (when (and (zerop (fits-file-num-hdus ff))
	     (not (and (read-fits-header ff "SIMPLE")
		       (read-fits-header ff "BITPIX")
		       (read-fits-header ff "NAXIS"))))
    (error "Required keywords SIMPLE, BITPIX, NAXIS are missing for a zero-extension fits file.  Adding a table to a fits file requires at least a valid null primary FITS extension."))

  
  (let ((colname-vec (map 'vector 'first columns))
	(tform-vec (map 'vector 'typecode-to-tform columns)))
    (when (not (or (null tunit-vec) (= (length columns) (length tunit-vec))))
      (error "TUNIT and COLUMNS have different lengths"))
    (when (not (every 'stringp colname-vec))
      (error "Every COLUMN-NAME must be a string"))
    
    (create-table-extension-for-fits-pointer 
     (fits-file-fptr ff) +BINARY_TBL+ nrows colname-vec
     tform-vec tunit-vec
     extname :throw-error throw-error))

  ;; note new number of hdus
  (setf (fits-file-num-hdus ff)
	(get-number-of-extensions-for-fits-pointer
	 (fits-file-fptr ff) :throw-error throw-error))
  (read-fits-file-hdu ff :throw-error throw-error)
  (values t nil nil))
     
					     
(defmethodL add-table-to-fits-file ((fits-file string) columns
				   &key
				   (nrows 0)
				   (table-type :binary)
				   extname 
				   tunit-vec
				   (throw-error t))
  (with-open-fits-file (fits-file ff :mode :io :throw-error throw-error)
    (add-table-to-fits-file ff columns :nrows nrows
			    :table-type table-type :extname extname
			    :tunit-vec tunit-vec
			    :throw-error throw-error)))
		       

(defgeneric write-column-to-fits-table (ff column-name-or-number data-vector
					   &key
					   firstrow nelements
					   firstelem
					   null-value
					   throw-error) 
  (:documentation 
   "Write DATA-VECTOR  to column COLUMN-NAME-OR-NUMBER in fits-file FF. 
FIRST-ROW (default 1) is the first now to write.   NELEMENTS is the number of elements,
defaulting to all of them. See documentation for READ-COLUMN-FROM-FITS-TABLE for
notes on data storage.  Values equal to NULL-VALUE are turned into FITS NULL"))
 
(defmethodL write-column-to-fits-table ((ff fits-file)
				       column-name-or-number data-vector
				       &key
				       (firstrow 1) nelements
				       (firstelem 1)
				       null-value
				       (throw-error t))
  (when (not (member (fits-file-current-hdu-type ff) '(:ascii-tbl :binary-tbl)))
    (error "Fits file current hdu is of type ~A, not :ascii-tbl or :binary-tbl"
	   (fits-file-current-hdu-type ff)))
  (let ((colnum 0)
	(type-symbol nil)
	(num-elements  (or nelements (array-total-size data-vector))))
    ;; get the column number
    (if (numberp column-name-or-number)
	(setf colnum column-name-or-number)
	(progn
	  (setf colnum (position column-name-or-number
				 (fits-file-current-table-columns ff)
				 :test 'string=))
	  (when (not colnum)
	    (error "table column name ~A not found" column-name-or-number))
	  (incf colnum)))
    (if (or (not (plusp colnum)) (> colnum (fits-file-current-table-ncols ff)))
	(error "Column number=~d but should be in 1 to ~d, inclusive"
	       colnum (fits-file-current-table-ncols ff)))
    ;;
    (setf type-symbol (aref (fits-file-current-table-types ff) (1- colnum)))
    ;;
    ;;
    (write-table-column-for-fits-pointer (fits-file-fptr ff)
					 data-vector
					 colnum firstrow
					 num-elements type-symbol
					 :null-value null-value
					 :firstelem firstelem
					 :throw-error throw-error)
    ;;
    (read-fits-file-hdu ff :throw-error throw-error) ;; we may have changed number of rows
    ;;
    (values t nil nil)))
       
 
(defmethodL write-column-to-fits-table ((fits-file string)
				       column-name-or-number data-vector
				       &key
				       (firstrow 1) nelements
				       (firstelem 1)
				       null-value
				       (throw-error t))
  (with-open-fits-file  (fits-file ff :mode :io :throw-error throw-error)
    (write-column-to-fits-table ff column-name-or-number data-vector
				:firstrow firstrow 
				:nelements nelements
				:firstelem firstelem
				:null-value null-value
				:throw-error throw-error)))
 


  

(defgeneric read-column-from-fits-table (ff column-name-or-number
					 &key
					   extension
					   firstrow nrows
					   repeats-as-matrix
					   firstelem
					   type-sym
					   string-length
					   return-null-array
					   null-value
					   string-trim-nulls
					   throw-error)
  (:documentation
   "Read a column from a fits table in FF.  Column is specified in COLUMN-NAME-OR-NUMBER..
FIRST-ROW (default 1) is the first now to read.   NROWS is the number of rows; the number
defaulting to all of them.  Note that the number of elements may be larger than the
number of rows if  REPEATS>1, so the length of data returned is NROWSxNRPEATS where
NREPEATS is the number of repeats for this row.

REPEATS-AS-MATRIX (default T) turns a returned vector into a matrix of 
NROWSxNREPEATS.
  
TYPE-SYM, if specified, is the output type.    If the user supplied
output type is :STRING, then it is also necessary to set STRING-LENGTH correctly to avoid
segmentation faults.    By default, RETURN-NULL-ARRAY is set to true, and the 2nd
value is an array of T/NIL relating where the table has valid/undefined values.  If
the user sets RETURN-NULL-ARRAY to false, then it is necessary to set NULL-VALUE
to an object of the correct type (eg double-float).

Note on data storage:  cfitsio starts at FIRSTELEM and reads straight forward, continuing
on subsequent rows.   Strings appear treated differently: the number of substrings
is repeat/width for a string field (see fits file structure).  Most of time,
repeat/width=1 and there will be one string per column so there are no complications.
"))

;; FIXME - REPEATS  should be handled as a MATRIX




(defmethodL read-column-from-fits-table ((ff fits-file)
					column-name-or-number
					&key
					extension
					(firstrow 1) nrows
					(repeats-as-matrix t)
					(firstelem 1)
					type-sym
					string-length
					(return-null-array t)
					null-value
					;; trim any nulls from returned strings
					(string-trim-nulls t)
					(throw-error t))
  (when extension (move-to-extension ff extension))
  (when (not (member (fits-file-current-hdu-type ff) '(:ascii-tbl :binary-tbl)))
    (error "Fits file current hdu is of type ~A, not :ascii-tbl or :binary-tbl"
	   (fits-file-current-hdu-type ff)))
  (when (and (eq type-sym :string) (not string-length))
    (error "string return value specified, but length not provided"))
  (when (and null-value return-null-array)
    (error "ERROR in READ-COLUMN-FROM-FITS-TABLE.  Both
    NULL-VALUE=~A and RETURN-NULL-ARRAY are set, but NULL-VALUE is
    ignored when RETURN-NULL-ARRAY is set (the default)." null-value))
  (let ((colnum 0)
	(type-symbol nil)
	(num-elements 0))

    ;; get the column number 
    (if (numberp column-name-or-number)
	(setf colnum column-name-or-number)
	(progn
	  (setf colnum (position column-name-or-number
				 (fits-file-current-table-columns ff)
				 :test 'string=))
	  (when (not colnum)
	    (error "table column name ~A not found" column-name-or-number))
	  (incf colnum)))
    (if (or (not (plusp colnum)) (> colnum (fits-file-current-table-ncols ff)))
	(error "Column number=~d but should be in 1 to ~d, inclusive"
	       colnum (fits-file-current-table-ncols ff)))


    (setf type-symbol (or type-sym (aref (fits-file-current-table-types ff) (1- colnum))))

    ;; according to manual, for strings, string_length=width/repeat
    (let* 
	;; repeat factor - how many items in this table cell (ie, sub-columns)
	((repeat (if (eq (fits-file-current-hdu-type ff) :ascii-tbl)
		      1 ;; repeat is always 1 for ascii table
		      (aref (fits-file-current-table-repeats ff) (1- colnum))))
	 (width  (aref (fits-file-current-table-widths ff) (1- colnum)))
	 (nitems/entry  (aref (fits-file-current-table-nitems/entry ff) (1- colnum)))
	 (string-len   (if (eq type-symbol :string) repeat)))

      (declare (ignorable width))
      ;;
      #+nil
      (format t "Repeat: ~A  width: ~A  string-len: ~A~%"
	      repeat width string-len)
      
      
      ;;
      (setf num-elements 
	    (or (if nrows (* nrows nitems/entry))
		(*  (fits-file-current-table-nrows ff) nitems/entry)))
      
      (multiple-value-bind (v null-array anynul status error-string)
	  (read-table-column-for-fits-pointer (fits-file-fptr ff)
					      colnum firstrow num-elements
					      type-symbol
					      :null-value null-value
					      :firstelem firstelem
					      :return-null-array return-null-array
					      :string-trim-nulls string-trim-nulls
					      :string-size string-len
					      :throw-error throw-error)
	
	;; check that the output array is an integer number of REPEAT long
	(when (and v (not (zerop (nth-value 1 (round (length v) nitems/entry)))))
	  (error "The output array for reading fits column ~A is ~A elements long, which is not an integer multiple of the column's REPEATS=~A"
		 column-name-or-number (length v) repeat))
	
	(flet ((maybe-convert-vec-to-matrix (vec)
		 (if (or (= nitems/entry 1) 
			 (not repeats-as-matrix))
		     vec
		     (loop 
		       with n = (array-total-size vec)
		       with newarr = (make-array (list (round n nitems/entry) nitems/entry)
						 :element-type (array-element-type vec))
		       for i below n
		       do (setf (row-major-aref newarr i) (aref vec i))
		       finally (return newarr)))))
	  (values 
	   (if v (maybe-convert-vec-to-matrix v))
	   (if null-array (maybe-convert-vec-to-matrix null-array))
	   anynul
	   status
	   error-string))))))
			 
    
  
(defmethodL read-column-from-fits-table ((fits-file string)
					column-name-or-number
					&key
					extension
					(firstrow 1) nrows
					(repeats-as-matrix t)
					(firstelem 1)
					type-sym
					string-length
					(return-null-array t)
					null-value
					;; trim any nulls from returned strings
					(string-trim-nulls t)
					(throw-error t))
  (with-open-fits-file
      (fits-file ff :mode :io :throw-error throw-error)

    (read-column-from-fits-table ff column-name-or-number
				 :extension extension
				 :firstrow firstrow
				 :nrows nrows
				 :repeats-as-matrix repeats-as-matrix
				 :firstelem firstelem
				 :type-sym type-sym
				 :string-length string-length
				 :return-null-array return-null-array
				 :null-value null-value
				 :string-trim-nulls string-trim-nulls
				 :throw-error throw-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compression

(defgeneric compressed-image-p (fits-file  &key extension)
  (:documentation
   "Tests is a FITS-FILE is compressed at EXTENSION.
    Return (VALUES
              COMPRESSED-P 
              ZCMPTYPE ;; compression type, eg 'RICE_1'
              ZBITPIX  ;; original bitpix
              ZNAXIS   ;; original naxis
              #(NAXIS1 NAXIS2 ...) ;; original naxes
              )
 the usual THROW-ERROR is set to T because this involves many low level calls."))


(defmethod compressed-image-p ((ff fits-file) &key extension)
  (cf:with-fits-extension (ff extension)
    (multiple-value-bind (compressed-bool status error-string)
	;; presumably this uses ZIMAGE=T header
	(fits-is-compressed-image (fits-file-fptr ff) :throw-error t)
      (declare (ignorable status error-string))
      
      ;;
      (let* ((zcmptype (read-fits-header ff "ZCMPTYPE"))
	     (zbitpix (read-fits-header ff "ZBITPIX"))
	     (znaxis  (read-fits-header ff "ZNAXIS"))
	     (znaxes (if (and znaxis (integerp znaxis) (<= 0 znaxis))
			   (loop with v = (make-array znaxis)
				 for i below znaxis
				 for naxisi
				   = (read-fits-header ff (format nil "ZNAXIS~D" (1+ i)))
				 do (setf (aref v i) naxisi)
				 finally (return v)))))
	(values
	   (plusp compressed-bool)
	   zcmptype
	   zbitpix
	   znaxis
	   znaxes)))))
  
	    
	    
  
(defmethod compressed-image-p ((fits-file string) &key extension)
   (with-open-fits-file
       (fits-file ff :mode :input)
     (compressed-image-p ff :extension extension)))


     


  







