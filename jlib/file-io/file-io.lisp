

;; routines for making file io easier

(defpackage #:file-io
  (:use #:common-lisp)
  (:export
   #:*home-directory*
   #:read-line-of-max-length
   #:read-file-as-string
   #:read-file-as-octets
   #:read-file-as-line-list
   #:read-file-as-split-line-list
   #:read-object-from-file
   #:read-object-list-from-file
   #:do-file-lines
   #:file-count-lines
   #:make-tempfile-name
   #:open-tempfile
   #:with-open-tempfile
   #:file-basename
   #:file-suffix
   #:file-minus-dir
   #:dir-of-file
   #:copy-file
   #:full-namestring
   #:full-namestring/no-symlink-expand 
   #:make-filename-absolute-using-default-pathname-defaults
   #:with-lock-file
   #:write-object-to-file
   ))



(in-package file-io)

(defparameter *home-directory* (uiop/os:getenv "HOME"))

(defun read-line-of-max-length (s max-len)
  (declare (type stream s)
	   (type fixnum max-len)
	   (optimize speed))
  "Read a line from stream S with a maximum length of MAX-LEN.
A longer line causes an error.
Returns LINE-STRING or NIL for EOF."
  (loop
    with n = 256 ;; initial buff len
    with buf = (make-string n)
    for i of-type fixnum from 0
    for c = (read-char s nil nil)
    do
       (cond
	((= i max-len)
	 (error "Line longer than MAX-LEN=~D" max-len))
	;;
	((not c) ;; EOF - if no chars, return NIL
	 (return (if (zerop i) nil (subseq buf 0 i))))
	;;
	((or (eql c #\cr) (eql c #\lf)) ;; end of line
	 ;; clear any additional cr/lf
	 (let ((cnext (peek-char nil s nil nil)))
	   (when (or (eql cnext #\cr) (eql cnext #\lf))
	     (read-char s nil nil)))
	 ;; and return the string
	 (return (subseq buf 0 i)))
	;;
	(t	   ;; c is a normal char, so save it in buf
	 (when (= i n) ;; double the buffer size if hit end of buf
	   (setf n (min max-len (* 2 n)))
	   (let ((newbuf (make-string n)))
	     (loop for j of-type fixnum below (length buf)
		   do (setf (aref newbuf j) (aref buf j)))
	     (setf buf newbuf)))
	 (setf (aref buf i) c)))))


(defun generic-file-to-input-stream (file)
  "Convert path,string, or stream to a stream"
  (cond ((pathnamep file)
         (open file :direction :input))
        ((stringp file)
         ;; (open (make-pathname :name file)  :direction :input);; this breaks for directory
	 (open file :direction :input))
        ((and (streamp file) (input-stream-p file))
         file)
        (t
         (error "generic-file-to-input-stream: FILE not a path, string, or stream"))))


(defun read-file-as-string (file &key (max-length #.(expt 2 20)))
  "Read a file as one long string."
  (let* ((ns 1024)
	 (string (make-string ns))  
	 (k 0))
    (declare (type fixnum ns k)
	     (type string string))
    (flet ((expand-buffer ()
	     (let ((new-string (make-string (* 2 ns))))
	       (loop for i of-type fixnum below ns
		     do (setf (aref new-string i) (aref string i)))
	       (setf string new-string)
	       (setf ns (length new-string)))))
      (with-open-file (s file)
	(loop for i from 0
	      for c = (read-char s nil nil)
	      until (not c)
	      do
		 (when (>= i ns) (expand-buffer))
		 (when (>= i max-length)
		   (error "ERROR in read-file-as-string: file longer than MAX-LENGTH=~A" max-length))
		 (setf (aref string i) c)
		 (incf k)))
      (subseq string 0 k))))


(defun read-file-as-octets (file &key (max-length #.(expt 2 20)))
  "Read a file as one vector of octets."
  (let* ((ns 1024)
	 (bytes (make-array ns :element-type '(unsigned-byte 8)))
	 (k 0))
    (declare (type fixnum ns k)
	     (type (simple-array (unsigned-byte 8)) bytes))
    (flet ((expand-buffer ()
	     (let ((new-bytes (make-array (* 2 ns) :element-type '(unsigned-byte 8))))
	       (loop for i of-type fixnum below ns
		     do (setf (aref new-bytes i) (aref bytes i)))
	       (setf bytes new-bytes)
	       (setf ns (length new-bytes)))))
      (with-open-file (s file :element-type '(unsigned-byte 8))
	(loop for i from 0
	      for b = (read-byte s nil nil)
	      until (not b)
	      do
		 (when (>= i ns) (expand-buffer))
		 (when (>= i max-length)
		   (error "ERROR in read-file-as-octets: file longer than MAX-LENGTH=~A"
			  max-length))
		 (setf (aref bytes i) b)
		 (incf k)))
      (subseq bytes 0 k))))	    


(defun read-file-as-line-list (file &key (split-chars nil) (strict nil)
				      (trim-carriage-return t))
"Return a FILE as a list of lines; if SPLIT-CHARS is non-nil, then
split each line on characters in SPLIT-CHAR.    STRICT means to allow
zero-length subsequences if splitting.  TRIM-CARRIAGE-RETURN means
eliminate #\CR = ^M from end."
  (setf file (generic-file-to-input-stream file))
  (unwind-protect
      (let ((list nil)
	    (line nil))
	(loop 
	 (setf line (read-line file nil nil))
	  (if (not line) (return (nreverse list)))
	  (when (and trim-carriage-return ;; can have ^M at end
		     (char= (aref line (1- (length line))) #\cr))
	    (setf line (string-trim #(#\cr) line)))
	  (if split-chars
	     (setf line (string-utils:split-string line split-chars
						   :strict strict)))
	  (push line list))) 
    (close file)))


(defun read-object-from-file (file &key (readable-arrays t))
  "Read one Lisp object from a file."
  (with-open-file (s file)
    (if readable-arrays
	(readable-arrays:with-readable-arrays 
	  (read s))
	(read s))))

(defun read-object-list-from-file (file &key (readable-arrays t))
  "Read Lisp objects from file until we hit end, and return as a list."
  (with-open-file (s file)
    (loop for eof = (gensym "eof-thingy")
	  for thing =  (if readable-arrays
			   (readable-arrays:with-readable-arrays 
			     (read s nil eof))
			   (read s nil eof))
	  until (eq thing eof)
	  collect thing)))




;; go thorough the lines of FILE, assigning them to VAR, and
;; splitting according to STRICT and SPLIT-CHARS
(defmacro do-file-lines ((var file  &key (split-chars nil)
			     (strict nil)) &body body)
  (let ((f (gensym))
	(b (gensym)))
    `(let ((,var nil)
	   (,f (generic-file-to-input-stream ,file))
	   (,b nil))
       (unwind-protect
	   (loop
	    (setf ,var (read-line ,f nil nil))
	    (if (not ,var) (return ,b))
	    ,(if split-chars
		 `(setf ,var (string-utils:split-string ,var ,split-chars
							:strict ,strict)))
	    (setf ,b
		  (progn ,@body)))
	 (close ,f)))))
	 


(defun file-count-lines (infile)
  "Count lines in INFILE by counting #\LINEFEED chars."
  (declare (optimize speed))
  ;; using a buffer is about 2x faster than read-char
  (flet ((count-lines-stream (s)
	   (loop with nbuf = #.(expt 2 9) ;; bigger buffer isn't faster
		 with buf = (make-string nbuf)
		 with nlines of-type fixnum = 0
		 for nread = (read-sequence buf s)
		 do
		    (loop for i of-type fixnum below nread
			  when (char= (aref buf i) #\lf)
			    do (incf nlines))
		    (when (< nread nbuf)  (return nlines)))))
    (if (streamp infile)
	(count-lines-stream infile)
	(with-open-file (s infile :direction :input :element-type 'character)
	  (count-lines-stream s)))))
    

(defun getpid ()
  #+cmu (unix:unix-getpid)
  #+sbcl (sb-posix:getpid)
  #+openmcl (ccl::getpid)
  #-(or cmu sbcl openmcl) 9999
  )

(defvar *tempfile-index* 1) ;; not thread safe
(defun make-tempfile-name (&key (prefix "/tmp/file-io-tmp"))
  "Make a tempfile name, iterating until one is found that doesn't exist"
  (loop
    ;; use UNDERSCORES as separator because weirdness can happen if
    ;; Lisp pathname merges treat this as an extension
    for filename = (format nil "~A_~D_~D_~D" 
			   prefix (getpid) (get-universal-time)
			   (incf *tempfile-index*))
    until (not (probe-file filename))
    finally (return filename)))

(defun open-tempfile (&key (prefix "./file-io-tmp")
			   (element-type 'base-char))
  "Open a new tempfile, iterating until one
is found that doesn't exist. Return (values stream filename)"
  (loop
    ;; ntries loops over the possibility that the file does not exist,
    ;; but it was created by someone else in the millisecond between
    ;; probe-file and open
    with ntries = 1
    for filename = (make-tempfile-name :prefix prefix)
    for does-not-exist = (not (probe-file filename))
    for stream = (when does-not-exist
		   (incf ntries)
		   (ignore-errors (open filename
					:direction :io 
					:if-exists :error
					:element-type element-type
					:if-does-not-exist :create)))
    when (> ntries 100)
      do (error "Cannot create temporary file with prefix ~A" prefix)
   until stream
    finally (return (values stream filename))))

(defmacro with-open-tempfile ((stream &key (prefix "./file-io-tmp")
					(delete t)
 					(element-type '(quote base-char)))
 			      &body body)
  "Run BODY with a tempfile, similar to OPEN-TEMPFILE.  If DELETE
is set (as default), delete the file at end.   Return the tempfile name."
  `(let ((,stream nil)
	 (%filename nil)
	 (%delete ,delete))
     (unwind-protect
	  (progn
	    (multiple-value-setq (,stream %filename)
	      (open-tempfile :prefix ,prefix
 			     :element-type ,element-type))
	    (progn ,@body)
	    %filename) ;; return filename
       (when %delete
	 (delete-file %filename)))))
       
	 
	 
	 
     

  


(defun file-basename (filename)
  "Return the basename (name minus suffix) of file"
  (let* ((n/ (position #\/ filename :from-end t))
	 (ndot (position #\. filename :from-end t)))
    (cond ((not ndot)
	   filename)
	  ((and n/ (< ndot n/)) ;; the directory has the dot
	   filename)
	  (t
	   (subseq filename 0 ndot)))))

(defun file-suffix (filename)
  "Return the suffix of a file, defined as the part after the dot, or
NIL if no suffix."
    (let* ((n/ (position #\/ filename :from-end t))
	   (ndot (position #\. filename :from-end t)))
    (cond ((not ndot)
	   nil)
	  ((and n/ (< ndot n/)) ;; the directory has the dot
	   nil)
	  (t
	   (subseq filename (1+ ndot))))))



(defun file-minus-dir (filename)
  "Return a file name minus a leading directory portion, by searching for a / character"
  (let ((n/ (position #\/ filename :from-end t)))
    (if (not n/) 
	filename
	(subseq filename (1+ n/)))))

(defun dir-of-file (filename)
  "Return the directory component of a file"
  (let ((n/ (position #\/ filename :from-end t)))
    (if (not n/) 
	""
	(subseq filename 0 (1+ n/)))))
	


(defun copy-file (infile outfile &key (overwrite nil) (block-size 262144))
  "Copy file INFILE to OUTFILE. If OVERWRITE is true, then clobber any output file
with the same name.  If namestrings of INFILE and OUTFILE are EQUALP, then do nothing."
  (when (and (probe-file outfile) (not overwrite))
    (error "Output file ~A exists and OVERWRITE is NIL" outfile))
  ;;
  (when (not (equalp (parse-namestring infile) (parse-namestring outfile)))
    (with-open-file (sin infile :direction :input :element-type '(unsigned-byte 8))
      (with-open-file (sout outfile :direction :output
			    :if-does-not-exist :create 
			    :if-exists (if overwrite :supersede :error)
			    :element-type '(unsigned-byte 8))
	(loop 
	 with buff = (make-array block-size :element-type '(unsigned-byte 8))
	 for n = (read-sequence buff sin)
	 until (zerop n)
	 do (write-sequence buff sout :end n))))))


(defun full-namestring (file &key (create nil))
  "Return the full path of FILE, and CREATE it if this keyword is set.  If 
file does not exist, and CREATE is NIL, return NIL.

WARNING: this expands symlinks."
  (or 
   (and (probe-file file)
	(namestring (truename file)))
   (and create
	(ignore-errors
	 (with-open-file (ff file :direction :output
				  :if-does-not-exist :create)
	    t))
	(namestring (truename file)))))

(defun full-namestring/no-symlink-expand (file)
  "Return the full path to a file, but returning the symlink if this a
symlink, rather than expanding the symlinks in the path.

For example if ./foo/bar/zip.dat is a link to /A/B/zork.dat but
./foo/bar is a real directory /blip/foo/bar, then this function
returns /blip/foo/bar/zork.dat."
  
  (when (full-namestring file)
    (let* ((dir (dir-of-file file))
	   (full-dir (full-namestring dir))
	   (file-only (file-minus-dir file))
	   (full-file (concatenate 'string
				   (string-right-trim "/" full-dir)
				   "/"
				   file-only)))
      (when (file-io:full-namestring full-file)
	full-file)))) ;; can return NIL
	

(defun make-filename-absolute-using-default-pathname-defaults (file)
  "Given a filename (path or string), either
  1. return it unchanged (as a string) if it is absolute relative to /
  2. append CL:*DEFAULT-PATHNAME-DEFAULTS* if it is relative.
This routine is useful for forcing unix routines to to use the
default Lisp current directory."
  (let* ((ns (namestring file))
	 (directory (pathname-directory ns)))
    (cond ((zerop (length ns))
	   (error "Zero length file."))
	  ((eq (first directory) :absolute)
	   ns)
	  ((concatenate 
	    'string 
	    (namestring cl::*default-pathname-defaults*)
	    ns)))))
	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-lock-file ((filename
			   &key
			     (timeout nil)
			     (on-timeout :error)
			     (sleep-time 0.1)
			     (dont-lock nil))
			  &body body)
  "Use a file as a locking mechanism to keep process from interfering
with each other, or using the same resource.

If TIMEOUT (seconds) is given then after a time TIMEOUT of not being
able to grab the lock file, ON-TIMEOUT is performed.

ON-TIMEOUT can be :ERROR, which throws an error, or :GRAB, which
recklessly deletes the old lock file and gets access to the resource.

If DONT-LOCK is set, then simply run body inside a PROGN - this is useful
for recursive use of lockfiles, to prevent locks inside locks."
  
  `(let ((%the-filename ,filename)
	 (%the-timeout ,timeout)
	 (%the-sleep-time , sleep-time))
     (flet ((%run-the-body ()
	      (progn ,@body)))
       (if ,dont-lock ;; don't do any locking
	   (%run-the-body)
	   (progn
	     (loop with start-time = (get-universal-time)
		   for str = (open (ensure-directories-exist %the-filename)
				   :direction :output
				   :if-exists nil ;; no file created; NIL is returned
				   :if-does-not-exist :create)
		   when str
		     return (close str)
		   when (not str)
		     ;; when timing out, take appropriate action
		     do
			(when (and
			       %the-timeout
			       (> (- (get-universal-time) start-time)
				  %the-timeout))
			  (cond ((eq ,on-timeout :grab)
				 (delete-file %the-filename))
				((eq ,on-timeout :error)
				 (error "Lock file ~A stayed locked for TIMEOUT=~A sec~%"
					%the-filename %the-timeout)))
			  (sleep %the-sleep-time)))
	     
	     (unwind-protect
		  (%run-the-body)
	       (delete-file %the-filename)))))))

(defun write-object-to-file (object file &key (if-exists :error))
  "Write a Lisp OBJECT (any printable thing) to FILE, respecting IF-EXISTS."
  (with-open-file (s file :direction :output :if-exists if-exists)
    (write object :stream s)))
