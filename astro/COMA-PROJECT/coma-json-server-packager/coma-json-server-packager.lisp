

(defpackage coma-json-server-packager
  (:use #:cl)
  (:export
   #:export-coma-json-server 
   ))

(in-package coma-json-server-packager)

(defparameter *lisp-lib-dir*
  (pathname
   (concatenate 'string
		(string-right-trim
		 "/"
		 (or (uiop:getenv "LISP_LIB")
		     (error "LISP_LIB environment var not defined.")))
		"/")))


;; found in a reddit post
(defun list-asdf-dependencies (system)
  "Return all the explicit dependencies of ‘system’ as a flat list. Note
that systems can have implicit dependencies, such as foreign libraries and
functions."
  (let ((table (make-hash-table :test #'equal))
        result)
    (labels
      ((resolve (sys)
         (let ((result
                 (if (consp sys)
                     (asdf/find-component::resolve-dependency-spec nil sys)
                     sys)))
           (if (typep result 'asdf:component)
               (asdf:component-name result)
               result)))
       (sub-deps (sys func)
         (let ((real-sys (resolve sys)))
           (when real-sys
             (setf (gethash real-sys table) t)
             (loop with sub-sys
		   for s in (funcall func (asdf:find-system real-sys))
                   when (not (gethash s table))
                     do (setf sub-sys (sub-deps s func)))))))
      (sub-deps system #'asdf:system-depends-on)
      (sub-deps system #'asdf:system-defsystem-depends-on)
      (maphash (lambda (k v) (declare (ignore v)) (push k result)) table)
      result)))


(defstruct cjsdep
  system-name
  system
  is-quicklisp
  pathname
  in-libdir
  relative-path ;; to lisp-lib
  )

(defparameter *gitignore*
  "SYSTEM-DATA
*.o
*~
#*
*.fasl
dynamic-libraries
")
  


(defparameter *cjs-dependencies* (list-asdf-dependencies
				  (asdf:find-system "coma-json-server")))


;; if PATH is inside BASE-PATH, buil the relative path, else return NIL
(defun compute-relative-path (path base-path)
  (let* ((d1 (pathname-directory path))
	 (d2 (pathname-directory base-path))
	 (nb (length d2)))
    (when (not (and (eq (first d1) :absolute)
		    (eq (first d2) :absolute)))
      (error "Both path ~A and ~A are not absolute."
	     path base-path))
    (when (equalp (subseq d1 0  nb) d2)
      (make-pathname :directory (cons :relative (subseq d1 nb))
		     :name (pathname-name path)
		     :type (pathname-type path)))))
				    

(defun construct-cjsdep (system-name)
  (let* ((sys (or (asdf:find-system system-name)
		  (error "Cannot find ASDF dependency ~A" system-name)))
	 (path (asdf:component-pathname sys)))
    (when path ;; UIOP is a virtal system?
      (make-cjsdep
       :system-name system-name
       :system sys
       :pathname path
       :is-quicklisp (not (not (search "quicklisp"
				       (namestring path)
				       :test 'char-equal)))
       :relative-path (compute-relative-path path (truename *lisp-lib-dir*))))))
     

;; thank you Google Gemini - cl-fad probably won't do this
(defun rsync-dirs (source-dir dest-dir)
  "Recursively copies SOURCE-DIR to DEST-DIR, preserving symbolic links.
       Uses the 'cp -a' command for this purpose."
      (let ((command (format nil "rsync -a ~a ~a"
                             (uiop:native-namestring source-dir)
                             (uiop:native-namestring dest-dir))))

	;;(format t "~A~%" command)
	(ensure-directories-exist
	 (format nil "~A/IGNORE" (namestring  dest-dir)))
        (uiop:run-program command
			  :output *standard-output*
			  :error-output *error-output*)))

(defun strip-final-dir (pathstring)
  (let* ((fulldir (pathname-directory (pathname pathstring)))
	 (dir-minus-one
	   (subseq fulldir 0 (1- (length fulldir)))))
  (make-pathname
   :directory dir-minus-one)))

(defun export-coma-json-server (output-dir)
 
  (let* ((extra-dependencies '("sbcl-scripting"))
	 (all-dependencies (append *cjs-dependencies* extra-dependencies))
	 (deps (remove nil (mapcar 'construct-cjsdep all-dependencies)))
	 (ql-deps (loop for dep in deps
			when (cjsdep-is-quicklisp dep)
			  collect dep))
	 (other-deps (loop for dep in deps
			   when (not (cjsdep-is-quicklisp dep))
			     collect dep))
	 (outdir (string-right-trim "/" (namestring output-dir)))
	 (qldir (format nil "~A/quicklisp-systems/" outdir)))
    
    (ensure-directories-exist (format nil "~A/IGNORE" outdir))
    (when ql-deps
      (ensure-directories-exist (format nil "~A/IGNORE" qldir))
      (ql:bundle-systems (mapcar 'cjsdep-system-name ql-deps)
			 :to qldir))

    (loop for dep in other-deps
	  for source =  (string-right-trim "/" (namestring (cjsdep-pathname dep)))
	  for dest = (strip-final-dir
		      (format nil "~A/~A" outdir (cjsdep-relative-path dep)))
	  do (rsync-dirs source dest))

    (with-open-file (s (format nil "~A/.gitignore" outdir) :direction :output
		       :if-does-not-exist :create :if-exists :overwrite)
      (write-string *gitignore* s))

    (cl-fad:delete-directory-and-files
     (format nil "~A/astro/phot-calib/phot-calib-tests" outdir))

    (flet ((do-link (file)
	     (let ((link-command
		     (format nil "ln -sf ./astro/COMA-PROJECT/Scripts/~A ~A/~A"
			     file (namestring outdir) file)))
	       (uiop:run-program link-command
				 :output *standard-output*
				 :error-output *error-output*))))
      (do-link "coma-json-server")
      (do-link "coma-json-server.lisp"))
    
    (loop for thing in '("slime" "asdf" "INIT-FILES")
	  do (rsync-dirs
	      (format nil "~A~A" *lisp-lib-dir* thing)
	      outdir))))


      
