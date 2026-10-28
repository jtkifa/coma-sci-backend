
(in-package pgplot)
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMUCL/SBCL specific FFI stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bind the functions of the pgplot library to make
;; a set of 'raw' functions, then make a higher level
;; set of functions that use the low level functions



(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *int-type* :long))

;; sometimes we see exceptions in foreign code
(defmacro with-float-traps-masked ((&key all invalid inexact overflow underflow 
				      divide-by-zero) &body body )
  "Mask float traps with keywords 
     :ALL :INVALID :INEXACT :OVERFLOW :UNDERFLOW :DIVIDE-BY-ZERO"
  #+sbcl
  `(sb-int:with-float-traps-masked
       ,(remove nil
		(list (if (or inexact   all)  :inexact)
		      (if (or invalid   all)  :invalid)
		      (if (or overflow  all)  :overflow)
		      (if (or underflow all)  :underflow)
		      (if (or divide-by-zero all) :divide-by-zero)))
     ,@body)
  ;;
  #+ccl
  (let ((old-fpu-mode-var (gensym "fpu-mode")))
    `(let ((,old-fpu-mode-var (ccl:get-fpu-mode)))
	(unwind-protect
	     (progn (ccl:set-fpu-mode :overflow ,(or overflow all)
				      :underflow ,(or underflow all)
				      :division-by-zero ,(or divide-by-zero all)
				      :invalid ,(or invalid all)
				      :inexact ,(or inexact all))
		    ,@body)
	  (apply #'ccl:set-fpu-mode ,old-fpu-mode-var))))
  ;;
  #-(or ccl sbcl)
  (declare (ignore all invalid inexact overflow underflow 
		   divide-by-zero))
  #-(or ccl sbcl ecl)
  (progn
    (format t "Warning - (with-float-traps-masked-macro) -
No float trap masking defined for this lisp implementation.  Not masking.~%")
    `(progn ,@body)))
     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define alien routines in library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sbdefine-alien-routine ("cpgarro" pgarro-raw) :void
  (x1 :float) (y1 :float) (x2 :float) (y2 :float))

(sbdefine-alien-routine ("cpgask" pgask-raw) :void
  (flag :long))

;; not in giza
(sbdefine-alien-routine ("cpgaxis" pgaxis-raw) :void
  (opt :string)
  (x1 :float)(y1 :float)(x2 :float)(y2 :float)(v1 :float)(v2 :float)
  (step :float) (nsub :long) (dmajl :float) (dmajr :float)
  (fmin :float)(disp :float)
  (orient :float))

(sbdefine-alien-routine ("cpgband" pgband-raw) :long
  (mode :long) (posn :long) (xref :float) (yref :float)
  (x :float :out)
  (y :float :out)
  (ch :uchar :out))

(sbdefine-alien-routine ("cpgbbuf" pgbbuf-raw) :void)

;; PGBEG absent (use PGCLOS instead)

(sbdefine-alien-routine ("cpgbin" pgbin-raw) :void
  (nbin :long)
  (x    (* :float) :in)
  (data (* :float) :in)
  (center :long))

(sbdefine-alien-routine ("cpgbox" pgbox-raw) :void
  (xopt :string) (xtick :float) (nxsub :long)
  (yopt :string) (ytick :float) (nysub :long))

(sbdefine-alien-routine ("cpgcirc" pgcirc-raw) :void
  (xcent :float) (ycent :float) (radius :float))

(sbdefine-alien-routine ("cpgclos" pgclos-raw) :void)

(sbdefine-alien-routine ("cpgconb" pgconb-raw) :void
  (idim :long) (jdim :long) (i1 :long) (i2 :long) (j1 :long) (j2 :long) (nc :long)
  (a     (* :float) :in) ;; 2d array
  (c     (* :float) :in) 
  (blank (* :float) :in) )

;; not in giza
(sbdefine-alien-routine ("cpgconf" pgconf-raw) :void
  (idim :long) (jdim :long) (i1 :long) (i2 :long) (j1 :long) (j2 :long) 
  (a     (* :float) :in) ;; 2d array
  (c1 :float) (c2 :float)
  (tr (* :float) :in) )


(sbdefine-alien-routine ("cpgconl" pgconl-raw) :void
  (a     (* :float) :in) ;; 2d array
  (idim :long) (jdim :long)
  (i1 :long) (i2 :long) (j1 :long) (j2 :long)
  (c   :float :in) ;; just a single number
  (tr (* :float) :in)
  (label :string)
  (intval :long)
  (minint :long))


(sbdefine-alien-routine ("cpgcons" pgcons-raw) :void
  (a     (* :float) :in) ;; 2d array
  (idim :long) (jdim :long)
  (i1 :long) (i2 :long) (j1 :long) (j2 :long)
  (c     (* :float) :in) ;; 1d array
  (nc :long)
  (tr (* :float) :in) )

(sbdefine-alien-routine ("cpgcont" pgcont-raw) :void
  (a     (* :float) :in) ;; 2d array
  (idim :long) (jdim :long)
  (i1 :long) (i2 :long) (j1 :long) (j2 :long)
  (c     (* :float) :in) ;; 1d array
  (nc :long)
  (tr (* :float) :in) )


;; PGCONX absent

(sbdefine-alien-routine ("cpgctab" pgctab-raw) :void
  (l (* :float) :in)
  (r (* :float) :in)
  (g (* :float) :in)
  (b (* :float) :in)
  (nc :long)
  (contra :float)
  (bright :float))




(sbdefine-alien-routine ("cpgcurs" pgcurs-raw) :void
  (x :float :in-out)   (y :float :in-out)   (c :uchar :out))



(sbdefine-alien-routine ("cpgdraw" pgdraw-raw) :void
  (x :float) (y :float))

(sbdefine-alien-routine ("cpgebuf" pgebuf-raw) :void)

(sbdefine-alien-routine ("cpgend" pgeend-raw) :void)

(sbdefine-alien-routine ("cpgenv" pgenv-raw) :long
  (xmin :float)
  (xmax :float)
  (ymin :float)
  (ymax :float)
  (just :long)
  (axis :long))


(sbdefine-alien-routine ("cpgeras" pgeras-raw) :void)

;; PGERR1 absent


(sbdefine-alien-routine ("cpgerrb" pgerrb-raw) :void
  (dir :long)
  (n :long)
  (x (* :float) :in)
  (y (* :float) :in)
  (e (* :float) :in)
  (tt :float))
  

;; PGERRX absent
;; PGERRY absent
;; PGETXT absent
;; PGFUNT absent (impossible)
;; PGFUNX absent (impossible)
;; PGFUNY absent (impossible)

(sbdefine-alien-routine ("cpggray" pggray-raw) :void
  (a     (* :float) :in) ;; 2d array
  (idim :long) (jdim :long) (i1 :long) (i2 :long) (j1 :long) (j2 :long) 
  (fg :float) (bg :float)
  (tr (* :float) :in) )

;; PGHI2D absent

(sbdefine-alien-routine ("cpghist" pghist-raw) :void
  (n :long)
  (data     (* :float) :in)
  (datmin :float) (datmax :float)
  (nbin :long) (pgflag :long))

(sbdefine-alien-routine ("cpgiden" pgiden-raw) :void)

(sbdefine-alien-routine ("cpgimag" pgimag-raw) :void
  (a     (* :float) :in) ;; 2d array
  (idim :long) (jdim :long) (i1 :long) (i2 :long) (j1 :long) (j2 :long) 
  (a1 :float) (a2 :float)
  (tr (* :float) :in) )

(sbdefine-alien-routine ("cpglab" pglab-raw) :long
  (xlabel :string)
  (ylabel :string)
  (toplabel :string))


;; PGLCUR absent

(sbdefine-alien-routine ("cpgldev" pgeldev-raw) :void)


(sbdefine-alien-routine ("cpglen" pglen-raw) :void
  (units :long) (string :string)
  (xl :float :out) (yl :float :out))
  
(sbdefine-alien-routine ("cpgline" pgline-raw)  :void
  (npt :long)
  (xvec (* :float) :in)
  (yvec (* :float) :in))

(sbdefine-alien-routine ("cpgmove" pgmove-raw) :void
  (x :float) (y :float))

(sbdefine-alien-routine ("cpgmtxt" pgmtxt-raw) :void
  (side :string)
  (disp :float) (coord :float) (fjust :float)
  (text :string))

;; PGNCUR absent
;; PGNUMB absent
;; PGOLIN absent

(sbdefine-alien-routine ("cpgopen" pgopen-raw) :long
  (device :string))

(sbdefine-alien-routine ("cpgpage" pgpage-raw) :void)

(sbdefine-alien-routine ("cpgpanl" pgpanl-raw) :void
  (ix :long) (iy :long))

(sbdefine-alien-routine ("cpgpap" pgpap-raw) :void
  (width :float) (aspect :float))

(sbdefine-alien-routine ("cpgpixl" pgpixl-raw) :void
  (idim :long) (jdim :long) (i1 :long) (i2 :long) (j1 :long) (j2 :long)
  (ia (* :long) :in)
  (x1 :float) (x2 :float) (y1 :float) (y2 :float))


(sbdefine-alien-routine ("cpgpoly" pgpoly-raw) :void
  (n :long)
  (xpts  (* :float) :in)
  (ypts  (* :float) :in))
  

(sbdefine-alien-routine ("cpgpt" pgpt-raw) :void
  (npt :long)
  (xvec (* :float) :in)
  (yvec (* :float) :in)
  (ptye :long))

;; PGPNTS absent
;; PGPT1 absent

(sbdefine-alien-routine ("cpgqpos" pgqpos-raw) :void
  (x :float :out) (y :float :out))

(sbdefine-alien-routine ("cpgptxt" pgptxt-raw) :void
  (x :float) (y :float) (angle :float) (fjust :float)
  (text :string))

;; PGQAH absent
;; PGQCF absent
;; PGQCH absent

(sbdefine-alien-routine ("cpgqci" pgqci-raw) :void
  (npt :long :out))

(sbdefine-alien-routine ("cpgqcol" pgqcol-raw) :void
  (nlo :long :out)
  (nnhi :long :out))


(sbdefine-alien-routine ("cpgqcir" pgqcir-raw) :void
  (icilo :long :out)
  (icihi :long :out))
;; PGQCLP absent

(sbdefine-alien-routine ("cpgqcr" pgqcr-raw) :void
  (ci :long :in)
  (cr  :float :out) (cg  :float :out) (cb  :float :out))

;; PGQCS absent
;; PGQDT absent
;; PGQFS absent
;; PGQHS absent
;; PGQINF absent
;; PGQITF absent
;; PGQLS absent 
;; PGQLW absent
;; PGQNDT absent 
;; PGQPOS absent
;; PGQTBG absent


(sbdefine-alien-routine ("cpgqtxt" pgqtxt-raw) :void
  (x :float) (y :float) (angle :float) (fjust :float)
  (text :string)
  (xbox (* :float) :in)
  (ybox (* :float) :in))



	      
;; PGQVP absent
  
(sbdefine-alien-routine ("cpgqvsz" pgqvsz-raw) :void
  (units :long)
  (x1 :float :out) (x2 :float :out)
  (y1 :float :out) (y2 :float :out))


;; PGQWIN absent

(sbdefine-alien-routine ("cpgrect" pgrect-raw) :void
  (x1 :float) (x2 :float) (y1 :float) (y2 :float))

;; PGRND absent
;; PGRNGE absent

(sbdefine-alien-routine ("cpgsah" pgsah-raw) :void
  (fs :long) (angle :float) (barb :float))

(sbdefine-alien-routine ("cpgsave" pgsave-raw) :void)

(sbdefine-alien-routine ("cpgunsa" pgunsa-raw) :void)

(sbdefine-alien-routine ("cpgscf" pgscf-raw) :void
  (font :long))

(sbdefine-alien-routine ("cpgsch" pgsch-raw) :void
  (size :float))

(sbdefine-alien-routine ("cpgsci" pgsci-raw) :void
  (ci :long))

(sbdefine-alien-routine ("cpgscir" pgscir-raw) :void
  (icilo :long) (icihi :long))

(sbdefine-alien-routine ("cpgsclp" pgsclp-raw) :void
  (state :long))

(sbdefine-alien-routine ("cpgscr" pgscr-raw) :void
  (ci :long)
  (cr :float) (cg :float) (cb :float))

;; not in giza
(sbdefine-alien-routine ("cpgscrl" pgscrl-raw) :void
  (dx :float) (dy :float))

;; PGSCRN absent

(sbdefine-alien-routine ("cpgsfs" pgsfs-raw) :void
  (fs :long))

(sbdefine-alien-routine ("cpgshls" pgshls-raw) :void
  (ci :long)
  (ch :float) (cl :float) (cs :float))

(sbdefine-alien-routine ("cpgshs" pgshs-raw) :void
  (angle :float) (sepn :float) (phase :float))

;; not in giza
(sbdefine-alien-routine ("cpgsitf" pgsitf-raw) :void
  (itf :long))

(sbdefine-alien-routine ("cpgslct" pgslct-raw) :void
  (id :long))

(sbdefine-alien-routine ("cpgsls" pgsls-raw) :void
  (ls :long))

(sbdefine-alien-routine ("cpgslw" pgslw-raw) :void
  (lw :long))

(sbdefine-alien-routine ("cpgstbg" pgstbg-raw) :void
  (tcbi :long))

(sbdefine-alien-routine ("cpgsubp" pgsubp-raw) :void
  (nxsub :long) (nysub :long))

(sbdefine-alien-routine ("cpgsvp" pgsvp-raw) :void
  (xleft :float) (xright :float) (ybot :float) (ytop :float))

(sbdefine-alien-routine ("cpgswin" pgswin-raw) :void
  (x1 :float) (x2 :float) (y1 :float) (y2 :float))

(sbdefine-alien-routine ("cpgwedg" pgwedg-raw) :void
  (side :string) (disp :float) (width :float) (fg :float) (bg :float) (label :string))

;; PGTXT is the more general version that we use, not PGTEXT
(sbdefine-alien-routine ("cpgtext" pgtext-raw) :void
  (x :float) (y :float)	(text :string))

;; not in giza
(sbdefine-alien-routine ("cpgtick" pgtick-raw) :void
  (x1 :float) (y1 :float)
  (x2 :float) (y2 :float)
  (v  :float)
  (tikl  :float) (tikr  :float) (disp :float) (orient :float)
  (str :string))



  
  

(sbdefine-alien-routine ("cpgupdt" pgupdt-raw) :void)

(sbdefine-alien-routine ("cpgvect" pgvect-raw) :void
  (a (* :float) :in) ;; 2d array
  (b (* :float) :in) ;; 2d array
  (idim :long) (jdim :long)
  (i1 :long) (i2 :long) (j1 :long) (j2 :long)
  (c :float :in) ;; scale factor for arrows
  (nc :long) ;; how vector is positioned on coords
  (tr (* :float) :in)
  (blank :float :in) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; all the following are 'non-standard aliases' and 
;;;;;;;;; will never be present in pgplot-cl
;;
;; PGVECT absent
;; PGVSTD absent
;; PGWEDG absent
;; PGWNAD absent
;; PGADVANCE absent
;; PGBEGIN absent
;; PGCURSE absent
;; PGLABEL absent
;; PGMTEXT absent
;; PGNCURSE absent
;; PGPAPER absent
;; PGPOINT absent
;; PGPTEXT absent
;; PGVPORT absent
;; PGVSIZE absent
;; PGVSTAND absent
;; PGWINDOW absent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coercion to single float - elaborate form is to prevent underflows
;; that seem to occur on some (Sun..and?) architectures
;; it isn't really clear that this is the right way to do this.
;; -- it would be nice if pgplot supported double floats instead of single
(declaim (inline to-single-float))
(defun to-single-float (x)
  (cond ((< x most-negative-single-float)
	 most-negative-single-float)
	((> x most-positive-single-float)
	 most-positive-single-float)
	;; we use  -normalized because plain least-postitive-float give
	;; underflows on some architectures (dangit..)
	((and (< x least-positive-normalized-single-float)
	      (> x least-negative-normalized-single-float))
	 0.0s0)
	(t
	 (coerce x 'single-float))))


	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coerce a vector into type (ARRAY SINGLE-FLOAT ...) because
;; that is what pgplot expects 
(defun vector-to-sa-sf (vec)
  (typecase vec
    ((simple-array single-float)
     vec)
    ;;
    (t ;; else duplicate into a new array
     (let ((u (make-array (length vec) :element-type 'single-float :initial-element 0.0)))
       (dotimes (i (length vec))
	 (if (not (numberp (aref vec i)))
	     (error
	      (format
	       t
	       "trying to coerce a non-numerical vector into single-float vector : bad element ~A at index ~A"
	       (aref vec i) i)))
	 ;; this elaborate coercion to single float prevents
	 ;; over/underflows on some (Sun and ...?) architectures
	 (setf (aref u i) (to-single-float (aref vec i))))
       u))))



;; lots of special cases for speed with big images
(defun 2d-array-to-sa-sf-copy (a)
  (let* ((n (array-total-size a))
	 (v  (make-array n :element-type 'single-float :initial-element 0.0)))
    (declare (type fixnum n)
	     (type (simple-array single-float (*)) v))
    
    (cond ((typep a '(simple-array single-float (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) (row-major-aref a i)))))
	  ((typep a '(simple-array double-float (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (to-single-float (row-major-aref a i))))))
	  ((typep a '(simple-array (unsigned-byte 8) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
	  ((typep a '(simple-array (signed-byte 8) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
	  ((typep a '(simple-array (unsigned-byte 16) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
	  ((typep a '(simple-array (signed-byte 16) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
 	  ((typep a '(simple-array (unsigned-byte 32) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
	  ((typep a '(simple-array (signed-byte 32) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
	  ((typep a '(simple-array (unsigned-byte 64) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
	  ((typep a '(simple-array (signed-byte 64) (* *)))
	   (locally (declare (optimize speed))
	     (loop for i below n 
		do (setf (aref v i) 
			 (float (row-major-aref a i) 1.0)))))
	  (t ;; general slow case
	   (loop for i below n 
	      do (setf (aref v i) 
		       (to-single-float (row-major-aref a i)))))
	  )
    v))
	  
	  
	   



;; convert a 2d array into a single float array. With sbcl or cmucl,
;; use the intrinsic 1d array inside a 2d array if possible.
;; Otherwise, copy.
(defun 2d-array-to-sa-sf (a)
  (2d-array-to-sa-sf-copy a))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro to coerce a set of variables to a type
(defmacro ensure-type (type &rest var-list)
  (let ((the-list '()))
    (dolist (var var-list)
      (setq the-list 
	    (append the-list 
		     (list 
		      (list 'setq var (list 'coerce var type))))))
    (cons 'progn the-list)))


;; (defmacro without-gcing (&body body)
;;   #+cmu `(ext::without-gcing ,@body)
;;   #+sbcl `(sb-ext::without-gcing ,@body))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now a layer of functions that interfaces between the "raw" 
;; ones and the high level CLOS ones -- often these are identical
;; to the raw ones  (some of these could be macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define some locking mechanisms because pgplot is not 
;; thread safe
(defmacro %make-lock (&key name)
  #+ccl  `(ccl:make-lock  ,name)
  #+sbcl `(sb-thread:make-mutex :name ,name)
  #-(or sbcl ccl) :dummy-lock)

(defvar *pgplot-lock* (%make-lock :name "pgplot-lock"))

(defmacro %with-pgplot-lock (&body body)
  #+ccl `(ccl:with-lock-grabbed (*pgplot-lock*) ,@body) 
  #+sbcl `(sb-thread:with-recursive-lock (*pgplot-lock*) ,@body)
  #-(or sbcl ccl) `(progn ,@body))

;; a wrapper for defun that ensures locking
(defmacro defunL (fname (&rest args) &body body)
  (let ((real-body body)
	(declare-list nil)
	(defun-doc nil))
    (when (stringp (first body))
      (setf defun-doc (car real-body))
      (setf real-body (cdr real-body)))
    (when (eq (caar real-body) 'declare)
      (setf declare-list (car real-body))
      (setf real-body (cdr real-body)))
    
  `(defun ,fname (,@args) 
    ,@(append
       (if defun-doc (list defun-doc))
       (if declare-list (list declare-list))
       (list `(%with-pgplot-lock 
	       ,@real-body))))))

(defmacro defmethodL (fname (&rest args) &body body)
  (let ((real-body body)
	(declare-list nil)
	(defun-doc nil))
    (when (stringp (first body))
      (setf defun-doc (car real-body))
      (setf real-body (cdr real-body)))
    (when (eq (caar real-body) 'declare)
      (setf declare-list (car real-body))
      (setf real-body (cdr real-body)))
    
  `(defmethod ,fname (,@args) 
    ,@(append
       (if defun-doc (list defun-doc))
       (if declare-list (list declare-list))
       (list `(%with-pgplot-lock 
	       ,@real-body))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defunL pgarro (x1 y1 x2 y2)
  (ensure-type 'single-float x1 y2 x2 y2)
  (pgarro-raw x1 y1 x2 y2))

(defunL pgask  (flag) (pgask-raw flag))

(defunL pgbox (xopt xtick nxsub yopt ytick nyxub)
  (ensure-type 'single-float xtick ytick)
  (pgbox-raw xopt xtick nxsub yopt ytick nyxub))

(defunL pgcirc (xcent ycent radius)
  (ensure-type 'single-float xcent ycent radius)
  (pgcirc-raw xcent ycent radius))


(defunL pgclos () (pgclos-raw))

(defunL pgscir (icilo icihi) (pgscir-raw icilo icihi))


(defunL pgcurs (x y)
  (ensure-type 'single-float x y)
  (multiple-value-bind (dummy x y c) (pgcurs-raw x y)
		       (declare (ignore dummy))
		       (values x y c)))

;; uses the good but slow pgcont algorithm, not pgcons
(defunL pgcont (a i1 i2 j1 j2 c n-cont tr)
  (let ((av (2d-array-to-sa-sf a))
	(cv (vector-to-sa-sf c))
	(trv (vector-to-sa-sf tr)))
    (with-arrays-as-foreign-pointers  
     ((av a-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (cv c-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (trv tr-ptr :float :lisp-type single-float :copy-from-foreign nil))
     (pgcont-raw a-ptr (array-dimension a 1) (array-dimension a 0)
		 ;; indices incremented by 1 for fortran
		 (1+ j1) (1+ j2) (1+ i1) (1+ i2) 
		 c-ptr n-cont tr-ptr))))

;; label for 1 contour
(defunL pgconl (a i1 i2 j1 j2 c tr label intval minint)
  (let ((av (2d-array-to-sa-sf a))
	(c (float c 1s0))
	(trv (vector-to-sa-sf tr)))
    (with-arrays-as-foreign-pointers
     ((av a-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (trv tr-ptr :float :lisp-type single-float :copy-from-foreign nil))
     (pgconl-raw a-ptr (array-dimension a 1) (array-dimension a 0)
		 ;; indices incremented by 1 for fortran
		 (1+ j1) (1+ j2) (1+ i1) (1+ i2) 
		 c tr-ptr label intval minint))))


(defunL pgimag (i1 i2 j1 j2 a a1 a2 tr)
  (let ((av (2d-array-to-sa-sf a))
	(trv (vector-to-sa-sf tr)))
    (with-arrays-as-foreign-pointers
     ((av a-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (trv tr-ptr :float :lisp-type single-float :copy-from-foreign nil))
     (pgimag-raw a-ptr
		 (array-dimension a 1) (array-dimension a 0)
		 (1+ j1) (1+ j2) (1+ i1) (1+ i2) 
		 a1 a2 tr-ptr))))

(defunL pgvect (i1 i2 j1 j2 a b c nc tr blank)
  (let ((av (2d-array-to-sa-sf a))	
	(bv (2d-array-to-sa-sf b))
    	(trv (vector-to-sa-sf tr)))
    (with-arrays-as-foreign-pointers
	((av a-ptr :float :lisp-type single-float :copy-from-foreign nil)
	 (bv b-ptr :float :lisp-type single-float :copy-from-foreign nil)
	 (trv tr-ptr :float :lisp-type single-float :copy-from-foreign nil))
      (pgvect-raw a-ptr b-ptr 
		  (array-dimension a 1) (array-dimension a 0)
		  (1+ j1) (1+ j2) (1+ i1) (1+ i2)
		  (float c 1.0)
		  nc
		  tr-ptr
		  (or blank most-positive-single-float)))))
      



(defunL pggray (i1 i2 j1 j2 a fg bg tr)
  (let ((av (2d-array-to-sa-sf a))
	(trv (vector-to-sa-sf tr)))
    (with-arrays-as-foreign-pointers
     ((av a-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (trv tr-ptr :float :lisp-type single-float :copy-from-foreign nil))
     (pggray-raw a-ptr 
		 (array-dimension a 1) (array-dimension a 0)
		 (1+ j1) (1+ j2) (1+ i1) (1+ i2) 
		 fg bg tr-ptr))))


(defunL pgerrb (dir n x y e tt)
  (ensure-type 'single-float tt)
  (let ((xv (vector-to-sa-sf x))
	(yv (vector-to-sa-sf y))
	(ev (vector-to-sa-sf e)))
    (with-arrays-as-foreign-pointers
     ((xv x-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (yv y-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (ev e-ptr :float :lisp-type single-float :copy-from-foreign nil))
     ;;
     (pgerrb-raw dir n x-ptr y-ptr e-ptr tt))))

	
(defunL pgtick (x1 y1 x2 y2 v tikl tikr disp orient str)
  (ensure-type 'single-float x1 y1 x2 y2 v tikl tikr disp orient)
  (pgtick-raw x1 y1 x2 y2 v tikl tikr disp orient str))

(defunL pgenv (xmin xmax ymin ymax just axis)
  (ensure-type 'single-float xmin xmax ymin ymax)
  (pgenv-raw xmin xmax ymin ymax just axis))

(defunL pglab (xlabel ylabel toplabel) (pglab-raw xlabel ylabel toplabel))

(defunL pglen (units string)
  (multiple-value-bind (dummy xl yl) (pglen-raw units string)
		       (declare (ignore dummy))
		       (values xl yl)))

(defunL pgline (npts xvec yvec)
  (let ((xv (vector-to-sa-sf xvec))
	(yv (vector-to-sa-sf yvec))
	(nx (length xvec))
	(ny (length yvec)))
    (if (not (and (<= npts nx) (<= npts ny)))
	(error (format t "pgline: npts=~A nx=~A  ny=~A" npts nx ny)))
    (with-arrays-as-foreign-pointers
     ((xv x-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (yv y-ptr :float :lisp-type single-float :copy-from-foreign nil))
     (pgline-raw npts x-ptr y-ptr))))

    
(defunL pgmove (x y)     (pgmove-raw x y))


(defunL pgopen (dev-str) (pgopen-raw dev-str))

(defunL pgpage ()  (pgpage-raw))

(defunL pgpap (width aspect)
  (ensure-type 'single-float width aspect)
  (pgpap-raw width aspect))

(defunL pgpoly (npts xvec yvec)
  (let ((xv (vector-to-sa-sf xvec))
	(yv (vector-to-sa-sf yvec))
	(nx (length xvec))
	(ny (length yvec)))
    (if (not (and (<= npts nx) (<= npts ny)))
	(error (format t "pgpoly: npts=~A nx=~A  ny=~A" npts nx ny)))
    (with-arrays-as-foreign-pointers
     ((xv x-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (yv y-ptr :float :lisp-type single-float :copy-from-foreign nil))
     (pgpoly-raw npts x-ptr y-ptr))))


(defunL pgpt (npts xvec yvec symbol)
  (let ((xv (vector-to-sa-sf xvec))
	(yv (vector-to-sa-sf yvec))
	(nx (length xvec))
	(ny (length yvec)))
    (if (not (and (<= npts nx) (<= npts ny)))
	(error (format t "pgpt: npts=~A nx=~A  ny=~A" npts nx ny)))
    (with-arrays-as-foreign-pointers
     ((xv x-ptr :float :lisp-type single-float :copy-from-foreign nil)
      (yv y-ptr :float :lisp-type single-float :copy-from-foreign nil))
     (pgpt-raw npts x-ptr y-ptr symbol))))


(defunL pgqcol ()
  (multiple-value-bind (dummy nlo nhi) (pgqcol-raw)
    (declare (ignore dummy))
    (values nlo nhi)))
  

(defunL pgrect (x1 x2 y1 y2)
  (ensure-type 'single-float x1 x2 y1 y2)
  (pgrect-raw x1 x2 y1 y2))


(defunL pgqtxt (x y angle fjust text xbox ybox)
  (ensure-type 'single-float x y angle fjust)
  (ensure-type 'simple-string text)
  (let ((xv (vector-to-sa-sf xbox))
	(yv (vector-to-sa-sf ybox)))
    (with-arrays-as-foreign-pointers
     ((xv x-ptr :float :lisp-type single-float :copy-from-foreign t)
      (yv y-ptr :float :lisp-type single-float :copy-from-foreign t))
     (pgqtxt-raw x y angle fjust text x-ptr y-ptr))))


(defunL pgqpos ()
  (multiple-value-bind (dummy x y) (pgqpos-raw)
		       (declare (ignorable dummy))
		       (values x y)))

(defunL pgqvsz (units)
  (multiple-value-bind (dummy x1 x2 y1 y2)  (pgqvsz-raw units)
		       (declare (ignorable dummy))
		       (values x1 x2 y1 y2) ))

(defunL pgptxt (x y angle fjust text)
  (ensure-type 'single-float x y angle fjust) 
  (ensure-type 'simple-string text)
  (pgptxt-raw x y angle fjust text))

(defunL pgqcr (ci)  ;; returns (values cr cg cb)
  ;; for some reason, OSX Sonoma x86-64 with macports barfs in
  ;; cpgqcr() with a floating point error when device is X11.
  ;; WITH-FLOAT-TRAPS-MASKED prevents it.  Neverthless, X11 server
  ;; windows might open slowly on this platform.
  (with-float-traps-masked (:all t)
    (multiple-value-bind (dummy cr cg cb) (pgqcr-raw ci)
      (declare (ignorable dummy))
      (values cr cg cb))))

(defunL pgsah (fs angle barb)  (pgsah-raw fs angle barb))


(defunL pgwedg (side disp width fg bg label)
  (ensure-type 'single-float disp width fg bg)
  (ensure-type 'simple-string side label)
  (pgwedg-raw side disp width fg bg label))


(defunL pgscf (font)   (pgscf-raw font))
(defunL pgsch (size)
  (ensure-type 'single-float size)
  (pgsch-raw size))
(defunL pgsci (ci) (pgsci-raw ci))
(defunL pgsclp (state) (pgsclp-raw state))
(defunL pgscr (ci cr cg cb)
  (ensure-type 'single-float cr cg cb)
  (pgscr-raw ci cr cg cb))
(defunL pgsfs (fill-style)
  (pgsfs-raw fill-style))
(defunL pgshs (angle sepn phase)
  (ensure-type 'single-float angle sepn phase)
  (pgshs-raw angle sepn phase))
(defunL pgsitf (itf) (pgsitf-raw itf))
(defunL pgsls (ls)     (pgsls-raw ls))
(defunL pgslw (lw)     (pgslw-raw lw))
(defunL pgstbg (tbci)  (pgstbg-raw tbci))
(defunL pgsvp (xleft xright ybot ytop)
  (ensure-type 'single-float xleft xright ybot ytop)
  (pgsvp-raw xleft xright ybot ytop))
(defunL pgswin (x1 x2 y1 y2)
  (ensure-type 'single-float x1 x2 y1 y2)
  (pgswin-raw x1 x2 y1 y2))
(defunL pgslct (dev) (pgslct-raw dev))
(defunL pgupdt ()    (pgupdt-raw))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

