#|

set of utilties to convert a request into an answer


|#

(in-package coma-json-server)

(defparameter *cc-allowed-bands*
  '(:usdss :gsdss :rsdss :isdss :zsdss ;; sdss
    :gps1 :rps1 :ips1 :zps1 :yps1      ;; ps1
    :uj :bj :vj :rc :ic                ;; standard bands
    :catlas :oatlas 
    ))
(defparameter *cc-allowed-band-strings*
  (mapcar 'string-upcase
	  (mapcar 'string *cc-allowed-bands*)))

(deftype band-symbol () ;; allowed colors
  `(member ,@*cc-allowed-bands*))

;; convert a case insenstive string to a keyword
(defun parse-band-symbol-string (string &key throw-error)
  (let* ((n (position string *cc-allowed-band-strings* :test 'equalp)))
    ;; return the symbol, or throw an error if THROW-ERROR, or return NIL
    (or (when n (nth n *cc-allowed-bands*))
	(if throw-error
	    (error "~A is not a valid photometric band.  Must be one of ~A" string *cc-allowed-band-strings*)
	    nil))))

;; convert a symbol like :usdss to a string
(defun band-symbol-to-string (band-symbol)
  (string band-symbol))
		  

;; a fully parsed request
(defclass cc-request ()
  ((id     :initarg :id      :accessor ccreq-id)
   (knowns :initarg :knowns  :accessor ccreq-knowns)
   (output :initarg :output  :accessor ccreq-output)
   ;; string if something went wrong
   (error  :initarg :error   :accessor ccreq-error :initform nil)))

;; a fully parsed response
(defclass cc-resp ()
  ((id     :initarg :id      :accessor ccresp-id)
   (output :initarg :output  :accessor ccresp-output)
   (value  :initarg :value   :accessor ccresp-value)
   (value-error  :initarg :value-error   :accessor ccresp-value-error)
   (transform-name :initform "<unnamed>" :initarg :transform-name
		   :accessor ccresp-transform-name)
   (error   :initarg :error :accessor ccresp-error :initform nil)))
   

;; parse a JSON (hash table) COLOR-CONVERSION-REQUEST into a
;; CC-REQUEST object, with the error set to a string if something went wrong 
(defun parse-json-color-conversion-request (req)
  (flet ((return-error (errstring)
	   (return-from parse-json-color-conversion-request
	     (make-instance 'cc-request :error errstring))))
    (when (not (hash-table-p req))
      (return-error "COLOR-CONVERSION-REQUEST is not a JSON object (hash)"))
    (let* ((id  (gethash "ID" req))
	   (output (gethash "OUTPUT" req))
	   (knowns (gethash "KNOWNS" req))
	   (output-sym (or (parse-band-symbol-string output)
			   (return-error (format nil "~A is not a valid output band" output))))
	   (parsed-knowns nil)) ;; kno
      (when (not (vectorp knowns))
	(return-error (format
		      nil "KNOWNS for COLOR-CONVERSION-REQUEST ID=~A is not a vector" id)))
      (setf parsed-knowns
	    (expand-set-of-colorcor-knowns ;; turn v-i,v into v,i,v-i eg
	     (map 'vector
		  (lambda (json-known)
		    (or (ignore-errors 
			 (parse-json-colorcor-known json-known))
			(return-error
			 (format nil "FAILEED to parse KNOWN in  COLOR-CONVERSION-REQUEST ID=~A"
				 id))))
		  knowns)))
      (make-instance 'cc-request :knowns parsed-knowns :id id :output output-sym))))
			     

;; turn a cc-request into a cc-resp, propagating any errors
(defmethod process-cc-req ((cc-request cc-request))
  (flet ((return-error (errstring)
	   (return-from process-cc-req
	     (make-instance 'cc-resp
			    :id (ccreq-id cc-request)
			    :error errstring))))
    ;; propagate error in request and quit
    (when (ccreq-error cc-request)
      (return-error (ccreq-error cc-request)))
      
    (let* ((xform (or (find-matching-color-transform-for-knowns
		       (ccreq-output cc-request)
		       (ccreq-knowns cc-request))
		      (return-error "NO-TRANSFORMATION-FOUND"))))
      (multiple-value-bind (val val-err)
	  (ignore-errors
	   (apply-color-transform-to-knowns xform (ccreq-knowns cc-request)))
	(when (not val) ;; error
	  (return-error (format nil "Error during transform: ~A" val-err)))
	(make-instance 'cc-resp
		       :value val
		       :value-error val-err
		       :id (ccreq-id cc-request)
		       :transform-name (transform-name xform)
		       :output  (ccreq-output cc-request))))))



(defclass %colorcor-known () ()) ;; parent class
;; class representing a magnitude
(defclass colorcor-known-mag   (%colorcor-known)
  ((band  :initarg :band :accessor known-band)  ;; eg :USDSS
   (val   :initarg :val  :accessor known-val)   ;; value
   (err   :initarg :err  :accessor known-err))) ;; error 
;; class representing a color 
(defclass colorcor-known-color   (%colorcor-known)
  ((band1  :initarg :band1 :accessor known-band1)  ;; eg :GSDSS
   (band2  :initarg :band2 :accessor known-band2)  ;; eg :RSDSS
   (val    :initarg :val   :accessor known-val)    ;; value of GSDSS-RSDSS
   (err    :initarg :err  :accessor  known-err)))   ;; error


(defmethod print-object ((known colorcor-known-mag) stream)
  (format stream "#<KNOWN-MAG ~A=~,3F +/- ~,3F>"
	  (known-band known)
	  (known-val known)
	  (known-err known)))

(defmethod print-object ((known colorcor-known-color) stream)
  (format stream "#<KNOWN-COLOR ~A-~A=~,3F +/- ~,3F>"
	  (known-band1 known)
	  (known-band2 known)
	  (known-val known)
	  (known-err known)))


(defun standardize-known-color (color) ;; ensure that color is always BLUE-RED
  (when (> (position (known-band1 color) *cc-allowed-bands*)
	   (position (known-band2 color) *cc-allowed-bands*))
    (rotatef (known-band1 color) (known-band2 color))
    (setf (known-val color) (- (known-val color))))
  color)


;; converts a KNOWN hash like {"QUALITY":"gsdss", "VALUE":19.0, "VALUE-ERROR":0.01}
;; into a COLORCOR-KNOWN-MAG object, or a color into a COLORCOR-KNOWN-COLOR object
(defun parse-json-colorcor-known (known)
  (declare (type hash-table known)) 
  (let* ((quality (gethash "QUALITY" known))
	 (value (gethash "VALUE" known))
	 (value-error  (gethash "VALUE-ERROR" known))
	 (trim-chars #.(make-array 2 :element-type 'base-char
				     :initial-contents '(#\space #\tab))))
    (when (not (stringp quality))
      (error "QUALITY=~A is not a string " quality))
    (when (not (and (realp value) (realp value-error)))
      (error "VALUE=~A and VALUE-ERRROR=~A are not real number." value value-error))

    (cond ((find #\- quality) ;; it's a color
	   (let ((band-string-list (string-utils:split-string quality "-" :trim-chars trim-chars)))
	     (when (not (= 2 (length band-string-list)))
	       (error "QUALITY=~A is an ill-formed color (has minus sign)" quality))
	     (let ((band1 (parse-band-symbol-string (first band-string-list) :throw-error t))
		   (band2 (parse-band-symbol-string (second band-string-list) :throw-error t)))
	       (standardize-known-color
		(make-instance 'colorcor-known-color
			       :band1 band1 :band2 band2
			       :val value :err value-error)))))
	  (t ;; it's a simple mag
	    (make-instance 'colorcor-known-mag
			   :band (parse-band-symbol-string (string-trim trim-chars quality)
							   :throw-error t)
			   :val value :err value-error)))))
  			      
    
(defparameter *known-mag-example1*
  (alexandria:alist-hash-table
   '(("QUALITY" . "VJ") ("value" . 19.99) ("value-Error" . 0.10))
   :test 'equalp))

(defparameter *known-mag-example2*
  (alexandria:alist-hash-table
   '(("QUALITY" . "gsdss") ("value" . 18.88) ("value-Error" . 0.33))
   :test 'equalp))

(defparameter *known-color-example1*
  (alexandria:alist-hash-table
   '(("QUALITY" . "VJ - RC") ("value" . 0.33) ("value-Error" . 0.11))
   :test 'equalp))

;; this one has reversed color that should be fixed in parsing
(defparameter *known-color-example2*
  (alexandria:alist-hash-table
   '(("QUALITY" . "isdss - gsdss") ("value" . 0.44) ("value-Error" . 0.10))
   :test 'equalp))

(defparameter *colorcor-test-input-knowns*
  (mapcar 'parse-json-colorcor-known
	  (list *known-mag-example1* *known-mag-example2* *known-color-example1*  *known-color-example2*)))


;; take a set of %colorcor-known objects and expand it.
;; for example, V-I and V would be expanded to V-I, I, and V
(defun expand-set-of-colorcor-knowns (known-seq
				   &key
				   (expand-mags-from-colors t) ;; V,V-I  --> V,I,V-I
				   (expand-colors-from-mags t)) ;; V,I    --> V,I,V-I

  
  (let* ((known-vec (coerce known-seq 'vector))
	 (output-list (coerce known-seq 'list))
	 ;; hash with KEY=V or KEY=(V-I) eg
	 (qhash (let ((h (make-hash-table :test 'equalp)))
		  (loop
		    for known across known-vec
		    do
		       (cond ((typep known 'colorcor-known-mag)
			      (setf (gethash (known-band known) h) t))
			     ((typep known 'colorcor-known-color)
			      (setf (gethash (list (known-band1 known)
						   (known-band2 known))
					     h)
				    t))))
		  h)))
    (flet ((expand-color-into-mags (known-color)
	     (loop
	       with b1 = (known-band1 known-color)
	       with b2 = (known-band2 known-color)
	       for known-mag across known-vec
	       ;; when KNOWN-COLOR has a band matching a KNOWN-MAG
	       when (and (typep known-mag 'colorcor-known-mag)
			 (or (eq b1 (known-band known-mag))
			     (eq b2 (known-band known-mag))))
		 ;; if color is V-I and mag is I, then V (band1) is V=(1*(V-I))+I
		 ;; but if mag is V then I (band2) is I=-1*(V-I))+V
		 do (let* ((b-out (if (eq b1 (known-band known-mag)) b2 b1)) ;; output band is the other band
			   (b-sign (if (eq b1 (known-band known-mag)) -1 +1))
			   (mag-out (+ (known-val known-mag) (* b-sign (known-val known-color)))))
		      ;; if mag doesn't yet exist, insert the new mag
		      (when (not (gethash b-out qhash))
			(push
			 (setf (gethash b-out qhash)
			       (make-instance 'colorcor-known-mag
					      :band b-out
					      :val mag-out
					      :err (sqrt (+ (expt (known-err known-mag) 2)
							    (expt (known-err known-color) 2)))))
			 output-list)))))
	   ;;
	   (expand-mag-into-colors (known-mag)
	     (let* ((b1 (known-band known-mag)))
	       (loop for known-mag2 across known-vec
		     for b2 = (if (typep known-mag2 'colorcor-known-mag)
				  (known-band known-mag2))
		     when (and b2 ;; it's a mag, not a color
			       ;; shouldn't happen unless they gave us two values for a given band
			       (not (eq (known-band known-mag2) b1))
			       ;; avoid duplicating a color (look for both variants)
			       (not (gethash (list b1 b2) qhash))
			       (not (gethash (list b2 b1) qhash)))
		       ;;
		       do
			  (let* ((new-known-color
				   (standardize-known-color ;; sort the bands in the color
				    (make-instance 'colorcor-known-color
						   :band1 b1
						   :band2 b2
						   :val (- (known-val known-mag)
							   (known-val known-mag2))
						   :err (sqrt
							 (+
							  (expt (known-err known-mag) 2)
							  (expt (known-err known-mag2) 2)))))))
			    (setf (gethash (list (known-band1 new-known-color)
						 (known-band2 new-known-color))
					   qhash)
				  new-known-color)
			    (push new-known-color output-list))))))
      ;; end of flet
      (loop for known across known-vec
	    do (cond ((and (typep known 'colorcor-known-color)
			   expand-mags-from-colors)
		      (expand-color-into-mags known))
		     ((and (typep known 'colorcor-known-mag)
			   expand-colors-from-mags)
		      (expand-mag-into-colors known))))
      ;;
      (coerce output-list 'vector))))								 
				 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a color transform object
(defclass color-transform ()
  ((provides ;; what the transform outputs, like :VJ
    :initform nil :initarg :provides :accessor transform-provides)
   ;; what the transform needs, like (:VJ '(:GSDSS :ISDSS)) means that it
   ;; needs VJ and GSDSS-ISDSS
   (needs
    :initform nil :initarg :needs :accessor transform-needs)
   ;; a function called as (in this example) (func vj vj-error gsdss-isdss gsdss-isdss-error)
   (name ;; a name for the xform
    :initform "<none>" :initarg :name :accessor transform-name)
   (func
    :initform nil :initarg :func :accessor transform-func)))

(defun make-color-transform (&key provides needs func name)
  (make-instance 'color-transform
		 :provides provides
		 :needs needs
		 :name name
		 :func func))

;; make a transform that uses the same function but with different names,
;; substituting PROVIDES and NEEDS if given; this is useful, eg, for
;; turning a tranform from AB system to both PS1 and SDSS
(defun make-synonym-color-transform (transform &key (provides nil) (needs nil))
  (make-color-transform
   :provides (or provides (transform-provides transform))
   :needs    (or needs (transform-needs transform))
   :func (transform-func transform)))


(defmethod print-object ((xform color-transform) stream)
  (format stream "#<COLOR-TRANSFORM ~A=function(~A)>" (transform-provides xform)
	  (with-output-to-string (s)
	    (loop for thing in (transform-needs xform)
		  for is-first = t then nil
		  do (when (not is-first) (write-string "," s))
		     (if (symbolp thing)
			 (format s "~A" thing) ;; a mag
			 (format s "~A-~A" (first thing) (second thing))))))) ;; a color
		  
		  
	  

;; list of COLOR-TRANFORMS
(defvar *color-transforms-list* nil)

(defun add-color-transform (xform)
  ;; don't add transform if it exists, based on NEEDS and PROVIDES
  (when (not (find xform *color-transforms-list*
		   :test (lambda (xf1 xf2)
			   (and (equalp (transform-needs xf1)
					(transform-needs xf2))
				(equalp (transform-provides xf1)
					(transform-provides xf2))))))
  (push xform *color-transforms-list*)))

;; turn the knowns into a hash with KEY=:GSDSS or (:GSDSS :ISDSS)
(defun hashify-knowns (known-seq)
  (loop with h = (make-hash-table :test 'equalp)
	with knowns = (if (vectorp known-seq)
			  known-seq
			  (coerce 'vector known-seq))
	for known across knowns
	for key = (cond ((typep known 'colorcor-known-mag)
			 (known-band known))
			((typep known 'colorcor-known-color)
			 (list (known-band1 known) (known-band2 known))))
	do (setf (gethash key h) known)
	finally (return h)))


(defun  make-identity-transform (desired-output)
  (make-instance 'color-transform
		 :needs (list desired-output)
		 :provides desired-output
		 :name "Identity-Transform"
		 :func (lambda (val err)
			 (values val err))))

;; find a transform in *COLOR-TRANSFORMS-LIST* 
(defun find-matching-color-transform-for-knowns
    (desired-output
     known-set ;; can be hash or list
     &key (color-transforms-list *color-transforms-list*))

  
  ;; return the first matching color-trasform 
  (loop with known-hash = (if (hash-table-p known-set)
			      known-set
			      (hashify-knowns known-set))
	;; initially check for identity, and return an identity transform
	  initially (when (gethash desired-output known-hash)
		      (return (make-identity-transform desired-output)))
	
	for xform in color-transforms-list
	when (and
	      ;; the output is what is desired
	      (eq desired-output (transform-provides xform))
	      ;; every TRANSFORM-NEEDS is in KNOWN-HASH
	      (every (lambda (thing) (gethash thing known-hash))
		     (transform-needs xform)))
	  do (return xform)))

;; apply the color transform XFORM to a list or hash table of knowns
;; in KNOWN-SET
(defun apply-color-transform-to-knowns (xform known-set)
  (let ((known-hash (if (hash-table-p known-set)
			      known-set
			      (hashify-knowns known-set)))
	(arg-list nil))

    (loop for arg-key in (transform-needs xform)
	  for known = (or (gethash arg-key known-hash)
			    (error "Required quality ~A for color-transform ~A not found in known set"
				   arg-key xform))
	  do (push (known-val known) arg-list)
	     (push (known-err known) arg-list))
    ;; note that ARG-LIST is in reversed order
    ;; this must return (value quanity error-in-quantity)
    (apply (transform-func xform) (reverse arg-list))))
    
  
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The color transforms generally don't have built in error computations,
;; so we compute the derivatives numerically:
;;
;;  f(a,b,..)  => f0
;;  sigma_f0 = SQRT{ [(df/da)*a_err]^2 +  [(df/db)*b_err]^2 + ...}
;;
;; given a function of arguments in arg-vec, and errors, return the
;; the central value of function, and its first order error
;; by perturbing the function in each direction by its error
;; and taking the perturbations in quadrature
(defun %color-eval-with-errors (function arg-vec arg-err-vec)
  (let* ((nargs (length arg-vec))
	 (xvec  (make-array nargs))
	 (dvec (make-array nargs))) ;; the numerical derivatives of function
    ;; eval the function with i-error being the error term that
    ;; is being pertubed from center, or NIL if no error
    (flet ((eval-func (i-error delta)
	     (loop for i below nargs
		   do (setf (aref xvec i) (aref arg-vec i)))
	     (when i-error
	       (incf (aref xvec i-error) delta))
	     (cond ((= nargs 1)
		    (funcall function (aref xvec 0)))
		   ((= nargs 2)
		    (funcall function (aref xvec 0) (aref xvec 1)))
		   ((= nargs 3)
		    (funcall function (aref xvec 0) (aref xvec 1)
			     (aref xvec 2)))
		   ((= nargs 4)
		    (funcall function (aref xvec 0) (aref xvec 1)
			     (aref xvec 2) (aref xvec 3)))
		   ((= nargs 5)
		    (funcall function (aref xvec 0) (aref xvec 1)
			     (aref xvec 2) (aref xvec 3) (aref xvec 4))))))
      (let ((f0 (eval-func nil 0.0))) ;; central value
	;; fill in vector of derivatives
	(loop with delta = 0.01
	      for i below nargs
	      do (setf (aref dvec i) ;; derivative[i] = df/dx_i
		       (/ (-  (eval-func i delta) f0) delta)))
	;; compute total sigma
	(let ((sigma (sqrt
		      (loop for i below nargs
			    sum (expt (* (aref dvec i) (aref arg-err-vec i))
				      2)))))
	  (values f0 sigma))))))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      ;
      
		   
		   
	       
	       
