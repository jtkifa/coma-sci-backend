

(defpackage nrwavelets
  (:use #:common-lisp #+cmu #:alien #+sbcl #:sb-alien)
  (:export
   #:pad-2d-dbl-array-2^n
   #:trim-2d-dbl-array
   #:pad-2d-sngl-array-2^n
   #:trim-2d-sngl-array
   #:wavelet-transform
   #:wavelet-transform-double-float
   #:wavelet-transform-single-float
  ))


(in-package nrwavelets)

(cffi:define-foreign-library nrwaveletslib
  (:darwin (:or "nrwavelets.dylib"))
  (:unix (:or "nrwavelets.so")))

(cffi:use-foreign-library nrwaveletslib)



;; the three routines wavelet routines for DAUB4 DAUB12 DAUB20

(waaf:sbdefine-alien-routine
 ("dwtn4" dwtn4) :void
  (a (* :double-float) :in)
  (nn (* :unsigned-long) :in)
  (ndim :int)
  (isign :int))




(waaf:sbdefine-alien-routine 
 ("dwtn12" dwtn12) :void
 (a (* :double-float) :in)
 (nn (* :unsigned-long) :in)
 (ndim :int)
 (isign :int))


(waaf:sbdefine-alien-routine 
 ("dwtn20" dwtn20) :void
 (a (* :double-float) :in)
 (nn (* :unsigned-long) :in)
 (ndim :int)
 (isign :int))


 
(waaf:sbdefine-alien-routine 
 ("fwtn4" fwtn4) :void
 (a (* :single-float) :in)
 (nn (* :unsigned-long) :in)
 (ndim :int)
 (isign :int))


(waaf:sbdefine-alien-routine 
 ("fwtn12" fwtn12) :void
 (a (* :single-float) :in)
 (nn (* :unsigned-long) :in)
 (ndim :int)
 (isign :int))


(waaf:sbdefine-alien-routine 
 ("fwtn20" fwtn20) :void
 (a (* :single-float) :in)
 (nn (* :unsigned-long) :in)
 (ndim :int)
 (isign :int))




 

(defun find-next-power-of-two (n)
  "find the next power of 2 that is greater than or equal to N"
  (declare (type (unsigned-byte 28) n))
  (loop
     initially (when (zerop n) (return 0))
     with i of-type (unsigned-byte 28) = 1
     until (>= i n)
     do (setf i (ash i 1))
     finally (return i)))

(defun is-power-of-two (n)
  "is N a power of 2?"
  (= n (find-next-power-of-two n)))


(defun pad-2d-dbl-array-2^n (a2)
  "inflate a two-dimensional double-float array so that its dimensions are
2^n - the array is padded with zeros at the large indices in each dimension"
  (declare (type (simple-array double-float (* *)) a2)
	   (optimize speed))
  (loop
     with n0 = (array-dimension a2 0)
     with n1 = (array-dimension a2 1)
     with m0 = (find-next-power-of-two n0)
     with m1 = (find-next-power-of-two n1)
     with b2 = (make-array (list m0 m1) :element-type 'double-float :initial-element 0d0)
     for i of-type (unsigned-byte 28) below n0
     do
       (loop for j of-type (unsigned-byte 28) below n1
	  do
	    (setf (aref b2 i j) (aref a2 i j)))
     finally (return b2)))


(defun trim-2d-dbl-array (a2 m0 m1)
  "trim a 2d double array so that it has dimensions m0 x m1"
  (declare (type (simple-array double-float (* *)) a2)
	   (type (unsigned-byte 28) m0 m1)
	   (optimize speed))
  (loop
     with n0 = (array-dimension a2 0)
     with n1 = (array-dimension a2 1)
     with b2 = (make-array (list m0 m1) :element-type 'double-float :initial-element 0d0)
     initially
       (when (or (> m0 n0) (> m1 n1))
	 (error "array with array dimensions ~Dx~D can't be trimmed to ~Dx~D" n0 n1 m0 m1))
     for i of-type (unsigned-byte 28) below m0
     do
       (loop for j of-type (unsigned-byte 28) below m1
	  do
	    (setf (aref b2 i j) (aref a2 i j)))
     finally (return b2)))



(defun pad-2d-sngl-array-2^n (a2)
  "inflate a two-dimensional single-float array so that its dimensions are
2^n - the array is padded with zeros at the large indices in each dimension"
  (declare (type (simple-array single-float (* *)) a2)
	   (optimize speed))
  (loop
     with n0 = (array-dimension a2 0)
     with n1 = (array-dimension a2 1)
     with m0 = (find-next-power-of-two n0)
     with m1 = (find-next-power-of-two n1)
     with b2 = (make-array (list m0 m1) :element-type 'single-float :initial-element 0e0)
     for i of-type (unsigned-byte 28) below n0
     do
       (loop for j of-type (unsigned-byte 28) below n1
	  do
	    (setf (aref b2 i j) (aref a2 i j)))
     finally (return b2)))


(defun trim-2d-sngl-array (a2 m0 m1)
  "trim a 2d single array so that it has dimensions m0 x m1"
  (declare (type (simple-array single-float (* *)) a2)
	   (type (unsigned-byte 28) m0 m1)
	   (optimize speed))
  (loop
     with n0 = (array-dimension a2 0)
     with n1 = (array-dimension a2 1)
     with b2 = (make-array (list m0 m1) :element-type 'single-float :initial-element 0e0)
     initially
       (when (or (> m0 n0) (> m1 n1))
	 (error "array with array dimensions ~Dx~D can't be trimmed to ~Dx~D" n0 n1 m0 m1))
     for i of-type (unsigned-byte 28) below m0
     do
       (loop for j of-type (unsigned-byte 28) below m1
	  do
	    (setf (aref b2 i j) (aref a2 i j)))
     finally (return b2)))









(defun wavelet-transform-double-float (a n-daub &optional (forwardp t))
  "peform a wavelet transform on a double-float array array with dimensions 2^n
N-DAUB is the order of the wavelet - allowed values are 4,12,20.   The array
is MODIFIED by the conversion.  The optional argument FORWARDP is by default T for
a forward transform."
  (declare (type (simple-array double-float) a)
	   (type (member 4 12 20) n-daub))
  (when (not (every #'is-power-of-two (array-dimensions a)))
    (error "The dimensions of the array ~A are not all powers of two"
	   (array-dimensions a)))

    (let ((nn (coerce (array-dimensions a)
		      '(simple-array #.waaf:+lisp-ulong-type+ (*))))
	  (ndim (array-rank a)))
      (waaf:with-arrays-as-foreign-pointers ((a ap  :double
						:lisp-type double-float)
					     (nn np  :ulong
						 :lisp-type #.waaf:+lisp-ulong-type+))
	(cond ((= n-daub 4)
	       (dwtn4 ap np
		      ndim
		      (if forwardp 1 -1)))
	      ((= n-daub 12)
	       (dwtn12 ap np
		       ndim
		       (if forwardp 1 -1)))
	      ((= n-daub 20)
	       (dwtn20 ap np 
		       ndim
		       (if forwardp 1 -1)))
	      (t 
	       "N-DAUB=~D must be 4, 12, or 20" n-daub))))
    a)
  




(defun wavelet-transform-single-float (a n-daub &optional (forwardp t))
  "peform a wavelet transform on a single-float array with dimensions 2^n
N-DAUB is the order of the wavelet - allowed values are 4,12,20.   The array
is MODIFIED by the conversion.  The optional argument FORWARDP is by default T for
a forward transform."
  (declare (type (simple-array single-float) a)
	   (type (member 4 12 20) n-daub))
  (when (not (every #'is-power-of-two (array-dimensions a)))
    (error "The dimensions of the array ~A are not all powers of two"
	   (array-dimensions a)))

  
    (let ((nn (coerce (array-dimensions a)
		      '(simple-array #.waaf:+lisp-ulong-type+ (*))))
	  (ndim (array-rank a)))
      (waaf:with-arrays-as-foreign-pointers ((a ap :float 
						:lisp-type single-float)
					     (nn np  :ulong
						:lisp-type #.waaf:+lisp-ulong-type+))
	(cond ((= n-daub 4)
	       (fwtn4 ap np
		      ndim
		      (if forwardp 1 -1)))
	      ((= n-daub 12)
	       (fwtn12 ap np
		       ndim
		       (if forwardp 1 -1)))
	      ((= n-daub 20)
	       (fwtn20 ap np
		       ndim
		       (if forwardp 1 -1)))
	      (t
	       "N-DAUB=~D must be 4, 12, or 20" n-daub))))
    a)
  




(defun wavelet-transform (a n-daub &optional (forwardp t))
  "peform a wavelet transform on a single-float or double-float array
with dimensions 2^n N-DAUB is the order of the wavelet - allowed
values are 4,12,20.  The array is MODIFIED by the conversion.  The
optional argument FORWARDP is by default T for a forward transform."
  (cond ((typep a '(simple-array single-float))
	 (wavelet-transform-single-float a n-daub forwardp))
	((typep a '(simple-array double-float))
	 (wavelet-transform-double-float a n-daub forwardp))
	(t
	 (error "A is neigher a (simple-array single-float) nor (simple-array double-float)"))))







	
    
