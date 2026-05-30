#|

Contain all astorb orbits, plus all comet orbits, in a single vector 
  *ORBIT-ELEMENT-VECTOR*


|#  

(in-package small-body-identify)


;; the set of all orbit elements
(defvar *orbit-element-vector* nil)


(defparameter *mpc-comet-data-file* "CometEls.txt")

(defparameter *mpc-comet-data-full-path* 
  (format nil "~A/data/~A"
	  (namestring (asdf:system-source-directory "small-body-identify"))
	  *mpc-comet-data-file*))

(defun read-comet-elements (&key (file *mpc-comet-data-full-path* ))
  (with-open-file (sin file :direction :input :if-does-not-exist :error)
    (loop for line = (read-line sin nil nil)
	  for i from 1
	  until (not line)
	  for elem = (ignore-errors (orbital-elements-parse:parse-mpc-elem-string line))
	  when (not elem)
	    do (error "Bad comet line ~A:<~A>~%" i line)
	  collect elem)))
  

(defun initialize-orbit-elements (&key (force-redo nil) (shuffle t))
  (when (or force-redo (not *orbit-element-vector*))
    (astorb:get-the-astorb) ;; will throw an error if no astorb database loaded
    (let ((comet-list (read-comet-elements))
	  (astorb-list
	    (loop for i below (astorb:astorb-n astorb:*the-astorb*)
		  for elem = (ignore-errors ;; sometimes numerical errors happen in SLALIB
			      (astorb:get-comet-elem-for-nth-asteroid i))
		  ;; now astorb returns DATA field
		  ;;when elem ;; can be NIL when SLALIB failed
		  ;;do (setf (slalib-ephem:comet-elem-data elem) i)
		  collect elem)))
      (setf *orbit-element-vector*
	    (coerce (append astorb-list comet-list) 'vector)))
    ;; shuffle it because some weird (high ecc) objects are concentrated
    ;; in certain parts, which makes multithreaded ops bog down in one thread
    (when shuffle
      (random:shuffle-vector *orbit-element-vector*)))
  
    (length *orbit-element-vector*))


;; it may be true that astorb is loaded, but not the giant
;; database.  In this case, don't force initialization.
(eval-when (:load-toplevel)
  (when astorb:*the-astorb* 
    (initialize-orbit-elements)))

