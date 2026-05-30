
(in-package astro-catalog)


;; value to indicate an invalid value
(progn
  (defparameter *invalid-dbl-value* float-utils:*double-float-nan*)
  (defun invalid-value-p (x)
    (declare (type double-float x))
    (float-utils:double-float-nan-p x)))

#+1d99badvalue  ;; alternative bad value representation if needed
(progn
  (defparameter *invalid-dbl-value* 1d99)
  (defun invalid-value-p (x)
    (declare (type double-float x))
    (= x 1d99)))

;; Sometimes a catalog returns strings to represent non-values, like
;; proper motions PMRA,PMDEC - this turns them into NANS
;; FIELD-LIST is a list of keywords like (:ra :dec :pmra :pmdec)
;; DATA-VECS is their corresponding data vectors #(ra-vector dec-vector ...).
;; This routine replaces any instance of a non-float with a double-float
;; NaN if FIELD exists
(defun %badvalify-field-dbl-vector (field field-list data-vecs)
  (let* ((field-index (position field field-list))
	 (dvec (if field-index (aref data-vecs field-index))))
    (when dvec
      (loop for i below (length dvec)
	    for x = (aref dvec i)
	    when (or (not (typep x 'double-float))
		     (float-utils:double-float-nan-or-infinity-p x))
	      do (setf (aref dvec i) *invalid-dbl-value*)))))
		       
