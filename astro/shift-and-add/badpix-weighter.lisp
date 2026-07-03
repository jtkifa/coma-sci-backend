

#|

An image weighter that uses the instrument-id:get-get-badpix-function-for-fits function
to generate initial image weights that are zero at bad pixels.

|#

(in-package shift-and-add)


(defclass badpix-image-weighter (image-weighter)
  ())

(defmethod run-weight-generation ((image-weighter badpix-image-weighter)
				  (saaplan saaplan)
				  fits-list)
  (dolist (fits fits-list)
    (multiple-value-bind (badpix-func badpix-func-does-nothing)
	(instrument-id:get-badpix-function-for-fits fits-file)
      (when (not badpix-func-does-nothing)
	(%make-badpix-weight-image-for-fits badpix-func fits-file)))))



(defun %make-badpix-weight-image-for-fits (badpix-func fits-file)
  (declare (type instrument-id:badpix-function-type badpix-func))
  
