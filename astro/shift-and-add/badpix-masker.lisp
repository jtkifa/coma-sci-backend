

#|

An image weighter that uses the instrument-id:get-get-badpix-function-for-fits function
to generate initial image weights that are zero at bad pixels.

|#
 
(in-package shift-and-add)


(defclass badpix-masker (image-weighter)
  ())

(defmethod run-weight-generation ((image-weighter badpix-masker)
				  (saaplan saaplan)
				  fits-list
				  &key (if-exists :overwrite) badpix-function-list)
  (loop with weight-outlist = nil
	for fits-file in fits-list
	for i from 0
	;;	for badpix-func = (instrument-id:get-badpix-function-for-fits fits-file)
	;; badpix function data may not exist once single extensions are stripped out of
	;; parent image
	for badpix-func = (if badpix-function-list
			      (nth i badpix-function-list))
	if badpix-func
	  do
	     (let ((weight-fits (make-weightfile-name-for-fits fits-file
							       :weight-suffix ".weight.fits"
							       :err-on-weird-suffix t)))
	       (when (and (probe-file weight-fits) (not (eq if-exists :overwrite)))
		 (error "Weight fits ~A already exists, and IF-EXISTS is not OVERWRITE"
			weight-fits))
	       (%make-badpix-weight-image-for-fits badpix-func fits-file weight-fits)
	       (push weight-fits weight-outlist))
	else
	  do (push nil weight-outlist) ;; no corresponding weight file
	finally (return (reverse weight-outlist)))) ;; return list of weight files


;; make a badpix map of the same size as the image
(defun %make-badpix-weight-image-for-fits (badpix-func fits-file fits-weight-file)
  (declare (type instrument-id:badpix-function-type badpix-func))
  (let ((im-ext (instrument-id:get-image-extension-for-onechip-fits fits-file)))
    (cf:maybe-with-open-fits-file (fits-file ff)
      (cf:with-new-fits-file (fits-weight-file ffw :overwrite t)
	(cf:move-to-extension ff im-ext) 
	(let* ((naxis1 (aref (cf:fits-file-current-image-size ff) 0))
	       (naxis2 (aref (cf:fits-file-current-image-size ff) 1))
	       (imweight (make-array (list naxis2 naxis1) :element-type '(unsigned-byte 8))))
	  (loop for ix of-type fixnum from 1 to naxis1
		do (loop for iy of-type fixnum from 1 to naxis2
			 for badpix-val = (if (zerop (funcall badpix-func iy ix)) 1 0)
			 do (setf (aref imweight (1- iy) (1- ix)) badpix-val)))
	  (cf:add-image-to-fits-file ffw :byte
				     (vector naxis1 naxis2)
					 :create-data imweight))))))



;; this wrongly assumed (based on Claude or Google Gemini) that the weight file should have its extension at the same extension
;; instead, it should be a one extension file
#+nil 
(defun %make-badpix-weight-image-for-fits (badpix-func fits-file fits-weight-file)
  (declare (type instrument-id:badpix-function-type badpix-func))
  (let ((im-ext (instrument-id:get-image-extension-for-onechip-fits fits-file)))
    (cf:maybe-with-open-fits-file (fits-file ff)
      (cf:with-new-fits-file (fits-weight-file ffw :overwrite t)
	(loop for iext from 1 to (cf:fits-file-num-hdus ff)
	      do (cf:move-to-extension ff iext)
		 ;;
		 (if (not (= iext im-ext))
		     ;; when not the image extension, add a dummy
		     (progn
		       (cf:add-image-to-fits-file ffw :byte #(1) :create-data #(0))
		       (cf:write-fits-header ffw "SIMPLE" t)
		       (cf:write-fits-header ffw "BITPIX" 8)
		       (cf:write-fits-header ffw "NAXIS" 1)
		       (cf:write-fits-header ffw "NAXIS1" 1)
		       (when (not (= iext 1))
			 (cf:write-fits-header ffw "EXTEND" t)))
		     ;; else add the weight image
		     (let* ((naxis1 (aref (cf:fits-file-current-image-size ff) 0))
			    (naxis2 (aref (cf:fits-file-current-image-size ff) 1))
			    (imweight (make-array (list naxis2 naxis1) :element-type '(unsigned-byte 8))))
		       (loop for ix of-type fixnum from 1 to naxis1
			     do (loop for iy of-type fixnum from 1 to naxis2
				      for badpix-val = (if (zerop (funcall badpix-func iy ix)) 1 0)
				      do (setf (aref imweight (1- iy) (1- ix)) badpix-val)))
		       (cf:add-image-to-fits-file ffw :byte
						  (vector naxis1 naxis2)
						  :create-data imweight))))))))
		 
		 
      
      
      
