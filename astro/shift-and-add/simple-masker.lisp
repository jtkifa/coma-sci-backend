#|

simple masker that uses sextractor catalogs to put a (by simple) 2.5x
fwhm blank mask around images

defines class simple-masker

|#

(in-package shift-and-add) 

(defclass simple-masker (image-weighter)
  ((fwhm-factor :initarg :fwhm-factor
		:initform 2.5
		:accessor masker-fwhm-factor)))

(defmethod run-weight-generation ((masker simple-masker) (saaplan saaplan) fits-list &key (if-exists :overwrite)
				  badpix-function-list)
  (simple-masker-function saaplan fits-list
			  :fwhm-factor (masker-fwhm-factor masker)
			  :if-exists if-exists
			  :badpix-function-list badpix-function-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-masker-function (saaplan fits-working-list
			       &key
				 (fwhm-factor 2.5)
				 (if-exists :overwrite)
				 badpix-function-list)
  (declare (ignore saaplan))
  (loop for fits-file in fits-working-list
	for weight-fits =  (make-weightfile-name-for-fits fits-file
							  :weight-suffix ".weight.fits"
							  :err-on-weird-suffix t)
	for i from 0
	for badpix-func = (if badpix-function-list (nth i badpix-function-list))
	do
	   (when (and (probe-file weight-fits)
		      (not (eq if-exists :overwrite)))
	     (error "Weight file ~A exists and IF-EXISTS != :OVERWRITE" weight-fits))
	   (simple-mask-one-fits-file fits-file weight-fits badpix-func :fwhm-factor fwhm-factor)))


;; mask a star by FWHM-FACTOR times its fwhm
(defun %simple-mask-star (ix iy im fwhm &key (fwhm-factor 1.0))
  (declare (type (signed-byte 28) ix iy)
	   (type (simple-array (unsigned-byte 8) (* *)) im))
  (let* ((fwhm (float fwhm 1d0))
	 (fwhm-factor (float fwhm-factor 1d0))
	 (nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 (r  (min (* fwhm-factor fwhm) 10d0))
	 (r2 (expt r 2))
	 (jx0 (max (round (- ix r)) 0))
	 (jx1 (min (round (+ ix r)) (1- nx)))
	 (jy0 (max (round (- iy r)) 0))
	 (jy1 (min (round (+ iy r)) (1- ny))))

    (loop for jx from jx0 to jx1
	  do (loop for jy from jy0 to jy1
		   for rr2 = (+ (expt (float (- ix jx)) 2)
				(expt (float (- iy jy)) 2))
		   when (<= rr2 r2)
		     do (setf (aref im jy jx) 0)))))
    
    
	
#+nil
(defun simple-mask-one-fits-file (fits &key (fwhm-factor 1.0))
  (let* ((hash (terapix:read-sextractor-catalog
		(concatenate 'string (file-io:file-basename fits)
			     "_DIR" "/sex.cat")))
	 ;(fluxvec (gethash "FLUX_BEST" hash))
	 (fwhmvec (gethash "FWHM_IMAGE" hash))
	 (xvec (gethash "X_IMAGE" hash))
	 (yvec (gethash "Y_IMAGE" hash))
	 ;;
	 (maskfits (concatenate 'string (file-io:file-basename fits) ".weight.fits"))
	 (nx (cf:read-fits-header fits "NAXIS1"))
	 (ny (cf:read-fits-header fits "NAXIS2"))
	 (im (make-array (list ny nx) :element-type '(unsigned-byte 16)
			 :initial-element 2)))

    (loop for x across xvec 
 	  for y across yvec
	  for fwhm across fwhmvec
	  for ix = (round x) and iy = (round y)
	  do (%simple-mask-star ix iy im (float fwhm 1d0) :fwhm-factor fwhm-factor))

    (cf:write-2d-image-to-new-fits-file im maskfits :type :short :overwrite t)))
	 
    
(defun simple-mask-one-fits-file (fits-file weight-fits badpix-func &key (fwhm-factor 1.0))
  (declare (type (or null instrument-id:badpix-function-type) badpix-func))

  ;; read the catalog
  (let* ((im-ext (instrument-id:get-image-extension-for-onechip-fits fits-file))
	 ;; the sextractor catalog
	 (fits-dir (terapix:get-fits-directory fits-file :extension im-ext))
	 (shash (terapix:read-sextractor-catalog
		(concatenate 'string fits-dir "/sex.cat")))
					;(fluxvec (gethash "FLUX_BEST" hash))
	 (fwhmvec (gethash "FWHM_IMAGE" shash))
	 (xvec (gethash "X_IMAGE" shash))
	 (yvec (gethash "Y_IMAGE" shash)))

    
    (cf:maybe-with-open-fits-file (fits-file ff)
      (cf:with-new-fits-file (weight-fits ffw)
	(loop for iext from 1 to (cf:fits-file-num-hdus ff)
	      do (cf:move-to-extension ff iext)
		 ;;
		 (if (not (= iext im-ext))
		     ;; when not the image extension, add a dummy
		     (progn
		       (cf:add-image-to-fits-file ffw :byte #() :create-data nil)
		       (cf:write-fits-header ffw "SIMPLE" t)
		       (cf:write-fits-header ffw "BITPIX" 8)
		       (cf:write-fits-header ffw "NAXIS" 0)
		       (when (not (= iext 1))
			 (cf:write-fits-header ffw "EXTEND" t)))
		     ;; else add the weight image
		     (let* ((naxis1 (aref (cf:fits-file-current-image-size ff) 0))
			    (naxis2 (aref (cf:fits-file-current-image-size ff) 1))
			    (imweight (make-array (list naxis2 naxis1) :element-type '(unsigned-byte 8)
								       :initial-element 1)))

		       ;; first do the sextracted stars
		       (loop for x across xvec 
 			     for y across yvec
			     for fwhm across fwhmvec
			     for ix = (round x) and iy = (round y)
			     do (%simple-mask-star ix iy imweight (float fwhm 1d0) :fwhm-factor fwhm-factor))
		       ;;
		       ;; then set the badpix
		       (when badpix-func
			 (loop for ix of-type fixnum from 1 to naxis1
			       do (loop for iy of-type fixnum from 1 to naxis2
					do (if (not (zerop (funcall badpix-func iy ix)))
					       (setf (aref imweight (1- iy) (1- ix)) 0)))))
		       ;;
		       (cf:add-image-to-fits-file ffw :byte
						  (vector naxis1 naxis2)
						  :create-data imweight))))))))
