#|

masker that uses sextractor catalogs to put a (by default) FWHM-FACTOR 2.5x fwhm 
blank mask around images, but takes list from a STACK image

defines class stacked-masker


|#

(in-package shift-and-add)


(defclass stacked-masker (image-weighter)
  ((fwhm-factor :initarg :fwhm-factor
		:initform 2.5
		:accessor masker-fwhm-factor)))

(defmethod run-weight-generation ((masker stacked-masker) (saaplan saaplan) fits-list
				  &key (if-exists :overwrite) badpix-function-list)
				  
  (stacked-masker-function saaplan fits-list
			   :fwhm-factor (masker-fwhm-factor masker)
			   :if-exists if-exists
			   :badpix-function-list badpix-function-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
(defun stacked-masker-function (saaplan fits-working-list
				&key (fwhm-factor 1.0)
				  (if-exists :overwrite)
				  badpix-function-list)
  ;(print *imageout-base*)

  (let ((stack-fits (make-stationary-stack-name
		     saaplan :append-suffix t)))
    (saaplan-log-format saaplan "SHIFT-AND-ADD: Making stack image ~A for mask.~%"
			stack-fits)
    (build-stationary-stack saaplan fits-working-list :force-rebuild nil)
    ;;
    ;; run sextractor on the stack
    (terapix:run-sextractor 
     stack-fits
     :output nil)
    ;;
    (loop with im-ext = 1 ;; the stack is a creation of swarp, so 1 extension
	  with stack-fits-dir = (terapix:get-fits-directory stack-fits :extension im-ext)
	  with shash =  (terapix:read-sextractor-catalog 
			 (format nil "~A/sex.cat" stack-fits-dir))
	  for fits-file in fits-working-list
	  for weight-fits =  (make-weightfile-name-for-fits fits-file
							  :weight-suffix ".weight.fits"
							  :err-on-weird-suffix t)
	  for i from 0
	  for badpix-func = (if badpix-function-list (nth i badpix-function-list))
	  do
	     (when (and (probe-file weight-fits)
			(not (eq if-exists :overwrite)))
	       (error "Weight file ~A exists and IF-EXISTS != :OVERWRITE" weight-fits))
	     (stack-mask-one-fits-file
	      fits-file weight-fits badpix-func shash :fwhm-factor fwhm-factor))))




(defun stack-mask-one-fits-file (fits-file weight-fits badpix-func shash &key (fwhm-factor 1.0))
  (declare (type (or null instrument-id:badpix-function-type) badpix-func))
  ;; read the catalog columns
  (let* ((im-ext (instrument-id:get-image-extension-for-onechip-fits fits-file))
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




    
    
	
#+nil ;; old one
(defun stack-mask-one-fits-file (fits shash &key (fwhm-factor 1.0))
  (cf:maybe-with-open-fits-file (fits ff)

    ;; try to move to correct extension
    (let ((n-ext (or (ignore-errors
		      (instrument-id:get-image-extension-for-onechip-fits fits))
		     ;; if the first image is of finite size
		     (and
		      (eql (cf:fits-file-current-hdu-type ff) :image)
		      (plusp (aref (cf:fits-file-current-image-size ff) 0))
		      1)
		     2)))
      (cf:move-to-extension ff n-ext)
		      
    
      (let* ((fwhmvec (gethash "FWHM_IMAGE" shash))
	     (ra-vec (gethash "ALPHA_J2000" shash))
	     (dec-vec (gethash "DELTA_J2000" shash))
	     (wcs (cf:read-wcs ff))
	     (xvec (make-array (length ra-vec) :element-type 'double-float))
	     (yvec (make-array (length ra-vec) :element-type 'double-float))	 
	     ;;
	     (maskfits (concatenate 'string (file-io:file-basename fits) 
				    ".weight.fits"))
	     (nx (cf:read-fits-header ff "NAXIS1"))
	     (ny (cf:read-fits-header ff "NAXIS2"))
	     (im (make-array (list ny nx) :element-type '(unsigned-byte 16)
					  :initial-element 2)))
	
	;; now turn ra-vec, dec-vec into x-vec,y-vec
	(loop for i from 0
	      for ra across ra-vec
	      for dec across dec-vec
	      do (multiple-value-bind (x y)
		     (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)
		   (setf (aref xvec i) x
			 (aref yvec i) y)))
	
	
	(loop for x across xvec 
 	      for y across yvec
	      for fwhm across fwhmvec
	      for ix = (round x) and iy = (round y)
	      do (%simple-mask-star ix iy im fwhm :fwhm-factor fwhm-factor))
	
	(cf:write-2d-image-to-new-fits-file
	 im maskfits
	 ;; make primary hdu if the original image has one
	 :primary-hdu (= n-ext 2) 
	 :type :short :overwrite t)))))
	 
    

