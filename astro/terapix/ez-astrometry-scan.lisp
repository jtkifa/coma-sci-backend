

#|

scan a wcs fits at various orientations, over a range of pixel scales 
and flips.

|#

(in-package terapix)


(defun scan-ez-astrometry (fits-file ra dec
			   &key
			     (extension nil)
			     (min-pixel-scale 0.1)
			     (position-maxerr 2.0) ;; arcmin
			     (posangle-maxerr 8.0)
			     (max-pixel-scale 3.0)
			     (catalog-type 'astro-catalog:usno-b1-catalog)
			     (nstars-min 12)
			     (rms-max 0.5)
			     (d-angle 90.0)) ;; angle for degrees
  
  "Given a FITS-FILE and a guess at the central RA and DEC, perform a scan of 
WCS fitting in pixel-scale and rotation angle, allowing reflections."
  
  (let* ((fbase (file-io:file-basename fits-file))
	 (astrometry-catalog-file (format nil "~A.ezscan.cat" fbase))
	 (work-file (format nil "~A_WORK.fits" fbase))
	 (naxis1 (cf:read-fits-header fits-file "naxis1"
				      :extension extension))
	 (naxis2 (cf:read-fits-header fits-file "naxis2"
				      :extension extension))
	 ;; max size of image in arcsec
	 (rmax (+ (* (max naxis1 naxis2) 0.5 max-pixel-scale)
		  (* 60 position-maxerr)))
	 (theta-list (loop for theta from 0.0 below 360.0 by d-angle
			   collect theta)))
			 
	      
	
    
    (format t "Making EZ astrometry catalog of radius ~A arcmin for scanning~%"  (/ rmax 60))
    (let ((catalog (astro-catalog:get-cached-catalog-object 
		    ra dec (/ rmax 3600) catalog-type)))
      (astro-catalog:write-catalog-to-fits-ldac catalog astrometry-catalog-file :overwrite t)
      (format t "Wrote LDAC catalog file ~A with N=~A objects~%"
	      astrometry-catalog-file (astro-catalog:astro-catalog-n catalog)))

    (format t "Copying fits file to work file ~A~%" work-file)
    (file-io:copy-file fits-file work-file :overwrite t)

    (block outer
      (loop for pix-scale = min-pixel-scale then (* 1.03 pix-scale)
	    for pxd = (/ pix-scale 3600d0)
	    while (<= pix-scale max-pixel-scale)
	    do
	       (loop for theta in theta-list
		     for theta/rad = (* (/ pi 180) theta)
		     for costheta = (cos theta/rad) and sintheta = (sin theta/rad)
		     for wcs-initial = (wcs:make-wcs-radec-tan
					:equinox 2000d0
					:crval1 (* 1d0 ra)
					:crval2 (* 1d0 dec)
					:crpix1 (* 0.5d0 naxis1)
					:crpix2 (* 0.5d0 naxis2)
					:cd1_1   (* +1d0 costheta pxd)
					:cd1_2   (* -1d0 sintheta pxd)
					:cd2_1   (* +1d0 sintheta pxd)
					:cd2_2   (* +1d0 costheta pxd))
		     do
			(format t "Trying theta=~A pix-scale = ~,4F~%" theta pix-scale)
			(cf:write-wcs wcs-initial work-file)
			(multiple-value-bind (wcs nstars rms)
			    (ignore-errors
			     (do-linear-astrometry work-file
			       :extension extension
			       :write-wcs t
			       :match-flipped "Y"
			       :nstars-min nstars-min
			       :position-maxerr position-maxerr
			       :posangle-maxerr posangle-maxerr
			       :pixscale-maxerr 1.03 ;; in agreement with stepping
			       :rms-max rms-max
			       :astref-catalog nil
			       :astref-catalog-filename astrometry-catalog-file))
			  (when wcs
			    (format t "FINISHED with N=~A RMS=~A~%" nstars rms)
			    (return-from outer (values wcs nstars rms)))))))))
			
  

  
  
