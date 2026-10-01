
(defpackage terapix
  (:use #:cl)
  (:export 
   ;; run-programs.lisp
   #:*terapix-directory*
   #:get-fits-directory
   #:run-sextractor
   #:run-scamp
   #:run-swarp
   
   ;; ez-astrometry.lisp
   #:do-linear-astrometry
   #:do-nonlinear-astrometry 
   #:restore-backup-wcs-headers
   
   ;; ez-astrometry-scan.lisp
   #:scan-ez-astrometry
   
   ;; parse-scamp.lisp
   #:read-scamp-head-file
   #:write-scamp-head-file
   #:parse-scamp-wcs-from-headfile
   
   ;; sextractor.lisp
   #:*default-sextractor-read-columns*
   #:read-sextractor-catalog
   #:find-nearest-index-in-sextractor-catalog
   #:estimate-seeing-fwhm-from-sextractor-catalog
   #:add-neighbor-dist-to-sextractor-catalog
   #:sextractor-flag-neighbor
   #:sextractor-flag-blended
   #:sextractor-flag-saturated
   #:sextractor-flag-truncated
   #:sextractor-flag-aperture-data-bad
   #:sextractor-flag-isophotal-data-bad
   #:sextractor-flag-memory-overflow-deblending
   #:sextractor-flag-memory-overflow-extraction
   #:private-flag-cosmic-ray
   
   ;; sextractor-cog.lisp
   #:curve-of-growth  #:curve-of-growth-p #:curve-of-growth-nstars
   #:*curve-of-growth-apertures*
   #:compute-aperture-curve-of-growth-for-fits-file
   #:evaluate-curve-of-growth
   #:read-curve-of-growth-for-fits-file 
   #:write-curve-of-growth-to-fits-file
   #:read-curve-of-growth-from-fits-file
   #:correct-aperture-mag-using-curve-of-growth

   ;; weight-images.lisp
   #:make-sextractor-badpix-weight-image
   
   )) 
