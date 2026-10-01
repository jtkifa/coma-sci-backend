;; various image utilities, mostly working on 2D float arrays
;; indexing is (AREF DATA IY IX), the same as read by cfitsio

(defpackage imutils
  (:use #:cl)
  (:import-from #:fftw3lib #:complex-fft-index-to-frequency)
  (:export
   ;;
   ;;
   ;; image-utils.lisp
   #:image #:image-or-null ;; the image type - 2d float array
   #:imindex #:simindex ;; integer types to index images with fast math
   #:imflag #:flag-image #:flag-image-or-null
   #:bit-image #:bit-image-or-null
   #:complex-image
   #:double-image #:complex-double-image
   #:any-type-image
   #:get-array-element-type-for-image-type
   #:floatvec
   #:make-image          #:make-same-size-image
   #:make-flag-image     #:make-same-size-flag-image
   #:make-bit-image      #:make-same-size-bit-image
   #:make-complex-image  #:make-same-size-complex-image
   #:make-double-image #:make-complex-double-image
   #:image-sizes-match-p
   #:map-images
   #:image-iterate
   #:image-iterate/subpixel
   #:subimage
   #:extract-image-around-position
   #:image-row #:image-column
   ;;
   ;; func-utils.lisp
   #:gaussian/1dnorm    #:gaussian/2dnorm
   #:gaussian-fwhm #:gaussian-sigma-for-fwhm
   #:moffat #:moffat-fwhm #:moffat-a-for-fwhm
   #:gaussian-1d #:gaussian-2d #:moffat ;; functions for convenience
   #:1/r-coma-func
   ;;
   ;; functional-images.lisp
   #:im-func #:flag-imfunc ;; function types that represent images
   #:floatpix-im-func ;; float version of imfunc
   #:generic-image #:generic-flag-image
   #:generic-imref #:generic-flag-imref
   #:image-to-im-func
   #:flag-image-to-flag-im-func
   ;;
   ;; imarith.lisp
   #:im-log #:im-scale #:im-increment
   #:im+ #:im- #:im* #:im/
   #:copy-array-to-single-float-array
   #:copy-image  #:copy-flag-image #:copy-bit-image  #:copy-double-image
   #:copy-complex-image   #:copy-complex-double-image
   #:copy-*-image
   #:pad-2d-image-2^n #:trim-2d-image
   #:fill-image
   #:trim-image
   #:sub-image
   #:pad-image-2^n
   ;;
   ;; imclean.lisp
   #:imclean-avg
   ;; image-interp.lisp
   #:nearest-pixel-interpolate-image 
   #:nearest-pixel-interpolate-image-with-flags
   #:linear-interpolate-image    
   #:linear-interpolate-image-with-flags
   #:lanczos2-interpolate-image
   #:lanczos2-interpolate-image-with-flags
   #:lanczos3-interpolate-image
   #:lanczos3-interpolate-image-with-flags
   #:lanczos4-interpolate-image
   #:lanczos4-interpolate-image-with-flags
   #:general-interpolate-image
   #:general-interpolate-image-with-flags

   ;; ring-stats.lisp
   #:compute-average-over-ring
   #:compute-radial-profile     ;; this is the new name
   #:compute-median-over-ring  ;; legacy special case of
			       ;;  compute-average-over-ring
   #:make-ring-median-vector   ;; legacy special case of 
			       ;;  compute-radial-profile
   #:fwhm-using-radial-profile
   
   ;; detections.lisp
   #:find-hottest-block
   #:find-peak

   ;; cosmic-ray-rejection.lisp
   #:is-cosmic-ray
   #:make-cosmic-ray-bitmap

   ;; imstats.lisp
   #:image-sum
   #:image-sum-in-annulus
   #:image-mean
   #:image-fraction
   #:gather-pixels-in-annulus
   #:image-fraction-in-annulus
   #:image-median-and-sigma-in-annulus
   #:image-median   
   #:get-random-pixel-sample
   #:compute-sampled-image-median-and-sigma
   #:estimate-image-gain

   ;; graphics-algorithms.lisp
   #:cohen-sutherland-clip-line
   #:draw-line
   
   ;;
   ;; convolve.lisp
   #:convolve-image
   #:make-gaussian-kernel
   #:make-2gaussian-kernel
   ;;
   ;; test-images.lisp
   #:make-2d-quadratic-test-array
   #:make-dot-image
   #:make-gaussian-image
   #:make-test-pattern-image
   #:make-bullseye-image 
   ;;
   ;; imfit-quadratic.lisp
   #:fit-quadratic
   #:fit-quadratic/deluxe ;; includes eigenvalues and FWHM
   #:quadfit-result #:quadfit-result-p
   #:quadfit-result-x0 #:quadfit-result-y0
   #:quadfit-result-sign #:quadfit-result-params
   #:quadfit-result-covar #:quadfit-result-chisqr
   #:quadfit-result-eigenval-1 #:quadfit-result-eigenvec-1
   #:quadfit-result-eigenval-2 #:quadfit-result-eigenvec-2
   #:quadfit-result-pa
   #:quadfit-result-fwhm-1 #:quadfit-result-fwhm-2
   #:quadfit-result-backd
   
   ;;
   ;; imwavelet.lisp
   #:complex-fft-index-to-frequency 
   #:transform-image-to-wavelet-space
   #:transform-image-from-wavelet-space
   #:compute-index-range-for-wavelet-order
   #:compute-starting-index-for-wavelet-order
   #:compute-ending-index-for-wavelet-order
   #:compute-index-range-for-wavelet-order-range
   #:compute-wavelet-order-for-index
   #:wavelet-hipass-filter
   #:zero-selected-wavelet-scales
   #:wavelet-filter-by-function-of-scales
   ;;
   ;; photometry-utils
   #:compute-mag+dmag-from-flux+dflux  ;; this should go elsewhere
   ;;
   ;; ap-phot.lisp
   #:ap-phot
   ;;
   ;; ring-phot.lisp
   #:ring-phot
   #:ring-phot-result
   #:ring-phot-result-flux    #:ring-phot-result-flux-err
   #:ring-phot-result-mag    #:ring-phot-result-mag-err
   #:ring-phot-result-x0    #:ring-phot-result-y0
   #:ring-phot-result-flux-ap         #:ring-phot-result-flux-ap-err
   #:ring-phot-result-profile-flux    #:ring-phot-result-profile-flux-err
   ;;
   ;; backd-image.lisp
   #:build-background-image-func
   #:build-background-image
   ;;
   ;; image-resample.lisp
   #:xform-linear #:xform-linear-p #:xform-linear-matrix
   #:xform-linear-x0 #:xform-linear-y0
   #:apply-xform-linear
   #:invert-xform-linear 
   #:build-xform-linear 
   #:make-xform-linear-map-pixels-by-changing-center
   #:make-xform-linear-map-centers
   #:make-xform-mapping-three-points
   #:resample-image-with-linear-xform
   #:resample-image-with-linear-xform-with-flags
   ;;
   #:xform-functional #:xform-functional-p 
   #:xform-functional-xin #:xform-functional-yin
   #:xform-functional-xout #:xform-functional-yout
   #:xform-functional-data
   #:resample-image-with-functional-xform
   #:resample-image-with-functional-xform-with-flags
   ;;
   ;;
   ;; image-stack.lisp
   #:image-stack
   #:image-stack-with-function
   ;;
   ;;
   ;; fft-image.lisp - uses fftw3
   #:complex-fft-index-to-frequency
   #:fftf-image-to-complex-image
   #:fftb-complex-image-to-image
   #:fft-convolve-images
   #:compute-image-power-spectrum
   #:fft-filter-image
   #:fft-wiener-deconvolve-image
   #:wrap-image-into-fft-kernel
   
   ;;
   ;;
   ;; one-dim-project.lisp - project image sections onto a line
   #:one-dim-project
   ;;
   ;;
   ;; image-smoothing.lisp - median filter an image (and compute
   ;; various pseudo-sigmas)
   #:median-filter-image
   #:mean-filter-image
   #:median-ring-filter-image
   #:make-pseudo-sigma-image
   #:make-ring-pseudo-sigma-image
   ;;
   ;;
   ;; fit-gaussian-psf.lisp - functional fitting of local peaks
   #:fit-round-gaussian
   #:fit-2axis-gaussian
   ;;
   ;; fit-moffat-psf.lisp
   #:fit-round-moffat
   #:fit-2axis-moffat
   ;;
   ;; fit-trailed-gaussian.lisp
   #:fit-trailed-gaussian
   #:trailed-gaussian-function
   ;;
   ;; fit-1d-profile.lisp
   #:fit-1d-profile
   #:fit-1d-profile/gaussian
   #:fit-1d-profile/moffat
   #:fit-1d-profile/moffat+1/r
   ;;
   ;; moments.lisp - calculate zeroth, first, second moments of an
   ;; image region
   #:flagged-pix-in-aperture-p
   #:zeroth-moment #:first-moments #:second-moments 
   #:first-radial-moment
   #:gaussian-weighted-second-moments
   #:xy-moments-to-ab-pa
   #:ab-pa-to-xy-moments
   #:convert-moments-under-linear-xform
   ;;
   ;; add-sources.lisp - add various shapes to images, including
   ;; subpix resample
   #:add-gaussian-to-image
   #:add-moffat-to-image
   #:add-radial-function-to-image
   ;;
   ;; trail-image.lisp - trail an image along a line
   #:trail-image
   ;;
   ;; simple-source-detector.lisp 
   #:source #:source-p
   #:source-x   #:source-y
   #:source-ix  #:source-iy
   #:source-ap-flux 
   #:source-flux #:source-sigma #:source-fwhm #:source-errors
   #:source-quadfit-ok  #:source-quadfit-result #:source-n-flagged-pix
   #:find-sources
   ;; psf-extract.lisp
   #:make-resampled-subimage-at-xy
   #:extract-psf-from-image
   ;;
   ;; static-sky-subtract.lisp
   #:subtract-static-sky
   ;;
   ;; histogram-normalize.lisp
   #:histogram-equalize-image
   ;;
   ;; imfill.lisp
   #:map-pixels-by-region
   #:imfill-pixels-by-region
   #:imfill-corner
   #:imfill-rectangle
   #:imfill-above/below-line
   #:imfill-edge
   #:imfill-border
   #:imfill-polygon
   ;;
   ;; bad-rowcol-clean.lisp
   #:bad-rowcol-filter-image
   #:apply-3x3-laplace-filter
   ;;
   ;; larson-sekanina-filter.lisp
   #:larson-sekanina-filter
   ;;
   ;; image-upsample.lisp
   #:image-upsample
   ))


