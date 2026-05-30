  

(defpackage instrument-id
  (:use #:cl)
  (:export
   #:instrument #:observatory #:instrument-observatory #:instrument-name 
   #:imaging-instrument #:multichip #:onechip #:instrument
   #:fzipped-p ;; incomplete support - only some instruments
   #:instruments-combinable-p
   #:identify-instrument
   #:saturation-level
   #:non-linear-level
   #:aperture
   #:has-overscan
   #:detector-type #:telescope-design
   #:gain-keyword #:instrument-gain-keyword
   #:chip-id ;; slot of onechip
   ;;
   #:defclass/inst
   #:%add-instrument-id-function
   ;;
   #:preprocessed
   #:preprocessed-p #:preprocessed-p-for-fits
   #:preproc-wcs-origin #:preproc-phot-calib-origin
   #:preproc-wcs-origin #:preproc-phot-calib-origin
   #:preproc-wcs-origin-for-fits #:preproc-phot-calib-origin-for-fits
   ;;
   #:make-instrument-for-name
   ;;
   ;; these functions must be defined for each instrument
   #:test-if-image-at-extension-for-instrument
   #:get-critical-headers-for-instrument
   #:get-standard-filter-for-instrument
   #:get-exptime-for-instrument
   #:get-object-for-instrument
   #:get-object-type-for-instrument
   #:get-mjd-mid-for-instrument
   #:get-mjd-start-for-instrument
   #:get-gain-for-instrument     
   #:write-gain-for-instrument
   #:get-chip-id-for-instrument  
   #:get-datasec-for-instrument  
   #:get-trimsec-for-instrument
   #:get-overscans-for-instrument
   #:get-statsec-for-instrument
   #:convert-imagesec-to-trimmed-imagesec  ;; an extra helper function 
   #:get-initial-wcs-for-instrument  
   #:get-pixel-scale-for-instrument  
   #:get-bright-flux-limit-for-instrument
   #:get-observatory-for-instrument
   #:insert-initial-wcs-for-instrument  
   #:get-image-extension-for-onechip-instrument
   #:get-calibrated-zeropoint-for-instrument
   #:is-reduced-for-instrument
   #:test-if-image-at-extension-for-instrument
   #:get-badpix-function-for-instrument
   #:extract-one-image-from-mosaic-fits
   ;;
   ;; these functions either determine the instrument type, or
   ;; they use the :INSTRUMENT keyword if given
   #:test-if-image-at-extension-for-fits
   #:get-standard-filter-for-fits
   #:aperture-for-fits
   #:detector-type-for-fits
   #:get-critical-headers-for-fits
   #:get-exptime-for-fits
   #:get-object-for-fits
   #:get-object-type-for-fits
   #:get-mjd-mid-for-fits
   #:get-mjd-start-for-fits
   #:get-gain-for-fits
   #:write-gain-for-fits
   #:get-chip-id-for-fits
   #:get-datasec-for-fits
   #:get-trimsec-for-fits
   #:get-overscans-for-fits
   #:get-statsec-for-fits
   #:insert-initial-wcs-for-fits 
   #:get-initial-wcs-for-fits
   #:get-pixel-scale-for-fits  
   #:get-image-extension-for-onechip-fits
   #:get-calibrated-zeropoint-for-fits
   #:is-reduced-for-fits
   #:get-bright-flux-limit-for-fits
   #:get-observatory-for-fits
   #:insert-standard-headers-for-fits-file
   #:test-if-image-at-extension-for-fits
   #:get-badpix-function-for-fits
   #:*standard-headers*
   #:set-standard-header
   #:get-standard-header
   ;; validation function to test whether a fits file is conforming
   #:validate-fits-file

   #:cfht-megacam-one-chip       ;; cfht-megacam.lisp
   #:cfht-megacam-one-chip/fzip
   #:cfht-megacam-one-chip-ext
   #:cfht-megacam-one-chip-ext/fzip
   #:cfht-megacam-array
   #:cfht-megacam-array/fzip
   ;;
   #:hct-hfosc                   ;; hct-hfosc.lisp
   #:hct-hfosc2
   ;;
   #:lowell-prism                ;; lowell-prism.lisp
   #:lowell-31in-direct-ccd      ;; lowell-31-direct-ccd.lisp
   #:lowell-perkins-tek-2k       ;; lowell-perkins-tek2k.lisp
   #:lowell-perkins-loral-2k     ;; lowell-perkins-loral-2k
   ;;
   #:subaru-suprime-cam-one-chip ;; suprime-cam.lisp
   #:subaru-suprime-cam-one-chip
   #:subaru-suprime-cam-array
   ;;
   #:subaru-hypersuprime-cam-one-chip ;; hypersuprime-cam.lisp
   #:subaru-hypersuprime-cam-one-chip/fzip
   #:subaru-hypersuprime-cam-one-chip-tanproj 
   #:subaru-hypersuprime-cam-one-chip-tanproj/fzip
   #:subaru-hypersuprime-cam-array
   #:subaru-hypersuprime-cam-array/fzip
   #:subaru-hypersuprime-cam-hscpipe ;; HSC reduced by pipeline into a projected chunk
   #:subaru-hypersuprime-cam-hscpipe/fzip
   ;;
   #:%uh88-camera #:%uh88-tek
   #:uh88-tek-tek   #:uh88-tek-tec
   #:uh88-tek/lfw #:uh88-tek2048/lfw #:uh88-tektronix-2048-ccd
   #:uh88-LL-Hi-Z-7-5-2-CCD #:uh88-orbit-ccd #:uh88-quirc
   #:uh88-old-tek-1024        #:uh88-old-tek-2048
   #:uh88-old-tek-1024/lfw    #:uh88-old-tek-2048/lfw
   #:uh88-optic
   #:uh88-gec
   #:uh88-snifs-imager
   ;;
   #:calar-alto-dlr-mkiii        ;; calar-alto-dlr-mkiii.lisp
   #:direct-site2k-1             ;; direct-site2k-1.lisp
   #:dct-lmi                     ;; dct-lmi.lisp
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; gemini-gmos.lisp 
   ;;    lots of variants of gmos, most of which not yet implemented
   ;;    the old (version 1) chips are not implemented yet
   #:gmos-n-e2v-frac-chip        
   #:gmos-n-e2v-full-chip
   #:gmos-n-e2v-frac-chip-array
   #:gmos-n-e2v-full-chip-array
   ;; 
   #:gmos-n-hamamatsu-frac-chip            ;; gemini-gmos.lisp 
   #:gmos-n-hamamatsu-full-chip
   #:gmos-n-hamamatsu-frac-chip-array
   #:gmos-n-hamamatsu-full-chip-array
   ;;   
   #:gmos-s-eev-frac-chip            ;; gemini-gmos.lisp 
   #:gmos-s-eev-full-chip
   #:gmos-s-eev-frac-chip-array
   #:gmos-s-eev-full-chip-array
   ;; gmos-s-hamamatsu 
   #:gmos-s-hamamatsu-frac-chip            ;; gemini-gmos.lisp 
   #:gmos-s-hamamatsu-full-chip
   #:gmos-s-hamamatsu-frac-chip-array
   #:gmos-s-hamamatsu-full-chip-array
   ;;
   ;; mixins for types, exported because imred uses them
   #:%gmos-e2v #:%gmos-hamamatsu 
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   #:gmos-n-e2v-swarped-file
   #:gmos-n-hamamatsu-swarped-file
   #:gmos-s-eev-swarped-file
   #:gmos-s-hamamatsu-swarped-file
   
   #:gmos-n-e2v-imred-mosaic-file
   #:gmos-n-hamamatsu-imred-mosaic-file
   #:gmos-s-eev-imred-mosaic-file
   #:gmos-s-hamamatsu-imred-mosaic-file
   
   #:gmos-n-e2v-dragons-mosaic-file
   #:gmos-n-hamamatsu-dragons-mosaic-file
   #:gmos-s-eev-dragons-mosaic-file
   #:gmos-s-hamamatsu-dragons-mosaic-file
   
   #:gmos-n-e2v-dragons-mosaic-stripped-file
   #:gmos-n-hamamatsu-dragons-mosaic-stripped-file
   #:gmos-s-eev-dragons-mosaic-stripped-file
   #:gmos-s-hamamatsu-dragons-mosaic-stripped-file
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;  trappist 
   #:trappist-north-1x1    #:trappist-south-2x2
   #:trappist-south-1x1    #:trappist-south-2x2
   ;; VLT FORS1 and FORS2
   #:vlt-fors1-tek-chip1 #:vlt-fors1-tek-chip2
   #:vlt-fors2-e2v-chip1 #:vlt-fors2-e2v-chip2
   #:vlt-fors2-mit/ll-chip1 #:vlt-fors2-mit/ll-chip2
   ;; ps1 and ps2
   #:ps1-chip-stamp #:ps1-chip-stamp-compressed
   #:ps1-warp-stamp #:ps1-warp-stamp-compressed
   #:ps2-chip-stamp #:ps2-chip-stamp-compressed
   #:ps2-warp-stamp #:ps2-warp-stamp-compressed
   ;; uh8k mosaic on cfht and uh88
   #:uh-8k-mosaic-cfht  #:uh-8k-mosaic-uh88
   ;; DFOSC on danish 1.54m at ESO
   #:dfosc-danish-154-eso
   ;; Keck LRIS, valid for old versions (one chip red side)
   #:keck-lris-red #:keck-lris-blue
   #:keck-lris-red-2chip-v2 keck-lris-blue-2chip-v2
   #:keck-lris-red-v1-2 ;; no blue?
   ;; ctio old cameras
   #:ctio-tek1k-2 #:ctio-421x576  #:ctio-515-419x400
   #:ctio-cfccd-on-ctio1.5m #:ctio-tek1-508x508
   #:ctio-ti1-800x800  #:ctio-ti2-800x800  #:ctio-ti3-800x800
   ;; old CFHT camera
   #:cfht-focam-rca2
   ;; TNG DOLORES (Italian Galileo Telescope on La Palma; in imaging mode)
   #:tng-dolores-imager

   ;; KPNO
   #:kpno-mosaic-one-chip-v1.0
   #:kpno-mosaic-array-v1.0 
   ;;
   #:kpno-south-444x400
   #:kpno-kp09m-t1ka #:kpno-kp21m-t1ka
   #:kpno-kp09m-t2ka #:kpno-kp21m-t2ka
   ;; divided into binnings because the headers don't give binning
   ;; so we treat as separate 
   #:kpno-TI2-400x400  #:kpno-TI2-800x800
   #:kpno-TI3-400x400  #:kpno-TI3-800x800
   #:kpno-rca2-400x400 #:kpno-rca2-800x800

   ;; LBT LBC
   #:lbt-lbc-onechip-red #:lbt-lbc-onechip-blue
   #:lbt-lbc-array-red   #:lbt-lbc-array-blue

   ;; magellan-imacs
   #:magellan-imacs-mosaic1
   #:magellan-imacs-mosaic2
   #:magellan-imacs-mosaic3

   ;; HST
   ;; HST WFC3 - drizzled
   #:hst-wfc3-drz-ir #:hst-wfc3-drz-uvis
   
   ;; minimally identified (but not used) in mis-unhandled-cameras.lisp
   #:cfht-aobir-ir-camera
   #:uk-schmidt-plate-scan
   #:palomar-schmidt-plate-scan
   #:ctio-plate-scan-calibration-spot

   ;; WHT ACAM
   #:wht-acam
   ;;
   ;; esa-ogs-esadc2, a small ESO camera 
   #:esa-ogs-esadc2
   ;;
   #:ctio-decam-onechip-one-ext
   #:ctio-decam-onechip-two-ext
   #:ctio-decam-onechip-compressed-one-ext
   #:ctio-decam-onechip-compressed-two-ext 
   #:ctio-decam-array
   #:ctio-decam-array-proc/oo
   ;;
   #:atlas-stamp/hko
   #:atlas-stamp-diff/hko
   #:atlas-stamp/mlo
   #:atlas-stamp-diff/mlo
   #:atlas-stamp/sutherland
   #:atlas-stamp-diff/sutherland
   #:atlas-stamp/chile
   #:atlas-stamp-diff/chile
   #:atlas-stamp/tdo
   #:atlas-stamp-diff/tdo
   ;;
   #:atlas-aux-stamp/hko
   #:atlas-aux-stamp-diff/hko
   #:atlas-aux-stamp/mlo
   #:atlas-aux-stamp-diff/mlo
   #:atlas-aux-stamp/sutherland
   #:atlas-aux-stamp-diff/sutherland
   #:atlas-aux-stamp/chile
   #:atlas-aux-stamp-diff/chile
   #:atlas-aux-stamp/tdo
   #:atlas-aux-stamp-diff/tdo
   ;;
   #:not-alfosc
   ;;
   #:eso-ntt-susi/raw
   #:eso-ntt-susi/reduced
   #:eso-ntt-susi2/raw1ext
   #:eso-ntt-susi2/raw2ext
   #:eso-ntt-susi2/reduced
   #:eso-ntt-susi2/reduced-onechip 
   #:eso-ntt-emmi-rild-thx1024/raw
   #:eso-ntt-emmi-rild-thx1024/reduced
   #:eso-ntt-emmi-rild-fa2048/raw2ext
   #:eso-ntt-emmi-rild-fa2048/raw4ext
   #:eso-ntt-emmi-rild-fa2048/reduced
   #:eso-ntt-emmi-rild-fa2048/reduced-onechip
   #:eso-ntt-emmi-rild-tek2048/raw
   #:eso-ntt-emmi-rild-tek2048/reduced
   #:eso-ntt-emmi-bimg-thx1024/raw
   #:eso-ntt-emmi-bimg-thx1024/reduced
   #:eso-ntt-emmi-bimg-tek1024/raw
   #:eso-ntt-emmi-bimg-tek1024/reduced

   ;;
   #:generic-onechip
   #:add-generic-headers-using-template
   ;;  
   ))  

 
 
