


(defpackage wcs
  (:use cl)
  (:export
   #:wcs-convert-pix-xy-to-world-xy
   #:wcs-convert-world-xy-to-pix-xy
   #:wcs-convert-pix-xy-to-ra-dec   
   #:wcs-convert-ra-dec-to-pix-xy   
   #:wcs-convert-ra-dec-to-world-xy 
   #:wcs-convert-p-to-x
   #:wcs-convert-x-to-p
   ;;
   #:wcs #:wcs-p
   ;;
   #:wcs-2d #:make-wcs-2d #:wcs-2d-p
   #:wcs-2d-crval1 #:wcs-2d-crval2
   #:wcs-2d-crpix1 #:wcs-2d-crpix2
   #:wcs-2d-cd1_1  #:wcs-2d-cd1_2
   #:wcs-2d-cd2_1  #:wcs-2d-cd2_2
   #:wcs-2d-equinox
   ;;
   #:wcs-radec-tan #:make-wcs-radec-tan #:wcs-radec-tan-p
   #:wcs-radec-tan-crval1 #:wcs-radec-tan-crval2
   #:wcs-radec-tan-crpix1 #:wcs-radec-tan-crpix2
   #:wcs-radec-tan-cd1_1  #:wcs-radec-tan-cd1_2
   #:wcs-radec-tan-cd2_1  #:wcs-radec-tan-cd2_2
   #:wcs-radec-tan-equinox
   ;;
   #:wcs-radec-sin #:make-wcs-radec-sin #:wcs-radec-sin-p
   #:wcs-radec-sin-crval1 #:wcs-radec-sin-crval2
   #:wcs-radec-sin-crpix1 #:wcs-radec-sin-crpix2
   #:wcs-radec-sin-cd1_1  #:wcs-radec-sin-cd1_2
   #:wcs-radec-sin-cd2_1  #:wcs-radec-sin-cd2_2
   #:wcs-radec-sin-equinox
   ;;
   ;; FIXME - we define SIP but don't do any math with them
   #:sip-cor #:sip-cor-p #:make-sip-cor #:sip-cor-order #:sip-cor-matrix
   #:sip-cor-a #:sip-cor-ap #:sip-cor-ab #:sip-cor-bp
   #:wcs-radec-tan-sip #:make-wcs-radec-tan-sip #:wcs-radec-tan-sip-p
   #:wcs-radec-tan-sip-crval1 #:wcs-radec-tan-sip-crval2
   #:wcs-radec-tan-sip-crpix1 #:wcs-radec-tan-sip-crpix2
   #:wcs-radec-tan-sip-cd1_1  #:wcs-radec-tan-sip-cd1_2
   #:wcs-radec-tan-sip-cd2_1  #:wcs-radec-tan-sip-cd2_2
   #:wcs-radec-tan-sip-a    #:wcs-radec-tan-sip-b
   #:wcs-radec-tan-sip-ap   #:wcs-radec-tan-sip-bp
   ;;
   #:wcs-radec-tan-tpv #:make-wcs-radec-tan-tpv #:wcs-radec-tan-tpv-p
   #:wcs-radec-tan-tpv-crval1 #:wcs-radec-tan-tpv-crval2
   #:wcs-radec-tan-tpv-crpix1 #:wcs-radec-tan-tpv-crpix2
   #:wcs-radec-tan-tpv-cd1_1  #:wcs-radec-tan-tpv-cd1_2
   #:wcs-radec-tan-tpv-cd2_1  #:wcs-radec-tan-tpv-cd2_2
   #:wcs-radec-tan-tpv-np
   #:wcs-radec-tan-tpv-pv1vec #:wcs-radec-tan-tpv-pv2vec
   #:wcs-radec-tan-tpv-equinox
   ;;
   #:wcs-radec-zpn #:make-wcs-radec-zpn #:wcs-radec-zpn-p
   #:wcs-radec-zpn-crval1 #:wcs-radec-zpn-crval2
   #:wcs-radec-zpn-crpix1 #:wcs-radec-zpn-crpix2
   #:wcs-radec-zpn-cd1_1  #:wcs-radec-zpn-cd1_2
   #:wcs-radec-zpn-cd2_1  #:wcs-radec-zpn-cd2_2
   #:wcs-radec-zpn-np     #:wcs-radec-zpn-pvec
   #:wcs-radec-zpn-equinox
   ;;
   #:wcs-1d #:wcs-1d-p #:wcs-1d-crval1 #:wcs-1d-crpix1
   #:wcs-linear-p1 #:wcs-linear-p2    #:wcs-linear-x1 #:wcs-linear-x2
   #:wcs-linear-ra #:wcs-linear-dec
   ;;
   #:wcs-linear #:make-wcs-linear #:wcs-linear-p #:wcs-linear-crval1
   #:wcs-linear-cdelt1 #:wcs-linear-crpix1
   #:wcs-linear-p1 #:wcs-linear-x1
   #:make-tan-proj-wcs-from-field-info
   ;;
   #:get-pixel-scale-for-wcs
   ;;
   ;; wcs-build.lisp
   #:build-wcs-radec-tan
   
   ))
