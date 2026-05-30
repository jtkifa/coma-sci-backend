
(in-package wcs)

(defun build-wcs-radec-tan
    (pixel-scale/arcsec north-angle east-flip
     &key
       (crval1 nil)
       (crval2 nil)
       (crpix1 nil)
       (crpix2 nil)
       (equinox 2000d0)
       (field-pa 0d0)
       (wcs nil))
  "Create or fill a square (same x,y PIXEL-SCALE) RADEC-TAN structure,
with PIXEL-SCALE in arcsec.

First, one imagines rotating the field so that North is +Y.  Then
EAST-FLIP is :LEFT if -X is East, and :RIGHT if +X is East.

Then NORTH-ANGLE is the angle, rotating from +Y into +X, that
is needed to rotate the field from North=+Y to its true configuration.

The traditional North-up, East-left system is thus
NORTH-ANGLE=0, EAST-FLIP=:LEFT.   If the handedness remains the
same, but North=+X, then NORTH-ANGLE becomes 90.

NORTH-ANGLE is thus the negative of PA in the conventional system.

WCS is NIL to create a new WCS-RADEC-TAN, or any type of
WCS-RADEC-TAN-xx to fill in the fields.

CRPIX1,CRPIX2,CRVAL1,CRVAL2,EQUINOX are the usual WCS values by default.

Equinox is mandatory because it is easy to forget, and it defaults to 2000.0

FIELD-PA is an additional overall rotation angle, for the case that
NORTH-ANGLE is for default PA=0, and the field is then rotated."
  (declare (type real pixel-scale/arcsec north-angle)
	   (type (member :left :right) east-flip)
	   (type (or real null) crval1 crval2 crpix1 crpix2)
	   (type real equinox)
	   (type (or wcs-radec-tan null) wcs))
  (let* ((east-dir (if (eq east-flip :left) -1 +1))
	 ;; PA rotates either 
	 (adj-north-angle
	   (if (eq east-flip :right)
	       (+ north-angle field-pa)
	       (- north-angle field-pa)))
	 (pscale/deg (/ pixel-scale/arcsec 3600d0))
	 ;; cos and sin of adj-north-angle
	 (cosna (cos (* (/ pi 180) adj-north-angle)))
	 (sinna (sin (* (/ pi 180) adj-north-angle)))
	 ;; east-dir multiplies x, or the first column of
	 ;; CD matrix, the CDx_1 elements
	 (cd1_1 (* east-dir pscale/deg cosna))
	 (cd2_2 (* pscale/deg cosna))
	 (cd1_2 (* +1 pscale/deg sinna)) ;; (0,1) rotates into (+sin,cos)
	 (cd2_1 (* -1 east-dir pscale/deg sinna))
	 (%wcs (or wcs (make-wcs-radec-tan))))
    (setf (wcs-radec-tan-cd1_1 %wcs) cd1_1)
    (setf (wcs-radec-tan-cd1_2 %wcs) cd1_2)
    (setf (wcs-radec-tan-cd2_1 %wcs) cd2_1)
    (setf (wcs-radec-tan-cd2_2 %wcs) cd2_2)
    (when crval1 (setf (wcs-radec-tan-crval1 %wcs) (float crval1 1d0)))
    (when crval2 (setf (wcs-radec-tan-crval2 %wcs) (float crval2 1d0)))
    (when crpix1 (setf (wcs-radec-tan-crpix1 %wcs) (float crpix1 1d0)))
    (when crpix2  (setf (wcs-radec-tan-crpix2 %wcs) (float crpix2 1d0)))
    (setf (wcs-radec-tan-equinox %wcs) (float equinox 1d0))
    %wcs))
	  
      
	
	
    
