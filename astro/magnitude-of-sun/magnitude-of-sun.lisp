(defpackage magnitude-of-sun
  (:use #:cl)
  (:export
   #:magnitude-of-sun))

#|

From

Willmer, C.N.A., The Astrophysical Journal Supplement Series,
Volume 236, Issue 2, article id. 47, 14 pp. (2018).

https://arxiv.org/pdf/1804.07788.pdf


|#

(in-package magnitude-of-sun)

;; table for apparent magnitude of sun in Vega, AB, and ST systems
;; in that order
;; eg (:FILTER :DEFAULT-SYSTEM  vega-mag  ab-mag st-mag)
(defparameter *sunmag-table*
  '((:uj :vega  -25.97 -25.25 -26.15)
    (:bj :vega  -26.13 -26.26 -26.74)
    (:vj :vega  -26.76 -26.77 -26.76)
    (:rc :vega  -27.15 -26.97 -26.57)
    (:ic :vega  -27.47 -27.06 -26.22)
    ;;
    (:bt-tycho       :vega -25.99 -26.09 -26.66)
    (:vt-tycho       :vega -26.69 -26.72 -26.78)
    (:hp-hipparchos  :vega -26.70 -26.70 -26.69)
    ;;
    (:j2mass :vega -27.90 -27.03 -25.26)  ;; 2MASS is almost Vega; -0.025, -0.004, -0.015 off for J,H,K
    (:h2mass :vega -28.25 -26.91 -24.51)
    (:k2mass :vega -28.30 -26.49 -23.50)
    ;;
    (:usdss :ab  -26.08 -25.18 -26.12)
    (:gsdss :ab  -26.34 -26.47 -26.80)
    (:rsdss :ab  -27.04 -26.93 -26.66)
    (:isdss :ab  -27.38 -27.05 -26.37)
    (:zsdss :ab  -27.56 -27.07 -26.00)
    ;;
    (:gps1  :ab  -26.43 -26.54 -26.80)
    (:rps1  :ab  -27.05 -26.93 -26.66)
    (:ips1  :ab  -27.39 -27.05 -26.35 )
    (:zps1  :ab  -27.55 -27.07 -26.07)
    (:yps1  :ab  -27.59 -27.07 -25.85)
    ;;
    ;; computed from ps1 mags using magnitude-of-sun/atlas helper package;
    ;; only as good as approximate PS1 to ATLAS conversion from Tonry 2018
    (:catlas :ab -26.7462 -26.7389 -26.7286)
    (:oatlas :ab -27.2234 -26.9912 -26.5019)
    ;; Casagrande & VandenBerg 2018, MNRAS Letters 479 L102
    (:gaia-g :vega -26.895  -26.792 nil)
    
    ;; the others are probably not useful
    ))



(defun magnitude-of-sun (filter &key (system :auto) (mag-type :apparent))
  "Return the magnitude of the in filter, in given system.
FILTER must be one of the following (or string-equal to it):
    (:UJ :BJ :VJ :RC :IC
     :BT-TYCHO :VT-TYCHO :HP-HIPPARCHOS
     :J2MASS :H2MASS :KS2MASS
     :USDSS :GSDSS :RSDSS :ISDSS :ZSDSS
     :CATLAS :OATLAS)
and SYSTEM must be one of (:AUTO :VEGA :AB :ST).  For :AUTO the system
typically used is used, for example :BJ would have :VEGA and :GSDSS
would use :AB.

MAG-TYPE must be one of (:ABSOLUTE :APPARENT), :APPARENT by default.


"
  (when (not (member system '(:auto :ab :vega :st)))
    (error "SYSTEM must be one of :AB, :VEGA, or :ST"))
  (when (not (member mag-type '(:absolute :apparent)))
    (error "MAG-TYPE  must be :ABSOLUTE or :APPARENT"))
  (let* ((mag-list (or (assoc filter *sunmag-table* :test 'string-equal)
		       (error "Filter ~A unknown - must be one of ~A"
			      filter (mapcar 'car *sunmag-table*))))
	 (%system (if (eq system :auto)
		      (second mag-list) ;; use default system
		      system))
	 (adj (if (eq mag-type :absolute)
		  31.5721
		  0.0))
	 (mag
	   (cond ((eq %system :vega)
	      (third mag-list))
	     ((eq %system :ab)
	      (fourth mag-list))
	     (t
	      (fifth mag-list)))))
    ;;
    (when (not mag) ;; some flavors (like ST) may be undefined
      (error "Solar mag in magnitude system ~A is unknown for filter ~A.  This is a gap in the conversion table."
	     system filter))
	   
    (+ adj
       mag)))
    
    
     
