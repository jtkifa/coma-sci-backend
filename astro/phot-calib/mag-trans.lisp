
#|

MAG-TRANSLATION-FUNCTIONS, denoted as MAG-TRANS-FUNC-XXX-TO-YYY
that convert a catalog to magnitude to a given magnitude, or return 
NIL if not possible.

 (GET-MAG-TRANS-FUNC-FOR-CATALOG-TYPE CATALOG-TYPE FILT)
     and
 (GET-MAG-TRANS-FUNC-FOR-CATALOG CATALOG FILT)

where FILT is in *ALLOWED-FILTERS* and CATALOG and CATALOG-TYPE
are as in the ASTRO-CATALOG package, returns a function of the form
 (MAG-TRANS-FUNC-XXX-TO-YYY ASTRO-CATALOG INDEX)
that returns the FILT type magnitude for object INDEX.

|#


(in-package phot-calib)

;; the list of filters INTO WHICH we may wish to convert from
;; a catalog
(defparameter *allowed-filters* 
  '(:uj :bj :vj :ic :rc                    ;; johnson-cousins
    :usdss :gsdss :rsdss :isdss :zsdss     ;; sdss
    :gps1  :rps1  :ips1  :zps1 :yps1 :wps1 ;; ps1
    :gri-cfht-megacam
    :open-uh88tek  ;; we pretend this is megacam gri
    :gri           ;; we pretend this is megacam gri
    :open          ;; we pretend this is megagam gri
    :wide          ;; we pretend this is megacam gri
    :y
    
;; we don't implement these (yet) because we'd probably never want to
;; go from XX to USNO
;;    :b1usno :r1usno :r2usno :i2usno        ;; usno-b plate colors
;;    :j :h :k                ;; 2mass
    ))


;; for filters that are just a synonym, replace it with
;; the true name
(defun get-filter-primary-name (filt)
  (cond
    ;; this one is just guessed to be close enough to z, being
    ;; a narrowband z (20nm vs about 135nm)
    ((eq filt :z-special-vlt-fors)
     :zsdss)
    (t
     filt)))

;; checks a filter, and replace it with its primary name if needed
(defun %check-filter (filt)
  (setf filt (get-filter-primary-name filt))
  (when (not (member filt *allowed-filters*))
    (error "Filter ~A is not one of ~{~A ~}" filt *allowed-filters*))
  filt)


;; macro to create a DEFUN to take SDSS mags to themselves
(defmacro %make-transform-sdss-to-itself (xform-name filter)
  `(defun ,xform-name (astro-catalog i)
     (declare (type astro-catalog:sdss-catalog astro-catalog)
	      (type fixnum i))
     (multiple-value-bind (mag mag-err invalid)
	 (astro-catalog:object-mag astro-catalog ,filter i)
       (if (not invalid) (values mag mag-err) nil))))

(%make-transform-sdss-to-itself mag-trans-func-sdss-to-usdss :u)
(%make-transform-sdss-to-itself mag-trans-func-sdss-to-gsdss :g) 
(%make-transform-sdss-to-itself mag-trans-func-sdss-to-rsdss :r)
(%make-transform-sdss-to-itself mag-trans-func-sdss-to-isdss :i)
(%make-transform-sdss-to-itself mag-trans-func-sdss-to-zsdss :z)
				

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PS1 to SDSS tranforms from Table 6 of 
;;  Tonry, J. L., Stubbs, C. W., Lykke, K. R., et al.,
;;  2012, ApJ, 750, 99
;;
;; given color c=(f1ps1 - f2ps1) compute fps1+a0+a1*c+a3*c^2
;; or return NIL if colors not defined.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %transform-ps1-to-other (astro-catalog f1ps1 f2ps1 fps1 a0 a1 a2 i) 
  (let (f fe finv f1 f1e f1inv f2 f2e f2inv c)
    (multiple-value-setq (f fe finv)
      (astro-catalog:object-mag astro-catalog fps1 i))
    (multiple-value-setq (f1 f1e f1inv)
      (astro-catalog:object-mag astro-catalog f1ps1 i))
    (multiple-value-setq (f2 f2e f2inv)
      (astro-catalog:object-mag astro-catalog f2ps1 i))
    ;;
    (cond ((or finv f1inv f2inv)
	   nil)
	  (t
	   (setf c (- f1 f2)) ;; color
	   (values 
	    ;; the polynomial fit
	    (+ f  a0 (* c a1) (* c c a2))
	    ;; the error is taken to be the error in the main component
	    fe)))))

;; FIXME - the transformations are already contained in package
;;  phot-transforms/ps1.lisp, so the transformation constants appear
;;  in two places, and they should appear in one place.  This is
;;  because the functions in that code are functions of 2 or 3
;;  variables, and are hard to put into a general macro. 


;; type for any catalog with a ps1 parent
(deftype ps1-derived-catalog ()
  `(or astro-catalog:psps-3pi-mean-kron-mag-catalog
       astro-catalog:psps-3pi-mean-psf-mag-catalog
       astro-catalog:psps-3pi-stack-kron-mag-catalog
       astro-catalog:psps-3pi-stack-psf-mag-catalog
       astro-catalog:psps-3pi-stack-psf-mag-catalog
       astro-catalog:refcat-catalog))


(defun mag-trans-func-ps1-to-gsdss (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog) 
	   (type fixnum i))
  (%transform-ps1-to-other astro-catalog :g :r :g 0.013 0.145 0.019 i))

(defun mag-trans-func-ps1-to-rsdss (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  (%transform-ps1-to-other
   astro-catalog 
   :g :r :r 
   (+ -0.001 0.0233) ;; 0.0233 is a corection to Tonry et al from DRAVG
   0.004 
   0.007 i))

(defun mag-trans-func-ps1-to-isdss (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  (%transform-ps1-to-other astro-catalog :g :r :i -0.005 0.011 0.010 i))

(defun mag-trans-func-ps1-to-zsdss (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  (%transform-ps1-to-other astro-catalog :g :r :z 0.013 -0.039 -0.012 i))


;; and johnson-cousins from the same table

(defun mag-trans-func-ps1-to-bj (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  (%transform-ps1-to-other astro-catalog :g :r :g 0.212 0.556 0.034 i))

(defun mag-trans-func-ps1-to-vj (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  (%transform-ps1-to-other astro-catalog :g :r :r 0.005 0.462 0.013 i))

(defun mag-trans-func-ps1-to-rc (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  (%transform-ps1-to-other astro-catalog :g :r :r -0.137 -0.108 -0.029 i))

(defun mag-trans-func-ps1-to-ic (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  (%transform-ps1-to-other astro-catalog :g :r :i -0.366 -0.136 -0.018 i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transform ps1 to itself

(progn
  ;; macro to create a DEFUN to take SDSS mags to themselves
(defmacro %make-transform-ps1-to-itself (xform-name filter)
  `(defun ,xform-name (astro-catalog i)
     (declare (type ps1-derived-catalog astro-catalog)
	      (type fixnum i))
     (multiple-value-bind (mag mag-err invalid)
	 (astro-catalog:object-mag astro-catalog ,filter i)
       (if (not invalid) (values mag mag-err) nil))))

(%make-transform-ps1-to-itself mag-trans-func-ps1-to-gps1 :g) 
(%make-transform-ps1-to-itself mag-trans-func-ps1-to-rps1 :r)
(%make-transform-ps1-to-itself mag-trans-func-ps1-to-ips1 :i)
(%make-transform-ps1-to-itself mag-trans-func-ps1-to-zps1 :z)
(%make-transform-ps1-to-itself mag-trans-func-ps1-to-yps1 :y)
;; no w-band in ps1 3pi
);; end of progn
				




;; translate SDSS to johhnson/cousins

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lupton's conversions from 
;;  http://www.sdss.org/dr4/algorithms/sdssUBVRITransform.html#Lupton2005
;; taken from phot-utils package, but add in errors
(defun %BVRI-from-sloan-gri/lupton (g r i ge re ie)
  "Estimate B,V,Rc,Ic from Sloan g,r,i - returns 
   (values B V Rc Ic Berr Berr Rcerr Icerr)"
  (let* ((bj  (+ g (* 0.313d0 (- g r)) 0.2271d0))
	 (vj  (- g (* 0.5784d0 (- g r)) 0.0038d0))
	 (rc (- r (* 0.1837d0 (- g r)) 0.0971d0))
	 (ic (- r (* 1.2444d0 (- r i)) 0.382d0))
	 ;; we approximate the errors with the largest component- we
	 ;; could work it out explictly, but probably not worth it because
	 ;; Lutpon's solution is not exact anyway
	 (bje ge)
	 ;; vj is essentially a mix of sloane g,r
	 (vje (* 0.5d0 (sqrt (+ (expt ge 2) (expt re 2)))))
	 (rce re)
	 (ice ie))	
    (values Bj  Vj  Rc  Ic
	    bje vje rce ice)))


;; compute bvri from sloan object i, and 
;; return (values b berr v verr rc rcerr ic icerr)
;; if any mag is invalid, set it NIL
(defun %bvri-from-sloan (astro-catalog i)
  (declare (type astro-catalog:sdss-catalog astro-catalog)
	   (type fixnum i))
  (let (gs egs gsinv rs ers rsinv is eis isinv
	bj vj rc ic bje vje rce ice)
    (multiple-value-setq (gs egs gsinv)
      (astro-catalog:object-mag astro-catalog :g i))
    (multiple-value-setq (rs ers rsinv)
      (astro-catalog:object-mag astro-catalog :r i))
    (multiple-value-setq (is eis isinv)
      (astro-catalog:object-mag astro-catalog :i i))
    
    (multiple-value-setq (bj vj rc ic   bje vje rce ice)
      (%bvri-from-sloan-gri/lupton gs rs is egs ers eis))
    
    ;; now note which of the outputs are invalid
    ;;  all but ic depend on g+r
    (when (or gsinv rsinv) (setf bj nil vj nil rc nil ic nil))
    (when isinv (setf ic nil))
    (values bj bje
	    vj vje 
	    rc rce 
	    ic ice)))


(defun mag-trans-func-sdss-to-bj (astro-catalog i)
  "Convert SDSS catalog object 'i' to B-Johnson"
  (declare (type astro-catalog:sdss-catalog astro-catalog)
	   (type fixnum i))
  (multiple-value-bind (bj bje vj vje rc rce ic ice)
      (%bvri-from-sloan astro-catalog i)
    (declare (ignorable bj bje vj vje rc rce ic ice))
    (if bj (values bj bje) nil)))


(defun mag-trans-func-sdss-to-vj (astro-catalog i)
  "Convert SDSS catalog object 'i' to V-Johnson"
  (declare (type  astro-catalog:sdss-catalog astro-catalog)
	   (type fixnum i))
  (multiple-value-bind (bj bje vj vje rc rce ic ice)
      (%bvri-from-sloan astro-catalog i)
    (declare (ignorable bj bje vj vje rc rce ic ice))
    (if vj (values vj vje) nil)))

(defun mag-trans-func-sdss-to-rc (astro-catalog i)
  "Convert SDSS catalog object 'i' to R-cousins"
  (declare (type astro-catalog:sdss-catalog astro-catalog)
	   (type fixnum i))
  (multiple-value-bind (bj bje vj vje rc rce ic ice)
      (%bvri-from-sloan astro-catalog i)
    (declare (ignorable bj bje vj vje rc rce ic ice))
    (if rc (values rc rce) nil)))

(defun mag-trans-func-sdss-to-ic (astro-catalog i)
  "Convert SDSS catalog object 'i' to a I-cousins"
  (declare (type astro-catalog:sdss-catalog astro-catalog)
	   (type fixnum i))
  (multiple-value-bind (bj bje vj vje rc rce ic ice)
      (%bvri-from-sloan astro-catalog i)
    (declare (ignorable bj bje vj vje rc rce ic ice))
    (if ic (values ic ice) nil)))


;; the case of converting u-sloan to u-johnson is a kludge 
;; using Jordi (https://www.sdss3.org/dr8/algorithms/sdssUBVRITransform.php)

(defun mag-trans-func-sdss-to-uj (astro-catalog i)
  "Convert SDSS catalog object 'i' to U-Johnson"
  (declare (type astro-catalog:sdss-catalog 
		 astro-catalog)
	   (type fixnum i))
  (multiple-value-bind (bj bje vj vje rc rce ic ice)
      (%bvri-from-sloan astro-catalog i)
    (declare (ignorable bj bje vj vje rc rce ic ice))
    (multiple-value-bind (us eus usinv)
	(astro-catalog:object-mag astro-catalog :u i)
    (if (or usinv (not bj))
	nil ;; can't give an answer
	;; use Jordi et al 2005
	(let* 
	    ;; must exist because we computed bj
	    ((gs (astro-catalog:object-mag astro-catalog :g i)) 
	     (uj-bj (* 0.79 (- us gs)))
	     (uj (+ uj-bj bj))
	     (uje eus))
	  (values uj uje))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special transform for wideband gri filter computed by
;; Simon Prunet at CFHT, valid for all 131 classes of Pickles stars
;;    F_g = 0.01551 10^(-0.4*g)  ;; catalog sdss AB g
;;    F_r = 0.01400 10^(-0.4*r)  ;; catalog sdss AB r
;;    F_i = 0.01020 10^(-0.4*i)  ;; catalog sdss AB i
;;    Fgri = 1.5256 * F_g +  1.5969 * F_r +  0.7877 * F_i   ;; gri flux
;;    MagAB_gri= -2.5 log10(Fgri/0.05392)  ;; true AB gri mag of catalog star

(defun %translate-sdss-g-r-i-to-cfht-megacam-gri (mg eg mr er mi ei) ;; sdss mags
  (let* ((fg (*  0.01551 (expt 10 (* -0.4 mg)))) ;; fluxes
	 (fr (*  0.01400 (expt 10 (* -0.4 mr))))
	 (fi (*  0.01020 (expt 10 (* -0.4 mi))))
	 (fgri (+ (* 1.5256 fg)
		  (* 1.5969 fr)
		  (* 0.7877 fi)))
	 (mgri ;; the final gri mag
		 (* -2.5 (log (/ fgri 0.05392) 10)))
	 ;; compute error on flux given that a 0.01 mag
	 ;; error is a 1% flux error, so error in 1.526*fg
	 ;; is 1.5256*fg*eg
	 (efgri
	   (sqrt (+ (expt (* 1.5256 fg eg) 2)    ;; err in fg
		    (expt (* 1.5969 fr er) 2)    ;; err in fr
		    (expt (* 0.7877 fi ei) 2)))) ;; err in fi
	 ;; compute mag error from fractional flux error
	 (dmgri (* 2.5 (log (+ 1.0 (/ efgri fgri)) 10))))
    (values mgri dmgri)))

(defun mag-trans-func-sdss-to-gri-cfht-megacam (astro-catalog i)
  (declare (type astro-catalog:sdss-catalog 
		 astro-catalog)
	   (type fixnum i))
  (let (mg eg ginv   mr er rinv  mi ei iinv)
    ;; get g,r,i mags from sdss catalog
    (multiple-value-setq (mg eg ginv)
      (astro-catalog:object-mag astro-catalog :g i))
    (multiple-value-setq (mr er rinv)
      (astro-catalog:object-mag astro-catalog :r i))
    (multiple-value-setq (mi ei iinv)
      (astro-catalog:object-mag astro-catalog :i i))
    ;;
    (if (or ginv rinv iinv)
	nil ;; no valid answer
	(%translate-sdss-g-r-i-to-cfht-megacam-gri mg eg mr er mi ei))))


(defun mag-trans-func-ps1-to-gri-cfht-megacam (astro-catalog i)
  (declare (type ps1-derived-catalog astro-catalog)
	   (type fixnum i))
  ;; convert ps1 to sdss, then sdss to gri-cfht-megacam
  (multiple-value-bind (mg eg)
      (mag-trans-func-ps1-to-gsdss astro-catalog i)
    (when mg
      (multiple-value-bind (mr er)
	  (mag-trans-func-ps1-to-rsdss astro-catalog i)
	(when er
	  (multiple-value-bind (mi ei)
	      (mag-trans-func-ps1-to-isdss astro-catalog i)
	    (when mi
	      (%translate-sdss-g-r-i-to-cfht-megacam-gri mg eg mr er mi ei))))))))
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-mag-trans-func-for-catalog-type (catalog-type filt)
"For a catalog-type in ASTRO-CATALOG:*ALLOWED-CACHE-CATALOG-TYPES* and
a filter FILT in *ALLOWED-FILTERS*, return an appropriate translation
function."
  (setf filt (%check-filter filt)) ;; if a synonym, replace with primary name
  (cond
    ;; sdss and ps1 use same mechanism for g,r,i,bj,vj,rc,ic
    ((member catalog-type
	     '(astro-catalog:sdss-catalog ;; what is a better way to do this?
	       astro-catalog:sdss7-catalog
	       astro-catalog:sdss8-catalog
	       astro-catalog:sdss9-catalog))
     (cond 
       ((eq filt :usdss) 'mag-trans-func-sdss-to-usdss)     
       ((eq filt :gsdss) 'mag-trans-func-sdss-to-gsdss)
       ((eq filt :rsdss) 'mag-trans-func-sdss-to-rsdss)
       ((eq filt :isdss) 'mag-trans-func-sdss-to-isdss)
       ((and (eq filt :zsdss)
	     ;; sdss7 doesn't have z
	     (not (eq catalog-type 'astro-catalog:sdss7-catalog))) 
	'mag-trans-func-sdss-to-zsdss)
       ((eq filt :uj) 'mag-trans-func-sdss-to-uj)
       ((eq filt :bj) 'mag-trans-func-sdss-to-bj)
       ((eq filt :vj) 'mag-trans-func-sdss-to-vj)
       ((eq filt :rc) 'mag-trans-func-sdss-to-rc)
       ((eq filt :ic) 'mag-trans-func-sdss-to-ic)
       ;;
       ((eq filt :gri-cfht-megacam)
	'mag-trans-func-sdss-to-gri-cfht-megacam)
       
       ;; FIXME - the following is BOGUS
       ((member filt '(:open-uh88tek :wide :open))
	'mag-trans-func-sdss-to-gri-cfht-megacam)

       ))

    ;; ps1 and refcat
    ((member catalog-type '(astro-catalog:psps-3pi-mean-kron-mag-catalog
			    astro-catalog:psps-3pi-mean-psf-mag-catalog
			    astro-catalog:psps-3pi-stack-kron-mag-catalog
			    astro-catalog:psps-3pi-stack-psf-mag-catalog
			    astro-catalog:psps-3pi-stack-psf-mag-catalog
			    astro-catalog:refcat-catalog)) ;; uses ps1 mags
			    
     (cond
       ((eq filt :gsdss) 'mag-trans-func-ps1-to-gsdss)
       ((eq filt :rsdss) 'mag-trans-func-ps1-to-rsdss)
       ((eq filt :isdss) 'mag-trans-func-ps1-to-isdss)
       ((eq filt :zsdss) 'mag-trans-func-ps1-to-zsdss)
       ;;
       ((eq filt :gps1) 'mag-trans-func-ps1-to-gps1)
       ((eq filt :rps1) 'mag-trans-func-ps1-to-rps1)
       ((eq filt :ips1) 'mag-trans-func-ps1-to-ips1)
       ((eq filt :zps1) 'mag-trans-func-ps1-to-zps1)
       ((eq filt :yps1) 'mag-trans-func-ps1-to-yps1)
       ((eq filt :y) 'mag-trans-func-ps1-to-yps1)
       ;; no w-band in PS1 3pi
       ;;
       ((eq filt :bj) 'mag-trans-func-ps1-to-bj)
       ((eq filt :vj) 'mag-trans-func-ps1-to-vj)
       ((eq filt :rc) 'mag-trans-func-ps1-to-rc)
       ((eq filt :ic) 'mag-trans-func-ps1-to-ic)
       ((eq filt :gri-cfht-megacam)
	'mag-trans-func-ps1-to-gri-cfht-megacam) 
       ;; FIXME - the following is BOGUS but is there to allow calib of
       ;;  uh88tek open filter       
       ((member filt '(:open-uh88tek :wide :open))
	'mag-trans-func-ps1-to-gri-cfht-megacam)))
    
    ;; No translation function available
    (t
     NIL)))


(defun get-mag-trans-func-for-catalog (catalog filt)
  "For a catalog of type ASTRO-CATALOG:CATALOG, and a filter in
*ALLOWED-FILTERS*, return the function that translates object 'i'
in the catalog to the filter."
  (declare (type astro-catalog:astro-catalog catalog))
  (get-mag-trans-func-for-catalog-type (type-of catalog) filt))

		 
