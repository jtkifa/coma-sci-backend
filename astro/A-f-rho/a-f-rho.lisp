

(defpackage a-f-rho
  (:use #:cl)
  (:export #:a-f-rho))


(in-package a-f-rho)

#|

From Karen Meech's Fortran code


xk=c * rhelio^2 * delta / r_aperture ^2
dm=ApparentMagSun - ApparentMagObj
c = 2.4669e19
 and
Afrho = xk  * 10^(dm/2.5)


and sigma_Afrho = (0.4/log10(e)) * Afrho * magerr

|#


(defun a-f-rho (mag-obj mag-obj-err rhelio delta r-aperture
		&key
		  filter mag-sun
		  (mag-system :auto))
  "Given the apparent
  MAG-OBJ (apparent), and MAG-OBJ-ERR
  RHELIO, DELTA   (au)
  R-APERTURE  (arcsec)
  FILTER -or- MAG-SUN (apparent)
    where FILTER is one of the usual {:uj :bj :rj :ic :rc :usdss, ... :zsdss}
return the a-f-rho measure of cometary activity.

If FILTER is given, then MAG-SYSTEM for computing magnitude of sun
defaults to :AUTO but can be :AB, :VEGA, or :ST

Output is  (VALUES A-F-RHO A-F-RHO-ERROR  MAG-SUN)."
  
  (when (or (not (or filter mag-sun))
	    (and filter mag-sun))
    (error "Either the FILTER or MAG-SUN must be given, but not both"))

  (let* ((%mag-sun (or mag-sun (magnitude-of-sun:magnitude-of-sun
				filter :system mag-system :mag-type :apparent)))
	 (c 2.4669e19)
	 (xk (* c (expt rhelio 2) delta (/ 1.0 (* 2 r-aperture))))
	 (dm (- %mag-sun mag-obj))
	 (afrho (* xk  (expt 10 (/ dm 2.5))))
	 (afrho-err (* (/ 0.4 #.(log (exp 1.0) 10))
		       afrho
		       mag-obj-err)))
    (values afrho afrho-err %mag-sun)))


;; test routine
;; won't worry about slight mismatch in 2nd set of tests because it depends on assumed mag of sun
(defun a-f-rho-test ()
  (format t "Comparing to two values obatained from Fortran version.~%")
  (multiple-value-bind (afrho dafro)
      (a-f-rho 21.479 0.014  4.482 3.525 2.0 :mag-sun -26.74)
    (format t "V AFRHO=~,3D +/- ~,3F    Expected ~,3F +/- ~,3F~%" afrho  dafro 22.52 0.29))
  (multiple-value-bind (afrho dafro)
      (a-f-rho 20.989 0.014  4.482 3.525 2.0 :mag-sun -27.10)
    (format t "R AFRHO=~,3D +/- ~,3F    Expected ~,3F +/- ~,3F~%" afrho  dafro 25.39 0.51))
  ;;
  (format t "Now using explicit filter instead of providing Msun - mag sun doesn't quite match~%")
  (multiple-value-bind (afrho dafro msun)
      (a-f-rho 21.479 0.014  4.482 3.525 2.0 :filter :vj)
    (format t "V AFRHO=~,3D +/- ~,3F Msun=~,2F   Expected ~,3F +/- ~,3F  Msun=~,2F~%"
	    afrho  dafro msun 22.52 0.29 -26.74))
  (multiple-value-bind (afrho dafro msun)
      (a-f-rho 20.989 0.014  4.482 3.525 2.0 :filter :rc)
    (format t "R AFRHO=~,3D +/- ~,3F Msun=~,2F   Expected=~,3F +/- ~,3F  Msun=~,2F~%" 
	    afrho  dafro msun 25.39 0.51 -27.10 )) )
  
			     






