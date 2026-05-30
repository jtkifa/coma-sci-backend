
#|

================================================================
interface some of Fortran starlink slalib library
================================================================
slalib is very difficult to find, even the GPL (?) Fortran version.
We got this from the IRAF distribution, by splitting up the
libslalib.a using (on a Mac)

 f77 -c -O all.f -o out.f

 f77 -dynamiclib -single_module -o libslalib.dylib \
     -install_name  /where/this/library/goes \
      *.o

 WARNING - on Linux, slalib must have gfortran option -fdefault-integer-8
           because otherwise SBCL passes 64 bit ints and fortran
           expects 32

NOTE - the naming notation of the library seems different,
with fortran functions FOO_BAR turning into "foo_bar_"
(note underscore).

================================================================

slalib library - EITHER use starlink version (which is better)
 or  uses IRAF version of code which
  converts names as
    sla_ADDET  ->      sladet_
    sla_AFIN   ->      slafin_   
    sla_AIRMAS ->      slarms_ 
search for STARLINK/IRAF to find code to change to toggle between them
by pushing :slalib-starlink or :slalib-iraf onto *features*
 

================================================================

 NOTE on PV vectors
    They are expressed in heliocentric (not barycentric) frame
    where x=cos(ra) cos(dec)
          y=sin(ra) sin(dec)
          z=sin(dec)
|#




(defpackage slalib
  (:use #:common-lisp #:waaf-cffi)
  (:export
   #:convert-pv-from-au-to-km
   #:convert-pv-from-km-to-au
   #:sla-djcl
   #:sla-dcs2c
   #:sla-evp  ;; low precision
   #:sla-epv  ;; high precision
   #:sla-pvobs
   #:sla-gmst
   #:sla-planel
   #:sla-plante
   #:sla-planet
   #:sla-plantu
   #:sla-dc62s
   #:sla-pv2el
   #:sla-pv2ue
   #:sla-ue2pv
   #:sla-pertel
   #:sla-pertue
   #:sla-el2ue
   #:sla-ue2el
   #:sla-prenut
   #:sla-dmxv
   #:sla-dtt
   #:correct-mjdut-to-mjdtt 
   #:sla-oap
   #:sla-amp
   #:sla-map
   #:sla-airmass
   #:sla-dmoon
   #:sla-rdplan
   
   ;;
   #:compute-local-sidereal-time
   #:compute-airmass-for-ra-dec
   #:compute-altaz-for-apparent-radec
   #:compute-hour-angle-for-ra
   ;;
   #:pv-to-radecr ;; extra routine not in standard slalib to convert PV to ra,dec
   #:sla-pv2-radec ;; obsolete name for above
   ;; convert an ra,dec,r to x,y,z coords
   #:radecr-to-pv/heliocentric
   #:radecr-to-pv/geocentric
   
   #:rvcor  ;; slightly bogus rv correction, mosly to check rvcorrect:bcv
   ;;
   #:+km/au+ ;; astronomical unit in km
   ))

(in-package slalib) 


;; must have one of the two - STARLINK/IRAF
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;;(pushnew :slalib-iraf cl:*features*)
  (pushnew :slalib-starlink cl:*features*)
  )

 
  


(eval-when (:load-toplevel :compile-toplevel)
  
  (cffi:define-foreign-library libslalib
    (:darwin (:or "libslalib_local.dylib" ;; allow for a local override
		  "libslalib.dylib"))
    (:unix (:or "libslalib.so"
		"libslalib_gfortran3.so"
		"libslalib_gfortran4.so"
		"libslalib_gfortran5.so")))

  (cffi:use-foreign-library libslalib)
  )




(defconstant +km/au+ 1.4959787069d8)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define translation table to convert original slalib names to IRAF
;; ones because we are using the easily available IRAF version of the
;; library
(eval-when (:load-toplevel :compile-toplevel :execute)
  #+slalib-iraf
  (defparameter 
   *slalib-name-translation-table*
    '(("sla_ADDET" . "slADET") ("sla_AFIN" . "slAFIN") ("sla_AIRMAS" . "slARMS")
      ("sla_ALTAZ" . "slALAZ") ("sla_AMP" . "slAMP") ("sla_AMPQK" . "slAMPQ")
      ("sla_AOP" . "slAOP") ("sla_AOPPA" . "slAOPA") ("sla_AOPPAT" . "slAOPT")
      ("sla_AOPQK" . "slAOPQ") ("sla__ATMS" . "slATMS") ("sla__ATMT" . "slATMT")
      ("sla_ATMDSP" . "slATMD") ("sla_AV2M" . "slAV2M") ("sla_BEAR" . "slBEAR")
      ("sla_CAF2R" . "slCAFR") ("sla_CALDJ" . "slCADJ") ("sla_CALYD" . "slCAYD")
      ("sla_CC2S" . "slCC2S") ("sla_CC62S" . "slC62S") ("sla_CD2TF" . "slCDTF")
      ("sla_CLDJ" . "slCLDJ") ("sla_CLYD" . "slCLYD") ("sla_CR2AF" . "slCRAF")
      ("sla_CR2TF" . "slCRTF") ("sla_CS2C" . "slCS2C") ("sla_CS2C6" . "slS2C6")
      ("sla_CTF2D" . "slCTFD") ("sla_CTF2R" . "slCTFR") ("sla_DAF2R" . "slDAFR")
      ("sla_DAFIN" . "slDAFN") ("sla_DAT" . "slDAT") ("sla_DAV2M" . "slDAVM")
      ("sla_DBEAR" . "slDBER") ("sla_DBJIN" . "slDBJI") ("sla_DC62S" . "slDC6S")
      ("sla_DCC2S" . "slDC2S") ("sla_DCMPF" . "slDCMF") ("sla_DCS2C" . "slDS2C")
      ("sla_DD2TF" . "slDDTF") ("sla_DE2H" . "slDE2H") ("sla_DEULER" . "slDEUL")
      ("sla_DFLTIN" . "slDFLI") ("sla_DH2E" . "slDH2E") ("sla_DIMXV" . "slDIMV")
      ("sla_DJCAL" . "slDJCA") ("sla_DJCL" . "slDJCL") ("sla_DM2AV" . "slDMAV")
      ("sla_DMAT" . "slDMAT") ("sla_DMOON" . "slDMON") ("sla_DMXM" . "slDMXM")
      ("sla_DMXV" . "slDMXV") ("sla_DPAV" . "slDPAV") ("sla_DR2AF" . "slDRAF")
      ("sla_DR2TF" . "slDRTF") ("sla_DRANGE" . "slDA1P") ("sla_DRANRM" . "slDA2P")
      ("sla_DS2C6" . "slDSC6") ("sla_DS2TP" . "slDSTP") ("sla_DSEP" . "slDSEP")
      ("sla_DT" . "slDT") ("sla_DTF2D" . "slDTFD") ("sla_DTF2R" . "slDTFR")
      ("sla_DTP2S" . "slDTPS") ("sla_DTP2V" . "slDTPV") ("sla_DTPS2C" . "slDPSC")
      ("sla_DTPV2C" . "slDPVC") ("sla_DTT" . "slDTT") ("sla_DV2TP" . "slDVTP")
      ("sla_DVDV" . "slDVDV") ("sla_DVN" . "slDVN") ("sla_DVXV" . "slDVXV")
      ("sla_E2H" . "slE2H") ("sla_EARTH" . "slERTH") ("sla_ECLEQ" . "slECEQ")
      ("sla_ECMAT" . "slECMA") ("sla_ECOR" . "slECOR") ("sla_EG50" . "slEG50")
      ("sla_EL2UE" . "slELUE") ("sla_EPB" . "slEPB") ("sla_EPB2D" . "slEB2D")
      ("sla_EPCO" . "slEPCO") ("sla_EPJ" . "slEPJ") ("sla_EPJ2D" . "slEJ2D")
      ("sla_EQECL" . "slEQEC") ("sla_EQEQX" . "slEQEX") ("sla_EQGAL" . "slEQGA")
      ("sla_ETRMS" . "slETRM") ("sla_EULER" . "slEULR") ("sla_EVP" . "slEVP")
      ;;("sla_EPV" . "slEPV") ;; not in this version of slalib
      ("sla_FITXY" . "slFTXY") ("sla_FK425" . "slFK45") ("sla_FK45Z" . "slF45Z")
      ("sla_FK524" . "slFK54") ("sla_FK52H" . "slFK5H") ("sla_FK54Z" . "slF54Z")
      ("sla_FK5HZ" . "slF5HZ") ("sla_FLOTIN" . "slRFLI") ("sla_GALEQ" . "slGAEQ")
      ("sla_GALSUP" . "slGASU") ("sla_GE50" . "slGE50") ("sla_GEOC" . "slGEOC")
      ("sla_GMST" . "slGMST") ("sla_GMSTA" . "slGMSA") ("sla_GRESID" . "slGRES")
      ("sla_H2E" . "slH2E") ("sla_H2FK5" . "slHFK5") ("sla_HFK5Z" . "slHF5Z")
      ("sla__IDCHI" . "slICHI") ("sla__IDCHF" . "slICHF") ("sla_IMXV" . "slIMXV")
      ("sla_INTIN" . "slINTI") ("sla_INVF" . "slINVF") ("sla_KBJ" . "slKBJ")
      ("sla_M2AV" . "slM2AV") ("sla_MAP" . "slMAP") ("sla_MAPPA" . "slMAPA")
      ("sla_MAPQK" . "slMAPQ") ("sla_MAPQKZ" . "slMAPZ") ("sla_MOON" . "slMOON")
      ("sla_MXM" . "slMXM") ("sla_MXV" . "slMXV") ("sla_NUT" . "slNUT")
      ("sla_NUTC" . "slNUTC") ("sla_OBS" . "slOBS") ("sla_OAP" . "slOAP") 
      ("sla_OAPQK" . "slOAPQ") ("sla_PA" . "slPA") ("sla_PAV" . "slPAV")
      ("sla_PCD" . "slPCD") ("sla_PDA2H" . "slPDAH") ("sla_PDQ2H" . "slPDQH")
      ("sla_PERTEL" . "slPRTL") ("sla_PERTUE" . "slPRTE") ("sla_PLANEL" . "slPLNE")
      ("sla_PLANET" . "slPLNT") ("sla_PLANTE" . "slPLTE") ("sla_PM" . "slPM")
      #+slalib-starlink ("sla_PLANTU" . "slPLTU")  ;; does not exist
      ("sla_POLMO" . "slPLMO") ("sla_PREBN" . "slPRBN") ("sla_PREC" . "slPREC")
      ("sla_PRECES" . "slPRCE") ("sla_PRECL" . "slPREL") ("sla_PRENUT" . "slPRNU")
      ("sla_PV2EL" . "slPVEL") ("sla_PV2UE" . "slPVUE") ("sla_PVOBS" . "slPVOB")
      ("sla_PXY" . "slPXY") ("sla_RANDOM" . "slRNDM") ("sla_RANGE" . "slRA1P")
      ("sla_RANORM" . "slRA2P") ("sla_RCC" . "slRCC") ("sla_RDPLAN" . "slRDPL")
      ("sla_REFCO" . "slRFCO") ("sla_REFCOQ" . "slRFCQ") ("sla_REFRO" . "slRFRO")
      ("sla_REFV" . "slREFV") ("sla_REFZ" . "slREFZ") ("sla_RVEROT" . "slRVER")
      ("sla_RVGALC" . "slRVGA") ("sla_RVLG" . "slRVLG") ("sla_RVLSRD" . "slRVLD")
      ("sla_RVLSRK" . "slRVLK") ("sla_S2TP" . "slS2TP") ("sla_SEP" . "slSEP")
      ("sla_SMAT" . "slSMAT") ("sla_SUBET" . "slSUET") ("sla_SUPGAL" . "slSUGA")
      ("sla_SVD" . "slSVD") ("sla_SVDCOV" . "slSVDC") ("sla_SVDSOL" . "slSVDS")
      ("sla_TP2S" . "slTP2S") ("sla_TP2V" . "slTP2V") ("sla_TPS2C" . "slTPSC")
      ("sla_TPV2C" . "slTPVC") ("sla_UE2EL" . "slUEEL") ("sla_UE2PV" . "slUEPV")
      ("sla_UNPCD" . "slUPCD") ("sla_V2TP" . "slV2TP") ("sla_VDV" . "slVDV")
      ("sla_VN" . "slVN") ("sla_VXV" . "slVXV") ("sla_WAIT" . "slWAIT")
      ("sla_XY2XY" . "slXYXY") ("sla_ZD" . "slZD")
      ;;
      ;; Routines we added to library ourselves are here
      ;;
      ;; convert PV vector to RA,DEC - PV2RADEC.f
      #+slalib-iraf ("slPVRD" . "slPVRD")
      ))
  ;; 
  #+slalib-iraf
  (defun %slaname (orig-name)
    (string-downcase
     (format nil "~A_"  ;; FORTRAN names have a trailing underscore
	     (or
	      (cdr (assoc orig-name *slalib-name-translation-table* :test 'equalp))
	      (error "Cannot translate SLALIB name ~A" orig-name)))))
  ;;

  ;; define slalib translation for the number of underscores, which
  ;; varies by FORTRAN compiler and system
  #+slalib-starlink
  (let ((underscores
	  (cond ((cffi:foreign-symbol-pointer "sla_gmst_")
		 "_")
		((cffi:foreign-symbol-pointer "sla_gmst__")
		 "__")
		(t
		 (error
		  "Could not find number of underscores in SLALIB names")))))
    (defun %slaname (orig-name)
      (string-downcase
       (format nil "~A~A" orig-name underscores)
       ))) 
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
 
;; on both OSX and Linux x86-64, :long is 8-bytes, like Fortran ints,
;; at least using the method we use to compile the Fortran library
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *slalib-fortran-int-type* 
    (or #+(and (or sbcl abcl ccl) :x86-64) :long
	#+(and sbcl darwin arm64) :long
	(error
	 "You will have to figure out what slalib::*slalib-fortran-int-type* 
is (:long or :int) for this Lisp implementation, and fix slalib.lisp 
accordingly.")
	)
    ))


(cffi:defctype fortran-int #.*slalib-fortran-int-type*)

;; if v is null, return the appropriate vector
(defmacro %3vec (v)  `(or ,v (make-array 3 :element-type 'double-float :initial-element 0d0)))
(defmacro %6vec (v)  `(or ,v (make-array 6 :element-type 'double-float :initial-element 0d0)))
(defmacro %deg2rad (x) `(* ,x #.(/ pi 180d0)))

(defmacro %dotprod3 (v1 v2)
  `(locally (declare (type (simple-array double-float (3)) ,v1 ,v2))
     (+ (* (aref ,v1 0) (aref ,v2 0))
	(* (aref ,v1 1) (aref ,v2 1))
	(* (aref ,v1 2) (aref ,v2 2)))))
      

(defun convert-pv-from-au-to-km (pv &optional pv-out)
  "Convert SLALIB PV array PV=[x,y,z,xdot,ydot,zdot] from units of
AU and AU/s to km and km/s.  If &optional pv-out is not specified,
then conversion is in-place"
  (declare (type (simple-array double-float (*)) pv)
	   (type (or null  (simple-array double-float (*))) pv-out))
  (let ((pv-out (or pv-out pv)))
    (declare (type (simple-array double-float (*)) pv-out))
    ;; from AU to km
    (loop for i from 0 to 5
	  do (setf (aref pv-out i)
		   (* (aref pv i) +km/au+)))
    pv-out))

(defun convert-pv-from-km-to-au (pv &optional pv-out)
  "Convert SLALIB PV array PV=[x,y,z,xdot,ydot,zdot] from units
 of km and km/s to AU and AU/s .  If &optional pv-out is not
 specified, then conversion is in-place"
  (declare (type (simple-array double-float (*)) pv)
	   (type (or null  (simple-array double-float (*))) pv-out))
  (let ((pv-out (or pv-out pv)))
    (declare (type (simple-array double-float (*)) pv-out))
    ;; from AU to km
    (loop for i from 0 to 5
	  do (setf (aref pv-out i)
		   (* (aref pv i) (/ 1 +km/au+))))
    pv-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread locking around all function
(defmacro %make-lock (&key name)
  #+ccl  `(ccl:make-lock  ,name)
  #+sbcl `(sb-thread:make-mutex :name ,name)
  #-(or sbcl ccl) :dummy-lock)

#-slalib-is-threadsafe
(defvar *slalib-lock* (%make-lock :name "slalib-lock"))

#-slalib-is-threadsafe
(defmacro %with-slalib-lock (&body body)
  #+ccl `(ccl:with-lock-grabbed (*slalib-lock*) ,@body) 
  #+sbcl `(sb-thread:with-recursive-lock (*slalib-lock*) ,@body)
  #-(or sbcl ccl) `(progn ,@body))

;; a wrapper for defun that ensures locking
#-slalib-is-threadsafe
(defmacro defunL (fname (&rest args) &body body)
  (let ((real-body body)
	(declare-list nil)
	(defun-doc nil))
    (when (stringp (first body))
      (setf defun-doc (car real-body))
      (setf real-body (cdr real-body)))
    (when (eq (caar real-body) 'declare)
      (setf declare-list (car real-body)) 
      (setf real-body (cdr real-body)))
    
  `(defun ,fname (,@args)  
    ,@(append
       (if defun-doc (list defun-doc))
       (if declare-list (list declare-list))
       (list `(%with-slalib-lock 
	       ,@real-body))))))  

#+slalib-is-threadsafe
(defmacro defunL (fname (&rest args) &body body)
  `(defun ,fname (,@args) ,@body))   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(Sbdefine-alien-routine (#.(%slaname "sla_djcl") sla-djcl-raw ) :void
   (djm :double  :in-out)
   (iy  #.*slalib-fortran-int-type*     :out)
   (im  #.*slalib-fortran-int-type*     :out)
   (id  #.*slalib-fortran-int-type*     :out)
   (fd  :double  :out)
   (jstatus #.*slalib-fortran-int-type* :out))

(defunL sla-djcl (mjd)
  "Convert MJD to date, returning (values iyear imonth iday frac-day)"
  (multiple-value-bind (dum1 dum2 iy im id fd jstatus)
      (sla-djcl-raw (float mjd 1d0))
    (declare (ignorable dum1 dum2))
    (when (minusp jstatus)
      (error "unacceptable date (before 4701BC March 1)"))
    (values iy im id fd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sbdefine-alien-routine (#.(%slaname "sla_dtt") sla-dtt-raw ) :double
		       (dju :double :copy))

(defunL sla-dtt (mjdut)
  "Return the correction to MJDUT (a UT MJD) to add to make it into
a Terrestrial Time JDTT  -  ie, return TT-UTC in seconds"
  (declare (type double-float mjdut))
  (sla-dtt-raw mjdut))

(defun correct-mjdut-to-mjdtt (mjdut)
  "Convert MJD-UT to MJD-TT"
  (declare (type double-float mjdut))
  (+ mjdut
     (/ (sla-dtt mjdut) 86400d0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sbdefine-alien-routine (#.(%slaname "sla_dcs2c") sla-dcs2c-raw) :void
  (alpha :double :in-out) ;; radians
  (delta :double :in-out)
  (v (* :double)))

(defunL sla-dcs2c (alpha delta &optional v)
  "Spherical coordinates to Cartesian coordinates: convert ALPHA,DELTA
in radians to Cartesian coordinate vector double-float vector V;
empty V may be input as optional argument"
  (declare (type (or null (simple-array double-float (3))) v))
  (let ((v (%3vec v))
	(a (float alpha 1d0))
	(d (float delta 1d0)))
    (with-array-as-foreign-pointer (v pv :double :lisp-type double-float)
				    (sla-dcs2c-raw a d pv))
    v))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE - distinguish from SLA-EPV (REVERSE P,V) - THIS IS THE LOWER
;; PRECISION ONE, GOOD TO 420 MM/S AND 7000 KM
(sbdefine-alien-routine (#.(%slaname "sla_evp") sla-evp-raw) :void
   (mjd     :double :in-out)
   (equinox  :double :in-out) ;; julian equinox, or mjd if equinox<0
   (dvb      (* :double))  ;; barycentric vel   (AU/sec)
   (dpb      (* :double))  ;; barycentric pos
   (dvh      (* :double))  ;; heliocentric vel  (AU/sec)
   (dph      (* :double))) ;; heliocentric pos  

(defunL sla-evp (MJD equinox &optional dvb dpb dvh dph)
  "Given date in MJD and EQUINOX (using MJD if EQUINOX<0), return
Heliocentric and Barycentric Positions and Velocities of Earth, as
  (VALUES DVB DPB DVH DPH).   Here, DVB DPB DVH DPH are double precision
vectors of length 3 that may be supplied as optional arguments.
Units are AU/sec for velocities, and AU for positions. This is the low
precision version; for high precision see SLA-EPV (not in IRAF SLALIB)"
  (declare (type (or null (simple-array double-float (3)))
		 dvb dpb dvh dph))
  (let ((dvb (%3vec dvb))
	(dpb (%3vec dpb))
	(dvh (%3vec dvh))
	(dph (%3vec dph)))
    (with-arrays-as-foreign-pointers
     ((dvb pdvb :double :lisp-type double-float)
      (dpb pdpb :double :lisp-type double-float)
      (dvh pdvh :double :lisp-type double-float)
      (dph pdph :double :lisp-type double-float))
     (sla-evp-raw (float mjd 1d0)
		  (float equinox 1d0)
		  pdvb pdpb pdvh pdph))
    (values dvb dpb dvh dph)))


#-slalib-iraf
(sbdefine-alien-routine (#.(%slaname "sla_epv") sla-epv-raw) :void
   (mjd      :double :in-out)
   (dvb      (* :double))  ;; barycentric vel
   (dpb      (* :double))  ;; barycentric pos
   (dvh      (* :double))  ;; heliocentric vel
   (dph      (* :double))) ;; heliocentric pos
   

#-slalib-iraf ;; SLA_EPV IS NOT IN IRAF VERSION OF SLALIB?!!!
(defunL sla-epv (MJD &optional dvb dpb dvh dph)
  "Given date in MJD and EQUINOX=MJD, return Heliocentric and
Barycentric Positions and Velocities of Earth, as
  (VALUES DVB DPB DVH DPH).   Here, DVB DPB DVH DPH are double precision
vectors of length 3 that may be supplied as optional arguments.
Units are AU/sec for velocities, and AU for positions.   This is the high
precision version of SLA-EVP.  Note that we convert velocities from AU/day
to AU/sec so that this agrees with SLA-EVP."
  (declare (type (or null (simple-array double-float (3)))
		 dvb dpb dvh dph))
  (let ((dvb (%3vec dvb))
	(dpb (%3vec dpb))
	(dvh (%3vec dvh))
	(dph (%3vec dph)))
    (with-arrays-as-foreign-pointers
     ((dvb pdvb :double :lisp-type double-float)
      (dpb pdpb :double :lisp-type double-float)
      (dvh pdvh :double :lisp-type double-float)
      (dph pdph :double :lisp-type double-float))
     (sla-epv-raw (float mjd 1d0)
		  pdph pdvh pdpb pdvb))  ;; YES, arguments are reversed in slalib routine
    ;;
    ;; the values returned are AU/day and we want AU/sec like sla-evp
    (loop with days/sec = #.(/ 1d0 (* 24d0 3600d0))
	 for i below 3
	 do 
	 (setf (aref dvh i) (* (aref dvh i) days/sec))
	 (setf (aref dvb i) (* (aref dvb i) days/sec)))
    ;;
    (values  dvb dpb dvh dph)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sbdefine-alien-routine (#.(%slaname "sla_pvobs") sla-pvobs-raw) :void
  (p :double    :in-out)   ;; atitude (geodetic, radians)
  (h :double    :in-out)   ;; height above reference spheroid (geodetic, metres)
  (stl :double  :in-out) ;; local apparent sidereal time (radians)
  (pv (* :double)))     ;; x,y,z,vx,vy,vz in AU,AU/s

(defunL sla-pvobs (p h stl &optional pv)
  "return position and velocity of observing station in PV, as
x,y,z,vx,vy,vz in units of AU and AU/s.   P is latitude(geodetic, radians);
H is height above reference spheroid (geodetic, metres);
STL is local apparent sidereal time (radians). Units are AU and AU/s"
  (declare (type (or null (simple-array double-float (6))) pv))
  (let ((pv (%6vec pv)))
    (with-array-as-foreign-pointer 
     (pv ppv :double  :lisp-type double-float)
     (sla-pvobs-raw (float p 1d0) (float h 1d0) (float stl 1d0)
		    ppv))
    pv))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sbdefine-alien-routine (#.(%slaname "sla_gmst") sla-gmst-raw) :double
  (ut1 :double :in-out))

(defunL sla-gmst (ut1)
  "Convert UT1 expressed as MJD to Greenwich mean sidereal time in RADIANS"
  (nth-value 0 (sla-gmst-raw (float ut1 1d0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; radial velocity correction routine
(defunL rvcor (hjd long lat alt ra dec equinox)
  "Given a HJD, and observatory LONG and LAT and ALT, and the
RA DEC and EQUINOX of an object, compute
 (VALUES BARYCENTRIC-CORRECTION HELIOCENTRIC-CORRECTION GEOCENTRIC-CORRECTION)
as additive corrections to the velocity measured.
This is slightly bogus because it uses JD for HJD, and fails
to compute the geocentric correction.   Nevertheless, it is a good
check for RVCORRECT:BCV.  It agrees with bcv, but not with IRAF fxcor"
  (declare (ignorable long lat alt))
  (let (hjmd starpos-vec hvel-vec bvel-vec dum1 dum2
	gvel bvel hvel)
    (setf hjmd (float (- hjd 2400000.5d0) 1d0)) ;; convert to mjd
    ;; the star vector
    (setf starpos-vec (sla-dcs2c (%deg2rad ra) (%deg2rad dec)))
    ;; the barycentric and heliocentric velocity vectors
    (multiple-value-setq (bvel-vec dum1 hvel-vec dum2)
      (sla-evp hjmd (float equinox 1d0)))
    ;; FIXME - PUT IN GEOCENTRIC CORRECTION HERE
    (setf gvel 0d0)
    ;; opposite sign convention from what is in docs
    (setf bvel (* 149.597870D6 (%dotprod3 starpos-vec bvel-vec)))
    (setf hvel (* 149.597870D6 (%dotprod3 starpos-vec hvel-vec)))
    ;;
    (values bvel hvel "we-do-not-do-geocentric-correction-yet")))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; given orbital elements, return PV=[x,y,z,xdot,ydot,zdot]
(sbdefine-alien-routine (#.(%slaname "sla_planel") sla-planel-raw) :void
    (mjd      :double :copy)
    (jform    #.*slalib-fortran-int-type* :copy)
    (epoch    :double :copy)
    (orbinc   :double :copy)
    (anode    :double :copy)
    (perih    :double :copy)
    (aorq     :double :copy)
    (e        :double :copy)
    (aorl     :double :copy)
    (dm       :double :copy)
    (pv       (* :double))
    (jstat    #.*slalib-fortran-int-type* :out))

 

(defunL sla-planel (mjd jform epoch orbinc anode perih aorq e aorl dm &key pv)
  "routine SLA_PLANEL, returning  PV=[x,y,z,xdot,ydot,zdot] as double float array"
  (declare (type (integer 1 3) jform)
	   (type double-float epoch orbinc anode perih aorq e aorl dm)
	   (type (or null (simple-array double-float (*))) pv))
  (let ((pv (%6vec pv)))
    (let ((jstat
	   (with-array-as-foreign-pointer 
	    (pv ppv :double  :lisp-type double-float)
	     (nth-value
	      1 ;; ignore dummy VOID
	      ;; trust that slalib catches bad math itself
	      (sla-planel-raw
	       mjd jform epoch orbinc anode perih aorq e aorl dm ppv)))))
      (if (not (zerop jstat))
	  (error "ERROR ~A in sla-planel-raw - ~A"
		 jstat
		 (case jstat
		   ;; this reflects the more informative errors JTK added
		   ;; to el2ue.f
		   (-1 (format nil "Illegal JFORM=~A" jform))
		   (-2 (format nil "Illegal E=~A" e))
		   (-3 (format nil "Illegal AORQ=~A" aorq))
		   (-4 (format nil "Illegal DM=~DM" dm))
		   (-5 "Numerical error")
		   ;; see u2epv.f
		   (-11 (format nil "sla_UE2PV: Null radius vector"))
		   (-12 (format nil "sla_UE2PV: Failure to converge"))
		   ;; see pv2ue.if
		   (-21 (format nil "sla_PV2UE: negative PMASS"))
		   (-22 (format nil "sla_PV2UE: Too close"))
		   (-23 (format nil "sla_PV2UE: Too slow"))
		   ;;
		   (otherwise  "Unknown error - dig through Fortran code!"))))
	  pv)))  
  


;; alpha,delta from orbital elements
(sbdefine-alien-routine (#.(%slaname "sla_plante") sla-plante-raw) :void
    (mjd      :double  :copy)
    (elong    :double  :copy)
    (phi      :double  :copy)
    (jform    #.*slalib-fortran-int-type* :copy)
    (epoch    :double  :copy)
    (orbinc   :double  :copy)
    (anode    :double  :copy)
    (perih    :double  :copy)
    (aorq     :double  :copy)
    (e        :double  :copy)
    (aorl     :double  :copy)
    (dm       :double  :copy)
    (ra       :double  :out)
    (dec      :double  :out)
    (r        :double  :out)
    (jstat    #.*slalib-fortran-int-type* :out))
 
(defunL sla-plante (mjd elong phi jform epoch orbinc anode perih aorq e aorl dm)
  "routine SLA_PLANTE, returning  RA,Dec,r"
  (declare (type (integer 1 3) jform)
	   (type double-float elong phi epoch orbinc anode perih aorq e aorl dm))
  (multiple-value-bind (dummy ra dec r jstat)
      (sla-plante-raw
       mjd elong phi jform epoch orbinc anode perih aorq e aorl dm)
    (declare (ignorable dummy))
    (if (not (zerop jstat))
	(error "ERROR ~A in sla-plante-raw: ~A"
	       jstat
	       (case jstat
		 (-1 (format nil "Illegal JFORM=~A" jform))
		 (-2 (format nil "Illegal E=~A" e))
		 (-3 (format nil "Illegal AORQ=~A" aorq))
		 (-4 (format nil "Illegal DM=~DM" dm))
		 (-5 "Numerical error"))))
    (values ra dec r)))
  

 

;; alpha,delta from universal elements
#+slalib-starlink ;; does not exist in iraf slalib ;(
(sbdefine-alien-routine (#.(%slaname "sla_plantu") sla-plantu-raw) :void
    (mjd      :double  :copy)
    (elong    :double  :copy)
    (phi      :double  :copy)		       
    (u (* :double)     :in)
    (ra       :double  :out)
    (dec      :double  :out)
    (r        :double  :out)
    (jstat    #.*slalib-fortran-int-type* :out))



#+slalib-starlink ;; does not exist in iraf slalib ;(
(defunL sla-plantu (mjd elong phi jform uvec)
  "routine SLA_PLANTU, returning  RA,Dec,r for a vector of universal elements"
  (declare (type (integer 1 3) jform)
	   (type double-float mjd elong phi)
	   (type (simple-array double-float (13)) uvec))

  (with-array-as-foreign-pointer 
      (uvec puvec :double 
	    :copy-from-foreign nil 
	    :lisp-type double-float)
    (multiple-value-bind (dummy ra dec r jstat)
	(sla-plantu-raw
	 mjd elong phi puvec)
      (declare (ignorable dummy))
    (if (not (zerop jstat))
	(error "ERROR ~A in sla-plantu-raw: ~A"
	       jstat
	       (case jstat
		 (-1 (format nil "Radius zero"))
		 (-2 (format nil "Failed to converge" )))))
    (values ra dec r))))



(sbdefine-alien-routine (#.(%slaname "sla_planet") sla-planet-raw) :void
    (mjd      :double  :copy)
    (np       #.*slalib-fortran-int-type*  :copy) ;; number of planet 1-9
    (pv       (* :double)     :in)
    (jstat    #.*slalib-fortran-int-type* :out))

(defunL sla-planet (mjd nplanet pv &key (ignore-out-of-range nil))
  "Return position of planets (1=Mercury, 9=Pluto);
IGNORE-OUT-OF-RANGE ignores the out-of-range error, because
the in-range data for Pluto doesn't cover a whole orbit."
  (declare (type (integer 1 9) nplanet) 
	   (type double-float mjd)
	   (type (simple-array double-float (6)) pv))
  (with-array-as-foreign-pointer 
      (pv ppv :double :lisp-type double-float)
    (multiple-value-bind (dummy jstat)
	(sla-planet-raw mjd nplanet ppv)
      (declare (ignorable dummy))
      (if (and (not (zerop jstat))
	       (not (and ignore-out-of-range
			 (= jstat +1))))
	    (error "ERROR ~A in sla-planet-raw: ~A"
		   jstat
		   (case jstat
		     (+1 (format nil "Date outside range"))
		     (-1 (format nil "Illegal nplanet"))
		     (-2 (format nil "Failed to converge" )))))
	pv )))
    









;; convert [x,y,z,xdot,ydot,zdot] to alpha,delta,rdot,alphadot,deltadot
(sbdefine-alien-routine (#.(%slaname "sla_dc62s") sla-dc62s-raw) :void
     (v      (* :double))
     (a      :double :out)
     (b      :double :out)
     (r      :double :out)
     (ad     :double :out)
     (bd     :double :out)
     (rd     :double :out))


(defunL sla-dc62s (v)
  (declare (type (simple-array double-float *) v))
  (with-array-as-foreign-pointer 
   (v pv :double  :lisp-type double-float)
   (sla-dc62s-raw pv)))




;; given orbital elements, return PV=[x,y,z,xdot,ydot,zdot]
(sbdefine-alien-routine (#.(%slaname "sla_pv2el") sla-pv2el-raw) :void
    (pv       (* :double))
    (mjd      :double  :copy)
    (pmass    :double  :copy)
    (jformr   #.*slalib-fortran-int-type* :copy)
    (jform    #.*slalib-fortran-int-type* :out)
    (epoch    :double  :out)
    (orbinc   :double  :out)
    (anode    :double  :out)
    (perih    :double  :out)
    (aorq     :double  :out)
    (e        :double  :out)
    (aorl     :double  :out)
    (dm       :double  :out)
    (jstat    #.*slalib-fortran-int-type* :out))


(defunL sla-pv2el (pv mjdtt pmass jformr)
  "routine SLA_PV2EL, converting  PV=[x,y,z,xdot,ydot,zdot] as double float array
to orbital elements; PMASS is mass in solar units, or 0d0 for small bodies. JFORM
may not be same as JFORMR (ie, format of orbit may be changed)"
  (declare (type (integer 1 3) jformr)
	   (type double-float mjdtt pmass)
	   (type  (simple-array double-float (6)) pv))
  (multiple-value-bind (dummy jform epoch orbinc anode perih aorq e aorl dm jstat)
      (with-array-as-foreign-pointer 
       (pv ppv :double  :lisp-type double-float :copy-from-foreign nil)
       (sla-pv2el-raw ppv mjdtt pmass jformr))
    (declare (ignorable dummy))
    ;;
    (if (not (zerop jstat))
	(error "ERROR ~A in sla-pv2el-raw: ~A"
	       jstat
	       (case jstat
		 (-1 (format nil "Illegal PMASS=~A" pmass))
		 (-2 (format nil "Illegal JFORMR=~A" jformr))
		 (-3 (format nil "Out of range - PV=~A" pv))
		 (t  (format nil "error code JSTAT=~A" jstat)))))
    (values jform epoch orbinc anode perih aorq e aorl dm)))


;; given PV construct universal elements
(sbdefine-alien-routine (#.(%slaname "sla_pv2ue") sla-pv2ue-raw) :void
   (pv      (* :double))
   (date    :double :copy) ;; MJD-TT
   (pmass   :double :copy)
   (u       (* :double))
   (jstat    #.*slalib-fortran-int-type* :out))

(defunL sla-pv2ue (pv ue mjdtt pmass)
  "routine SLA_PV2UE, converting PV[0..5] as double float array to
double-float universal orbital elements UE[0..12]; PMASS is mass in
solar units, or 0d0 for small bodies."
  (declare (type double-float mjdtt pmass)
	   (type  (simple-array double-float (6))  pv)
	   (type  (simple-array double-float (13)) ue))
  (multiple-value-bind (dummy jstat)
      (with-arrays-as-foreign-pointers 
	  ((pv ppv  :double  :lisp-type double-float :copy-from-foreign nil)
	   (ue pue  :double  :lisp-type double-float :copy-from-foreign t))
	(sla-pv2ue-raw ppv mjdtt pmass pue))
    (declare (ignorable dummy))
    ;;
    (if (not (zerop jstat))
	(error "ERROR ~A in sla-pv2ue-raw: ~A"
	       jstat
	       (case jstat
		 (-1 (format nil "Illegal PMASS=~A" pmass))
		 (-2 "Too close to Sun.")
		 (-3 "Too slow.")
		 (t  (format nil "error code JSTAT=~A" jstat)))))
    ue))


;; given universal elements, get PV
(sbdefine-alien-routine (#.(%slaname "sla_ue2pv") sla-ue2pv-raw) :void
   (date    :double :copy) ;; MJD-TT
   (u       (* :double))
   (pv      (* :double))
   (jstat    #.*slalib-fortran-int-type* :out))

(defunL sla-ue2pv (ue pv mjdtt)
  "routine SLA_UE2PV, converting double float universal elements UE[0..12] 
to double float PV[0..5]"
  (declare (type double-float mjdtt)
	   (type  (simple-array double-float (6))  pv)
	   (type  (simple-array double-float (13)) ue))
  (multiple-value-bind (dummy jstat)
      (with-arrays-as-foreign-pointers 
	  ((pv ppv  :double  :lisp-type double-float :copy-from-foreign t)
	   (ue pue  :double  :lisp-type double-float :copy-from-foreign nil))
	(sla-ue2pv-raw mjdtt pue ppv))
    (declare (ignorable dummy))
    ;;
    (if (not (zerop jstat))
	(error "ERROR ~A in sla-ue2pv-raw: ~A"
	       jstat
	       (case jstat
		 (-1 "Radius vector zero.")
		 (-2 "Failed to converge.")
		 (t  (format nil "error code JSTAT=~A" jstat)))))
    pv))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perturb orbital elements
(sbdefine-alien-routine (#.(%slaname "sla_pertel") sla-pertel-raw) :void
    (jform     #.*slalib-fortran-int-type* :copy)
    (mjd0      :double  :copy)
    (mjd1      :double  :copy)
    ;;
    (epoch0    :double  :copy)
    (orbinc0   :double  :copy)
    (anode0    :double  :copy)
    (perih0    :double  :copy)
    (aorq0     :double  :copy)
    (e0        :double  :copy)
    (am0       :double  :copy)
    ;;
    (epoch1    :double  :out)
    (orbinc1   :double  :out)
    (anode1    :double  :out) 
    (perih1    :double  :out)  
    (aorq1     :double  :out)
    (e1        :double  :out)
    (am1       :double  :out)
    ;;
    (jstat     #.*slalib-fortran-int-type* :out))


(defunL sla-pertel (jform mjd0 mjd1 epoch0 orbinc0 anode0 perih0 aorq0 e0 am0)
  "Update the osculating elements of an asteroid or comet by applying 
planetary perturbations.

Warnings returned as JFORM=+102 (distant epoch) and JFORM=+101 (large
timespan) and JFORM=1..8 (coincident with large planet)"
  (declare (type (integer 2 3) jform)
	   (type double-float mjd0 mjd1 epoch0 orbinc0 anode0 perih0 aorq0 e0 am0))
  (multiple-value-bind (dummy epoch1 orbinc1 anode1 perih1 aorq1 e1 am1 jstat)
      (sla-pertel-raw jform mjd0 mjd1 epoch0 orbinc0 anode0 perih0 aorq0 e0 am0)
    (declare (ignore dummy))
    ;;
    (when (not (zerop jstat))
      (error  "ERROR ~A in sla-pertel-raw: ~A"
	      jstat
	     (case jstat
	       (-1 (format nil "Illegal JFORM=~A" jform))
	       (-2 (format nil "Illegal EO=~A" e0))
	       (-3 (format nil "Illegal AORQ0 =~A" aorq0))
	       (-4 "Internal error")
	       (-5 "Numerical error")
	       (102 "Distant epoch - WARNING only")
	       (101 "Excessively long timespan - WARNING only")
	       ((1 2 3 4 5 6 7 8)
		(format nil "Near planet ~A - WARNING only" jstat)))))

    (values epoch1 orbinc1 anode1 perih1 aorq1 e1 am1 jstat)))


;; perturb universal elements
(sbdefine-alien-routine (#.(%slaname "sla_pertue") sla-pertue-raw) :void
			(mjd-out :double :copy) ;; date
			(u (* :double)) ;; vector of 13 univ elememns
			(jstat     #.*slalib-fortran-int-type* :out))


(defunL sla-pertue (mjd-out u)
  "Perturb universal element U (length 13) to MJD-OUT.  
Warnings returned as JFORM=+102 (distant epoch) and JFORM=+101 (large
timespan) and JFORM=1..8 (coincident with large planet)"
  (declare (type double-float mjd-out)
	   (type (simple-array double-float (13)) u))
  (with-array-as-foreign-pointer (u u-ptr :double :lisp-type double-float)
    (multiple-value-bind (dummy jstat)
	(sla-pertue-raw mjd-out u-ptr)
      (declare (ignore dummy))
      (when (not (zerop jstat))
	(error  "ERROR ~A in sla-pertue-raw: ~A"
		jstat
		(case jstat
		  (-1 (format nil "Numerical error."))
		  (102 "Distant epoch - WARNING only")
		  (101 "Excessively long timespan - WARNING only")
		  ((1 2 3 4 5 6 7 8)
		   (format nil "Near planet ~A - WARNING only" jstat)))))
      (values u jstat))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert conventional elements to universal elements
(sbdefine-alien-routine (#.(%slaname "sla_el2ue") sla-el2ue-raw) :void
    (mjd      :double  :copy)
    (jform    #.*slalib-fortran-int-type*    :copy)
    (epoch    :double  :copy)
    (orbinc   :double  :copy)
    (anode    :double  :copy)
    (perih    :double  :copy)
    (aorq     :double  :copy)
    (e        :double  :copy)
    (aorl     :double  :copy)
    (dm       :double  :copy)
    ;;
    (velem    (* :double))
    ;;
    (jstat    #.*slalib-fortran-int-type* :out))

(defunL sla-el2ue (mjd jform epoch orbinc anode perih aorq e aorl dm velem)
  "Convert conventional osculating elements 
     MJD JFORM EPOCH ORBINC ANODE PERIH AORQ E AORL DM
into 13 Universal Elements in VELEM.  All input angles are in RADIANS."
  (declare (type (integer 1 3) jform)
	   (type double-float mjd epoch orbinc anode perih aorq e aorl dm)
	   (type (simple-array double-float (13)) velem))
  (fill velem 0d0) ;; to un-confuse WAAF in case this lisp doesn't have types
  (multiple-value-bind (dummy jstat)
      (with-array-as-foreign-pointer 
	  (velem pvelem :double :lisp-type double-float)
	(sla-el2ue-raw mjd jform epoch orbinc anode perih aorq e aorl dm
		       pvelem))
    (declare (ignore dummy))
    ;;
    (when (minusp jstat)
      (error "ERROR in sla-el2ue-raw: ~A"
	     (case jstat
	       (-1 (format nil "Illegal JFORM=~A" jform))
	       (-2 (format nil "Illegal E=~A" e))
	       (-3 (format nil "Illegal AORQ =~A" aorq))
	       (-4 (format nil "Illegal DM =~A" dm))
	       (-5 "Numerical error (unknown)") ;; this is gone
	       ;; these are our additions to error reporting in el2ue.f
	       (-11 "Numerical error - null radius vector in UE2PV")
	       (-12 "Numerical error - convergence failure in UE2PV")
	       (-21  "Numerical error - negative PMASS in PV2UE")
	       (-22  "Numerical error - too close in PV2UE")
	       (-23  "Numerical error - too slow in PV2UE")
	       (t  (format nil "Unknown error ~A" jstat)))))
    ;;
    velem))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert universal elements to conventional elements
(sbdefine-alien-routine (#.(%slaname "sla_ue2el") sla-ue2el-raw) :void
    (velem    (* :double)) 
    (jformr   #.*slalib-fortran-int-type* :copy)  ;; universal elements requested
    (jform    #.*slalib-fortran-int-type* :out) ;; universal elements actually returned
    (epoch    :double  :out)
    (orbinc   :double  :out)
    (anode    :double  :out)
    (perih    :double  :out)
    (aorq     :double  :out)
    (e        :double  :out)
    (aorl     :double  :out)
    (dm       :double  :out)
    ;;
    (jstat    #.*slalib-fortran-int-type*  :out))

(defunL sla-ue2el (velem jformr)
  "Convert universal elements VELEM to conventional elements
 (VALUES  orbinc anode perih aorq e aorl dm).
Outputs are RADIANS."
  (declare (type (integer 1 3) jformr)
	   (type  (simple-array double-float (13)) velem))
  
  (multiple-value-bind (dummy jform epoch orbinc anode perih aorq e aorl dm jstat)
      (with-array-as-foreign-pointer 
	  (velem pvelem :double :lisp-type double-float)
	(sla-ue2el-raw pvelem jformr))
    (declare (ignorable dummy))
    (when (not (= jform jformr))
      (error "requested elements type ~A but got ~A" jformr jform))
    (when (minusp jstat)
      (error "ERROR in sla-ue2el-raw: ~A"
	     (case jstat
	       (-1 (format nil "Illegal PMASS"))
	       (-2 (format nil "Illegal JFORMR=~A" jformr))
	       (-3 (format nil "Position or velocity out of range")))))
    ;;
    (values epoch orbinc anode perih aorq e aorl dm)))


    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compute precession matrix
(sbdefine-alien-routine (#.(%slaname "sla_prenut") sla-prenut-raw) :void
    (epoch :double :copy)
    (mjd   :double :copy)
    (rmat  (* :double)))

(defunL sla-prenut (epoch mjd rmat)
  "Compute precession+nutation matrix, placing it into RMAT"
  (declare (type double-float epoch mjd)
	   (type (simple-array double-float (9)) rmat))
  (fill rmat 0d0)
  (with-array-as-foreign-pointer 
      (rmat prmat :double  :lisp-type double-float)
    (sla-prenut-raw epoch mjd prmat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apply rotation dm to va putting result in vb - by Fortran standard
;; va and vb should not be the same
(sbdefine-alien-routine (#.(%slaname "sla_dmxv") sla-dmxv-raw) :void
    (dm (* :double))
    (va (* :double))
    (vb (* :double)))

(defunL sla-dmxv (dm va vb)
  "apply rotation dm to va putting result in vb"
  (declare (type (simple-array double-float (9)) dm)
	   (type (simple-array double-float (3)) va vb))
  (fill vb 0d0) ;; in case this lisp doesn't have typed arrays
  (with-arrays-as-foreign-pointers 
      ((dm pdm :double :lisp-type double-float :copy-from-foreign nil)
       (va pva :double :lisp-type double-float :copy-from-foreign nil)
       (vb pvb :double :lisp-type double-float :copy-from-foreign t))
    (sla-dmxv-raw pdm pva pvb)))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Our added routine to convert PV to ra,dec,r
;;
(sbdefine-alien-routine (#.(%slaname "sla_PVRD") sla-pv2radecr-raw) :void
   (mjd   :double :copy)
   (elong :double :copy)
   (phi   :double :copy)
   (height   :double :copy)			
   (pv    (* :double))
   (ra    :double :out) 
   (dec   :double :out)
   (r     :double :out))

 
(defunL pv-to-radecr (mjd elong phi height pv) 
  "Convert position-velocity vector PV at time MJD observed from point
ELONG and PHI (radians) and HEIGHT (meters) on surface to J2000 RA,DEC
in radians and R from observer in AU.  Returns (values RA/radians
DEC/radians R/AU).  Does a linear light-travel correction using the
velocity to figure out where the object was at MJD (see PV2RADEC.f)."
  (declare (type double-float mjd elong phi)
	   (type (simple-array double-float (6)) pv))
  (with-array-as-foreign-pointer
      (pv ppv :double   :lisp-type double-float)
    (multiple-value-bind (dummy ra dec r)
	(sla-pv2radecr-raw mjd elong phi height ppv)
      (declare (ignorable dummy))
      (values ra dec r))))


;; obsolete name for  sla-pv2radecr 
(defunL sla-pv2-radec (mjd elong height phi pv) 
  (format t "Don't use this routine - use pv-to-radecr~%")
  (pv-to-radecr mjd elong phi height pv))


(defun radecr-to-pv/heliocentric (ra dec r &key (pv nil))
  "Given RA,DEC in radians and heliocentric R in AU, create a PV
vector with the velocity fields blank. There is no adjustment
for light travel time, which could be obtained from the time
derivatives d/dt(ra,dec,r)."
  (declare (type (or null (simple-array double-float (6))) pv))
  (let* ((pv (or pv (make-array 6 :element-type 'double-float :initial-element 0d0)))
	 (cosra (cos ra))
	 (sinra (sin ra))
	 (cosdec (cos dec))
	 (sindec (sin dec))
	 ;;
	 (x (* r cosra cosdec))
	 (y (* r sinra cosdec +1))  ;; plus is SLALIB convention
	 (z (* r sindec)))
    ;;
    (setf (aref pv 0) x
	  (aref pv 1) y
	  (aref pv 2) z)
    pv))

(defun  radecr-to-pv/geocentric (ra dec r mjd &key (pv nil))
   "Given RA,DEC in radians and geocentric R in AU and an MJD, create
a PV vector with the velocity fields blank.  Works by adding the
heliocentric x,y,z to the earth's x,y,z. 

WARNING, does not adjust for observatory location. It is geo-CENTRIC.
Also, there is no adjustment for light travel time, which could be obtained
from the time derivatives d/dt(ra,dec,r). Thus it is not quite the inverse of
SLA-PV2RADECR.  Given the absence of a light-time correction, there's no 
point in correcting from geocentric to observatory coordinates."
       ;; compute position of the earth in helio-coords
    ;; the 4th column of ah is the position of the earth
    (multiple-value-bind (vb pb vh ph)
	;; use high precsion if avail.  What does it mean to use julian
	;; epoch of mjd (indicated by -1)?
	#-slalib-iraf (sla-epv mjd)  
	#+slalib-iraf (sla-evp mjd -1d0)
	(declare (ignorable vb vh pb))
	(let ((xe (aref ph 0))
	      (ye (aref ph 1))
	      (ze (aref ph 2)))
	  ;; pv is now r-length vector in ra,dec direction
	  (let ((pv (radecr-to-pv/heliocentric ra dec r :pv pv)))
	    ;; now add earths position, so that pa is sum of vector from
	    ;; sun to earth then r-length vector in ra,dec direction
	    (incf (aref pv 0) xe)
	    (incf (aref pv 1) ye)
	    (incf (aref pv 2) ze)
	    pv))))
    

 
;; observed to apparent - lots of stuff
(sbdefine-alien-routine (#.(%slaname "sla_OAP") sla-oap-raw) :void
   (type :string)
   (ob1 :double :copy)		       
   (ob2 :double :copy)		       
   (date :double :copy)
   (dut :double :copy)
   (elongm :double :copy)
   (phim :double :copy)		       
   (hm :double :copy)
   (xp :double :copy)
   (yp :double :copy)
   (tdk :double :copy)
   (pmb :double :copy)
   (rh :double :copy)
   (wl :double :copy)
   (tlr :double :copy)
   (rap :double :out)
   (decp :double :out))


(defunL sla-oap (type ob1 ob2 date dut elongm phim
	        hm xp yp tdk pmb rh wl tlr)
  "Convert observe coordinate to apparent - return (values ra dec)"
  (declare (type string type)
	   (type double-float ob1 ob2 date dut ob1 date dut elongm phim
		 hm xp yp tdk pmb rh wl tlr))
  (multiple-value-bind (dummy ra dec)
      (sla-oap-raw type ob1 ob2 date dut elongm phim
		   hm xp yp tdk pmb rh wl tlr)
    (declare (ignore dummy))
    (values ra dec)))
  
   



;; convert geocentric apparent to mean
(sbdefine-alien-routine (#.(%slaname "sla_AMP") sla-amp-raw) :void
    (ra :double :copy)   ;; input RA,DEC apparent
    (da :double :copy)
    (date :double :copy) ;; MJD for apparent place
    (eq :double :copy)   ;; EQUINOX for mean
    (rm :double :out)    ;; output ra and dec MEAN
    (dm :double :out))




(defunL sla-amp (ra-app dec-app date-app equinox-mean)
  "Convert apparent RA-APP, DEC-APP at DATE-APP to mean RA,DEC
at EQUINOX-MEAN; return (VALUES RA-MEAN DEC-MEAN) - for example,
if EQUINOX-MEAN is 2000d0, then return J2000."
  (declare (type double-float ra-app dec-app date-app equinox-mean))
  (multiple-value-bind (dummy ra dec)
      (sla-amp-raw ra-app dec-app date-app equinox-mean)
    (declare (ignore dummy))
    (values ra dec)))

;; convert mean to geocentric apparent 
(sbdefine-alien-routine (#.(%slaname "sla_MAP") sla-map-raw) :void
    (rm :double :copy)
    (dm :double :copy)
    (pr :double :copy)
    (pd :double :copy)
    (px :double :copy)
    (rv :double :copy)
    (eq :double :copy)
    (date :double :copy)
    (ra :double :out)
    (da :double :out))


(defunL sla-map (ra-mean dec-mean ra-prop-vel dec-prop-vel
		parallax rv equinox-mean date)
  "Given:
  RA-MEAN DEC-MEAN EQUINOX-MEAN    - mean position and MJD at equinox
  RA-PROV-VEL  and DEC-PROP-VEL    - proper motions in arcsec/yr
    where RA-PROP-VEL is d/dt RA, not d/dt (RA cos(DEC)) and
  PARALLAX                         - parallax in arcsec
  RV                               - radial velocity in km/s with
                                     meaning receding
  DATE                             - MJD date of observation

return (VALUES RA DEC) at DATE"
  (declare (type double-float ra-mean dec-mean ra-prop-vel dec-prop-vel
		 parallax rv equinox-mean date))
  (multiple-value-bind (dummy ra dec)
      (sla-map-raw ra-mean dec-mean ra-prop-vel dec-prop-vel
		   parallax rv equinox-mean date)
    (declare (ignore dummy))
    (values ra dec)))




;; airmass as function of zenith distance
(sbdefine-alien-routine (#.(%slaname "sla_AIRMAS") sla-airmas-raw) :double
    (zd :double :copy))


;; convert HA, DEC, latitude to zenith distance
(sbdefine-alien-routine (#.(%slaname "sla_ZD") sla-zd-raw) :double
    (ha :double :copy)   ;; hour angle in rad
    (dec :double :copy)  ;; dec in rad
    (phi :double :copy)) ;; latitude in rad



;; greenwich mean sidereal time in radians
(sbdefine-alien-routine  (#.(%slaname "sla_GMSTA") sla-gmsta-raw) :double
   (date :double :copy) ;; integer part of mjd
   (ut1 :double :copy)) ;; fraction part of mjd - actually, can be mixed 

;; equation of equinoxes
(sbdefine-alien-routine  (#.(%slaname "sla_EQEQX") sla-eqeqx-raw) :double
   (date :double :copy)) ;; mjd


;; convert hour angle, dec, longitude to az,alt
(sbdefine-alien-routine  (#.(%slaname "sla_DE2H") sla-de2h-raw) :void
   (ha  :double :copy)  ;; hour angle, rad
   (dec :double :copy) ;; dec, rad
   (phi :double :copy) ;; latitude, rad
   (az  :double :out)  ;; elevation, rad
   (el  :double :out)) ;; elevation, rad



;; convert P (geodetic longitude, radians) and H (altitude above
;; reference spheroid) to R (distance from earth's axis) and Z
;; (distance above axis, both in AU.  This is mostly useful for
;; computing a geocentric observatory.
(sbdefine-alien-routine  (#.(%slaname "sla_GEOC") sla-geoc-raw) :void
   (p  :double :copy)
   (h  :double :copy)
   (r  :double :out)
   (z  :double :out))
			    
			    


;; this is verified to work using horizons, down to a couple of seconds
;; with ut1-utc being uncertain
(defun compute-local-sidereal-time (mjd east-longitude &key (ut1-utc 0d0))
  "Compute the local apparent sidereal time, from the mjd and
  longitude (east, deg)

MJD is assumed to be coordinated universal time UTC.
UT1-UTC is UT1 - UTC, a small earth rotation correction factor.
Note that we're not really too sure of the finer corrections here.
Return sidereal time in HOURS (not radians)."
  (let* ((mjd (+ (float mjd 1d0) ;; mjd corrected for ut1-tc
		 (/ (float ut1-utc 1d0) #.(* 24 3600))))
	 (lon (float east-longitude 1d0))
	 (lon/rad (* lon #.(/ pi 180))) ;; lon in radians
	 ;; greenwich mean sidereal time in radians
	 (gmsta (multiple-value-bind (date ut1frac)
		   (floor mjd)
		 (sla-gmsta-raw date ut1frac)))
	 ;; eq of equinoxes in radians
	 (eqeqx (sla-eqeqx-raw mjd))
	 ;; final answer of local time in radians
	 (lst (+ gmsta lon/rad eqeqx))
	 ;; in hours
	 (lsth (* lst #.(/ 12 pi))))
    ;; put it into correct range using remainder component of FLOOR
    (setf lsth (nth-value 1 (floor lsth 24)))
    lsth))

    
(defun compute-hour-angle-for-ra (ra mjd east-longitude  &key (ut1-utc 0d0))
  "Compute the apparent hour angle for RA at time MJD at
  EAST-LONGITUDE in degrees

UT1MUTC is UT1 - UTC, a small earth rotation correction factor."
  (let ((lst (compute-local-sidereal-time 
	      mjd east-longitude :ut1-utc ut1-utc)))
    (let ((ha (nth-value 1
			 (floor (- lst (* ra #.(/ 24d0 360d0)))
				24))))
      ;; HA is how between 0 and 24
      ;; fix the hour angle to be between -12 and 12
      (cond ((> ha 12)
	     (- ha 24))
	    (t
	     ha)))))
    




;; check this again with more values near and far from horizon - seems to
;; break down near horizon

(defun compute-altaz-for-apparent-radec 
    (ra-app dec-app  mjd east-longitude latitude &key (ut1-utc 0d0))
"Compute the ALT,AZ (deg) for a given apparent RA-APP,DEC-APP (deg),
an MJD, an EAST-LONGITUDE and LATITUDE (deg).  

Return (VALUES ALT/DEG AZ/DEG).

Note that RA-APP and DEC-APP are apparent values, and not the
astrometric values generally returned by SLALIB.

This is decent (0.001 deg) agreement with JPL Horizons when using JPL
apparent RA,DEC as input, but becomes worse when atmospheric
effects (and aberration) are ignored by using astrometric RA,DEC."
  (let ((ha (compute-hour-angle-for-ra ra-app mjd east-longitude :ut1-utc ut1-utc)))
    ;; when we set our HA to be equal to JPL's, the agreement does not
    ;; improve, suggesting the disagreement lies in sla-de2h
    (multiple-value-bind (dummy az/rad alt/rad)
	(sla-de2h-raw 
	 (* ha #.(/ pi 12))
	 (* dec-app #.(/ pi 180))
	 (* latitude #.(/ pi 180)))
      (declare (ignore dummy))
      ;; convert to deg
      (values
       (* alt/rad #.(/ 180 pi))
       (* az/rad #.(/ 180 pi))))))




;; verified to work using a couple of jpl horizons values
(defun compute-airmass-for-ra-dec
    (ra dec  mjd east-longitude latitude &key (ut1-utc 0d0))
  "Given an RA,DEC (deg), MJD, EAST-LONGITUDE and LATITUDE (deg), return the
airmass.  Returns (VALUES AIRMASS HOUR-ANGLE)"
  (let* ((ha (compute-hour-angle-for-ra ra mjd east-longitude :ut1-utc ut1-utc))
	 (zd (sla-zd-raw  ;; zd is now in rad
	      (* ha #.(/ pi 12))
	      (* dec #.(/ pi 180))
	      (* latitude #.(/ pi 180))))
	 (airmass (sla-airmas-raw zd)))
    (values airmass ha)))
      



 

	
    

(sbdefine-alien-routine  (#.(%slaname "sla_dmoon") sla-dmoon-raw) :void	 
   (date :double :copy) ;; mjd
   (pv (* :double)))


(defunL sla-dmoon (mjd &optional pv)
  "Given MJD, return its PV vector [x,y,z,xdot,ydot,zdot] in AU, AU/s
in vector PV (which may be supplied).  Return PV. This is in
GEOCENTRIC units."
  (declare (type double-float mjd)
	   (type (or null (simple-array double-float (6))) pv))
  (let ((pv (%6vec pv)))
    (with-array-as-foreign-pointer 
	(pv ppv :double  :lisp-type double-float)
      (sla-dmoon-raw mjd ppv))
    pv))



(sbdefine-alien-routine  (#.(%slaname "sla_rdplan") sla-rdplan-raw) :void	 
   (date  :double :copy) ;; mjd
   (np    #.*slalib-fortran-int-type*    :copy)
   (elong :double :copy) ;; obs e longitude
   (phi   :double :copy) ;; obs latitude
   (ra    :double :out)
   (dec   :double :out)
   (diam  :double :out)) ;; angular diameter, radians


(defunL sla-rdplan (mjd np elong phi)
  (declare (type real mjd elong phi)
	   (type (signed-byte 32) np))
  "Computes position of planet number NP and returns
  (values ra/rad dec/rad diameter/rad), at MJD from 
east longitude ELONG and latitude PHI.  All angles are 
in radians.  Planets are NP=

  1=Mercury  2=Venus    3=Moon     4=Mars   5=Jupiter
  6=Saturn   7=Uranus   8=Neptune  9=Pluto  Else=Sun"
  (multiple-value-bind (dummy ra dec diameter)
      (sla-rdplan-raw (float mjd 1d0) np (float elong 1d0)
		      (float phi 1d0))
  (declare (ignore dummy))
    (values ra dec diameter)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at end, test if we're OK by checking coordinates of Mars
(eval-when (:load-toplevel)
  ;;
  ;; handle a very specific glitch in SBCL on OSX x86 with OpenCore
  ;; that causes a math exception on certain initial FFI calls
  #+(and sbcl darwin x86-64) (sb-int:get-floating-point-modes)
  ;;
  (multiple-value-bind (year month day sec)
      (slalib::sla-djcl 54500.5d0)
    (when (not (and (= year 2008)
		    (= month 2)
		    (= day 4)
		    (= sec 0.5d0)))
      (error "ERROR - (slalib::sla-djcl 54500.5d0) did not evaluate
to (values 2008 2 4 0.5d0).  It may be that the compile-time variable
slalib::*slalib-fortran-int-type* (reflecting whether a Fortran integer 
is a LONG or an INT) is not correct for your lisp implementation 
and compilation of slalib.  You may need to insert another special case.
Perhaps lislalib.so or libslalib.dylib is not compiled with 8-byte integers.")))
  ;;
  ;; check position of mars
  (multiple-value-bind (ra dec diam)
      (sla-rdplan 54500.5d0 4 100d0 10d0)
    (when (not (and (< (abs (- ra 1.457694117613156d0)) 0.0000001d0)
		    (< (abs (- dec 0.4645891105129919d0)) 0.0000001d0)
		    (< (abs (- diam 5.6361733929901924d-5)) 0.0000001d-5)))
      (error "Position of Mars given by  
  (slalib::sla-rdplan 54500.5d0 4 100d0 10d0) 
was not as expected.
Expected ra=1.457694117613156d0 
         dec=0.4645891105129919d0  
         diameter=5.6361733929901924d-5 (radians)
and got  ra=~A 
         dec=~A 
         diam=~A
Check your slalib. Perhaps lislalib.so or libslalib.dylib is not compiled 
with 8-byte integers." ra dec diam))))
		    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

