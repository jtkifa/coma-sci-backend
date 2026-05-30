
#|

EMMI is a past instrument on 3.5m  NTT, working in imaging and spectroscopy.

It was decommissioned in 1999 or so

Documentation is sparse; ESO page is a dead link

https://web.archive.org/web/20150216015758/https://www.eso.org/public/teles-instr/lasilla/ntt/emmi/
https://web.archive.org/web/20150306090738/http://www.ls.eso.org/sci/facilities/lasilla/instruments/emmi/

There are two modes, RILD and BIMG, denoted by keyword "HIERARCH ESO INS MODE"
http://www.ls.eso.org/sci/facilities/lasilla/instruments/emmi/emmiGeneralDescription.html

It appears that RILD in some variant has 2 chips, almost next to each, but rotated by a tiny bit
 http://www.ls.eso.org/sci/facilities/lasilla/instruments/emmi/emmiImagingChar.html
This is hard to reconcile with the table below


SUSI2 info: https://web.archive.org/web/20250308123213/https://www.eso.org/public/spain/teles-instr/lasilla/ntt/susi2/

And ESO docs that seem to be alive still:


     https://www.ls.eso.org/lasilla/sciops/ntt/emmi/index.html (in particular .../emmiDetectors.html )
     https://www.ls.eso.org/lasilla/sciops/ntt/susi/index.html



PDF manual: https://www.eso.org/sci/libraries/historicaldocuments/Operating_Manuals/Operating_Manual_No.15_Sept_1992_A1b.pdf

From page 6, S-2.4 of manual, there are several chips:

--------------------------------------------------------------------------------
                                    pixel size           seen? Nchips  NHDUs
Inst cfg     CCD         Camera    um    "     FOV/'    
 
EMMI-RILD    THX 1024     F/2.5    19   0.44   7.6x7.6     n     1?    ?
EMMI-RILD    FA 2048      F/2.5    15   0.35   10x10       Y     2     2,4  ;; scale is for 2x2!!
EMMI-RILD    TEK 2048     F/5.3    24   0.26   8.9x8.9     Y     1     1

EMMI-BIMG    THX 1024     F/4      19   0.29   4.9x4.9     n     1?    ?
EMMI-BIMG    TEK 1024     F/4      24   0.37   6.2x6.2     Y     1?    1

SUSI         Tek 1024              24   0.13   2.2x2.2     n     1?    ?

SUSI2        EEV44-80              15   0.0805 5.5x5.7     Y     2     1,2
---------------------------------------------------------------------------------

SUSI2 is a two-chip mosaic, not just multiple amps reading one chip.
      It is not in the PDF manual above.  Unfortunately, early versions
      put both chips on one HDU, while latter versions had two HDUs, hence
      it has to be split into two instruments



Whether a raw file is a single chip or a multichip depends on whether
it has more than one extension.

After processing, a file is a multichip or single-chip based on the
number of physical chips 

-------

CHIP orientation

 EMMI:
   https://www.ls.eso.org/lasilla/sciops/ntt/emmi/emmiDetectors.html#orientation
   - for one variant of EMMI, for rotator angle of 0:
       - BIMG was N-left, E-bottom, and rotator of 90 makes it N-down, E-right
       - RILD was N-down, E-right,  and rotator of 90 makes it N-right, E-up
   - There is a chip gap of 47 pixels, or 7.82".
   - The left chip is only half-exposed in imaging mode, so extension 1 (of four)
     is of little use.
   - the optical axis, however, is offset about 0.18 chip widths to right 
     (into chip 2), which is 61.3", or 
   

 SUSI2: 
  https://www.ls.eso.org/lasilla/sciops/ntt/susi/docs/SUSIccds.html
  for rotator angle of zero,
    - SUSI2 was N-up, E-right, and rotator of 90 makes it N-right, E-down
  There is a chip gap of 96 pixels, or 7.7 arcsec
  
In every object image (not bias or flat, necessarily) there seems to be a header
"HIERARCH ESO ADA ABSROT START" for the rotator angle.  There is also a keyword
"HIERARCH ESO ADA POSANG" but this seems to be zero often, even when ABSROT
is not 0 or 90.


|#

(defpackage instrument-id/eso-ntt-emmi+susi
  (:use #:cl #:instrument-id)
  (:import-from #:instrument-id #:%gethead-or-error)
  (:export
   ;; export normally private inner classes for benefit of imred package
   #:%eso-ntt-emmi+susi
   #:%eso-ntt-emmi
   #:%eso-ntt-susi-x
   #:%eso-ntt-emmi-bimg
   #:%eso-ntt-emmi-rild
   #:%emmi-susi-raw
   #:%emmi-susi-reduced
   #:%eso-ntt-susi
   #:%eso-ntt-susi2
   #:%eso-ntt-emmi-rild-thx1024
   #:%eso-ntt-emmi-rild-fa2048
   #:%eso-ntt-emmi-rild-tek2048
   #:%eso-ntt-emmi-bimg-thx1024
   #:%eso-ntt-emmi-bimg-tek1024
   
   ))

(in-package instrument-id/eso-ntt-emmi+susi)
;;(in-package instrument-id)

;; parent class - avoid making it a onechip or multichip 
(defclass/inst %eso-ntt-emmi+susi (imaging-instrument) ;; can be multichip or onechip
    ((name :initform "ESO-NTT-EMMI") 
     (observatory :initform "esontt")
     ;; just guessing
     (saturation-level :initform 65535)
     (non-linear-level :initform 65535)
     (aperture  :initform 3.5)
     (has-overscan :initform t)
     ;; have a field for the pixel scale with binning=1 to simplify pixel scale
     (%chip-id) 
     (%pix-scale-bin1 :accessor %pix-scale-bin1)
     (%xbinning :accessor %xbinning)
     (%ybinning :accessor %ybinning)
     ;; number of chips 
     (%nchips  :accessor %nchips)
     (%nexts   :accessor %nexts)))

 
;; general EMMI or SUSI
;;   EMMI abstract class
(defclass/inst %eso-ntt-emmi (%eso-ntt-emmi+susi) ())
;;   SUSI abstract class, x can be original susi or sus2
(defclass/inst %eso-ntt-susi-x (%eso-ntt-emmi+susi) ()) 
 
;; EMMI divides into BIMG and RILD
;;   blue side 0.37"/pix and 6.2'x6.2' FOV
(defclass/inst %eso-ntt-emmi-bimg (%eso-ntt-emmi) ()) 
;;   red side  0.1665"/pix and 9.1'x9.9' FOV
(defclass/inst %eso-ntt-emmi-rild (%eso-ntt-emmi) ())

;; a mixin to denote that it's a raw image
(defclass %emmi-susi-raw () ())
;; a mixin to denote that it's reduced
(defclass %emmi-susi-reduced () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specific instruments - these are still abstract becase they can
;; be raw or reduced
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;;;;;;;;;;;;;;
;; SUSI and SUSI2
;;   the parent
(defclass/inst %eso-ntt-susi   (%eso-ntt-susi-x) ;; SUSI - not seen
  ((%pix-scale-bin1 :initform 0.13)
   (%chip-id :initform "TEK1024")))
;;   we GUESS that SUSI (original) raw is one HDU
(defclass/inst eso-ntt-susi/raw   (%eso-ntt-susi %emmi-susi-raw onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))
;; more confident that SUSI reduced is onechip
(defclass/inst eso-ntt-susi/reduced  (%eso-ntt-susi %emmi-susi-reduced onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))

;;;;;;;;;;;;;;;;

(defclass/inst %eso-ntt-susi2  (%eso-ntt-susi-x)  ;; SUSI2 (2x FOV) - seen this
  ((%pix-scale-bin1 :initform 0.0805)
   (%chip-id :initform "EEV4480")))
;; version of SUSI2 with 1 HDU, and 2 chips on it - must be treated as onechip
;; when reducing this, it should be split into 2 ext version
(defclass/inst eso-ntt-susi2/raw1ext  (%eso-ntt-susi2 %emmi-susi-raw onechip)
   ((%nchips :initform 2)
    (%nexts  :initform 1)))
;; version of SUSI2 with 2 HDU, each with one chip
(defclass/inst eso-ntt-susi2/raw2ext  (%eso-ntt-susi2 %emmi-susi-raw multichip)
   ((%nchips :initform 2)
    (%nexts  :initform 2)))
;; the reduced version will always separate chips
(defclass/inst eso-ntt-susi2/reduced  (%eso-ntt-susi2 %emmi-susi-reduced multichip)
   ((%nchips :initform 2)
    (%nexts  :initform 2)))
;; one chip from reduces version
(defclass/inst eso-ntt-susi2/reduced-onechip  (%eso-ntt-susi2 %emmi-susi-reduced multichip)
   ((%nchips :initform 1)
    (%nexts  :initform 1)))


;;;;;;;;;;;;;;;;

;; various chips of EMMI-RILD

;;   GUESS that THX1024 is onechip, with one HDU - we've never seen it
(defclass/inst %eso-ntt-emmi-rild-thx1024 (%eso-ntt-emmi-rild)   ;; NOT SEEN
  ((%pix-scale-bin1 :initform 0.44)
   (%chip-id :initform "THX1024")))
(defclass/inst eso-ntt-emmi-rild-thx1024/raw
  (%eso-ntt-emmi-rild-thx1024 %emmi-susi-raw onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))
(defclass/inst eso-ntt-emmi-rild-thx1024/reduced
  (%eso-ntt-emmi-rild-thx1024 %emmi-susi-reduced onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMI RILD FA2048 can have 2 or 4 HDUS for 2 chips
(defclass/inst %eso-ntt-emmi-rild-fa2048 (%eso-ntt-emmi-rild)  
  ((%pix-scale-bin1 :initform 0.1674) ;; do docs say it is 0.35, assuming 2x2?
   (%chip-id :initform "FA2048")))
;; two types of raw (2 and 4 ext)
;;   the 2ext is actually 4 amps so it will have to be split into 4ext before processing
(defclass/inst eso-ntt-emmi-rild-fa2048/raw2ext
  (%eso-ntt-emmi-rild-fa2048 %emmi-susi-raw multichip)
    ((%nchips :initform 2)
     (%nexts  :initform 2)))
(defclass/inst eso-ntt-emmi-rild-fa2048/raw4ext
  (%eso-ntt-emmi-rild-fa2048 %emmi-susi-raw multichip)
  ((%nchips :initform 2)
   (%nexts  :initform 4)))
;; just one type of reduced, with 2 chips
(defclass/inst eso-ntt-emmi-rild-fa2048/reduced
  (%eso-ntt-emmi-rild-fa2048 %emmi-susi-reduced multichip)
  ((%nchips :initform 2)
   (%nexts  :initform 2)))
;; separating out chips
(defclass/inst eso-ntt-emmi-rild-fa2048/reduced-onechip
  (%eso-ntt-emmi-rild-fa2048 %emmi-susi-reduced onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))

;;;;;;;;;;;;;;;;;;;;;;;
;; for completeness, extracted raw extensions
(defclass/inst eso-ntt-emmi-rild-fa2048/raw4ext-onechip
  (%eso-ntt-emmi-rild-fa2048 %emmi-susi-raw onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))

(defclass/inst eso-ntt-emmi-rild-fa2048/raw2ext-onechip
  (%eso-ntt-emmi-rild-fa2048 %emmi-susi-raw onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EMMI RILD TEK 2048 has 1 chip, 1 HDU
(defclass/inst %eso-ntt-emmi-rild-tek2048 (%eso-ntt-emmi-rild) 
  ((%pix-scale-bin1 :initform 0.26)
   (%chip-id :initform "TEK2048")))
(defclass/inst eso-ntt-emmi-rild-tek2048/raw
  (%eso-ntt-emmi-rild-tek2048 %emmi-susi-raw onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))
(defclass/inst eso-ntt-emmi-rild-tek2048/reduced
  (%eso-ntt-emmi-rild-tek2048 %emmi-susi-reduced onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))

;; various chips of EMMI-BIMG
;;  we GUESS that 1024 chips are 1 ext, 1 chip because we haven't see this instd
(defclass/inst %eso-ntt-emmi-bimg-thx1024 (%eso-ntt-emmi-bimg)   ;; NOT SEEN
  ((%pix-scale-bin1 :initform 0.29)
   (%chip-id :initform "THX1024")))
(defclass/inst eso-ntt-emmi-bimg-thx1024/raw
  (%eso-ntt-emmi-bimg-thx1024 %emmi-susi-raw onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))
(defclass/inst eso-ntt-emmi-bimg-thx1024/reduced
  (%eso-ntt-emmi-bimg-thx1024 %emmi-susi-reduced  onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))


(defclass/inst %eso-ntt-emmi-bimg-tek1024 (%eso-ntt-emmi-bimg)   
  ((%pix-scale-bin1 :initform 0.37)
   (%chip-id :initform "TEK1024")))
(defclass/inst eso-ntt-emmi-bimg-tek1024/raw
  (%eso-ntt-emmi-bimg-tek1024 %emmi-susi-raw onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))
(defclass/inst eso-ntt-emmi-bimg-tek1024/reduced
  (%eso-ntt-emmi-bimg-tek1024 %emmi-susi-reduced onechip)
  ((%nchips :initform 1)
   (%nexts  :initform 1)))


;; read header in first or 2nd ext
(defun %read-fits-header-1or2 (fits-file keyword)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (or (cf:read-fits-header ff keyword :extension 1)
	(and (> (cf:fits-file-num-hdus ff) 1)
	     (cf:read-fits-header ff keyword :extension 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  



;; these fits files are messy because some have multiple extensions, some don't
(defun %eso-ntt-emmi+susi-identify-instrument (fits-file)
  (flet ((%safe-round (x) (if (realp x) (round x) x)))
    (cf:maybe-with-open-fits-file (fits-file ff :mode :input)
      (let ((instrum   (%read-fits-header-1or2 ff "INSTRUME")) ;; EMMI/2.6
	    (telescop  (%read-fits-header-1or2 ff "TELESCOP")) ;; ESO-NTT
	    (dpr-tech  (%read-fits-header-1or2 ff "HIERARCH ESO DPR TECH"))
	    (ins-mode  (%read-fits-header-1or2 ff "HIERARCH ESO INS MODE"))
	    (chip-name (or (%read-fits-header-1or2 ff "HIERARCH ESO DET CHIP1 NAME")
			   (%read-fits-header-1or2 ff "HIERARCH ESO DET CHIP2 NAME")
			   ;; needed for some versions of EMMI-REMD
			   (%read-fits-header-1or2 ff "HIERARCH ESO DET CHIP NAME")))
	    (det-name  (%read-fits-header-1or2 ff "HIERARCH ESO DET NAME")) ;; SUSI seems to use DET-NAME
	    ;; WARNING - IT SEEMS THAT BINNING IS REPORTED WRONG SOMETIMES FOR RILD-FA2048
	    ;;   BEING GIVEN AS 2X2 WHEN IT IS 1X1
	    (xbinning  (or (%read-fits-header-1or2 ff "HIERARCH ESO DET WIN1 BINX")
			   ;; following is for SUSI
			   (%safe-round (%read-fits-header-1or2 ff "HIERARCH ESO DET FRAM CDELT1"))))
	    (ybinning  (or (%read-fits-header-1or2 ff "HIERARCH ESO DET WIN1 BINY")
			   ;; following is for SUSI
			   (%safe-round (%read-fits-header-1or2 ff "HIERARCH ESO DET FRAM CDELT2"))))
	    (is-reduced (cf:read-fits-header ff "IMRED.CCDPROC"))
	    ;; number of image extensions
	    (nimexts  (if (= (cf:fits-file-num-hdus ff) 1)
			  1
			  (1- (cf:fits-file-num-hdus ff)))) ;; allow for primary header
	    ;; we can recognize the chip this way
	    (pixsize/um (or (%read-fits-header-1or2 ff "HIERARCH ESO DET CHIP1 PSZX")
			    (%read-fits-header-1or2 ff "HIERARCH ESO DET CHIP2 PSZX")
			    ;; EMMI/3.17 RILD needs CHIP instead of CHIP1
			    (%read-fits-header-1or2 ff "HIERARCH ESO DET CHIP PSZX")))
	    ;; original SUSI has different header
	    (pixsize-susi/m (%read-fits-header-1or2 ff "HIERARCH ESO DET PIXSIZE"))
	    ;; naxis1 naxis2 ;; currently don't need these
	    )


	
	;(print (list instrum telescop dpr-tech ins-mode chip-name pixsize/um))
	(when (and (or (equalp telescop "ESO-NTT")
		       (equalp telescop "ESONTTA")) ;; this one is for SUSI
		   (or (eql 0 (search "EMMI" instrum))
		       (eql 0 (search "SUSI" instrum))))

	  ;; dig around and get naxis1,2 examples because we may need to fix bad
	  ;; binning
	  #+nil
	  (let ((ext-first-im (if (= (cf:fits-file-num-hdus ff) 1) ;; simple image
				  1
				  2))) ;; else first extension is primary
	    (setf naxis1 (cf:read-fits-header ff "NAXIS1" :extension ext-first-im))
	    (setf naxis2 (cf:read-fits-header ff "NAXIS2" :extension ext-first-im)))

	  
	  (let ((inst
		  (cond
		    ;; EMMI
		    ((and (or (search "EMMI" instrum)
			      (search "REMD" instrum)) ;; not sure why REMD is IMAGE here
			  (equalp dpr-tech "IMAGE"))   ;; imaging mode
		     (cond
		       ;; EMMI-RILD
		       ((or (equalp ins-mode "RILD")
			    (equalp ins-mode "REMD"))
			(cond ((and (equalp chip-name "TK2048EB4") ;; in this case, we know the chip name
				    (equalp pixsize/um 24.0))
			       (if is-reduced
				   (make-instance 'eso-ntt-emmi-rild-tek2048/reduced)
				   (make-instance 'eso-ntt-emmi-rild-tek2048/raw)))
			      ((equalp pixsize/um 19.0) ;; all we have is the pix size
			       (if is-reduced
				   (make-instance 'eso-ntt-emmi-rild-thx1024/reduced)
				   (make-instance 'eso-ntt-emmi-rild-thx1024/raw)))
			      ((and (equalp chip-name "MIT/LL mosaic") ;; this one is 2 chips
				    (equalp pixsize/um 15.0))
			       (if is-reduced
				   (cond ((= nimexts 2)
					  (make-instance 'eso-ntt-emmi-rild-fa2048/reduced))
					 ((= nimexts 1)
					  (make-instance 'eso-ntt-emmi-rild-fa2048/reduced-onechip)))
				   (cond ((= nimexts 4)
					  (make-instance 'eso-ntt-emmi-rild-fa2048/raw4ext))
					 ((= nimexts 2)
					  (make-instance 'eso-ntt-emmi-rild-fa2048/raw2ext))
					 ((= nimexts 1)
					  (let ((nexts-orig
						  (cf:read-fits-header ff "INSTRID.EMMISUSI.NEXTS")))
					  (cond ((eql nexts-orig 4)
						 (make-instance 'eso-ntt-emmi-rild-fa2048/raw4ext-onechip))
						((eql nexts-orig 2)
						 (make-instance 'eso-ntt-emmi-rild-fa2048/raw2ext-onechip))
						(t
						 (error "Found instrument eso-ntt-emmi-rild-fa2048 raw with neither 2 nor 4 extensions, nor an identifiable parent type (2 or 4 ext) from which it was extracted"))))))))))
		       ;; EMMI-BIMG
		       ((or (equalp ins-mode "BIMG")
			    (equalp ins-mode "DIMD")) ;; not sure why DIMD but chip matches tek1024
			(cond ((equalp pixsize/um 19.0)
			       (if is-reduced
				   (make-instance 'eso-ntt-emmi-bimg-thx1024/reduced)
				   (make-instance 'eso-ntt-emmi-bimg-thx1024/raw)))
			      ((equalp pixsize/um 24.0)
			       (if is-reduced
				   (make-instance 'eso-ntt-emmi-bimg-tek1024/reduced)
				   (make-instance 'eso-ntt-emmi-bimg-tek1024/raw))))))) ;; done with EMMI
		    ;; SUSI
       		    ((and (search "SUSI" instrum)
			  #+nil (equalp dpr-tech "IMAGE")) ;; imaging mode not reliably present in early versions
		     (cond
		       ;; SUSI2
		       ((and (equalp chip-name "EEV44-80")
			     (equalp pixsize/um 15.0))
			(if is-reduced
			    (cond ((= nimexts 2)
				   (make-instance 'eso-ntt-susi2/reduced))
				  ((= nimexts 1)
				   (make-instance 'eso-ntt-susi2/reduced-onechip)))
				  
			    (cond ((= nimexts 1)
				   (make-instance 'eso-ntt-susi2/raw1ext))
				  ((= nimexts 2)
				   (make-instance 'eso-ntt-susi2/raw2ext))
				  (t
				   (error "Found instance of eso-nott-susi2 raw with neither 1 or 2 extensions")))))
		       ;; SUSI
		       ((and (equalp det-name "TK1024AF") ;; susi uses 'DET NAME' not 'CHIP NAME'
			     (and pixsize-susi/m
				  (realp pixsize-susi/m)
				  (= (round (* pixsize-susi/m 1000000)) 24)))
			(if is-reduced
			    (make-instance 'eso-ntt-susi/reduced)
			    (make-instance 'eso-ntt-susi/raw)
			    )))))))

	    ;; set binning
	    (when inst
	      (setf (%xbinning inst) xbinning)
	      (setf (%ybinning inst) ybinning))
	    inst))))))
     



(%add-instrument-id-function '%eso-ntt-emmi+susi-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %eso-ntt-emmi+susi) fits-file)
  (declare (ignore inst fits-file)
	   (special *ntt-emmi+susi-hdrs*)) ;; define at end of file
  (remove-duplicates
   (append
    '("HIERARCH ESO DET CHIP1 NAME"
      "HIERARCH ESO INS FILT1 ID" ;; not sure if this ever exists
      "HIERARCH ESO INS FILT2 NAME" ;; not sure if this ever exists
      "HIERARCH ESO INS FILT1 ID"
      "HIERARCH ESO INS FILT2 NAME" 
      "HIERARCH ESO DET OUT1 CHIP"
      "HIERARCH ESO DET OUT1 CONAD"
      "HIERARCH ESO DPR CATG" ;; eg 'science'
      "HIERARCH ESO DPR TECH" ;; "IMAGE"
      "HIERARCH ESO DPR TYPE" ;; "IMAGE"
      "HIERARCH ESO INS MODE" ;; RILD or BIMG for imaging mode
      )
    *ntt-emmi+susi-hdrs* ;; just grab all the ones we know
    (call-next-method))
    :test 'equalp))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wild flippin' guesswork from
;; https://web.archive.org/web/20150906102516/http://www.ls.eso.org/sci/facilities/lasilla/instruments/emmi/emmiFilters.html

(defun %standard-emmi-susi-filter-parse (filter)
  (declare (type (or null string) filter))
  (flet ((starts-with (str parent)
	   (and parent (eql 0 (search str parent)))))
    (cond 
      ((starts-with "B" filter) :bj)
      ((starts-with "V" filter) :vj) ;; we've seen this one
      ((starts-with "R" filter) :rc) ;; we've seen this one
      ((starts-with "I" filter) :ic) 
      ((starts-with "Z" filter) :z)
      ;; 
      ((starts-with "g" filter) :gsdss)
      ((starts-with "r" filter) :rsdss) ;; we've seen this one
      ((starts-with "i" filter) :isdss) 
      ((starts-with "z" filter) :zsdss)
      ;;
      (t
       :unknown))))

(defmethod get-standard-filter-for-instrument ((inst %eso-ntt-emmi) fits-file)
  (let ((filter (cf:read-fits-header fits-file "HIERARCH ESO INS FILT2 NAME"
 				     :extension 1)))
    (%standard-emmi-susi-filter-parse filter)))

(defmethod get-standard-filter-for-instrument ((inst %eso-ntt-susi) fits-file)
  (let ((filter (cf:read-fits-header fits-file "HIERARCH ESO INS OPTI-2 NAME"
				     :extension 1)))
    (%standard-emmi-susi-filter-parse filter)))

(defmethod get-standard-filter-for-instrument ((inst %eso-ntt-susi2) fits-file)
  (let ((filter (cf:read-fits-header fits-file "HIERARCH ESO INS FILT1 NAME"
				     :extension 1)))
    (%standard-emmi-susi-filter-parse filter)))


(defmethod get-object-for-instrument ((inst %eso-ntt-emmi+susi) fits-file)
  (or (cf:read-fits-header fits-file "OBJECT" :extension 1)
      (cf:read-fits-header fits-file "HIERARCH ESO OBS TARG NAME" :extension 1)
      ))

(defmethod get-object-type-for-instrument ((inst %eso-ntt-emmi+susi) fits-file)
  (let ((dpr-type (cf:read-fits-header  fits-file  "HIERARCH ESO DPR TYPE"
					:extension 1)) ;; NIL for non-calib
	(dpr-catg (cf:read-fits-header  fits-file  "HIERARCH ESO DPR CATG"
					:extension 1)))
    (cond ((equalp dpr-type "BIAS") :BIAS)
	  ((equalp dpr-type "DARK") :DARK) ;; guess
	  ((search "FLAT" dpr-type) :FLAT) ;; eg LAMPFLAT
	  ((equalp dpr-catg "SCIENCE") :OBJECT)
	  (T :OTHER))))

;; SUSI2 fails to have ESO DPR TYPE header, so we fall back on OBJECT
(defmethod get-object-type-for-instrument ((inst %eso-ntt-susi2) fits-file)
  (if (or  (cf:read-fits-header  fits-file  "HIERARCH ESO DPR TYPE"
				 :extension 1)
	   (cf:read-fits-header  fits-file  "HIERARCH ESO DPR CATG"
				 :extension 1))
      (call-next-method) ;; it seems to have the advanced dpr headers
      (let ((object (cf:read-fits-header fits-file "OBJECT" :extension 1)))
	(cond ((equalp  object "BIAS") :BIAS)
	      ;; lampflat is the only one we know
	      ((member object '("LAMPFLAT" "DOMEFLAT" "SKYFLAT" "FLAT") :test 'equalp)
	       :FLAT)
	      ((member object '("DARK") :test 'equalp) :DARK)
	      (t :OBJECT)))))
	      
	   

(defmethod get-mjd-start-for-instrument ((inst %eso-ntt-emmi+susi) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "MJD-OBS"))

(defmethod get-mjd-mid-for-instrument ((inst %eso-ntt-emmi+susi) fits-file)
  (declare (ignorable inst))
  (+ (%gethead-or-error fits-file "MJD-OBS")
     (* 0.5 (/ (%gethead-or-error fits-file "EXPTIME") 24d0 3600d0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one big function for gain because there can be different keywords
;; depending on chip, so it's easier to try them all rather than try
;; to figure out which amp it is.
(defmethod instrument-gain-keyword ((inst %eso-ntt-emmi+susi))  NIL)
(defmethod get-gain-for-instrument ((inst %eso-ntt-emmi+susi) fits-file &key extension)
  ;; set the extension to be correct for either multipchip or onechip
  (cond ((typep inst 'multichip)
	 (when (not extension)
	   (error "ERROR in get-gain-for-instrument (emmi+susi) - must specify EXTENSION for multichip."))
	 (instrument-id::err-if-not-image-at-extension inst fits-file "get-gain (emmi+susi)" extension))
	((typep inst 'onechip)
	 (setf extension (get-image-extension-for-onechip-instrument inst fits-file))))
  ;;
  ;; default to IMRED.GAIN if reduced
  (or
   ;; if reduced try to get IMRED.GAIN
   (and (typep inst '%emmi-susi-reduced)
	(cf:read-fits-header fits-file "IMRED.GAIN" :extension extension))
   ;; and fall back on original gain 
   (let ((gain-keywords
	   (cond ((typep inst '%eso-ntt-emmi-rild-fa2048) ;; even though sometimes 4 amps, just 1,2 keywords
		  '("ESO DET OUT1 CONAD"  "ESO DET OUT2 CONAD"  "HIERARCH ESO DET OUT CONAD"))
		 ((typep inst '%eso-ntt-susi)
		  '("HIERARCH ESO DET ADU-1" "HIERARCH ESO DET ADU-2")) 
		 (t
		  '("HIERARCH ESO DET OUT1 CONAD"  "HIERARCH ESO DET OUT2 CONAD")))))
     (loop for keyword in gain-keywords
	   for value = (cf:read-fits-header fits-file keyword :extension extension)
	   when value do (return value)))))

;; the pixel scale is hardwird into object and binning was read in at ID time
(defmethod get-pixel-scale-for-instrument ((inst %eso-ntt-emmi+susi) fits-file &key extension)
  (declare (ignorable fits-file extension))
  (let ((xbinning (%xbinning inst))
	(ybinning (%ybinning inst))
	(ps (slot-value inst '%pix-scale-bin1)))
    (when (not (and xbinning ybinning (eql xbinning ybinning)))
      (error "XBINNING=~A and YBINNINING=~A are not set and/or equal for ~A" xbinning ybinning inst))
    (* ps xbinning)))
    


(defmethod get-chip-id-for-instrument ((inst %eso-ntt-emmi+susi) fits-file &key extension)
  (declare (ignorable fits-file extension))
  (slot-value inst '%chip-id))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper method to get the overcan region, for each device
;; returning #(prescan-x overscan-x prescan-y overscan-y)
;; FF is assumed to be in the correct HDU

(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-susi/raw) ff)
  (declare (ignore inst ff)) ;; the info does not seem to be in header
  (vector 49 49 0 0))

(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-susi2/raw1ext) ff)
  (declare (ignore inst))
  ;; note that this is 2 chips in a single extension, so 
  (vector (cf:read-fits-header ff "ESO DET OUT1 PRSCX")
	  (cf:read-fits-header ff "ESO DET OUT3 OVSCX")
	  0 0))

(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-susi2/raw2ext) ff)
  (declare (ignore inst))
  ;; NOTE - always uses DET1 OUT1 for both extensions; our routine in imred
  ;;        to convert raw2ext to raw2ext obeys this
  (multiple-value-bind (naxis1 naxis2)
      ;; uses current extension if not given
      (instrument-id::%get-naxis1-naxis2 ff) 
    (when (not (and (eql naxis1 1072)
		    (eql naxis2 2048)))
      (error "When trying to determine overscan, an unknown SUSI2 NAXIS1=~A,NAXIS2=~A (not 1072,2048).  The overscan headers are suspect. Fix instrument-id code to deal with this case." naxis1 naxis2)))
      
  (vector (cf:read-fits-header ff "ESO DET OUT1 PRSCX")
	  (cf:read-fits-header ff "ESO DET OUT1 OVSCX")
	  0 0
	  ;; NOTE - the headers ERRONEOUSLY say that PRSCY=5, OVSCY=25
	  ;; but in fact the whole 2048 height of image is valid data.
	  ;;(cf:read-fits-header ff "ESO DET OUT1 PRSCY")
	  ;;(cf:read-fits-header ff "ESO DET OUT1 OVSCY")
	  ))

(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-emmi-rild-thx1024/raw) ff)
  (declare (ignore inst))
  (error "Method %get-emmi-susi-overscans-in-hdu does not yet exist for inst eso-ntt-emmi-rild-thx1024/raw"))


(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-emmi-rild-tek2048/raw) ff)
  (declare (ignore inst))
  (vector (cf:read-fits-header ff "ESO DET OUT1 PRSCX")  ;; these seem to be 0
	  (cf:read-fits-header ff "ESO DET OUT1 OVSCX")
	  0 0))


(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-emmi-rild-fa2048/raw2ext) ff)
  (declare (ignore inst))
  (cond ((eql (cf:read-fits-header ff "ESO DET CHIP1 INDEX") 1)
	 (vector (cf:read-fits-header ff "ESO DET OUT1 PRSCX")
		 (cf:read-fits-header ff "ESO DET OUT1 OVSCX")
		 (cf:read-fits-header ff "ESO DET OUT1 PRSCY")
		 (cf:read-fits-header ff "ESO DET OUT1 OVSCY")))
	((eql (cf:read-fits-header ff "ESO DET CHIP2 INDEX") 2)
	 (vector (cf:read-fits-header ff "ESO DET OUT2 PRSCX")
		 (cf:read-fits-header ff "ESO DET OUT2 OVSCX")
		 (cf:read-fits-header ff "ESO DET OUT2 PRSCY")
		 (cf:read-fits-header ff "ESO DET OUT2 OVSCY")))
	(t
	 (error "Error getting header 'DET CHIPi INDEX' in  %get-emmi-susi-overscans-in-hdu for eso-ntt-emmi-rild-fa2048/raw2ext"))))


(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-emmi-rild-fa2048/raw4ext) ff)
  (declare (ignore inst))
  ;; this one initially used 'DET OUTi' (i=1,2) for chip halves, then moved to
  ;; then finally simple header 'DET OUT' in each extension.  
  (cond
    ;; final version when they came to their senses, after about 2003
    ((cf:read-fits-header ff "ESO DET OUT PRSCX")
     (vector (cf:read-fits-header ff "ESO DET OUT PRSCX")
	     (cf:read-fits-header ff "ESO DET OUT OVSCX")
	     (cf:read-fits-header ff "ESO DET OUT PRSCY")
	     (cf:read-fits-header ff "ESO DET OUT OVSCY")))
    ;; before that, this would be first half of either chip
    ((cf:read-fits-header ff "ESO DET OUT1 PRSCX")
     (vector (cf:read-fits-header ff "ESO DET OUT1 PRSCX")
	     (cf:read-fits-header ff "ESO DET OUT1 OVSCX")
	     (cf:read-fits-header ff "ESO DET OUT1 PRSCY")
	     (cf:read-fits-header ff "ESO DET OUT1 OVSCY")))
    ;; and this would be second half of either chip
    ((cf:read-fits-header ff "ESO DET OUT2 PRSCX")
     (vector (cf:read-fits-header ff "ESO DET OUT2 PRSCX")
	     (cf:read-fits-header ff "ESO DET OUT2 OVSCX")
	     (cf:read-fits-header ff "ESO DET OUT2 PRSCY")
	     (cf:read-fits-header ff "ESO DET OUT2 OVSCY")))))
	

 
(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-emmi-bimg-tek1024/raw) ff)
  (declare (ignore inst))
  (vector (cf:read-fits-header ff "ESO DET OUT1 PRSCX")
	  (cf:read-fits-header ff "ESO DET OUT1 OVSCX")
	  0 0))

(defmethod %get-emmi-susi-overscans-in-hdu ((inst eso-ntt-emmi-bimg-thx1024/raw) ff)
  (declare (ignore inst))
  (error "Method %get-emmi-susi-overscans-in-hdu does not yet exist for inst eso-ntt-emmi-bimg-thx1024/raw"))

;; end of methods  %get-emmi-susi-overscans-in-hdu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-datasec-for-instrument ((inst %eso-ntt-emmi+susi) fits-file &key extension)
  (cf:maybe-with-open-fits-file (fits-file ff)
    ;; for a one-chip, set the extension
    (when (typep inst 'onechip)
      (let ((true-ext (get-image-extension-for-onechip-instrument inst ff)))
	(when (and extension
		   (not (eql extension true-ext)))
	  (error "The provided EXTENSION=~A does not match actual image extension ~A for instrument type ~A" extension true-ext inst))
	(setf extension true-ext)))
    ;; for a multipchip, require an extension
    (when (and (typep inst 'multichip)
	       (not (integerp extension)))
      (error "An EXTENSION keyword value must be supplied for a multichip instrument."))
      
    ;; move to the extension and read the correct headers
    (cf:move-to-extension ff extension)
    (multiple-value-bind (naxis1 naxis2)
	(instrument-id::%get-naxis1-naxis2 ff :extension extension)
      (when (not (and (integerp naxis1) (integerp naxis2)
		      (plusp naxis1) (plusp naxis2)))
	(error "Failed to find positive NAXIS1=~A NAXIS2=~A in extension ~A"
	       naxis1 naxis2 extension))
      
      (cond
	;; ifNCHIPS<NDETS this is SUSI2 early version with 2 chips in
	;; an ext, so just say that the whole image is the datasec
	;; (really, the image goes
	;; PRESCAN1-IMAGE1-OVERSCAN1-PRESECAN2-IMAGE2-OVERSCAN2)
	((and (typep inst '%emmi-susi-raw)
	      (> (%nchips inst) (%nexts inst)))
	 (vector 1 naxis1 1 naxis2))
	;;
	;; if not a raw image, the overscan has been removed
	((not (typep inst '%emmi-susi-raw))
	 (vector 1 naxis1 1 naxis2))
	;;
	;; else if a raw image, compute detesec using
	;; overscan/underscan keywords assuming there is one set of
	;; OUTi headers in this extension
	((typep inst '%emmi-susi-raw)
	 (let ((overscans ;; #(prx ovx pry ovy)
		 (%get-emmi-susi-overscans-in-hdu inst ff)))
	   (when (not (every (lambda (x) (and (integerp x) (not (minusp x))))
			     overscans))
	     (error "pre- and over-scan vector ~A is not all positive integers"
		    overscans))
	   (let ((prscx  (aref overscans 0))
		 (ovscx  (aref overscans 1))
		 (prscy  (aref overscans 2))
		 (ovscy  (aref overscans 3)))
	     (vector (+ 1 prscx) (- naxis1 ovscx)
		     (+ 1 prscy) (- naxis2 ovscy)))))))))
      
	    





;; Get (values ra0 dec0 equinox pixel-scale/deg rotator-angle) for a
;; SUSI/EMMI file.  This behavior seems universal.  This function
;; ignores extensions.  This function will fail on biases and
;; domeflats.
(defmethod %emmi-susi-get-central-radec
    ((inst %eso-ntt-emmi+susi) fits-file)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (cf:with-fits-extension (ff 1)
      (let* ((ffname (if (cf:fits-file-p fits-file)
			 (cf:fits-file-filename fits-file)
			 fits-file))
	     ;; RA,DEC are usually strings but sometimes floats
	     (%ra  (cf:read-fits-header fits-file "RA"))
	     (%dec (cf:read-fits-header fits-file "DEC"))
	     ;; float versions with validation
	     (ra
	       (cond ((stringp %ra)
		      (ra-dec:hms-string->deg %ra))
		     ((realp %ra)
		      (float %ra 1d0))
		     (t
		      (error "Header RA=~S ~A for fits ~A"
			       %ra
			       (if (not %ra) "missing" "not a string or float")
			       ffname))))
	     (dec
	       (cond ((stringp %dec)
		      (ra-dec:dms-string->deg %dec))
		     ((realp %dec)
			 (float %dec 1d0))
		     (t
		      (error "Header DEC=~S ~A for fits ~A"
			     (if (not %dec) "missing" "not a string or float")
			     %dec ffname))))
	     ;; raw and validated equinox
	     (%equinox (cf:read-fits-header fits-file "EQUINOX"))
	     (equinox
	       (if (realp %equinox)
		   (float %equinox 1d0)
		   (error "Header EQUINOX=~S is ~A for fits ~A"
			  %equinox
			  (if (not %equinox) "missing" "not real number")
			  ffname)))
	     (%rotangle
	       (cf:read-fits-header ff
				    ;; according to some ESO docs, "This is the offset
				    ;; from the rotator angle that would give the
				    ;; default orientation"
				    "HIERARCH ESO ADA POSANG"))
	     (rotangle
	       (if (realp %rotangle)
		   (float %rotangle 1d0)
		   (error "Header 'HIERARCH ESO ADA ABSROT START'=~S is ~A for fits ~A"
			  %rotangle
			  (if (not %rotangle) "missing" "not real number")
			  ffname)))
	     (pix-scale (get-pixel-scale-for-instrument inst fits-file))
	     (pix-scale/deg (/ pix-scale 3600d0)))
	(values ra dec equinox pix-scale/deg rotangle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute the CRPIX1,2 for the initial WCS guess assuming that
;; RA,DEC from %emmi-susi-get-central-radec represent the center
;; of the field.
;;
;; Helper function - by default, images are always ordered 1,2,3,4.
;; This returns image indices, not HDU numbers; eg,
;; the actual image in a 2 HDU onechip is '1' not '2'
(defmethod %get-emmi-susi-image-ordering ((inst %eso-ntt-emmi+susi))
  (declare (ignore inst))
  '(1 2 3 4))

;; emmi rild has images reversed (in xy plane system)
(defmethod %get-emmi-susi-image-ordering ((inst %eso-ntt-emmi-rild-fa2048))
  (cond ((or (typep inst 'eso-ntt-emmi-rild-fa2048/raw2ext)
	     (typep inst 'eso-ntt-emmi-rild-fa2048/raw2ext-onechip)
	     (typep inst 'eso-ntt-emmi-rild-fa2048/reduced)
	     (typep inst 'eso-ntt-emmi-rild-fa2048/reduced-onechip))
	 '(2 1))
	((or (typep inst 'eso-ntt-emmi-rild-fa2048/raw4ext)
	     (typep inst 'eso-ntt-emmi-rild-fa2048/raw4ext-onechip))
	 '(4 3 2 1))
	(t
	 (error "Failed in %get-emmi-susi-image-ordering for type %eso-ntt-emmi-rild-fa2048"))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper function to compensate for non-centra pointing - the
;; pointing center is not necessarily the field center.  This computes
;; the CRPIX1 adjustment necessary for CRPIX1 to represent the true
;; chip center

;; default is centered field
(defmethod %get-emmi-susi-crpix-pointing-adj ((inst  %eso-ntt-emmi+susi))
  (declare (ignore inst))
  (values 0d0 0d0)) ;; (values crpix1 crpix2)

;; EMMI in RILD mode has 2 chips and center is offset
;; into the right hand chip by about 61 arcsec.
;; https://www.ls.eso.org/lasilla/sciops/ntt/emmi/emmiDetectors.html#gap
(defmethod %get-emmi-susi-crpix-pointing-adj ((inst  %eso-ntt-emmi-rild))
  (let* ((arcsec-offset 61.3d0) ;; to RIGHT into 2nd chip; see link
	 (pix-size (* (%pix-scale-bin1 inst) (%xbinning inst)))
	 (num-pix (/ arcsec-offset pix-size)))
    (values num-pix 0d0)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper function to return chip gap NAXIS correction in pixels.
;; this is tricky because only some gaps are known, but it's only
;; to help guesstimate the initial WCS.  IMAGE-NUMBER numbers
;; the images 1; or 1,2; or 1,2,3,4. It is not necessarily the
;; EXTENSION (HDU number)

;; by default, pretend gap is zero unless we know better
(defmethod %get-emmi-susi-crpix1-chip-gap-naxis1-adj  ((inst  %eso-ntt-emmi+susi) image-number)
  (declare (ignore inst image-number))
  0d0)


;; for SUSI2 reduced two extension, the chip gap is 96 pixels
;; https://www.ls.eso.org/lasilla/sciops/ntt/susi/docs/SUSIccds.html
(defmethod %get-emmi-susi-crpix1-chip-gap-naxis1-adj  ((inst  eso-ntt-susi2/reduced) image-number)
  ;; SUSI2 chip gap is 96 pixels at bin=1
  ;; https://www.ls.eso.org/lasilla/sciops/ntt/susi/docs/SUSIccds.html
  ;; chip 1 is left (image-number=1), chip 2 is right (image-number=2).
  ;; For image-number=1 the gap pushes CRPIX1 further right (+), for image-number=2 left (-).
  (let ((gap-pix (/ 96d0 (%xbinning inst))))
    (cond ((= image-number 1) (+  (/ gap-pix 2d0)))
          ((= image-number 2) (- (/ gap-pix 2d0)))
          (t 0d0))))

;; for emmi-fa-20438 we observe that the left chip (ie, image-number=2 or 3,4)
;; is shifted left by about 10", which is about 30 pixels
(defmethod %get-emmi-susi-crpix1-chip-gap-naxis1-adj  ((inst %eso-ntt-emmi-rild-fa2048) image-number)
  (let* ((xbin (* 1d0 (%xbinning inst)))
	 (xshift (/ -30 xbin)) ;; size of shift in pix
	 (final-xshift ;; the shift applied according to image-number
	   (cond
	     ;; two ext version, one ext per chip
	     ((or (typep inst 'eso-ntt-emmi-rild-fa2048/raw2ext)
		  (typep inst 'eso-ntt-emmi-rild-fa2048/raw2ext-onechip)
		  (typep inst 'eso-ntt-emmi-rild-fa2048/reduced)
		  (typep inst 'eso-ntt-emmi-rild-fa2048/reduced-onechip))
	      (if (= image-number 2) xshift 0d0))
	     ;; four chip
	     ((or (typep inst 'eso-ntt-emmi-rild-fa2048/raw4ext)
	   (typep inst 'eso-ntt-emmi-rild-fa2048/raw4ext-onechip))
	      (if (member image-number '(3 4)) xshift 0d0))
	     (t
	      (error "%get-emmi-susi-crpix1-chip-gap-naxis1-adj failed for rild-ra2048 subtype ~A"
		     inst)))))
    final-xshift))


;; for a EMMI or SUSI fits file, return the (VALUES CRPIX1 CRPIX2) for
;; the WCS estimate of an extension, assuming the image is centered on
;; the overall 1 or 2 chip field.  For example, for the extension at
;; the extreme left of a four extension image corresponding to two 2
;; chips, CRPIX1 (x) would be 1.5 frames (NAXIS1 units) to the right
;; of the extension's center.  CHIP-ORDERING is the actual number of
;; the chip in the left-right sequence.
(defmethod %get-emmi-susi-crpix ((inst %eso-ntt-emmi+susi) 
				 fits-file
				 extension)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (let* ((is-extracted-ext (cf:read-fits-header ff "INSTRID.EMMISUSI.EXTRACTED")) 
	   (next (%nexts inst))
	   (primary-hdu-exists (and (eq (cf:fits-file-current-hdu-type ff) :image)
				    (zerop (cf:fits-file-current-image-ndims ff))))
	   (num-hdus (cf:fits-file-num-hdus ff))
	   ;; compute the image number, which is not the extension number if
	   ;; there's a primary header
	   (image-number
	     (cond
	       ;; if it's an extracted 1chip header, look at original value in headers
	       (is-extracted-ext
		(or (cf:read-fits-header ff "INSTRID.EMMISUSI.IMGNUM")
		    (error "INSTRID.EMMISUSI.IMGNUM header not found.")))
	       ;; check for consistency - if there is a primary extension than the number of HDUs
	       ;; has to be one more than the supposed number of extensions
	       ((and primary-hdu-exists (not (= (1+ next) num-hdus)))
		(error "Something went wrong.  There is a primary header, but the supposed number of extensions NEXT does is not 1+NUM_HDUS"))
	       ;; if there's a primary header, then EXTENSION 2 has image 1, etc.
	       (primary-hdu-exists
		(1- extension))
	       ;;
	       ;; for onechip, has to be first extension
	       ((and (not extension)
		     (typep inst 'instrument-id:onechip))
		1)
	       ;; otherwise image number is extension number
	       (t
		extension)))
	   ;; the number of images in original
	   (num-orig-exts
	     (cond
	       ;; if it's an extracted 1chip header, look at original value in headers
	       (is-extracted-ext
		(or (cf:read-fits-header ff "INSTRID.EMMISUSI.NEXTS")
		    (error "INSTRID.EMMISUSI.NEXTS header not found.")))
	       ;; don't need to repeat above consistency check
	       ;; if there's a primary header, then num
	       (primary-hdu-exists
		(1- num-hdus))
	       ;; onechip has just one original extension
	       ((and (or (not extension)
			 (= extension 1))
		     (typep inst 'instrument-id:onechip))
		1)
	       (t
		(error "Somehow could not compute N-EXTS in %get-emmi-susi-crpix. INST=~A  IS-EXTRACTED-EXT=~A  NUM-HDUS=~A PRIMARY-HDU-EXISTS=~A" inst is-extracted-ext num-hdus primary-hdu-exists))))
	   ;;
	   (image-ordering (%get-emmi-susi-image-ordering inst))
	   (img-pos ;; where in left-to-right ordering this img is, starting [0]
	     (or (position image-number image-ordering)
		 (error "IMAGE-NUMBER ~A corresponding to EXTENSION ~A not found in IMAGE-ORDERING ~A"
			image-number extension image-ordering)))
	   ;; How many images (or amp-sections)  this extension is offset to
	   ;; right (positive) or left (negative) from center.
	   ;; Ignores any inter-chip gaps, which are small for SUSI,EMMI, and which
	   ;; don't really matter for initial guess
	   (x-offsets
	     ;; could use a formula but this is more self-explanatory
	     (cond ((= num-orig-exts 1)
		    0d0)
		   ((= num-orig-exts 2)
		    (aref #(-0.5d0 0.5d0) img-pos))
		   ((= num-orig-exts 4)
		    (aref #(-1.5d0 -0.5d0 0.5d0 1.5d0) img-pos))
		   (t
		    (error "There should not be NUM-ORIG-EXTS=~A not in {1,2,4}"
			   image-number)))))
      #+nil ;; diagnostics
      (progn
	(format t "is-extracted: ~A   primary-hdu-exist: ~A    num-hdus: ~A~%"
		is-extracted-ext primary-hdu-exists num-hdus)
	(format t "image-number: ~A extension: ~A num-orig-exts=~A~%" image-number extension num-orig-exts)
	(format t "x-offsets=~A~%" x-offsets))

      ;; adjustment for pointing not being centered
      (multiple-value-bind (crpix1-pointing-adj crpix2-pointing-adj)
	  (%get-emmi-susi-crpix-pointing-adj inst)
	;; adjustment for chip gap
	(let ((crpix1-chip-gap-adjust (%get-emmi-susi-crpix1-chip-gap-naxis1-adj inst image-number)))
	  ;; 
	  (multiple-value-bind (naxis1 naxis2)
	      (instrument-id::%get-naxis1-naxis2 ff :extension extension)
	    (let ((crpix1
		    ;; start at chip center, then move by naxis1 offset
		    (+ crpix1-pointing-adj
		       crpix1-chip-gap-adjust
		       (* 0.5d0 naxis1)
		       (- (* naxis1 x-offsets))))
		  (crpix2
		    (+ crpix2-pointing-adj
		       (* 0.5d0 naxis2))))
	      (values crpix1 crpix2))))))))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; return (values ROTANGLE XFLIP YFLIP)
;; xflip/yflip are the +/-1 terms to apply to xpix,ypix respectively
;; at rotator angle = 0.  ROTANGLE is then the actual rotator angle
;; read from the header "HIERARCH ESO ADA ABSROT START".
;; Base orientations at rotator=0:
;;   SUSI2:     N-up,   E-right  => xflip= 1, yflip= 1
;;   EMMI-RILD: N-down, E-right  => xflip=-1, yflip= 1
;;   EMMI-BIMG: N-left, E-bottom => xflip=-1, yflip=-1
#+nil
(defun %eso-ntt-emmi-compute-orientation (inst fits-file)
  (declare (type %eso-ntt-emmi+susi inst))
  (let ((rotangle (cf:read-fits-header
		   fits-file
		   ;; according to some ESO docs, "This is the offset
		   ;; from the rotator angle that would give the
		   ;; default orientation"
		   "HIERARCH ESO ADA POSANG"
		   :extension 1)))
    (when (not (realp rotangle))
      (error "Header 'HIERARCH ESO ADA ABSROT START'=~S is ~A for fits ~A"
	     rotangle
	     (if (not rotangle) "missing" "not a real number")
	     (if (cf:fits-file-p fits-file)
		 (cf:fits-file-filename fits-file)
		 fits-file)))
    (cond ((typep inst '%eso-ntt-susi2)
	   (values rotangle 1 1))
	  ((typep inst '%eso-ntt-emmi-rild)
	   (values rotangle -1 1))
	  ((typep inst '%eso-ntt-emmi-bimg)
	   (values rotangle -1 -1))
	  (t
	   (error "Cannot compute orientation for unknown instrument type ~A" (type-of inst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WCS SECTION


;; return (values north-angle east-flip) for an UNROTATED FIELD, where
;; north-angle is the angle of north on the chip, rotating from +Y
;; into +X.  The traditional North-up, East-left system is thus
;; NORTH-ANGLE=0, EAST-FLIP=:LEFT.
(defun %emmi-susi-chip-orientation (inst)
    (cond ((typep inst '%eso-ntt-emmi-rild-tek2048)
	   (values -90 :left))
	  ((typep inst '%eso-ntt-emmi-bimg-tek1024)
	   (values 0 :left))
	  ;; validated for 2ext and 4ext
	  ((typep inst '%eso-ntt-emmi-rild-fa2048)
	   (values 180 :left))
	  ((typep inst '%eso-ntt-susi2)
	   (values 0 :right))
      
	  (t
	   (error "Cannot perform  %emmi-susi-chip-orientation for instrument type ~A" inst))))
    
;; returns the field PA relative to default configuration in %emmi-susi-chip-orientation
(defun %emmi-susi-field-pa (inst fits-file)
  (declare (ignorable inst))
  (let ((posangle (cf:read-fits-header
		   fits-file
		   ;; according to some ESO docs, "This is the offset
		   ;; from the rotator angle that would give the
		   ;; default orientation"
		   "HIERARCH ESO ADA POSANG"
		   :extension 1)))
     ;;
     (when (not (realp posangle))
       (error "Header 'HIERARCH ESO ADA POSANG'=~S is ~A for fits ~A"
	      posangle
	      (if (not posangle) "missing" "not a real number")
	      (if (cf:fits-file-p fits-file)
		  (cf:fits-file-filename fits-file)
		  fits-file)))
     ;;
     posangle))
     
  

  
(defmethod get-initial-wcs-for-instrument ((inst %eso-ntt-emmi+susi) fits-file &key extension)
  (let ((pixel-scale/arcsec (get-pixel-scale-for-instrument inst fits-file))
	(field-pa (%emmi-susi-field-pa inst fits-file))
	(%extension
	  (if (typep inst 'onechip)
	      (instrument-id:get-image-extension-for-onechip-instrument
	       inst fits-file)
	      (or extension
		  (error "ERROR in GET-INITIAL-WCS-FOR-INSTRUMENT: EXTENSION not provided for multichip of type ~A"
			 inst)))))
    (multiple-value-bind (north-angle east-flip )
	(%emmi-susi-chip-orientation inst)
      (multiple-value-bind (ra dec equinox)
	  (%emmi-susi-get-central-radec inst fits-file)
	(multiple-value-bind (crpix1 crpix2)
	    (%get-emmi-susi-crpix inst fits-file %extension)
	  (wcs:build-wcs-radec-tan 
	   pixel-scale/arcsec
	   north-angle
	   east-flip
	   :equinox equinox
	   :field-pa field-pa
	   :crval1 ra :crval2 dec
	   :crpix1 crpix1 :crpix2 crpix2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; when splitting extensions (which may be chips, or amps) from a fits
;; file using extract-one-image-from-mosaic-fits, add some notes about
;; which extension it was
(defmethod instrument-id::patch-extracted-extension-headers
    ((inst %eso-ntt-emmi+susi) fits-file extname/extnum output-fits-file)
  (if (not (integerp extname/extnum))
      (error "For EMMI/SUSI extraction must be by extension NUMBER but EXTNAME/EXTNUM is ~S"
	     extname/extnum))
  (cf:maybe-with-open-fits-file (fits-file ff)
    (let* ((extension extname/extnum)
	   (nexts (%nexts inst)) ;; number of IMAGE exts
	   (nchips (%nchips inst))
	   (primary-hdu-exists
	     (and (eq (cf:fits-file-current-hdu-type ff) :image)
		  (zerop (cf:fits-file-current-image-ndims ff))))
	   ;;(num-hdus (cf:fits-file-num-hdus ff))
	   ;; image number starting with 1, maybe an amp or a chip
	   (image-number (if primary-hdu-exists (1- extension) extension))
	   ;; is this chip 1 or 2?
	   (chip-num (cond
		       ;; trivial cases - always chip 1
		       ((or (= nexts 1) (= nchips 1))
			    1)
		       ;; 2 total extensions and 2 total chips, so image-number is chip
		       ((and (= nexts 2) (= nchips 2))
			image-number)
		       ;; four extensions
		       ((and (= nexts 4) (= nchips 2))
			(if (<= image-number 2) 1 2))
		       ;;
		       (t
			(error
			 "patch-extracted-extension-headers: NEXTS=~A NCHIPS=~A for ~A should not happen?!"
			  nexts nchips inst)))))
      (cf:maybe-with-open-fits-file (output-fits-file ffout :mode :io)
	(cf:write-fits-comment ffout "INSTRID.xx hdrs are from Lisp instrument-id pkg")
	(cf:write-fits-header ffout "INSTRID.EMMISUSI.EXTRACTED" t
			      :comment "This is 1 img/ext from multi-ext fits")
	(cf:write-fits-header ffout "INSTRID.EMMISUSI.CHIPNUM" chip-num
			      :comment "Chip number 1-based")
	(cf:write-fits-header ffout "INSTRID.EMMISUSI.NCHIPS" nchips
			      :comment "Total num chips in orig")
	(cf:write-fits-header ffout "INSTRID.EMMISUSI.IMGNUM" image-number
			      :comment "Image number (chip or amp), 1-based")
	(cf:write-fits-header ffout "INSTRID.EMMISUSI.HDUNUM" extension
			      :comment "HDU number, 1-based")
	(cf:write-fits-header ffout "INSTRID.EMMISUSI.NEXTS" nexts
			      :comment "Total image exts in orig (not incl primary)")
	(cf:write-fits-header ffout "INSTRID.EMMISUSI.WHOLECHIP"
			      (= nexts nchips)
			      :comment "Is this a whole chip?")))))
	
	
	
    

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; critical headers

(defparameter *ntt-emmi+susi-hdrs*
  '("HIERARCH ESO DET CHIP1 ID" "HIERARCH ESO DET CHIP1 NAME"
    "HIERARCH ESO DET CHIP1 DATE" "HIERARCH ESO DET CHIP1 X"
    "HIERARCH ESO DET CHIP1 Y" "HIERARCH ESO DET CHIP1 NX"
    "HIERARCH ESO DET CHIP1 NY" "HIERARCH ESO DET CHIP1 PSZX"
    "HIERARCH ESO DET CHIP1 PSZY" "HIERARCH ESO DET CHIP1 XGAP"
    "HIERARCH ESO DET CHIP1 YGAP" "HIERARCH ESO DET OUT1 INDEX"
    "HIERARCH ESO DET OUT1 ID" "HIERARCH ESO DET OUT1 NAME"
    "HIERARCH ESO DET OUT1 CHIP" "HIERARCH ESO DET OUT1 X"
    "HIERARCH ESO DET OUT1 Y" "HIERARCH ESO DET OUT1 NX"
    "HIERARCH ESO DET OUT1 NY" "HIERARCH ESO DET OUT1 PRSCX"
    "HIERARCH ESO DET OUT1 OVSCX" "HIERARCH ESO DET OUT1 PRSCY"
    "HIERARCH ESO DET OUT1 OVSCY" "HIERARCH ESO DET OUT1 CONAD"
    "HIERARCH ESO DET OUT1 RON" "HIERARCH ESO DET OUT1 GAIN"
    "HIERARCH ESO OBS DID" "HIERARCH ESO OBS PI-COI ID" "HIERARCH ESO OBS GRP"
    "HIERARCH ESO OBS NAME" "HIERARCH ESO OBS ID" "HIERARCH ESO OBS PROG ID"
    "HIERARCH ESO OBS START" "HIERARCH ESO OBS TPLNO" "HIERARCH ESO TPL DID"
    "HIERARCH ESO TPL ID" "HIERARCH ESO TPL NAME" "HIERARCH ESO TPL PRESEQ"
    "HIERARCH ESO TPL START" "HIERARCH ESO TPL VERSION" "HIERARCH ESO TPL NEXP"
    "HIERARCH ESO TPL EXPNO" "HIERARCH ESO DPR CATG" "HIERARCH ESO DPR TYPE"
    "HIERARCH ESO DPR TECH" "HIERARCH ESO TEL DID" "HIERARCH ESO TEL ID"
    "HIERARCH ESO TEL DATE" "HIERARCH ESO TEL ALT" "HIERARCH ESO TEL AZ"
    "HIERARCH ESO TEL GEOELEV" "HIERARCH ESO TEL GEOLAT" "HIERARCH ESO TEL GEOLON"
    "HIERARCH ESO TEL OPER" "HIERARCH ESO TEL FOCU ID" "HIERARCH ESO TEL FOCU LEN"
    "HIERARCH ESO TEL FOCU SCALE" "HIERARCH ESO TEL FOCU VALUE"
    "HIERARCH ESO TEL PARANG STAR" "HIERARCH ESO TEL AIRM START"
    "HIERARCH ESO TEL AMBI WINDDI" "HIERARCH ESO TEL AMBI RHUM"
    "HIERARCH ESO TEL AMBI TEMP" "HIERARCH ESO TEL MOON RA"
    "HIERARCH ESO TEL MOON DEC" "HIERARCH ESO TEL CHOP ST"
    "HIERARCH ESO TEL PARANG END" "HIERARCH ESO TEL AIRM END"
    "HIERARCH ESO ADA ABSROT STAR" "HIERARCH ESO ADA POSANG"
    "HIERARCH ESO ADA ABSROT END" "HIERARCH ESO INS MODE" "HIERARCH ESO INS PATH"
    "HIERARCH ESO INS DATE" "HIERARCH ESO INS SWSIM" "HIERARCH ESO INS ID"
    "HIERARCH ESO INS MIRR4 ID" "HIERARCH ESO INS MIRR4 NAME"
    "HIERARCH ESO INS MIRR4 NO" "HIERARCH ESO INS MIRR4 TYPE"
    "HIERARCH ESO INS MIRR6 ST" "HIERARCH ESO INS FILT2 ID"
    "HIERARCH ESO INS FILT2 NAME" "HIERARCH ESO INS FILT2 NO"
    "HIERARCH ESO INS FILT2 TYPE" "HIERARCH ESO INS FOCU1 TEMP"
    "HIERARCH ESO INS FOCU1 ROT" "HIERARCH ESO INS GRIS1 ID"
    "HIERARCH ESO INS GRIS1 NAME" "HIERARCH ESO INS GRIS1 NO"
    "HIERARCH ESO INS GRIS1 TYPE" "HIERARCH ESO INS SLIT2 ID"
    "HIERARCH ESO INS SLIT2 NAME" "HIERARCH ESO INS SLIT2 NO"
    "HIERARCH ESO INS SLIT2 TYPE" "HIERARCH ESO INS TEMP1"
    "HIERARCH ESO INS TEMP2" "HIERARCH ESO INS TEMP3" "HIERARCH ESO INS TEMP4"
    "HIERARCH ESO INS TEMP5" "HIERARCH ESO INS TEMP6" "HIERARCH ESO DET ID"
    "HIERARCH ESO DET NAME" "HIERARCH ESO DET DATE" "HIERARCH ESO DET DID"
    "HIERARCH ESO DET BITS" "HIERARCH ESO DET RA" "HIERARCH ESO DET DEC"
    "HIERARCH ESO DET CHIPS" "HIERARCH ESO DET OUTPUTS" "HIERARCH ESO DET OUTREF"
    "HIERARCH ESO DET WINDOWS" "HIERARCH ESO DET SOFW MODE"
    "HIERARCH ESO DET EXP NO" "HIERARCH ESO DET EXP TYPE"
    "HIERARCH ESO DET WIN1 ST" "HIERARCH ESO DET WIN1 STRX"
    "HIERARCH ESO DET WIN1 STRY" "HIERARCH ESO DET WIN1 NX"
    "HIERARCH ESO DET WIN1 NY" "HIERARCH ESO DET WIN1 BINX"
    "HIERARCH ESO DET WIN1 BINY" "HIERARCH ESO DET WIN1 NDIT"
    "HIERARCH ESO DET WIN1 UIT1" "HIERARCH ESO DET WIN1 DIT1"
    "HIERARCH ESO DET WIN1 DKTM" "HIERARCH ESO DET READ MODE"
    "HIERARCH ESO DET READ SPEED" "HIERARCH ESO DET READ CLOCK"
    "HIERARCH ESO DET READ NFRAM" "HIERARCH ESO DET FRAM ID"
    "HIERARCH ESO DET FRAM TYPE" "HIERARCH ESO DET SHUT TYPE"
    "HIERARCH ESO DET SHUT ID" "HIERARCH ESO DET CHIP1 DATE"
    "HIERARCH ESO DET CHIP1 ID" "HIERARCH ESO DET CHIP1 INDEX"
    "HIERARCH ESO DET CHIP1 NAME" "HIERARCH ESO DET CHIP1 NX"
    "HIERARCH ESO DET CHIP1 NY" "HIERARCH ESO DET CHIP1 PSZX"
    "HIERARCH ESO DET CHIP1 PSZY" "HIERARCH ESO DET CHIP1 X"
    "HIERARCH ESO DET CHIP1 XGAP" "HIERARCH ESO DET CHIP1 Y"
    "HIERARCH ESO DET CHIP1 YGAP" "HIERARCH ESO DET OUT1 CHIP"
    "HIERARCH ESO DET OUT1 CONAD" "HIERARCH ESO DET OUT1 GAIN"
    "HIERARCH ESO DET OUT1 ID" "HIERARCH ESO DET OUT1 INDEX"
    "HIERARCH ESO DET OUT1 NAME" "HIERARCH ESO DET OUT1 NX"
    "HIERARCH ESO DET OUT1 NY" "HIERARCH ESO DET OUT1 OVSCX"
    "HIERARCH ESO DET OUT1 OVSCY" "HIERARCH ESO DET OUT1 PRSCX"
    "HIERARCH ESO DET OUT1 PRSCY" "HIERARCH ESO DET OUT1 RON"
    "HIERARCH ESO DET OUT1 X" "HIERARCH ESO DET OUT1 Y"
    "HIERARCH ESO ADA ABSROT END" "HIERARCH ESO ADA ABSROT PPOS"
    "HIERARCH ESO ADA ABSROT START" "HIERARCH ESO ADA GUID STATUS"
    "HIERARCH ESO ADA POSANG" "HIERARCH ESO DET BITS" "HIERARCH ESO DET CHIPS"
    "HIERARCH ESO DET DATE" "HIERARCH ESO DET DEC" "HIERARCH ESO DET DID"
    "HIERARCH ESO DET EXP NO" "HIERARCH ESO DET EXP RDTTIME"
    "HIERARCH ESO DET EXP TYPE" "HIERARCH ESO DET EXP XFERTIM"
    "HIERARCH ESO DET FRAM ID" "HIERARCH ESO DET FRAM TYPE" "HIERARCH ESO DET ID"
    "HIERARCH ESO DET NAME" "HIERARCH ESO DET OUTPUTS" "HIERARCH ESO DET OUTREF"
    "HIERARCH ESO DET RA" "HIERARCH ESO DET READ CLOCK"
    "HIERARCH ESO DET READ MODE" "HIERARCH ESO DET READ NFRAM"
    "HIERARCH ESO DET READ SPEED" "HIERARCH ESO DET SHUT ID"
    "HIERARCH ESO DET SHUT TMCLOS" "HIERARCH ESO DET SHUT TMOPEN"
    "HIERARCH ESO DET SHUT TYPE" "HIERARCH ESO DET SOFW MODE"
    "HIERARCH ESO DET WIN1 BINX" "HIERARCH ESO DET WIN1 BINY"
    "HIERARCH ESO DET WIN1 DIT1" "HIERARCH ESO DET WIN1 DKTM"
    "HIERARCH ESO DET WIN1 NDIT" "HIERARCH ESO DET WIN1 NX"
    "HIERARCH ESO DET WIN1 NY" "HIERARCH ESO DET WIN1 ST"
    "HIERARCH ESO DET WIN1 STRX" "HIERARCH ESO DET WIN1 STRY"
    "HIERARCH ESO DET WIN1 UIT1" "HIERARCH ESO DET WINDOWS"
    "HIERARCH ESO DPR CATG" "HIERARCH ESO DPR TECH" "HIERARCH ESO INS DATE"
    "HIERARCH ESO INS DID" "HIERARCH ESO INS FILT1 ID"
    "HIERARCH ESO INS FILT1 NAME" "HIERARCH ESO INS FILT1 NO"
    "HIERARCH ESO INS ID" "HIERARCH ESO INS MIRR4 ID"
    "HIERARCH ESO INS MIRR4 NAME" "HIERARCH ESO INS MIRR4 NO"
    "HIERARCH ESO INS MODE" "HIERARCH ESO INS SWSIM" "HIERARCH ESO OBS DID"
    "HIERARCH ESO OBS EXECTIME" "HIERARCH ESO OBS GRP" "HIERARCH ESO OBS ID"
    "HIERARCH ESO OBS NAME" "HIERARCH ESO OBS OBSERVER"
    "HIERARCH ESO OBS PI-COI ID" "HIERARCH ESO OBS PI-COI NAME"
    "HIERARCH ESO OBS PROG ID" "HIERARCH ESO OBS START"
    "HIERARCH ESO OBS TARG NAME" "HIERARCH ESO OBS TPLNO"
    "HIERARCH ESO OCS DET IMGNAME" "HIERARCH ESO TEL AIRM END"
    "HIERARCH ESO TEL AIRM START" "HIERARCH ESO TEL ALT"
    "HIERARCH ESO TEL AMBI FWHM END" "HIERARCH ESO TEL AMBI FWHM START"
    "HIERARCH ESO TEL AMBI PRES END" "HIERARCH ESO TEL AMBI PRES START"
    "HIERARCH ESO TEL AMBI RHUM" "HIERARCH ESO TEL AMBI TEMP"
    "HIERARCH ESO TEL AMBI WINDDIR" "HIERARCH ESO TEL AMBI WINDSP"
    "HIERARCH ESO TEL AZ" "HIERARCH ESO TEL CHOP ST" "HIERARCH ESO TEL DATE"
    "HIERARCH ESO TEL DID" "HIERARCH ESO TEL DOME STATUS"
    "HIERARCH ESO TEL FOCU ID" "HIERARCH ESO TEL FOCU LEN"
    "HIERARCH ESO TEL FOCU SCALE" "HIERARCH ESO TEL FOCU VALUE"
    "HIERARCH ESO TEL GEOELEV" "HIERARCH ESO TEL GEOLAT" "HIERARCH ESO TEL GEOLON"
    "HIERARCH ESO TEL ID" "HIERARCH ESO TEL MOON DEC" "HIERARCH ESO TEL MOON RA"
    "HIERARCH ESO TEL OPER" "HIERARCH ESO TEL PARANG END"
    "HIERARCH ESO TEL PARANG START" "HIERARCH ESO TEL TH M1 TEMP"
    "HIERARCH ESO TEL TRAK RATEA" "HIERARCH ESO TEL TRAK RATED"
    "HIERARCH ESO TEL TRAK STATUS" "HIERARCH ESO TPL DID" "HIERARCH ESO TPL EXPNO"
    "HIERARCH ESO TPL ID" "HIERARCH ESO TPL NAME" "HIERARCH ESO TPL NEXP"
    "HIERARCH ESO TPL PRESEQ" "HIERARCH ESO TPL START" "HIERARCH ESO TPL VERSION"))




