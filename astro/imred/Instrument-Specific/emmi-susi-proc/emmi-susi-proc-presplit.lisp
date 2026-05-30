#|

For some (one?) generations EMMI/SUSI instrument, 2 chips were put into 1 image extension.

This seems to be only the early SUSI2 generation INSTRUMENT-ID:ESO-NTT-SUSI2/RAW1EXT

To make reduction uniform, such images should be spit into the more modern format
of one chip per extension.

Provided methods:

 (INST-NEEDS-CHIP-PRESPLIT instrument-id:instrument)  ==> T/NIL

 (CHIP-PRESPLIT instrument-id:instrument fits-file-in fits-file-out :overwrite t)



|#

(in-package emmi-susi-proc)

(defmethod inst-needs-chip-presplit ((inst instrument-id/eso-ntt-emmi+susi:%eso-ntt-emmi+susi))
  (declare (ignore inst))
  nil)

;; the only instrument to put 2 chips in one ext
(defmethod inst-needs-chip-presplit ((inst instrument-id:eso-ntt-susi2/raw1ext))
  (declare (ignore inst))
  t)

(defmethod chip-presplit ((inst instrument-id/eso-ntt-emmi+susi:%eso-ntt-emmi+susi) fits-file-in fits-file-out
		     &key (overwrite t))
  (declare (ignore fits-file-in fits-file-out overwrite))
  (error "Instrument ~A does not need presplit" inst))

(defmethod chip-presplit ((inst instrument-id:eso-ntt-susi2/raw1ext) fits-file-in fits-file-out
		     &key (overwrite t))
  (declare (ignore inst))
  (cf:maybe-with-open-fits-file (fits-file-in ffin)
    ;; if it's not a 1-HDU file, something went wrong
    (when (not (= (cf:fits-file-num-hdus ffin) 1))
      (error "Expect 1 HDU in presplit for instrument-id:eso-ntt-susi2/raw1ext"))
    ;;
    (when (not (and (eql 16 (cf:read-fits-header ffin "BITPIX"))
		    (ignore-errors (= 32768 (cf:read-fits-header ffin "BZERO")))
		    (ignore-errors (= 1 (cf:read-fits-header ffin "BSCALE")))))
      (error "Expect BITPIX=16, BZERO=32768, BSCALE=1 - ie, 16 bit unsigned data."))
    ;;
    ;; divide up the headers and read the image section
    (let* ((headers (cf:read-fits-header-list ffin))
	   (imsec (cf:read-image-section ffin :type :unsigned-byte-16))
	   (im   (cf:image-section-data imsec))
	   (nx    (array-dimension im 1))
	   (ny    (array-dimension im 0)))
      (if (oddp nx)
	  (error "Expect image dimension NX=~A to be even for division into two chips." nx))
      (let* ((nx/2 (ash nx -1))
	     ;; chip1,2 arrays
	     (imc1  (make-array (list ny nx/2) :element-type '(unsigned-byte 16)))
	     (imc2  (make-array (list ny nx/2) :element-type '(unsigned-byte 16))))
	;; fill the two chip arrays
	(loop for iy below ny
	      do (loop for ix from 0 below nx/2
		       do (setf (aref imc1 iy ix) (aref im iy ix))
			  (setf (aref imc2 iy ix) (aref im iy (+ ix nx/2)))))

 
	
	;; divide headers into those that belong in each extension
	(multiple-value-bind (hprim hchip1 hchip2)
	    (%presplit-divide-headers-susi2/raw1ext headers)
	  (cf:with-new-fits-file (fits-file-out ffout :overwrite overwrite
						      :make-primary-headers t)
	    (flet ((write-header-list (hdr-list)
		     (loop for (key value comment) in hdr-list
			   do (cond ((equalp key "COMMENT")
				     (cf:write-fits-comment ffout value))
				    (t
				     (cf:write-fits-header ffout key value :comment comment)))))
		   (find-header-value (key hdr-list) 
		     (second (find key hdr-list :test 'equalp :key 'first))))
	      ;;
	      (cf:write-fits-comment ffout "Fits file created from old form SUSI2 data with 2 chips in 1 ext")
	      (cf:write-fits-comment ffout "This file contains 1 chip per ext")
	      (write-header-list hprim) ;; into primary header
	      ;; do chip1
	      (cf:add-image-to-fits-file ffout
					 :ushort
					 (vector nx/2 ny) ;; fortran style #(NX NY)
					 :create-data imc1)
	      (write-header-list hchip1)
	      ;; name the extension after the chip
	      (cf:write-fits-header "EXTNAME"  (find-header-value  "HIERARCH ESO DET CHIP1 ID" hchip1))
	      ;; do chip2
	      (cf:add-image-to-fits-file ffout
					 :ushort
					 (vector nx/2 ny) ;; fortran style #(NX NY)
					 :create-data imc2)
	      (write-header-list hchip2)
	      ;; name the extension after the chip
	      (cf:write-fits-header "EXTNAME"  (find-header-value  "HIERARCH ESO DET CHIP2 ID" hchip2)))))))))



;; the headers that should go into primary and chip1,chip2 extensions based
;; on a later 2 extension ESO file, so we turn the old files into a mimic of the new ones
(defparameter *presplit-primary-headers-susi2/raw1ext*
  '("SIMPLE" "BITPIX" "NAXIS" "EXTEND"  
    "ORIGIN" "DATE" "TELESCOP" "INSTRUME"
    "OBJECT" "RA" "DEC" "EQUINOX" "RADECSYS" "EXPTIME" "MJD-OBS" "DATE-OBS" "UTC"
    "LST" "PI-COI" "OBSERVER" "ESO ADA ABSROT END" "ESO ADA ABSROT PPOS"
    "ESO ADA ABSROT START" "ESO ADA GUID STATUS" "ESO ADA POSANG" "ESO DET BITS"
    "ESO DET CHIPS" "ESO DET DATE" "ESO DET DEC" "ESO DET DID" "ESO DET EXP NO"
    "ESO DET EXP RDTTIME" "ESO DET EXP TYPE" "ESO DET EXP XFERTIM"
    "ESO DET FRAM ID" "ESO DET FRAM TYPE" "ESO DET ID" "ESO DET NAME"
    "ESO DET OUTPUTS" "ESO DET OUTREF" "ESO DET RA" "ESO DET READ CLOCK"
    "ESO DET READ MODE" "ESO DET READ NFRAM" "ESO DET READ SPEED"
    "ESO DET SHUT ID" "ESO DET SHUT TMCLOS" "ESO DET SHUT TMOPEN"
    "ESO DET SHUT TYPE" "ESO DET SOFW MODE" "ESO DET WIN1 BINX"
    "ESO DET WIN1 BINY" "ESO DET WIN1 DIT1" "ESO DET WIN1 DKTM"
    "ESO DET WIN1 NDIT" "ESO DET WIN1 NX" "ESO DET WIN1 NY" "ESO DET WIN1 ST"
    "ESO DET WIN1 STRX" "ESO DET WIN1 STRY" "ESO DET WIN1 UIT1" "ESO DET WINDOWS"
    "ESO DPR CATG" "ESO DPR TECH" "ESO INS DATE" "ESO INS DID" "ESO INS FILT1 ID"
    "ESO INS FILT1 NAME" "ESO INS FILT1 NO" "ESO INS ID" "ESO INS MIRR4 ID"
    "ESO INS MIRR4 NAME" "ESO INS MIRR4 NO" "ESO INS MODE" "ESO INS SWSIM"
    "ESO OBS DID" "ESO OBS EXECTIME" "ESO OBS GRP" "ESO OBS ID" "ESO OBS NAME"
    "ESO OBS OBSERVER" "ESO OBS PI-COI ID" "ESO OBS PI-COI NAME" "ESO OBS PROG ID"
    "ESO OBS START" "ESO OBS TARG NAME" "ESO OBS TPLNO" "ESO OCS DET IMGNAME"
    "ESO TEL AIRM END" "ESO TEL AIRM START" "ESO TEL ALT" "ESO TEL AMBI FWHM END"
    "ESO TEL AMBI FWHM START" "ESO TEL AMBI PRES END" "ESO TEL AMBI PRES START"
    "ESO TEL AMBI RHUM" "ESO TEL AMBI TEMP" "ESO TEL AMBI WINDDIR"
    "ESO TEL AMBI WINDSP" "ESO TEL AZ" "ESO TEL CHOP ST" "ESO TEL DATE"
    "ESO TEL DID" "ESO TEL DOME STATUS" "ESO TEL FOCU ID" "ESO TEL FOCU LEN"
    "ESO TEL FOCU SCALE" "ESO TEL FOCU VALUE" "ESO TEL GEOELEV" "ESO TEL GEOLAT"
    "ESO TEL GEOLON" "ESO TEL ID" "ESO TEL MOON DEC" "ESO TEL MOON RA"
    "ESO TEL OPER" "ESO TEL PARANG END" "ESO TEL PARANG START"
    "ESO TEL TH M1 TEMP" "ESO TEL TRAK RATEA" "ESO TEL TRAK RATED"
    "ESO TEL TRAK STATUS" "ESO TPL DID" "ESO TPL EXPNO" "ESO TPL ID"
    "ESO TPL NAME" "ESO TPL NEXP" "ESO TPL PRESEQ" "ESO TPL START"
    "ESO TPL VERSION" "ORIGFILE" "ARCFILE" "HDRVER" "COMMENT"))

(defparameter *presplit-chip1-headers-susi2/raw1ext*
  '("XTENSION" "BITPIX" "NAXIS" "NAXIS1" "NAXIS2"
    "PCOUNT" "GCOUNT" "EXTNAME"
    "BZERO" "BSCALE"
    "ORIGIN" "DATE" "EXPTIME" "MJD-OBS" "DATE-OBS" "CTYPE1"
    "CTYPE2" "CRVAL1" "CRVAL2" "CRPIX1" "CRPIX2" "CDELT1" "CDELT2"
    "ESO DET CHIP1 DATE" "ESO DET CHIP1 ID" "ESO DET CHIP1 INDEX"
    "ESO DET CHIP1 NAME" "ESO DET CHIP1 NX" "ESO DET CHIP1 NY"
    "ESO DET CHIP1 PSZX" "ESO DET CHIP1 PSZY" "ESO DET CHIP1 X"
    "ESO DET CHIP1 XGAP" "ESO DET CHIP1 Y" "ESO DET CHIP1 YGAP"
    "ESO DET OUT1 CHIP" "ESO DET OUT1 CONAD" "ESO DET OUT1 GAIN" "ESO DET OUT1 ID"
    "ESO DET OUT1 INDEX" "ESO DET OUT1 NAME" "ESO DET OUT1 NX" "ESO DET OUT1 NY"
    "ESO DET OUT1 OVSCX" "ESO DET OUT1 OVSCY" "ESO DET OUT1 PRSCX"
    "ESO DET OUT1 PRSCY" "ESO DET OUT1 RON" "ESO DET OUT1 X" "ESO DET OUT1 Y"))

(defparameter *presplit-chip2-headers-susi2/raw1ext*
  '("XTENSION" "BITPIX" "NAXIS" "NAXIS1" "NAXIS2"
    "PCOUNT" "GCOUNT" "EXTNAME"
    "BZERO" "BSCALE"
    "ORIGIN" "DATE" "EXPTIME" "MJD-OBS" "DATE-OBS" "CTYPE1"
    "CTYPE2" "CRVAL1" "CRVAL2" "CRPIX1" "CRPIX2" "CDELT1" "CDELT2"
    "ESO DET CHIP2 DATE" "ESO DET CHIP2 ID" "ESO DET CHIP2 INDEX"
    "ESO DET CHIP2 NAME" "ESO DET CHIP2 NX" "ESO DET CHIP2 NY"
    "ESO DET CHIP2 PSZX" "ESO DET CHIP2 PSZY" "ESO DET CHIP2 X"
    "ESO DET CHIP2 XGAP" "ESO DET CHIP2 Y" "ESO DET CHIP2 YGAP"
    ;; OUT2 and OUT3 will be converted to OUT1 for 2nd chip, so we ignore them
    ;; "ESO DET OUT1 CHIP" "ESO DET OUT1 CONAD" "ESO DET OUT1 GAIN" "ESO DET OUT1 ID"
    ;; "ESO DET OUT1 INDEX" "ESO DET OUT1 NAME" "ESO DET OUT1 NX" "ESO DET OUT1 NY"
    ;; "ESO DET OUT1 OVSCX" "ESO DET OUT1 OVSCY" "ESO DET OUT1 PRSCX"
    ;; "ESO DET OUT1 PRSCY" "ESO DET OUT1 RON" "ESO DET OUT1 X" "ESO DET OUT1 Y"
    ))

;; headers we don't want to copy
(defparameter *presplit-avoid-headers-susi2/raw1ext*
  '("XTENSION" "BITPIX" "NAXIS" "NAXIS1" "NAXIS2"
    "BZERO" "BSCALE"
    "SIMPLE" "BITPIX" "EXTEND"  ))


;; fix any header of the form OUT2 or OUT3 to OUT1
;; to match convention of latter SUSI2, where both chips
;; had OUT1 headers
(defun %fix-out2or3-to-out1 (header)
  (let ((key (copy-seq (first header))))
    (let ((nout (or (search "OUT2" key) (search "OUT3" key)
		    (error "No OUT2,3 found - should not happen"))))
      (setf (aref key (+ nout 3)) #\1)
      (setf (first header) key)
      header)))
     



;; divide a one-ext file's headers into (values primary chip1-hdu chip2-hdu)
(defun %presplit-divide-headers-susi2/raw1ext (header-list)
  (let ((hprim nil)
	(hchip1 nil)
	(hchip2 nil)
	(hunused nil))
    (loop for header in header-list
	  for key = (first header)
	  for used = nil ;; was this header used by a rule?
	  do
	     (cond ;; don't copy these at all
	       ((find key *presplit-avoid-headers-susi2/raw1ext* :test 'equalp)
		(setf used t)
		nil)
	       ;; headers with chip name
	       ((search "CHIP1" key)
		(setf used t)
		(push header hchip1))
	       ((search "CHIP2" key)
		(setf used t)
		(push header hchip2))
	       ;; it seems that old single-hdu used OUT1,OUT3 while
	       ;; newer double-HDU put OUT1 into correct header.  We will
	       ;; put OUT1,OUT3 into correct header and hope it's right
	       ((search "OUT1" key)
		(setf used t)
		(push header hchip1))
	       ((or (search "OUT3" key)
		    (search "OUT2" key)) ;; not sure OUT2 exists
		(setf used t)
		;; to turn it into a bona-fide 2-ext, adopt convention that
		;; 2nd extension uses OUT1 instead of OUT3 (and possibly OUT2
		;; though we've never seen it)
		(setf header (%fix-out2or3-to-out1 header))
		(push header hchip2))
	       ;; headers that could go into multiple extensions follow
	       (t
		(when (find key *presplit-primary-headers-susi2/raw1ext* :test 'equalp)
		  (setf used t)
		  (push header hprim))
		(when (find key *presplit-chip1-headers-susi2/raw1ext*  :test 'equalp)
		  (setf used t)
		  (push header hchip1))
		(when (find key *presplit-chip2-headers-susi2/raw1ext*  :test 'equalp)
		  (setf used t)
		  (push header hchip2))))
	     ;; and headers that were not used go into unused (for diagnosis)
	     (when (not used)
	       (push header hprim)
	       (push header hunused)))
    ;;
    (values (nreverse hprim)
	    (nreverse hchip1)
	    (nreverse hchip2)
	    (nreverse hunused))))
			     
			     
		  
			    
		   
		   

      
  
