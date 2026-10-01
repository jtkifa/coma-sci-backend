#|

Code to merge extensions for EMMI-RILD-FA2048/raw4ext.

The FA2048 detector has 2 physical chips, each read out through 2 amplifiers.
Early data formats put each amp readout in a separate extension (4 total).
Later formats merged the amps into 1 extension per chip (2 total).

Raw 4-extension structure (HDU numbers 2-5):
  Extension 2: Right half of Chip 1 (has vignetting)
  Extension 3: Left half of Chip 1
  Extension 4: Right half of Chip 2
  Extension 5: Left half of Chip 2 (mostly dead/vignetted)

This code merges the 4-extension format into the 2-extension format:
  Chip 1 = Extension 3 (left) + Extension 2 (right) → merged ext 2
  Chip 2 = Extension 5 (left) + Extension 4 (right) → merged ext 3

This allows uniform processing using the 2-extension reduction path.

|#

(in-package emmi-susi-proc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic methods for determining if amp-merge is needed

(defmethod inst-needs-amp-merge ((inst instrument-id/eso-ntt-emmi+susi:%eso-ntt-emmi+susi))
  (declare (ignore inst))
  nil)

;; Only FA2048/raw4ext needs amp merging
(defmethod inst-needs-amp-merge ((inst instrument-id:eso-ntt-emmi-rild-fa2048/raw4ext))
  (declare (ignore inst))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default method - error for instruments that don't need merging

(defmethod amp-merge ((inst instrument-id/eso-ntt-emmi+susi:%eso-ntt-emmi+susi)
                      fits-file-in fits-file-out
                      &key (overwrite t))
  (declare (ignore fits-file-in fits-file-out overwrite))
  (error "Instrument ~A does not need amp-merge" inst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main amp-merge method for FA2048/raw4ext

(defmethod amp-merge ((inst instrument-id:eso-ntt-emmi-rild-fa2048/raw4ext)
                      fits-file-in fits-file-out
                      &key (overwrite t))
  (declare (ignore inst))
  (cf:maybe-with-open-fits-file (fits-file-in ffin)
    ;; Verify it's a 4-extension file (primary + 4 image extensions = 5 HDUs)
    (let ((num-hdus (cf:fits-file-num-hdus ffin)))
      (when (not (= num-hdus 5))
        (error "Expected 5 HDUs (primary + 4 image extensions) for FA2048/raw4ext, got ~A"
               num-hdus)))

    ;; Verify data format
    (cf:move-to-extension ffin 2) ;; first image extension
    (when (not (and (eql 16 (cf:read-fits-header ffin "BITPIX"))
                    (ignore-errors (= 32768 (cf:read-fits-header ffin "BZERO")))
                    (ignore-errors (= 1 (cf:read-fits-header ffin "BSCALE")))))
      (error "Expected BITPIX=16, BZERO=32768, BSCALE=1 (16-bit unsigned data)"))

    ;; Read all 4 image sections (extensions 2,3,4,5 in HDU numbering)
    (let* ((imsec1 (cf:read-image-section ffin :extension 2 :type :unsigned-byte-16))
           (imsec2 (cf:read-image-section ffin :extension 3 :type :unsigned-byte-16))
           (imsec3 (cf:read-image-section ffin :extension 4 :type :unsigned-byte-16))
           (imsec4 (cf:read-image-section ffin :extension 5 :type :unsigned-byte-16))
           (im1 (cf:image-section-data imsec1))
           (im2 (cf:image-section-data imsec2))
           (im3 (cf:image-section-data imsec3))
           (im4 (cf:image-section-data imsec4))
           ;; Dimensions - all should have same NY, NX may vary
           (ny1 (array-dimension im1 0))
           (nx1 (array-dimension im1 1))
           (ny2 (array-dimension im2 0))
           (nx2 (array-dimension im2 1))
           (ny3 (array-dimension im3 0))
           (nx3 (array-dimension im3 1))
           (ny4 (array-dimension im4 0))
           (nx4 (array-dimension im4 1)))

      ;; Sanity checks - NY should match within each chip pair
      (when (not (= ny1 ny2))
        (error "NY mismatch for chip 1 amps: ~A vs ~A" ny1 ny2))
      (when (not (= ny3 ny4))
        (error "NY mismatch for chip 2 amps: ~A vs ~A" ny3 ny4))

      ;; Create merged arrays - horizontal concatenation
      (let* ((nx-chip1 (+ nx1 nx2))
             (nx-chip2 (+ nx3 nx4))
             (merged-chip1 (make-array (list ny1 nx-chip1) :element-type '(unsigned-byte 16)))
             (merged-chip2 (make-array (list ny3 nx-chip2) :element-type '(unsigned-byte 16))))

        ;; Merge chip 1: im2 (ext3=left half) | im1 (ext2=right half)
        ;; Ext 2 is RIGHT edge of chip 1, Ext 3 is LEFT part of chip 1
        (loop for iy below ny1 do
          (loop for ix below nx2 do
            (setf (aref merged-chip1 iy ix) (aref im2 iy ix)))
          (loop for ix below nx1 do
            (setf (aref merged-chip1 iy (+ nx2 ix)) (aref im1 iy ix))))

        ;; Merge chip 2: im4 (ext5=left half) | im3 (ext4=right half)
        ;; Ext 4 is RIGHT part of chip 2, Ext 5 is LEFT part of chip 2
        (loop for iy below ny3 do
          (loop for ix below nx4 do
            (setf (aref merged-chip2 iy ix) (aref im4 iy ix)))
          (loop for ix below nx3 do
            (setf (aref merged-chip2 iy (+ nx4 ix)) (aref im3 iy ix))))

        ;; Read and divide headers
        (let ((headers-ext1 (progn (cf:move-to-extension ffin 2)
                                   (cf:read-fits-header-list ffin)))
              (headers-ext2 (progn (cf:move-to-extension ffin 3)
                                   (cf:read-fits-header-list ffin)))
              (headers-ext3 (progn (cf:move-to-extension ffin 4)
                                   (cf:read-fits-header-list ffin)))
              (headers-ext4 (progn (cf:move-to-extension ffin 5)
                                   (cf:read-fits-header-list ffin)))
              (headers-primary (progn (cf:move-to-extension ffin 1)
                                      (cf:read-fits-header-list ffin))))

          ;; Create output file
          (multiple-value-bind (hprim hchip1 hchip2)
              (%amp-merge-divide-headers-fa2048/raw4ext
               headers-primary headers-ext1 headers-ext2 headers-ext3 headers-ext4
               nx1 nx2 nx3 nx4)

            (cf:with-new-fits-file (fits-file-out ffout :overwrite overwrite
                                                  :make-primary-headers t)
              (flet ((write-header-list (hdr-list)
                       (loop for (key value comment) in hdr-list
                             do (cond ((equalp key "COMMENT")
                                       (cf:write-fits-comment ffout value))
                                      (t
                                       (cf:write-fits-header ffout key value
                                                             :comment comment))))))

                ;; Write primary header
                (cf:write-fits-comment ffout
                  "FITS file created by amp-merge from 4-ext FA2048 data")
                (cf:write-fits-comment ffout
                  "Original had 2 amps per chip; this has 1 extension per chip")
                (write-header-list hprim)

                ;; Write chip 1 extension
                (cf:add-image-to-fits-file ffout
                                           :ushort
                                           (vector nx-chip1 ny1)
                                           :create-data merged-chip1)
                (write-header-list hchip1)
                (cf:write-fits-header ffout "EXTNAME" "CHIP1"
                                      :comment "Merged from amps 1+2")

                ;; Write chip 2 extension
                (cf:add-image-to-fits-file ffout
                                           :ushort
                                           (vector nx-chip2 ny3)
                                           :create-data merged-chip2)
                (write-header-list hchip2)
                (cf:write-fits-header ffout "EXTNAME" "CHIP2"
                                      :comment "Merged from amps 3+4")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header handling for FA2048/raw4ext amp-merge

;; Headers that should go into primary HDU (not into extensions)
(defparameter *amp-merge-primary-headers-fa2048*
  '("SIMPLE" "EXTEND"
    "ORIGIN" "DATE" "TELESCOP" "INSTRUME"
    "OBJECT" "RA" "DEC" "EQUINOX" "RADECSYS" "EXPTIME" "MJD-OBS" "DATE-OBS" "UTC"
    "LST" "PI-COI" "OBSERVER"
    "HIERARCH ESO ADA ABSROT END" "HIERARCH ESO ADA ABSROT PPOS"
    "HIERARCH ESO ADA ABSROT START" "HIERARCH ESO ADA GUID STATUS"
    "HIERARCH ESO ADA POSANG"
    "HIERARCH ESO DET BITS" "HIERARCH ESO DET CHIPS" "HIERARCH ESO DET DATE"
    "HIERARCH ESO DET DEC" "HIERARCH ESO DET DID"
    "HIERARCH ESO DET EXP NO" "HIERARCH ESO DET EXP RDTTIME"
    "HIERARCH ESO DET EXP TYPE" "HIERARCH ESO DET EXP XFERTIM"
    "HIERARCH ESO DET FRAM ID" "HIERARCH ESO DET FRAM TYPE"
    "HIERARCH ESO DET ID" "HIERARCH ESO DET NAME"
    "HIERARCH ESO DET OUTPUTS" "HIERARCH ESO DET OUTREF" "HIERARCH ESO DET RA"
    "HIERARCH ESO DET READ CLOCK" "HIERARCH ESO DET READ MODE"
    "HIERARCH ESO DET READ NFRAM" "HIERARCH ESO DET READ SPEED"
    "HIERARCH ESO DET SHUT ID" "HIERARCH ESO DET SHUT TMCLOS"
    "HIERARCH ESO DET SHUT TMOPEN" "HIERARCH ESO DET SHUT TYPE"
    "HIERARCH ESO DET SOFW MODE"
    "HIERARCH ESO DET WIN1 BINX" "HIERARCH ESO DET WIN1 BINY"
    "HIERARCH ESO DET WIN1 DIT1" "HIERARCH ESO DET WIN1 DKTM"
    "HIERARCH ESO DET WIN1 NDIT" "HIERARCH ESO DET WIN1 NX"
    "HIERARCH ESO DET WIN1 NY" "HIERARCH ESO DET WIN1 ST"
    "HIERARCH ESO DET WIN1 STRX" "HIERARCH ESO DET WIN1 STRY"
    "HIERARCH ESO DET WIN1 UIT1" "HIERARCH ESO DET WINDOWS"
    "HIERARCH ESO DPR CATG" "HIERARCH ESO DPR TECH" "HIERARCH ESO DPR TYPE"
    "HIERARCH ESO INS DATE" "HIERARCH ESO INS DID"
    "HIERARCH ESO INS FILT1 ID" "HIERARCH ESO INS FILT1 NAME"
    "HIERARCH ESO INS FILT1 NO"
    "HIERARCH ESO INS FILT2 ID" "HIERARCH ESO INS FILT2 NAME"
    "HIERARCH ESO INS FILT2 NO"
    "HIERARCH ESO INS ID"
    "HIERARCH ESO INS MIRR4 ID" "HIERARCH ESO INS MIRR4 NAME"
    "HIERARCH ESO INS MIRR4 NO"
    "HIERARCH ESO INS MODE" "HIERARCH ESO INS SWSIM"
    "HIERARCH ESO OBS DID" "HIERARCH ESO OBS EXECTIME" "HIERARCH ESO OBS GRP"
    "HIERARCH ESO OBS ID" "HIERARCH ESO OBS NAME" "HIERARCH ESO OBS OBSERVER"
    "HIERARCH ESO OBS PI-COI ID" "HIERARCH ESO OBS PI-COI NAME"
    "HIERARCH ESO OBS PROG ID" "HIERARCH ESO OBS START"
    "HIERARCH ESO OBS TARG NAME" "HIERARCH ESO OBS TPLNO"
    "HIERARCH ESO OCS DET IMGNAME"
    "HIERARCH ESO TEL AIRM END" "HIERARCH ESO TEL AIRM START"
    "HIERARCH ESO TEL ALT"
    "HIERARCH ESO TEL AMBI FWHM END" "HIERARCH ESO TEL AMBI FWHM START"
    "HIERARCH ESO TEL AMBI PRES END" "HIERARCH ESO TEL AMBI PRES START"
    "HIERARCH ESO TEL AMBI RHUM" "HIERARCH ESO TEL AMBI TEMP"
    "HIERARCH ESO TEL AMBI WINDDIR" "HIERARCH ESO TEL AMBI WINDSP"
    "HIERARCH ESO TEL AZ" "HIERARCH ESO TEL CHOP ST" "HIERARCH ESO TEL DATE"
    "HIERARCH ESO TEL DID" "HIERARCH ESO TEL DOME STATUS"
    "HIERARCH ESO TEL FOCU ID" "HIERARCH ESO TEL FOCU LEN"
    "HIERARCH ESO TEL FOCU SCALE" "HIERARCH ESO TEL FOCU VALUE"
    "HIERARCH ESO TEL GEOELEV" "HIERARCH ESO TEL GEOLAT"
    "HIERARCH ESO TEL GEOLON" "HIERARCH ESO TEL ID"
    "HIERARCH ESO TEL MOON DEC" "HIERARCH ESO TEL MOON RA" "HIERARCH ESO TEL OPER"
    "HIERARCH ESO TEL PARANG END" "HIERARCH ESO TEL PARANG START"
    "HIERARCH ESO TEL TH M1 TEMP"
    "HIERARCH ESO TEL TRAK RATEA" "HIERARCH ESO TEL TRAK RATED"
    "HIERARCH ESO TEL TRAK STATUS"
    "HIERARCH ESO TPL DID" "HIERARCH ESO TPL EXPNO" "HIERARCH ESO TPL ID"
    "HIERARCH ESO TPL NAME" "HIERARCH ESO TPL NEXP" "HIERARCH ESO TPL PRESEQ"
    "HIERARCH ESO TPL START" "HIERARCH ESO TPL VERSION"
    "ORIGFILE" "ARCFILE" "HDRVER" "COMMENT"))

;; Headers we should not copy (structural headers managed by FITS library)
(defparameter *amp-merge-avoid-headers-fa2048*
  '("XTENSION" "BITPIX" "NAXIS" "NAXIS1" "NAXIS2"
    "BZERO" "BSCALE" "PCOUNT" "GCOUNT"
    "SIMPLE" "EXTEND"))

;; Divide headers from 4-ext file into primary and per-chip headers
;; Returns (values primary-headers chip1-headers chip2-headers)
(defun %amp-merge-divide-headers-fa2048/raw4ext
    (headers-primary headers-ext1 headers-ext2 headers-ext3 headers-ext4
     nx1 nx2 nx3 nx4)
  (declare (ignorable headers-ext2 headers-ext4 nx1 nx2 nx3 nx4))
  (let ((hprim nil)
        (hchip1 nil)
        (hchip2 nil))

    ;; Process primary headers
    (loop for header in headers-primary
          for key = (first header)
          when (not (find key *amp-merge-avoid-headers-fa2048* :test 'equalp))
          do (push header hprim))

    ;; Process headers from ext1 (amp 1 of chip 1) - this becomes chip 1 base
    (loop for header in headers-ext1
          for key = (first header)
          do (cond
               ;; Skip structural headers
               ((find key *amp-merge-avoid-headers-fa2048* :test 'equalp)
                nil)
               ;; Skip headers that go in primary
               ((find key *amp-merge-primary-headers-fa2048* :test 'equalp)
                nil)
               ;; Chip-specific headers for CHIP1
               ((search "CHIP1" key)
                (push header hchip1))
               ;; OUT1 headers for chip 1
               ((or (search "DET OUT1" key)
                    (search "DET OUT " key)) ;; simplified form
                (push header hchip1))
               ;; Other headers go to chip 1
               (t
                (push header hchip1))))

    ;; Process headers from ext3 (amp 1 of chip 2) - this becomes chip 2 base
    (loop for header in headers-ext3
          for key = (first header)
          do (cond
               ;; Skip structural headers
               ((find key *amp-merge-avoid-headers-fa2048* :test 'equalp)
                nil)
               ;; Skip headers that go in primary
               ((find key *amp-merge-primary-headers-fa2048* :test 'equalp)
                nil)
               ;; Chip-specific headers for CHIP2
               ((search "CHIP2" key)
                (push header hchip2))
               ;; OUT2 headers for chip 2
               ((or (search "DET OUT2" key)
                    (search "DET OUT " key)) ;; simplified form
                (push header hchip2))
               ;; Other headers go to chip 2
               (t
                (push header hchip2))))

    ;; Add marker headers to identify this as amp-merged
    (push (list "AMPMERGE" t "Amps merged from 4-ext to 2-ext format") hchip1)
    (push (list "AMPMERGE" t "Amps merged from 4-ext to 2-ext format") hchip2)

    (values (nreverse hprim)
            (nreverse hchip1)
            (nreverse hchip2))))
