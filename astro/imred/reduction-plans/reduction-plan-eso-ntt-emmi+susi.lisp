
;; Reduction plans for ESO NTT EMMI and SUSI instruments
;;
;; EMMI variants:
;;   EMMI-RILD: TEK2048 (1 chip), THX1024 (1 chip), FA2048 (2 chips, 2 or 4 ext)
;;   EMMI-BIMG: TEK1024 (1 chip), THX1024 (1 chip)
;;
;; SUSI variants:
;;   SUSI (original): 1 chip
;;   SUSI2: 2 chips (1 or 2 extensions in raw form)
;;
;; Some raw formats require pre-processing:
;;   - SUSI2/raw1ext: 2 chips in 1 extension -> split to 2 extensions
;;   - FA2048/raw4ext: 4 amps (2 per chip) -> merge to 2 extensions (1 per chip)

(in-package imred)


;; parent containing defaults
(defclass %%reduction-plan-eso-ntt-emmi+susi  (reduction-plan)
  ((inst-id-type :initform nil)
   (trim :initform t)
   (trimsec :initform nil) ;; use instrument-id package
   (overscan-subtract :initform nil)
   (overscans :initform nil)
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   ;; filter2 has the normal broadband filters
   (flat-name   :initform "FLAT")
   (min-flat-counts :initform 4000)  ;; saturation is 100K e-
   (max-flat-counts :initform 60000) ;; saturation is 100K e-
   (min-flat-frames :initform 1) ;; to allow certain old data to be procesed
   (min-final-flat-value :initform 0.0001)
   (input-fits-patch-function :initform nil)
   (output-fits-patch-function :initform 'ntt-emmi+susi-write-initial-wcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WCS writing for output files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write initial WCS to appropriate extensions based on instrument type
(defun %write-initial-wcs-to-extensions (fits-file)
  "Write initial WCS to FITS file extensions if available from instrument-id.
   For onechip instruments, writes to the single image extension.
   For multichip instruments (2-chip), writes to extensions 2 and 3."
  (let ((inst (instrument-id:identify-instrument fits-file)))
    (cond
      ;; onechip: WCS method determines the extension automatically
      ((typep inst 'instrument-id:onechip)
       (let ((wcs (ignore-errors
                    (instrument-id:get-initial-wcs-for-fits fits-file))))
         (when wcs
           (cf:write-wcs wcs fits-file
                         :extension (instrument-id:get-image-extension-for-onechip-instrument
                                     inst fits-file)))))
      ;; multichip (2-chip): extensions 2 and 3
      ((typep inst 'instrument-id:multichip)
       (dolist (ext '(2 3))
         (let ((wcs (ignore-errors
                      (instrument-id:get-initial-wcs-for-fits fits-file :extension ext))))
           (when wcs
             (cf:write-wcs wcs fits-file :extension ext)))))))
  fits-file)

;; Default output patch function - writes initial WCS
(defun ntt-emmi+susi-write-initial-wcs (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (%write-initial-wcs-to-extensions fits-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for float conversion with NaN masking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Read extension data as float and headers
(defun %read-ext-data-and-headers (ff ext)
  "Read image data as single-float and headers from extension."
  (cf:move-to-extension ff ext)
  (let ((imsec (cf:read-image-section ff :type :single-float))
        (headers (cf:read-fits-header-list ff)))
    (values (cf:image-section-data imsec) headers)))

;; Write extension as float with given data and headers
(defun %write-float-ext-with-headers (ff data headers)
  "Add a float image extension with given data and headers."
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1)))
    (cf:add-image-to-fits-file ff :float (vector nx ny) :create-data data)
    ;; Write headers, excluding those that are auto-generated
    (loop for (key value comment) in headers
          unless (member key '("SIMPLE" "XTENSION" "BITPIX" "NAXIS" "NAXIS1" "NAXIS2"
                               "PCOUNT" "GCOUNT" "BZERO" "BSCALE" "END")
                         :test 'equalp)
          do (cond ((equalp key "COMMENT")
                    (cf:write-fits-comment ff value))
                   ((equalp key "HISTORY")
                    (cf:write-fits-comment ff (format nil "HISTORY ~A" value)))
                   (t
                    (cf:write-fits-header ff key value :comment comment))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; requires splitting image into two extensions
(defclass %reduction-plan-eso-ntt-susi2/raw1ext (%%reduction-plan-eso-ntt-emmi+susi)
  ((input-fits-patch-function :initform 'ntt-susi2-fits-input-patch-function/raw1ext)))

(defclass %reduction-plan-eso-ntt-susi2/raw2ext (%%reduction-plan-eso-ntt-emmi+susi)
  ())

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-susi2/raw1ext))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-susi2/raw1ext)

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-susi2/raw2ext))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-susi2/raw2ext)


 
;; split the two extensions and destroy the parent fits file
(defun ntt-susi2-fits-input-patch-function/raw1ext (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (let* ((fits-file (namestring (truename fits-file)))  ;; ensure clean absolute path
         (inst (instrument-id:identify-instrument fits-file))
	 (orig-fits-file (concatenate 'string fits-file ".imred_1ExtVersion")))
    (when (probe-file orig-fits-file) (delete-file orig-fits-file))
    (rename-file fits-file orig-fits-file)
    (emmi-susi-proc:chip-presplit inst orig-fits-file fits-file)
    (delete-file orig-fits-file)
    fits-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUSI (original) - 1 chip, 1 HDU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass %reduction-plan-eso-ntt-susi/raw (%%reduction-plan-eso-ntt-emmi+susi)
  ())

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-susi/raw))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-susi/raw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMI-RILD-TEK2048 - 1 chip, 1 HDU
;;   Has bad regions that need to be masked with NaN after reduction:
;;   - Columns 1-19 (left edge)
;;   - Columns >= 2068 (right edge)
;;   - Rows above an irregular upper boundary defined by (X,Y) vertices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass %reduction-plan-eso-ntt-emmi-rild-tek2048/raw (%%reduction-plan-eso-ntt-emmi+susi)
  ((input-fits-patch-function :initform 'ntt-emmi-rild-tek2048-input-patch-function)))
  ;; output-fits-patch-function inherits default WCS writing from parent

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-emmi-rild-tek2048/raw))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-emmi-rild-tek2048/raw)

;; Upper boundary vertices for TEK2048 bad region (0-based pixel indices)
;; Converted from 1-based: (21,1938) (281,1975) ... -> (20,1937) (280,1974) ...
(defparameter *emmi-rild-tek2048-upper-boundary*
  '((20 1937) (280 1974) (623 1991) (1340 2002) (1410 2001) (1761 1999) (1923 1986) (2065 1938)))

;; Apply TEK2048 bad region mask to a float data array
(defun %apply-tek2048-nan-mask (data)
  "Set bad regions in EMMI-RILD-TEK2048 image data to NaN.
   Bad regions (0-based indices):
   1. Columns 0-18 (left edge)
   2. Columns >= 2067 (right edge)
   3. Rows above the interpolated upper boundary"
  (let ((nan float-utils:*single-float-nan*))
    (imutils:imfill-pixels-by-region data nan
      :left-of-x 19                              ; columns 0-18
      :right-of-x 2066                           ; columns >= 2067
      :above-y *emmi-rild-tek2048-upper-boundary*)
    data))

;; Input patch function for TEK2048 - masks bad/vignetted regions with NaN
;; Applied to ALL input files (including flat inputs) before processing
;; Converts file to float format to support NaN storage
(defun ntt-emmi-rild-tek2048-input-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (let ((temp-file (format nil "~A.tmp-float-convert" fits-file)))
    (cf:with-open-fits-file (fits-file ffin :mode :input)
      ;; Find the image extension
      (let ((img-ext (if (and (eq (cf:fits-file-current-hdu-type ffin) :image)
                              (> (cf:fits-file-current-image-ndims ffin) 0))
                         1  ;; primary is the image
                         2))) ;; else first extension after primary
        ;; Read primary headers
        (let ((prim-headers (cf:read-fits-header-list ffin)))
          ;; Read and process image extension
          (multiple-value-bind (data img-headers)
              (%read-ext-data-and-headers ffin img-ext)
            (%apply-tek2048-nan-mask data)
            ;; Write new file
            (cf:with-new-fits-file (temp-file ffout :overwrite t :make-primary-headers t)
              ;; Write primary headers
              (loop for (key value comment) in prim-headers
                    unless (member key '("SIMPLE" "BITPIX" "NAXIS" "EXTEND" "END")
                                   :test 'equalp)
                    do (cond ((equalp key "COMMENT")
                              (cf:write-fits-comment ffout value))
                             ((equalp key "HISTORY")
                              (cf:write-fits-comment ffout (format nil "HISTORY ~A" value)))
                             (t
                              (cf:write-fits-header ffout key value :comment comment))))
              (cf:write-fits-comment ffout "IMRED: Converted to float format for NaN masking")
              ;; Write image as float
              (if (= img-ext 1)
                  ;; Image is in primary - need to write as extension for float support
                  (progn
                    (%write-float-ext-with-headers ffout data img-headers))
                  ;; Image is already in extension
                  (%write-float-ext-with-headers ffout data img-headers)))))))
    ;; Replace original with converted file
    (delete-file fits-file)
    (rename-file temp-file fits-file))
  fits-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMI-RILD-THX1024 - 1 chip, 1 HDU (not seen, guessed from docs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass %reduction-plan-eso-ntt-emmi-rild-thx1024/raw (%%reduction-plan-eso-ntt-emmi+susi)
  ())

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-emmi-rild-thx1024/raw))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-emmi-rild-thx1024/raw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMI-BIMG-TEK1024 - 1 chip, 1 HDU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass %reduction-plan-eso-ntt-emmi-bimg-tek1024/raw (%%reduction-plan-eso-ntt-emmi+susi)
  ())

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-emmi-bimg-tek1024/raw))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-emmi-bimg-tek1024/raw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMI-BIMG-THX1024 - 1 chip, 1 HDU (not seen, guessed from docs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass %reduction-plan-eso-ntt-emmi-bimg-thx1024/raw (%%reduction-plan-eso-ntt-emmi+susi)
  ())

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-emmi-bimg-thx1024/raw))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-emmi-bimg-thx1024/raw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMI-RILD-FA2048 - 2 chips
;;   raw2ext: 2 HDUs (1 per chip) - straightforward
;;   raw4ext: 4 HDUs (2 amps per chip) - needs pre-processing to merge amps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass %reduction-plan-eso-ntt-emmi-rild-fa2048/raw2ext (%%reduction-plan-eso-ntt-emmi+susi)
  ((input-fits-patch-function :initform 'ntt-emmi-rild-fa2048-input-patch-function)))
  ;; output-fits-patch-function inherits default WCS writing from parent

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-emmi-rild-fa2048/raw2ext))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-emmi-rild-fa2048/raw2ext)

;; FA2048 with 4 extensions needs amp merging before reduction
(defclass %reduction-plan-eso-ntt-emmi-rild-fa2048/raw4ext (%%reduction-plan-eso-ntt-emmi+susi)
  ((input-fits-patch-function :initform 'ntt-emmi-rild-fa2048-fits-input-patch-function/raw4ext)))
  ;; output-fits-patch-function inherits default WCS writing from parent

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:eso-ntt-emmi-rild-fa2048/raw4ext))
  (declare (ignorable inst))
  '%reduction-plan-eso-ntt-emmi-rild-fa2048/raw4ext)

;; Input patch function for FA2048/raw2ext - masks bad/vignetted regions with NaN
;; Applied to ALL input files (including flat inputs) before processing
(defun ntt-emmi-rild-fa2048-input-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (%emmi-rild-fa2048-nan-all-bad-regions fits-file)
  fits-file)

;; Merge 4 extensions (2 amps per chip) into 2 extensions (1 per chip),
;; then mask bad/vignetted regions with NaN
(defun ntt-emmi-rild-fa2048-fits-input-patch-function/raw4ext (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (let* ((fits-file (namestring (truename fits-file)))  ;; ensure clean absolute path
         (inst (instrument-id:identify-instrument fits-file))
         (orig-fits-file (concatenate 'string fits-file ".imred_4ExtVersion")))
    (when (probe-file orig-fits-file) (delete-file orig-fits-file))
    (rename-file fits-file orig-fits-file)
    (emmi-susi-proc:amp-merge inst orig-fits-file fits-file)
    (delete-file orig-fits-file)
    ;; Now mask bad regions (after amp merge, we have standard 2-ext format)
    (%emmi-rild-fa2048-nan-all-bad-regions fits-file)
    fits-file))

;; FA2048 bad region masking polygons (0-based pixel indices)
;; These define the GOOD regions - everything outside gets NaN'd
;; Polygons are defined for 2x2 binned reference dimensions and scaled at runtime
;;
;; Reference dimensions for 2x2 binned reduced data
(defparameter *emmi-rild-fa2048-reference-nx* 1031)
(defparameter *emmi-rild-fa2048-reference-ny* 2048)

;; Extension 2 (Chip 1): Has vignetting on right side creating curved boundary
;; The polygon traces the good region clockwise from bottom-left
;; Coordinates are for 2x2 binned reference
(defparameter *emmi-rild-fa2048-ext2-good-polygon/ref*
  '((7 96)      ; bottom-left corner
    (750 106)   ; bottom edge before curve starts
    (850 131)   ; bottom-right curve begins
    (900 157)   ; continuing curve
    (950 196)   ; continuing curve
    (1000 261)  ; continuing curve
    (1030 300)  ; right edge straight portion begins
    (1030 1600) ; right edge straight portion ends
    (1008 1700) ; top-right curve begins
    (946 1800)  ; continuing curve
    (888 1850)  ; continuing curve
    (765 1900)  ; top of vignetting curve
    (50 1895)   ; top edge
    (7 1850)))  ; back to left edge

;; Extension 3 (Chip 2): Has dead band on left (columns 0-~420)
;; The good region is approximately rectangular starting at column 420
;; Coordinates are for 2x2 binned reference
(defparameter *emmi-rild-fa2048-ext3-good-polygon/ref*
  '((430 85)    ; bottom-left (after dead band)
    (1030 94)   ; bottom-right
    (1030 1893) ; top-right
    (430 1884))); top-left

;; Scale a polygon based on actual image dimensions vs reference
(defun %scale-polygon-for-binning (polygon actual-nx actual-ny ref-nx ref-ny)
  "Scale polygon coordinates from reference dimensions to actual dimensions."
  (let ((scale-x (/ (float actual-nx) (float ref-nx)))
        (scale-y (/ (float actual-ny) (float ref-ny))))
    (mapcar (lambda (pt)
              (list (round (* (first pt) scale-x))
                    (round (* (second pt) scale-y))))
            polygon)))

;; Set bad regions to NaN for a float data array
(defun %apply-nan-mask-to-data (data polygon/ref ref-nx ref-ny)
  "Set pixels outside the good-region polygon to NaN.
   Returns the modified data array."
  (let* ((nan float-utils:*single-float-nan*)
         (actual-ny (array-dimension data 0))
         (actual-nx (array-dimension data 1))
         (polygon (%scale-polygon-for-binning polygon/ref
                                               actual-nx actual-ny
                                               ref-nx ref-ny)))
    (imutils:imfill-pixels-by-region data nan :out-of-polygon polygon)
    data))

;; Mask bad regions on all extensions of FA2048 file
;; Called from input patch functions to mask vignetted regions before flat combining
;; Converts file to float format to support NaN values
(defun %emmi-rild-fa2048-nan-all-bad-regions (fits-file)
  "Mask bad/vignetted regions with NaN on both extensions of FA2048 FITS file.
   Converts image extensions to float format to support NaN storage."
  (let ((temp-file (format nil "~A.tmp-float-convert" fits-file)))
    ;; Create new file with float extensions
    (cf:with-open-fits-file (fits-file ffin :mode :input)
      ;; Read primary HDU headers
      (let ((prim-headers (cf:read-fits-header-list ffin)))
        ;; Read image extensions
        (multiple-value-bind (data2 headers2)
            (%read-ext-data-and-headers ffin 2)
          (multiple-value-bind (data3 headers3)
              (%read-ext-data-and-headers ffin 3)
            ;; Apply NaN masks
            (%apply-nan-mask-to-data data2
                                      *emmi-rild-fa2048-ext2-good-polygon/ref*
                                      *emmi-rild-fa2048-reference-nx*
                                      *emmi-rild-fa2048-reference-ny*)
            (%apply-nan-mask-to-data data3
                                      *emmi-rild-fa2048-ext3-good-polygon/ref*
                                      *emmi-rild-fa2048-reference-nx*
                                      *emmi-rild-fa2048-reference-ny*)
            ;; Write new file
            (cf:with-new-fits-file (temp-file ffout :overwrite t :make-primary-headers t)
              ;; Write primary headers
              (loop for (key value comment) in prim-headers
                    unless (member key '("SIMPLE" "BITPIX" "NAXIS" "EXTEND" "END")
                                   :test 'equalp)
                    do (cond ((equalp key "COMMENT")
                              (cf:write-fits-comment ffout value))
                             ((equalp key "HISTORY")
                              (cf:write-fits-comment ffout (format nil "HISTORY ~A" value)))
                             (t
                              (cf:write-fits-header ffout key value :comment comment))))
              (cf:write-fits-comment ffout "IMRED: Converted to float format for NaN masking")
              ;; Write extensions as float
              (%write-float-ext-with-headers ffout data2 headers2)
              (%write-float-ext-with-headers ffout data3 headers3))))))
    ;; Replace original with converted file
    (delete-file fits-file)
    (rename-file temp-file fits-file))
  fits-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
