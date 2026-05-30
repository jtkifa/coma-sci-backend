

;; reduction plan for GMOS-N array consisting of full or half-chips

;; HALF-CHIPS are present when the image has not been pre-processed to merge chips

;; FULL-CHIPS are when there has been pre-processing



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
   (output-fits-patch-function :initform nil)))

;; requires splitting image into two extensions
(defclass %reduction-plan-eso-ntt-susi2/raw1ext (%%reduction-plan-eso-ntt-emmi+susi)
  ((input-fits-patch-function :initform 'ntt-susi2-fits-input-patch-function/raw1ext)
   (output-fits-patch-function :initform nil)))

(defclass %reduction-plan-eso-ntt-susi2/raw2ext (%%reduction-plan-eso-ntt-emmi+susi)
  ((input-fits-patch-function  :initform  nil)
   (output-fits-patch-function :initform nil)))

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
  (let* ((inst (instrument-id:identify-instrument fits-file))
	 (orig-fits-file (concatenate 'string fits-file ".imred_1ExtVersion")))
    (when (probe-file orig-fits-file) (delete-file orig-fits-file))
    (rename-file fits-file orig-fits-file)
    (emmi-susi-proc:chip-presplit inst orig-fits-file fits-file)
    (delete-file orig-fits-file)
    fits-file))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
