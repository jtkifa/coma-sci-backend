

(in-package instrument-id)

;; parent class of all GMOS
(defclass %gmos (imaging-instrument)
  ((name :initarg :name :initform "GMOS" :accessor instrument-name) ;; a string
   (aperture :initform 8.1)
   (observatory :initform "")))


;; parent class of frac-chip or full-chip single chip fits files
(defclass %gmos-onechip (%gmos onechip)
  ())


;; a half chip (img from one amp)
(defclass %gmos-frac-chip (%gmos-onechip)
  ())

;; a full chip (combined amps)
(defclass %gmos-full-chip (%gmos-onechip)
  ())

(defclass %gmos-north-mixin ()
  ((observatory :initarg :observatory  :initform "Gemini-North"
		:accessor instrument-observatory)))
(defclass %gmos-south-mixin ()
  ((observatory :initarg :observatory  :initform "Gemini-South"
		:accessor instrument-observatory)))

(defclass %gmos-multichip (%gmos multichip)
  ())

(defclass %gmos-frac-chip-array (%gmos-multichip)
  ())

(defclass %gmos-full-chip-array (%gmos-multichip)
  ())

;; these are MIXINS for convenience in classifying/identifying
(defclass %gmos-e2v () ()) 
(defclass %gmos-hamamatsu () ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the data as you get it from the telescope - 6 images each
;; with frac-chip image
(defclass/inst gmos-n-e2v-frac-chip-array (%gmos-north-mixin %gmos-frac-chip-array %gmos-e2v)
  ((name :initform "GMOS-N-E2V-FRAC-CHIP-ARRAY")))

;; north

;; an array of 3 chips, combined across amps
(defclass/inst gmos-n-e2v-full-chip-array (%gmos-north-mixin %gmos-full-chip-array %gmos-e2v)
  ((name :initform "GMOS-N-E2V-FULL-CHIP-ARRAY")))

;; single uncombined frac-chip for gmos-north e2v chips
(defclass/inst gmos-n-e2v-frac-chip (%gmos-north-mixin %gmos-frac-chip %gmos-e2v)
  ((name :initform "GMOS-N-E2V-FRAC-CHIP")))

;; image of one chip combined across both amps
(defclass/inst gmos-n-e2v-full-chip (%gmos-north-mixin %gmos-full-chip %gmos-e2v)
  ((name :initform "GMOS-N-E2V-FULL-CHIP")))


(defclass/inst gmos-n-e2v-frac-chip-array (%gmos-north-mixin %gmos-frac-chip-array  %gmos-e2v)
  ((name :initform "GMOS-N-E2V-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-n-e2v-full-chip-array (%gmos-north-mixin %gmos-full-chip-array  %gmos-e2v)
  ((name :initform "GMOS-N-E2V-FULL-CHIP-ARRAY")))


;;;;  the oldest chips (pre 2011?)
(defclass/inst gmos-n-e2v-old-frac-chip-array
  (%gmos-north-mixin %gmos-frac-chip-array %gmos-e2v)
  ((name :initform "GMOS-N-E2V-OLD-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-n-e2v-old-full-chip-array
  (%gmos-north-mixin %gmos-full-chip-array %gmos-e2v)
  ((name :initform "GMOS-N-E2V-OLD-FULL-CHIP-ARRAY")))

;; single uncombined frac-chip for gmos-north e2v-old chips
(defclass/inst gmos-n-e2v-old-frac-chip (%gmos-north-mixin %gmos-frac-chip %gmos-e2v)
  ((name :initform "GMOS-N-E2V-OLD-FRAC-CHIP")))

;; image of one chip combined across both amps
(defclass/inst gmos-n-e2v-old-full-chip (%gmos-north-mixin %gmos-full-chip %gmos-e2v)
  ((name :initform "GMOS-N-E2V-OLD-FULL-CHIP")))


(defclass/inst gmos-n-e2v-old-frac-chip-array
  (%gmos-north-mixin %gmos-frac-chip-array  %gmos-e2v)
  ((name :initform "GMOS-N-E2V-OLD-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-n-e2v-old-full-chip-array (%gmos-north-mixin %gmos-full-chip-array  %gmos-e2v)
  ((name :initform "GMOS-N-E2V-OLD-FULL-CHIP-ARRAY")))

;; south

(defclass/inst gmos-n-hamamatsu-frac-chip-array (%gmos-north-mixin %gmos-frac-chip-array  %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-n-hamamatsu-full-chip-array
  (%gmos-north-mixin %gmos-full-chip-array  %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-FULL-CHIP-ARRAY")))


;; single uncombined frac-chip for gmos-north hamamatsu chips
(defclass/inst gmos-n-hamamatsu-frac-chip (%gmos-north-mixin %gmos-frac-chip %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-FRAC-CHIP")))

;; image of one chip combined across both amps
(defclass/inst gmos-n-hamamatsu-full-chip (%gmos-north-mixin %gmos-full-chip %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-FULL-CHIP")))


(defclass/inst gmos-n-hamamatsu-frac-chip-array
  (%gmos-north-mixin %gmos-frac-chip-array %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-n-hamamatsu-full-chip-array
  (%gmos-north-mixin %gmos-full-chip-array %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-FULL-CHIP-ARRAY")))

;;;;

(defclass/inst gmos-s-eev-frac-chip-array (%gmos-south-mixin %gmos-frac-chip-array %gmos-e2v)
  ((name :initform "GMOS-S-EEV-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-s-eev-full-chip-array (%gmos-south-mixin %gmos-full-chip-array %gmos-e2v)
  ((name :initform "GMOS-S-EEV-FULL-CHIP-ARRAY")))


;; single uncombined frac-chip for gmos-sorth e2v chips
(defclass/inst gmos-s-eev-frac-chip (%gmos-south-mixin %gmos-frac-chip %gmos-e2v)
  ((name :initform "GMOS-S-EEV-FRAC-CHIP")))

;; image of one chip combined across both amps
(defclass/inst gmos-s-eev-full-chip (%gmos-south-mixin %gmos-full-chip %gmos-e2v)
  ((name :initform "GMOS-S-EEV-FULL-CHIP")))


(defclass/inst gmos-s-eev-frac-chip-array (%gmos-south-mixin %gmos-frac-chip-array %gmos-e2v)
  ((name :initform "GMOS-S-EEV-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-s-eev-full-chip-array (%gmos-south-mixin %gmos-full-chip-array)
  ((name :initform "GMOS-S-EEV-FULL-CHIP-ARRAY")))

;;;; 

(defclass/inst gmos-s-hamamatsu-frac-chip-array
  (%gmos-south-mixin %gmos-frac-chip-array %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-s-hamamatsu-full-chip-array
  (%gmos-south-mixin %gmos-full-chip-array %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-FULL-CHIP-ARRAY")))


;; single uncombined frac-chip for gmos-sorth hamamatsu chips
(defclass/inst gmos-s-hamamatsu-frac-chip
  (%gmos-south-mixin %gmos-frac-chip %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-FRAC-CHIP")))

;; image of one chip combined across both amps
(defclass/inst gmos-s-hamamatsu-full-chip
  (%gmos-south-mixin %gmos-full-chip %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-FULL-CHIP")))


(defclass/inst gmos-s-hamamatsu-frac-chip-array
  (%gmos-south-mixin %gmos-frac-chip-array %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-FRAC-CHIP-ARRAY")))

;; an array of 3 chips, combined across amps
(defclass/inst gmos-s-hamamatsu-full-chip-array
  (%gmos-south-mixin %gmos-full-chip-array %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-FULL-CHIP-ARRAY")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a special class for a SWARP'ed GMOS file
(defclass/inst %gmos-swarped-file (%gmos-onechip)
  ((name :initform "GMOS-SWARPED-FILE")))

;; north

(defclass/inst %gmos-n-swarped-file (%gmos-north-mixin %gmos-swarped-file)
  ((name :initform "GMOS-N-SWARPED-FILE")))

(defclass/inst gmos-n-e2v-swarped-file (%gmos-n-swarped-file %gmos-e2v)
  ((name :initform "GMOS-N-E2V-SWARPED-FILE")))

(defclass/inst gmos-n-hamamatsu-swarped-file (%gmos-n-swarped-file %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-SWARPED-FILE")))

;; south

(defclass/inst %gmos-s-swarped-file (%gmos-south-mixin %gmos-swarped-file)
  ((name :initform "GMOS-S-SWARPED-FILE")))

(defclass/inst gmos-s-eev-swarped-file (%gmos-s-swarped-file %gmos-e2v)
  ((name :initform "GMOS-S-EEV-SWARPED-FILE")))

(defclass/inst gmos-s-hamamatsu-swarped-file (%gmos-s-swarped-file %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-SWARPED-FILE")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mosaic reduced by our IMRED

(defclass/inst %gmos-imred-mosaic-file (%gmos-onechip)
  ((name :initform "GMOS-IMRED-MOSAIC")))


;; north

(defclass/inst %gmos-n-imred-mosaic-file (%gmos-north-mixin %gmos-imred-mosaic-file)
  ((name :initform "GMOS-N-IMRED-MOSAIC")))

(defclass/inst gmos-n-e2v-imred-mosaic-file (%gmos-n-imred-mosaic-file %gmos-e2v)
  ((name :initform "GMOS-N-E2V-IMRED-MOSAIC")))

(defclass/inst gmos-n-hamamatsu-imred-mosaic-file (%gmos-n-imred-mosaic-file  %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-IMRED-MOSAIC")))

;; south

(defclass/inst %gmos-s-imred-mosaic-file (%gmos-south-mixin %gmos-imred-mosaic-file)
  ((name :initform "GMOS-S-IMRED-MOSAIC")))

(defclass/inst gmos-s-eev-imred-mosaic-file (%gmos-s-imred-mosaic-file %gmos-e2v)
  ((name :initform "GMOS-S-EEV-IMRED-MOSAIC")))

(defclass/inst gmos-s-hamamatsu-imred-mosaic-file (%gmos-s-imred-mosaic-file  %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-IMRED-MOSAIC")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mosaic reduced by official DRAGONS package

(defclass %gmos-dragons-mixin () ())

(defclass/inst %gmos-dragons-mosaic-file (%gmos-dragons-mixin %gmos-onechip)
  ((name :initform "GMOS-DRAGONS-MOSAIC")))

;; north

(defclass/inst %gmos-n-dragons-mosaic-file (%gmos-north-mixin %gmos-dragons-mosaic-file)
  ((name :initform "GMOS-N-DRAGONS-MOSAIC")))

(defclass/inst gmos-n-e2v-dragons-mosaic-file (%gmos-n-dragons-mosaic-file %gmos-e2v)
  ((name :initform "GMOS-N-E2V-DRAGONS-MOSAIC")))

(defclass/inst gmos-n-hamamatsu-dragons-mosaic-file (%gmos-n-dragons-mosaic-file %gmos-hamamatsu)
  ((name :initform "GMOS-N-HAMAMATSU-DRAGONS-MOSAIC")))

;; south

(defclass/inst %gmos-s-dragons-mosaic-file (%gmos-south-mixin %gmos-dragons-mosaic-file)
  ((name :initform "GMOS-S-DRAGONS-MOSAIC")))

(defclass/inst gmos-s-eev-dragons-mosaic-file (%gmos-s-dragons-mosaic-file %gmos-e2v)
  ((name :initform "GMOS-S-EEV-DRAGONS-MOSAIC")))

(defclass/inst gmos-s-hamamatsu-dragons-mosaic-file (%gmos-s-dragons-mosaic-file %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-DRAGONS-MOSAIC")))




;; a version that has extra extensions - stripped out; maybe one or two extensions
(defclass %gmos-stripped-mixin () ())
;; north
(defclass %gmos-stripped-dragons-mosaic-file (%gmos-stripped-mixin %gmos-dragons-mosaic-file) ())


(defclass/inst %gmos-n-dragons-mosaic-stripped-file
  (%gmos-north-mixin %gmos-stripped-dragons-mosaic-file)
  ((name :initform "GMOS-N-DRAGONS-MOSAIC-STRIPPED")))

(defclass/inst gmos-n-hamamatsu-eev-mosaic-stripped-file
  (%gmos-n-dragons-mosaic-stripped-file %gmos-e2v %gmos-stripped-mixin)
  ((name :initform "GMOS-N-E2V-DRAGONS-MOSAIC-STRIPPED")))

(defclass/inst gmos-n-hamamatsu-dragons-mosaic-stripped-file
  (%gmos-n-dragons-mosaic-stripped-file %gmos-hamamatsu %gmos-stripped-mixin)
  ((name :initform "GMOS-N-HAMAMATSU-DRAGONS-MOSAIC-STRIPPED")))

;; south

(defclass/inst %gmos-s-dragons-mosaic-stripped-file
  (%gmos-south-mixin %gmos-stripped-dragons-mosaic-file)
  ((name :initform "GMOS-S-DRAGONS-MOSAIC-STRIPPED")))

(defclass/inst gmos-s-e2v-dragons-mosaic-stripped-file
  (%gmos-s-dragons-mosaic-stripped-file %gmos-e2v)
  ((name :initform "GMOS-S-EEV-DRAGONS-MOSAIC-STRIPPED")))

(defclass/inst gmos-s-hamamatsu-dragons-mosaic-stripped-file
  (%gmos-s-dragons-mosaic-stripped-file %gmos-hamamatsu)
  ((name :initform "GMOS-S-HAMAMATSU-DRAGONS-MOSAIC-STRIPPED")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return (values telescope detector type) where telescope is "N" or "S",
;; and detector is "EEV" "e2v" or "Hamamatsu",
;; and  type is "
;; this is in progress to clean up %gmos-identify-instrument

#+NIL ;; THIS WAS IN PROGRES
(defun %gmos-get-telescope/detector/type (fits-file) ..)

(defun %gmos-identify-instrument (fits-file)
  (let* ((instr (cf:read-fits-header fits-file "INSTRUME" :extension 1))
	 ;; detec changes after Dragons reduction
	 (detec (cf:read-fits-header fits-file "DETECTOR" :extension 1))
	 (detid (cf:read-fits-header fits-file "DETID" :extension 1))
	 (eev   (search "EEV" detid :test 'equalp)) ;; fallback if DETECTOR header fails
	 (ham   (search "HAM" detid :test 'equalp))
	 (softw  (cf:read-fits-header fits-file "SOFTNAME")) ;; for swarp
	 (is-imred-mosaic (cf:read-fits-header fits-file
					       "IMRED.GMOS-MOSAIC"
					       :extension 1)) 
	 (nhdu  (if (typep fits-file 'cf:fits-file)
		    (cf:fits-file-num-hdus fits-file)
		    (cf:with-open-fits-file (fits-file ff)
		      (cf:fits-file-num-hdus ff))))
	 (naxis (%gethead-or-error fits-file "NAXIS" :extension 1))
	 (naxis1 (cf:read-fits-header fits-file "NAXIS1" :extension 1))
	 (gmoscombined (cf:read-fits-header fits-file
					    "IMRED.GMOSCOMBINED"))
	 (is-dragons-mosaic-any ;; dragons does not provide a simple way of identifying it
	   (and
	    (cf:read-fits-header fits-file "GEM-TLM"  ;; last mod time with GEMINI
				 :extension 1)
	    (cf:read-fits-header fits-file "SDZHDRSG" ;; standardize obs hdr
				 :extension 1)
	    (cf:read-fits-header fits-file "MOSAIC"
				 :extension 1)))
	 (is-dragons-mosaic ;; later generations of DRAGONS added more extensions
	   (and is-dragons-mosaic-any (>= nhdu 4)))
	 (is-dragons-mosaic-stripped ;; can be simple one ext
	   (and is-dragons-mosaic-any (<= nhdu 2)))
	 (chip-type
	   (cond ((search "e2v" detec :test 'equalp) "e2v")
		 ((search "ham" detec :test 'equalp) "hamamatsu")
		 ;; fallback for dragons reduced images that don't preserve DETEC
		 ((and is-dragons-mosaic-any detid ;; DRAGONS and we found DETID header
		       (or eev ham) ;; one of EEV or HAM
		       (not (and eev ham))) ;; but not both
		  (cond (eev "e2v")
			(ham "hamamatsu")
			(t   "unknown")))
		 ;;
		 (t "unknown"))))

    #+nil
    (format t "is-dragons: ~A~%is-stripped: ~A~%chip-type: ~A~%"
	    is-dragons-mosaic is-dragons-mosaic-stripped chip-type)
    
    (cond
      ;; NOT GMOS
      ((not (member instr '("GMOS-N" "GMOS-S") :test 'equalp))
       nil)
      ;; first test - a swarped GMOS file
      (t ;; all GMOS
       (when (equalp chip-type "unknown")
	 (error
	  "Cannot identify Gemini GMOS chip type as one of 'e2v' or 'Hamamatsu': detector='~A'"
	  detec))
       ;; now test gemini flavors
       (cond ((equalp softw  "SWarp")
	      (make-instance (cond ((equalp instr "GMOS-N")
				    (cond ((equalp chip-type "e2v")
					   'gmos-n-e2v-swarped-file)
					  ((equalp chip-type "hamamatsu")
					   'gmos-n-hamamatsu-swarped-file)))
				   ((equalp instr "GMOS-S")
				    (cond ((equalp chip-type "e2v")
					   'gmos-s-eev-swarped-file)
					  ((equalp chip-type "hamamatsu")
					   'gmos-s-hamamatsu-swarped-file))))))
	     ;; is it a gmos mosaic?
	     (is-imred-mosaic
	      (make-instance (cond ((equalp instr "GMOS-N")
				    (cond ((equalp chip-type "e2v")
					   'gmos-n-e2v-imred-mosaic-file)
					  ((equalp chip-type "hamamatsu")
					   'gmos-n-hamamatsu-imred-mosaic-file)))
				   ((equalp instr "GMOS-S")
				    (cond ((equalp chip-type "e2v")
					   'gmos-s-eev-imred-mosaic-file)
					  ((equalp chip-type "hamamatsu")
					   'gmos-s-hamamatsu-imred-mosaic-file))))))
			     
	     ;; is it a dragons mosaic?
	     (is-dragons-mosaic
	      (make-instance
	       (cond ((equalp instr "GMOS-N")
	              (cond ((equalp chip-type "e2v")
			     'gmos-n-e2v-dragons-mosaic-file)
			    ((equalp chip-type "hamamatsu")
			     'gmos-n-hamamatsu-dragons-mosaic-file)))
		     ((equalp instr "GMOS-S")
	              (cond ((equalp chip-type "e2v")
			     'gmos-s-eev-dragons-mosaic-file)
			    ((equalp chip-type "hamamatsu")
			     'gmos-s-hamamatsu-dragons-mosaic-file))))))
	     ;; is a stripped dragons mosaic?
	     (is-dragons-mosaic-stripped
	       (make-instance
		(cond ((equalp instr "GMOS-N")
	               (cond ((equalp chip-type "e2v")
			      'gmos-n-e2v-dragons-stripped-mosaic-file)
			     ((equalp chip-type "hamamatsu")
			      'gmos-n-hamamatsu-dragons-mosaic-stripped-file)))
		      ((equalp instr "GMOS-S")
	               (cond ((equalp chip-type "e2v")
			      'gmos-s-eev-dragons-stripped-mosaic-file)
			     ((equalp chip-type "hamamatsu")
			      'gmos-s-hamamatsu-dragons-stripped-mosaic-file))))))
	     ;;
	     ;; GMOS-NORTH
	     ((equalp instr "GMOS-N")
	      (cond ((equalp detec "GMOS + Red1")  
		     ;;
		     (cond ((and (equalp naxis 0)
				 (= nhdu 7)
				 (not gmoscombined))
			    (make-instance 'gmos-n-e2v-old-frac-chip-array ))
			   ((and (equalp naxis 0)
				 (= nhdu 4)
				 gmoscombined)
			    (make-instance 'gmos-n-e2v-old-full-chip-array))
			   ((and (eql naxis 2)
				 (= nhdu 1)
				 (= naxis1 1056)) ;; this one had ONE amp, I think ???
			    (make-instance
			     'gmos-n-e2v-old-full-chip
			     ;; our IMRED routine COMBINE-GMOS-CHIPS writes
			     ;; EXTNAME=CHIPn for n=1,2,3
			     ;;   FIXME - no idea what chip naming scheme is, so assume "2"
			     :chip-id  2)) ;; (%gethead-or-error fits-file "EXTNAME")))
			   ((and (eql naxis  2)
				 (= nhdu 1)
				 (and naxis (<= naxis1 544))) ;; could be trimmed
			    (make-instance 'gmos-n-e2v-old-frac-chip
					   :chip-id
					   (%gethead-or-error fits-file "FRAMEID")))))
		    ;;
		    ((equalp chip-type "e2v") ;; post-2011 e2v
		     (cond ((and (equalp naxis 0)
				 (= nhdu 7)
				 (not gmoscombined))
			    (make-instance 'gmos-n-e2v-frac-chip-array))
			   ((and (equalp naxis 0)
				 (= nhdu 4)
				 gmoscombined)
			    (make-instance 'gmos-n-e2v-full-chip-array))
			   ((and (eql naxis 2)
				 (= nhdu 1)
				 gmoscombined)
			    (make-instance
			     'gmos-n-e2v-full-chip
			     ;; our IMRED routine COMBINE-GMOS-CHIPS writes
			     ;; EXTNAME=CHIPn for n=1,2,3
			     :chip-id  (%gethead-or-error fits-file "EXTNAME")))
			   ((and (eql naxis  2)
				 (= nhdu 1)
				 (and naxis (<= naxis1 544))) ;; could be trimmed
			    (make-instance 'gmos-n-e2v-frac-chip
					   :chip-id
					   (%gethead-or-error fits-file "FRAMEID")))))
		    ;;
		    ((equalp chip-type "hamamatsu")
		     (cond ((and (equalp naxis 0)
				 (= nhdu 13) ;; four amps
				 (not gmoscombined))
			    (make-instance 'gmos-n-hamamatsu-frac-chip-array))
			   ((and (equalp naxis 0)
				 (= nhdu 4)
				 gmoscombined)
			    (make-instance 'gmos-n-hamamatsu-full-chip-array))
			   ((and (eql naxis 2)
				 (= nhdu 1)
				 gmoscombined)
			    (make-instance
			     'gmos-n-hamamatsu-full-chip
			     ;; our IMRED routine COMBINE-GMOS-CHIPS writes
			     ;; EXTNAME=CHIPn for n=1,2,3
			     :chip-id  (%gethead-or-error fits-file "EXTNAME")))
			   ((and (eql naxis  2)
				 (= nhdu 1)
				 (and naxis (<= naxis1 544))) ;; could be trimmed
			    (make-instance 'gmos-n-hamamatsu-frac-chip
					   :chip-id
					   (%gethead-or-error fits-file "FRAMEID")))))
		    ;;
		    (t
		     (error "Unknown GMOS-NORTH fits file ~A" fits-file))))
	     ;; GMOS-SOUTH
	     ((equalp instr "GMOS-S") ;; checkme
	      (cond ((equalp chip-type "e2v")
		     ;;
		     (cond ((and (equalp naxis 0)
				 (= nhdu 13) ;; four amps
				 (not gmoscombined))
			    (make-instance 'gmos-s-eev-frac-chip-array))
			   ((and (equalp naxis 0)
				 (= nhdu 4)
				 gmoscombined)
			    (make-instance 'gmos-s-eev-full-chip-array))
			   ((and (eql naxis 2)
				 (= nhdu 1)
				 gmoscombined)
			    (make-instance
			     'gmos-s-eev-full-chip
			     ;; our IMRED routine COMBINE-GMOS-CHIPS writes
			     ;; EXTNAME=CHIPn for n=1,2,3
			     :chip-id  (%gethead-or-error fits-file "EXTNAME")))
			   ((and (eql naxis  2)
				 (= nhdu 1)
				 (and naxis (<= naxis1 544))) ;; could be trimmed
			    (make-instance 'gmos-s-eev-frac-chip
					   :chip-id
					   (%gethead-or-error fits-file "FRAMEID")))))
		    ;;
		    ((equalp chip-type "hamamatsu")
		     (cond ((and (equalp naxis 0)
				 (= nhdu 13) 
				 (not gmoscombined))
			    (make-instance 'gmos-s-hamamatsu-frac-chip-array))
			   ((and (equalp naxis 0)
				 (= nhdu 4)
				 gmoscombined)
			    (make-instance 'gmos-s-hamamatsu-full-chip-array))
			   ((and (eql naxis 2)
				 (= nhdu 1)
				 gmoscombined)
			    (make-instance
			     'gmos-s-hamamatsu-full-chip
			     ;; our IMRED routine COMBINE-GMOS-CHIPS writes
			     ;; EXTNAME=CHIPn for n=1,2,3
			     :chip-id  (%gethead-or-error fits-file "EXTNAME")))
			   ((and (eql naxis  2)
				 (= nhdu 1)
				 (and naxis (<= naxis1 544))) ;; could be trimmed
			    (make-instance 'gmos-s-hamamatsu-frac-chip
					   :chip-id
					   (%gethead-or-error fits-file "FRAMEID")))))
		    ;;
		    (t
		     (error "Unknown GMOS-SOUTH fits file ~A" fits-file)))))))))
     
  
(%add-instrument-id-function '%gmos-identify-instrument)


(defmethod get-critical-headers-for-instrument ((inst %gmos)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("FILTER1" "FILTER2" "FRAMEID" "CCDSUM"
      ;; DRAGONS
      "GEM-TLM" "SDZHDRSG" "MOSAIC"
      ;; IMRED
      "SOFTNAME" "IMRED.GMOS-MOSAIC" "IMRED.GMOSCOMBINED")
    (call-next-method))
   :test 'equalp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gmos-dragons is reduced by definition
(defmethod is-reduced-for-instrument ((inst %gmos-dragons-mixin) fits-file)
  (declare (ignore inst fits-file))
  :gmos-dragons)
(defmethod is-reduced-for-instrument ((inst %gmos-swarped-file) fits-file)
  (declare (ignore inst fits-file))
  :gmos-swarped)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-image-extension-for-onechip-instrument ((inst %gmos-dragons-mosaic-file) fits)
  (cond
    ;; if it's stripped, then look how may extensions there are
    ((typep inst '%gmos-stripped-mixin)
     (cf:maybe-with-open-fits-file (fits ff)
       (if (= (cf:fits-file-num-hdus ff) 1)
	   1
	   2)))
	(t ;; otherwise, image data lives in 2nd extension
	 2)))


    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-standard-filter-for-instrument ((inst %gmos) fits-file)
  ;; this info is in the FIRST header
  (let ((filter1 (%gethead-or-error fits-file "FILTER1" :extension 1))
	(filter2 (%gethead-or-error fits-file "FILTER2" :extension 1)))
    (when (or (not filter1) (not filter2)
	      (not (stringp filter1))
	      (not (stringp filter2))
	      (< (length filter1) 1)
	      (< (length filter2) 1))
      (error "FILTER1 and FILTER2 keyword not found in  GMOS image ~A"
	     fits-file))
    ;; see https://www.gemini.edu/pio/?q=node/10419
    ;; there seems to be a z installed in filter2, and a UKIRT z
    ;; installed in filter2
    (cond ((search "open" filter1)
	   (cond ((char= (aref filter2 0) #\u) :usdss)
		 ((char= (aref filter2 0) #\g) :gsdss)
		 ((char= (aref filter2 0) #\r) :rsdss)
		 ((char= (aref filter2 0) #\i) :isdss)
		 ((char= (aref filter2 0) #\z) :zsdss)
		 ((equalp filter2 "open2-8") :open)
		 ;; fixme - there are more filters
		 (t NIL)))
	  ((search "open" filter2)
	   (cond ((char= (aref filter1 0) #\Z) :zsdss) ;; different from above?
		 ;; Y is between 0.95 and 1.14 microns - not sure
		 ;; how standard it is
		 ((char= (aref filter1 0) #\Y) :Y)
		 (t NIL)))
	  (t
	   NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %gmosv) fits-file) ... )

(defmethod get-object-for-instrument ((inst %gmos) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %gmos) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "OBSTYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((and (equalp obj-type "OBJECT")
		(equalp (%gethead-or-error fits-file "OBJECT")
			"Twilight"))
	   :flat)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %gmos-onechip) fits-file)
  (%gethead-or-error fits-file "MJD-OBS"))



;; the primary header doesn't have the mjd, so we need to go to an image
(defmethod get-mjd-start-for-instrument ((inst %gmos-multichip) fits-file)
  (%gethead-or-error fits-file "MJD-OBS" :extension 2))

(defmethod get-mjd-start-for-instrument ((inst %gmos-swarped-file) fits-file)
  (%gethead-or-error fits-file "MJD-OBS"))

;; DRAGONS puts data into extension 2
(defmethod get-mjd-start-for-instrument ((inst %gmos-dragons-mosaic-file) fits-file)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (or (%gethead-or-error ff "MJD-OBS" :extension 1)
	(and (> (cf:fits-file-num-hdus ff) 1)
	     (%gethead-or-error ff "MJD-OBS" :extension 2)))))

;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst XXX) fits-file) ...  )
;(defmethod get-gain-for-instrument ((inst %gmos) fits-file)   )

;; has overscans, so we use the DATASEC
(defmethod get-datasec-for-instrument ((inst %gmos-frac-chip) fits-file
				       &key extension)
  (declare (ignore extension))
  (cf:parse-image-section-string
   (cf:read-fits-header fits-file "DATASEC")))


#+nil ;; generic one is fine
(defmethod get-datasec-for-instrument ((inst  %gmos-frac-chip-array) fits-file
				       &key extension) ....)


;;;;;;;;;;;;;;;;;

;; get the statistics section account for vignetting - FRAMEID is relevent header
(defun %gmos-e2v-get-statsec-for-fracchip-frameid (frameid binning)
  (declare (type string frameid))
  (let ((almost-whole-frame #(65 500 100 2204))
	(scale (if (= binning 1) 2 1))) ;; multiply by 2 for 1x1 binning
    ;; note that FRAMEID is out of order and crazy
    (map 'vector
	 (lambda (x) (* x scale))
	 (cond ((equalp frameid "1") almost-whole-frame)
	       ((equalp frameid "0") #(400 510 300 2000))  
	       ((equalp frameid "3") almost-whole-frame)
	       ((equalp frameid "2") almost-whole-frame)   
	       ((equalp frameid "4") almost-whole-frame)   
	       ((equalp frameid "5") #(40 130 300 2000))))))

;; for hamamatsu, the 1/4 amp images are either almost completely vignetted
;; (four at side) or almost completely full.  In a reduction, we will PRETEND
;; that the four side chips have valid data because they don't matter much.
(defun %gmos-hamamatsu-get-statsec-for-fracchip-frameid (frameid binning)
  (declare (type string frameid)
	   (ignorable frameid))
  (let ((almost-whole-frame #(65 500 100 2204))
	(scale (if (= binning 1) 2 1))) ;; multiply by 2 for 1x1 binning
    (declare (ignorable almost-whole-frame scale))
    ;; note that FRAMEID is out of order and crazy
    (map 'vector
	 (lambda (x) (* x scale))
	 #(70 190 300 1800))))


(defmethod get-statsec-for-instrument  ((inst %gmos-frac-chip) fits-file
					&key extension)
  (declare (ignore extension))
  (let* ((frameid (%gethead-or-error fits-file "FRAMEID"))
	 (ccdsum  (%gethead-or-error fits-file "CCDSUM"))
	 (binning (cond ((equalp ccdsum "2 2") 2)
			((equalp ccdsum "1 1") 1)
			(t
			 (error "get-statsec-for-instrument - unknown binning?")))))
    (cond ((typep inst '%gmos-e2v)
	   (%gmos-e2v-get-statsec-for-fracchip-frameid frameid binning))
	  ((typep inst '%gmos-hamamatsu)
	   (%gmos-hamamatsu-get-statsec-for-fracchip-frameid frameid binning)))))
    

(defmethod get-statsec-for-instrument  ((inst %gmos-frac-chip-array) fits-file
					&key extension)
  (err-if-not-image-at-extension inst fits-file "get-statsec" extension)
  (let* ((frameid (%gethead-or-error fits-file "FRAMEID"  :extension extension))
	 (ccdsum  (%gethead-or-error fits-file "CCDSUM"   :extension extension))
	 (binning (cond ((equalp ccdsum "2 2") 2)
			((equalp ccdsum "1 1") 1)
			(t
			 (error "get-statsec-for-instrument - unknown binning?")))))
    (cond ((typep inst '%gmos-e2v)
	   (%gmos-e2v-get-statsec-for-fracchip-frameid frameid binning))
	  ((typep inst '%gmos-hamamatsu)
	   (%gmos-hamamatsu-get-statsec-for-fracchip-frameid frameid binning)))))


;;;;;;;;;;;;;;

;; get the statistics section account for vignetting - EXTNAME is relevant header
(defun %gmos-e2v-get-statsec-for-fullchip-extname (extname binning)
  (declare (type string extname))
  (map 'vector (lambda (x) (* x (if (= binning 2) 1 2)))
       (cond ((equalp extname "CHIP1") #(500 1000 300 2000))
	     ((equalp extname "CHIP2") #(10 1000 10 2200))
	     ((equalp extname "CHIP3") #(10 500 300 2000)))))


(defun %gmos-hamamatsu-get-statsec-for-fullchip-extname (extname binning)
  (declare (type string extname))
  (map 'vector (lambda (x) (* x (if (= binning 2) 1 2)))
       (cond ((equalp extname "CHIP1") #(500 1000 300 2000))
	     ((equalp extname "CHIP2") #(10 1000 10 2200))
	     ((equalp extname "CHIP3") #(10 500 300 2000)))))

	


(defmethod get-statsec-for-instrument  ((inst %gmos-full-chip) fits-file
					&key extension)
  (declare (ignore extension))
  (let* ((extname (%gethead-or-error fits-file "EXTNAME"))
	 (naxis1   (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2   (%gethead-or-error fits-file "NAXIS2"))
	 (binning  (cond ((and (= naxis1 1024) (= naxis2 2304))
			  2)
			 ((and (= naxis1 (* 2 1024)) (= naxis2 (* 2 2304)))
			  1)
			 (t
			  (error "get-statsec-for-instrument - unknown binning")))))
    (cond ((typep inst '%gmos-e2v)
	   (%gmos-e2v-get-statsec-for-fullchip-extname extname binning))
	  ((typep inst '%gmos-hamamatsu)
	   (%gmos-hamamatsu-get-statsec-for-fullchip-extname extname binning)))))
	   

(defmethod get-statsec-for-instrument  ((inst %gmos-full-chip-array) fits-file
					&key extension)
  (err-if-not-image-at-extension inst fits-file "get-statsec" extension)
  (let* ((extname  (%gethead-or-error fits-file "EXTNAME" :extension extension))
	 (naxis1   (%gethead-or-error fits-file "NAXIS1"  :extension extension))
	 (naxis2   (%gethead-or-error fits-file "NAXIS2"  :extension extension))
	 (binning  (cond ((and (= naxis1 1024) (= naxis2 2304))
			  2)
			 ((and (= naxis1 (* 2 1024)) (= naxis2 (* 2 2304)))
			  1)
			 (t
			  (error "get-statsec-for-instrument - unknown binning")))))
        (cond ((typep inst '%gmos-e2v)
	   (%gmos-e2v-get-statsec-for-fullchip-extname extname binning))
	  ((typep inst '%gmos-hamamatsu)
	   (%gmos-hamamatsu-get-statsec-for-fullchip-extname extname binning)))))

    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


(defmethod get-chip-id-for-instrument ((inst %gmos-frac-chip) fits-file
				       &key (extension nil))
  (declare (ignore extension))
  (%gethead-or-error fits-file "FRAMEID"))  ;; a numerical string

(defmethod get-chip-id-for-instrument ((inst %gmos-frac-chip-array) fits-file
				       &key (extension nil))
  (err-if-not-image-at-extension inst fits-file "get-chip-id" extension)
  (%gethead-or-error fits-file "FRAMEID" :extension extension)) ;; a numerical string


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining bad pixels for mosaics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-badpix-function-for-instrument ((inst %gmos-dragons-mosaic-file) fits-file &key extension)
  (declare (ignorable extension)) ;; onechip
  (if (typep inst '%gmos-stripped-mixin)
      nil ;; data is lost
      ;; use use extension 4
      (cf:with-open-fits-file (fits-file ff)
	(cf:move-to-extension ff 4)
	(when (not (equalp (cf:read-fits-header ff "EXTNAME") "DQ"))
	  (error "Extension 4 of GMOS DRAGONS mosaic is not EXTNAME=DQ"))
	(let* ((imsec (cf:read-image-section ff :type :unsigned-byte-8))
	       (dqbytes (cf:image-section-data imsec))
	       (bad-bitmap (make-array (array-dimensions dqbytes) :element-type 'bit)))
	  (declare (type (simple-array (unsigned-byte 8) (* *)) dqbytes)
		   (type (simple-array bit (* *)) bad-bitmap))
	  (loop for i of-type fixnum below (array-total-size dqbytes)
		do (setf (row-major-aref bad-bitmap i)
			 (if (zerop (row-major-aref dqbytes i)) 0 1)))
	  ;;
	  (lambda (iy ix)
	    (declare (type (unsigned-byte 20) iy ix)
		     (optimize speed))
	    ;; NOTE - using 1-based indexing
	    (aref bad-bitmap (1- iy) (1- ix))))))) ;; 1 if bad, 0 if good 
  

