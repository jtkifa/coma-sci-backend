
;; DELETE ME after development - define list of data files

(in-package emmi-susi-proc)


(defparameter *datadir* 
  (namestring
   (truename "~/scratch/Hale-Bopp-ESO/")))

(defparameter *rawdir* (concatenate 'string *datadir* "raw/"))


(defparameter *raw-dir-list*
  (mapcar 'namestring
	  (append (directory (format nil "~A*EMMI" *rawdir*))
		  (directory (format nil "~A*SUSI2" *rawdir*)))))


(defparameter *inst-list* nil)
(defparameter *samplefile-list* nil)

(when (not *inst-list*)
  (loop for dir in *raw-dir-list*
	for fits-files = (directory (format nil "~A/*.fits" dir))
	do
	   (loop with good = nil
		 for fits in fits-files
		 for inst = (instrument-id:identify-instrument fits)
		 until good
		 when (and (> (instrument-id:get-exptime-for-fits fits) 60)
			   (eq (instrument-id:get-object-type-for-fits fits) :object))
		   do
		      (setf good t)
		      (push fits *samplefile-list*)
		      (push inst *inst-list*)
		 finally
		    (when (not good)
		      (error "Failed for directory ~A" dir)))
	   (setf *inst-list* (nreverse *inst-list*))
	   (setf *inst-list* (nreverse *inst-list*))
			      
	  

;; files with samples of data
(defparameter *samplefiles-rel*
  (file-io:read-file-as-line-list
   (concatenate 'string *datadir* "sample-files.txt")))

(defparameter *samplefiles*
  (mapcar (lambda (file)
	    (concatenate 'string *datadir* file))
	  *samplefiles-rel*))

(defparameter *samplefile-insts*
  (mapcar 'instrument-id:identify-instrument
	  *samplefiles*))


(defun get-sample-file-for-type (inst-type)
  (loop for file in *samplefiles*
	for inst in *samplefile-insts*
	when (eq (type-of inst) inst-type)
	  do (return file)))
	
(defparameter *sample-susi*
  (concatenate 'string *rawdir* "199x-SUSI-examples/ONTT.1994-01-05T06:05:30.000.fits"))

(defparameter *sample-susi2-1ext*
  (get-sample-file-for-type
   'instrument-id:eso-ntt-susi2/raw1ext))

(defparameter *sample-susi2-2ext*
  (get-sample-file-for-type
   'instrument-id:eso-ntt-susi2/raw2ext))

(defparameter *sample-eso-ntt-emmi-rild-tek2048*
  (get-sample-file-for-type
   'instrument-id:eso-ntt-emmi-rild-tek2048/raw))


(defparameter *sample-eso-ntt-emmi-bimg/tek1024*
  (get-sample-file-for-type
   'instrument-id:eso-ntt-emmi-bimg-tek1024/raw))

(defparameter *sample-eso-ntt-emmi-rild-fa2048-4ext*
  (get-sample-file-for-type
   'instrument-id:eso-ntt-emmi-rild-fa2048/raw4ext))

(defparameter *sample-eso-ntt-emmi-rild-fa2048-2ext*
  (get-sample-file-for-type
   'instrument-id:eso-ntt-emmi-rild-fa2048/raw2ext))






				    
