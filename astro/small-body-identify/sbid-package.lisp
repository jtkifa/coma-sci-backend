





(defpackage small-body-identify
  (:use #:cl)
  (:nicknames #:sbid)
  (:export
   #:candidate #:candidate-p #:candidate-id #:candidate-dist
   #:candidate-elem #:candidate-index #:candidate-ra #:candidate-dec
   #:candidate-delta #:candidate-dra/dt #:candidate-ddec/dt
   #:candidate-mjd 
   
   #:make-sbid-mem       ;; make a memory object to store tables
   #:generate-candidates ;; genearate list of candidates (and
			 ;; auto-fill memory table)
   #:generate-candidates-with-caching
   ))
