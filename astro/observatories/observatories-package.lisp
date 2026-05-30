(defpackage observatories
  (:use #:cl)
  (:export 
   #:make-observatory
   #:observatory #:observatory-p  #:observatory-id
   #:observatory-name  #:observatory-obscode
   #:observatory-wlongitude  #:observatory-latitude  #:observatory-altitude
   #:observatory-timezone
   #:*obs-list* 
   #:get-observatory))
 


