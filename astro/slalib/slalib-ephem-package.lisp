 
 
(defpackage slalib-ephem  
  (:use #:cl #:observatories #:orbital-elements)
  (:export 

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; export symbols from observatories package for past compatibility
   #:make-observatory
   #:observatory #:observatory-p  #:observatory-id
   #:observatory-name  #:observatory-obscode
   #:observatory-wlongitude  #:observatory-latitude  #:observatory-altitude
   #:observatory-timezone
   #:*obs-list* 
   #:get-observatory

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; export symbols from orbital-elements for past compatibility
   #:comet-elem #:make-comet-elem #:comet-elem-p
   #:make-comet-elem-from-jpl
   #:comet-elem-id #:comet-elem-epoch #:comet-elem-time-peri
   #:comet-elem-orbinc #:comet-elem-anode #:comet-elem-perih
   #:comet-elem-q #:comet-elem-e #:comet-elem-nongravs #:comet-elem-data
   ;;
   #:comet-elem+err #:make-comet-elem+err #:comet-elem+err-p
   #:comet-elem+err-time-peri-err
   #:comet-elem+err-orbinc-err #:comet-elem+err-anode-err #:comet-elem+err-perih-err
   #:comet-elem+err-q-err #:comet-elem+err-e-err
   ;;
   #:nongravs #:nongravs-p #:nongravs-dt
   #:nongravs-a1 #:nongravs-a2 #:nongravs-a3
   ;;
   ;; parent class for asteroid-desc and comet-desc
   #:object-desc #:object-desc-p #:object-desc-name #:object-desc-source
   ;;
   #:asteroid-desc #:asteroid-desc-p #:asteroid-desc-h #:asteroid-desc-g
   #:asteroid-period #:asteroid-albedo
   ;;
   #:comet-desc #:comet-desc-p

   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; slalib-ephem
   ;;
   #:asteroid-elem #:make-asteroid-elem #:asteroid-elem-p
   #:asteroid-elem-epoch #:asteroid-elem-orbinc #:asteroid-elem-anode
   #:asteroid-elem-perih #:asteroid-elem-a #:asteroid-elem-e #:asteroid-elem-m
   #:asteroid-elem-data
   ;;
   #:univ-elem  #:make-univ-elem #:univ-elem-p 
   #:univ-elem-id #:univ-elem-velem #:univ-elem-data
   ;;
   #:perturb-universal-elem
   #:perturb-comet-elem
   #:compute-pv-from-comet-elem
   #:compute-pv-from-comet-elem-for-observatory
   #:compute-pv-from-universal-elem
   #:compute-pv-from-universal-elem-for-observatory
   ;;
   ;; the /plante versions use SLA_PLANTE, which does not support observatory
   ;; altitude (and thus geocenter)
   #:compute-radecr-from-universal-elem
   #:compute-radecr-from-universal-elem-for-observatory
   ;;
   #:compute-radecr-from-comet-elem
   #:compute-radecr-from-comet-elem/plante 
   #:compute-radecr-from-comet-elem-for-observatory
   #:compute-radecr-from-comet-elem-for-observatory/plante
   #:compute-radecr-from-comet-elem-with-rates
   #:compute-radecr-from-comet-elem-with-rates/plante
   #:compute-radecr-from-comet-elem-with-rates-for-observatory
   #:compute-radecr-from-comet-elem-with-rates-for-observatory/plante
   ;;
   #:compute-comet-elem-from-pv
   #:convert-comet-elem-to-universal-elem
   #:convert-asteroid-elem-to-universal-elem
   #:convert-universal-elem-to-comet-elem
   #:convert-universal-elem-to-asteroid-elem
   #:convert-comet-elem-to-asteroid-elem
   #:convert-asteroid-elem-to-comet-elem
   #:compute-radecr-from-pv
   #:compute-radecr-from-pv-for-observatory
   #:compute-true-anomaly-from-pv 
   #:compute-planet-radecr
   #:compute-planet-radecr-for-observatory
   #:compute-airmass-for-ra-dec-for-observatory
   #:compute-hour-angle-for-ra-for-observatory
   #:rdplan-planet-position-for-observatory
   #:compute-elongation-for-ra-dec
   #:moon-position-for-observatory
   #:moon-illuminated-fraction-for-observatory
   #:days-from-new-moon-for-observatory
   #:altaz-for-apparent-radec-for-observatory
   #:sun-state-for-observatory
   #:compute-ephem-for-observatory
   ;;
   #:ephem #:ephem-p #:ephem-elem #:ephem-observatory
   #:ephem-mjd #:ephem-ra #:ephem-dec #:ephem-ra-apparent #:ephem-dec-apparent
   #:ephem-dra/dt #:ephem-ddec/dt
   #:ephem-rhelio #:ephem-delta  #:ephem-ddelta/dt #:ephem-drhelio/dt
   #:ephem-alt #:ephem-az #:ephem-airmass
   #:ephem-hour-angle #:ephem-pv #:ephem-true-anomaly
   #:ephem-phase-angle #:compute-phase-angle-for-pv
   #:ephem-pv-earth #:ephem-elongation-for-observatory
   #:ephem-ra-moon #:ephem-dec-moon #:ephem-alt-moon
   #:ephem-moon-frac #:ephem-moon-phase-angle
   #:ephem-moon-dist #:ephem-alt-sun #:ephem-az-sun
   #:ephem-sun-state
   ;;
   ;; slalib-ephem-proj
   #:orbit-dirs #:orbit-dirs-p #:make-orbit-dirs
   #:orbit-dirs-mjd #:orbit-dirs-olat #:orbit-dirs-olon
   #:orbit-dirs-altitude #:orbit-dirs-mjdtt #:orbit-dirs-ra #:orbit-dirs-dec
   #:orbit-dirs-pv #:orbit-dirs-pvscr 
   #:orbit-dirs-solar-2vec  #:orbit-dirs-orbit-2vec 
   #:orbit-dirs-plane-2vec  #:orbit-dirs-osolar-2vec
   #:orbit-dirs-solar-3vec  #:orbit-dirs-orbit-3vec 
   #:orbit-dirs-plane-3vec  #:orbit-dirs-osolar-3vec
   #:orbit-dirs-ec-3vec #:orbit-dirs-ra-3vec #:orbit-dirs-dec3vec
   #:orbit-dirs-solardot #:orbit-dirs-orbitdot 
   #:orbit-dirs-planedot #:orbit-dirs-osolardot
   #:orbit-dirs-solar-pa #:orbit-dirs-orbit-pa 
   #:orbit-dirs-plane-pa #:orbit-dirs-osolar-pa
   #:generate-orbit-dirs-for-comet-elem
   #:generate-orbit-dirs-for-comet-elem-for-observatory
   ;; a simpler one giving just some directions on the sky
   #:compute-sun-and-orbit-directions-for-comet-elem
   
   ;;
   ;;
   ;; slalib-ephem-misc
   #:could-it-be-this-observatory?
   #:which-observatory-could-it-be?

   ))
