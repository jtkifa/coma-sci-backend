

(defpackage astorb
  (:use #:cl)
  (:export
   #:*astorb-info-output-stream* ;; set to NIL for quiet
   #:*the-astorb*
   #:*read-astorb-on-load*  ;; default true
   #:*download-astorb-automatically* ;; default t (if not present)
   #:*astorb-quiet* 
   
   #:get-the-astorb
   ;;
   #:astorb #:astorb-p #:astorb-n #:astorb-epoch-of-elements
   #:astorb-astnum #:astorb-name #:astorb-hmag #:astorb-g #:astorb-iras-km
   #:astorb-class 
   #:astorb-code1 #:astorb-code2 #:astorb-code3
   #:astorb-code4 #:astorb-code5 #:astorb-code6
   #:astorb-orbarc #:astorb-nobs #:astorb-epoch-osc #:astorb-mean-anomaly
   #:astorb-arg-peri #:astorb-orbinc #:astorb-ecc #:astorb-a #:astorb-orbit-date
   ;;
   #:get-comet-elem-for-nth-asteroid 
   #:search-for-asteroids-by-name 
   #:find-numbered-asteroid
   ;;
   ;; astorb-retrieve.lisp
   #:retrieve-newest-astorb-file
   ;;
   ;; proximity.lisp - find nearest asteroids on sky.  expensive startup
   #:prox #:make-prox #:prox-p
   #:find-nearest-asteroids-in-prox
   ;;
   ;; astorb-data.lisp
   #:update-to-latest-astorb
   ))


;; define important variables
(in-package astorb)


(defvar *the-astorb* nil) ;; the global astorb structure


(defparameter *read-astorb-on-load*
  (not (pconfig:get-config "astorb:dont-read-data-on-load"))
  "If true, read (and possibly compile) astorb database automatically when loading package.")

(defparameter *download-astorb-automatically*
  (not (pconfig:get-config "astorb:dont-auto-download-astorb"))
  "If true, download astorb database from Lowell if not present in astorb datadir.")

(defparameter *astorb-quiet*
  (pconfig:get-config "astorb:quiet")
  "Load and compile astorb database without verbose output")

(defparameter *astorb-data-dir*
  (namestring (jk-datadir:get-datadir-for-system "astorb")))


(defparameter  *astorb-info-output-stream*
  (if *astorb-quiet*
      NIL
      *standard-output*))




