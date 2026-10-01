(defpackage astorb
  (:use #:cl)
  (:export
   ;; Config variables
   #:*astorb-info-output-stream*
   #:*the-astorb*
   #:*read-astorb-on-load*
   #:*download-astorb-automatically*
   #:*astorb-quiet*

   ;; Main accessor
   #:get-the-astorb

   ;; Wrapper struct and accessors
   #:astorb #:astorb-p #:make-astorb
   #:astorb-table #:astorb-file-path #:astorb-epoch-of-elements

   ;; Row count
   #:astorb-n

   ;; Field accessors (using table-peek)
   #:astorb-astnum #:astorb-name #:astorb-sname
   #:astorb-hmag #:astorb-g #:astorb-iras-km
   #:astorb-iras-class
   #:astorb-code1 #:astorb-code2 #:astorb-code3
   #:astorb-code4 #:astorb-code5 #:astorb-code6
   #:astorb-orbarc #:astorb-nobs #:astorb-epoch-osc
   #:astorb-mean-anomaly #:astorb-arg-peri #:astorb-anode
   #:astorb-orbinc #:astorb-ecc #:astorb-a #:astorb-orbit-date

   ;; Query functions
   #:get-comet-elem-for-nth-asteroid
   #:get-universal-elem-for-nth-asteroid
   #:search-for-asteroids-by-name
   #:find-numbered-asteroid

   ;; Data management
   #:retrieve-newest-astorb-file
   #:update-to-latest-astorb
   #:convert-astorb-text-to-mmap

   ;; Proximity search
   #:prox #:make-prox #:prox-p
   #:prox-mjd #:prox-observatory
   #:find-nearest-asteroids-in-prox))


(in-package astorb)


(defvar *the-astorb* nil
  "The global astorb structure (wrapper around memory-mapped table).")

(defparameter *read-astorb-on-load*
  (not (pconfig:get-config "astorb:dont-read-data-on-load"))
  "If true, read (and possibly convert) astorb database automatically when loading package.")

(defparameter *download-astorb-automatically*
  (not (pconfig:get-config "astorb:dont-auto-download-astorb"))
  "If true, download astorb database from Lowell if not present in astorb datadir.")

(defparameter *astorb-quiet*
  (pconfig:get-config "astorb:quiet")
  "Load astorb database without verbose output.")

(defparameter *astorb-data-dir*
  (namestring (jk-datadir:get-datadir-for-system "astorb")))

(defparameter *astorb-info-output-stream*
  (if *astorb-quiet*
      NIL
      *standard-output*))

(defvar *astorb-lock* (bordeaux-threads:make-recursive-lock "astorb-lock"))

(defmacro with-astorb-lock (&body body)
  `(bordeaux-threads:with-recursive-lock-held (*astorb-lock*)
     ,@body))
