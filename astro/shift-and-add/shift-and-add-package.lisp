
(defpackage shift-and-add
  (:use #:cl #:slalib-ephem)
  (:export
   #:saaplan #:saaplan-p
   #:stack-result    #:stack-result-p #:stack-result-output-stack
   #:stack-result-error #:stack-result-accepted-fits-list
   #:stack-result-rejected-fits-list    #:stack-result-reject-reasons-list
   #:shift-and-add-fits-list
   #:read-orbit-info-file
   #:*default-shift-and-add-hash*

   ;; object-locators.lisp
   #:orbit-locator 
   #:build-orbit-locator
   #:rate-locator #:build-rate-locator

   ;; prepocessor.lisp defines the parent image-preproc class
   ;;   static-sky-subtract.lisp  defines a subclass
   #:static-sky-subtract-preproc

   ;; weighter.lisp defines the parent image-weighter class
   ;;  badpix-weighter.lisp
   #:badpix-masker
   ;;  simple-masker.lisp
   #:simple-masker
   ;;  stacked-masker.lisp
   #:stacked-masker

   ;; simulated-object.lisp
   #:simulated-object-preproc
   
   ))
  
