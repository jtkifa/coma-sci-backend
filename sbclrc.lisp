
;; sbclrc lisp configuration loaded before any Lisp package

(require 'asdf) ;; built into SBCL

(defparameter *lisp-lib*
  (or (uiop:getenv "LISP_LIB")
      (error "Environment variable $LISP_LIB not defined")))
(defparameter *asdf-cache-dir*
  (or (uiop:getenv "LISP_CACHE_DIR")
      (error "Environment variable $LISP_CACHE_DIR not defined")))

;; clean up the dirs to have one '/' at end
(setf *lisp-lib* (concatenate 'string (string-right-trim "/" *lisp-lib*) "/"))
(setf *asdf-cache-dir* (concatenate 'string (string-right-trim "/" *asdf-cache-dir*) "/"))

(setf asdf:*user-cache* *asdf-cache-dir*)


;; set up asdf to search for .asd files in $LISP_DIR
(asdf:initialize-source-registry
 `(:source-registry (:tree ,*lisp-lib*)
   :inherit-configuration))

;; turn off infix banner at startup
(setf (get :infix :dont-print-copyright) t) 

(asdf:load-system "pconfig") ;; package config tool

;; disable auto-loading of astorb because Docker volume not available
;; at compile time
(when (equalp (uiop:getenv "DO_NOT_GET_ASTORB") "TRUE")
  (pconfig:set-config "astorb:dont-read-data-on-load" t)
  (pconfig:set-config "astorb:dont-auto-download-astorb" t))


;; initialize parallel kernel
(asdf:load-system "lparallel")
(setf lparallel:*kernel* (lparallel:make-kernel 4))


