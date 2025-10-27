
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

;; load quicklisp
(load "/root/quicklisp/setup.lisp")

(asdf:load-system "trivial-gray-streams") ;; may not be present in sbcl?


