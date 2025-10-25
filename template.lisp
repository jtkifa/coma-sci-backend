
;; a template lisp script - this should not be run, but is
;; the basis for other scripts

(require 'asdf) ;; built into SBCL
(load "/root/quicklisp/setup.lisp")

;; set up asdf to search code tree from top
(asdf:initialize-source-registry
 '(:source-registry (:tree "/root/coma-backend-jtk/")
   :inherit-configuration))



