
(defparameter *quicklisp-packages*
  '(:alexandria
    :hunchentoot
    :drakma
    :cl-ppcre
    :cffi
    :bordeaux-threads
    :cl-fad
    :cxml
    :xmls
    :fare-csv
    :md5
    :salza2
    :trivial-gray-streams
    :flexi-streams
    :lparallel))

(load "quicklisp.lisp")

(quicklisp-quickstart:install
 :path (concatenate 'string *lisp-lib* "quicklisp"))

(dolist (package *quicklisp-packages*)
  (ql:quickload package))

