
(defparameter *quicklisp-package*
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

(dolist (package *quicklisp-packages*)
  (ql:quickload package))

