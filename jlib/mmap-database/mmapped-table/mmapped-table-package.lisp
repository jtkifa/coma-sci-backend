(in-package :cl-user)

(defpackage :mmapped-table
  (:use :cl :cffi :endian-buffer)
  (:export #:define-binary-table
	   #:define-binary-table-type
           #:create-binary-table
           #:create-empty-table
           #:open-binary-table
           #:close-binary-table
	   #:verify-binary-table-integrity
           #:with-table-locked
           ;; Generic row access methods (primary API)
           #:load-row
           #:save-row
           #:append-row
           #:nullify-row
           ;; Field-level access
           #:table-peek
           #:table-poke
           #:get-header-row-count
           #:get-header-max-rows
           #:resize-mapped-table
           #:truncate-mapped-table))
