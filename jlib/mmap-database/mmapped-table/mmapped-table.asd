(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:mmapped-table
  :description "A thread-safe mmap dynamic binary record table engine."
  :author "Gemini Collaborator"
  :license "MIT"
  :depends-on (#:cffi #:bordeaux-threads #:closer-mop #:endian-buffer)
  :serial t
  :components ((:file "mmapped-table-package")
               (cffi-grovel:grovel-file "groveller")
               (:file "mmapped-table")))


(asdf:defsystem #:mmapped-table/test
  :description "A thread-safe mmap dynamic binary record table engine."
  :author "Gemini Collaborator"
  :license "MIT"
  :depends-on (#:mmapped-table)
  :serial t
  :components ((:file "test-mmapped-table")))



