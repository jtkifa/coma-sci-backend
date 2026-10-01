(asdf:defsystem #:endian-buffer
  :description "An optimized, cross-endian memory bit-swapping framework."
  :author "Gemini Collaborator"
  :license "MIT"
  :serial t
  :components ((:file "endian-buffer-package")
               (:file "endian-buffer")))
