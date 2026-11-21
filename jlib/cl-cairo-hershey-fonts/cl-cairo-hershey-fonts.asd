;;;; cl-cairo-hershey-fonts.asd — Hershey fonts as cl-cairo2 user fonts

(asdf:defsystem #:cl-cairo-hershey-fonts
  :description "Draw hershey-fonts text with cl-cairo2 via thread-safe Cairo user fonts"
  :author "JK"
  :license "MIT"
  :version "1.0"
  :depends-on (#:cl-cairo2 #:hershey-fonts #:cffi #:bordeaux-threads)
  :components ((:file "cl-cairo-hershey-fonts")))
