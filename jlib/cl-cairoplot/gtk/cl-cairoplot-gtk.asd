;;;; cl-cairoplot-gtk.asd — plotting into a window on screen
;;;;
;;;;   (asdf:load-system "cl-cairoplot-gtk")
;;;;   (defvar *d* (cl-cairoplot:open-device :gtk "my plot" :size 800 :aspect 0.75))
;;;;
;;;; Kept apart from cl-cairoplot, so GTK (through cl-ez-gtk-server and
;;;; GTK-server) is loaded only by those who load this system.

(asdf:defsystem #:cl-cairoplot-gtk
  :description "A :GTK output type for cl-cairoplot: plots drawn live in a GTK4 window"
  :author "JK"
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-cairoplot #:cl-ez-gtk-server/canvas #:bordeaux-threads #:cffi)
  :serial t
  :components ((:file "cl-cairoplot-gtk")
               (:file "demo")))
