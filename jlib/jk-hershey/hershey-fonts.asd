;;;; hershey-fonts.asd — Runtime Hershey font loading and drawing

(asdf:defsystem #:hershey-fonts
  :description "Load and draw Hershey vector fonts from binary format"
  :author "JK"
  :license "MIT"
  :version "3.0"
  :components ((:file "hershey-fonts")))

(asdf:defsystem #:hershey-fonts/convert
  :description "Convert FontForge SFD files to Hershey binary format"
  :author "JK"
  :license "MIT"
  :version "3.0"
  :depends-on (#:hershey-fonts)
  :components ((:file "hershey-convert")))

(asdf:defsystem #:hershey-fonts/otf
  :description "Load and draw AVHershey OpenType fonts (data/OTF/*.otf) via FreeType"
  :author "JK"
  :license "MIT"
  :version "3.0"
  :depends-on (#:hershey-fonts #:cl-freetype2)
  :components ((:file "hershey-otf")))

(asdf:defsystem #:hershey-fonts/ttf
  :description "Write Hershey stroke fonts as TrueType (stroked or single-line)"
  :author "JK"
  :license "MIT"
  :version "3.0"
  :depends-on (#:hershey-fonts)
  :components ((:file "hershey-ttf")))
