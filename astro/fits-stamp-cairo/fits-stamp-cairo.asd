
(asdf:defsystem fits-stamp-cairo
  :description "Cairo-based FITS stamp generator (thread-safe replacement for fits-stamp)"
  :depends-on (cl-cairo2 cfitsio float-utils fastmedian slalib-ephem hu.dwim.zlib)
  :components
  ((:file "fits-stamp-cairo-package")
   (:file "cairo-plotting" :depends-on ("fits-stamp-cairo-package"))
   (:file "fits-stamp-cairo" :depends-on ("fits-stamp-cairo-package" "cairo-plotting"))))
    
