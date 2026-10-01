(asdf:defsystem astorb
  :description "Asteroid orbit database using memory-mapped storage"
  :depends-on (mmapped-table orbital-elements
               numio file-io astro-time slalib-ephem
               gzip-stream string-utils
               drakma jk-datadir pconfig)
  :components
  ((:file "astorb-package" :depends-on ())
   (:file "astorb" :depends-on ("astorb-package"))
   (:file "astorb-retrieve" :depends-on ("astorb-package"))
   (:file "astorb-data" :depends-on ("astorb" "astorb-retrieve"))
   (:file "astorb-query" :depends-on ("astorb" "astorb-data"))
   (:file "proximity" :depends-on ("astorb-query"))))
