(asdf:defsystem astorb
    ;;  
  :depends-on (numio file-io astro-time slalib-ephem readable-arrays gzip-stream
		     string-utils drakma jk-datadir)
    ;;
    :components
  ((:file "astorb-package" :depends-on ())
   (:file "astorb" :depends-on ("astorb-retrieve"))
   (:file "astorb-retrieve" :depends-on ("astorb-package"))
   (:file "proximity" :depends-on (astorb))))


