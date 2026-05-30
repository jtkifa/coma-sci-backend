
(asdf:defsystem emmi-susi-proc
  :depends-on (cfitsio instrument-id file-io) ;;delete file-io after debug done
  :components ((:file "emmi-susi-proc-package" :depends-on ())
	       (:file "emmi-susi-proc-presplit"
		:depends-on ("emmi-susi-proc-package"))
	       (:file "emmi-susi-proc-merge-exts"
		:depends-on ("emmi-susi-proc-package"))))



(asdf:defsystem emmi-susi-proc/devel
  :depends-on (emmi-susi-proc)
  :components
  ((:file "emmi-susi-proc-debug")))

