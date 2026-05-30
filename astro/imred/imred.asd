
(asdf:defsystem imred
  :depends-on (imutils instrument-id
		       wcs cfitsio astro-time random cl-fad file-io float-utils
		       logger stats fastmedian ra-dec suprime-cam-fix shellsort-jk
		       hypersuprime-cam-fix gmos-proc emmi-susi-proc)
  :components
  ((:file "imred-package" :depends-on ())
   (:file "utils"  :depends-on ("imred-package"))
   (:file "analyze-images"  :depends-on ("imred-package"))
   (:file "trim-image" :depends-on ("analyze-images" "imred-package" "utils"))
   (:file "copy-image" :depends-on ("analyze-images" "imred-package" "utils"))
   (:file "stack-image" :depends-on ("analyze-images" "imred-package" "utils"))
   (:file "normalize-gain" :depends-on ("analyze-images" "imred-package" ))
   (:file "zerocombine" :depends-on ("utils" "stack-image"))
   (:file "flatcombine" :depends-on ("utils" "stack-image"))
   (:file "fringecombine" :depends-on ("utils" "stack-image"))
   (:file "fringecor" :depends-on ("utils" "stack-image" "fringecombine"))
   (:file "ccdproc" 
    :depends-on ("utils" "stack-image" 
		  "zerocombine" "analyze-images"
		  "flatcombine"))
   (:file "reduction-plan" 
    :depends-on ("utils" "analyze-images"))
   (:file "process-set" 
    :depends-on ("utils" "analyze-images" "reduction-plan"
		 "zerocombine" "flatcombine" 
		 "ccdproc"))
   (:file "gen-correct-reduction-plan" :depends-on ("reduction-plans"))
   ;; specific reduction plans
   (:module "reduction-plans"
    :pathname "reduction-plans"
    :depends-on ("reduction-plan")
    :components  ((:file "reduction-plan-uh88tek")
		  (:file "reduction-plan-smarts")
		  (:file "reduction-plan-suprime-cam")
		  (:file "reduction-plan-hypersuprime-cam")
		  (:file "reduction-plan-lowell-prism")
		  (:file "reduction-plan-hct-hfosc")
		  (:file "reduction-plan-hct-hfosc2")
		  (:file "reduction-plan-gmos-n-e2v-chip2")
		  (:file "reduction-plan-gmos-array")
		  (:file "reduction-plan-fors1+2")
		  (:file "reduction-plan-wht-acam")
		  (:file "reduction-plan-cfccd-on-ctio1.5m")
		  (:file "reduction-plan-keck-lris")
		  (:file "reduction-plan-eso-ntt-emmi+susi")
		  ))))
                           
