


;; quiet astorb loading message in case we need astorb
(defparameter cl-user::astorb-quiet t) 
 
(asdf:defsystem coma-json-server
  :depends-on (alexandria imutils instrument-id imred slalib-ephem
		     small-body-name observatories fits-stamp jk-parse-float
		     mpc jpl-horizons units phot-calib terapix orbphot
		     small-body-identify isochrones-isodynes
		     magnitude-of-sun
		     simple-comet-activity-detector
		     id-ss-object-at-pos
		     shift-and-add a-f-rho asteroids
		     jtk-yason ;; our slightly modded yason
		     orbital-mech
		     hunchentoot ;; web server
		     drakma      ;; web client for testing
		     cl-ppcre    ;; regexp
		     jk-datadir  ;; find package data directory
		     )
  :components
  ((:module "coma-json-server"
    :components
	    ((:file "coma-json-package" :depends-on ())
	     (:file "constants" :depends-on ("coma-json-package"))
	     (:file "structs" :depends-on ("coma-json-package" "constants"))
	     (:file "utils" :depends-on ("coma-json-package" "structs"))
	     (:file "dispatcher" :depends-on ("coma-json-package" "structs"))
	     (:file "status" :depends-on ("dispatcher" "coma-json-package" "structs"))
	     (:file "web-service" :depends-on ("coma-json-package" "dispatcher" "structs"))
	     ;; individual functions
	     (:file "hello" :depends-on ("coma-json-package" "structs" "dispatcher"))
	     (:file "name-fix" :depends-on ("coma-json-package" "structs" "dispatcher"))
	     (:file "describe-fits" :depends-on ("coma-json-package" "utils" "name-fix"
								     "dispatcher"))
	     (:file "which-extension" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "extract-extension" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "fits-headers" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "orbits" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     ;;
	     (:file "ephem" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "ephem-jpl-ephem" :depends-on ("coma-json-package" "utils" "ephem" "dispatcher"))
	     (:file "ephem-orbit" :depends-on ("coma-json-package" "utils" "ephem" "dispatcher"))
	     ;;
	     (:file "get-orbit-xyz" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     ;;
	     (:file "calib-wcs" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "calib-phot" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "calib-qualities" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "describe-fits-calib" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "legacy-zeropoint" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "calibrate"
	      :depends-on ("coma-json-package"
			   "utils"
			   "calib-wcs" "calib-phot" "calib-qualities" "dispatcher"))
	     (:file "make-stamp" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "id-object-at-location" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "find-known-objects" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "isochrones-isodynes" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "orbital-precision" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "photometry"
	      :depends-on ("coma-json-package" "utils" "orbital-precision" "dispatcher"))
	     (:file "afrho"
	      :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "shift-and-add" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "object-position-in-image"
	      :depends-on ("coma-json-package" "utils" "orbital-precision" "dispatcher"))
	     (:file "filter-image" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "reduce-images" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "split-images-into-compatible-sets"
	      :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "color-convert-utils" :depends-on ("coma-json-package" "utils" "dispatcher"))
	     (:file "color-convert-transforms" :depends-on ("coma-json-package"
							    "utils"
							    "color-convert-utils"
							    "dispatcher"))
	     (:file "color-convert" :depends-on ("color-convert-utils" "coma-json-package" "utils"
								       "dispatcher"))
	     ))))
