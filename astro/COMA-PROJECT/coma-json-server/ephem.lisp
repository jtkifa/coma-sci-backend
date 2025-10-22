(in-package coma-json-server)

#|

METHODs to get orbit: JPL-EPHEM JPL-ORBIT MPC-ORBIT

The required fields are in NOTES/ephem-fields.txt - we just get all JPL Horizons
fields.

 


|#

(defparameter *sample-jpl-horizons-request*
  "
{
       \"type\":\"request\",
       \"command\":\"get-ephem\",
       \"id\":\"123abc\",
       \"parameters\": {\"METHOD\":\"JPL-EPHEM\",
                      \"OBJECT\":\"P/2020 O1\",
                      \"DT-MINUTES\":60,
                      \"OBSCODE\":\"568\",
                      \"UTC-START\":\"2020-08-20T00:00:00\",
                      \"UTC-END\":\"2020-08-20T00:20:00\"
                      }
}
")

;; all fields in COMA eph database, also in JSON eph structure
(defparameter *db-eph-fields*
  '("targetbody" "centerbody" "centersite" "ephstr" "ctdatetime"
    "jd"  "solarpres"  "lunarpres"
    "j2kra"  "j2kdec"      "j2kradeg"  "j2kdecdeg"
    "appra"  "appdec"      "appradeg"  "appdecdeg"
    "dracosd"  "ddect"  "amass"
    "tmageph"  "nmageph"  "r"  "rdot"  "delta"
    "deldot"  "sto"  "tom"  "illupercent"  "glxlon"  "glxlat"
    ;; new ones added at request of KJM
    "trueanom" "onewaylttime"
    ))



(defun %centerbody-is-earth (centerbody)
  (member centerbody '("earth" "@earth" "399" "@399") :test 'equalp))

(def-json-command get-ephem (json-req)
  (with-json-command-setup (json-req)
    (let* ((method (get-param "METHOD" :default "JPL-EPHEM"))
	   (object (get-param "OBJECT" :required t))
	   ;; only for METHOD=ORBIT - this can be object, or "jpl-orbit" or "mpc-orbit"
	   (orbit-specifier (get-param "ORBIT")) 
	   (mjd-start (get-param "MJD-START"))
	   (mjd-end (get-param "MJD-END"))
	   (utc-start (get-param "UTC-START"))
	   (utc-end (get-param "UTC-END"))
	   (mjd-start-from-utc (when utc-start
				 (ignore-errors
				  (astro-time:parse-ut-date-and-time-string-to-mjd utc-start))))
	   (mjd-end-from-utc (when utc-end
			       (ignore-errors
				(astro-time:parse-ut-date-and-time-string-to-mjd utc-end))))
	   (dt/min (get-param "DT-MINUTES" :default 60))
	   (centerbody (get-param "CENTERBODY" :default "399")) ;; Earth is default
	   (obs-code (get-param "OBSCODE")))


    
      (jcom-test-expr (not parameters)  "NO-PARAMETERS" "No parameters given.")
      (jcom-test-expr (not (or (and mjd-start mjd-end)
			       (and utc-start utc-end)))
		       "NO-TIME-RANGE"
		       "Valid MJD-START+MJD-END or UTC-START+UTC-END not given.")
      (jcom-test-expr (or (and mjd-start utc-start)
			  (and mjd-end utc-end))
		      "BOTH-MJD-AND-UTC-GIVEN"
		      "Gave both input MJD-START+MJD-END and UTC-START+UTC-END.  Only one time type allowed.")
      (jcom-test-expr (or (and mjd-start (not mjd-start-from-utc))
			  (and mjd-end (not mjd-end-from-utc)))
		      "INVALID-UTC-FORMAT"
		      "Invalid YYYY-MM-DDTHH:MM:SS format for UTC-START or UTC-END")
      (jcom-test-expr (not object)  "NO-OBJECT" "OBJECT not given.")
      (jcom-test-expr (not obs-code)  "NO-OBSCODE" "OBSCODE not given.")
      (jcom-test-expr (not (member method '("JPL-EPHEM" "ORBIT") :test 'equalp))
		      "METHOD-NOT-SUPPORTED"
		      (format nil "METHOD ~A not supported." method))
      
      
      

      
    (cond ((equalp method "JPL-EPHEM")
	   (funcall 'fill-jpl-ephem-response ;; so we can define later
	    json-resp
	    object
	    (or mjd-start mjd-start-from-utc)
	    (or mjd-end mjd-end-from-utc)
	    dt/min
	    obs-code centerbody))

	  ((equalp method "ORBIT")
	   (jcom-test-expr (not orbit-specifier)
			   "NO-ORBIT-GIVEN"
			   "METHOD=ORBIT had ORBIT=NULL")
	   
	   (let* ((elem (ignore-errors (get-orbit-using-method orbit-specifier object))))
	     (jcom-test-expr (not elem)
			     "ORBIT-NOT-PARSED-OR-RETRIEVED"
			     (format
			      nil
			      "METHOD=ORBIT, but ORBIT=~A was present but not parsable as JSON orbit object (hash table), or retrievable from JPL/MPC as string 'mpc-orbit' or 'jpl-orbit'" orbit-specifier))
	     ;;
	     (jcom-test-expr (not (%centerbody-is-earth centerbody))
			     "CENTERBODY-IS-NOT-EARTH"
			     "METHOD=ORBIT, but CENTERBODY is not the Earth (@399).")
	     (funcall 'fill-orbit-ephem-response ;; so we can define later
		      json-resp elem
		      object
		      (or mjd-start mjd-start-from-utc)
		      (or mjd-end mjd-end-from-utc)
		      dt/min
		      obs-code centerbody)))))))

