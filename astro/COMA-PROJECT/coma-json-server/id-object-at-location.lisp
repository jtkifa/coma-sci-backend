#|

Request:

{
    "TYPE":"REQUEST",
    "COMMAND":"ID-OBJECT-AT-LOCATION",
    "ID":"123ABC",
        "PARAMETERS": {
        "OBJECT":"2P",  
        "MJD":55555.0,
        "RA": 123.0,
        "DEC": 33.0,
        "SEARCH-RADIUS-ARCMIN": 10.0,
        "FUZZY-MATCH-THRESHOLD": 0.30
        "MATCH-COMETS": true,
        "MATCH-ASTEROIDS": true,
        "MATCH-LANDOLT-STANDARDS": true,
        "BRUTE-FORCE-COMETS": true
        }
}


Response:

{
     "TYPE":"RESPONSE",
     "COMMAND":"ID-OBJECT-AT-LOCATION",
     "ID":"123ABC",
     "PARAMETERS": {
       "TYPE": "OBJECT-ID-AT-LOCATION-RESULT",
       "FOUND-MATCH": true,   
       "DIST-ARCMIN":1.234,
       "RA-MATCH": 123.11,
       "DEC-MATCH": 33.33,
       "OBJECT-ID": "1P",
       "OBJECT-COMMON-NAME": "Halley",
       "OBJECT-TYPE": "COMET" // or ASTEROID or LANDOLT-STANDARD
       "FUZZY-MATCH-STRENGTH":0.765, // NULL if not a fuzzy match
     }
}


|#


(in-package coma-json-server)



(def-json-command id-object-at-location (json-req)
  (with-json-command-setup (json-req)
  (let* ((object      (get-param "OBJECT" :required t))
	 (mjd         (get-param "MJD" :required t))
	 (ra          (get-param "RA"  :required t))
	 (dec         (get-param "DEC" :required t))
	 (search-radius-arcmin (get-param "SEARCH-RADIUS-ARCMIN" :default 10.0))
	 (fuzzy-match-threshold (get-param "FUZZY-MATCH-THRESHOLD" :default 10.0))
	 ;; all of these default to T
	 (match-comets    (get-param "MATCH-COMETS" :default t)) 
	 (match-asteroids (get-param "MATCH-ASTEROIDS" :default t))
	 (brute-force-comets (when match-comets
			       (get-param "BRUTE-FORCE-COMETS" :default t)))
	 (match-landolt-standards (get-param "MATCH-LANDOLT-STANDARDS" :default t)))


   
    (jcom-test-expr (not (and (realp fuzzy-match-threshold)
			      (<= 0.1 fuzzy-match-threshold 1.0)))
		    "BAD-FUZZY-MATCH-THRESHOLD"
                    "FUZZY-MATCH-THRESHOLD must be in [0.1,1.0]")
    
    (jcom-test-expr (not (stringp object))
		    "BAD-OBJECT"
                    "OBJECT must be a string")
    
    (jcom-test-expr (not (and (realp ra) (realp dec) (<= -90 dec 90)))
		    "BAD-RA-DEC"
                    "RA and DEC must be in-range real numbers.")
    
    (jcom-test-expr (not (and (realp mjd)
			      (< 0 mjd 64693.0d0))) ;; 1976 to 2036
		    "OUT-OF-RANGE-MJD"
                    "MJD out of range 1858-11-17 - 2036-01-01")
    
    (multiple-value-bind (id-ret err)
	(ignore-errors 
	 (id-ss-object-at-pos:find-matching-object 
	  object ra dec mjd
	  :verbose nil
	  :fuzzy-match-threshold fuzzy-match-threshold
	  :dist-tol-arcmin search-radius-arcmin
	  :match-comets match-comets
	  :match-asteroids match-asteroids
	  :brute-force-comets brute-force-comets
	  :match-landolt-standards match-landolt-standards))
      (if err
	  (setf (json-object-error json-resp)
                (make-error-object :error "INTERNAL-ERROR"
				   :desc (format nil "ERROR ~A" err)))
	  (cond ((not id-ret)
		 (set-param "FOUND-MATCH"  'yason:false))
		(t
		 (set-param "FOUND-MATCH"  'yason:true)
		 (set-param "DIST-ARCMIN" 
			    (id-ss-object-at-pos:id-ret-dist/arcmin id-ret))
		 (set-param "RA-MATCH" 
			    (id-ss-object-at-pos:id-ret-ra-obj id-ret))
		 (set-param "DEC-MATCH" 
			    (id-ss-object-at-pos:id-ret-dec-obj id-ret))
		 (set-param "OBJECT-ID" 
			    (id-ss-object-at-pos:id-ret-id id-ret))
		 (set-param "MATCH-METHOD" 
			    (id-ss-object-at-pos:id-ret-match-method id-ret))
		 (set-param "OBJECT-COMMON-NAME" 
			    (second (id-ss-object-at-pos:id-ret-match id-ret)))
		 (set-param "OBJECT-TYPE" 
			    (string-upcase (third (id-ss-object-at-pos:id-ret-match id-ret))))
		 (set-param "FUZZY-MATCH-STRENGTH" 
			    (id-ss-object-at-pos:id-ret-fuzzy-strength id-ret)))))))))	       
   
