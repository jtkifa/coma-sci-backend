

(in-package coma-json-server)

#|
Request:

{  
  "TYPE":"REQUEST",
  "COMMAND":"COMPUTE-AFRHO",
  "ID":"123abc",
  "PARAMETERS":
   {
     "HMAG-NUCLEUS": 14.0, // the H mag of nucleus of comet - defaults to 100.0
     "NUCLEUS-COLOR": 0.0, // term to subtract from nuclues mag (defaults to 0)
                           // computed using HMAG-NUCLEUS
                           // usually V-<FILTER> color
     "RHELIO": 3.0,        // distance of sun in AU
     "DELTA":  2.0,        // distance to earth in AU
     "PHASE-ANGLE": 3.455  // phase angle in deg
     "MAG-VEC": [20.1, 19.1, 18,1],     // vector of magnitudes
     "MEG-ERR-VEC": [0.11, 0.12, 0.13], // vector of magnitude errors
     "APERTURE-DIAMETER-VEC": [2.0, 3.0, 4.0] // diameters of apertures (not radii)
     "FILTER":    : "rsdss",  // one of our known filters, for mag of sun
     "MAG-SUN": -27.1       // apparent mag of sun if not using FILTER
    }
}

Response

{
  "COMMAND": "AFRHO",
  "ID": "123abc",
  "PARAMETERS":
   {
     "TYPE": "PARAMETERS",
     "AFRHO-VEC": [21.0, 22.0, 23.0],
     "AFRHO-ERR-VEC": [0.1, 0.2, 0.3],
     "MAG-NUCLEUS" 21.2
   }
}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

This real world case matches KJM's test outputs

{
    "type":"request",
    "command":"compute-afrho",
    "id":"123abc",
    "parameters": {
	"HMAG-NUCLEUS":99.0,
	"NUCLEUS-COLOR":0.0,
	"FILTER": "VJ",
	"RHELIO": 4.482,
	"DELTA": 3.525,
	"PHASE-ANGLE": 55.0,
	"MAG-VEC": [21.479],
	"MAG-ERR-VEC": [0.014],
	"APERTURE-DIAMETER-VEC": [4.0]
    }
}


{
    "type":"request",
    "command":"compute-afrho",
    "id":"123abc",
    "parameters": {
	"HMAG-NUCLEUS":99.0,
	"NUCLEUS-COLOR":0.0,
	"FILTER": "VJ",
	"RHELIO": 4.482,
	"DELTA": 3.525,
	"PHASE-ANGLE": 55.0,
	"MAG-VEC": [21.479],
	"MAG-ERR-VEC": [0.014],
	"APERTURE-DIAMETER-VEC": [4.0]
    }
}




|#

(def-json-command compute-afrho (json-req)
  (with-json-command-setup (json-req)
    (let* ((hmag (get-param "HMAG-NUCLEUS" :default 100.0))
	   (nucleus-color (get-param "NUCLEUS-COLOR" :default 0.0))
	   (rhelio (get-param "RHELIO" :required t))
	   (delta (get-param "DELTA" :required t))
	   (phase-angle (get-param "PHASE-ANGLE" :required t))
	   (mag-vec (get-param "MAG-VEC" :required t))
	   (mag-err-vec (get-param "MAG-ERR-VEC" :required t))
	   (ap-diam-vec (get-param "APERTURE-DIAMETER-VEC" :required t))
	   (filter (get-param "FILTER"))
	   (mag-sun (get-param "MAG-SUN"))
	   ;;
	   (%mag-sun (or mag-sun
			 ;; if MAG-SUN was not given, then FILTER must be provided to compute MAG-SUN
			 (when (not filter)
			   (return-with-error "NO-FILTER-PROVIDED"
					      "No filter was provided but MAG-SUN was not given.")
			   nil) ;; NIL provides (OR..) fall-through to next expr
			 (ignore-errors (magnitude-of-sun:magnitude-of-sun
					 filter :system :vega)))))
	(jcom-test-expr (not (realp %mag-sun))
			"NO-MAG-OF-SUN"
			(format nil "Cannot compute absolute magnitude of sun using MAG-SUN=~A or FILTER=~A.   One of these must be given." mag-sun filter))
      (jcom-test-expr (not (every 'realp (list hmag nucleus-color rhelio delta phase-angle)))
		      "INVALID-INPUT"
		      (format nil "One of the following is not a valid real number: HMAG-NUCLEUS=~A; NUCLEUS-COLOR=~A; RHELIO=~A; DELTA=~A; PHASE-ANGLE=~A"
			      hmag nucleus-color rhelio
			      delta phase-angle))
	       
	       ;; error; mag, mag-err, and apertures must be equal
	       ;; length vectors of reals
      (jcom-test-expr (not
		       (and (every
			     ;; v is a vector of reals?
			     (lambda (v) (and (vectorp v) (every 'realp v)))
			     (list
			      mag-vec mag-err-vec ap-diam-vec))
			    (= (length mag-vec) (length mag-err-vec)
			       (length ap-diam-vec))))
		      "INVALID-VECTORS"
		      "MAG-VEC, MAG-ERR-VEC, and APERTURE-DIAMETER-VEC are not equal-length vectors of reals.")
      
	       ;; passed input tests
      (let* ((nucleus-mag (- (asteroids:apparent-magnitude
			      hmag
			      rhelio
			      delta
			      phase-angle)
			     nucleus-color))
	     ;; elements can be nil if no valid result (nucleus brighter than obj)
	     (mag-vec-fixed
	       (map 'vector
		    (lambda (mag)
		      (if (< mag nucleus-mag)
			  (subtract-mag-from-totmag mag nucleus-mag)))
		    mag-vec))
	     (afro+err-vec
	       (map 'vector ;; of afrho,afrho-err
		    (lambda (mag mag-err ap-diam)
		      (if mag ;; can invalid mag if nucleus too bright
			  (multiple-value-bind (afrho afrho-err)
			      (a-f-rho:a-f-rho mag mag-err
					       rhelio delta 
					       (* 0.5 ap-diam)
					       :mag-sun %mag-sun)
			    (list afrho afrho-err))))
		    mag-vec-fixed mag-err-vec ap-diam-vec))
	     (afrho-vec (map 'vector 'first afro+err-vec))
	     (afrho-err-vec (map 'vector 'second afro+err-vec)))
	;;
	(set-param "AFRHO-VEC" afrho-vec)
	(set-param "AFRHO-ERR-VEC" afrho-err-vec)
	(set-param "MAG-NUCLEUS" nucleus-mag)
	(set-param "MAG-SUN" %mag-sun)
	))))
