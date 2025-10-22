
(in-package coma-json-server)
#|

The request:

{
    "TYPE":"REQUEST",
    "COMMAND":"COLOR-CONVERT",
    "ID":"123ABC",
    "PARAMETERS": {"COLOR-CONVERSION-REQUESTS" :  
                           [   // vector of requests for conversions
                              {   // one color conversion request, possibly of many
                                "TYPE": "COLOR-CONVERSION-REQUEST",
                                "ID": "color-conversion-xyz789", // a name to identify this sub-request
                                "OUTPUT": "VJ",   // what we want to get out; Johnson V in this example
                                "KNOWNS": // vector of what we know about this measurement
                                      [
                                          { 
                                            "TYPE" : "COLOR-CONVERSION-KNOWN",
                                            "QUALITY": "gsdss",     // a gsdss magnitude
                                            "VALUE": 23.32,         // its value
                                            "VALUE-ERROR": 0.12     // its error
                                          },
                                          { 
                                            "TYPE" : "COLOR-CONVERSION-KNOWN",
                                            "QUALITY": "gsdss-rsdss",  //  a color
                                            "VALUE": 22.55,            // its value
                                            "VALUE-ERROR": 0.07        // its error
                                          },
                                          ...
                                      ]  // end of KNOWNS
                                },   // end of this COLOR-CONVERSION-REQUEST
                               ...
                            ] // end of vector of COLOR-CONVERSION-REQUESTS
                    }
}

The response: 

{
    "TYPE":"RESPONSE",
    "COMMAND":"COLOR-CONVERT",
    "ID":"123ABC",
    "PARAMETERS":{
       "COLOR-CONVERSION-RESPONSES":
         [ // parallel to request vector above
              {
                "TYPE": "COLOR-CONVERSION-RESPONSE",
                "ID": "color-conversion-xyz789",
                "OUTPUT": "VJ", // same as above
                "VALUE": 23.0001, 
                "VALUE-ERROR": 0.112,
                "TRANSFORM-NAME": "Some-transform-name"
                },

               {  // example of a case where an error occurred and there is  no result
                "TYPE": "COLOR-CONVERSION-RESPONSE",
                "ERROR": {
                           "TYPE":"ERROR",
                           "ERROR": "COLOR-CONVERSION-ERROR",
                           "ID": "color-conversion-xyz789",
                           "DESCRIPTION": "details of error"
                          }
               }
         ]
    }
}




|#




(def-json-command color-convert (json-req)
  (with-json-command-setup (json-req)
   (let* ((cc-request-vec (get-param "COLOR-CONVERSION-REQUESTS" :required t)))
     
     
     (jcom-test-expr (not (vectorp cc-request-vec))
                     "COLOR-CONVERSION-REQUESTS-NOT-A-VECTOR"
                     "COLOR-CONVERSION-REQUESTS must b a vector")

     (let* ((req-vec (map 'vector 'parse-json-color-conversion-request cc-request-vec))
	    (resp-vec (map 'vector 'process-cc-req req-vec))
	    (json-resp-vec
	      (map 'vector 
		   (lambda (resp)
		     ;;(describe resp)
		     (let ((h (make-hash-table :test 'equalp)))
		       (setf (gethash "TYPE" h) "COLOR-CONVERSION-RESPONSE")
		       (setf (gethash "ID" h) (ccresp-id resp))
		       (if (ccresp-error resp)
			   (setf (gethash "ERROR" h)
				 (build-error-hash-json
				  :name "ERROR-DURING-COLOR-CONVERSION"
				  :desc (ccresp-error resp)))
			   (progn
			     (setf (gethash "OUTPUT" h)
				   (band-symbol-to-string  (ccresp-output resp)))
			     (setf (gethash "VALUE" h)
				   (ccresp-value resp))
			     (setf (gethash "VALUE-ERROR" h)
				   (ccresp-value-error resp))
			     (setf (gethash "TRANSFORM-NAME" h)
				   (ccresp-transform-name resp))))
		       h))
		   resp-vec)))
       (set-param "COLOR-CONVERSION-RESPONSES"
		  json-resp-vec)))))
				    
	   
     
     

  
