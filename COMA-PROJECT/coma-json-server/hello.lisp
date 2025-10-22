
(in-package coma-json-server)

(def-json-command hello (json-req)
  (with-json-command-setup (json-req)
    (let ((a (get-param "A" :default 99))
	  (delay (get-param "DELAY" :default 0)))

      (jcom-test-expr (or (not (realp delay))
			  (not (<= 0 delay 3600)))
		      "INVALID-DELAY"
		      "Delay must be a real between 0 and 3600 seconds.")
      (sleep delay)
      (jcom-test-expr (not (realp a))
		      "A-IS-NOT-REAL"
		      (format nil "parameter A=~A is not real" a))
      (set-param "PI" pi)
      (set-param "A-PLUS-ONE" (+ a 1))
      (set-param "VECTOR" #(1 2 3 4))
      (set-param "STRING"  "The quick brown fox ...")
      (set-param "INPUT-PARAMETERS" parameters))))

;; old version before macros
#+nil
(def-json-command hello (json-req)
  (combobulate-result-object ;; set up ID, COMMAND, etc
   json-req
   (make-json-object
    :type "response"
    :parameters
    (alexandria:alist-hash-table 
     `(("PI" . ,pi)
       ("VECTOR" . #(1 2 3 4))
       ("STRING" . "The quick brown fox ...")
       ("INPUT-PARAMETERS" ;; a hash table
	. ,(json-object-parameters json-req)))
     :test 'equalp))))


  
