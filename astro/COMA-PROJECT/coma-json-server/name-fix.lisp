
#|

NAMEFIX-ing takes the object in a header, and corrects it using
a regexp table

An environment variable called "COMA_NAMEFIX_FILE" points to a file
with fields like

 REGEX, STANDARD_NAME, COMMON_NAME
"^(?i)temp(el|le)[ ]*1"   ,  "9P" , "Tempel"
"K2 PanSTARRS"              , "C/2017 K2",  "PanSTARRS"
 ...

that will translate, eg, TeMpeL1 to (values "9P" "Tempel1")


the file should be read with a LOAD-NAMEFIX-

Request:

{
  "TYPE":"REQUEST",
  "COMMAND":"LOAD-NAMEFIX-TABLE",
  "ID":"123abc",
  "PARAMETERS": {
                  "NAMEFIX-FILE":"/dir/sample.csv", 
                }
}

Response 
{
  "COMMAND": "LOAD-NAMEFIX-TABLE",
  "ID": "123abc",
  "PARAMETERS": {
       "TYPE": "PARAMETERS",
       "SUCCESS:" true
   }
}

If SUCCESS=false then there is an ERROR object:

{
    "TYPE"            : "ERROR", // this is an ERROR object
    "ERROR"           : <ErrorName>,
    "DESCRIPTION"     : <optional description string of this error>,
} 

|#

(in-package coma-json-server)
 


(defvar *namefix-lock*
  (bordeaux-threads:make-recursive-lock "name-fix-lock"))

;; list of ((regexp1 name1 common-name1) ;; COMMON-NAME can be NIL
;;          (regexp2 name2 common-name2)
;;          ...))
(defparameter *namefix-table* nil)


(def-json-command load-namefix-table (json-req)
  (with-json-command-setup (json-req)
    ;;
    (set-param "SUCCESS" nil) ;; by default
    ;;
    (let* ((namefix-file (get-param "NAMEFIX-FILE" :required t)))
      ;;
      (multiple-value-bind (ret err)
	  (setf *namefix-table*
		(read-namefix-file namefix-file))
	(when (not ret)
	  (return-with-error
	   "FAILED-TO-READ-NAMEFIX-FILE"
	   (format nil
		   "Failed to read NAMEFIX-FILE ~A - ERROR: ~A"
		   namefix-file err)))))
    (set-param "SUCCESS" t)))


(defun read-namefix-file (namefix-filename)
  (when (not (probe-file namefix-filename))
    (error "NAMEFIX file ~A not found" namefix-filename))
  (let* ((namefix-hash (or
			 (ignore-errors
			  (csv-read:read-csv-headers/columns-from-file
			   namefix-filename))
			 (error "Error reading NAMEFIX csv file ~A"
				namefix-filename)))
	 (regexp-vec   (gethash "REGEX" namefix-hash))
	 (id-vec       (gethash "STANDARD_NAME" namefix-hash))
	 (common-name-vec  (gethash "COMMON_NAME" namefix-hash)))
    
    (when (not (and regexp-vec id-vec common-name-vec))
      (error "NAMEFIX csv file does not have named columns REGEX, STANDARD_NAME, and COMMON_NAME"))
    
    ;; parse and validate the table
    (let ((namefix-table
	    (loop for regexp-string across regexp-vec
		  for id across id-vec
		  for common-name across common-name-vec
		  for i from 1
		  for regexp = (multiple-value-bind (func err)
				   (cl-ppcre:create-scanner regexp-string)
			       (if (not func)
				   (error "Error in line ~A of NAMEFIX file ~A - cannot parse regexp <~A> with error ~A" i namefix-filename regexp-string err))
				 func)
		  collect (list regexp id common-name))))
      (bordeaux-threads:with-lock-held (*namefix-lock*)
	(setf *namefix-table* namefix-table)))))

	       
	  

(defun get-namefix-table ()
  (bordeaux-threads:with-lock-held (*namefix-lock*)
    (or *namefix-table*
	(error "No NAMEFIX table loaded."))))



(defun namefix-object-name (object-name)
  (declare (type string object-name))
  (format t "Namefix ~A~%" object-name)
  (loop with namefix-table = (get-namefix-table)
	for entry in namefix-table
	for regex = (first entry)
	for id = (second entry)
	for cname = (third entry)
	for match = (cl-ppcre:scan regex object-name)
	for fullname = (format nil "~A~A~A"
			       id
			       (if cname " " "")
			       (or cname ""))
	when match
	  do
	     (return (values
		      id
		      cname
		      fullname
		      (ignore-errors
		       (small-body-name:parse-small-body-name
			fullname))))))
