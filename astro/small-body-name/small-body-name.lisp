

(defpackage small-body-name
  (:use #:cl)
  (:export
   #:parse-small-body-name
   #:fuzzy-match-common-name
   #:find-matching-common-names
   #:get-body-by-id 
   #:iterate-over-small-bodies))

(in-package small-body-name)

#|

some cases we address

(1) Ceres  --> '("(1)" "Ceres" :asteroid)
1   Ceres  --> '("(1)" "Ceres" :asteroid)
Ceres      --> '("(1)" "Ceres" :asteroid)
1_Ceres    --> '("(1)" "Ceres" :asteroid)
1Ceres     --> '("(1)" "Ceres" :asteroid)  ;; note that 2Panda won't work because
                                          ;; it could be 2P Anda

;; common names are parsed but only if not degenerate
(small-body-name:parse-small-body-name "West-Kohoutek-Ikemura")
           --> '("76P" "West-Kohoutek-Ikemura" :comet)
Halley     --> '(nil "Halley"  :degenerate) ;; both asteroid and comet exist

C/2010 A2  --> '("C/2010 A2" nil :comet)
C/2010 A2  Linear --> '("C/2010 A2" "Linear" :comet)
C2010A2          --> '("C/2010 A2" nil :comet)
 (C may be be any one of CPDXA)

238P              --> '("238P" nil :comet)
238P/Read         --> '("238P" "Read" :comet)

2I/Borisov        --> '("2I" "Borisov" :interstellar)

;; MPC provisional designations
2006VW139         --> '("2006 VW139" nil :asteroid)
2006 VW139        --> '("2006 VW139" nil :asteroid)
;; but if if overlaps with a comet (ie, a re-designated known comet)
2020 WJ5          --> '("C/2020 WJ5" "Lemmon" :comet)

;; MPC confirmation designations
eg C04AKE1, A10ceJ9 

;; MPC packed designations
J95F13B             -->  '("1995 FB13" nil :asteroid)
CK17K020            -->  '("C/2017 K2" "PANSTARRS" :comet)
K17K020             -->  '("C/2017 K2" "PANSTARRS" :comet)

;; MISSING (see http://www.minorplanetcenter.net/iau/info/OldDesDoc.html)
Survey designations, and old-style provisional designations


|#
 

(defun %space-trim (string)
  (declare (type string string))
  (string-trim #(#\space #\tab #\cr #\lf) string))

(defun %space-quote-trim (string) ;; for reading csv fields
  (declare (type string string))
  (string-trim #(#\space #\tab #\cr #\lf #\") string))

;; safe string ref
(defun %cref (string n)
  (declare (type string string)
	   (type (unsigned-byte 28) n))
  (when (> (length string) n)
    (aref string n)))

;; NIL if string is empty
(defun %notemptystring (string)
  (declare (type string string))
  (if (plusp (length string))
      string
      nil))
    
(defun %remove-enclosing-parens (name)
  (declare (type string name))
  (string-left-trim
   '(#\space #\tab #\( )
   (string-right-trim
    '(#\space #\tab #\) )
    name)))

;; is STR made up of digits 0..9?   it can also be (nnn) if :allow-parens set
(defun %numstring-p (str &key (allow-parens nil))
  (declare (type string str)
	   (optimize speed))  
  (loop with nmax of-type fixnum = (if (zerop (length str))
				       (return nil) ;; bail on empty string
				       (1- (length str)))
	with ndigits of-type fixnum = 0
	with nparens of-type fixnum = 0 
	with first-paren = (char= (aref str 0) #\( )
	with last-paren = (char= (aref str nmax) #\) )
	with two-parens = (and first-paren last-paren)
	with one-paren = (and (not two-parens) (or first-paren last-paren))
	  initially (when one-paren (return nil)) ;; bad - just one paren
		    (when (and (not allow-parens) two-parens) ;; bad - parens not allowed
		      (return nil))
	for i of-type fixnum from (if first-paren 1 0) to (if last-paren (1- nmax) nmax)
	for c = (aref str i)
	when (not (digit-char-p c))
	  do (return nil)
	do (incf ndigits)
	finally (if (plusp ndigits)
		    (return t)
		    (return nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load numbered comets from file - EQUALP so case insensitive
(defvar *small-body-names-loaded* nil)

(defvar *numbered-comet-name-hash* (make-hash-table :test 'equalp))
(defvar *periodic-comet-name-hash* (make-hash-table :test 'equalp))
(defvar *all-comet-els-name-hash*      (make-hash-table :test 'equalp))

;; has KEY=1 and KEY="Ceres" for VAL=("1" "CERES")
(defvar *asteroid-name-hash* (make-hash-table :test 'equalp)) 

;; a hash of all things tied to a common name, where there
;; may be dupilicates
;; KEY="Kopff" and VALUE='(("(1631)" "Kopff" :asteroid)
;;                         ("22P"  "Kopff" :comet))
(defvar *common-name-hash*  (make-hash-table :test 'equalp))

;; KEY="1P" VALUE=("1P" "Halley" :comet)  or 
;; KEY="1"  VALUE=("1"  "Ceres"  :asteroid)
(defvar *all-id-hash* (make-hash-table :test 'equalp))

(defun get-body-by-id (id)
  "Retrieve a body by ID, which can be a number (for an asteroid), or
a formal comet designation."
  (let ((hash *all-id-hash*))
    (cond
      ;; a number, so express as "(123)"
      ((and (integerp id) (plusp id))
       (gethash (format nil "(~D)" id) hash))
      ;; a string like "123", so insert parens
      ((%numstring-p id :allow-parens nil)
       (gethash (format nil "(~D)" id) hash))
      ;; any other string, including "(123)"
      (t
       (gethash id hash)))))




(defun load-numbered-comet-names ()
  (let ((comet-name-file
	  (asdf:system-relative-pathname  
	   (asdf:find-system "small-body-name")
	   "data/numbered_comets.txt")))
    (clrhash *numbered-comet-name-hash*)
    (with-open-file (s comet-name-file)
      (loop for line = (read-line s nil nil)
	    until (not line)
	    for nslash = (position #\/ line)
	    when nslash ;; some comets don't have a common name
	      do (let* ((name (subseq line 0 nslash))
			(%desc (subseq line (1+ nslash)))
			(desc (if (equalp %desc "") nil %desc)))
		   (setf (gethash name *numbered-comet-name-hash*) desc))))))

(defun load-periodic-comet-names ()
  (let ((comet-name-file
	  (asdf:system-relative-pathname
	   (asdf:find-system "small-body-name")
	   "data/periodic_comets.csv")))
    (clrhash *periodic-comet-name-hash*)
    (with-open-file (s comet-name-file)
      (loop for line = (read-line s nil nil)
	    until (not line)
	    for ncomma
	      = (or (position #\, line)
		    (error "Invalid line ~A in periodic_comets.csv" line))
	    for name = (%space-quote-trim (subseq line 0 ncomma))
	    for %desc = (%space-quote-trim (subseq line (1+ ncomma)))
	    for desc  = (if (equalp %desc "") nil %desc)
	    do (setf (gethash name *periodic-comet-name-hash*) desc)))))


#|

AllCometEls.txt  has data like
    CK23K010  2023 09  8.9095  2.034502  0.969092  338.0035  223.8755  137.9720  20230225  14.1  4.0  C/2023 K1 (ATLAS)                                        MPEC 2023-M14
    PK23M010  2023 12 14.4356  2.825807  0.588049   79.7030  216.8657   12.2896  20230225  13.5  4.0  P/2023 M1 (PANSTARRS)                                    MPEC 2023-MA9
0001I         2017 09  9.4886  0.255240  1.199252  241.6845   24.5997  122.6778  20170904  23.0  2.0  1I/`Oumuamua                                             MPC107687
0001P         1986 02 26.1811  0.602947  0.966266  112.3598   59.4332   62.2400  20230225   4.0  6.0  1P/Halley                                                 98, 1083
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
0         1         2         3         4         5         6         7         8         8        10        11        12        13        14        15        16
01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
|#
(defun load-all-comet-els-names ()
  (clrhash *all-comet-els-name-hash*)
  (let ((comet-name-file
	  (asdf:system-relative-pathname
	   (asdf:find-system "small-body-name")
	   "data/AllCometEls.txt")))
     (clrhash *all-comet-els-name-hash*)
     (labels ((parse-periodic (str)
		(let* ((nlp (position #\( str))
		       (nrp (position #\) str))
		       (id (string-trim " " (subseq str 0 nlp)))
		       (common-name
			 (when (and nlp nrp)
			   (subseq str (1+ nlp) nrp))))
		  (values id common-name)))
	      (parse-numbered (str)
		(let* ((n/ (position #\/ str))
		       (id (string-trim " " (subseq str 0 n/)))
		       (common-name
			 (when n/
			   (string-trim " " (subseq str (1+ n/))))))
		  (values id common-name)))
	      (parse-any (type str)
		(cond ((eq type :periodic) (parse-periodic str))
		      ((eq type :numbered) (parse-numbered str)))))
     (with-open-file (s comet-name-file)
       (loop for line = (read-line s nil nil)
	     until (not line)
	     for nmax = (length line)
	     for fullname = (string-trim " " (subseq line 102 (min 158 nmax)))
	     for type = (if (eql (aref line 0) #\space)
			    :periodic
			    :numbered)
	     do (multiple-value-bind (name desc)
		    (parse-any type fullname)
		  (setf (gethash name *all-comet-els-name-hash*) desc)))))))
	     

 
(defun load-asteroid-names ()
  (clrhash *asteroid-name-hash*)
  (let ((ast-name-file
	  (asdf:system-relative-pathname
	   (asdf:find-system "small-body-name")
	   "data/asteroid_names.csv")))
    (clrhash *asteroid-name-hash*)
    (with-open-file (s ast-name-file)
      (loop for line = (read-line s nil nil)
	    until (not line)
	    for ncomma
	      = (or (position #\, line)
		    (error "Invalid line ~A in asteroid_names.csv" line))
	    for name = (concatenate 'string "(" (%space-quote-trim (subseq line 0 ncomma)) ")")
	    for desc = (%space-quote-trim (subseq line (1+ ncomma)))
	    for val = (list name desc)
	    do (setf (gethash name *asteroid-name-hash*) val)
	       (setf (gethash desc *asteroid-name-hash*) val)))))

(defun build-common-name-and-id-hash ()
  (clrhash *common-name-hash*)
  (clrhash *all-id-hash* )
  (loop for ast-key being the hash-key of *asteroid-name-hash*
	for ast-pair being the hash-value of *asteroid-name-hash*
	for is-numerical = (%numstring-p ast-key :allow-parens t)
	for body = (list (first ast-pair) (second ast-pair) :asteroid)
	;; for *common-name-hash*, count only key,val= "Ceres",("(1)" "Ceres") and not  "(1)",("(1)" "Ceres") 
	when (not is-numerical)
	  do (push body   (gethash ast-key *common-name-hash*))
	     ;; for *all-id-hash* use "1" but not "Ceres"
	when is-numerical
	  do (setf (gethash ast-key *all-id-hash*) body))
  ;;
  (let ((comet-id-hash (make-hash-table :test 'equalp))) ;; to avoid duplicating comet IDs
    (flet ((do-comet-hash (hash)
	     (loop for formal-name being the hash-key of hash
		   for common-name being the hash-value of hash
		   for body = (list formal-name common-name :comet)
		   ;; put it into hash by ID
		   when (not (gethash formal-name *all-id-hash*))
		   do (setf (gethash formal-name *all-id-hash*) body)
		   ;; put it into hash clustered by common name
		   when (and common-name ;; some asteroids don't have a common name
			     (not (gethash formal-name comet-id-hash)))
		     do
			(setf (gethash formal-name comet-id-hash) t) ;; we used this formal name
			(push body
			      (gethash common-name *common-name-hash*)))))
      (do-comet-hash *all-comet-els-name-hash*)
      ;; the following are probably superfluous, but they won't be done
      ;; if an entry from *all-comet-els-name-hash* exists
      (do-comet-hash *numbered-comet-name-hash*)
      (do-comet-hash *periodic-comet-name-hash*))))  

(defun load-all-small-body-data ()
  (load-all-comet-els-names) ;; this is PROBABLY the only one we need for comets
  (load-numbered-comet-names)
  (load-periodic-comet-names)
  (load-asteroid-names)
  (build-common-name-and-id-hash) 
  (setf *small-body-names-loaded* t))

(eval-when (:load-toplevel)
  (when (not *small-body-names-loaded*)
    (load-all-small-body-data)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-matching-common-names (common-name)
  "Find the objects with a common name exactly matching COMMON-NAME.
See also FUZZY-MATCH-COMMON-NAME."
  (declare (type string common-name))
  (nth-value 0 (gethash (%space-trim common-name) *common-name-hash*)))


(defun iterate-over-small-bodies (function &key (do-asteroids t) (do-comets t))
  "Iterate over *ALL-ID-HASH* and call (FUNCTION BODY) for each body
in it, where BODY can be of the form
  (\"1P\" \"Halley\" :comet) or (\"1\" \"Ceres\" :asteriod)"
  (loop for bodY being the hash-value of *all-id-hash* 
	for type = (third body)
	when (or (and (eq type :comet) do-comets)
		 (and (eq type :asteroid) do-asteroids))
	  do (funcall function body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fuzzy-match-common-name (name &key (match-threshold 0.4)
				     (comets-first nil))
  "Usie fuzzy matching to match objects that might be an object.
MATCH-THRESHOLD is in [0.0,1.0].
Returns, eg,

  (((\"1\" \"Ceres\" :asteroid) 1.0)
   ((\"25778\" \"Csere\" :asteroid) 0.705)
   ...)

where, the numbers at the end are the match strength.

COMETS-FIRST means to reorder the output list, so that it places
all comets first, on the assumption that a mystery object is more likely
to be a comet."
  (declare (type string name))
  (let* ((asteroid-matches nil)
	 (comet-matches nil))

    (loop for cname being the hash-key of *common-name-hash*
	  for matches being the hash-value of *common-name-hash*
	  for match-strength = (string-utils:fuzzy-match name cname
							 :use :alphabetic)
	  when (> match-strength match-threshold)
	    do 
	       (loop for match in matches
		     for ms = (list match match-strength)
		     for type = (third match)
		     do (cond ((eq type :comet)
			       (push ms comet-matches))
			      ((eq type :asteroid)
			       (push ms asteroid-matches)))))

    (if comets-first
	(append
	 (sort comet-matches '> :key 'second)
	 (sort asteroid-matches '> :key 'second))
	(sort
	 (append comet-matches asteroid-matches)
	 '> :key 'second))))

	  
	  






  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "123" or "(123)" or "(1) Ceres" this parses "(1) FOOBAR" as ("1"
;;   "FOOBAR") so it does not correct the name.
;;
;; RECURSING is for the case of 6478Gault being split up and re-done
;;   recursively.
(defun %parse-numbered-asteroid-name (sname &key (recursing nil))
  (when (stringp (first sname)) ;; safety check
    (let (ast-num ast-name
	 ;; kludgy way of parsing 1Ceres
	  nast nafter) 
      (let ((s1 (first sname)))
	(cond ((every 'digit-char-p (first sname))
	       (setf ast-num (parse-integer (first sname))))
	      ((and (> (length s1) 2)
		    (char= (aref s1 0) #\( )
		    (char= (aref s1 (1- (length s1))) #\))
		    (every 'digit-char-p
			   (subseq s1 1 (1- (length s1)))))
	       (setf ast-num
		     (parse-integer s1 :start 1 :end (1- (length s1)))))
	      ;; now the case of 1Ceres - split it into ("1" "Ceres ..."
	      ;; and recurse - 
	      ((progn ;; kludgy - if nast is parsed, return T and continue
		 (multiple-value-setq (nast nafter) 
		   (parse-integer (first sname) :junk-allowed t))
		 (and nast (not recursing)))
	       (when (and nast
			  ;; if the name has a digit, it's probably not an asteroid
			  (notany #'digit-char-p
				  (subseq (first sname) nafter))
			  ;; 2Pxx or 2Ixx could be comet or interstellar 
			  (not (member (aref (first sname) nafter) '(#\P #\I))))

		 (return-from %parse-numbered-asteroid-name
		   (%parse-numbered-asteroid-name 
		    (append (list (format nil "~A" nast)
				  (subseq (first sname) nafter))
			    (cdr sname))
			    ;; don't recurse more than once
			    :recursing t)))))
			    
	;;
	(when (and ast-num (> (length sname) 1))
	  (setf ast-name (format nil "~{~A~^ ~}" (cdr sname))))
	;;
	(when ast-num
	  (let ((desig (format nil "~D" ast-num)))
	    (list
	     (concatenate 'string "(" desig ")")
	     (or ast-name (second (gethash desig *asteroid-name-hash*)))
	     :asteroid)))))))

;; eg "238P" or "238P/Read" or "238P Read" or "238PRead"
;; or similar for "2I" etc for interstellars
;; letter is P for comet, I for intestellar
(defun %parse-numbered-thing-name (sname letter output-type)
  (when (stringp (first sname)) ;; safety check
    (let ((s1 (first sname))
	  (after-p nil)
	  (fragment "") ;; the "-A" style fragment name if any
	  (name nil))
      (multiple-value-bind (num nc)
	  (parse-integer s1 :junk-allowed t)
	(cond
	  ;; interstellars can't have large N
	  ((and num
		(> num 10) ;; ha ha ha
		(eql letter #\I))
	   nil)
	  ;; it's valid nnnP
	  ((and num 
		(> (length s1) nc)
		(char-equal (aref s1 nc) letter))
	   ;; specially handle 238P-A by checking for -x where
	   ;; x is a letter, and incrementing nc by 2 if it is present, and
	   ;; continuing as before
	   (when (and (> (length s1) (+ 2 nc))
		      (char= (aref s1 (1+ nc)) #\-)
		      (alpha-char-p (aref s1 (+ 2 nc))))
	     (setf fragment (string-upcase (subseq s1 (+ 1 nc) (+ 3 nc))))
	     (incf nc 2))
	   ;; now continue various forms of 238P, 9PTempel, etc
	   (cond
	     ((and (> (length s1) (1+ nc)) ;; do we have a slash like 238P/Read
		   (char-equal (aref s1 (1+ nc)) #\/))
	      (setf after-p
		    (%notemptystring
		     (%space-trim (subseq s1 (+ nc 2))))))
	     (t ;; a case like "9PTempel1"
	      (setf after-p
		    (%notemptystring
		     (%space-trim (subseq s1 (+ nc 1)))))))
	   ;;
	   (when (or (and after-p (plusp (length after-p)))
		     (cdr sname))
	     (setf name
		   (%remove-enclosing-parens
		    (format nil "~{~A~^ ~}"
			    (if after-p
				(cons after-p (cdr sname))
				(cdr sname))))))
	   (when num
	     (let* ((parent-desig (format nil "~D~A" num letter)) ;; no fragment
		    (desig (concatenate 'string parent-desig fragment)))
	       (list desig
		     (or name
			 (if (eql letter #\P)
			     (or (gethash desig *numbered-comet-name-hash*)
				 (gethash parent-desig *numbered-comet-name-hash*))))
		     output-type)))))))))

(defun %parse-numbered-comet-name (sname)
  (%parse-numbered-thing-name sname #\P :comet))
(defun %parse-interstellar-name (sname)
  (%parse-numbered-thing-name sname #\I :interstellar))

  



;; sometimes we get "2018 F4" when it hasn't been designated as
;; "C/2018 F4" so we cautiously parse this before we parse comets
(defun %parse-uncategorized-comet-name (sname)
  (when (= (length sname) 2) ;; consider only clean names, so 2018 F4 but not 2018F4 
    (let ((year (ignore-errors (parse-integer (first sname)))))
      (when (and year (<= 1990 year 2100)) ;; consider only plausible years
	(let ((subdesig (second sname)))
	  (when (and (<= 2 (length subdesig) 3)
		     (alpha-char-p (aref subdesig 0))
		     (every 'digit-char-p (subseq subdesig 1)))
	    (list
	     (format nil "~A ~A" year subdesig) nil :comet)))))))

;; parse "2018 F4" but try to match it to "C/2018 F4" if it exists
(defun %parse-uncategorized-comet-name-and-match (sname)
  (let ((match (%parse-uncategorized-comet-name sname)))
    (when match
      (let ((name (first match))) ;; eg "2018 F4"
	(let ((real-id ;; if exists, eg ("C/2022 L1" . "Catalina")
		(when name
		  (loop for prefix in '("C" "P" "D" "A" "X")
			for fullname = (concatenate 'string prefix "/" name)
			for desc = (gethash fullname
					    *periodic-comet-name-hash*)
			when desc do (return (cons fullname desc))))))
	  (if (not real-id)
	      match ;; use original match
	      (list (car real-id) (cdr real-id) :comet))))))) ;; or the new one
	  

		 
  


;; Tn ugly function to enforce the subdesig of a comet, defined as
;; Cnnn-FF or CC-nnn-FF, where C is an upperchase char, nnn is a
;; number>1, and -FF is a fragment made up of at most 2 alphabetic
;; chars.  Returns SD if it is of valid form Cnnn-FF.  This is
;; heinous, and should be done with some kind of pattern matching.
(defun %validate-comet-subdesig (sd)
  (declare (type (or null string) sd))
  (when (>= (length sd) 2)
    (loop with in-frag = nil ;; nil or number of frag chars
	  with ndigits  = 0 ;; ensure at least one digit after letter(s)
	  for i below (length sd)
	  for c = (aref sd i)
	  do (cond 
	       ;; first char must be uppercase letter
	       ((and (= i 0)
		     (not (upper-case-p c)))
		(return nil)) ;; does not start with letter
	       ;; second char must be a digit 1-9, or a 2nd letter
	       ((and (= i 1)
		     (and (not (upper-case-p c))
			  (or (not (digit-char-p c))
			      (eql c #\0))))
		(return nil))
	       ;; if not in-frag and found -, set in-frag
	       ((and (not in-frag)
		     (eql c #\-))
		(setf in-frag 0))
	       ;; if still not in-frag, must be a digit
	       ((and (not in-frag)
		     (> i 1)
		     (not (digit-char-p c)))
		(return nil))
	       ;; increase the digit count
	       ((and (not in-frag) (digit-char-p c))
		(incf ndigits))
	       ;; if in-frag, c must be letter, and not more than 2
	       (in-frag
		(when (= in-frag 2) (return nil)) ;; too many frag chars
		(when (not (alpha-char-p c))
		  (return nil))
		(incf in-frag)))
	  finally (return ;; if in-frag, must have 1 char
		    (cond ((zerop ndigits)
			   nil)
			  ((not in-frag)
			   sd)
			  ((plusp in-frag)
			   sd)
			  (t
			   nil))))))


;; "C/2008 AB123"  "C/2008 AB123 FOO" "C/2008 AB123 (FOO)"  "C2012AB123"  "C_2008_AB123"
;;  "C/2008 AB123/FOO"  
;; --> ("C/2018 AB123" "FOO" :comet)
;; this does respect fragments, but does not check validity.
(defun %parse-comet-name (sname)
  (when (stringp (first sname)) ;; safety check
    (let* ((s1 (first sname))
	   (comet-type (char-upcase (%cref s1 0)))
	   (is-comet (member comet-type '(#\C #\P #\D #\X #\A)))
	   (has-slash (equalp (%cref s1 1) #\/)))
      (when is-comet
	(multiple-value-bind (yr ny)
	    (parse-integer s1
			   :start (if has-slash 2 1)
			   :junk-allowed t)
	  (when (and yr (< 1900 yr 2999))
	    (let* ((after-yr (%notemptystring (%space-trim (subseq s1 ny))))
		   ;; subdesig is "A2" in "P/2010 A2"
		   (subdesig (%validate-comet-subdesig (if after-yr after-yr (cadr sname))))
		   (namelist (if after-yr (cdr sname) (cddr sname))))
	      (cond (subdesig
		     (let ((desig
			     (string-upcase
			      (format nil "~A/~D ~A" comet-type yr subdesig)))
			   (name (%notemptystring
				  (%remove-enclosing-parens
				   (format nil "~{~A~^ ~}" namelist)))))
		       (list desig
			     (or name (gethash desig *periodic-comet-name-hash*))
			     :comet)))
		    (t
		     nil)))))))))
		     
      
		 
;; "2006 VW139" or "2006VW139" -- YYYY AVnnnn
;; or "2015 WZ" - YYYY XX
;; if FIX-KNOWN-COMET is true, it checks that the provisional designation
;; has been converted to a comet
(defun %parse-mpc-provisional-designation (sname &key (fix-known-comet t))
  (when (stringp (first sname)) ;; safety check
    (cond ((= (length sname) 1)
	   (multiple-value-bind (yr ny)
	       (parse-integer (first sname) :junk-allowed t)
	     (when (and yr (< 1900 yr 2999))
	       (let ((tail (%notemptystring
			    (%space-trim (subseq (first sname) ny)))))
		 (when
		     (and (>= (length tail) 2) ;; digits MAY be at end
			  (alpha-char-p (aref tail 0))
			  (alpha-char-p (aref tail 1))
			  (every 'digit-char-p
				 (subseq tail 2)))
		   (%maybe-fix-provisional-designation-desig-to-comet
		    (list (string-upcase (format nil "~D ~A" yr tail))
			  nil :asteroid)
		    fix-known-comet
		    ))))))
	  ((= (length sname) 2)
	   (let ((yr (ignore-errors (parse-integer (first sname)))))
	     (when yr
	       ;; just recurse to the double version
	       (%parse-mpc-provisional-designation
		(list (concatenate 'string (first sname) (second sname)))))))
	  (t ;; no other length besides 1,2 is OK
	   nil))))

;; given a result like ("2020 WJ5" nil :asteroid) that is a provisional designation,
;; look in *periodic-comet-name-hash* to see if the name has been transformed to a
;; named comet
(defun %maybe-fix-provisional-designation-desig-to-comet (result &optional do-the-fix)
  (if (not do-the-fix)
      result ;; do nothing if flag was not set
      (let ((name (first result)))
	(loop for prefix in '("P" "C" "A")
	      for pname = (concatenate 'string prefix "/" name)
	      for common-name = (gethash pname *periodic-comet-name-hash* t)
	      when (not (eq common-name t)) ;; T means 'nothing found'
		do
		   (return (list pname
				 common-name ;; could be NIL
				 :comet))
	      finally (return result)))))


;; designations on MPC confirmation pages.
;; have no idea how these really worked - they seem to be
;; different than PACKED PROVISIONAL
(defun %parse-mpc-confirmation-designation (sname)
  (when (and (stringp (first sname))
	     (= (length sname) 1))
    (let ((name (first sname)))
      (when (and (or (= (length name) 7)
		     (= (length name) 6))
		 ;; first letter is capital
		 (<= (char-code #\A)
		     (char-code (aref name 0))
		     (char-code #\Z))
		 ;; all alphanumerical
		 (every 'alphanumericp name))
      (list name nil :mpc-confirmation-designation)))))

;; a misc catchall designation
(defun %parse-misc-designation (sname)
  (when (and (stringp (first sname))
	     (= (length sname) 1)
	     (some 'alpha-char-p (first sname)))
    (list (first sname) nil :misc-designation)))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MPC packed designations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %parse-mpc-packed-designation (sname)
  (multiple-value-bind (desig type)
      (mpc-packed-desig:parse-packed-name sname)
    (when desig
      (or ;; try to turn into a known
       ;; good for most comets and asteroids
       (get-body-by-id desig)
       ;; perhaps packed desig was 2017 K2 and not C/2017 K2
       (when (and (eq type :comet)
		  (not (eql 0 (search "C/" desig))))
	 (get-body-by-id (concatenate 'string "C/" desig)))
       ;; or just return what we found without matching to a known
       (list desig nil type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
;; some major bodies we know by name
(defparameter *planets*
  '("Mercury" "Venus" "Earth" "Mars"  "Jupiter" "Saturn" "Uranus" 
    "Neptune" "Pluto"))
(defparameter *major-asteroids* ;; in order of number
  '("Ceres" "Pallas" "Juno" "Vesta" "Astraea"
    "Hebe" "Iris" "Flora" "Metis" "Hygiea" "Parthenope" "Victoria"
    "Egeria" "Irene"  "Eunomia" "Psyche" "Thetis" ))


(defun %parse-planet (sname)
  (when (and (= (length sname) 1)
	     (find (first sname) *planets* :test 'equalp))
    (list (first sname) nil :planet)))

(defun %parse-major-asteroid (sname)
  (when (= (length sname) 1)
    (let ((n (position (first sname) *major-asteroids* :test 'equalp)))
      (when n
	(list (format nil "~D" (1+ n))
	      (string-upcase (first sname)) :asteroid)))))




;; given "blah (foo) boo" return "(foo)"
(defun %extract-parenthetical-expression (name)
  (declare (type string name))
  ;; the name is some kind of parenthetical expression  BLAH (16P)"
  (when (or (and (find #\( name)  (find #\( name))
	    (and (find #\[ name)  (find #\] name)))
    (let ((nopen  (or (position #\( name) (position #\[ name)))
	  (nclose (or (position #\) name) (position #\) name))))
      (when (< nopen nclose)
	(subseq name (1+ nopen) nclose)))))

;; the internal routine than can recurse.
;; RECURSE means recurse this many more levels if AGGRESSIVE
;; AGGRESSIVE means we're in aggressive mode (using RECURSE)
;; RECURSING means we're already in a recursion, so cut down on types of parsing
(defun %parse-small-body-name-internal (name &key (recurse 0) (aggressive nil)
					     (recursing nil))
  (declare (type string name))

  (let* ((sname ;; split string, on spaces, after replacing underscores
	   (string-utils:split-string  
	    (substitute #\space #\_ name)
	    #(#\space #\tab)))
	 ;; a special sname, to handle "C_2015_P2"=="C/2015_P2"
	 (sname2 (if (equalp (%cref name 1) #\_)
		     (let ((name2 (copy-seq name)))
		       (setf (aref name2 1) #\/)
		       (string-utils:split-string  
			(substitute #\space #\_ name2)
			#(#\space #\tab)))))
	 ;; a special sname, without parens, to handle provisional
	 ;; "(1999 XY143)"
	 (sname3 (string-utils:split-string
		  (substitute #\space #\_ (%remove-enclosing-parens name))
		   #(#\space #\tab))))

    (block done
      (flet ((ret-vals (parsing-success parsing-method)
	       (when parsing-success
		 (return-from done (values parsing-success parsing-method)))))
	       
	(progn
	  ;; must go before asteroid
	  (ret-vals (%parse-mpc-provisional-designation sname) ;; "1999 XY143"
		  "mpc-provisonal")
	  (ret-vals (%parse-mpc-provisional-designation sname3) ;; "(1999 XY143)"
		  "mpc-provisional3")
	  ;; a comet without a C/ or a P/
	  (ret-vals (%parse-uncategorized-comet-name-and-match sname)
		  "comet-without-C-or-P")
	  ;; a packed designation
	  (when (= (length sname) 1) ;; a SINGLE string
	    (ret-vals (%parse-mpc-packed-designation (first sname))
		      "mpc-packed"))
	  ;; if we're recursing, don't grab any number as an asteroid -
	  ;;  that's too loose
	  (when (not recursing)
	    (ret-vals (%parse-numbered-asteroid-name sname)
		    "numbered-asteroid"))
	  (ret-vals (%parse-numbered-comet-name sname)
		  "numbered-comet")
	  (ret-vals (%parse-interstellar-name sname)
		  "interstellar")
	  (ret-vals (%parse-comet-name sname)
		    "comet-name")
	  (ret-vals (%parse-comet-name sname2)
		    "comet-name-with-slash-replacing-underscore")
	  (ret-vals (%parse-planet sname)
		  "planet")
	  (ret-vals (%parse-major-asteroid sname)
		  :major-asteroid)
	  ;; the strange mpc confirmation format we don't get
	  (ret-vals (%parse-mpc-confirmation-designation sname)
		  "mpc-confirmation-designation")
	  ;; various other designations
	  (ret-vals (%parse-misc-designation sname)
		  "misc-designation")

	  
	  (when (plusp recurse)
	    (when aggressive
	      (progn
	       (let ((parenexp (%extract-parenthetical-expression name)))
		 (when parenexp ;; no more recursion in a paren exp
		   (multiple-value-bind (ret method)
		       (%parse-small-body-name-internal parenexp
							:recurse 0
							:recursing nil
							:aggressive nil)
		     (ret-vals ret method)))))

	      ;; more aggressive recursions could go here
	      )) ;; end OR of agressive parsing

     
	  ;; returns NIL if none of these parse successfully
	  NIL
	  )))))
     


(defun parse-small-body-name (object-name &key (aggressive nil))
  "Given a target NAME as might be found in a telescope object
header, return

  (VALUES (DESIGNATION  DESC-NAME  TYPE)
          PARSING-METHOD-STRING)

where 
DESIGNATION is a number for an asteroid or '238P',
            'C/2010 A2' for a comet, or a planet name
DESC-NAME   is a descriptive name if present, like 
            'Jacques' in 'C/2014 E2 Jacques'
TYPE        is one of :ASTEROID or :COMET or :PLANET

The keyword AGGRESSIVE means to search more aggressively for
designations lurking inside NAME.

The second value is a string indicating the method that was used
to parse it, for diagnostics.
"
  (declare (type string object-name))
  
  (let* ((name (%space-trim object-name))
	 ;; first get the simple common-name matches like "Halley"
	 (common-matches
	   (gethash name *common-name-hash*)))

    (cond
      ;; just one common-match
      ((= (length common-matches) 1)
       (values (first common-matches) :common-name-match))
      ;; more than one - degenerate object
      ((> (length common-matches) 1)
       (values (list nil name :degenerate)
	       :degenerate-common-name-match))
      ;; else do full parsing
      (t
       (%parse-small-body-name-internal
	name
	:recurse 2 ;; permit 2 recursion levels
	:aggressive aggressive)))))
