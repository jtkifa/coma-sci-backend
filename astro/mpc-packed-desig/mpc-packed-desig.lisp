
(defpackage mpc-packed-desig
  (:use #:cl)
  (:export
   #:parse-packed-asteroid-name
   #:parse-packed-comet-name
   #:parse-packed-name ;; returns :comet or :asteroid as 2nd value

   #:pack-minor-planet-number
   #:pack-periodic-comet
   #:pack-asteroid-provisional-desig
   #:pack-comet-provisional-desig 
   #:pack-desig

   #:test-packing-examples
   ))

(in-package mpc-packed-desig)


(defvar *09azaz*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(defvar *09lcase*
  "0123456789abcdefghijklmnopqrstuvwxyz")
(defvar *az-noi* "ABCDEFGHJKLMNOPQRSTUVWXYZ") ;; A-Z no I
(declaim (type string *09azaz* *az-noi* *09lcase*))


;; decode packing scheme in which 0=0,1-1, .. A=10,B=20, .. z=61
;; return NIL if invalid char
(defun %decode-mpc-number (char)
  (declare (type character char))
  (let ((numchar-vec "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") )
    (position char numchar-vec)))

;; decode MPC century letter code, or NIL
(defun %decode-mpc-century (cyear)
  (declare (type character cyear))
   (cond ((char= cyear #\I) 1800)
	 ((char= cyear #\J) 1900)
	 ((char= cyear #\K) 2000)
	 ((char= cyear #\L) 2100) ;; this one not defined, yet
	 (t nil)))



;; see  http://www.minorplanetcenter.net/iau/info/PackedDes.html
;; section "Permanent Designations / Minor Planets"
;; asteroids below 100,000 are stored as 5 digits
;; asteroids over 100,000 have numbers mod 10,000 stored in chars 2-5
;; and A-Za-Z represent the next series, then numbers above 619999
;; start with a "~" in a complicated scheme
(defun parse-numbered-minor-planet-packed-desig (string) ;; 5 chars long
  "Parse a Minor Planet designation that is a number, or a
code like A1234 for overflows.  These are just 5 chars long."
  (let ((chars-a-z   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	(chars-0-z   "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (and (= (length string) 5)
	 (cond
	   ;; the simplest case of a number
	   ((every 'digit-char-p string)
	    (string-left-trim "0" string))
	   ;; minor  planet < 620,000, as "A1234"
	   ((and (every 'digit-char-p (subseq string 1))
                 (alpha-char-p (aref string 0)) 
		 (parse-integer string :start 1))
	    (let ((num-mod-10000 (parse-integer string  :start 1))
		  (num-div-10000 (+ 10
				    (position (aref string 0) chars-a-z))))
	      (format nil "~D"
		      (+ (* 10000 num-div-10000)
			 num-mod-10000))))
	   ;; minor planet above 620,000 as ~0000
	   ((and (eql #\~ (aref string 0)) ;; between 620,000 and 15396335
		 (every 'alphanumericp (subseq string 1)))
	    (loop for i from 4 downto 1
		  for pow62 = 1 then (* 62 pow62)
		  for c = (aref string i)
		  sum (* (position c chars-0-z) pow62) into ast-num
		  finally (return (format nil "~D" (+ 620000 ast-num)))))
	   ;;
	   (t
	    NIL)))))


;; parse a packed minor provisional asteroid desig like
;; J95X00A = 1995 XA or K07Tf8A = 2007 TA418
(defun parse-packed-provisional-minor-planet-desig (name)
  (block ret 
    (cond ((and (= (length name) 7)
		(upper-case-p (aref name 6)))
	   (let* ((mpc-century (or (%decode-mpc-century (aref name 0))
				   (return-from ret nil)))
		  (year (+ mpc-century
			   (parse-integer name :start 1 :end 3)))
		  (month-letter (aref name 3))
		  (2nd-letter (aref name 6))
		  (mpc-number (or (%decode-mpc-number (aref name 4))
				  (return-from ret nil)))
		  (cycle-count (+ (* 10 mpc-number)
				  (parse-integer name :start 5 :end 6))))
	     (format nil "~4D ~A~A~A" year month-letter 2nd-letter
		     (if (zerop cycle-count) "" cycle-count))))
	  ;; it can be the 7 char desig with a C,P,D,X,A in front of it
	  ((= (length name) 8)
	   (when (member (aref name 0) '(#\C #\P #\D #\X #\A))
	     (let ((desig (parse-packed-provisional-minor-planet-desig
			   (subseq name 1))))
	       (when desig
		 (format nil "~C/~A" (aref name 0) desig))))))))

(defun parse-packed-survey-designation (name)
  (when (and (= (length name) 7)
	     (eql (aref name 2) #\S)) ;; it's a survey object
    (format nil "~A ~A-~A" (subseq name 3 7) (aref name 0) (aref name 1))))

;; parsed all packed asteroid names
(defun parse-packed-asteroid-name (name &key
					  (space-trim t)
					  (allow-all-digits nil))
  "Parse any packed asteroidal designation in MPC, including
numbered asteroids, survey objects, and packed provisional designations

SPACE-TRIM means to trim spaces first.
ALLOW-ALL-DIGITS means to allow all-digits even not a strict packed designation."
  (let ((tname (if space-trim (string-trim " " name) name)))
    (or
     ;; all digits, so an asteroid (even if not the right length)
     (and allow-all-digits (every #'digit-char-p tname) tname)
     ;; packed representation of numbered asteroids
     (parse-numbered-minor-planet-packed-desig tname)
     ;; packed survey objects
     (parse-packed-survey-designation tname)
     ;; packed provisionals
     (parse-packed-provisional-minor-planet-desig tname)
     ;; new Rubin designations
     (parse-extended-packed-provisional-designation tname))))




(defun parse-packed-comet-name (name &key (space-trim t))
  "Parse a MPC packed comet name, like J94P010, which is 6 chars long,
or it can have a C or P added"
  (declare (type string name))
  (block ret
    (let ((name (if space-trim (string-trim " " name) name)))
      (cond ((= (length name) 5) ;; for "0099P", just validate and strip zeros
	     (when (and (eql (aref name 4) #\P)
			(digit-char-p (aref name 0)) (digit-char-p (aref name 1))
			(digit-char-p (aref name 2)) (digit-char-p (aref name 3)))
	       (string-left-trim "0" name)))

	    ;; 7 char compressed comet
	    ((and ;; validation
	      ;; century
	      (= (length name) 7)
	      (%decode-mpc-century (aref name 0))
	      ;; year 
	      (digit-char-p (aref name 1))
	      (digit-char-p (aref name 2))
	      ;; month letter
	      (find (aref name 3) *az-noi*)
	      ;; position in month is 09AZaz scheme
	      (find (aref name 4) *09azaz*) 
	      (find (aref name 5) *09azaz*)
	      ;; fragment is 0 or lowercase
	      (find (aref name 6) *09lcase*))
	     ;;
	     (let* ((mpc-century (or (%decode-mpc-century (aref name 0))
				     (return-from ret nil)))
		    (year (+ mpc-century
			     (parse-integer name :start 1 :end 3)))
		    (month-letter (aref name 3))
		    (mpc-number (or (%decode-mpc-number (aref name 4))
				    (return-from ret nil)))
		    (cycle-count (+ (* 10 mpc-number)
				    (parse-integer name :start 5 :end 6)))
		    (frag-char (aref name 6))
		    (fragment (if (char= frag-char #\0) ""
				  (format nil "-~A" (char-upcase frag-char)))))
	       (format nil "~4D ~A~A~A" year month-letter 
		       (if (zerop cycle-count) "" cycle-count)
		       fragment)))
	    ;; has a leading C,P,D,A,or X
	    ((and (= (length name) 8)
		  (find (aref name 0) "CPDAX"))
	     (let ((desig (parse-packed-comet-name (subseq name 1))))
	       (when desig
		 (format nil "~C/~A" (aref name 0) desig))))))))

;; new extended designation starting with _
;; https://minorplanetcenter.net/mpcops/documentation/provisional-designation-definition/#extended_packed_provid
(defun parse-extended-packed-provisional-designation (name &key (space-trim t))
  "Parse Rubin extended designation '_YMxxxx' where _ indicates
extended packed provisional (and year 20nn), Y is a capital letter
starting with P=25...Z=35 (no I), and xxxx is the base-62 designation of the number,
but starting at 15500"
  (declare (type string name))
  (let ((name (if space-trim (string-trim " " name) name)))
    (when  (and (= (length name) 7)
		(char= (aref name 0) #\_)
		;; year is P..Z
		(and (alpha-char-p (aref name 1))
		     (char<= #\P (aref name 1) #\Z))
		;; half-month is A..Y but no I
		(and (alpha-char-p (aref name 2))
		     (char<= #\A (aref name 2) #\Y)
		     (not (char= (aref name 2) #\I)))
		;; last 4 chars
		(alphanumericp (aref name 3))
		(alphanumericp (aref name 4))
		(alphanumericp (aref name 5))
		(alphanumericp (aref name 6)))
      ;; now it is known to be valid
      (let* ((year (+ 2000
		      (+ 25 (- (char-code (aref name 1)) (char-code #\P)))))
	     (half-month-letter (aref name 2))
	     (nth ;; nth in half month
	       (+ 15500 
		  (* (%decode-mpc-number (aref name 6)) 1)
		  (* (%decode-mpc-number (aref name 5)) 62)
		  (* (%decode-mpc-number (aref name 4)) #.(* 62 62))
		  (* (%decode-mpc-number (aref name 3)) #.(* 62 62 62)))))
	;; now the encoding within the half-month is the usual: [A-Z]nnn for 1-25
	;; (omitting I)
	;; and nnn means 'how many times the letters A-Z have rolled over
	(multiple-value-bind (az-rollovers in-half-month)
	    (floor nth 25)
	  (format nil "~D ~A~A~D"
		  year
		  half-month-letter
		  ;; letter A-Z ommiting I, 
		  (aref *az-noi* in-half-month) ;; 'I' omitted!
		  az-rollovers))))))
		     
		 
	      
      



(defun parse-packed-name (desig &key (space-trim t) (allow-all-digits nil))
  "Parse any MPC packed DESIG, including asteroids or comets.  Return
NIL if this was not a valid packed NAME.
Return (VALUES PARSED-NAME :COMET)
         or
       (VALUES PARSED-NAME :ASTEROID)

For asteroids:
 SPACE-TRIM means to trim spaces first.
 ALLOW-ALL-DIGITS means to allow all-digits even not a strict packed designation."
  (let ((tdesig (if space-trim (string-trim " " desig) desig)))
    (let* ((asteroid
	     (parse-packed-asteroid-name
	      tdesig
	      :space-trim nil ;; already did it
	      :allow-all-digits allow-all-digits))
	   (comet (when (not asteroid)
		    (parse-packed-comet-name tdesig))))
      (cond (asteroid
	     (values asteroid :asteroid))
	    (comet
	     (values comet :comet))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packing routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; right-pad a string to 12 spaces, if pad-to-12 is set
(defun %maybe-pad-to-12 (string pad-to-12)
  (if (not pad-to-12)
      string
      (concatenate 'string
		   string
		   (make-string (- 12 (length string))
				:initial-element #\space))))
      
(defun pack-minor-planet-number (number &key (pad-to-12 t))
  "Pack a minor planet number (which may be a digit string).
PAD-TO-12 means space pad to full length of 12."
  (declare (type (or (integer 1 10000000) string) number))

  (let ((num (cond ((integerp number)
		    number)
		   ((stringp number)
		    (ignore-errors (parse-integer number))))))
    (when (and num (plusp num))
      (let ((chars-a-z
              "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
            (chars-0-z
              "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
	(let* ((out-string
		 (cond ((< num 100000)
			(format nil "~5,'0D" num))
		       ((< num 619999)
			(let* ((mod10000 (mod num 10000))
			       (div10000 (floor num 10000))
			       (char (aref chars-a-z  (- div10000 10))))
			  (format nil "~A~4,'0D" char mod10000)))
		       ((< num 15396336) ;; above 619999 but below max representation
			(let* ((n0 (- num 620000))
			       (c4 (aref chars-0-z (mod n0 62)))
			       (n1 (floor n0 62))
			       (c3 (aref chars-0-z (mod n1 62)))
			       (n2 (floor n1 62))
			       (c2 (aref chars-0-z (mod n2 62)))
			       (n3 (floor n2 62))
			       (c1 (aref chars-0-z n3)))
			  ;; yes, "~" char
			  (format nil "~A~A~A~A~A" "~" c1 c2 c3 c4))))))
	  (%maybe-pad-to-12 out-string pad-to-12))))))
	    
;; eg 2P or 2I or 99D
(defun %is-periodic-comet (name)
  (declare (type string name))
  (let ((n (1- (length name))))
    (and (> (length name) 1)
	 (every #'digit-char-p (subseq name 0 n))
	 (<= (parse-integer name :junk-allowed t) 9999)
	 (find (aref name n)  "PDI")
	 t)))

(defun pack-periodic-comet (name &key (pad-to-12 t))
  "Pack a periodic comet like 2P, 1I, 99D (defunct)"
  (declare (type string name))
  (when (%is-periodic-comet name)
    (%maybe-pad-to-12 
     (concatenate 'string
		  ;; ensure total length of 5 by adding zeros
		  (append (make-string (- 5 (length name))
				       :initial-element #\0))
		  name)
     pad-to-12)))
	    
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determine if something is asteroid provisional designation
;; this means   "YYYY XXn.n"
;; Returns (values valid? YYYY XX nnn) where YYYY and nnn are integers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %a-through-z? (c)  ;; capital a through z
  (declare (type character c))
   (<= #.(char-code #\A)
       (char-code c)
       #.(char-code #\Z)))

;; is this a something like 1995 XL1?
;; return (values t 1995 "XL" 1) 
(defun %is-minor-planet-provisional-desig (name)
  (declare (type string name))
  (block ret
    (let ((yyyy nil)
	  (xx nil)
	  (ncyc 0))  ;; number of cycles
	  
      (when (< (length name) 7) (return-from ret nil))
      ;; pattern match "YYYY XXnnn.."
      (loop for i from 0
	    for c across name
	    when (or (and (<= i 3) (not (digit-char-p c))) ;; not YYYY
		     (and (= i 4) (not (eql #\space c))) ;; no space
		     (and (<= 5 i 6) ;; two cap letters
			  (not (%a-through-z? c)))
		     (and (>= i 7) (not (digit-char-p c)))) ;; ending digits
	      do
		 (return-from ret nil))
      (setf yyyy (parse-integer name :end 5))
      (when (not (<= 1800 yyyy 2199))
	(return-from ret nil)) ;; year out of range
      (setf xx   (subseq name 5 7))
      (when (> (length name) 7)
	(setf ncyc   (parse-integer name :start 7)))
      (values t yyyy xx ncyc))))

;; is this something like C/1995XL1 - ie, an initial
;; minor planet desig promoted to a comet?
;; return (values t #\C 1995 "XL" 1) 
(defun %is-minor-planet-style-comet-provisional-desig (name)
  (block ret
    (when (<= (length name) 8)
      (return-from ret nil))
    (when (and (char= (aref name 1) #\/)
	       (member (aref name 0) '(#\C #\A #\P #\X #\D)))
      (multiple-value-bind (is-minor-planet yyyy xx ncyc)
	  (%is-minor-planet-provisional-desig (subseq name 2))
	(when is-minor-planet
	  (values t
		  (aref name 0)
		  yyyy xx ncyc))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determine if something is a comet provisional designation
;; this means   "YYYY Xn..n" or "YYYY Xn..n-F" where X is
;; an uppercase letter and -F is an optional fragment.
;; it may also be preceeded by "C/" "X/" "P/" or "D/"
;; Returns (values valid? comet-char YYYY X nn frag-string)
;;  where YYYY and nn are integers
(defun %is-comet-provisional-desig (name)
  (declare (type string name))
  (block ret
    (let ((name name)
	  (comet-char nil)
	  (yyyy nil)
	  (xchar nil)
	  (num nil)
	  (frag nil)
	  (ystart 0)) ;; where the year starts

      (when (< (length name) 7) (return-from ret nil)) ;; too short
      (when (char= (aref name 1) #\/)
	(setf comet-char (aref name 0))
	(setf ystart 2) ;; year starts here, not at 0
	(when (not (member comet-char '(#\C #\P #\X #\C)))
	  (return-from ret nil)))

      (loop
	with have-frag = nil
	with start-num = 0
	with end-num = 0
	with have-num ;; the number MM after "YYYY XMM"
	for j from ystart below (length name)
	for i from 0
	for c = (aref name j)
	do
	   (cond ((< i 3)
		  (when (not (digit-char-p c))
		    (return-from ret nil))) ;; year bad
		 ((= i 4)
		  (when (not (eql #\space c))
		    (return-from ret nil)) ;; no space after year
		  (setf yyyy (parse-integer name :start ystart
						 :end (+ ystart 4))))
		 ((= i 5)
		  (when (not (%a-through-z? c))
		    (return-from ret nil)) ;; no letter after year
		  (setf xchar c))
		 ((= i 6)
		  (when (not (digit-char-p c)) ;; digit after char
		    (return-from ret nil))
		  (setf start-num j)
		  (setf end-num (1+ j))
		  (setf have-num t))
		 ((and (> i 6) (digit-char-p c))
		  (incf end-num))
		 ;; at this point, moved beyond MM in "YYYY XMMM.."
		 ((and (> i 6) (not have-frag))
		  (when (not (eql c #\-)) ;; when digits end, must have #\-
		    (return-from ret nil)) ;; to designate frag
		  (setf have-frag i)
		  (setf frag (subseq name (1+ j)))
		  (when (or (equalp frag "") ;; no fragment following #\-
			    (notevery #'%a-through-z? frag))
		    (return-from ret nil))))
	finally
	   ;; get the number
	   (when have-num (setf num (parse-integer
				     name
				     :start start-num
				     :end end-num))))

      (values t comet-char yyyy xchar num frag))))		 
		


(defun %encode-year  (yyyy &key (stream nil)) ;; use IJKL for century
  (let ((century-char
	  (cond ((<= 1800 yyyy 1899) #\I)
		((<= 1900 yyyy 1999) #\J)
		((<= 2000 yyyy 2099) #\K)
		((<= 2100 yyyy 2199) #\L))))
    (format stream "~A~2,'0D" century-char  (rem yyyy 100))))

;; once >99, use A-Za-z, eg "101"=>"A1" - largest possible number is
;; 619 ("z1")
(defun %encode-number-2char (n &key (stream nil))
  (multiple-value-bind (tens ones)
      (floor n 10)
    ;; number to high to encode, so fail
    (when (<= tens 61) 
      (format stream "~A~A"
	      (aref  *09azaz* tens)
	      (aref  *09azaz* ones)))))


#+nil
(defun pack-asteroid-provisional-desig (name &key (pad-to-12 t))
  "Pack a provisional designation like 1995 XA ==> J95X00A"
  (multiple-value-bind (valid? yyyy xx ncyc)
      (%is-minor-planet-provisional-desig name)
    (when valid?
      (let ((enc-year (%encode-year yyyy))
	    (cycle-count (%encode-number-2char ncyc)))
	(when (and enc-year cycle-count)
	  (%maybe-pad-to-12
	   (format nil "~A~A~A~A" enc-year (aref xx 0) cycle-count (aref xx 1))
	   pad-to-12))))))

;; given eg year=2026, month+nmonth="CZ", rollovers=6190 (from 
;; "2026 CZ6190", encode into "_QC0aEM" Rubin style designation
(defun %pack-extended-provisional-designation-helper (year month+inmonth rollovers)
  (let* ((year-letter ;; P=25 ... Z=35
	   (code-char (+ (- year 2025) (char-code #\P))))
	 (half-month-letter (aref month+inmonth 0)) 
	 (n-in-month (position (aref month+inmonth 1) *az-noi*))
	 (nth (+ (* rollovers 25) n-in-month))
	 (nth62 (make-string 4))) ;; four digits as packed base 62 string
    ;; encode to base62
    (loop with m = (- nth 15500) ;; start of extended numbering
	  for i from 3 downto 0
	  for digit = (mod m 62)
	  do (setf (aref nth62 i) (aref *09azaz* digit))
	     (setf m (floor (- m digit) 62)))
    (format nil "_~A~A~A" year-letter half-month-letter nth62)))    


(defun pack-asteroid-provisional-desig (name &key (pad-to-12 t))
  "Pack a provisional designation like 1995 XA ==> J95X00A"
  (multiple-value-bind (valid? yyyy xx ncyc)
      (%is-minor-planet-provisional-desig name)
    (when valid?
      (let ((enc-year (%encode-year yyyy))
	    (cycle-count (%encode-number-2char ncyc)))
	(if (and enc-year cycle-count)
	    ;; the old style
	    (%maybe-pad-to-12
	     (format nil "~A~A~A~A" enc-year (aref xx 0) cycle-count (aref xx 1))
	     pad-to-12)
	    ;; the extended Rubin style
	    (when (<= 2025 yyyy 2035) ;; limited validity span
	      (%maybe-pad-to-12
	       (%pack-extended-provisional-designation-helper yyyy xx ncyc)
	       pad-to-12)))))))


(defun pack-comet-provisional-desig (name &key (pad-to-12 t))
  "Pack a provisional comet designation like
  '1995 A1-C' ==> J95A010c
 or
  'C/1995 A1-C' == CJ95A010c
Note that if there is a leading 'C/' then the packed leads with a 'C'.
This will fail for fragments beyond -Z because it seems the packed format
does not support them.

Also, if this fails, try to see if this is an minor planet style
designation, with a C/ (or X,A,P,D) in front of it, which indicates
a provisional minor planet promoted to a comet."
  (or
   ;; try real comets
   (multiple-value-bind (valid? comet-char yyyy xchar num frag)
       (%is-comet-provisional-desig name)
     (when valid?
       (let ((enc-year (%encode-year yyyy))
	     (frag-char (cond ((not frag)
			       #\0)
			      ((= (length frag) 1)
			       (char-downcase (aref frag 0)))
			      (t
			       nil))) ;; can't encode longer frags!
	     (enc-num (%encode-number-2char num)))
	 (when (and enc-year frag-char enc-num)
	   (%maybe-pad-to-12
	    (format nil "~A~A~A~A~A"
		    (if comet-char (format nil "~A" comet-char) "")
		    enc-year
		    xchar
		    enc-num
		    frag-char)
	    pad-to-12)))))
   ;; try minor planets that were promoted to comets
   (multiple-value-bind (valid? comet-char yyyy xx ncyc)
       (%is-minor-planet-style-comet-provisional-desig name)
     (when valid?
       (let ((enc-year (%encode-year yyyy))
	     (cycle-count (%encode-number-2char ncyc)))
	 (when (and enc-year cycle-count)
	   (%maybe-pad-to-12
	    (format nil "~A~A~A~A~A" comet-char enc-year
		    (aref xx 0) cycle-count (aref xx 1))
	   pad-to-12)))))))
		    

(defun pack-desig (name &key (pad-to-12 t))
  "Pack any valid small comet designation.  Return NIL if this is not
valid packable designation."
  (block ret
    (flet ((ret-type (result type) ;; if result is true, return values
	     (when result (return-from ret (values result type)))))
      (progn
	(ret-type (pack-minor-planet-number name :pad-to-12 pad-to-12)
		  :asteroid)
	(ret-type (pack-periodic-comet name :pad-to-12 pad-to-12)
		  :comet)
	(ret-type (pack-asteroid-provisional-desig name :pad-to-12 pad-to-12)
		  :asteroid)
	(ret-type (pack-comet-provisional-desig name :pad-to-12 pad-to-12)
		  :comet)
	nil))))
	     
     


#+nil
(defun %test-asteroid-provis-desig-packing ()
  (let ((examples '(("J95X00A" "1995 XA")
		    ("J95X01L" "1995 XL1")
		    ("J95F13B" "1995 FB13")
		    ("J98SA8Q" "1998 SQ108")
		    ("J98SC7V" "1998 SV127")
		    ("J98SG2S" "1998 SS162")
		    ("K99AJ3Z" "2099 AZ193")
		    ("K08Aa0A" "2008 AA360")
		    ("K07Tf8A" "2007 TA418"))))
    (loop for (packed normal) in examples
	  for packed-test = (pack-asteroid-provisional-desig normal
							     :pad-to-12 nil)
	  for ok = (equalp packed packed-test)
	  do
	     (format t "~A ==> ~A  ~A~%" normal packed (if ok "OK" "ERROR")))))

#+nil
(defun %test-comet-provis-desig-packing ()
  (let ((examples '(("J95A010"   "1995 A1")
		    ("J94P01b"   "1994 P1-B")
		    ("J94P010"   "1994 P1")  
		    ("K48X130"   "2048 X13")
		    ("K33L89c"   "2033 L89-C")
		    ("K88AA30"   "2088 A103"))))
    (loop for (packed normal) in examples
	  for packed-test = (pack-comet-provisional-desig normal
							  :pad-to-12 nil)
	  for ok = (equalp packed packed-test)
	  do
	     (format t "~A ==> ~A  ~A~%" normal packed (if ok "OK" "ERROR")))))

(defparameter *examples*
  '(;; provisional asteroids
    ("J95X00A" "1995 XA" "Provisional minor planet")
    ("J95X01L" "1995 XL1" "Provisional minor planet")
    ("J95F13B" "1995 FB13" "Provisional minor planet")
    ("J98SA8Q" "1998 SQ108" "Provisional minor planet")
    ("J98SC7V" "1998 SV127" "Provisional minor planet")
    ("J98SG2S" "1998 SS162" "Provisional minor planet")
    ("K99AJ3Z" "2099 AZ193" "Provisional minor planet")
    ("K08Aa0A" "2008 AA360" "Provisional minor planet")
    ("K07Tf8A" "2007 TA418" "Provisional minor planet")
    ;; provisonal comets
    ("J95A010"   "1995 A1" "Provisonal comet")
    ("J94P01b"   "1994 P1-B" "Provisonal comet")
    ("J94P010"   "1994 P1" "Provisonal comet")  
    ("K48X130"   "2048 X13" "Provisonal comet")
    ("K33L89c"   "2033 L89-C" "Provisonal comet")
    ("K88AA30"   "2088 A103" "Provisonal comet")
    ;; provisional comets that were originally asteroids
    ("CJ95X00A" "C/1995 XA" "Prov. minor planet turned into asteroid")
    ("AJ95X01L" "A/1995 XL1" "Prov. minor planet turned into asteroid")
    ("XJ95F13B" "X/1995 FB13" "Prov. minor planet turned into asteroid")
    ("DJ98SA8Q" "D/1998 SQ108" "Prov. minor planet turned into asteroid")
    ("PK07Tf8A" "P/2007 TA418" "Prov. minor planet turned into asteroid")
    ;;
    ;; numbered comet
    ("0099P"     "99P"  "Numbered comet")
    ;;
    ;; numbered comet with fragment - this isn't expected to work, but it is what
    ;; packing would do if it were better designed.
    ;;("0099Pa"    "99P-A"  "Numbered comet with fragment - will FAIL")
    ;;
    ;; numbered asteroid
    ("00099"    "99"  "Numbered asteroid")
    ;; high numbered asteroids with encoding
    ("A0345"   "100345"  "High numbered asteroid")
    ("a0017"   "360017"  "High numbered asteroid")
    ("~AZaz"   "3140113" "High numbered asteroid")
    ;;
    ;; extended provisional for Rubin
    ("_QC0000" "2026 CA620" "Extended Provisional (for Rubin)")
    ("_QC0aEM" "2026 CZ6190" "Extended Provisional (for Rubin)")
    ("_QCzzzz" "2026 CL591673" "Extended Provisional (for Rubin)")
    ("_PD0000" "2025 DA620"  "Extended Provisional (for Rubin)")
    ("_QD000N" "2026 DY620"  "Extended Provisional (for Rubin)")
    ("_RD0aEM" "2027 DZ6190"  "Extended Provisional (for Rubin)")
    ("_SEZZZZ" "2028 EA339749"  "Extended Provisional (for Rubin)")
    ("_TFzzzz" "2029 FL591673"  "Extended Provisional (for Rubin)")
    ))


(defun test-packing-examples (&key (examples *examples*))
  (loop with nfail = 0
	for (packed canonical desc) in examples
	for packed-test = (pack-desig canonical
				      :pad-to-12 nil)
	for canonical-test = (parse-packed-name packed)
	  for ok-packing = (equalp packed packed-test)
	for ok-parsing = (equalp canonical canonical-test)
	do
	   (format t "~A  '~A' <==> '~A':~%" desc canonical packed)
	   (format t "  '~A' ==> '~A'  ~A~%" canonical packed-test
		   (if ok-packing "OK" (and (incf nfail) "ERROR")))
	   (format t "  '~A' ==> '~A'  ~A~%" packed canonical-test
		   (if ok-parsing "OK" (and (incf nfail) "ERROR")))
	   (terpri)
	finally
	   (format t "~%Number of failures: ~A~%" nfail)))

    
