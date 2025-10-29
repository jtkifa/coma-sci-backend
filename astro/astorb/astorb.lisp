


(in-package astorb)



(defvar *astorb-lock* (bordeaux-threads:make-recursive-lock "astorb-lock"))
(defmacro with-astorb-lock (&body body)
  `(bordeaux-threads:with-recursive-lock-held (*astorb-lock*)
     ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get the astorb, but with locking (to allow hotswap), and errorcheck
(defun get-the-astorb ()
  (with-astorb-lock
    (or *the-astorb*
	(error "variable *the-astorb* not set; astorb database not loaded."))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  
;; define a struct with NAME, with a write for it, name NAME-writer
(defmacro defstruct-with-writer (name &rest slots)
  `(progn
     (defstruct ,name ,@slots)
     (defun ,(intern (string-upcase (format nil "~A-writer" name))) (obj)
       (list
	',(intern (string-upcase (format nil "MAKE-~A" name)))
	 ,@(loop with outlist = nil
		 for slot in slots
		 for slot-name = (first slot)
		 for slot-keyword = (intern (string-upcase slot-name) :keyword)
		 for slot-accessor = (intern
				      (string-upcase (format nil "~A-~A" name slot-name)))
		 do (push slot-keyword outlist)
		    (push `(,slot-accessor obj) outlist)
		 finally (return (reverse outlist)))))))

(defstruct-with-writer astorb
  (astorb-file nil)  
  ;; the epoch of the elements which we put into the file as a rough book-keeping
  ;; notice - the actual mjd of osculation is in each orbit
  (epoch-of-elements 0d0 :type double-float)
  (n 0 :type (unsigned-byte 32)) ;; number of elements
  ;;
  ;;
  ;; asteroid number, or 0 for no number
  (astnum (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;; name (ascii), or NIL
  (name (make-array 0) :type (simple-array t (*)))
  ;; simplified name with whitespace removed, lowercase (ascii), or NIL
  (sname (make-array 0) :type (simple-array t (*)))
  ;; H mag
  (hmag (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; slop param G
  (g (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; IRAS size
  (iras-km
   (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; IRAS classification (single letter as string, possibly with question mark)
  (iras-class
   (make-array 0 :element-type 't) :type (simple-array t (*)))
  ;; codes as described in doc
  (code1 (make-array 0 :element-type '(unsigned-byte 8))
	 :type (simple-array(unsigned-byte 8) (*)))
  (code2 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code3 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code4 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code5 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code6 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  ;; orbital arc in days
  (orbarc
   (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; number of observations for arc
  (nobs (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;; epoch of osculation YYYYMMDD - add 0.5
  (epoch-osc (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;; mean anomaly, deg
  (mean-anomaly  (make-array 0 :element-type 'double-float) 
		 :type (simple-array double-float (*)))
  ;; argument of perihelion
  (arg-peri  (make-array 0 :element-type 'double-float) 
	     :type (simple-array double-float (*)))
  ;; long of ascending node, deg
  (anode  (make-array 0 :element-type 'double-float) 
	  :type (simple-array double-float (*)))
  ;; orbital inclination
  (orbinc  (make-array 0 :element-type 'double-float) 
	   :type (simple-array double-float (*)))
  ;; eccentricity
  (ecc  (make-array 0 :element-type 'double-float) 
	   :type (simple-array double-float (*)))
  ;; semimajor axis
  (a  (make-array 0 :element-type 'double-float) 
	 :type (simple-array double-float (*)))
  ;; date of orbit computation, YYYYMMDD
  (orbit-date  (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;;
  ;; there are higher fields, but we ignore them for now 
  )


;; buil an astorb structure of size N
(defun build-astorb (n)
  (flet ((afixnum () (make-array n :element-type 'fixnum :initial-element 0))
	 (ageneric () (make-array n :element-type 't :initial-element nil))
	 (afloat () (make-array n :element-type 'single-float :initial-element 0.0))
	 (adbl () (make-array n :element-type 'double-float :initial-element 0d0))
	 (a8bit () (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
    (make-astorb 
     :astnum (afixnum)
     :n n
     :name (ageneric) :sname (ageneric)
     :hmag (afloat)
     :g (afloat)
     :iras-km (afloat)
     :iras-class (ageneric)
     :code1 (a8bit)  :code2 (a8bit)  :code3 (a8bit)
     :code4 (a8bit)  :code5 (a8bit)  :code6 (a8bit)
     :orbarc (afloat)
     :nobs (afixnum)
     :epoch-osc (afixnum)
     :mean-anomaly (adbl)
     :arg-peri (adbl)
     :anode (adbl)
     :orbinc (adbl)
     :ecc (adbl)
     :a (adbl)
     :orbit-date (afixnum))))
	    
  
;; see if Juno is where it is supposed to be
(defun %test-astorb-on-juno  (astorb &key 
			      (mjd (astro-time:calendar-date-to-mjd 2017 02 03 0 0 0))
			      (jpl-ra 269.67695d0)
			      (jpl-dec -12.53366d0))
  (let ((elem-juno (get-comet-elem-for-nth-asteroid 2 :astorb astorb)))
    (multiple-value-bind (ra dec)
	(slalib-ephem:compute-radecr-from-comet-elem-for-observatory  
	 elem-juno
	 mjd
	 "uh88"
	 :perturb t)
      (let ((dra  (* 3600 (abs (- jpl-ra ra))))
	    (ddec (* 3600 (abs (- jpl-dec dec)))))
	(when (or (> dra 0.5)
		  (> ddec 0.5))
	  (error 
	   "ASTORB predicted Juno position does not match JPL value. 
      ra-JPL=~,6F     ra-pred=~,6F    err=~,5F
      dec-JPL=~,6F   dec-pred=~,6F    err=~,5F
   Is MJD of coords valid?"
	   jpl-ra ra   dra
	   jpl-dec dec ddec))))))






	
	 





;; convert YYYYMMMDD to MJD
(defun %mjd-from-astorb-date (yyyymmdd)
  (multiple-value-bind (year mmdd)
      (floor yyyymmdd 10000)
    (multiple-value-bind (month day)
	(floor mmdd 100)
      (+ (astro-time:calendar-date-to-mjd year month day 0 0 0)))))


(defun get-comet-elem-for-nth-asteroid (n &key (astorb (get-the-astorb)))
  "For Nth asteroid (0 indexed) in astorb database, return a SLALIB-EPHEM:COMET-ELEM,
after converting asteroidal orbit to cometary."
  (let* ((epoch-osc (aref (astorb-epoch-osc astorb) n))
	 (epoch-osc-mjd (%mjd-from-astorb-date epoch-osc))
	 (mean-anomaly (aref (astorb-mean-anomaly astorb) n))
	 (arg-peri (aref (astorb-arg-peri astorb) n))
	 (anode (aref (astorb-anode astorb) n))
	 (orbinc (aref (astorb-orbinc astorb) n))
	 (ecc (aref (astorb-ecc astorb) n))
	 (a (aref (astorb-a astorb) n))
	 (name (aref (astorb-name astorb) n))
	 (ast-num (aref (astorb-astnum astorb) n))
	 (fullname (if (zerop ast-num)
		       name (format nil "(~A) ~A"
				    ast-num name)))
	 (dm 0d0) ;; ignored
	 (velem (make-array 13 :element-type 'double-float)))
    ;; first convert asteroid to universal elements
    (slalib:sla-el2ue epoch-osc-mjd
		      2 ;; jform=2 => asteroid orbit
		      epoch-osc-mjd
		      (* (/ pi 180) orbinc)
		      (* (/ pi 180) anode)
		      (* (/ pi 180) arg-peri)   
		      a ecc  
		      (* (/ pi 180) mean-anomaly) 
		      dm velem)
    ;; then convert universal elem to comet elem
    (multiple-value-bind (epoch orbinc anode perih aorq e aorl dm)
	(slalib:sla-ue2el velem 3)
      (declare (ignore aorl dm))
      (slalib-ephem:make-comet-elem 
	 :id fullname 
	 :epoch epoch-osc-mjd 
	 :time-peri epoch
	 :orbinc (* orbinc (/ 180 pi))
	 :anode  (* anode (/ 180 pi))
	 :perih  (* perih (/ 180 pi))
	 :q      aorq 
	 :e      e
	 :data (slalib-ephem::make-asteroid-desc
		:name fullname
		:number (if (plusp ast-num) ast-num)
		:source "Astorb"
		:h  (aref (astorb-hmag astorb) n)
		:g  (aref (astorb-g astorb) n)
		:radius  (if (aref (astorb-iras-km astorb) n)
			     (* 0.5 (aref (astorb-iras-km astorb) n))
			     nil)
		:period nil
		:albedo nil   )))))

;; is s1 in s2? 
(defun %stringsearch (s1 s2)
  (declare (type simple-string s1 s2)
	   (optimize speed))
  (block done
    (loop
       with c0 = (aref s1 0)
       for i of-type (signed-byte 28) below (1+ (- (length s2) (length s1)))
       when (char= c0 (aref s2 i))
       ;; test remainder of string after zeroth char
       do
	 (loop 
	    for j from 1 below (length s1)
	    when (not (char= (aref s1 j) (aref s2 (+ i j))))
	    do (return) ;; quit this loop
	    finally (return-from done t)) ;; match
       finally (return-from done nil))))
	    

(defun search-for-asteroids-by-name (name &key (astorb (get-the-astorb)) 
				     (match-type :substring))
  "Return a list of asteroid indices that match name, and a list of the names

MATCH-TYPE can be 
    :SUBSTRING - the name is contained inside the true name, but case insensitive.
    :EXACT     - the name is an exact but case insensitive match

In both instances, both names are lowercased and have non-alphanumeric chars removed"
  (declare (type string name)
	   (type (member :substring :exact) match-type)
	   (optimize speed))

  (when (= (length name) 0) (error "Zero length name"))
  
  (loop 
     ;; the-name is lowercase, with whitespace removed
     with the-name of-type string = (%scrub-string name)
     for  i of-type (unsigned-byte 28) from 0       
     for ast-name of-type string across (astorb-sname astorb)
     when (and
	   ast-name
	   (cond ((eq match-type :exact)
		  (string= ast-name the-name)) ;; both are lowercase
		 ((eq match-type :substring)
		  (%stringsearch  the-name ast-name))))
     collect i into indices and collect (aref (astorb-name astorb) i) into names
     finally (return (values indices names))))

(defun find-numbered-asteroid (n  &key (astorb (get-the-astorb)))
  "Return the astorb index for numbered asteroid N"
  (let ((k (1- n)))
    (when
	(and (<= 0 k (1- (length (astorb-astnum astorb))))
	     (= (aref (astorb-astnum astorb) k) n))
      k)))

 






    
				   

