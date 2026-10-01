;;;; astorb-query.lisp - Query functions for astorb database

(in-package :astorb)

;;; ============================================================================
;;; Global Accessor
;;; ============================================================================

(defun get-the-astorb ()
  "Get the global astorb database, with locking and error check."
  (with-astorb-lock
    (or *the-astorb*
        (error "Variable *THE-ASTORB* not set; astorb database not loaded."))))


;;; ============================================================================
;;; Date Conversion
;;; ============================================================================

(defun %mjd-from-astorb-date (yyyymmdd)
  "Convert YYYYMMDD integer to MJD."
  (multiple-value-bind (year mmdd)
      (floor yyyymmdd 10000)
    (multiple-value-bind (month day)
        (floor mmdd 100)
      (+ (astro-time:calendar-date-to-mjd year month day 0 0 0)))))


;;; ============================================================================
;;; Comet Element Conversion
;;; ============================================================================


(defun get-universal-elem-for-nth-asteroid (n &key (astorb (get-the-astorb)))
  "For Nth asteroid (0 indexed) in astorb database, return a SLALIB-EPHEM:UNIVERAL-ELEM,
after converting asteroidal orbit to cometary."
  (declare (optimize speed))
  (let ((table (astorb-table astorb))
        (rec (make-asteroid-orbit-rec))
	(velem (make-array 13 :element-type 'double-float)))
    (declare (dynamic-extent rec))
    ;; Load all fields at once using generated loader (faster than individual peeks)
    (mmapped-table:load-row table n :fill-obj rec)
    (let* ((epoch-osc (asteroid-orbit-rec-epoch-osc rec))
           (epoch-osc-mjd (%mjd-from-astorb-date epoch-osc))
           (mean-anomaly (asteroid-orbit-rec-mean-anomaly rec))
           (arg-peri (asteroid-orbit-rec-arg-peri rec))
           (anode (asteroid-orbit-rec-anode rec))
           (orbinc (asteroid-orbit-rec-orbinc rec))
           (ecc (asteroid-orbit-rec-ecc rec))
           (a (asteroid-orbit-rec-a rec))
           (name (asteroid-orbit-rec-name rec))
           (ast-num (asteroid-orbit-rec-astnum rec))
           (fullname (if (zerop ast-num)
                         name
                         (format nil "(~D) ~A" ast-num name)))
	   (iras-km  (asteroid-orbit-rec-iras-km rec))
           (dm 0d0)
	   ;;
	   (data
	     (orbital-elements:make-asteroid-desc
              :name fullname
              :number (if (plusp ast-num) ast-num)
              :source "astorb"
              :h (asteroid-orbit-rec-hmag rec)
              :g (asteroid-orbit-rec-g rec)
              :radius (if (and iras-km (plusp iras-km))
                          (* 0.5 iras-km)
                          nil)
              :period nil
              :albedo nil)))
      ;; Convert asteroid elements to universal elements - this fills VELEM
      (slalib:sla-el2ue epoch-osc-mjd
                      2  ;; jform=2 => asteroid orbit
                      epoch-osc-mjd
                      (* (/ pi 180) orbinc)
                      (* (/ pi 180) anode)
                      (* (/ pi 180) arg-peri)
                      a ecc
                      (* (/ pi 180) mean-anomaly)
                      dm velem)
      (orbital-elements:make-univ-elem :id fullname :velem velem :data data))))



(defun get-comet-elem-for-nth-asteroid (n &key (astorb (get-the-astorb)))
  "For Nth asteroid (0 indexed) in astorb database, return a SLALIB-EPHEM:COMET-ELEM,
after converting asteroidal orbit to cometary."
  (declare (optimize speed))
  (let* ((ue (get-universal-elem-for-nth-asteroid n :astorb astorb))
	 (velem (orbital-elements:univ-elem-velem ue))
	 (epoch-osc (aref velem 11))) ;; slalib UE format
    (multiple-value-bind (time-peri orbinc anode perih aorq e aorl dm)
        (slalib:sla-ue2el (orbital-elements:univ-elem-velem ue) 3)
      (declare (ignore aorl dm))
      (orbital-elements:make-comet-elem
        :id (orbital-elements:univ-elem-id ue)
        :epoch epoch-osc
        :time-peri time-peri
        :orbinc (* orbinc (/ 180 pi))
        :anode (* anode (/ 180 pi))
        :perih (* perih (/ 180 pi))
        :q aorq
        :e e
        :data (orbital-elements:univ-elem-data ue)))))


;;; ============================================================================
;;; String Search Utilities
;;; ============================================================================

(defun %stringsearch (s1 s2)
  "Is S1 contained in S2?"
  (declare (type simple-string s1 s2)
           (optimize speed))
  (block done
    (loop
      with c0 = (aref s1 0)
      for i of-type (signed-byte 28) below (1+ (- (length s2) (length s1)))
      when (char= c0 (aref s2 i))
        do
           (loop
             for j from 1 below (length s1)
             when (not (char= (aref s1 j) (aref s2 (+ i j))))
               do (return)
             finally (return-from done t))
      finally (return-from done nil))))


;;; ============================================================================
;;; Name Search
;;; ============================================================================

(defun search-for-asteroids-by-name (name &key (astorb (get-the-astorb))
                                            (match-type :substring))
  "Return a list of asteroid indices that match name, and a list of the names.

MATCH-TYPE can be:
    :SUBSTRING - the name is contained inside the true name, case insensitive.
    :EXACT     - the name is an exact but case insensitive match

In both instances, names are lowercased and have non-alphanumeric chars removed."
  (declare (type string name)
           (type (member :substring :exact) match-type)
           (optimize speed))

  (when (= (length name) 0)
    (error "Zero length name"))

  (let ((table (astorb-table astorb))
        (n (astorb-n astorb)))
    (loop
      with the-name of-type string = (%scrub-string name)
      for i of-type (unsigned-byte 28) from 0 below n
      for ast-sname = (mmapped-table:table-peek table i 'sname)
      when (and ast-sname
                (> (length ast-sname) 0)
                (cond ((eq match-type :exact)
                       (string= ast-sname the-name))
                      ((eq match-type :substring)
                       (%stringsearch the-name ast-sname))))
        collect i into indices
        and collect (mmapped-table:table-peek table i 'name) into names
      finally (return (values indices names)))))


;;; ============================================================================
;;; Numbered Asteroid Lookup
;;; ============================================================================

(defun find-numbered-asteroid (n &key (astorb (get-the-astorb)))
  "Return the astorb index for numbered asteroid N.
Returns NIL if not found."
  (let* ((table (astorb-table astorb))
         (k (1- n))
         (nrows (astorb-n astorb)))
    (when (and (<= 0 k (1- nrows))
               (= (mmapped-table:table-peek table k 'astnum) n))
      k)))


;;; ============================================================================
;;; Test Function
;;; ============================================================================

(defun %test-astorb-on-juno (&key
                               (astorb (get-the-astorb))
                               (mjd (astro-time:calendar-date-to-mjd 2017 02 03 0 0 0))
                               (jpl-ra 269.67695d0)
                               (jpl-dec -12.53366d0))
  "Verify that Juno is predicted correctly against JPL values."
  (let ((elem-juno (get-comet-elem-for-nth-asteroid 2 :astorb astorb)))
    (multiple-value-bind (ra dec)
        (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
         elem-juno
         mjd
         "uh88"
         :perturb t)
      (let ((dra (* 3600 (abs (- jpl-ra ra))))
            (ddec (* 3600 (abs (- jpl-dec dec)))))
        (when (or (> dra 0.5)
                  (> ddec 0.5))
          (error
           "ASTORB predicted Juno position does not match JPL value.
      ra-JPL=~,6F     ra-pred=~,6F    err=~,5F
      dec-JPL=~,6F   dec-pred=~,6F    err=~,5F
   Is MJD of coords valid?"
           jpl-ra ra dra
           jpl-dec dec ddec))))))
