
(in-package astro-catalog)

(defun list-objects-near-location (ra dec &key
			      (rmax 2.0)
			      (list-output nil)
			      (catalog-type  'astro-catalog:psps-3pi-mean-psf-mag-catalog))
  "Find the list of stars within RMAX arcsec of RA,DEC (either string
or decimal degree float) by retrieving a catalog.  If LIST-OUTPUT is
set, then return an assoc list.  Otherwise, print results.  The DIST
returned is the distance from RA,DEC given and object position."
  (let* ((ra/deg (if (stringp ra) (ra-dec:hms-string->deg ra) ra))
	 (dec/deg (if (stringp dec) (ra-dec:dms-string->deg dec) dec))
	 (cat 
	   (astro-catalog:get-cached-catalog-object 
	    ra/deg
	    dec/deg
	    (* 1/60 10.0) ;; catalog radius
	    catalog-type))
	 (outlist nil))
    (when (not list-output)
      (format t "Found catalog ~A ~%" cat)
      (terpri)
      (format t
	      "     ID              RA            DEC         dist(\")     g      r      i~%"))
	    
    (loop for i below (astro-catalog:astro-catalog-n cat)
	  for id = (astro-catalog:get-value cat :id i)
	  for rai  =(astro-catalog:get-value cat :ra i)
	  for deci = (astro-catalog:get-value cat :dec i)
	  for rmag = (astro-catalog:get-value cat :r i)
	  for gmag = (astro-catalog:get-value cat :g i)
	  for imag = (astro-catalog:get-value cat :i i)		   
	  for dist = (astro-coords:sky-angle ra/deg dec/deg rai deci
					     :units :arcsec)
	  when (< dist rmax)
	    do
	       (if (not list-output)
		   (format t 
			   "~,3D ~15A ~A   ~A   ~4,1F      ~,2f  ~,2F  ~,2F~%"
			   i
			   id
			   (ra-dec:deg->hms-string rai)
			   (ra-dec:deg->dms-string deci)
			   dist gmag rmag imag)
		   (push `((:id . ,id) (index . ,i) (:ra . ,rai) (:dec . ,deci) (:dist . ,dist)
			   (:g . ,gmag) (:r . ,rmag) (:i . ,imag))
			 outlist)))
    (when outlist
      (setf outlist (sort outlist 
			  '< 
			  :key 
			  (lambda (al) (cdr (assoc :dist al))))))
    outlist))


(defun describe-object-at-index (cat index &key (stream *standard-output*))
  (declare (type astro-catalog cat)
	   (type (integer 0) index))
  (when (not (< index (astro-catalog-n cat)))
    (error "Index ~A is out of range for ~A" index cat))
  (format stream "ID:               ~A~%" (astro-catalog:get-value cat :id index))
  (format stream " RA,DEC (deg):    ~,5F  ~,5F~%"
	  (astro-catalog:get-value cat :ra index)
	  (astro-catalog:get-value cat :dec index))
  (format stream " RA,DEC:          ~A  ~A~%"
	  (ra-dec:deg->hms-string (astro-catalog:get-value cat :ra index))
	  (ra-dec:deg->dms-string (astro-catalog:get-value cat :dec index)))
  (format stream " RA-DEC-ERR [mas]: ~,4F  ~,4F~%"
	  (* 1000 (object-ra-err cat index))
	  (* 1000 (object-dec-err cat index)))
  (multiple-value-bind (pmra pmdec)
      (object-proper-motions cat index)
    (format stream " Motion [mas/hr]: ~,4F  ~,4F~%" pmra pmdec))
  (format stream " Object Type   :  ~A~%" (object-type cat index))
  (format stream " Magnitudes and errors:~%")
  (loop for mag-name in (available-mags cat)
	do (multiple-value-bind (mag emag)
	       (object-mag cat mag-name index) 
	     (format stream "    ~A ~,3F  +/- ~,3F~%"
		   mag-name mag emag))))
	
  
  
  
  
  
