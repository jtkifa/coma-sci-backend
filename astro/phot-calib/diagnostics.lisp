
#|

Some diagnostics:

   plot-calibration-match-file - plot a match file that is output by various calibration routines

   catalog-vs-catalog-comparison - compare two catalogs against each other
 

|#


(in-package phot-calib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plotting with cl-cairoplot

(defun open-plot-device (device filename &key (size 600) (aspect 1)
					      (title "phot-calib"))
  "Open a cl-cairoplot device.  DEVICE is a file type (:PDF :PS :SVG
:PNG), which needs FILENAME, or a screen device (:GTK, or the old PGPLOT
names :X11 :XWINDOW :XW), which opens a window called TITLE."
  (case device
    ((:gtk :x11 :xwindow :xw)
     (cairoplot:open-device :gtk title :size size :aspect aspect))
    ((:pdf :ps :svg :png)
     (unless filename
       (error "Plot device ~S needs a FILENAME." device))
     (cairoplot:open-device device filename :size size :aspect aspect))
    (t
     (error "Unknown plot device ~S: use :PDF :PS :SVG :PNG or :GTK."
	    device))))

(defun subscripted (&rest parts)
  "Text runs for cl-cairoplot from PARTS, strings in which a list
 (\"text\") is a subscript."
  (loop for part in parts
	collect (if (consp part)
		    (list (first part) :scale 0.6 :rise -0.25)
		    part)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plot-calibration-match-file
    (match-csv-file &key (device :gtk) (filename nil)
		      (zp-mag nil) ;; if given plot it
		      (dmag-span 0.25) (mag-min nil) (mag-max nil)
		      (toplabel nil))
  "Plot a file with fields 
 OBJ_ID OBJ_MAG OBJ_MAG_ERR   CAT_ID CAT_MAG CAT_MAG_ERR DMAG DMAG_ERR  RA DEC

DEVICE is :PDF, :PS, :SVG or :PNG, written to FILENAME, or :GTK (or :X11)
for a window."
  (let* ((h (csv-read:read-csv-headers/columns-from-file match-csv-file
							 :comment-char #\#))
	 (catmag (gethash "CatMag" h)) 
	 (dmag   (gethash "Dmag" h)) 
	 (dmag-err (gethash "DmagErr" h)) 
	 (dmag-med (stats:median-of-elements dmag))
	 (mag-min (or mag-min (+ -0.5 (stats:min-of-elements catmag))))
	 (mag-max (or mag-max (+ +0.5 (stats:max-of-elements catmag))))
	 (dotted '(1.5 3))
	 (p nil))

    (unwind-protect ;; if this bombs make sure we close device,
		    ;; because this might be run in an automated loop
	 (progn
	   (setf p (open-plot-device device filename :size 600 :aspect 1
						     :title (or toplabel "Calibration")))
	   (let ((panels (cairoplot:make-panel-grid p 2 1 :left 0.15 :right 0.95
							  :bottom 0.09 :top 0.93
							  :hspace 0)))
	     (flet ((make-plot (vp mspan label &key (labels-bottom t))
		      (cairoplot:set-window vp mag-min mag-max
					    (- dmag-med mspan) (+ dmag-med mspan))
		      (when zp-mag 
			(cairoplot:with-style (vp :color :red :dash dotted)
			  (cairoplot:plot-line vp mag-min zp-mag mag-max zp-mag)))
		      (cairoplot:with-style (vp :color :light-gray :dash dotted)
			(cairoplot:plot-line vp mag-min dmag-med mag-max dmag-med))
		      (cairoplot:plot-error-bars vp catmag dmag :yerr dmag-err
								:cap-size 0)
		      (cairoplot:plot-points vp catmag dmag :marker :filled-circle)
		      (cairoplot:draw-box
		       vp
		       :numbers (if labels-bottom '(:bottom :left) '(:left))
		       :omit-end-numbers (if labels-bottom nil '(:y-low))
		       :x-label (when labels-bottom "Reference Magnitude")
		       :y-label (subscripted "M" '("ref") " - M" '("image"))
		       :title (unless labels-bottom toplabel))
		      (cairoplot:draw-panel-label vp label :position :upper-right)
		      (when labels-bottom ;; only bottom panel
			(cairoplot:draw-legend
			 vp
			 `((,(format nil "Median Mag ~,3F" dmag-med)
			    :line t :dash ,dotted :color :light-gray)
			   (,(if zp-mag
				 (format nil "ZP Mag ~,3F" zp-mag)
				 "ZP Mag NA")
			    :line t :dash ,dotted :color :red))))))
	       ;;
	       (make-plot (aref panels 1 0) dmag-span       "Zoom view"  :labels-bottom t)
	       (make-plot (aref panels 0 0) (* 7 dmag-span) "Broad view" :labels-bottom nil))))
      ;;
      (when p (cairoplot:close-device p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; catalog vs catalog comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun catalog-vs-catalog-comparison (catalog-ref catalog filter
				      &key (matches-file nil)
				      (extra-error 0.01d0)
				      ;; parameters of bayes-outlie
				      (f 0.1d0) (s 0.15d0))
  "Convert CATALOG to CATALOG-REF's filter, and compute the offset"
  (let* ((cat1 catalog-ref)
	 (cat2 catalog)
	 (match-indices ;; in cat2
	   (aobj:match-ra-dec-by-index  
	    (astro-catalog:get-astro-catalog-vector cat1 :ra)
	    (astro-catalog:get-astro-catalog-vector cat1 :dec)
	    (astro-catalog:get-astro-catalog-vector cat2 :ra)
	    (astro-catalog:get-astro-catalog-vector cat2 :dec)))
	 (transfunc1 ;; translation function from cat1 to filter 
	   (get-mag-trans-func-for-catalog cat1 filter))
	 (transfunc2 ;; translation function from cat1 to filter 
	   (get-mag-trans-func-for-catalog cat2 filter))
	 (ngood
	   (loop for i1 from 0
		 for i2 across match-indices
		 when (and (plusp i2)
			   (funcall transfunc1 cat1 i1)
			   (funcall transfunc2 cat2 i2))
		   count 1))
	 (ra-vec (make-array ngood))
	 (dec-vec (make-array ngood))
	 (id1-vec (make-array ngood))
	 (mag1-vec (make-array ngood))
	 (magerr1-vec (make-array ngood))
	 (id2-vec (make-array ngood))
	 (mag2-vec (make-array ngood))
	 (magerr2-vec (make-array ngood))
	 (dmag-vec (make-array ngood))
	 (dmagerr-vec (make-array ngood)))

    (loop with k = 0
	  for ra across (astro-catalog:get-astro-catalog-vector cat1 :ra)
	  for dec across (astro-catalog:get-astro-catalog-vector cat1 :dec)
	  for i1 from 0
	  for i2 across match-indices
	  when (plusp i2)
	    do
	       (multiple-value-bind (m1 dm1)
		   (funcall transfunc1 cat1 i1)
		 (when m1
		   (multiple-value-bind (m2 dm2) 
		       (funcall transfunc2 cat2 i2)
		     (when m2 
		       (setf (aref ra-vec      k) ra)
		       (setf (aref dec-vec     k) dec)
		       (setf (aref id1-vec     k) (astro-catalog:get-value cat1 :id i1))
		       (setf (aref id2-vec     k) (astro-catalog:get-value cat2 :id i2))
		       (setf (aref mag1-vec    k) m1
			  (aref magerr1-vec k) dm1
			  (aref mag2-vec    k) m2
			  (aref magerr2-vec  k) dm2)
		       (setf (aref dmag-vec  k) (- m1 m2))
		       (setf (aref dmagerr-vec k) 
			     (sqrt (+ (expt extra-error 2) (expt dm1 2) (expt dm2 2))))
		       (incf k))))))

    (when matches-file
      (with-open-file (sout matches-file 
			    :direction :output 
			    :if-does-not-exist :create
			    :if-exists :supersede)
	(format sout "# Matches from catalog/catalog calib
# 1=~A; 2=~A;   Dmag=Mag2-Mag1
#
"
		(type-of catalog) (type-of catalog-ref))
	(format 
	 sout
"# CatID1                CatMag1  CatMagErr1      CatID2                  CatMag2  CatMagErr2     Dmag     DmagErr   RA       DEC~%")
	(loop for ra across ra-vec and dec across dec-vec 
	      for id1 across id1-vec and id2 across id2-vec
	      for m1 across mag1-vec for merr1 across magerr1-vec
	      for m2 across mag2-vec for merr2 across magerr2-vec
	      for dm across dmag-vec for dmerr across dmagerr-vec
	      do (format 
		  sout 
 "~20A   ~7,3F   ~7,3F         ~20A   ~7,3F   ~7,3F      ~7,3F   ~7,3F ~9,5F ~9,5F~%"
   id2 m2 merr2 ;; cat2 is the one one being calibrated
   id1 m1 merr1
   dm dmerr
   ra dec))))

    (let* ((outlier-result (bayes-outlier:bayes-outlier-estimate-mean 
			    dmag-vec dmagerr-vec :f f :s s))
	   (dmag (bayes-outlier:outlier-result-xm outlier-result))
	   (dmag-err (bayes-outlier:outlier-result-xm-err outlier-result)))


      (values dmag dmag-err ngood outlier-result))))
	     

    
