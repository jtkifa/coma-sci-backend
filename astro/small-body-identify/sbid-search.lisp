

(in-package small-body-identify)

;; set four worker threads if a kernel is not set
(eval-when (:load-toplevel)
  (when (not lparallel:*kernel*)
    (setf lparallel:*kernel* (lparallel:make-kernel 4))))


(defstruct candidate
  id
  dist ;; arcsec
  elem ;; comet elements
  index ;; in our array
  mjd observatory ;; when ra,dec,delta,dra/dt,ddec/dt hold
  ra
  dec
  delta
  dra/dt
  ddec/dt)


;; a snapshot in time - each should consume roughly 12 MB for 1M objects
(defstruct sbid-snap
  (mjdtt 0d0 :type double-float)
  (observatory nil)
  ;; a hash table of perturbed orbits for nearby objects, so we compute
  ;; perturbation only once.  Certain orbits for nearby objects are tested
  ;; each time, so it's good to perturb only once.
  (perturbed-orbits (make-hash-table :test 'equal))
  (perturbed-orbits-lock (bordeaux-threads:make-lock "perturbed-orbit-hash-lock"))
  ;;
  (ra-vec  nil :type (or null (simple-array single-float (*))))
  (dec-vec nil :type (or null (simple-array single-float (*))))
  (delta-vec nil :type (or null (simple-array single-float (*)))) ;; AU
  (flag-vec nil  :type (or null (simple-array bit (*)))) ;; 0 flag =
  ;; good value kdtrees for objects with various speeds according to
  ;; naive rule of %max-daily-motion
  (kdtree-0-3  ;; this will have 99% of points
   (kdtree-jk/latlon:build-kdtree-latlon
    :npoints 10000))
  ;; kdtree for objects from 3 to 5 deg/day
  (kdtree-3-5 ;; 1% of points
   (kdtree-jk/latlon:build-kdtree-latlon
    :npoints 10000))
  ;; kdtree for objects 5 to 15 deg/day
  (kdtree-5-15 ;; 1% of points
   (kdtree-jk/latlon:build-kdtree-latlon
    :npoints 10000))
  ;;
  ;; list of object indexes moving faster than 5 deg/day
  (fast-movers nil)) ;; 0.1% of points
  
  

;; sbid's memory, made up of snapshots
(defstruct sbid-mem
  (sbid-snap-list nil)
  (lock (bordeaux-threads:make-lock "sbid-mem-lock"))
  (nsnaps-max 10))



;; get an orbit perturbed to the SNAP's MJD, possibly retrieving it
;; from an internal hash table; minor innaccuracy: perturbing
;; to mjdtt instead of mjd
(defun %get-perturbed-orbit (elem snap)
  (declare (type slalib-ephem:comet-elem elem)
	   (type sbid-snap snap))
  (bordeaux-threads:with-lock-held ((sbid-snap-perturbed-orbits-lock snap))
    (or
     (gethash (slalib-ephem:comet-elem-id elem)
	      (sbid-snap-perturbed-orbits snap))
     (setf
      (gethash (slalib-ephem:comet-elem-id elem)
	       (sbid-snap-perturbed-orbits snap))
      (or
       ;; sometimes perturbing an extreme orbit can fail numerically so we just
       ;; fall back on the original orbit on failure
       (ignore-errors 
	(slalib-ephem:perturb-comet-elem  (sbid-snap-mjdtt snap) elem))
       elem)))))





;; utility function that takes a task indexed i1..i2, and
;; calls (func j1 j2) in parallel to span ranges of i1,i2, and returns
;; a sorted list of the return values
(defun chop-up-indexed-tasks-into-threads (i1 i2 func &key (nthreads nil))
  (loop with ncpu = (or nthreads (lparallel:kernel-worker-count))
	with ntot = (- i2 i1)
	with nblock = (ceiling ntot ncpu) ;; number in a block
	with ithread = 0
	with channel = (lparallel:make-channel)
	;; function called in each thread returns its thread id
	with channel-func = (lambda (ith i j)  (list ith (funcall func i j)))
	for j1 = i1 then (+ 1 j1 nblock)
	for j2 = (min (+ j1 nblock) i2)
	do (lparallel:submit-task channel channel-func				   
				  ithread j1 j2)
	   (incf ithread)
	until (= j2 i2)
	finally
	   (return
	     (loop for k below ithread
		   collect (lparallel:receive-result channel)
		     into outlist
		   finally
		      (return (mapcar 'second
				      (sort outlist '< :key 'car)))))))



;; is the orbit too risky to use unperturbed?
(defun unperturbed-orbit-is-risky (elem mjdtt)
  (declare (type orbital-elements:comet-elem elem)
	   (type double-float mjdtt))
  (or
   ;; a nearby orbit (small perilehion q)
   (and (> (abs (- mjdtt (orbital-elements:comet-elem-epoch elem))) 10) ;; orbit epoch 10 days off
	(< (orbital-elements:comet-elem-q elem) 1.4))
   ;; any others risky?
   ))

;; fixme - maybe cache perturbed risky orbits in a global cache?
(defun perturb-if-risky (elem mjdtt)
  (if (unperturbed-orbit-is-risky elem mjdtt)
      ;; allow for perturb failure
      (or (ignore-errors (slalib-ephem:perturb-comet-elem mjdtt elem))
	  elem)
      ;; otherwise return original orbit
      elem))


;; builds range i1 to i1 inside snap - to be run inside one thread
(defun %build-sbid-snap-range (i1 i2 ccomp snap orbit-element-vector)
  (declare (type fixnum i1 i2)
	   (type ccomp ccomp)
	   (type sbid-snap snap)
	   (type (simple-array T (*))  orbit-element-vector)
	   (optimize speed))
  (loop with mjdtt = (sbid-snap-mjdtt snap)
	with ra-vec = (sbid-snap-ra-vec snap)
	with dec-vec = (sbid-snap-dec-vec snap)
	with delta-vec = (sbid-snap-delta-vec snap)
	with flag-vec = (sbid-snap-flag-vec snap)
	with 3vscr = (make-array 3 :element-type 'double-float)
	with pvscr = (make-array 6 :element-type 'double-float)
	for i from i1 to i2
	for elem-raw = (aref orbit-element-vector i)
	;; we perturb the orbit if it is risky - a rough orbit isn't good enough for some
	;; NEOs, even for the first estimate
	for elem = (perturb-if-risky elem-raw mjdtt)
	when elem
	do (multiple-value-bind (ra dec delta)
	       (ignore-errors
		(compute-ra-dec-delta-using-ccomp ccomp elem
						  :3vec-scratch 3vscr
						  :pv-scratch pvscr))
	     (if ra
		 (progn
		   (setf (aref ra-vec i) ra)
		   (setf (aref dec-vec i) dec)
		   (setf (aref delta-vec i) delta)
		   (setf (aref flag-vec i) 0))  ;; valid value
		 (progn ;; else an error occurred
		     (setf (aref ra-vec i) 0.0)
		     (setf (aref dec-vec i) 0.0)
		     (setf (aref delta-vec i)   0.0)
		     (setf (aref flag-vec i) 1)))))) ;; bad value


;; the fastest an object might be moving based on its distance from
;; observer and eccentricity, based on a very conservative
;; interpretation of observed motions.  in reality, we see only 4
;; objects moving faster than 5 deg/day, and they're all within 0.15
;; AU of observer.  See utils/plot-motions.lisp and output pdf.
(declaim (inline %max-daily-motion))
(defun %max-daily-motion (delta elem) ;; return NIL or motion in deg/day
  (let ((ecc (orbital-elements:comet-elem-e elem))
	(inc (orbital-elements:comet-elem-orbinc elem)))
    
    (cond
      ;; first, allow weird orbits relatively nearby to move very fast
      ((and (<= 1.0 delta 2.0)  
	    (or (> ecc 0.4) ;; high eccentricity objects can move faster (see it at e=0.60)
		(> (abs inc) 90))) ;; high inc. orbits can be retrograde
       10.0) ;; we saw ONE that had 6 degree/day of motion, at 1 AU
      ;; the weird objects, even closer to earth
      ((and (<= 0.4 delta 1.0)  
	    (or (> ecc 0.4) ;; high eccentricity objects can move faster (see it at e=0.60)
		(> (abs inc) 90))) ;; high inc. orbits can be retrograde
       15.0) ;; we saw ONE that had 6 degree/day of motion, at 1 AU
      ;;
      ;; then all orbits very nearby can move very fast
      ((< delta 0.05) nil) ;; so close, we always consider it a candidate
      ((< delta 0.1) 20.0) ;; have not see any moving this fast in astorb
      ((< delta 0.4) 10.0)
      ((< delta 1.0)  5.0)
     
      ((and (< delta 2.0)  
	    (> ecc 0.3)) ;; slightly eccentric orbits might move a bit faster
       5.0)
      ;; everything inside 2AU might be moving at 3 deg/day - this
      ;; is a conservative over-estimate, because the earth's parallax
      ;; induces just 1 degree of motion (360/365.25 of a circle).  Looking at
      ;; actual orbit ensemble, the max speed we see is about 2 deg/day
      ((< delta 2.0) 3.0) 
      (t
       2.0))))  ;; otherwise assume 2 deg/day



    

(defun build-sbid-snap (mjdtt &key (orbit-element-vector *orbit-element-vector*)
			     (observatory "MKO") (nthreads nil))
  (let* ((n (length orbit-element-vector))
	 (ra-vec (make-array n :element-type 'single-float))
	 (dec-vec (make-array n :element-type 'single-float))
	 (delta-vec (make-array n :element-type 'single-float))
	 (flag-vec  (make-array n :element-type 'bit))
	 (ccomp (make-ccomp-for-mjdtt mjdtt :observatory observatory))
	 (snap (make-sbid-snap
		:mjdtt mjdtt :observatory observatory
		:flag-vec flag-vec
		:ra-vec ra-vec :dec-vec dec-vec :delta-vec delta-vec)))
    (chop-up-indexed-tasks-into-threads
     0 (1- n)
     (lambda (j1 j2)
       (%build-sbid-snap-range j1 j2 ccomp snap orbit-element-vector))
     :nthreads nthreads)
    ;; fill the kd-tree
    (loop for i below n
	  for elem  across *orbit-element-vector*
	  with kdtree-0-3 = (sbid-snap-kdtree-0-3 snap)
	  with kdtree-3-5 = (sbid-snap-kdtree-3-5 snap)
	  with kdtree-5-15 = (sbid-snap-kdtree-5-15 snap)
	  with scratch = (make-array 3 :element-type 'kdtree-jk:kd-float)
	  for ra across (sbid-snap-ra-vec snap)
	  for dec across (sbid-snap-dec-vec snap)
	  for delta across (sbid-snap-delta-vec snap)
	  for max-vel = (%max-daily-motion delta elem)
	  do
	     (cond
	       ;; the fast movers are just a list
	       ((or (not max-vel)
		    (> max-vel 15))
		(push i (sbid-snap-fast-movers snap)))
	       ;;
	       ;;
	       ;; slower movers are divided into kd trees
	       ((<= max-vel 3.0)
		(kdtree-jk/latlon:insert-latlon
		 kdtree-0-3
		 (* 1d0 ra) (* 1d0 dec) i :vec scratch))
	       ((<= max-vel 5.0)
		(kdtree-jk/latlon:insert-latlon
		 kdtree-3-5
		 (* 1d0 ra) (* 1d0 dec) i :vec scratch))
	       ((<= max-vel 15.0)
		(kdtree-jk/latlon:insert-latlon
		 kdtree-5-15
		 (* 1d0 ra) (* 1d0 dec) i :vec scratch))))
    ;;
    snap))




;; return T if an object is a candidate for RA,DEC,MJDTT given that its
;; position in the estimate SNAP was RA-EST,DEC-EST,MJDTT-EST, with
;; DELTA and ECC
(defun %is-object-a-candidate (iobj ra dec mjdtt
			       snap elem
			       &key (search-radius 3600d0)) ;; arcsec
  (declare (type double-float ra dec mjdtt)
	   (type sbid-snap snap)
	   (type orbital-elements:comet-elem elem)
	   (optimize speed))
  (let* ((ra-est (* 1d0 (aref (sbid-snap-ra-vec snap) iobj)))
	 (dec-est (* 1d0 (aref (sbid-snap-dec-vec snap) iobj)))
	 (delta (aref (sbid-snap-delta-vec snap) iobj))
	 (flag (aref (sbid-snap-flag-vec snap) iobj))
	 (mjdtt-est (sbid-snap-mjdtt snap))
	 (speed-max  (%max-daily-motion delta elem)))
    (declare (type double-float ra-est dec-est mjdtt-est)
	     (type single-float delta)
	     (type (or null single-float) speed-max)
	     (type bit flag))
    (if (or (not speed-max) ;; NIL speed-max means it is a candidate (small delta)
	    (= flag 1))     ;; or we can't compute its position in the searcher
	t
	(progn
	  (let* ((dmjd (abs (- mjdtt mjdtt-est)))
		 (max-sky-angle (* 1d0 (max 1.0 ;; in case mjdtt=mjdtt-est
					    (+ (/ search-radius 3600)
					       (* dmjd speed-max)))))
		 ;; we could check simply if DEC spread is too big, but
		 ;; in practice it doesn't speed things up over full sky-angle
		 (sky-angle 
		   (astro-coords:sky-angle
		    ra dec ra-est dec-est :units :degrees)))
	    (declare (type double-float dmjd max-sky-angle sky-angle))
	    ;; T when it is a candidate
	    (< sky-angle max-sky-angle))))))
  


;; 
(defun %find-nearest-sbid-snap-in-mem (mjdtt mem &key (max-mjd-diff 1.0))
  (declare (type sbid-mem mem))
  (loop with best-mjd-diff = 1d99
	with best-snap = nil
	for snap in (sbid-mem-sbid-snap-list mem)
	for mjd-diff =  (abs (- (sbid-snap-mjdtt snap) mjdtt))
	when (and (< mjd-diff max-mjd-diff) ;; small enough to acc
		  (< mjd-diff best-mjd-diff)) ;; better than current best
	  do (setf best-snap snap)
	     (setf best-mjd-diff mjd-diff)
	finally (return best-snap)))


(defun %find-or-create-sbid-snap-in-mem  (mjdtt mem
					  &key
					    (max-mjd-diff 1.0)
					    (orbit-element-vector *orbit-element-vector*)
					    (observatory "MKO") ;; ignored
					    (nthreads nil)) 
  (declare (type sbid-mem mem))
  (let ((snap (%find-nearest-sbid-snap-in-mem mjdtt mem :max-mjd-diff max-mjd-diff)))
    ;; if a nearby snap (within max-mjd-diff) not found, then create one
    (when (not snap)
      (setf snap (build-sbid-snap mjdtt
				  :orbit-element-vector orbit-element-vector
				  :observatory observatory
				  :nthreads nthreads))
      
      ;; build the new sbid-mem-sbid-snap-list by adding new snap to the end,
      ;; possibly removing snap from the front
      (setf (sbid-mem-sbid-snap-list mem)
	    (append
	     (if (> (length (sbid-mem-sbid-snap-list mem))
		    (sbid-mem-nsnaps-max mem))
		 ;; remove front snap if too long
		 (cdr (sbid-mem-sbid-snap-list mem))
		 ;; else just keep the original list
		 (sbid-mem-sbid-snap-list mem))
	     (list snap)))) ;; put new snap at the end
    ;;
    snap))
  


;; create a list of '((index dist/arcsec) ...) of potential candidates that match
;; ra,dec at mjdtt
(defun %generate-list-of-sbid-candidate-indices
    (ra dec mjdtt snap
     &key
       (search-radius 3600d0)
       (orbit-element-vector *orbit-element-vector*))
  (declare (type double-float ra dec mjdtt)
	   (type sbid-snap snap))

  ;; it's possible that orbits weren't initialized if starting with 
  ;; astorb init that doesn't load data at compile time
  (initialize-orbital-elements) ;; won't do anything if *orbit-element-vector*
                                ;; is already set
  
  (let ((d-mjd (abs (- mjdtt (sbid-snap-mjdtt snap))))
	(iobj-list nil))
    ;; with search radii scaling with the change in mjd D-MJD from the
    ;; snap's mjd, extract lists of possible candidates from the kd trees,
    ;; and add the list for the fast movers
    (let ((kdresult-0-3
	    (kdtree-jk/latlon:kd-latlon-search-in-angle
	     (sbid-snap-kdtree-0-3 snap)
	     ra dec (+ 0.1d0 d-mjd 3.01) ))
	  (kdresult-3-5
	    (kdtree-jk/latlon:kd-latlon-search-in-angle
	     (sbid-snap-kdtree-3-5 snap)
	     ra dec (+ 0.1d0 d-mjd 5.01)))
	  (kdresult-5-15
	    (kdtree-jk/latlon:kd-latlon-search-in-angle
	     (sbid-snap-kdtree-5-15 snap)
	     ra dec (+ 0.1d0 d-mjd 15.01))))
      (loop for i below  (kdtree-jk:kdresult-n kdresult-0-3)
	    do (push (aref (kdtree-jk:kdresult-obj-vec kdresult-0-3) i)
		     iobj-list))
      (loop for i below  (kdtree-jk:kdresult-n kdresult-3-5)
	    do (push (aref (kdtree-jk:kdresult-obj-vec kdresult-3-5) i)
		     iobj-list))
      (loop for i below  (kdtree-jk:kdresult-n kdresult-5-15)
	    do (push (aref (kdtree-jk:kdresult-obj-vec kdresult-5-15) i)
		     iobj-list))
      (setf iobj-list (nconc ;; destructive append on iobj-list
		       iobj-list
		       (sbid-snap-fast-movers snap))))
    ;;
    (loop for iobj in iobj-list
	  for elem = (aref orbit-element-vector iobj)
	  when   (and elem ;; can be NIL for SLALIB error on load
		      (%is-object-a-candidate iobj ra dec mjdtt snap elem
					      :search-radius search-radius))
	    collect iobj)))


(defun %generate-list-of-sbid-candidate-indices-old
    (ra dec mjdtt snap
     &key
       (search-radius 3600d0)
       (orbit-element-vector *orbit-element-vector*))
  (declare (type double-float ra dec mjdtt)
	   (type sbid-snap snap))
  (loop for iobj below  (length orbit-element-vector)
	for elem = (aref orbit-element-vector iobj)
	when   (and elem ;; can be NIL for SLALIB error on load
		    (%is-object-a-candidate iobj ra dec mjdtt snap elem
					    :search-radius search-radius))
	  collect iobj))


;; get cached (ra dec delta) cached by (elem mjd observatory perturb)
;;#+nil ;; relatively minor speedup 
(let ((radecr-hash (make-hash-table :test 'equal))
      (rlock (bordeaux-threads:make-lock "radecr-hash"))
      (ncache-max 100000))
  (defun get-cached-orbit-radecr (elem mjd observatory &key (perturb t))
    (block gco
      (let ((key (list elem mjd observatory perturb)))
	(bordeaux-threads:with-lock-held (rlock)
	  (let ((radecr (gethash key radecr-hash)))
	    (when radecr ;; we have our cached value
	      (return-from gco (values-list radecr)))
	    ;; cache is full, so empty it
	    (when (> (hash-table-count radecr-hash) ncache-max)
	      (clrhash radecr-hash))))
	;; at this point, we didn't get cached value
	(multiple-value-bind (ra dec delta)
	    (ignore-errors ;; catch slalib error
	     (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
	      elem mjd observatory
	      :perturb perturb
	      :correct-mjd-to-tt nil))
	  (bordeaux-threads:with-lock-held (rlock)
	    (setf (gethash key radecr-hash)
		  (list ra dec delta)))
	  (values ra dec delta))))))
	  
	

;; given a preliminary list of indices in INDEX-VEC, return the list
(defun %calculate-candidates-for-indices
    (ra dec
     mjd/tt ;; was already corrected to mjdtt if desired
     snap index-vec i1 i2
     &key
       (max-distance 20.0)
       (perturb t)
       (observatory "MKO")
       (orbit-element-vector *orbit-element-vector*))
  "Generates candidates from initial candidates in index-vec - see generate-candidates"
  (declare (type double-float ra dec mjd/tt)
	   (type sbid-snap snap))

  (loop with outlist = nil
	for i from i1 to i2
	for index = (aref index-vec i)
	for elem = (aref orbit-element-vector index)
	for perturbed-elem
	  = (if (and elem perturb)
		(%get-perturbed-orbit elem snap) ;; finds 'close enough' element
		elem)
	when perturbed-elem
	  do (multiple-value-bind (ra-c dec-c delta-c dra/dt-c ddec/dt-c)
		 (progn ;ignore-errors  

		  ;; cache ra,dec,r by orbit - this is is 10x faster if we hit the same
		  ;; object candidates again and again, as in a narrow survey region
		  ;;#+nil
		  ;; FIXME - this might be a good idea, but it leaves dra/dt etc as NIL
		  #+nil
		  (get-cached-orbit-radecr
		   ;; use peturbed orbit caching because
		   ;; repeated perturbing of nearby orbits is
		   ;; costly
		   peturbed-elem
		   mjd/tt observatory
		   :perturb nil)
		  ;;#+nil
		  ;; 
		  ;; for now, just compute it each time without caching
		  (ignore-errors ;; slalib can fail for extreme orbits like 1I
		    (slalib-ephem:compute-radecr-from-comet-elem-with-rates-for-observatory
		     ;; use peturbed orbit caching because
		     ;; repeated perturbing of nearby orbits is
		     ;; costly
		     perturbed-elem
		     mjd/tt observatory
		     :perturb nil ;; handled by %get-perturbed-orbit
		     :correct-mjd-to-tt nil)))

	       (when ra-c ;; if NIL, then SLALIB failed interally; nothing we can do
		 (let ((dist (astro-coords:sky-angle ra dec ra-c dec-c
						     :units :arcsec)))
		   (when (< dist max-distance)
		     (push (make-candidate
			    :id (slalib-ephem:comet-elem-id elem)
			    :dist dist :elem elem :index index
			    :mjd mjd/tt
			    :observatory (if (observatories:observatory-p observatory)
					     (observatories:observatory-id observatory)
					     observatory)
			    :ra ra-c :dec dec-c :delta delta-c
			    :dra/dt dra/dt-c :ddec/dt ddec/dt-c)
			   outlist)))))
	finally
	   (return outlist)))


		
		
(defun generate-candidates  (ra dec mjd mem
     &key
       (max-distance 20.0)
       (correct-mjd-to-tt t)
       (perturb t)
       (max-mjd-diff 1.0)
       (orbit-element-vector *orbit-element-vector*)
       (observatory "MKO")
       (nthreads nil))
  "Return a list of candidate matches for RA,DEC,MJDTT, using MEM of
type SBID-MEM, returning a list of CANDIDATE structures sorted by
distance; MAX-DISTANCE is the maximum distance of a candidate in
arcsec."

  ;; do this first - also multi-threaded
  (bordeaux-threads:with-lock-held ((sbid-mem-lock mem))
    (%find-or-create-sbid-snap-in-mem mjd mem
				      :orbit-element-vector orbit-element-vector
				      :observatory observatory
				      :max-mjd-diff max-mjd-diff
				      :nthreads nthreads))
   (let* ((mjdtt
	    (if correct-mjd-to-tt
		(slalib:correct-mjdut-to-mjdtt mjd)
		mjd))
	  (snap  (%find-nearest-sbid-snap-in-mem   
		  mjdtt mem :max-mjd-diff max-mjd-diff))
	  ;; fast generation of rough candidates using KD-trees
	  (indices (%generate-list-of-sbid-candidate-indices
		    ra dec
		    mjd ;; supposed to be mjdtt but at this level doesn't matter
		    snap
		    :orbit-element-vector orbit-element-vector
		    :search-radius max-distance))
	  (index-vec (coerce indices 'vector)))


     
     ;; check the rough candidates exactly, spreading the task among threads
     (let ((candidates
	     (if (not nthreads) ;; no threading, for testing mainly
	       (%calculate-candidates-for-indices
		        ra dec mjd snap index-vec
			0 (1- (length index-vec))
			:max-distance max-distance
			:perturb perturb
			;; get the observatory once
			:observatory (observatories:get-observatory observatory)
			:orbit-element-vector orbit-element-vector)
	       ;; threaded version for speed
	       (apply 'append
		      (chop-up-indexed-tasks-into-threads
		       0 (1- (length index-vec))
		       (lambda (i1 i2)
			 (%calculate-candidates-for-indices
		          ra dec mjd snap index-vec
			  i1 i2
			  :max-distance max-distance
			  :perturb perturb
			  ;; get the observatory once
			  :observatory (observatories:get-observatory observatory)
			  :orbit-element-vector orbit-element-vector))
		       :nthreads nthreads))  )))
       (sort candidates '< :key #'candidate-dist))))





(let ((chash (make-hash-table :test 'equal))
      (chash-lock (bordeaux-threads:make-lock "chash-lock"))
      (nhash-max 100))
  ;; cache ((ra dec caching-radius orbit-element-vector) candidate-list)
  ;; using key (mjd perturb observatory correct-mjd-to-tt)

  ;; get just those candidates in big-candidates within max-distance of
  ;; ra,dec
  (defun %filter-cached-candidates (big-candidates ra dec max-distance)
    (sort 
     (loop for candidate in big-candidates
	   for distance = (astro-coords:sky-angle
			   ra dec
			   (candidate-ra candidate) (candidate-dec candidate) :units :arcsec)
		when (< distance max-distance)
		  ;; we have to copy the candidate, with the correct distance, because a different
		  ;; distance may have been cached
		  collect (let ((cand2 (copy-structure candidate)))
			    (setf (candidate-dist cand2) distance)
			    cand2))
     '<
     :key 'candidate-dist))

  (defun %clear-gen-hash () (clrhash chash)) ;; for diagnostics
     
  (defun generate-candidates-with-caching
      (ra dec mjd mem
       &key
	 (max-distance 20.0)
	 (caching-distance 7200.0) ;; 2x the size of a 1 degree field
	 (correct-mjd-to-tt t)
	 (perturb t)
	 (max-mjd-diff 1.0)
	 (orbit-element-vector *orbit-element-vector*)
	 (observatory "MKO")
	 (nthreads nil))
    "Like GENERATE-CANDIDATES, but grab all objects within CACHING-DISTANCE,
cache them by key=(mjd perturb), and use this cache for subsequent respects
if possible.  This speeds things up for repeated searches in small regions."
    (let ((big-candidates-pair nil)
	  ;; key should really contain orbit-element-vector too
	  (key (list mjd perturb observatory correct-mjd-to-tt)))

      (bordeaux-threads:with-lock-held (chash-lock)
	(setf big-candidates-pair (gethash key chash)))

      (block done
	(when big-candidates-pair
	  (let* ((radecplus (first big-candidates-pair)) ;; center of cached cands
		 (rac (first radecplus))
		 (decc (second radecplus))
		 (rcached (third radecplus))
		 ;; the orbit element vector - will realistically never change
		 (oev (fourth radecplus))
		 (big-cached-candidates (second big-candidates-pair)))
	    (when (and
		   ;; using same orbit elements? (will always be true, realistically)
		   (eq oev orbit-element-vector)
		   ;; desired circle inside big cached circle?
		   (< (astro-coords:sky-angle rac  decc ra dec :units :arcsec)
		      (- rcached  max-distance)))
	      (return-from done
		(%filter-cached-candidates big-cached-candidates ra dec max-distance)))))
	
	(let ((big-candidates
		(generate-candidates ra dec mjd mem
				     :max-distance caching-distance
				     :correct-mjd-to-tt correct-mjd-to-tt
				     :perturb perturb
				     :max-mjd-diff max-mjd-diff
				     :orbit-element-vector orbit-element-vector
				     :observatory observatory
				     :nthreads nthreads)))
	  ;; clean up hash if needed, and put them in
	  (bordeaux-threads:with-lock-held (chash-lock)
		 (when (> (hash-table-count chash) nhash-max)
		   (clrhash chash))
		 (setf (gethash key chash)
		       (list (list ra dec caching-distance orbit-element-vector)
			     big-candidates)))
	  ;; and return those inside our desired circle
	  (return-from done
	    (%filter-cached-candidates big-candidates ra dec max-distance)))))))
	       
	       
	    
	
      
  
