;; some higher level charts


(in-package pgplot)



(defgeneric horizontal-bar-chart (p quantity-vec
				  &key label-vec border-color
				      fill-color fill-style
				      character-size-scale
				      label-buffer-scale set-limits
				      draw-box size bar-width x-span
				      x-log x-label-offset label-position)
  (:documentation
"Draw a bar chart with horizontal bars

  P               the pgplot device
  QUANTITY-VEC    the lengths of the bars
  LABEL-VEC       optional vector of labels for bars
  BORDER-COLOR    symbol or vector of symbols for bar border color 
  FILL-COLOR      symbol or vector of symbols for bar interior color
  FILL-STYLE      symbol or vector of symbols for bar interior style
  CHARACTER-SIZE-SCALE  factor by which to adjust label sizes; usually 1.0
  LABEL-BUFFER-SCALE  factor by which to adjust space around labels;
                      usually 1.0
  SET-LIMITS      set limits of plot from data
  DRAW-BOX        draw the box?
  SIZE            size for box
  BAR-WIDTH       how wide the bars should be (compared to 1.0)
  X-SPAN          how wide to make plot, relative to longest bar
  X-LOG           the X axis is a base-10 log; make axis logarithmic
  X-LABEL-OFFSET  where (in fraction of horizontal span) to start bar labels
  LABEL-POSITION  :AFTER or :INSIDE; where labels go relative to bars
"
   ))

(defmethod horizontal-bar-chart ((p pgplot) quantity-vec
				 &key 
				 (label-vec nil)
				 (border-color :default) ;; vector or symbol
				 (fill-color :light-gray)
				 (fill-style :cross-hatched)
				 (character-size-scale 1.0)
				 (label-buffer-scale 1.0)
				 (set-limits t)
				 (draw-box t) 
				 (size nil)
				 (bar-width 0.8)
				 (x-span 1.1)
				 (x-log nil)
				 (x-label-offset 0.10)
				 (label-position :inside))
				 

  ;;
  (let* ((val-max (%vecmax quantity-vec))
	 (half-bar-width (* 0.5 bar-width))
	 (n (length quantity-vec))
	 (y-max (+ n bar-width))
	 (y-min (- 1 bar-width))
	 ;; bar width in [0,1] units
	 (bwidth (/ (- (- y-max y-min) 0.5) n))
	 (character-height (* bwidth character-size-scale (/ 10.0 n)))
	 (label-buffer (* label-buffer-scale character-height 0.05)))
    
    (flet ((check-vec (thing name)
	     (when (and (vectorp thing)
			(not (= (length thing) n)))
	       (error "~A is not a vector of length ~A" name n))))
      (check-vec fill-color "FILL-COLOR")
      (check-vec border-color "BORDER-COLOR")
      (check-vec fill-style "FILL-STYLE"))
    (when (and label-vec
	       (not (and (vectorp label-vec) (= (length label-vec) n))))
      (error "LABEL-VEC is not a vector of length ~A" n))

    (when set-limits
      (pgplot:set-window p 0.0 (* x-span val-max)   y-min y-max))
    ;;
    (when draw-box
      (pgplot:box p :y-num-labels-left nil 
		    :y-minor-ticks nil  
		    :y-major-ticks nil :x-log x-log :size size))
    ;;
    (loop
       for i from 0
       for j = (1+ i)
       for val across quantity-vec 
       for label = (if label-vec (aref label-vec i))
       for bc = (if (vectorp border-color) (aref border-color i) border-color)
       for fc = (if (vectorp fill-color) (aref fill-color i) fill-color)
       for fs = (if (vectorp fill-style) (aref fill-style i) fill-style)
       do
	 (pgplot:rectangle p 0 val (- j half-bar-width) (+ j half-bar-width) 
			   :fill-area-style fs
			   :color fc)
	 (pgplot:rectangle p 0 val (- j half-bar-width) (+ j half-bar-width) 
			   :color bc)
	 (when label
	   (let ((label-xpos
		  (cond ((eq label-position :inside)
			 (* x-label-offset val-max))
			((eq label-position :after)
			 (+ val (* val-max x-label-offset)))
			(t
			 (error "Invalid LABEL-POSITION ~a" label-position)))))
	   (pgplot:write-blanked-text  
	    p 
	    label
	    label-xpos (- j 0.1)
	    :character-height character-height 
	    :buffer label-buffer ))))))

