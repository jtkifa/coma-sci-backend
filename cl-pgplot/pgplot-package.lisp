  
;; package definition for pgplot

(defpackage #:pgplot
  ;;
  (:use #:common-lisp
	#:waaf-cffi
	)
  ;;
  (:export  #:*pgplot-list*     ;; list of all living devices
	    #:*current-pgplot*  ;; the currently active device
	    #:pgplot-p #:pgplot
	    ;;
	    #:open-device
	    #:close-device
	    #:set-device-geometry
	    #:close-all-devices
	    #:activate-device   ;; not really meant to be used by user
	    ;;
	    ;; macro to set pgplot's qualities temporarily
	    #:with-pgplot-settings
	    ;;
	    #:is-open
	    ;;
	    ;; getters and setters that work on a device
	    ;; the getters are just :accessors; the setters also set
	    ;; the pgplot library global property if the device being set is
	    ;; the currently active one
	    #:set-character-font    #:get-character-font
	    #:set-character-height  #:get-character-height
	    #:set-text-background-color #:get-text-background-color
	    #:set-color             #:get-color-index
	    #:set-fill-area-style   #:get-fill-area-style
	    #:set-color-index       #:get-color-index
	    #:set-line-style        #:get-line-style
	    #:set-line-width        #:get-line-width
	    #:set-pen-position      #:get-pen-position
	    #:set-arrow-head-style  #:get-arrow-head-style
	    #:set-hatching-style    #:get-hatching-style
	    #:set-clipping-state    #:get-clipping-state
	    #:set-color-index-range #:get-color-index-range
	    #:set-window            #:get-window
	    #:set-viewport          #:get-viewport
;;	    #:set-width
	    #:get-width   
;;	    #:set-aspect-ratio
	    #:get-aspect-ratio
	    #:set-current-pane
	    #:with-viewport 
	    #:box #:plain-box
	    #:circle
	    #:ellipse
	    #:connect
	    #:contour
	    #:image 
	    #:color-wedge
	    #:vector-field
	    #:draw-arrow
	    #:erase
	    #:errorbars
	    #:pgplot-cursor-position
	    #:plotlabel  ;; built-in x,y,top labels -- poor positioning
	    #:pgmtxt
	    #:points
	    #:polygon
	    #:rectangle
	    #:set-colormap-color
	    #:modify-colormap
;;	    #:string-size  ;; rather useless compared to text-bounding-box
	    #:text-bounding-box
	    #:write-text
	    #:world-to-device #:device-to-world ;; coord transformations
	    #:xlabel #:ylabel #:toplabel ;; better labels than plotlabel
	    #:draw-labeled-axis
	    #:hist
	    #:pgaxis 

	    ;; pgplot-extras.lisp
	    #:pgplot-encode-float-sci-notation 
	    #:draw-wedge
	    #:with-window
	    #:make-2d-transform-matrix
	    #:draw-plot-legend
	    #:write-blanked-text
	    #:compute-aspect-ratio-for-panes
	    #:make-2d-density-image
	    #:bin-2d-data ;; legacy for make-2d-density-image
	    #:generate-linear-tick-intervals
	    #:generate-log-tick-intervals
	    
	    ;; fancy-plots.lisp
	    #:horizontal-bar-chart
	    ;;
	    ;; colormap.lisp
	    #:insert-colormap ;; handles all colormaps now
           ))

