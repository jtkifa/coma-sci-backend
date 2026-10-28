;; pgplot-cl version 1.1
;; information in README file 
;; distribution site is http://www.geocities.com/pgplot_cl


(in-package pgplot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define class for a pgplot plot, with attributes included --
;; thus the attributes get carried around by a plot, and changing
;; one plot's attributes doesn't mess up other plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass pgplot () 
  ((name :reader get-name :accessor pgplot-name :initform "untitled" :initarg :name)
   ;; numerical device id of plot
   (id   :reader get-id :accessor pgplot-id :initarg :id)
   ;; is the device open?
   (is-open :reader is-open :accessor pgplot-is-open :initform nil :initarg :is-open)
   ;; symbolic representation of device
   (device :reader get-device :accessor pgplot-device :initform nil :initarg :device)
   ;; pgplot has a limited output file length, so for devices creating
   ;; output files, we need to have a temporary file and a final destination file.
   ;; At device closing, we copy from the temporary file to the final file.
   (tmp-output-file :accessor pgplot-tmp-output-file :initform nil :initarg :tmp-output-file)
   (output-file :accessor pgplot-output-file :initform nil :initarg :output-file)
   ;;
   (character-font    :reader get-character-font :accessor pgplot-character-font)
   (character-height  :reader get-character-height :accessor pgplot-character-height)
   (color-index       :reader get-color-index :accessor pgplot-color-index)
   (fill-area-style   :reader get-fill-area-style :accessor pgplot-fill-area-style)
   (line-style        :reader get-line-style :accessor pgplot-line-style)
   (line-width        :reader get-line-width :accessor pgplot-line-width)
   (pen-position      :reader get-pen-position :accessor pgplot-pen-position)
   (arrow-head-style  :reader get-arrow-head-style :accessor pgplot-arrow-head-style)
   (hatching-style    :reader get-hatching-style :accessor pgplot-hatching-style)
   (clipping-state    :reader get-clipping-state :accessor pgplot-clipping-state)
   (text-background-color-index
                      :reader get-text-background-color-index 
		      :accessor pgplot-text-background-color-index)
   ;;
   (viewport          :reader get-viewport :accessor pgplot-viewport)  ;; '(XLEFT XRIGHT YBOT YTOP)
   (window            :reader get-window :accessor pgplot-window)   ;; world coords '(x1 x2 y1 y2)
   (aspect-ratio      :reader get-aspect-ratio :accessor pgplot-aspect-ratio)
   (width             :reader get-width :accessor pgplot-width)
   ;;
   (color-index-range :reader get-color-index-range :accessor pgplot-color-index-range)
   ;;
   ;; we use colormaps, rather than individual color indices
   (colormap          :reader get-colormap :accessor pgplot-colormap)
;   (default-colormap  :reader get-default-colormap :accessor pgplot-default-colormap)
   ;; last cursor position, or NULL if none
   (last-curs-x :reader get-last-curs-x :accessor pgplot-last-curs-x :initform nil)
   (last-curs-y :reader get-last-curs-y :accessor pgplot-last-curs-y :initform nil)
   ))


(defun error-on-giza (lisp-function-name c-function-name)
  (when *using-pgplot-giza*
    (error "ERROR - using giza pgplot library replacement in Lisp function ~A; ~S is not a supported in giza."
	   lisp-function-name c-function-name)))


(defun pgplot-p (x)
  (eq (type-of x) 'pgplot))

;; a colormap is three vectors of red,blue,green
;; determining color indices nos. 0..npts-1
;; we keep the first 16 at the default values
(defstruct colormap
  (name nil) ;; a general name for this colormap
  npoints    ;; how many color points?
  (color-alist nil) ;; an alist for the keyword-named colors in this map
  ;; single-float vectors for the actual colors
  (red-vec  nil :type (or null (simple-array single-float (*))))
  (green-vec nil :type (or null (simple-array single-float (*))))
  (blue-vec nil :type (or null (simple-array single-float (*))))
  ;; a list of the form eg '((:default 34 76) (:gray 34 76) (:rainbow 18 77) ...)
  ;; that delimits inclusive color ranges in the map, for making
  ;; images - each colormap should have a default range
  (default-range :default) ;; which range to use by default
  (named-ranges (list (list :default 0 16))))


;; name to color index mapping for default device - background/default
;; are black/white or white/black depending on device 
(defvar *default-colormap-color-alist*
  '((:BACKGROUND . 0) (:DEFAULT . 1) (:RED . 2) (:GREEN . 3) (:BLUE . 4)
    (:CYAN . 5) (:MAGENTA . 6) (:YELLOW . 7) (:ORANGE . 8)
    (:GREEN+YELLOW . 9) (:GREEN+CYAN . 10) (:BLUE+CYAN . 11)
    (:BLUE+MAGENTA . 12) (:RED+MAGENTA . 13)
    (:DARK-GREY . 14) (:LIGHT-GREY . 15) (:DARK-GRAY . 14) (:LIGHT-GRAY . 15)))



;; put the background color (at index 0) into index i, for the purpose
;; of having it as the first color of a colormap
(defun %put-background-color-at-index (cmap i)
  (setf (aref (colormap-red-vec cmap) i)
	(aref (colormap-red-vec cmap) 0))
  (setf (aref (colormap-green-vec cmap) i)
	(aref (colormap-green-vec cmap) 0))
  (setf (aref (colormap-blue-vec cmap) i)
	(aref (colormap-blue-vec cmap) 0)))



;; generate the default colormap for current device - 16<=nelem<=255 --
;; nelem>16 only if rest of colormap is to be filled in with other colors
;; fixme - currently uses 100 colors, which is OK for X11, but could
;; adjust automatically to number of available colors.  also, our
;; color colormap is not so good - eg, no yellow

;; we would like to have more than 100 colors, but X11 device seems to support only
;; 100 (as determined by calling PGQCOL)
(defunL make-default-colormap ()
"make a default colormap with the normal colors, and a :grey
component"
  (let* ((nelem 100) ;; ok for X
	 (rv  (make-array nelem :element-type 'single-float :initial-element 0.0))
	 (gv  (make-array nelem :element-type 'single-float :initial-element 0.0))
	 (bv  (make-array nelem :element-type 'single-float :initial-element 0.0))	      
	 (cm (make-colormap
	      :name :default
	      :npoints nelem :color-alist *default-colormap-color-alist*
	      :red-vec rv :green-vec gv :blue-vec bv
	      )))
    (loop for i from 0 below 16
	  do
	  (multiple-value-bind (r g b) (pgqcr i)
	    (setf (aref rv i) r)
	    (setf (aref gv i) g)
	    (setf (aref bv i) b)))
    ;; make the next highest ones be gray scale for a start - this will be changed
    (insert-colormap-component cm 16 99  :grey 
			       (lambda (x) (values x x x)))
    cm ;; return value
    ))




;; insert a colormap component into indices - func is a function of
;; (i-imin)/(imax-imin) that returns the red,green,blue values for
;; this colormap, over a domain [0,1].  name is the keyword name of
;; this colormap - delete-overlapping-ranges tells us to
;; remove any other ranges that were overwritten by this insertiion
(defunL insert-colormap-component (cm imin imax name func &key
				   (delete-overlapping-ranges t)
				   (first-color-is-background nil))
  (declare (type colormap cm)
	   (type (unsigned-byte 16) imin imax)
	   (type (function (float) (values float float float)) func))
  (when (or (>= imin imax) (>= imax (colormap-npoints cm)))
    (error "out of range colormap indices imin=~a imax=~a - must be in [~A,~A]" imin imax
	   0 (1- (colormap-npoints cm))))

  ;; if we want the first color to be backd (for purpose of blanking images)
  (when first-color-is-background
    (%put-background-color-at-index cm imin))

  (loop
    with imin* = (if first-color-is-background (1+ imin) imin)
    with dx = (float (- imax imin))
    with rv = (colormap-red-vec cm)
    with gv = (colormap-green-vec cm)
    with bv = (colormap-blue-vec cm)
    for i of-type (unsigned-byte 16) from imin* to imax
    for j of-type (unsigned-byte 16) from 0 to (- imax imin)
    for x = (/ (float (- i imin)) dx)
    do (multiple-value-bind (r g b) (funcall func x)
	 (setf (aref rv i) (coerce r 'single-float)
	       (aref gv i) (coerce g 'single-float)
	       (aref bv i) (coerce b 'single-float))))
  ;;
  (when delete-overlapping-ranges
    (setf (colormap-named-ranges cm)
	  (remove-if (lambda (range)
		       (not (or (>= (second range) imax)
				(<= (third range) imin))))
		     (colormap-named-ranges cm))))
  ;;
  (cond ((listp name)
	 (dolist (one-name name)
	   (push (list one-name imin imax)  (colormap-named-ranges cm))))
	(t
	 (push (list name imin imax)  (colormap-named-ranges cm)))))






;; this function probably won't be used much except for user-custom colormaps,
;; since INSERT-COLORMAP  handles the standard ones
(defgeneric modify-colormap (p imin imax colormap-name
			     &key color-function
			       first-color-is-background)
							 
  (:documentation "Insert a new colormap such that (COLOR-FUNCTION x) returns
  (VALUES RED BLUE GREEN) where red,blue,green are in [0,1] and X
is the distance in the index range, also [0,1].    This operation
will clobber existing colormaps.  Default colormap ranges are

  Default Colors [0,15]  - do not alter this to allow normal plotting
  Others         [16,99]

If FIRST-COLOR-IS-BACKGROUND is set, then the first color of the range
is set to be be the background color.  This is useful for blanking images."))

(defmethodL modify-colormap ((p pgplot) imin imax colormap-name
			     &key
			     (color-function nil)
			     (first-color-is-background nil))
  (declare (type (unsigned-byte 16) imin imax)
	   (type keyword colormap-name)
	   (type (or null (function (float) (values float float float)))
		 color-function))
  (insert-colormap-component (pgplot-colormap p) imin imax
			     colormap-name color-function
			     :first-color-is-background first-color-is-background)
  (activate-device p)
  (set-colormap (pgplot-colormap p)))

	
	 


		     

;; set color map in active device to colormap cm
(defunL set-colormap (cm)
  (dotimes (i (colormap-npoints cm))
    (declare (type (unsigned-byte 16) i))
    (pgscr i
	   (aref (colormap-red-vec   cm)  i)
	   (aref (colormap-green-vec cm)  i)
	   (aref (colormap-blue-vec  cm)  i))))
	   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-viewport ((p xleft xright ybot ytop) &body body)
"Evaluate BODY with pgplot P's viewport temporarliy set to XLEFT XRIGHT
XBOT YTOP"
  (let ((psym (gensym "p"))
	(xleft-sym (gensym "xleft"))
	(xright-sym (gensym "xright"))
	(ybot-sym (gensym "ybot"))
	(ytop-sym (gensym "ytop"))
	(vp-sym (gensym "vp")))
    `(let* ((,psym ,p)
	    (,xleft-sym ,xleft)
	    (,xright-sym ,xright)
	    (,ybot-sym ,ybot)
	    (,ytop-sym ,ytop)
	    ;;
	    (,vp-sym (pgplot:get-viewport ,psym)))
      ;;
      (pgplot:set-viewport ,psym ,xleft-sym ,xright-sym ,ybot-sym ,ytop-sym)
      ;;
      ,@body
      (apply 'pgplot:set-viewport ,psym ,vp-sym))))
      
      


(defmacro with-window ((p x0 x1 y0 y1) &body body)
"Evaluate BODY with pgplot P's viewport temporarily set to X0 X1
YO Y1"
  (let ((psym (gensym "p"))
	(x0-sym (gensym "x0"))
	(x1-sym (gensym "x1"))
	(y0-sym (gensym "y0"))
	(y1-sym (gensym "y1"))
	(win-sym (gensym "win")))
    `(let* ((,psym ,p)
	    (,x0-sym ,x0)
	    (,x1-sym ,x1)
	    (,y0-sym ,y0)
	    (,y1-sym ,y1)
	    ;;
	    (,win-sym (pgplot:get-window ,psym)))
      ;;
      (pgplot:set-window ,psym ,x0-sym ,x1-sym ,y0-sym ,y1-sym)
      ;;
      ,@body
      (apply 'pgplot:set-window ,psym ,win-sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now the higher level (user) versions of the pgplot routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *current-pgplot* nil
  "The current active pgplot device; ie, the last device used")
(defvar *pgplot-list* nil "List of all alive PGPLOT devices.")

;; how does one get this directly from pgplot?
(defconstant +max-pgplot-open-devices+ 8) ;; change this for giza?


(defparameter *pgplot-devices-alist*
;;   name      str        need-filename?  file-suffix
  '((:XWINDOW  "/xwindow" nil)  
    (:X        "/xwindow" nil)
    (:X11      "/xwindow" nil)
    ;; xs resizable persistent window but it doesn't seem to resizable
    (:X11S     "/xserve"  nil)  
    (:NULL     "/null"    nil)
    (:GIF      "/gif"     t ".gif")
    (:VGIF     "/vgif"    t ".gif")
    (:PSLAND   "/cps"     t ".ps")       ;; we use only color postscript
    (:PSPORT   "/vcps"    t ".ps")  
    (:PS       "/cps"     t ".ps")       ;; square postscript
    #+pgplot-does-pdf
    (:PDF       "/cps"     t)))       ;; square postscript

(defparameter *pgplot-giza-devices-alist*
  '((:XWINDOW  "/xw"    nil)  
    (:X        "/xw"    nil)
    (:X11      "/xw"    nil)
    (:PNG      "/png"   t  "png")
    (:SVG      "/svg"   t  "svg")
    (:PDF      "/pdf"   t  "pdf")
    (:VPDF      "/vpdf" t  "pdf") ;; landscape
    (:EPS       "/eps"  t  "eps")
    (:PS        "/ps"   t   "ps")
    (:VPS        "/vps" t  "ps")
    ;;(:MP4        "/mp4" "mp4") ;; would require compiled in mp4 support
    ))

(defun get-pgplot-devices-alist ()
  (if *using-pgplot-giza*
      *pgplot-giza-devices-alist*
      *pgplot-devices-alist*))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions to handle temporary output files

(defun %random-string (n)
  (loop with abc = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	with outstr = (make-string n)
	with m = (length abc)
	for i below n
	do (setf (aref outstr i) (aref abc (random m)))
	finally (return outstr)))
  
;; return a filename that has a unique temporary file
(defun %make-temp-output-file (&key (suffix nil) (dir "/tmp"))
  (flet ((make-random-outfile-name ()
	   (format nil "~A/PGPLOT_tmp_~A~A~A" dir (%random-string 10)
		   (if suffix "." "")
		   (or suffix ""))))
    (loop
      for file = (make-random-outfile-name)
      for sout = (ignore-errors
		 (open file :direction :output :if-exists :error :if-does-not-exist :create))
      until sout
      finally
	 (write-line  "Placeholder for temporary pgplot output file." sout)
	 (close sout)
	 (return file))))

;; check that pgplot output file is writable
(defun %verify-output-file (file)
  (let ((sout (ignore-errors
	       (open file  :direction :output :if-exists :supersede :if-does-not-exist :create))))
    (when (not sout)
      (error "Could not make pgplot output file ~A" file))
    (write-line  "Placeholder for temporary pgplot output file." sout)
    (close sout)
    t))
      
(defun %copy-file (infile outfile &key (block-size 32768))
  (with-open-file (sin infile :direction :input :element-type '(unsigned-byte 8))
      (with-open-file (sout outfile :direction :output
                            :if-does-not-exist :create ;; could have :ERROR
                            :if-exists :supersede ;; it will exist
                            :element-type '(unsigned-byte 8))
        (loop 
         with buff = (make-array block-size :element-type '(unsigned-byte 8))
         for n = (read-sequence buff sin)
         until (zerop n)
         do (write-sequence buff sout :end n)))))
			
			



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defunL open-device (device &key
		     (plot-name "untitled") (filename nil)
		     (font :roman)
		     (colormap :viridis) (first-color-is-background nil)
		     (square nil)
		     (aspect-ratio)
		     (height)
		     (width)
		     (invert nil))
  "open a pgplot device, where DEVICE is one of 
:X11 :NULL :GIF :VGIF :PSLAND :PSPORT :PS.
PLOT-NAME is an optional name
FILENAME is the file to which to write if device type chosen targets file
FONT is the font to use
COLORMAP is one of the colormaps supported by INSERT-COLORMAP and
  FIRST-COLOR-IS-BACKGROUND determines if the first color is set to background
SQUARE is a flag that tries to make the final plot square
ASPECT-RATIO tries to set the aspect ratio (over-rides SQUARE)
  1 means square; <1 means horizontal rectangle; >1 means vertical rectangle
HEIGHT-WIDTH tries to set the height and width (mm), and overrides ASPECT-RATIO
INVERT inverts normal color scheme"
  (if (>= (length *pgplot-list*) +max-pgplot-open-devices+)
      (error "pgplot supports only ~A simultaneously open devices.
   You must close one or more devices before making more plots
   [eg, use (pgplot:close-all-devices) ] "
	     +max-pgplot-open-devices+))
  (let* ((dev-list (assoc device (get-pgplot-devices-alist)))
	 (dev-str  (second dev-list))
	 (needs-file (third dev-list))
	 (file-suffix (if needs-file (fourth dev-list)))
	 (tmp-output-file (when (and needs-file filename
				     (%verify-output-file filename))
			    (%make-temp-output-file :suffix file-suffix)))
	 (dev-id 0)
	 (p nil))
    
    ;;
    (if (not dev-str)
	(error "No device ~A -- Available pgplot devices are ~A"
	       device (map 'list #'car  (get-pgplot-devices-alist))))
    (if (and needs-file (not filename))
	(error "Need to specify :FILENAME for device ~A" device))
    (if (and (not needs-file) filename)
	(error "Device ~A does not take a :FILENAME" device))
    ;;
    ;;
    #+nil
    (if (member device '(:gif :ps)) ;; these end up messed up if not :SQUARE
	(setf square t))

    (let ((device-path (concatenate 'string (or tmp-output-file "") dev-str)))
      (setf dev-id (pgopen-raw device-path)))
    ;;
    (setf p (make-instance 'pgplot :id dev-id
				   :device device
				   :tmp-output-file tmp-output-file
				   :output-file filename
				   :name plot-name))
    ;; mark device as open
    (setf (pgplot-is-open p) t)
    ;;
    ;; and set defaults in p, but don't call pgplot routines yet
    (setf (pgplot-character-font p) 1)      ;; default - changed below
    (setf (pgplot-character-height p) 1.7)  ;; biggish
    (setf (pgplot-color-index p) 1)
    (setf (pgplot-fill-area-style p) 2)     ;; OUTLINE
    (setf (pgplot-line-style p) 1)          ;; full line
    (setf (pgplot-line-width p) 2)          ;; double minimal thickness
    (setf (pgplot-pen-position p) '(0.0 0.0)) ;; x,y
    (setf (pgplot-arrow-head-style p) '(2 45.0 0.4)) ;; FS ANGLE BARB
    (setf (pgplot-hatching-style p) '(45.0 2.0 0.0)) ;; ANGLE SEPN PHASE
    (setf (pgplot-clipping-state p) 1)      ;; CLIPPING ENABLED
    (setf (pgplot-text-background-color-index p) -1)
    (setf (pgplot-viewport p) '(0.20 0.95 0.15 0.90)) ;; what looks good
    (setf (pgplot-window p)   '(0.0 1.0 0.0 1.0))
    (setf (pgplot-colormap p) (make-default-colormap))
    ;;(set-colormap (pgplot-default-colormap p)) ;; sets a default gray colormap
    ;; insert-colormap is in next file, so we funcall it to avoid compiler complaints
    (setf (pgplot-color-index-range p)
	  (list 1 (colormap-npoints (pgplot-colormap p))))
    (funcall 'insert-colormap p colormap :first-color-is-background first-color-is-background)


    ;;
    (push p *pgplot-list*) ;; add to global list
    (pgask 0) ;; always have this - having to hit RET is annoying

    (let ((ar (cond ((and square aspect-ratio
			  (not (= aspect-ratio 1)))
		     (error "ASPECT-RATIO=~A contradicts SQUARE" aspect-ratio))
		    (square 1.0)
		    (t aspect-ratio)))
	  (w 0.0))
      (cond ((and height width aspect-ratio)
	     (error "Can't set WIDTH, HEIGHT, and ASPECT-RATIO"))
	    ((and height width)
	     (setf ar (* 1.0 (/ height width))))
	    ((and aspect-ratio height)
	     (setf w (/ height aspect-ratio)))
	    ((and aspect-ratio width)
	     (setf w (* 1.0 width)))
	    (aspect-ratio ;; no width given
	     (setf w 0.0))
	    ((or height width)
	     (error "WIDTH or HEIGHT given but not ASPECT-RATIO")))
      (when ar ;; maybe no dimensions given
	(pgpap (/ w 25.4) ar)) ;; convert w in mm to inches
      ;; new page to reset size
      (when (not (zerop w)) (pgpage)))
    ;; set aspect ratio
    (multiple-value-bind (x1 x2 y1 y2) (pgqvsz 1)
      (declare (ignorable x1 y1))
      (setf (pgplot-width  p)  (* 25.4 x2)) ;; inches to mm
      (setf (pgplot-aspect-ratio p)  (/ y2 x2)))
    (activate-device p) ;; make current active device match this open dev
    (if invert
	(progn
	  (set-colormap-color p 0 1 1 1)
	  (set-colormap-color p 1 0 0 0)))
    (set-character-font	p font)
    ;; eliminate the worst colormap flashing for x devices by erasing
    ;; first
    (if (member device '(:XWINDOW :X :X11))
	(erase p))
    ;;
    p))



;; close device, ensuring that current device remains set
(defgeneric close-device (p)
  (:documentation "close a pgplot device object"))

(defmethodL close-device ((p pgplot))
  (if (pgplot-is-open p)  ;; ignore closed devices
      (progn
	(setf (pgplot-is-open p) nil) ;; mark device as closed
	(let ((old-dev (if *current-pgplot* (pgplot-id *current-pgplot*) nil))
	      (this-dev (pgplot-id p))
	      (ps-success nil)) ;; postscript failed - special case
	  (pgslct this-dev)
	  (pgask 0) ;; disable the silly return key requirement
	  (pgupdt)
	  (pgclos)
	  (when (pgplot-output-file p)
	    ;; put the bounding box in FRONT of the file (only for normal pgplot library)
	    (when (not *using-pgplot-giza*) ;; giza has its own functional pdf
	      (when (member (pgplot-device p) '(:pdf :ps))
		(setf ps-success
		      (fix-ps-bounding-box (pgplot-tmp-output-file p) :error-on-fail nil))))
	    
	    ;; maybe convert ps to pdf, if device is :PDF
	    #+pgplot-does-pdf 
	    (if (and
		 (not *using-pgplot-giza*) ;; this fix only needed for normal pgplot, not giza
		 (eq (pgplot-device p) :pdf) ;; tmpfile must be a .ps
		 ps-success)
		(ps2pdf:ps2pdf (pgplot-tmp-output-file p)
			       :outfile (pgplot-output-file p)
			       :orientation :seascape) ;; fixes orientation we hope
		(%copy-file (pgplot-tmp-output-file p) (pgplot-output-file p)))


	    ;; if we're using pgplot without extra pdf conversion, or using giza,
	    ;; then the tmp output file is the correct time, so we just copy it
	    (when (or 
		   *using-pgplot-giza*
		   (not (find :pgplot-does-pdf *features*)))
	      (%copy-file (pgplot-tmp-output-file p) (pgplot-output-file p)))
	    (delete-file (pgplot-tmp-output-file p)))
	  ;; restore the old device, if it was not the new device
	  (if (and old-dev (not (= this-dev old-dev)))
	      (pgslct old-dev))
	  (setf *pgplot-list* (remove p *pgplot-list*))
	  t
	  ))
    nil))

(defunL close-all-devices (&key (force nil))
"Close all open devices contained in pgplot::*pgplot-list*.

FORCE option causes the close to be on low device level, iterating
through all device numbers, in case normal closing causes problems."
  (if (not force)
      ;; nice close
      (dolist (p *pgplot-list*) (close-device p))
      ;; low level close that doesn't clean up
      (loop for idev from 0 to 7 ;; 8 devices total
	    do (pgslct idev)
	       (pgask 0)
	       (pgclos)))
  (setf *pgplot-list* nil)) ;; just in case a closed dev was on list


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric set-device-geometry (p width height)
  (:documentation "Set the window height to WIDTH x HEIGHT in mm"))

(defmethodL set-device-geometry ((p pgplot) (width real) (height real))
  (activate-device p)
  (pgpap (float (/ width 25.4) 1.0)
	 (float (/ height width) 2.0))
  (pgpage))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activate a device p -- ie, if p is not the active device in
;; *current-pgplot*, make p be the active dev. and set up the
;; active dev's parameters to be p's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric activate-device (p &key force-reset)
 (:documentation "Make pgplot device P the active one, and enable its settings."))
;;
(defmethodL activate-device ((p pgplot) &key (force-reset nil))
  (cond ((not (pgplot-is-open p))
	 (error "Tried to activate a closed pgplot device"))
	((and (eq p *current-pgplot*) (not force-reset))
	 t) ;; already open
	(t ;; else restore settings
	 (setf *current-pgplot* p)
	 (pgslct   (pgplot-id p))
	 (pgscf    (pgplot-character-font p))
	 (pgsch    (pgplot-character-height p))
	 (pgstbg   (pgplot-text-background-color-index p))
	 (pgsci    (pgplot-color-index p))
	 (pgsfs    (pgplot-fill-area-style p))
	 (pgsls    (pgplot-line-style p))
	 (pgslw    (pgplot-line-width p))
	 (apply #'pgmove (pgplot-pen-position p))
	 (apply #'pgsah  (pgplot-arrow-head-style p))
	 (apply #'pgshs  (pgplot-hatching-style p))
	 (apply #'pgsvp  (pgplot-viewport p))
	 (apply #'pgswin (pgplot-window p))
	 (pgsclp   (pgplot-clipping-state p))
	 (set-colormap (pgplot-colormap p))
	 (apply #'pgscir   (pgplot-color-index-range p))
	 t)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at the end of routines that move the pen, we call
;; fix-pen-position to update our remembered pen position
;; for this device (fix-pen-position is internal to this package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defunL fix-pen-position (p)
  (multiple-value-bind (x y) (pgqpos)
			(setf (pgplot-pen-position p) (list x y))))
			

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *character-font-alist* '((:DEFAULT . 1) (:ROMAN . 2) (:ITALIC . 3)
				 (:SCRIPT . 4)))

(defgeneric set-character-font (p font)
  (:documentation
   "set the current character font of device P, where font
is either an integer 1-4, or one of :DEFAULT :ROMAN :ITALIC :SCRIPT"))

(defmethodL set-character-font ((p pgplot) (font integer))
  (if (or (< font 1) (> font 4))
      (error "Bad font number ~A" font))
  (setf (pgplot-character-font p) font)
  (if (eq p *current-pgplot*)
      (pgscf font))
  t)
;;
(defmethodL set-character-font ((p pgplot) (font symbol))
  (let ((nf (cdr (assoc font *character-font-alist* :test 'string-equal))))
    (if (not nf)
	(error
	 "Invalid font ~A -- Available fonts are ~A"
	 font (map 'list #'car  *character-font-alist*)))
    (set-character-font p nf)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-character-height (p height)
  (:documentation   
   "set the character height of pgplot device P to a number.
Default character height is 2.0"))

(defmethodL set-character-height ((p pgplot) (height number))
  (if (or (< height 0.0) (> height 50))
      (error "Bad font number ~A -- want 0<height<50" height))
  (setf (pgplot-character-height p) height)
  (if (eq p *current-pgplot*)
      (pgsch height))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric set-text-background-color (p c)
  (:documentation
   "set the text background color of pgplot device P to
C, where C is either an integer from the colormap, or a
symbol for named colors"))

(defmethodL set-text-background-color ((p pgplot) (ci integer))
  (setf (pgplot-text-background-color-index p) ci)
  (if (eq p *current-pgplot*)
      (pgstbg ci))
  t)
;;
(defmethodL set-text-background-color ((p pgplot) (cs symbol))
  (let* ((c-alist (cons '(:TRANSPARENT . -1) ;; add special transparent color
			(colormap-color-alist (pgplot-colormap p))))
	 (ci (cdr (assoc cs c-alist :test 'string-equal))))
    (if (not ci)
	(error "Color ~A not available.  Available colors are: ~A"
	       cs (map 'list #'car c-alist)))
    (set-text-background-color p ci)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

(defgeneric set-color (p c)
  (:documentation
   "set the plotting color of pgplot device P to
C, where C is either an integer from the colormap, or a
symbol for named colors"))

(defmethodL set-color ((p pgplot) (ci integer))
  (setf (pgplot-color-index p) ci)
  (if (eq p *current-pgplot*)
      (pgsci ci))
  t)

(defmethodL set-color ((p pgplot) (cs symbol))
  (let ((ci (cdr (assoc cs (colormap-color-alist (pgplot-colormap p))
			:test 'string-equal))))
    (if (not ci)
	(error "Color ~A not available.  Available colors are: ~A"
	       cs (map 'list #'car
		       (colormap-color-alist (pgplot-colormap p)))))
    (set-color p ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *fill-area-style-alist* '((:SOLID . 1) (:OUTLINE . 2)
				  (:HATCHED . 3) (:CROSS-HATCHED . 4)))

(defgeneric set-fill-area-style (p s)
  (:documentation
   "set the fill area style of pgplot device P to
S, where S is either an integer or one of
:SOLID :OUTLINE :HATCHED :CROSS-HATCHED"))

(defmethodL set-fill-area-style ((p pgplot) (style integer))
  (if (or (< style 0) (> style 4))
      (error "out of range fill area style ~A" style))
  (setf (pgplot-fill-area-style p) style)
  (if (eq p *current-pgplot*)
      (pgsfs style))
  t)

(defmethodL set-fill-area-style ((p pgplot) (style symbol))
  (let ((ns (cdr (assoc style *fill-area-style-alist* :test 'string-equal))))
    (if (not ns)
	(error "Invalid fill-area style ~A -- Available styles are ~A"
	       style (map 'list #'car  *fill-area-style-alist*)))
    (set-fill-area-style p ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *linestyle-alist* '((:SOLID . 1) (:DASHED . 2) (:DASH-DOT . 3)
			    (:DOTTED . 4) (:DASH-DOT-DOT-DOT . 5)))

(defgeneric set-line-style (p s)
  (:documentation
   "set the line style of pgplot device P to
S, where S is either an integer or one of
:SOLID :DASHED :DASHED-DOT :DOTTED :DASH-DOT-DOT-DOT"))
;;
(defmethodL set-line-style ((p pgplot) (ls integer))
  (if (or (< ls 1) (> ls 5))
      (error "out of range line style ~A" ls))
  (setf (pgplot-line-style p) ls)
  (if (eq p *current-pgplot*)
      (pgsls ls))
  t)
;;
(defmethodL set-line-style ((p pgplot) (lss symbol))
  (let ((ls (cdr (assoc lss *linestyle-alist* :test 'string-equal))))
    (if (not ls)
	(error "Invalid line style ~A -- Available styles are ~A"
	       lss (map 'list #'car  *linestyle-alist*)))
    (set-line-style p ls)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-line-width (p lw)
  (:documentation
   "set the line width pgplot device P to
integer value LW"))

(defmethodL set-line-width ((p pgplot) (lw integer))
  (if (or (< lw 1) (> lw 1000))
      (error "out of range line width ~A -- expect 1<=width<=1000" lw))
  (setf (pgplot-line-width p) lw)
  (if (eq p *current-pgplot*)
      (pgslw lw))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-arrow-head-style (p fs angle barb)
  (:documentation
   "set the arrow head style of pgplot.  FS can be 1, 2, :FILLED,
:OUTLINE.  ANGLE is the angle of the tip; BARB is the fraction of the
triangular arrow-head that is cut away from the back."))

(defmethodL set-arrow-head-style
  ((p pgplot) fs (angle real) (barb real))
  (cond ((eq fs :FILLED) (setq fs 1))
	((eq fs :OUTLINE) (setq fs 2))
	((and (integerp fs) (or (= fs 1) (= fs 2))) t)
	(t
	 (error
	  "Invalid arrow head style ~A. Available styles are ~A"
	  fs '(:FILLED :OUTLINE 1 2))))
  (setf angle (float angle 1.0))
  (setf barb (float barb 1.0))
  (setf (pgplot-arrow-head-style p) (list fs angle barb))
  (if (eq p *current-pgplot*)
      (pgsah fs angle barb))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-hatching-style
    (p angle sepn phase)
  (:documentation 
   "set hatching style of pgplot device P.
ANGLE is angle wrt horizontal.  SEPN is separation in
units of the view surface.  PHASE in [0,1] is the offset
within the separation"))

(defmethodL set-hatching-style ((p pgplot) (angle real) 
                               (sepn real) (phase real))
  (setf (pgplot-hatching-style p) (list angle sepn phase))
  (if (eq p *current-pgplot*)
      (pgshs angle sepn phase))
  t)

(defgeneric set-clipping-state (p state)
  (:documentation
   "set clipping state of pgplot device P.  STATE=1 means clip at
edge of viewport, 0 means don't clip"))


(defmethodL set-clipping-state ((p pgplot) (state integer))
  (cond ((or (= state 0) (= state 1))  t) ;; OK
	(t (error "Bad state in set-clipping-state")))
  (if (or (eq state t) (= state 1)) (setq state 1) (setq state 0))
  (setf (pgplot-clipping-state p) state)
  (if (eq p *current-pgplot*)
      (pgsclp state))
  t)


(defmacro with-pgplot-settings
    ((pgplot &key 
	     (font nil font%)
	     (character-height nil character-height%)
	     (text-background-color nil text-background-color%)
	     (color nil color%)
	     (fill-area-style nil fill-area-style%)
	     (line-style nil line-style%)
	     (line-width nil line-width%)
	     (arrow-head-style nil arrow-head-style%);; list (fs angle barb)
	     (hatching-style nil hatching-style%);; list (angle sepn phase)
	     (clipping-state nil clipping-state%))
     &body body)
  "Macro to run BODY with pgplot's  properties temporarily
set to given values.

 eg draw a blue box
  (with-pgplot-settings (pgplot :color :blue)
       (pgplot:box pgplot))

available settings are 
 :FONT :CHARACTER-HEIGHT :TEXT-BACKGROUND-COLOR :COLOR
 :FILL-AREA-STYLE :LINE-STYLE :CLIPPING-STATE
 :ARROW-HEAD-STYLE  (a list of fs,angle,barb)
 :HATCHING-STYLE    (a list of angle,sepn,phase)

whenever NIL is passed as as value (eg :font nil) in the
macroexpanded code at runtime, then this item is neither saved
nor restored, it being assumed that no value was specified.
"
  (let ((p (gensym "pgplot"))
	(sym-old-character-font
	 (when font% (gensym "old-character-font")))
	(sym-old-character-height
	 (when character-height% (gensym "old-character-height")))
	(sym-old-text-background-color
	 (when text-background-color (gensym "old-text-background-color")))
	(sym-old-color
	 (when color% (gensym "old-color")))
	(sym-old-fill-area-style
	 (when fill-area-style% (gensym "old-fill-area-style")))
	(sym-old-line-style
	 (when line-style% (gensym "old-line-style")))
	(sym-old-line-width
	 (when line-width% (gensym "old-line-width")))
	(sym-old-arrow-head-style
	 (when arrow-head-style% (gensym "old-arrow-head-style")))
	(sym-old-hatching-style
	 (when hatching-style% (gensym "old-hatching-style")))
	(sym-old-clipping-state
	 (when clipping-state% (gensym "old-clipping-state")))
	;;
	(sym-font
	 (when font% (gensym "character-font")))
	(sym-character-height
	 (when character-height% (gensym "character-height")))
	(sym-text-background-color
	 (when text-background-color% (gensym "text-background-color")))
	(sym-color
	 (when color% (gensym "color")))
	(sym-fill-area-style
	 (when fill-area-style% (gensym "fill-area-style")))
	(sym-line-style
	 (when line-style% (gensym "line-style")))
	(sym-line-width
	 (when line-width% (gensym "line-width")))
	(sym-arrow-head-style
	 (when arrow-head-style% (gensym "arrow-head-style")))
	(sym-hatching-style
	 (when hatching-style% (gensym "hatching-style")))
	(sym-clipping-state
	 (when clipping-state% (gensym "clipping-state"))))
  `(let*
    ,(append
      `((,p ,pgplot)) ;; bind pgplot to new var in case it is a complex form
      ;;
      ;; sym-font is variable evaluated version of FONT, to
      ;; prevent multiple-evaluation of FONT form.
      ;; sym-old-character-font is a variable for saving old version
      (when font%
	`((,sym-font ,font)
	  (,sym-old-character-font (when ,sym-font (pgplot-character-font ,p)))))
      (when character-height%
	`((,sym-character-height ,character-height)
	  (,sym-old-character-height
	   (when ,sym-character-height (pgplot-character-height ,p)))))
      (when text-background-color%
	`((,sym-text-background-color ,text-background-color)
	  (,sym-old-text-background-color
	   (when ,sym-text-background-color
	     (pgplot-text-background-color-index ,p)))))
      (when color%
	`((,sym-color ,color)
	  (,sym-old-color (when ,sym-color (pgplot-color-index ,p)))))
      (when fill-area-style%
	`((,sym-fill-area-style ,fill-area-style)
	  (,sym-old-fill-area-style
	   (when ,sym-fill-area-style (pgplot-fill-area-style ,p)))))
      (when line-style%
	`((,sym-line-style ,line-style)
	  (,sym-old-line-style
	   (when ,sym-line-style (pgplot-line-style ,p)))))
      (when line-width%
	`((,sym-line-width ,line-width)
	  (,sym-old-line-width
	   (when ,sym-line-width (pgplot-line-width ,p)))))
      (when arrow-head-style%
	`((,sym-arrow-head-style ,arrow-head-style)
	  (,sym-old-arrow-head-style
	   (when ,sym-arrow-head-style (pgplot-arrow-head-style ,p)))))
      (when hatching-style%
	`((,sym-hatching-style ,hatching-style)
	  (when ,sym-hatching-style (hatching-style (pgplot-hatching-style ,p)))))
      (when clipping-state%
	`((,sym-clipping-state ,clipping-state)
	  (clipping-state (when ,sym-clipping-state (pgplot-clipping-state ,p)))))
      )
    (unwind-protect
	 (progn
	   ,@(append
	     (when font%
	       `((when ,sym-font (set-character-font ,p ,sym-font))))
	     (when character-height%
	       `((when ,sym-character-height
		   (set-character-height ,p ,sym-character-height))))
	     (when text-background-color%
	       `((when ,sym-text-background-color
		   (set-text-background-color ,p ,sym-text-background-color))))
	     (when color%
	       `((when ,sym-color (set-color ,p ,sym-color))))
	     (when fill-area-style% 
	       `((when ,sym-fill-area-style
		   (set-fill-area-style ,p ,sym-fill-area-style))))
	     (when line-style%
	       `((when ,sym-line-style (set-line-style ,p ,sym-line-style))))
	     (when line-width%
	       `((when ,sym-line-width (set-line-width ,p ,sym-line-width))))
	     (when arrow-head-style% ;; this one takes several args in list
	       `((when ,sym-arrow-head-style
		   (apply 'set-arrow-head-style ,p ,sym-arrow-head-style))))
	     (when hatching-style% ;; this one takes several args in list
	       `((when ,sym-hatching-style
		   (apply set-hatching-style ,p ,sym-hatching-style))))
	     (when clipping-state%
		  `((when ,sym-clipping-state
		      (set-clipping-state ,p ,sym-clipping-state)))))
	   ;; now run the body
	   ,@body)
	   ;; and restore the old values
	   (progn
	     ,@(append
		(when font
		  `((when ,sym-font (set-character-font ,p ,sym-old-character-font))))
		(when character-height%
		  `((when ,sym-character-height
		     (set-character-height ,p ,sym-old-character-height))))
		(when text-background-color%
		  `((when ,sym-text-background-color
		      (set-text-background-color
		       ,p ,sym-old-text-background-color))))
		(when color%
		  `((when ,sym-color
		      (set-color ,p ,sym-old-color))))
		(when fill-area-style% 
		  `((when ,sym-fill-area-style
		      (set-fill-area-style ,p ,sym-old-fill-area-style))))
		(when line-style%
		  `((when ,sym-line-style
		      (set-line-style ,p ,sym-old-line-style))))
		(when line-width%
		  `((when ,sym-line-width
		      (set-line-width ,p ,sym-old-line-width))))
		(when arrow-head-style% ;; this one takes several args in list
		  `((when ,sym-arrow-head-style
		      (apply 'set-arrow-head-style ,p ,sym-old-arrow-head-style))))
		(when hatching-style% ;; this one takes several args in list
		  `((when ,sym-hatching-style
		      (apply set-hatching-style ,p ,sym-old-hatching-style))))
		(when clipping-state%
		  `((when ,sym-clipping-state
		      (set-clipping-state ,p ,sym-old-clipping-state)))))
		))))) 
    
			       
				       



(defgeneric set-color-index-range (p ilo ihi)
  (:documentation  
   "Set the color index rangle of pgplot device P to be ILO..IHI.
This is used for drawing imagemaps or greymaps"))

(defmethodL set-color-index-range ((p pgplot) (ilo integer) (ihi integer))
  (setf (pgplot-color-index-range p) (list ilo ihi))
  (if (eq p *current-pgplot*)
      (pgscir ilo ihi))
  t)


(defgeneric set-viewport (p xleft xright ybot ytop &key viewport)
  (:documentation
   "Set the viewport of device P.  This is the region in absolute
coordinates over which plotting is active.  XLEFT, XRIGHT, YBOT, YTOP
are all in [0,1].  If VIEWPORT is given, then the coordinates given are 
relative to it, and not the absolute frame boundaries 0..1"))

(defmethodL set-viewport ((p pgplot) 
			  (xleft real) (xright real)
			  (ybot real) (ytop real) 
			  &key (viewport '(0.0 1.0 0.0 1.0)))

  (let ((x0vp (elt viewport 0))
	(x1vp (elt viewport 1))
	(y0vp (elt viewport 2))
	(y1vp (elt viewport 3)))
    (let ((xx0 (+ x0vp (* xleft (- x1vp x0vp))))
	  (xx1 (+ x0vp (* xright (- x1vp x0vp))))
	  (yy0 (+ y0vp (* ybot (- y1vp y0vp))))
	  (yy1 (+ y0vp (* ytop (- y1vp y0vp)))))
      
      (setf (pgplot-viewport p) (list xx0 xx1 yy0 yy1))
      (if (eq p *current-pgplot*)
	  (pgsvp xx0 xx1 yy0 yy1))
      t)))



(defgeneric set-window (p x1 x2 y1 y2)
  (:documentation
   "Set the window (user coordinates) of pgplot device P within the
current viewport.  X1,X2,Y1,Y2 are the user coordinates to map
to the viewport."))

(defmethodL set-window ((p pgplot) (x1 real) (x2 real)
		       (y1 real) (y2 real))
  (ensure-type 'single-float x1 x2 y1 y2)
  (setf (pgplot-window p) (list x1 x2 y1 y2))
  (if (eq p *current-pgplot*)
      (pgswin x1 x2 y1 y2))
  t)


(defgeneric move-to (p x y) 
  (:documentation "For pgplot device P, move to user coordinates X,Y")) 
;;
(defmethodL move-to ((p pgplot) (x real) (y real)) 
  (setf (pgplot-pen-position p) (list x y))
  (if (eq p *current-pgplot*)
      (pgmove x y))
  t)  

(defgeneric box
  (p &key
   size font color
   x-axis y-axis 
   x-bottom x-top
   y-left y-right 
   x-grid y-grid 
   x-invert-ticks y-invert-ticks 
   x-log y-log 
   x-num-labels-bottom  y-num-labels-left 
   x-num-labels-top     y-num-labels-right 
   x-project-ticks y-project-ticks
   x-major-ticks y-major-ticks 
   x-minor-ticks  y-minor-ticks 
   y-vertical-num-labels 
   x-force-decimal y-force-decimal 
   x-force-exponential y-force-exponential 
   x-tick y-tick  ;; automatic by default
   nxsub nysub)
  (:documentation
   "Draw a box for pgplot device P.
Keywords are:
  SIZE             -  size used for labels (real)
  FONT             -  which font to use (number or symbol)
  X-AXIS           -  draw central x axis?   (default NIL)
  Y-AXIS           -  draw central y axis?   (default NIL)
  X-BOTTOM, X-TOP  -  draw x axes            (default T)
  Y-LEFT, Y-RIGHT  -  draw y axis            (default T)
  X-GRID, Y-GRID   -  draw grids in X,Y      (default NIL)
  X-INVERT-TICKS, Y-INVERT-TICKS - ticks project outward (default NIL)
  X-LOG, Y-LOG     -  log axes               (default NIL)
  X-NUM-LABELS-BOTTOM, Y-NUM-LABELS-LEFT - numerical axes labels (default NIL)
  X-NUM-LABELS-TOP, Y-NUM-LABELS-RIGHT   - numerical axes labels (default T)
  X-PROJECT-TICKS, Y-PROJECT-TICKS - project ticks outward, ignored if
                                     X-INVERT-TICKS set (default NIL)
  X-MAJOR-TICKS, Y-MAJOR-TICKS  - draw big interval ticks   (default T)
  X-MINOR-TICKS, Y-MINOR-TICKS  - draw small interval ticks (default T)
  Y-VERTICAL-NUM-LABELS  - make y axes labels normal vertical text (default T)
  X-FORCE-DECIMAL, Y-FORCE-DECIMAL - force axis label to be of form 1.234
                                     (default NIL)
  X-FORCE-EXPONENTIAL, Y-FORCE-EXPONENTIAL - force exponential notation for
                                             axis labels (default NIL)
  X-TICK, Y-TICK   -  world coordinate interval between major tick marks,
                      0 means choose automatically (default 0)
  NXSUB, NYSUB     -  the number of subintervals to divide the major
                      coordinate interval into, 0 means choose automatically
                      (default 0)
"))
  
(defmethodL box ((p pgplot) &key
		(size nil) (font nil) (color nil) (line-width nil)
		(x-axis nil) (y-axis nil)
		(x-bottom t) (x-top t)
		(y-left t) (y-right t)
		(x-grid nil) (y-grid nil)
		(x-invert-ticks nil) (y-invert-ticks nil)
		(x-log nil) (y-log nil)
		(x-num-labels-bottom t)   (y-num-labels-left t)
		(x-num-labels-top nil)    (y-num-labels-right  nil)
		(x-project-ticks nil) (y-project-ticks nil)
		(x-major-ticks t) (y-major-ticks t)
		(x-minor-ticks t) (y-minor-ticks t)
		(y-vertical-num-labels t)
		(x-force-decimal nil) (y-force-decimal nil)
		(x-force-exponential nil) (y-force-exponential nil)
		(x-tick 0.0) (y-tick 0.0) ;; automatic by default
		(nxsub 0) (nysub 0))
  ;;
  (activate-device p)
  (let ((xopt (concatenate 'string
			   (if x-axis "A" "")
			   (if x-bottom "B" "")
			   (if x-top "C" "")
			   (if x-grid "G" "")
			   (if x-invert-ticks "I" "")
			   (if x-log "L" "")
			   (if x-num-labels-bottom "N" "")
			   (if x-num-labels-top    "M" "")
			   (if x-project-ticks "P" "")
			   (if x-major-ticks "T" "")
			   (if x-minor-ticks "S" "")
			   (if x-force-decimal "1" "")
			   (if x-force-exponential "2" "")))
	(yopt (concatenate 'string
			   (if y-axis "A" "")
			   (if y-left "B" "")
			   (if y-right "C" "")
			   (if y-grid "G" "")
			   (if y-invert-ticks "I" "")
			   (if y-log "L" "")
			   (if y-num-labels-left "N" "")
			   (if y-num-labels-right  "M" "")
			   (if y-project-ticks "P" "")
			   (if y-major-ticks "T" "")
			   (if y-minor-ticks "S" "")
			   (if y-vertical-num-labels "V" "")
			   (if y-force-decimal "1" "")
			   (if y-force-exponential "2" ""))))
    ;;
    (with-pgplot-settings (p :font font :character-height size
			     :color color :line-width line-width)
      (pgbox xopt x-tick nxsub yopt y-tick nysub))
    ;; it appears that in log-axis mode, pgplot changes the character height
    ;; on its own, messing up our understanding of current character height.
    ;; the macro with-pgplot-settings doesn't fix this, because it was never
    ;; 'officially' in our code
    (set-character-height p (get-character-height p))
    (fix-pen-position p)
    t))


(defgeneric plain-box (p &key color line-width x-axis y-axis x-bottom x-top
			   y-left y-right x-grid y-grid)
  (:documentation   "Draw a box without ticks or numbers"))

(defmethodL plain-box ((p pgplot) &key 
		       (color nil) (line-width nil)
		       (x-axis nil) (y-axis nil)
		       (x-bottom t) (x-top t)
		       (y-left t) (y-right t)
		       (x-grid nil) (y-grid nil))
  (box p :color color :line-width line-width  
       :x-axis x-axis :y-axis y-axis 
       :x-bottom x-bottom :x-top x-top
       :y-left y-left :y-right y-right
       :x-grid x-grid :y-grid y-grid
       :x-num-labels-bottom nil :y-num-labels-left nil
       :x-major-ticks nil :x-minor-ticks nil
       :y-major-ticks nil :y-minor-ticks nil))
       

(defgeneric plotlabel
    (p xlabel ylabel toplabel)
  (:documentation
   "Label pgplot device P with XLABEL, YLABEL, TOPLABEL.
Labels are not well placed, and (pgplot:xlabel p \"label\")
is preferred."))

(defmethodL plotlabel
    ((p pgplot) (xlabel string) (ylabel string) (toplabel string))
  (activate-device p)
  (pglab xlabel ylabel toplabel)
  (fix-pen-position p)
  t)



(defgeneric pgmtxt (p side disp coord fjust text)
  (:documentation
   "A simple front end to the PGMTXT PGPLOT routine

SIDE   = 'B', 'L', 'T',or 'R'; if it includes 'LV' 
         text is perpendicular to frame.
DISP   = displacment (+/-) relative to viewport in chars
COORD  = location of string along viewport (0.5=center)
FJUST  = justification relative to COORD.   0 means left 
         justified, 1 is right justified, 0.5 means 
         centered, and other values are not useful.
TEXT   = string to be written."))

(defmethodL pgmtxt ((p pgplot) side disp coord fjust text)
  (declare (type string side)
	   (type real disp)
	   (type (real 0.0 1.0) coord)
	   (type (real 0.0 1.0) fjust)
	   (type string text))
  (activate-device p)
  (pgmtxt-raw side (float disp 1.0) (float coord 1.0) (float fjust 1.0) text))
   


;; is m a 2x3 numerical matrix?  local to pgplot
(defunL 2x3-numerical-matrix-p (m) 
  (and (arrayp m) (equalp '(2 3) (array-dimensions m)))
  (realp (aref m 0 0))  (realp (aref m 1 0))
  (realp (aref m 0 1))  (realp (aref m 1 1))
  (realp (aref m 0 2))  (realp (aref m 1 2)))

;; return min and max of 2d matrix a, ignoring values set to ignore-value
(defunL 2d-array-min-max (a ignore-value)
  (let ((amin nil) (amax nil) (aa 0.0))
    (dotimes (i (array-dimension a 0))
      (dotimes (j (array-dimension a 1))
	(setf aa (aref a i j))
	(if (or (not ignore-value) (not (= aa ignore-value)))
	    (progn
	     (if (or (not amin) (< aa amin)) (setf amin aa)
	       (if (or (not amax) (> aa amax)) (setf amax aa)))))))
    (values amin amax)))
      
;; to-single-float is defined in pgplot-ffi.lisp

(defun convert-array-to-single-float-array (a)
  (cond ((typep a '(simple-array single-float (* *)))
	 a)
	(t
	 (let* ((ni (array-dimension a 0))
		(nj (array-dimension a 1))
		(v  (make-array (list ni nj) :element-type 'single-float :initial-element 0.0)))
	   (declare (type (unsigned-byte 28) ni nj))
	   (dotimes (i  ni)
	     (dotimes (j nj)
	       (declare (type (unsigned-byte 28) i j))
	       (setf (aref v i j) (to-single-float (aref a i j)))))
	   v))))


;; for CONTOUR and IMAGE convert transformation matrix to tv one-dim array
(defun %convert-transformation-matrix-to-tv (transformation-matrix tv)
  (declare (type (array * (2 3)) transformation-matrix)
	   (type (simple-array single-float (6)) tv))
  
  (setf (aref tv 1) (to-single-float (aref transformation-matrix 0 1)))
  (setf (aref tv 2) (to-single-float (aref transformation-matrix 0 2)))
  (setf (aref tv 4) (to-single-float (aref transformation-matrix 1 1)))
  (setf (aref tv 5) (to-single-float (aref transformation-matrix 1 2)))
  
  ;; tv elements 0,3 need to be adjusted to take into acct fact that
  ;; fortran arrays start at 1;  ie, we need fortran indices 1,1
  ;; to transform to same x,y as lisp indices 0,0
  (setf (aref tv 0) (to-single-float   ;; x0-adj
		     (- (aref transformation-matrix 0 0)    ;; x0
			(aref transformation-matrix 0 1)))) ;; dx
  (setf (aref tv 3) (to-single-float   ;; y0-adj
		     (- (aref transformation-matrix 1 0)    ;; y0
			(aref transformation-matrix 1 2)))) ;; dy
  
  
  (setf (aref tv 0) (to-single-float   ;; x0-adj
		     (- (aref transformation-matrix 0 0)    ;; x0
			(aref tv 1) 
			(aref tv 2))))
  (setf (aref tv 3) (to-single-float   ;; y0-adj
		     (- (aref transformation-matrix 1 0)    ;; y0
			(aref tv 4)
			(aref tv 5)))))
  


(defgeneric contour (p a &key
		    contours   ;; vector of contours
		    n-contours
		    min-contour max-contour
		    transformation-matrix
		    i-min-index i-max-index
		    j-min-index j-max-index
		    blank-value ;; value of A at which contour undefined
		    line-width color line-style 
		    contour-labels ;;vector of contour-labels, NIL ignored
		    intval ;;spacing of contour labels, in cells
		    minint) ;;contours that cross <minint cells not labeled
 (:documentation
  "Create a contour plot of array A[i,j].  Contours are made at locations in
vector CONTOURS, but if this is NIL then N-CONTOURS evenly spaced contours
between optional MIN-CONTOUR and MAX-CONTOUR are made instead.
TRANSFORMATION-MATRIX (as described in pgplot docs) transforms array i,j to
user coordinates.  I-MIN-INDEX...etc are the ranges within A to use;
by default the whole array is used.   CONTOUR-LABELS is an optional
vector of contour labels corresponding to CONTOURS containing labels,
and INTVAL and MININT specify when to label contours, as described
in pgplot manual.  BLANK-VALUE is an optional out-of-range value which
should not be used when finding MIN/MAX of A[i,j].

For the data array A[I,J], the default coordinate system has
X increasing with J, and Y increasing with I, rescaled to fit
exactly within the user coordinates.   If used, the
transformation matrix (TR) changes this to:
  X = TR[0,0] + TR[0,1]*J + TR[0,2]*I
  Y = TR[1,0] + TR[1,1]*J + TR[1,2]*I
"))
;;
(defmethodL contour ((p pgplot) (a array) &key
		    (contours nil)  ;; vector of contours
		    (n-contours 10) 
		    (min-contour nil) (max-contour nil)
		    (transformation-matrix nil)
		    (i-min-index nil) (i-max-index nil)
		    (j-min-index nil) (j-max-index nil)
		    (blank-value nil) ;; value of A at which contour undefined
		    (line-width nil) (color nil) (line-style nil)
		    (contour-labels nil);;vector of contour-labels, NIL ignored
		    (intval 20);;spacing of contour labels, in cells
		    (minint 10));;contours that cross <minint cells not labeled
  "Create a contour plot of array A[i,j].  Contours are made at locations in
vector CONTOURS, but if this is NIL then N-CONTOURS evenly spaced contours
between optional MIN-CONTOUR and MAX-CONTOUR are made instead.
TRANSFORMATION-MATRIX (as described in pgplot docs) transforms array i,j to
user coordinates.  I-MIN-INDEX...etc are the ranges within A to use;
by default the whole array is used.   CONTOUR-LABELS is an optional
vector of contour labels corresponding to CONTOURS containing labels,
and INTVAL and MININT specify when to label contours, as described
in pgplot manual.  BLANK-VALUE is an optional out-of-range value which
should not be used when finding MIN/MAX of A[i,j].

For the data array A[I,J], the default coordinate system has
X increasing with J, and Y increasing with I, rescaled to fit
exactly within the user coordinates.   If used, the
transformation matrix (TR) changes this to:
  X = TR[0,0] + TR[0,1]*J + TR[0,2]*I
  Y = TR[1,0] + TR[1,1]*J + TR[1,2]*I
"
  (assert (= (array-rank a) 2))
  ;; fix  i-min-index, etc
  (let ((nx (+ -1 (array-dimension a 0)))
	(ny (+ -1 (array-dimension a 1))))
    (declare (type (unsigned-byte 28) nx ny))
    (if (not i-min-index) (setf i-min-index 0))
    (if (not j-min-index) (setf j-min-index 0))
    (if (not i-max-index) (setf i-max-index nx)) 
    (if (not j-max-index) (setf j-max-index ny))
    (if (or (< i-min-index 0) (> i-min-index nx)
	    (< j-min-index 0) (> j-min-index ny))
	(error "contour: bad min/max indices")))

  (if (and transformation-matrix
	   (not (2x3-numerical-matrix-p a)))
      (error "contour: transformation matrix ~A not a 2x3 matrix" 
	     transformation-matrix))
  ;;
  (let ((tv (make-array 6 :element-type 'single-float :initial-element 0.0))
	;; convert array NOW because we may need to convert it many times
	(af (convert-array-to-single-float-array a)) 
	(cvec nil)    ;; vector of contours
	(minc nil)    ;; min contour
	(maxc nil))   ;; max contour
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; build transformation vector 
    (if (not transformation-matrix)
	;; if the transformation matrix is undefined, make the
	;; array fit into the current coordinate box
	(let* ((xmin (first   (pgplot-window p)))
	       (xmax (second  (pgplot-window p)))
	       (ymin (third   (pgplot-window p)))
	       (ymax (fourth  (pgplot-window p)))
	       (nx (array-dimension a 1)) ;; note flip
	       (ny (array-dimension a 0)) ;;  of x,y
	       (dx (/  (- xmax xmin) (- nx 1.0)))
	       (dy (/  (- ymax ymin) (- ny 1.0))))
	  (setf (aref tv 0) (to-single-float (- xmin dx)))
	  (setf (aref tv 3) (to-single-float (- ymin dy)))
	  (setf (aref tv 1) (to-single-float dx))
	  (setf (aref tv 2) 0.0)
	  (setf (aref tv 4) 0.0)
	  (setf (aref tv 5) (to-single-float dy)))
        ;;
        ;; if tranformation matrix is defined, use it for tv
	(%convert-transformation-matrix-to-tv transformation-matrix tv))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; now compute contour levels
    (if contours
	(progn
	  (setf cvec contours)  ;; contour vector given, so ignore all else
	  (setf n-contours (length contours)))
      (progn ;; if contours not given, compute them
	(setf minc min-contour)
	(setf maxc max-contour)
	;; if min-contour and max-contour were not defined, then use A
	(if (not (and minc maxc))
	    (multiple-value-bind (mina maxa) (2d-array-min-max a blank-value)
	      (if (not minc) (setf minc mina))
	      (if (not maxc) (setf maxc maxa))))
	;;
	(if (not (and minc maxc))
	    (error
	     (concatenate
	      'string
	      "contour: could not find min/max elements in array"
	      " maybe all the array elements are the blank-value?")))
	;;
	(setf minc (to-single-float minc))
	(setf maxc (to-single-float maxc))
	(setf cvec (make-array n-contours :element-type 'single-float :initial-element 0.0))
	(dotimes (i n-contours)
	  (declare (type (unsigned-byte 28) i))
	  (setf (aref cvec i)
		(to-single-float
		 (+ minc (* 1.0s0 (/ (- maxc minc) (- n-contours 1.0s0))  i))
		 )))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; do the contour plot
    (activate-device p)
    (with-pgplot-settings (p :line-width line-width :color color
			     :line-style line-style)
      (pgcont af
	      i-min-index i-max-index
	      j-min-index j-max-index
	      cvec (- n-contours) ;; minus for solid lines
	      tv)
      ;;
      ;; now do contour-labels
      (when contour-labels
	(loop for contour across cvec
	      for contour-label across contour-labels
	      for contour-label-string = (if (stringp contour-label)
					     contour-label
					     (format nil "~A" contour-label))
	      do
	      (when contour-label ;; NIL labels are ignored
		(pgconl af
			i-min-index i-max-index
			j-min-index j-max-index
			contour
			tv 
			contour-label-string 
			intval minint)))))
    (fix-pen-position p)
    t)) 


(defgeneric connect (p xvec yvec &key line-width color line-style)
  (:documentation
    "For pgplot device P, draw lines connecting pairs in XVEC, YVEC
with LINE-WIDTH (integer), COLOR (integer or symbol), and LINE-STYLE
 (integer or symbol)"))

(defmethodL connect ((p pgplot) (xvec vector) (yvec vector)
		    &key (line-width nil) (color nil) (line-style nil))
 
  (activate-device p)
  (with-pgplot-settings (p :line-width line-width :line-style line-style
			   :color color)
	  (pgline (min (length xvec) (length yvec)) xvec yvec))
  (fix-pen-position p)
  t)

(defgeneric errorbars (p xvec yvec evec &key direction size line-width
					  color line-style)
  (:documentation
   "For pgplot device P, draw errorbars at points represented by pairs
in XVEC, YVEC, of magnitude of points in vector EVEC.
DIRECTION can be :X :Y :PLUS-X :PLUS-Y :MINUS-X :MINUS-Y (default is :Y)
LINE-WIDTH is an integer, COLOR is an integer or symobl, and
LINE-STYLE is an integer or symbol."))

(defmethodL errorbars ((p pgplot) (xvec vector) (yvec vector) (evec vector)
		      &key (direction :y) (size 1.0) (line-width nil)
		      (color nil) (line-style nil))
 
  (activate-device p)
  (let ((n (min (length xvec) (length yvec) (length evec)))
	(ndir-list (case direction
		     (:x '(1 3)) (:y '(2 4))
		     (:plus-x '(1)) (:plus-y '(2)) (:minus-x '(3)) (:minus-y '(4)))))
    (if (not ndir-list)
	(error
            "Bad direction in errorbar: allowed directions are :x :y :plus-x :plus-y :minus-x :minus-y"))
    (with-pgplot-settings (p :line-width line-width :line-style line-style
			     :color color :character-height 1.0)
      (loop for ndir in ndir-list
	 do (pgerrb ndir n xvec yvec evec size)))
    (fix-pen-position p)
    t))

(defmethodL errorbars ((p pgplot) (x real) (y real) (e real)
		      &key (direction :y) (size 1.0) (line-width nil)
		      (color nil) (line-style nil))
  "For pgplot device P, draw errorbars at points represented by point
X, Y, of magnitude ERR.  DIRECTION can
be :X :Y :PLUS-X :PLUS-Y :MINUS-X :MINUS-Y (default is :Y) LINE-WIDTH
is an integer, COLOR is an integer or symobl, and LINE-STYLE is an
integer or symbol."
  (errorbars p (vector x) (vector y) (vector e)
	     :direction direction :size size :line-width line-width
	     :color color :line-style line-style))


(defgeneric erase (p)
  (:documentation   "Erase a pgplot device P"))

(defmethodL erase ((p pgplot))
  (activate-device p)
  (pgeras-raw)
  (pgpage-raw)  ;; this allows a window resize to take effect
  (fix-pen-position p)
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *point-symbol-alist*
  '((:OPEN-SQUARE . 0) (:DOT . 1) (:PLUS . 2) (:ASTERISK . 3)
    (:CIRCLE . 4) (:TIMES . 5)
    (:OPEN-TRIANGLE . 7) (:CIRCLE-PLUS . 8) (:CIRCLE-DOT . 9)
    (:SQUARE-WITH-CORNER-DOTS . 10) (:DIAMOND . 11) (:OPEN-STAR . 12)
    (:OPEN-PLUS . 14) (:STAR-OF-DAVID . 15) (:FILLED-SQUARE . 16)
    (:SPOT . 17)     (:FILLED-CIRCLE . 17) ;; two names
    (:FILLED-STAR . 18) (:LARGER-SQUARE . 19)
    (:CIRCLE-1 . 20) (:CIRCLE-2 . 21) (:CIRCLE-3 . 22) (:CIRCLE-4 . 23)
    (:CIRCLE-5 . 24) (:CIRCLE-6 . 25) (:CIRCLE-7 . 26) (:CIRCLE-8 . 27)
    (:LEFT-ARROW . 28) (:RIGHT-ARROW . 29) (:UP-ARROW . 30) (:DOWN-ARROW . 31)
    (:POINT . -1)
    ;; warning - the documentation says that pgplot obeys fill area style
    ;; but that does not seem to be the case
    (:FILLED-TRIANGLE . -3) (:FILLED-DIAMOND . -4)
    (:FILLED-PENTAGON . -5) (:FILLED-HEXAGON . -6) (:FILLED-SEPTAGON . -7)
    (:FILLED-OCTAGON . -8)
    ;; sun and  planets in the Hershey set
    (:sun . 2281) (:mercury . 2282) (:venus . 2283) (:earth . 2284)
    (:mars . 2285) (:jupiter . 2286) (:saturn . 2287) (:uranus . 2288)
    (:neptune . 2289)))


	
	
	     


(defgeneric points (p x y symbol &key size  color border-color)
  (:documentation
   "In pgplot device P draw SYMBOLS at X,Y (vector or real).
SYMBOL may be a symbol in pgplot::*point-symbol-alist* or
an integer.  SIZE is a real, and COLOR is an integer or symbol.

If BORDER-COLOR is set, then draw a border around the symbol
in this color.  This works only for symbols like :FILLED-SQUARE
that have a matching symbol :OPEN-SQUARE."))

(defmethodL points ((p pgplot) x y (symbol integer)
		    &key (size nil) (color nil) (border-color nil))
  (activate-device p)
  (if (or (> symbol 2932)
	  (< symbol -8))
      (error "Bad point symbol ~A.  Points must be in range -8 to 2132"
	     symbol))

  (flet ((do-points (symbol)
	   (cond ((and (vectorp x) (vectorp y))
		  (pgpt (min (length x) (length y)) x y symbol))
		 ((and (realp x) (realp y))
		  (pgpt 1 (vector x) (vector y) symbol))
		 (t
		  (error "Invalid or mismatched x,y types in points")))))

    ;; fill-area-style does nothing here, contrary to PGPLOT
    ;; documentation

    ;; if we're drawing a border around (only for certain points) then
    ;; first draw a large background color symbol, then draw a smaller
    ;; colored one on top
    (let* ((doing-border (and border-color (member symbol '(-3 -4 -5 -6 -7 -8
							    16 17 18))))
	   (size (or size (get-character-height p)))
	   (true-size (* size (if doing-border  0.80 1.0))))

      ;; background symbol 
      (when doing-border
	(with-pgplot-settings (p :character-height size
				 :color border-color
				 :fill-area-style :solid)
	  (do-points symbol)))
    
      ;; colored symbol on top
      (with-pgplot-settings (p :character-height true-size
			       :color color
			       :fill-area-style :solid)
	(do-points symbol)))
      
    (fix-pen-position p)
    t))

(defmethodL points ((p pgplot) x y (symbol symbol)
		    &key (size nil) (color nil) (border-color nil))
  (let ((ns (cdr (assoc symbol *point-symbol-alist* :test 'string-equal))))
    (if (not ns)
	(error
	 "Invalid point symbol ~A -- Available symbols are ~%~A ~%~A"
	  symbol (map 'list #'car  *point-symbol-alist*)
	  "Symbols may also be numbers 32-127 (ASCII) or 128-2132 (Hershey)"))
    (points p x y ns :size size :color color :border-color border-color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert world coordinates to device coordinates	
(defgeneric world-to-device  (p x y)
    (:documentation
     "For pgplot device P convert world coordinates X,Y (either
reals or vectors) to device coordinates in [0,1]"))   
	   
(defmethodL world-to-device ((p pgplot) (x real) (y real))
  (let* ((vp (pgplot-viewport p))
	 (w  (pgplot-window   p))
	 (vx1 (first vp)) (vx2 (second vp)) (vy1 (third vp)) (vy2 (fourth vp))
	 (wx1 (first w))  (wx2 (second w))  (wy1 (third w))  (wy2 (fourth w)))
    (values
     (+ vx1 (* (/ (- x wx1) (- wx2 wx1)) (- vx2 vx1)))
     (+ vy1 (* (/ (- y wy1) (- wy2 wy1)) (- vy2 vy1))))))

;; for vectors
(defmethodL world-to-device ((p pgplot) (x vector) (y vector))
  (if (not (= (length x) (length y)))
      (error "Unequal vectors in  world-to-device"))
  (let ((xx 0.0) (yy 0.0)
	(xv (make-array (length x) :element-type 'single-float  :initial-element 0.0))
	(yv (make-array (length x) :element-type 'single-float  :initial-element 0.0)))
    (dotimes (i (length x))
      (multiple-value-setq (xx yy) (world-to-device p (aref x i) (aref y i)))
      (setf (aref xv i) (to-single-float xx))
      (setf (aref yv i) (to-single-float yy)))
    (values xv yv)))

;; convert device coordinates to world coordinates
(defgeneric device-to-world  (p x y)
    (:documentation
     "For pgplot device P convert device coordinates X,Y 
reals or vectors) to user coordinates"))   
	   
(defmethodL device-to-world ((p pgplot) (x real) (y real))
  (let* ((vp (pgplot-viewport p))
	 (w  (pgplot-window   p))
	 (vx1 (first vp)) (vx2 (second vp)) (vy1 (third vp)) (vy2 (fourth vp))
	 (wx1 (first w))  (wx2 (second w))  (wy1 (third w))  (wy2 (fourth w)))
    (values
     (+ wx1 (* (/ (- x vx1) (- vx2 vx1)) (- wx2 wx1)))
     (+ wy1 (* (/ (- y vy1) (- vy2 vy1)) (- wy2 wy1))))))

;; this time for vectors
(defmethodL device-to-world ((p pgplot) (x vector) (y vector))
  (if (not (= (length x) (length y)))
      (error "Unequal vectors in  device-to-world"))
  (let ((xx 0.0) (yy 0.0)
	(xv (make-array (length x) :element-type 'single-float :initial-element 0.0))
	(yv (make-array (length x) :element-type 'single-float :initial-element 0.0)))
    (dotimes (i (length x))
      (multiple-value-setq (xx yy) (device-to-world p (aref x i) (aref y i)))
      (setf (aref xv i) xx)
      (setf (aref yv i) yy))
    (values xv yv)))
    

(defgeneric write-text (p text x y &key fjust angle device-coords
				     color character-height font line-width
				     center
				     shadow shadow-color shadow-scale)
  (:documentation
   "For pgplot device P, write text string STRING at location x,y,
FJUST is justification (0=left justification at x,y; 1=right justified).
ANGLE is angle counterclockwise from horizontal.
COLOR is a symbol or integer;  
CHARACTER-HEIGHT is a real;
FONT is a symbol or integer
LINE-WIDTH is NIL or an integer
CENTER is T/NIL whether the text should be centered at X,Y. Otherwise
  it starts at X,Y.  Unlike FJUST, CENTER adjusts up/down as well 
  as left/right.
SHADOW is T if a shadow version of text is desired under the text, for example to 
  make the text visible against a background image
  -  SHADOW-COLOR is the color of the shadow
  -  SHADOW-SCALE is the multiple of the text line-width to use for the shadow
"))

(defmethodL write-text ((p pgplot) (text string) (x real) (y real) &key
			(fjust 0.0) (angle 0.0) (device-coords nil)
			(color nil) (character-height 1.0) (font nil)
			(line-width nil)
			(center nil)
			(shadow nil) (shadow-color :background) (shadow-scale 6))
 
  ;; if device coords non-nil, assume x,y are really in device coordinates
  ;; and transform to world coords
  (if device-coords (multiple-value-setq (x y) (device-to-world p x y)))
  (activate-device p)
  (let ((xx x) (yy y)
	(%line-width (or line-width (get-line-width p))))
    (when center
      (multiple-value-bind (xvec yvec)
	  (text-bounding-box p text :font font :angle angle :x0 0 :y0 0
			     :character-height character-height)
	;; decrement position x,y by mean position of bounding box
	(loop for xc across xvec and yc across yvec
	      do  
		 (incf xx (* -0.25 xc)) ;; 0.25 for 4 corners
		 (incf yy (* -0.25 yc)))))
    ;;
    ;; if shadow text is enabled, write text under the real text but
    ;; with a thicker line width
    (when shadow
      (with-pgplot-settings (p :color shadow-color 
			       :character-height character-height
			       :line-width (ceiling (* shadow-scale %line-width))
			       :font font)
	(pgptxt xx yy angle fjust text)))
    ;;
    (with-pgplot-settings (p :color color 
			     :character-height character-height
			     :line-width %line-width
			     :font font)
      (pgptxt xx yy angle fjust text)))
  ;;
  (fix-pen-position p)
  t)
  
;; return width and height of string in units where plot has width 1.
;(defmethod string-size ((p pgplot) (text string))
;  (multiple-value-bind (xlen ylen) (pglen 2 text)
;    (values (/ xlen (pgplot-width p))
;	    (/ (* (pgplot-aspect-ratio p) ylen (pgplot-width p))))))

;; return two vectors representing bounding box of text

(defgeneric text-bounding-box (p text &key x0 y0 angle fjust
					device-coords character-height font)
  (:documentation
   "Like WRITE-TEXT but returns two vectors representing the bounding
box the text would occupy if printed. Returns (VALUES XVEC YVEC) where
each vector is of length 4, and contains 4 corners of bounding box:
lower left, upper left, upper right, lower right"))

(defmethodL text-bounding-box  ((p pgplot) (text string) 
			       &key (x0 0.0) (y0 0.0) (angle 0.0) (fjust 0.0)
			       (device-coords nil)
			       (character-height nil) (font nil))

  (if (or (eq device-coords t) (eq device-coords :in))
      (multiple-value-setq (x0 y0) (device-to-world p x0 y0)))
  (activate-device p)
  (let ((xvec (make-array 4 :element-type 'single-float :initial-element 0.0))
	(yvec (make-array 4 :element-type 'single-float :initial-element 0.0)))
    (with-pgplot-settings (p :character-height character-height :font font)
      (pgqtxt x0 y0 angle fjust text xvec yvec))
    (if (or (eq device-coords t) (eq device-coords :out))
	(world-to-device p xvec yvec) ;; back to device coords
	(values xvec yvec))))


(defgeneric xlabel (p text &key x-offset y-offset
			     color character-height font)
  (:documentation
   "For pgplot device P, draw x axis label TEXT, nudging it
by X-OFFSET, Y-OFFSET in device coordinates.
If used, COLOR is an integer or symbol, CHARACTER-HEIGHT is a real,
and FONT is an integer or symbol"))
  

(defmethodL xlabel  ((p pgplot) (text string) &key
		    (x-offset 0.0) (y-offset 0.0)
		    (color nil) (character-height nil) (font nil))

  (activate-device p)
  (let ((theight 0.0)  ;; text width
	(twidth  0.0)
	(ybot    (third  (pgplot-viewport p)))
	(xleft   (first  (pgplot-viewport p)))
	(xright  (second (pgplot-viewport p))))
    (multiple-value-bind (xbox ybox)
	(text-bounding-box p text :device-coords t
			   :character-height character-height :font font)
      (setf twidth  (abs (- (aref xbox 2) (aref xbox 0))))
      (setf theight (abs (- (aref ybox 2) (aref ybox 0)))))
    ;;
    (write-text p text
		(+ x-offset xleft (* 0.5 (- xright xleft twidth)))
		;; the 0.05 takes into account a guess at the axis labels
	        (+ y-offset  (- ybot 0.05 (* 1.0 theight)))
		:device-coords t
		:color color :character-height character-height :font font)
    t))



(defgeneric ylabel (p text &key x-offset y-offset color
			     character-height font)
  (:documentation
   "For pgplot device P, draw y axis label TEXT, nudging it
by X-OFFSET, Y-OFFSET in device coordinates.
If used, COLOR is an integer or symbol, CHARACTER-HEIGHT is a real,
and FONT is an integer or symbol"))

(defmethodL ylabel  ((p pgplot) (text string) &key (x-offset 0.0) (y-offset 0.0)
		    (color nil) (character-height nil) (font nil))
  
  (activate-device p)
  (let ((theight 0.0)  ;; text width
	(twidth  0.0)
	(ybot    (third  (pgplot-viewport p)))
	(ytop    (fourth (pgplot-viewport p)))
	(xleft   (first  (pgplot-viewport p))))
    (multiple-value-bind (xbox ybox)
	(text-bounding-box p text :device-coords t :angle 90.0
			   :character-height character-height :font font)  
      (setf twidth  (abs (- (aref xbox 2) (aref xbox 0))))
      (setf theight (abs (- (aref ybox 2) (aref ybox 0)))))
      ;;
      (write-text p text
		  (max (+ x-offset (- xleft 0.05 (* 1.0 twidth)))
		       0)
		  (+ y-offset ybot (* 0.5 (- ytop ybot theight)))
		  :angle 90.0 :device-coords t
		  :color color :character-height character-height :font font)
      t))



(defgeneric toplabel (p text &key x-offset y-offset color
			       character-height font)
  (:documentation
   "For pgplot device P, draw top label TEXT, nudging it
by X-OFFSET, Y-OFFSET in device coordinates.
If used, COLOR is an integer or symbol, CHARACTER-HEIGHT is a real,
and FONT is an integer or symbol"))

(defmethodL toplabel ((p pgplot) (text string) &key (x-offset 0.0)
		     (y-offset 0.0) (color nil) (character-height nil)
		     (font nil))
   (activate-device p)
  (let ((theight 0.0)  ;; text width
	(twidth  0.0)
	(ytop    (fourth (pgplot-viewport p)))
	(xleft   (first  (pgplot-viewport p)))
	(xright  (second (pgplot-viewport p))))
    (multiple-value-bind (xbox ybox)
	(text-bounding-box p text :device-coords t
			   :character-height character-height :font font)
      (setf twidth  (abs (- (aref xbox 2) (aref xbox 0))))
      (setf theight (abs (- (aref ybox 2) (aref ybox 0)))))
    ;;
    (write-text p text
		(+ x-offset xleft (* 0.5 (- xright xleft twidth)))
		(+ y-offset  (+ ytop 0.03 ))
		:device-coords t
		:color color :character-height character-height :font font)
    t))
    

(defgeneric pgplot-cursor-position (p &key x y use-last-pos)
  (:documentation
   "For pgplot device P, get the cursor position, returning
 (values x y char-number), initially placing cursor at keyword arguments
x,y or using the last cursor position if use-last-pos is T, the default"))


(defmethodL pgplot-cursor-position ((p pgplot) &key (x nil) (y nil)
				(use-last-pos t))
  (when use-last-pos
    (setf x (or x (pgplot-last-curs-x p)))
    (setf y (or y (pgplot-last-curs-y p))))
  (when (not x) (setf x (first (pgplot-pen-position p))))
  (when (not y) (setf y (second (pgplot-pen-position p))))
  (activate-device p)
  (multiple-value-bind 
   (xx yy char) (pgcurs x y)
   (setf (pgplot-last-curs-x p) xx)
   (setf (pgplot-last-curs-y p) yy)
   (values xx yy char)))


(defgeneric draw-arrow (p x1 y1 x2 y2 &key size color)
  (:documentation "For pgplot device P, draw an arrow from X1,Y1 to
X2,Y2, using current arrow qualities.  SIZE is a real."))

(defmethodL draw-arrow ((p pgplot)
		       (x1 real) (y1 real) (x2 real) (y2 real)
		       &key (size nil) (color nil))

  (activate-device p)
  (with-pgplot-settings (p :character-height size :color color)
    (pgarro (float x1 1.0)
	    (float y1 1.0)
	    (float x2 1.0)
	    (float y2 1.0)))
  (fix-pen-position p)
  t)


(defgeneric circle (p x0 y0 radius &key line-width color line-style
				     fill-area-style)
  (:documentation
   "For pgplot device P, draw a circle of radius RADIUS at X0,Y0
LINE-WIDTH is an integer, COLOR an integer or symbol, LINE-STYLE an
integer or symbol, and FILL-AREA-STYLE an integer or symbol."))

(defmethodL circle ((p pgplot) (x0 real) (y0 real) (radius real)
		   &key (line-width nil) (color nil) (line-style nil)
		   (fill-area-style nil))
  (activate-device p)
    (with-pgplot-settings (p :line-width line-width
			     :color color
			     :line-style line-style
			     :fill-area-style fill-area-style)
      (pgcirc x0 y0 radius))
    (fix-pen-position p)
    t)


(defgeneric ellipse (p x-center y-center major-axis minor-axis
		       theta-deg
		       &key line-width color line-style 
			 fill-area-style n-points)
  (:documentation
   "For pgplot device P, draw an ellipse of semi-major-axes
MAJOR-AXIS and MINOR-AXIS, with the major axis at an angle
THETA-DEG counterclockwise from +x axis.
LINE-WIDTH is an integer, COLOR an integer or symbol, LINE-STYLE an
integer or symbol, and FILL-AREA-STYLE an integer or symbol.
N-POINTS is the number of segments to use."))

;; theta-deg is counterclockwise angle of major axis to positive x axis
(defmethodL ellipse (p x-center y-center
		    major-axis minor-axis 
		    theta-deg
		    &key line-width color line-style 
		    fill-area-style (n-points 180))
    (when (> (abs theta-deg) 65536)
    (error "Out of range theta-deg - should have |theta-deg|<65536"))
  (let* ((theta-deg (the (float -65536.0 65536.0)
		      (coerce theta-deg 'single-float)))
	 (xvec (make-array n-points :element-type 'single-float :initial-element 0.0))
	 (yvec (make-array n-points :element-type 'single-float :initial-element 0.0))
	 (cos-rot (cos  (* 0.017453292 theta-deg)))
	 (sin-rot (sin  (* 0.017453292 theta-deg)))
	 (xx 0.0)
	 (yy 0.0)
	 (x  0.0)
	 (y  0.0)
	 (phi 0.0))
    (declare (type (single-float -10.0 10.0) phi)) ;; make trig functions happy
    (dotimes (i n-points)
      (setf phi (* (/ (float i) (float n-points))  6.28318530717s0))
      (setf xx (* (cos phi) major-axis))
      (setf yy (* (sin phi) minor-axis))
      (setf x (+ (* xx cos-rot) (* -1.0 yy sin-rot)))
      (setf y (+ (* xx sin-rot) (* yy cos-rot)))
      (setf (aref xvec i) (to-single-float (+ x x-center) ))
      (setf (aref yvec i) (to-single-float (+ y y-center) )))
    (polygon p xvec yvec
	     :line-width line-width :color color :line-style line-style
	     :fill-area-style fill-area-style)))


(defgeneric rectangle (p x1 x2 y1 y2 &key line-width color line-style
				       draw-outline outline-color fill-area-style)
  (:documentation
   "For pgplot device P, draw an rectangle with corners X1,Y1 and X2,Y2
LINE-WIDTH is an integer, COLOR an integer or symbol, LINE-STYLE an
integer or symbol, and FILL-AREA-STYLE an integer or symbol.

DRAW-OUTLINE, if T (default), draws an outline as well as the areay
style specified.
"))

(defmethodL rectangle ((p pgplot) (x1 real) (x2 real) (y1 real)
		      (y2 real)
		      &key (line-width nil) (color nil) (line-style nil)
		       (draw-outline t)
		       (outline-color nil)
		       (fill-area-style nil))
  (activate-device p)
  (with-pgplot-settings (p :line-width line-width
			   :color color
			   :line-style line-style
			   :fill-area-style fill-area-style)
    (pgrect x1 x2 y1 y2))

    ;; force an outline as well as hatching
    (when (and draw-outline (not (eq fill-area-style :outline)))
       (with-pgplot-settings (p :line-width line-width
			     :color (or outline-color color)
			     :line-style line-style
			     :fill-area-style :outline)
	 (pgrect x1 x2 y1 y2)))

    (fix-pen-position p)
    t)


(defgeneric polygon (p xvec yvec &key line-width color line-style
				   draw-outline outline-color fill-area-style )
  (:documentation
   "For pgplot device P, draw a polygon with vertices
given by pairs in vectors XVEC YVEC.
LINE-WIDTH is an integer, COLOR an integer or symbol, LINE-STYLE an
integer or symbol, and FILL-AREA-STYLE an integer or symbol."))

(defmethodL polygon ((p pgplot) (xvec vector) (yvec vector) 
		     &key (line-width nil) (color nil) (line-style nil)
		     (draw-outline nil) (outline-color nil)
		     (fill-area-style nil))
  (activate-device p)
  (with-pgplot-settings (p :line-width line-width
			   :color color
			   :line-style line-style
			   :fill-area-style fill-area-style)
    (pgpoly (min (length xvec) (length yvec)) xvec yvec))

  (when (and draw-outline (not (eq fill-area-style :outline)))
    (with-pgplot-settings (p :line-width line-width
			     :color (or color outline-color)
			     :line-style line-style
			     :fill-area-style :outline)
      (pgpoly (min (length xvec) (length yvec)) xvec yvec)))
  
  (fix-pen-position p)
  t)


;; sets background color, destructively modifying colormap
;; COLORMAP IS NO LONGER VALID (should we insert new name too?)

(defgeneric set-colormap-color (p ic r g b &key name)
  (:documentation
   "For pgplot device P, replace the current colormap's color
denoted by integer IC with one given by the reals R,G,B
NAME is the optional new name for the color"))


(defmethodL set-colormap-color ((p pgplot) (ic integer) 
			       (r real) (g real) (b real)
			       &key (name nil))

  (ensure-type 'single-float r g b)
  (if (or (< r 0) (> r 1) (< g 0) (> g 1) (< b 0) (> b 1))
      (error "Invalid colors in pgplot:set-background-color - must be in [0,1]"))
  (let* ((cm (pgplot-colormap p))
	 (alist-new (remove-if #'(lambda (pair) (= (cdr pair) ic))
			       (colormap-color-alist cm))))
    (if (or (>= ic (colormap-npoints cm)) (< ic 0))
	(error "Invalid color index in set-colormap-color"))
    (if (member name alist-new)
	(print "WARNING: two colors will have same name in colormap"))
    ;; set up new color alist
    (if name 
	(setf (colormap-color-alist cm) (cons (cons name ic) alist-new))
      (setf (colormap-color-alist cm)  alist-new))
    ;;
    (setf (aref (colormap-red-vec   cm) ic) r)
    (setf (aref (colormap-red-vec   cm) ic) g)
    (setf (aref (colormap-red-vec   cm) ic) b))
  (pgscr ic r g b))
  


;; set current viewport to be a pane of larger plot
;; ix=1..nx is x number, iy=1...ny is y number
;; scales are in normalized device coords 0...1
(defgeneric set-current-pane (p ix nx iy ny &key
				x-separation y-separation top-pad
				bottom-pad right-pad left-pad
					      viewport)
  (:documentation
   "For pgplot device P, divide the entire plottng area so that the
current viewport correspond to the IX'th of NX and IY'th of NY
sub-panes.  X-SEPARATION, Y-SEPARATION are the distances to assume
between panes; TOP-PAD, BOTTOM-PAD, LEFT-PAD, RIGHT-PAD are blank
spaces to leave at sides of entire plot, to allow for labels.
VIEWPORT is a list representing the viewport within which the panes
are defined.  By default VIEWPORT is the entire device. "))


(defmethodL set-current-pane ((p pgplot)
			      (ix integer) (nx integer)
			      (iy integer) (ny integer)
			      &key (x-separation 0.05)
			      (y-separation 0.05)
			      (top-pad 0.02)    ;; extra blank space on top
			      (bottom-pad 0.10) ;; on bottom
			      (right-pad 0.03)  ;; on right
			      (left-pad 0.15)  ;; on left
			      (viewport '(0.0 1.0 0.0 1.0)))
 
  (let* ((x-avail (- 1.0 left-pad right-pad))
	 (y-avail (- 1.0 top-pad bottom-pad))
	 (xsize (/ (- (float x-avail) (* (- nx 1.0) x-separation))
		  (float nx)))
	 (ysize (/ (- (float y-avail) (* (- ny 1.0) y-separation))
		  (float ny)))
	 (x0 (+ left-pad (* (- ix 1) xsize) (* (- ix 1) x-separation)))
	 (y0 (+ bottom-pad (* (- iy 1) ysize) (* (- iy 1) y-separation)))
	 (x1 (+ x0 xsize))
	 (y1 (+ y0 ysize)))
    ;; now remap to viewport
    (let ((x0vp (elt viewport 0))
	  (x1vp (elt viewport 1))
	  (y0vp (elt viewport 2))
	  (y1vp (elt viewport 3)))
      (let ((xx0 (+ x0vp (* x0 (- x1vp x0vp))))
	    (xx1 (+ x0vp (* x1 (- x1vp x0vp))))
	    (yy0 (+ y0vp (* y0 (- y1vp y0vp))))
	    (yy1 (+ y0vp (* y1 (- y1vp y0vp)))))
	(set-viewport p xx0 xx1 yy0 yy1)))))

	
			     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; histogram routines
;; to do: options for input binvec, equal-number-per-bin, hatching
;; arguments
;;   p       - the pgplot object
;;   xpts    - a vector of points to histogram
;;   line-width,color,line-style - what line to use
;;   binvec  - vector of bin edges - overrides nbins, and incompatible with
;;             non-nil xleft,xright
;;   nbins   - number of bins
;;   xleft,xright - x limits for histogram (if none supplied, use xpts)
;;   log      - logarithmic (base 10) bins
;;   normalize - normalize histogram so that part that falls within binvec
;;               has unit integral; done before taking log, if :log set.
;;               Points falling outside binvec are ignored
;;   flip-x   - flip x axis so that positive x bins are on left; not
;;              compatible with non-nil binvec
;;   y-frac   - fraction of y axis filled by histogram
;;   y-scale  - number by which to multiply y
;;   y-max    - maximum y -- overrides y-frac
;;   box      - draw a box
;;   set-window - set window to xleft,xright (which may be taken from binvec)
;;                     if nil, histogram is drawn using current window
;;                     if t, histogram is drawn using new window
;;                     if :restore histogram is drawn using new window,
;;                        but old window is restored
;;   fill, fill-color, fill-line-width, fill-line-style -- fill histogram?
;;                     fill is nil (no fill, default), or a fill style
;;                     like :hatched
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric hist (p xpts   
		  &key
		  binvec
		  nbins xleft xright flip-x log
		  normalize
		  normalize-includes-outside
		  y-frac y-scale y-max
		  set-window
		  line-width color line-style 
		  fill fill-color fill-line-width 
		  fill-line-style)
(:documentation
 "For pgplot device P, histogram a list or array of numbers XPTS.
If BINVEC is specified, use it as the bin boundaries, else make
 NBINS evenly spaced bins.
XLEFT, XRIGHT are the optional X axis limits for the plot, set by
 default to be the limits of XPTS
FLIP-X is a flag about whether to mirror-reflect the x axis.
LOG is a flag whether to plot the log10 of the number of points in
 each bin.
NORMALIZE is a flag denoting whether the bin counts should be
 normalized so the integral of the distribution is 1 or
 (if NORMALIZE is a number) NORMALIZE
NORMALIZE-INCLUDES-OUTSIDE - if T, then adjust the normalization to include
 the points lying outside XLEFT, XRIGHT - eg, if 1/2 of the points are outside
 then the histogram will be only 1/2 as high
Y-FRAC is the fraction of the total height the highest bin should reach.
Y-SCALE is a rescaling for the bin contents.
SET-WINDOW is a flag (T by default) that controls whether the window
 is reset before plotting. SET-WINDOW=NIL is useful for multiple plots
LINE-WIDTH is an optional integer, COLOR an integer or symbol,
  LINE-STYLE an integer or symbol
FILL is a boolean flag whether to fill the histogram, and
  FILL-COLOR, FILL-LINE-WIDTH FILL-LINE-STYLE are like
  COLOR, LINE-WIDTH, LINE-STYLE"))

(defmethodL hist ((p pgplot) (xpts T)  
		 &key
		 (binvec nil)
		 (nbins 20) (xleft nil) (xright nil) (flip-x nil) (log nil)
		 (normalize nil) 
		 (normalize-includes-outside nil)
		 (y-frac 0.8) (y-scale 1.0) (y-max nil)
		 (set-window t)
		 (line-width nil) (color nil) (line-style nil)
		 (fill nil) (fill-color nil) (fill-line-width nil) 
		 (fill-line-style nil))
  (when (not (or (listp xpts) (arrayp xpts)))
    (error "XPTS must be a list or an array"))
  (when (listp xpts) (setf xpts (coerce xpts 'vector)))
  ;;
  (if (and (or xleft xright) (not (and xleft xright)))
      (error "hist: you must specify both xleft and xright, or neither"))
  (if (and flip-x xleft xright (< xleft xright))
      (error "hist: your have flip-x turned on, but this is contradicted by xleft < xright"))
  (if (and binvec (or xleft xright))
      (error "hist: you have *both* binvec and xleft or xright specified"))
  (if (and binvec flip-x)
      (error "hist: you have *both* binvec and flip-x specified"))
  ;;
  ;; if a binvector was not supplied, then make it
  ;; else check the one supplied for monotonic increase
  (if (not binvec)
      (progn
	(if (not xleft) ;; set xleft and xright if not given
	    (progn (setf xleft (row-major-aref xpts 0))
		   (setf xright (row-major-aref xpts 0))
		   (dotimes (i (array-total-size xpts))
		     (setf xleft (min xleft (row-major-aref xpts i)))
		     (setf xright (max xright (row-major-aref xpts i))))))
	;; flip xleft,xright if asked
	(if flip-x (let ((tmp xleft)) (setf xleft xright) (setf xright tmp)))
	(setf binvec (hist-make-binvec nbins xleft xright))) ;; and make the binvec
    (progn
      (setf nbins (+ -1 (length binvec))) ;; ignore any supplied nbins
      (multiple-value-setq (xleft xright binvec)
	(hist-check-binvec binvec))))


  ;;
  (let ((nbinvec  (make-array nbins :element-type 'single-float
			      :initial-element 0.0))
	(x-vec (make-array (+ 2 (* 2 nbins))
			   :element-type 'single-float :initial-element 0.0))
	(y-vec (make-array (+ 2 (* 2 nbins))
			   :element-type 'single-float :initial-element 0.0))
	(y-min 0.0s0))
    ;; fill nbinvec
    (hist-fill-nbinvec xpts binvec nbinvec
		       (coerce xleft 'double-float)
		       (coerce xright 'double-float))
    ;; if normalization sought, normalize histogram so that integral between
    ;; xleft and xright is 1.0
    (if normalize
	;; take possibility of normalize-includes-outside into account
	(let ((norm-fac (if (realp normalize) (float normalize 1.0) 1.0)))
	  (when normalize-includes-outside
	    (loop with xmin = (min xleft xright)
		  with xmax = (max xleft xright)
		  with fac = 1.0
		  with noutside = 0
		  for x across xpts
		  when (or (< x xmin) (> x xmax))
		  do (incf noutside)
		  finally
		  (setf fac (- 1.0 (/ noutside (length xpts))))
		  (setf norm-fac (* norm-fac fac))))
	  (hist-normalize-histogram binvec nbinvec norm-fac)))
    ;; fill the xy vecs
    (hist-make-xy binvec nbinvec x-vec y-vec y-scale)
    ;;
    ;;
    (when log ;; is histogram to be logarithmic?
      (dotimes (i (length y-vec))
	(setf (aref y-vec i)
	      (let ((y (aref y-vec i)))
		(if (= y 0.0s0)
		    -10000.0
		    (let ((tmp (log y 10.s0)))
		      ;; set y-min to 1/10 of max bin
		      (setf y-min (min y-min (- tmp 1.0s0)))
		      tmp)))))
      ;; now if we are not normalizing, set y-min to a sensible general value
      (when (not normalize)
	(setf y-min -0.3)))
    ;;
    (if set-window
	(progn
	  (if (not y-max)
	      (progn
		(setf y-max (aref y-vec 0))
		(dotimes (i (length y-vec))
		  (setf y-max (max y-max (aref y-vec i))))
		(setf y-max (/ y-max y-frac))))
	  (set-window p xleft xright y-min y-max)))
    ;;
    ;; now patch the y vector by making it a little bit negative where 
    ;; y=0 the lines at y=0 don't show up
    (when (not log)
      (loop
       with ybiggest = (loop with yb = (aref y-vec 0)
			     for y across y-vec do (setf yb (max yb y))
			     finally (return yb))
       with yz = (* -0.1e0 ybiggest)
       for i below (length y-vec)
       for y across y-vec
       when (zerop y) do (setf (aref y-vec i) yz)))
    ;;
    ;; and connect the points	
    (connect p x-vec y-vec :color color :line-width line-width
	     :line-style line-style)
    (if fill
	(polygon p x-vec y-vec :line-width fill-line-width :color fill-color
		 :fill-area-style fill :line-style fill-line-style)))
  ;;
  (fix-pen-position p)
  t)

;; make this double-float so that all comparisons work
(defun hist-make-binvec (nbins xleft xright)
  (let ((binvec (make-array (+ nbins 1) :element-type 'double-float)))
    (dotimes (i  (+ 1 nbins))
      (setf (aref binvec i)
	    (coerce (+ xleft (* i (/ (- xright xleft) nbins))) 'double-float)))
    ;; now we nudge the outside bins outward by a little so that xleft
    ;; and xright always fall inside the bin vector, even with
    ;; numerical roundoff
    (let ((nudge (* 1.0e-12 (- xright xleft))))
      (incf (aref binvec 0) (- nudge))
      (incf (aref binvec nbins) nudge))
    binvec))


;; check binvec for monotonic increase, and coerce it to double float
(defun hist-check-binvec (binvec)
  (if (not (vectorp binvec)) (error "binvec supplied to hist not a vector"))
  (if (> 1 (length binvec)) (error "binvec supplied to hist too short"))
  ;;
  ;; if binvec not double float, copy it to a double float vec
  (if (not (equal (type-of binvec) '(simple-array double-float)))
      (let ((bv (make-array (length binvec) :element-type 'double-float)))
	(dotimes (i (length binvec))
	  (setf (aref bv i) (coerce (aref binvec i) 'double-float)))
	(setf binvec bv)))
  ;;
  (let ((sgn (if (< (aref binvec 0) (aref binvec 1)) 1.0 -1.0)))
    (dotimes (i (+ -1 (length binvec)))
      (if (> (* sgn (aref binvec i)) (* sgn (aref binvec (+ i 1))))
	  (error
 "hist: binvec supplied to hist not monotonically increasing or decreasing")))
    (values (aref binvec 0) (aref binvec (+ -1 (length binvec))) binvec)))
  

;; normalize histogram so that parts within binvec integrate to 1, or to
;; normalize if normalize is a real
(defun hist-normalize-histogram (binvec nbinvec normalize)
    (declare (type (simple-array double-float) binvec)
	     (type (simple-array single-float) nbinvec)
	     (type (or null real) normalize)
	     (optimize (speed 3) (safety 1)))
    (let ((integral (abs
		     (the double-float
		       (loop for i fixnum from 0 to (- (length nbinvec) 1)
			     with sum of-type double-float = 0.0d0
			     do
			     (incf sum
				   (* (aref nbinvec i)
				      (- (aref binvec (+ i 1))
					 (aref binvec i))))
			     finally (return sum)))))
	  (fac (if (realp normalize) (float normalize 1.0) 1.0)))
      (if (= integral 0.0d0)
	  (error "hist: cannot normalize histogram with all empty bins"))
      (loop for i fixnum from 0 to (- (length nbinvec) 1)
	    do (setf (aref nbinvec i)
		     (coerce (* fac (/ (aref nbinvec i) integral))
			     'single-float)))))
	  
	  
    
    

(defun hist-hunt-bin (binvec x)
  (let ((ibot 0)
	(itop  (length binvec))
	(icent 0)
	(dir (> (aref binvec 1) (aref binvec 0))))
    (declare (fixnum ibot itop icent))
		       (loop
			(if (<= (- itop ibot) 1) (return ibot)) 
			(setf icent (floor (+ ibot itop) 2))
			(if (eq (> x (aref binvec icent))  dir)
			    (setf ibot icent)
			  (setf itop icent)))))
    


(defun to-dbl-float-vec (v)
  (declare (type (or list array) v))
  (cond ((typep v '(simple-array double-float (*)))
	 v)
	((typep v 'list)
	 (map '(simple-array double-float (*)) (lambda (x) (float x 1d0)) v))
	(t
	 (loop with vv = (make-array (array-total-size v) 
				     :element-type 'double-float)
	       for i fixnum from 0 below (array-total-size v)
	       do (setf (aref vv i) (coerce (row-major-aref v i) 'double-float))
	       finally (return vv)))))

;; use bisection to fill nbinvec -- types are assured by calling function
(defun hist-fill-nbinvec (xpts binvec nbinvec xleft xright)
  (declare (type (simple-array double-float) binvec)
	   (type (simple-array single-float) nbinvec)
	   (type simple-array xpts)
	   (type double-float xleft xright))
  (let ((dir (> (aref binvec 1) (aref binvec 0)))
	(xpts (to-dbl-float-vec xpts)))
    (declare (type (simple-array double-float (*)) xpts)
	     (optimize (speed 3) (safety 1)))
    (flet ((find-bin (x)
		     (declare (type double-float x))
		     (let ((ibot 0)
			   (itop   (+ -1 (length binvec)))
			   (icent 0))
		       (declare (type (unsigned-byte 28) ibot itop icent))
		       (loop
			(if (<= (- itop ibot) 1) (return ibot)) 
			(setf icent (floor (+ ibot itop) 2))
			(if (eq (> x (aref binvec icent))  dir)
			    (setf ibot icent)
			  (setf itop icent))))))
      ;;
      (declare (inline find-bin))
      ;;
      (dotimes (i (length xpts))
	(declare (type (unsigned-byte 28) i))
	(if (and (eq (>= (aref xpts i) xleft) dir)
		 (eq (<= (aref xpts i) xright) dir))
            (let ((j (find-bin (coerce (aref xpts i) 'double-float))))
	      (if (or (< j 0) (>= j (length nbinvec)))
		  (error
		   "Invalid bin in hist-fill-nbinvec; should not happen"))
              (setf (aref nbinvec j) (+ 1.0 (aref nbinvec j)))))))))

	     
(defun hist-make-xy (binvec nbinvec x-vec y-vec y-scale)
  (setf (aref x-vec 0) (to-single-float (aref binvec 0)))
  (setf (aref y-vec 0) 0.0s0)
  ;;
  (dotimes (i (length nbinvec))
    (setf (aref x-vec  (+ (* i 2) 1))
	  (to-single-float (aref binvec  (+ i 0))))
    (setf (aref y-vec  (+ (* i 2) 1))
	  (to-single-float (* y-scale (aref nbinvec (+ i 0))) ))
    (setf (aref x-vec  (+ (* i 2) 2))
	  (to-single-float (aref binvec  (+ i 1)) ))
    (setf (aref y-vec  (+ (* i 2) 2))
	  (to-single-float (* y-scale (aref nbinvec (+ i 0))) )))
  ;;
  (setf (aref x-vec (+ -1 (length x-vec)))
	(to-single-float (aref binvec (+ -1 (length binvec)))))
  (setf (aref y-vec (+ -1 (length y-vec)))  0.0s0))

(defun %set-color-indices (p &key  (type :default)
			   ;; max and min color indices if this is not the default
			   ;; colormap
			   (min-color-index nil) (max-color-index nil))
  "Helper to set the color index using the agove keywords"
  ;; either we are using the default colormap, or we must specify
  ;; the index range explicitly
  (let ((cm (pgplot-colormap p))
	(cmrange nil))
    (cond
      ;; indices specified
      (min-color-index
       (when (not (and min-color-index max-color-index
		      (< min-color-index max-color-index)
		      (>= min-color-index 0)
		      (< max-color-index (colormap-npoints cm))))
	 (error "image: Bad min/max color-indices ~a ~a" 
		min-color-index max-color-index)))
      ;; else using a named range inside the colormap
      (t
       (setf cmrange (cdr (assoc type (colormap-named-ranges cm) :test 'eq)))
       (when (not cmrange)
	 (error "image: Could not find named colormap range ~A - available are ~A"
		type (colormap-named-ranges cm)))
       (setf min-color-index  (first cmrange)
	     max-color-index  (second cmrange)))))
     ;; set color index range 
  (pgscir min-color-index max-color-index) )
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image routines
(defgeneric image (p a &key type transformation-matrix
			 i-min-index i-max-index
			 j-min-index j-max-index
			 val1 val2
			 min-color-index max-color-index
			 reverse transfer-function
			 color-wedge-side color-wedge-width
			 color-wedge-disp color-wedge-label
			 character-height)
  (:documentation
   "For a pgplot P, plot 2d real array A as an image.
TYPE is the name of the colormap to use (eg :DEFAULT :GRAY :COLOR :HEAT)
TRANSFORMATION-MATRIX is a matrix to transform array coordinates to
  user coordinates.
I-MIN-INDEX, etc are the inclusive indices to use in array A[I,J]
VAL1 and VAL2 are optional values in A to the extremes of the color range.
MIN-COLOR-INDEX and MAX-COLOR-INDEX are the color index range to use,
 overriding TYPE.
TRANSFER-FUNCTION is the transfer function of the array to the color index,
 one of :LINEAR :LOG :SQRT 
REVERSE reverses the color map by swapping VAL1 and VAL2

Returns (values VAL1 VAL2) to allow subsequent use of COLOR-WEDGE
However, it is better to use COLOR-WEDGE-SIDE etc to make a color
wedge immediately, because colormap and transfer function may be
reset if wedge is made in two calls.

For the data array A[I,J], the default coordinate system has
X increasing with J, and Y increasing with I, rescaled to fit
exactly within the user coordinates.   If used, the
transformation matrix (TR) changes this to:
  X = TR[0,0] + TR[0,1]*J + TR[0,2]*I
  Y = TR[1,0] + TR[1,1]*J + TR[1,2]*I

It is probably necessary to reset the viewport to leave a margin of
about 0.1 on the COLOR-WEDGE-SIDE to fit the wedge onto the plot. 
  eg: (pgplot:set-viewport p xx 0.90 xx xx)

QUIRK - the color wedge, if drawn, is linear, which is awkward for a
nonlinear transfer function, because the entire wedge will be colored
at the high end of the scale, and most of the colors will be
compressed into the low end.  To make this work, one would need a
custom wedge plotting function with an appropriate non-linear scale."))
		     

(defmethodl image ((p pgplot) (a array) &key
		  ;; type refers to name in (colormap-named-ranges colormap)
		  (type nil)   ;; use (colormap-default-range colormap) if NIL
		  (transformation-matrix nil)
		  (i-min-index nil) (i-max-index nil)
		  (j-min-index nil) (j-max-index nil)
		  ;; values in a that map to extremes of colors
		  (val1 nil) (val2 nil)
		  ;; max and min color indices if this is not the default
		  ;; colormap
		  (min-color-index nil) (max-color-index nil)
		  ;; if REVERSE is T, then reverse-video by swapping VAL1,VAL2
		  (reverse nil)
		  ;; how do values scale to colormap?
		  (transfer-function :linear)
		  ;;
		  (color-wedge-side NIL)
		  (color-wedge-width 1.5)
		  (color-wedge-disp  0.2) ;; displacement in char units
		  (color-wedge-label "")
		  (character-height nil)) ;; for color wedge
 
  ;;
  (declare (type (member :linear :log :sqrt) transfer-function)
	   (type (array * (* *)) a))

  ;;
  ;; fix  i-min-index, etc - these are inclusive lisp (0..n-1) indices
  (let ((nx (+ -1 (array-dimension a 0)))
	(ny (+ -1 (array-dimension a 1))))
    (if (not i-min-index) (setf i-min-index 0))
    (if (not j-min-index) (setf j-min-index 0))
    (if (not i-max-index) (setf i-max-index nx)) 
    (if (not j-max-index) (setf j-max-index ny))
    (if (or (< i-min-index 0) (> i-min-index nx)
	    (< j-min-index 0) (> j-min-index ny))
	(error "image: bad min/max indices")))
  ;;
  ;; fix the min/max values val1 and val2
  (when (or (not val1) (not val2))
    (loop for i fixnum from 1 below (array-total-size a)
	  for val = (row-major-aref a i)
	  for min-val = (row-major-aref a 0) then (min min-val val)
	  for max-val = (row-major-aref a 0) then (max max-val val)
	  finally
	  (when (not val1) (setf val1 min-val))
	  (when (not val2) (setf val2 max-val))))
  ;;
  (setf val1 (coerce val1 'single-float)
	val2 (coerce val2 'single-float))
  ;;
  (when reverse (rotatef val1 val2))
  ;;
  (if (and transformation-matrix
	   (not (2x3-numerical-matrix-p a)))
      (error "image: transformation matrix ~A not a 2x3 matrix" 
	     transformation-matrix))
  (let ((tv (make-array 6 :element-type 'single-float :initial-element 0.0))
	;; set type of colormap to be the default
	(type (or type (colormap-default-range (pgplot-colormap p))))
	;; index for transfer function
	(n-pgsitf  (cond ((eq transfer-function :linear) 0) ;; set transfer function
			 ((eq transfer-function :log) 1)
			 ((eq transfer-function :sqrt) 2)))
	;; ;; in case we have to computer own transfer function for gia
	(a* a)
	(val1* val1)
	(val2* val2))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; build transformation vector 
    (if (not transformation-matrix)
	;; if the transformation matrix is undefined, make the
	;; array fit into the current coordinate box
	(let* ((xmin (first   (pgplot-window p)))
	       (xmax (second  (pgplot-window p)))
	       (ymin (third   (pgplot-window p)))
	       (ymax (fourth  (pgplot-window p)))
	       (nx (array-dimension a 1)) ;; note flip
	       (ny (array-dimension a 0)) ;;  of x,y
	       (dx (/  (- xmax xmin) (- nx 1.0)))
	       (dy (/  (- ymax ymin) (- ny 1.0))))
	  (setf (aref tv 0) (to-single-float (- xmin dx)))
	  (setf (aref tv 3) (to-single-float (- ymin dy)))
	  (setf (aref tv 1) (to-single-float dx))
	  (setf (aref tv 2) 0.0)
	  (setf (aref tv 4) 0.0)
	  (setf (aref tv 5) (to-single-float dy)))
	;;
	;; if tranformation matrix is defined, use it for tv
	(%convert-transformation-matrix-to-tv transformation-matrix tv))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make the image plot
    (activate-device p)
    ;; set transfer function
    (cond ((not *using-pgplot-giza*)
	   (pgsitf n-pgsitf))
	  ;; using giza library so we have to compute transfer function ourselves
	  ((not (eql transfer-function :linear)) ;; using giza, and non-linear, so compute transfer
	   (setf a* (make-array (array-dimensions a) :element-type 'single-float))
	   (cond ((eq transfer-function :log)
		  (setf val1* (log val1))
		  (setf val2* (log val2)))
		 ((eq transfer-function :sqrt)
		  (setf val1* (log val1))
		  (setf val2* (log val2))))
	   (loop for i below (array-total-size a)
		 for val = (float (row-major-aref a i) 1.0)
		 for newval = (cond ((eq transfer-function :log)
				     (if (not (plusp val)) -10000.0 (log val)))
				    ((eq transfer-function :sqrt)
				     (if (minusp val) most-negative-single-float (sqrt val))))
		 do (setf (row-major-aref a* i) newval))))
			  
				   
		 
    (%set-color-indices p :type type :min-color-index min-color-index
		       :max-color-index max-color-index)
    (pgimag i-min-index i-max-index j-min-index j-max-index
	    a* val1* val2* tv)

    (when color-wedge-side
      (let ((side-string
	     (case color-wedge-side 
	       (:top "TI") (:left "LI") (:right "RI") (:bottom "BI"))))
	(with-pgplot-settings (p :character-height character-height)
	  (pgsitf n-pgsitf)  ;; not needed
	  (pgwedg side-string color-wedge-disp color-wedge-width val1 val2 
		color-wedge-label))))
		  

    (values val1 val2) ;; return the limiting values, for a subsequent call to 
                       ;; color-wedge - but note that colormap must be the
                       ;; same
    )) 
 

(defgeneric draw-labeled-axis (p x1 y1 x2 y2 ticks labels
			       &key orient xdisplacement tikl tikr
				 line-width character-height
				 line-style color)
  (:documentation
 "Draw a labeled axis from x1 to x2, with:

 TICKS a vector of values in [0,1] representing points on line to draw 
   tick marks.
 LABELS a vector of strings to to write (or NIL)
 ORIENT the angle in degrees of the text relative to line
 DISPLACEMENT the offset of the labels, in character units
 TIKL,TIKR the left and right tick lengths in character units
 LINE-WIDTH, LINE-STYLE, CHRACTER-HEIGHT, COLOR as usual."))

(defmethodL draw-labeled-axis
    ((p pgplot) (x1 real) (y1 real) (x2 real) (y2 real)
     (ticks vector) (labels vector) &key (orient 0.0) 
     (xdisplacement 1.0)
     (tikl -0.5) (tikr 0.0) (line-width 1) 
     (character-height 1.0)
     (line-style :solid) (color :default))
  (error-on-giza "DRAW-LABELED-AXIS" "PGTICK")
  (when (not (= (length ticks) (length labels)))
    (error "TICKS and LABELS do not have the same length."))
  (pgplot:connect p (vector x1 x2) (vector y1 y2) 
		  :line-style line-style :color color :line-width line-width)
  (with-pgplot-settings (p :character-height character-height :color color :line-width line-width)
    (loop for v across ticks
	  for string across labels
	  do (pgtick x1 x2 y1 y2 v tikl tikr xdisplacement orient string))))
  

       
			      
   

;; this is an IN-PROGRESS attempt to make color wedges that behave
;; correctly for log images
(defun %draw-color-box (p val1 val2 xw1 xw2 yw1 yw2 &key 
			(label-pos :right) 
			(type nil)
			(direction :vertical)
			(transfer-function :linear)
			(draw-box t))
  (with-viewport (p xw1 xw2 yw1 yw2)
    (let*  ((nbig 500)
	    (type (or type (colormap-default-range (pgplot-colormap p))))
	    (nx (if (eq direction :vertical) 2 nbig ))
	    (ny (if (eq direction :vertical) nbig 2))
	    (a (make-array (list ny nx) :element-type 'single-float :initial-element 0.0))
	    (z1 (case transfer-function
		    (:linear val1) (:sqrt (sqrt val1)) (:log (log val1 10))))
	    (z2 (case transfer-function
		    (:linear val2) (:sqrt (sqrt val2)) (:log (log val2 10)))))
	    
      (loop 
	for i below nbig
	for z = (+ z1 (* (/ (* 1.0 i) (1- nbig)) (- z2 z1)))
	do (loop for j below 2
		 do (if (eq direction :vertical)
			(setf (aref a i j) z)
			(setf (aref a j i) z))))
      (image p a  :type  type)
      (when draw-box (pgplot:plain-box p)))))
	
	    
	    
      
      
(defgeneric color-wedge (p val1 val2
			    &key  side type min-color-index max-color-index
			      transfer-function disp width label)
  (:documentation

   "Draw a color wedge to annotate an image. 
 
 VAL1 and VAL2 are the limiting values for the brightest/dimmest values
 SIDE is one of :left :right :top :bottom
 DISP is the diaplacement of the wedge, in char units
 WIDTH is the width of the wedge in char units
 LABEL is the units label

WARNING: the color map and transfer function must be consistent with
the original IMAGE call - Thus it is best to use this routine from
within the IMAGE routine, using the COLOR-WEDGE-SIDE keyword to
activate a color wedge.

Works only for IMAGE, not GRAY. This would be easy to change. But we never
really use PGGRAY."))
    
  
 
(defmethodL color-wedge ((p pgplot) (val1 real) (val2 real) &key (side :right) 
			 (type :default) ;; colormap name
			 ;; max and min color indices if this is not the default
			 ;; colormap
			 (min-color-index nil) (max-color-index nil)
			 (transfer-function :linear)
			 (disp 1.0) (width 3.0) (label ""))

  (declare (type (member nil :left :right :top :bottom) side)
	   (type (member nil :linear :log :sqrt) transfer-function)
	   (type string label)
	   (type real val1 val2 disp width))
  (activate-device p)
  (pgsitf (cond ((eq transfer-function :linear) 0) ;; set transfer function
		((eq transfer-function :log) 1)
		((eq transfer-function :sqrt) 2)))
  (%set-color-indices p :type type :min-color-index min-color-index
		      :max-color-index max-color-index)
  (let ((side-string
	 (case side (:top "TI") (:left "LI") (:right "RI") (:bottom "BI"))))
    (pgwedg side-string disp width val1 val2 label)))



(defgeneric vector-field (p a b &key 
			    arrow-scale-factor
			    arrow-head-style ;; list (fs angle barb)
			    arrow-head-size
			    arrow-position ;; :head, :base, or :center
			    transformation-matrix
			    i-min-index i-max-index j-min-index j-max-index
			    line-width color line-style
			    blank-val)
  (:documentation
   "Create a vector field from similar arrays A and B.

A is the X, or second index, component of the flow and B is the Y, or
first index, component of the flow.  Most arguments are similar to
CONTOUR otherwise.  In other words, if A filled with 1 and B with
zero, the flow will go from left to right assuming no transformation
matrix.

ARROW-SCALE-FACTOR is what is called C in the PGPLOT manual.
BLANK-VAL is the value that is ignored for plotting, by default
MOST-POSITIVE-SINGLE-FLOAT"))

(defmethodL vector-field ((p pgplot) (a array) (b array) &key
			  (arrow-scale-factor 0.0)
			  (arrow-head-style  '(:filled 45.0 0.0)) ;; a good style for this
			  (arrow-head-size 0.5) ;; a good size
			  (arrow-position :center)
			  (transformation-matrix nil)
			  i-min-index i-max-index j-min-index j-max-index
			  (line-width nil)
			  (color nil)
			  (line-style nil)
			  (blank-val most-positive-single-float))
  
  (assert (= (array-rank a) 2))
  (assert (= (array-rank b) 2))
  
  ;; fix  i-min-index, etc
  (let ((nx (+ -1 (array-dimension a 0)))
	(ny (+ -1 (array-dimension a 1))))
    (declare (type (unsigned-byte 28) nx ny))
    (if (not i-min-index) (setf i-min-index 0))
    (if (not j-min-index) (setf j-min-index 0))
    (if (not i-max-index) (setf i-max-index nx)) 
    (if (not j-max-index) (setf j-max-index ny))
    (if (or (< i-min-index 0) (> i-min-index nx)
	    (< j-min-index 0) (> j-min-index ny))
	(error "contour: bad min/max indices")))

    (if (and transformation-matrix
	   (not (2x3-numerical-matrix-p a)))
      (error "contour: transformation matrix ~A not a 2x3 matrix" 
	     transformation-matrix))
  ;;
  (let ((tv (make-array 6 :element-type 'single-float :initial-element 0.0))
	;; convert array NOW because we may need to convert it many times
	(af (convert-array-to-single-float-array a)) 
	(cvec nil)    ;; vector of contours
	(minc nil)    ;; min contour
	(maxc nil))   ;; max contour
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; build transformation vector 
    (if (not transformation-matrix)
	;; if the transformation matrix is undefined, make the
	;; array fit into the current coordinate box
	(let* ((xmin (first   (pgplot-window p)))
	       (xmax (second  (pgplot-window p)))
	       (ymin (third   (pgplot-window p)))
	       (ymax (fourth  (pgplot-window p)))
	       (nx (array-dimension a 1)) ;; note flip
	       (ny (array-dimension a 0)) ;;  of x,y
	       (dx (/  (- xmax xmin) (- nx 1.0)))
	       (dy (/  (- ymax ymin) (- ny 1.0))))
	  (setf (aref tv 0) (to-single-float (- xmin dx)))
	  (setf (aref tv 3) (to-single-float (- ymin dy)))
	  (setf (aref tv 1) (to-single-float dx))
	  (setf (aref tv 2) 0.0)
	  (setf (aref tv 4) 0.0)
	  (setf (aref tv 5) (to-single-float dy)))
        ;;
        ;; if tranformation matrix is defined, use it for tv
	(%convert-transformation-matrix-to-tv transformation-matrix tv))

    (activate-device p)
    
    (let ((nc (cond ((eq arrow-position :head) -1)
		    ((eq arrow-position :tail) +1)
		    ((eq arrow-position :center) 0)
		    (t (error "Invalid arrow position ~A" arrow-position)))))
      
      (with-pgplot-settings (p :line-width line-width :color color :line-style line-style
			       :character-height arrow-head-size
			       :arrow-head-style arrow-head-style)
	(pgvect i-min-index i-max-index
		j-min-index j-max-index
		a b arrow-scale-factor
		nc tv blank-val)))))
      


(defgeneric pgaxis (p opt x1 y1 x2 y2 v1 v2 step nsub dmajl dmajr
		    fmin disp orient)
  (:documentation
   "Direct call of pgplot PGAXIS routine to draw a labeled axis.  
See PGPLOT documentation."))

(defmethodL pgaxis ((p pgplot) opt x1 y1 x2 y2 v1 v2 step nsub dmajl dmajr
		    fmin disp orient)
  (declare (type string opt)
	   (type real x1 y1 x2 y2 v1 v2 step
	    nsub dmajl dmajr fmin disp orient  
	    dmajl dmajr fmin disp orient))
  (error-on-giza "PGAXIS" "PGAXIS")
  (activate-device p)
  (pgaxis-raw opt
	      (float x1 1.0)    (float y1 1.0)
	      (float x2 1.0)    (float y2 1.0)
	      (float v1 1.0)    (float v2 1.0)
	      (float step 1.0)
	      nsub
	      (float dmajl 1.0) (float dmajr 1.0)
	      (float fmin 1.0)  (float  disp 1.0)
	      (float orient 1.0)))
  
