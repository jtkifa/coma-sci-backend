;;;; markers.lisp — point markers, as PGPLOT's PGPT
;;;;
;;;; (PLOT-POINTS DEVICE XS YS :MARKER :FILLED-CIRCLE) draws a marker at
;;;; each point.  A marker spec is
;;;;   - a keyword naming a marker (see MARKER-NAMES): open and filled
;;;;     families (:CIRCLE / :FILLED-CIRCLE, :SQUARE, :TRIANGLE,
;;;;     :TRIANGLE-DOWN, :DIAMOND, :PENTAGON, :HEXAGON, :STAR), line
;;;;     markers (:PLUS, :CROSS, :ASTERISK) and combinations;
;;;;   - an integer, PGPLOT's symbol numbers: 0-27 as listed in
;;;;     *PGPLOT-MARKERS* (approximately PGPLOT's glyphs), and -3 to -31 a
;;;;     filled regular polygon with that many sides;
;;;;   - a function of (DEVICE CX CY SIZE) adding the marker's outline to
;;;;     the current path at canvas point (CX,CY); it is filled.
;;;;
;;;; Astronomical symbols are markers too: :SUN :MOON :MERCURY ... :PLUTO,
;;;; the zodiac :ARIES ... :PISCES, :COMET and the lunar nodes.  They are
;;;; glyphs of a font, by default Hershey "astrology" (PGPLOT's look),
;;;; else PLOT-POINTS's :FONT, centered on their ink and scaled together
;;;; so all symbols match.  A symbol the font lacks comes from Hershey
;;;; "astrology", else from matching strokes: Hershey has no Libra,
;;;; drawn from lines and an arc, and no waxing moon, drawn as the
;;;; waning moon mirrored.
;;;;
;;;; Markers are sized in canvas units (SIZE is roughly the radius) and
;;;; drawn upright in the current drawing space, so they keep their size
;;;; and shape whatever the plot scales; in a rotated viewport they turn
;;;; with it.  Lines use the current line width and color.
;;;;
;;;; All markers of one call form a single path that is stroked or
;;;; filled once, which is fast for large data sets.  A consequence:
;;;; overlapping translucent filled markers do not darken where they
;;;; overlap.

(in-package :cl-cairoplot)

;;; ============================================================
;;; Marker definitions
;;; ============================================================
;;;
;;; A marker is a list of parts, each (:STROKE shape) or (:FILL shape),
;;; drawn in order.  Shapes are in unit coordinates, y up, fitting about
;;; a circle of radius 1:
;;;   (:POLYGON (x y) ...)        closed polygon
;;;   (:CIRCLE r)                 circle of radius r
;;;   (:SEGMENTS (x1 y1 x2 y2) ...) separate line segments
;;;   (:ARC xc yc r a1 a2)        open arc counterclockwise from A1 to A2
;;;                               degrees
;;; Two more parts draw text instead of a path, one glyph per point:
;;;   (:GLYPH char &key flip)     CHAR in the marker font (mirrored if FLIP)
;;;   (:SYMBOL char fallback)     CHAR if the marker font has it, else the
;;;                               list of parts FALLBACK

(defvar *markers* (make-hash-table :test 'eq)
  "Marker name -> list of parts.")

(defun define-marker (name &rest parts)
  "Define the marker NAME (a keyword) for PLOT-POINTS from PARTS, as the
   built-in markers are defined (see the top of markers.lisp)."
  (setf (gethash name *markers*) parts)
  name)

(defvar *astronomical-symbols* '()
  "Names of the astronomical symbol markers, in a conventional order.")

(defun marker-names (&optional all)
  "Names of the geometric markers, sorted; with ALL, also the
   astronomical symbols (see *ASTRONOMICAL-SYMBOLS*)."
  (sort (loop for k being the hash-keys of *markers*
              when (or all (not (member k *astronomical-symbols*)))
                collect k)
        #'string< :key #'symbol-name))

(defun regular-polygon (n &key (radius 1) (start 90))
  "Vertices of a regular N-gon of circumradius RADIUS, the first at
   angle START degrees."
  (loop for i below n
        for a = (* (/ pi 180) (+ start (/ (* 360 i) n)))
        collect (list (* radius (cos a)) (* radius (sin a)))))

(defun star-polygon (points &key (outer 1) (inner 0.4) (start 90))
  "Vertices of a star with POINTS points."
  (loop for i below (* 2 points)
        for a = (* (/ pi 180) (+ start (/ (* 180 i) points)))
        for r = (if (evenp i) outer inner)
        collect (list (* r (cos a)) (* r (sin a)))))

(defun spokes (angles)
  "Segments through the center at each of ANGLES (degrees)."
  (loop for d in angles
        for a = (* d (/ pi 180))
        collect (list (- (cos a)) (- (sin a)) (cos a) (sin a))))

;; shapes, scaled so their visual weight roughly matches a unit circle
(let ((square (regular-polygon 4 :radius 1.15 :start 45))
      (triangle (regular-polygon 3 :radius 1.25 :start 90))
      (triangle-down (regular-polygon 3 :radius 1.25 :start -90))
      (diamond '((0 1.25) (0.85 0) (0 -1.25) (-0.85 0)))
      (pentagon (regular-polygon 5 :radius 1.1))
      (hexagon (regular-polygon 6 :radius 1.1))
      (star (star-polygon 5 :outer 1.35 :inner 0.55)))
  (loop for (name shape) in `((circle (:circle 1))
                              (square (:polygon ,@square))
                              (triangle (:polygon ,@triangle))
                              (triangle-down (:polygon ,@triangle-down))
                              (diamond (:polygon ,@diamond))
                              (pentagon (:polygon ,@pentagon))
                              (hexagon (:polygon ,@hexagon))
                              (star (:polygon ,@star)))
        do (define-marker (intern (symbol-name name) :keyword) (list :stroke shape))
           ;; SYMBOL-NAME, not ~A, so *PRINT-CASE* cannot change the name
           (define-marker (intern (concatenate 'string "FILLED-" (symbol-name name))
                                  :keyword)
                          (list :fill shape)))
  (define-marker :dot '(:fill (:circle 0.35)))
  (define-marker :plus `(:stroke (:segments ,@(spokes '(0 90)))))
  (define-marker :cross `(:stroke (:segments ,@(spokes '(45 135)))))
  (define-marker :asterisk `(:stroke (:segments ,@(spokes '(90 30 150)))))
  (define-marker :circle-plus '(:stroke (:circle 1))
                 `(:stroke (:segments ,@(spokes '(0 90)))))
  (define-marker :circle-cross '(:stroke (:circle 1))
                 `(:stroke (:segments ,@(spokes '(45 135)))))
  (define-marker :circle-dot '(:stroke (:circle 1)) '(:fill (:circle 0.3)))
  (define-marker :square-cross `(:stroke (:polygon ,@square))
                 `(:stroke (:segments (-0.81 -0.81 0.81 0.81) (-0.81 0.81 0.81 -0.81))))
  (define-marker :hexagram `(:stroke (:polygon ,@triangle))
                 `(:stroke (:polygon ,@triangle-down))))

;; astronomical symbols
(loop for (name code) in '((:sun #x2609) (:mercury #x263F) (:venus #x2640)
                           (:earth #x2295) (:mars #x2642) (:jupiter #x2643)
                           (:saturn #x2644) (:uranus #x2645) (:neptune #x2646)
                           (:pluto #x2647) (:moon #x263E) (:comet #x2604)
                           (:ascending-node #x260A) (:descending-node #x260B)
                           (:aries #x2648) (:taurus #x2649) (:gemini #x264A)
                           (:cancer #x264B) (:leo #x264C) (:virgo #x264D)
                           (:scorpio #x264F) (:sagittarius #x2650)
                           (:capricorn #x2651) (:aquarius #x2652) (:pisces #x2653))
      do (define-marker name (list :symbol (code-char code) '())))
(setf *astronomical-symbols*
      '(:sun :moon :moon-waxing :mercury :venus :earth :mars :jupiter :saturn
        :uranus :neptune :pluto :comet :ascending-node :descending-node
        :aries :taurus :gemini :cancer :leo :virgo :libra :scorpio
        :sagittarius :capricorn :aquarius :pisces))
;; not in Hershey "astrology": the waxing moon is the waning one mirrored,
;; and Libra is a base line under a line with an arch (an omega) in it
(define-marker :moon-waxing (list :symbol (code-char #x263D)
                                  (list (list :glyph (code-char #x263E) :flip t))))
(define-marker :libra
  (list :symbol (code-char #x264E)
        '((:stroke (:segments (-1.1 -0.6 1.1 -0.6) (-1.1 -0.05 -0.3 -0.05)
                              (0.3 -0.05 1.1 -0.05)))
          ;; endpoints (+-0.3, -0.05): radius 0.5 at -53.13 and 233.13 degrees
          (:stroke (:arc 0 0.35 0.5 -53.13 233.13)))))

(defparameter *pgplot-markers*
  '((0 . :square) (1 . :dot) (2 . :plus) (3 . :asterisk) (4 . :circle)
    (5 . :cross) (6 . :square) (7 . :triangle) (8 . :circle-plus)
    (9 . :circle-dot) (10 . :square-cross) (11 . :diamond) (12 . :star)
    (13 . :filled-triangle) (14 . :plus) (15 . :hexagram)
    (16 . :filled-square) (17 . :filled-circle) (18 . :filled-star)
    (19 . :square))
  "PGPLOT symbol number -> marker name.  20-27 are circles of increasing
   size; -3 to -31 filled regular polygons.  10, 14, 15 and 19 only
   approximate PGPLOT's glyphs.")

(defun resolve-marker (spec)
  "Return the parts of marker SPEC and a size multiplier."
  (etypecase spec
    (keyword (values (or (gethash spec *markers*)
                         (error "Unknown marker ~S; see MARKER-NAMES." spec))
                     1))
    (integer
     (cond ((<= 20 spec 27)
            (values (gethash :circle *markers*) (+ 0.35 (* 0.2 (- spec 20)))))
           ((<= -31 spec -3)
            (values (list (list :fill (cons :polygon (regular-polygon (- spec)))))
                    1))
           ((<= -2 spec -1) (values (gethash :dot *markers*) 1))
           (t (let ((name (cdr (assoc spec *pgplot-markers*))))
                (unless name (error "No PGPLOT marker ~A." spec))
                (values (gethash name *markers*) (if (= spec 19) 1.6 1))))))))

;;; ============================================================
;;; Drawing
;;; ============================================================

(defun prepare-shape (shape size angle)
  "SHAPE with unit coordinates turned ANGLE degrees, scaled by SIZE and
   flipped to canvas orientation (y down): a list of double-float
   vectors of relative canvas coordinates, or (:CIRCLE radius)."
  (let* ((a (* angle (/ pi 180))) (c (cos a)) (s (sin a)))
    (flet ((xy (x y)
             ;; rotate counterclockwise, then flip y for the canvas
             (values (float (* size (- (* c x) (* s y))) 1d0)
                     (float (- (* size (+ (* s x) (* c y)))) 1d0))))
      (ecase (first shape)
        (:circle (list :circle (float (* size (second shape)) 1d0)))
        (:arc
         (destructuring-bind (xc yc r a1 a2) (rest shape)
           (multiple-value-bind (ox oy) (xy xc yc)
             ;; canvas angles run the other way (y down)
             (list :arc (float (* size r) 1d0) ox oy
                   (float (* (/ pi -180) (+ a1 angle)) 1d0)
                   (float (* (/ pi -180) (+ a2 angle)) 1d0)))))
        (:polygon
         (list :polygon
               (let ((v (make-array (* 2 (length (rest shape)))
                                    :element-type 'double-float)))
                 (loop for (x y) in (rest shape)
                       for i from 0 by 2
                       do (setf (values (aref v i) (aref v (1+ i))) (xy x y)))
                 v)))
        (:segments
         (list :segments
               (let ((v (make-array (* 4 (length (rest shape)))
                                    :element-type 'double-float)))
                 (loop for (x1 y1 x2 y2) in (rest shape)
                       for i from 0 by 4
                       do (setf (values (aref v i) (aref v (+ i 1))) (xy x1 y1)
                                (values (aref v (+ i 2)) (aref v (+ i 3))) (xy x2 y2)))
                 v)))))))

(defun add-shape-at (device prepared cx cy)
  "Add a PREPARED shape centered at canvas point (CX,CY) to the path.
   Caller must be inside WITH-BATCHED-GRAPHICS or WITH-CANVAS-LOCK."
  (declare (type double-float cx cy))
  (ecase (first prepared)
    (:circle
     (new-sub-path/tus device)
     (arc/tus device cx cy (second prepared) 0 (* 2 pi))
     (close-path/tus device))
    (:arc
     (destructuring-bind (r ox oy start end) (rest prepared)
       (new-sub-path/tus device)
       ;; counterclockwise on the page is decreasing canvas angle
       (arc-negative/tus device (+ cx ox) (+ cy oy) r start end)))
    (:polygon
     (let ((v (second prepared)))
       (declare (type (simple-array double-float (*)) v))
       (move-to/tus device (+ cx (aref v 0)) (+ cy (aref v 1)))
       (loop for i from 2 below (length v) by 2
             do (line-to/tus device (+ cx (aref v i)) (+ cy (aref v (1+ i)))))
       (close-path/tus device)))
    (:segments
     (let ((v (second prepared)))
       (declare (type (simple-array double-float (*)) v))
       (loop for i from 0 below (length v) by 4
             do (move-to/tus device (+ cx (aref v i)) (+ cy (aref v (+ i 1))))
                (line-to/tus device (+ cx (aref v (+ i 2))) (+ cy (aref v (+ i 3)))))))))

(defun default-marker-size (device)
  (* 0.3 (default-text-size device)))

(defun glyph-em (device font size)
  "The em size at which FONT's glyphs match markers of SIZE: the sun
   symbol (or \"O\" if FONT lacks it) is 2*SIZE tall."
  (with-saved-state (device)
    (apply-font device (copy-font font :size 100))
    (let ((ref (if (glyph-present-p device (code-char #x2609))
                   (string (code-char #x2609))
                   "O")))
      (* 100 (/ (* 2 size) (nth-value 3 (text-extents device ref)))))))

(defun font-has-glyph-p (device font char)
  (with-saved-state (device)
    (apply-font device font)
    (glyph-present-p device char)))

(defun draw-glyph-markers (device string xs ys m em font angle flip
                           &optional halo-color halo-width)
  "Draw STRING in FONT at size EM centered on its ink at each point; with
   HALO-COLOR, draw only the glyphs' halos."
  ;; with-saved-state holds the canvas lock; internal /tus variants are used
  (with-saved-state (device)
    (apply-font device (copy-font font :size em))
    (multiple-value-bind (xb yb w h) (text-extents device string)
      (let ((dx (- (+ xb (/ w 2))))
            (dy (- (+ yb (/ h 2)))))
        (flet ((glyph (x y)
                 (if halo-color
                     (progn (new-path/tus device)
                            (move-to/tus device x y)
                            (text-path/tus device string)
                            (stroke-halo device halo-color halo-width))
                     (draw-text/tus device x y string))))
          (do-points (x y xs ys)
            (multiple-value-bind (cx cy) (affine-apply m x y)
              (if (and (zerop angle) (not flip))
                  (glyph (+ cx dx) (+ cy dy))
                  (with-saved-state (device)
                    (translate/tus device cx cy)
                    (rotate/tus device (* angle (/ pi -180)))   ; canvas y is down
                    (when flip (scale/tus device -1 1))
                    (glyph dx dy))))))))))

(defplot plot-points (device xs ys &key (marker :filled-circle) size (angle 0)
                                      (coords :plot) (font :astrology) (halo *halo*)
                                      colors sizes (ink t))
  "Draw MARKER at each point of the sequences XS, YS in COORDS (as
   PGPLOT's PGPT).  MARKER is a marker name, PGPLOT symbol number or
   function (see the top of markers.lisp).  SIZE, about the marker's
   radius in canvas units, defaults to 0.3 of DEFAULT-TEXT-SIZE.  ANGLE
   turns each marker by that many degrees counterclockwise.  FONT is
   the font spec for glyph markers (astronomical symbols).  HALO
   outlines the markers (see WITH-HALO; default width 0.18 SIZE).
   COLORS and SIZES, sequences, give each point its own color spec and
   size (see VALUES-TO-COLORS and VALUES-TO-SIZES); a NIL color uses the
   current one.  INK NIL draws only the halos."
  (when (member coords '(:sky :polar))
    (setf (values xs ys) (curved-to-plot-sequences device xs ys coords)
          coords :plot))
  (when (or colors sizes)
    ;; points one at a time: all halos first, then all markers
    (let ((n (min (length xs) (length ys))))
      (flet ((one (k halo ink)
               (with-saved-state (device)
                 (let ((color (and colors (elt colors k))))
                   (when color (set-color device color)))
                 (plot-points device (list (elt xs k)) (list (elt ys k))
                              :marker marker :size (if sizes (elt sizes k) size) :angle angle
                              :coords coords :font font :halo halo :ink ink))))
        (when (parse-halo halo) (dotimes (k n) (one k halo nil)))
        (when ink (dotimes (k n) (one k nil t)))))
    (return-from plot-points nil))
  (let ((size (or size (default-marker-size device)))
        (m (coords-transform device coords))
        (font (parse-font font)))
    (let ((fp (recording-footprint device)))
      (when fp
        (do-points (x y xs ys)
          (multiple-value-bind (cx cy) (affine-apply m x y)
            (note-point fp cx cy)))))
    ;; set per halo layer by DO-HALO-LAYERS below
    (let (halo-color halo-width)
      (labels ((draw-parts (parts size halo-pass)
                 ;; HALO-PASS draws only the parts' halos
                 (dolist (part parts)
                   (ecase (first part)
                     ((:stroke :fill)
                      ;; one path per part over all points, then a single
                      ;; stroke or fill; one lock for the whole batch
                      (let ((prepared (prepare-shape (second part) size angle)))
                        (with-batched-graphics (device)
                          (new-path device)
                          (do-points (x y xs ys)
                            (multiple-value-bind (cx cy) (affine-apply m x y)
                              (add-shape-at device prepared cx cy)))
                          (cond (halo-pass
                                 (stroke-halo device halo-color halo-width
                                              (if (eq (first part) :stroke)
                                                  (line-width device)
                                                  0)))
                                ((eq (first part) :stroke) (stroke device))
                                (t (fill-path device))))))
                     (:glyph
                      (destructuring-bind (char &key flip) (rest part)
                        (draw-glyph-markers device (string char) xs ys m
                                            (glyph-em device font size) font angle flip
                                            (and halo-pass halo-color) halo-width)))
                     (:symbol
                      ;; the marker font's glyph, else Hershey astrology's,
                      ;; else the stroked FALLBACK
                      (destructuring-bind (char fallback) (rest part)
                        (cond ((font-has-glyph-p device font char)
                               (draw-glyph-markers device (string char) xs ys m
                                                   (glyph-em device font size)
                                                   font angle nil
                                                   (and halo-pass halo-color) halo-width))
                              ((and (not (eq font (parse-font :astrology)))
                                    (font-has-glyph-p device (parse-font :astrology) char))
                               (let ((astrology (parse-font :astrology)))
                                 (draw-glyph-markers device (string char) xs ys m
                                                     (glyph-em device astrology size)
                                                     astrology angle nil
                                                     (and halo-pass halo-color) halo-width)))
                              ((typep font 'hershey-font)
                               ;; strokes as thick as the Hershey pen
                               (with-line-width
                                   (device (* (or (hershey-font-pen-width font)
                                                  cl-cairo-hershey-fonts:*default-pen-width*)
                                              (glyph-em device font size)))
                                 (draw-parts fallback size halo-pass)))
                              (t (draw-parts fallback size halo-pass))))))))
               (draw-all (halo-pass)
                 (if (functionp marker)
                     ;; custom marker function: one lock for the whole batch
                     (with-batched-graphics (device)
                       (new-path device)
                       (do-points (x y xs ys)
                         (multiple-value-bind (cx cy) (affine-apply m x y)
                           (funcall marker device cx cy size)))
                       (if halo-pass
                           (stroke-halo device halo-color halo-width)
                           (fill-path device)))
                     (multiple-value-bind (parts scale) (resolve-marker marker)
                       (draw-parts parts (* scale size) halo-pass)))))
        ;; all the halos first, then the markers over them
        (do-halo-layers (device halo (* 0.18 size) layer-color layer-width)
          (setf halo-color layer-color halo-width layer-width)
          (draw-all t))
        (when ink (draw-all nil))))))

(defun plot-point (device x y &rest keys &key marker size angle coords font halo ink)
  "Draw one marker at (X,Y); keywords as for PLOT-POINTS."
  (declare (ignore marker size angle coords font halo ink))
  (apply #'plot-points device (list x) (list y) keys))
