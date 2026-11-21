;;;; heatmap.lisp — annotated heatmaps
;;;;
;;;; HEATMAP draws a whole chart in a viewport: a matrix of cells colored
;;;; through a colormap, separated by thin gaps, each with its value
;;;; written in black or white (whichever shows on the cell), row and
;;;; column names on the axes (row 0 at the top, as the matrix is
;;;; written), and optionally a colorbar.  For signed data such as
;;;; correlations, give a diverging colormap and CENTER (e.g. 0): the
;;;; limits are then made symmetric about it, so it maps to the neutral
;;;; middle.

(in-package :cl-cairoplot)

(defun matrix-rows (matrix)
  "MATRIX, a 2D array or a list of row sequences, as a list of lists."
  (if (arrayp matrix)
      (destructuring-bind (rows cols) (array-dimensions matrix)
        (loop for r below rows collect (loop for c below cols collect (aref matrix r c))))
      (mapcar (lambda (row) (coerce row 'list)) matrix)))

(defplot heatmap (viewport matrix &key row-names col-names (colormap :viridis) vmin vmax
                                  center (scale :linear) reverse (annotate t)
                                  (value-format #'default-value-text) (gap-color :white)
                                  (gap-width 1) (nan-color :light-gray) colorbar
                                  colorbar-label col-angle x-label y-label title size
                                  (cols-on :bottom))
  "Draw MATRIX (a 2D array or list of rows; NaN or NIL cells in
   NAN-COLOR) as a heatmap filling VIEWPORT, whose window it sets: cell
   (row R, column C) is centered on x = C, y = R, row 0 at the top.
   Cells are colored through COLORMAP with SCALE and REVERSE (as
   PLOT-IMAGE) over VMIN..VMAX (default the data's range, made symmetric
   about CENTER if given) and separated by GAP-WIDTH lines of GAP-COLOR.
   ANNOTATE writes each value with VALUE-FORMAT (a function of the
   value) in black or white for contrast.  ROW-NAMES and COL-NAMES label
   the axes (COLS-ON :BOTTOM or :TOP; COL-ANGLE slants them).  COLORBAR
   draws one beside, with COLORBAR-LABEL.  Returns VMIN and VMAX."
  (let* ((device (viewport-device viewport))
         (rows (matrix-rows matrix))
         (nrows (length rows))
         (ncols (reduce #'max rows :key #'length))
         (cells (loop for row in rows append (remove-if (lambda (v) (or (null v) (nan-p v))) row)))
         (lo (or vmin (reduce #'min cells)))
         (hi (or vmax (reduce #'max cells)))
         (size (or size (* 0.8 (default-text-size device)))))
    (when center
      (let ((half (max (abs (- hi center)) (abs (- lo center)))))
        (unless vmin (setf lo (- center half)))
        (unless vmax (setf hi (+ center half)))))
    (set-window viewport -0.5 (- ncols 0.5) (- nrows 0.5) -0.5)
    (let ((cm (parse-colormap (if reverse (reverse-colormap colormap) colormap)))
          (f (scale-function scale))
          (range (if (= hi lo) 1 (- hi lo))))
      (loop for row in rows
            for r from 0
            do (loop for v in row
                     for c from 0
                     do (let ((color (if (or (null v) (nan-p v))
                                         nan-color
                                         (multiple-value-list
                                          (colormap-rgb cm (funcall f (max 0 (min 1 (/ (- v lo) range)))))))))
                          (with-color (viewport color)
                            (plot-rectangle viewport (- c 0.5) (- r 0.5) (+ c 0.5) (+ r 0.5)
                                            :fill t))
                          (when (and annotate v (not (nan-p v)))
                            (with-color (viewport (if (< (color-luminance color) 0.18)
                                                      :white :black))
                              (plot-text viewport c r (funcall value-format v)
                                         :halign :center :valign :center :size size)))))))
    (when (and gap-color (plusp gap-width))
      (with-style (viewport :color gap-color :line-width gap-width)
        (loop for c from 1 below ncols
              do (plot-line viewport (- c 0.5) -0.5 (- c 0.5) (- nrows 0.5)))
        (loop for r from 1 below nrows
              do (plot-line viewport -0.5 (- r 0.5) (- ncols 0.5) (- r 0.5)))))
    (let ((col-side (if (eq cols-on :top) :top :bottom)))
      (draw-box viewport :frame nil :ticks '() :minor nil
                         :numbers (append (when col-names (list col-side))
                                          (when row-names '(:left)))
                         :x-ticks (category-ticks (or col-names '()))
                         :y-ticks (category-ticks (or row-names '()))
                         :x-numbers (or col-angle :horizontal)
                         :x-label x-label :y-label y-label :title title))
    (when colorbar
      (draw-colorbar (colorbar-viewport viewport :gap 0.015) :colormap colormap :vmin lo :vmax hi
                                                             :scale scale :reverse reverse
                                                             :label colorbar-label))
    (values lo hi)))
