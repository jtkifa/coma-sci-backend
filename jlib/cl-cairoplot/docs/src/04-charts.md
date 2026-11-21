# Charts

This chapter shows each kind of chart with the code that draws it. The examples
use a few helper functions for repeatable synthetic data:

```lisp
(defun noise (k)
  "A repeatable pseudo-random number between -0.5 and 0.5."
  (- (mod (* 43758.5453d0 (sin (* 12.9898d0 k))) 1) 0.5))

(defun gaussian (k)
  "A repeatable, roughly normal pseudo-random number (mean 0, sigma 1)."
  (loop for i below 12 sum (noise (+ (* 12 k) i 7919))))

(defun normal-sample (n mean sigma seed)
  (loop for k below n collect (+ mean (* sigma (gaussian (+ seed k))))))

(defun samples (f x1 x2 &optional (n 200))
  "Values XS and YS: F at N + 1 points from X1 to X2."
  (let ((xs (loop for k from 0 to n collect (+ x1 (* k (/ (- x2 x1) n))))))
    (values xs (mapcar f xs))))
```

## Lines and points

`plot-polyline` joins points with lines, `plot-points` draws markers, and
`plot-line` draws one segment. Shapes in plot coordinates are drawn with
`plot-rectangle`, `plot-polygon`, `plot-circle` and `plot-ellipse`, filled with
`:fill t`, and `plot-arrow` draws arrows:

```{.lisp image="lines-points.png"}
(defun damped (x)
  (* (exp (/ x -6)) (sin (* 2 x))))

(with-device (d :pdf "lines.pdf" :size 560 :aspect 0.55)
  (set-window d 0 10 -1.4 1.4)
  (multiple-value-bind (xs ys) (samples #'damped 0 10)
    (with-style (d :color "#2a78d6" :line-width 1.5)
      (plot-polyline d xs ys)))
  (let ((xs (loop for x from 0.25 to 10 by 0.5 collect x)))
    (with-color (d "#eb6834")
      (plot-points d xs
                   (mapcar (lambda (x)
                             (+ (damped x) (* 0.3 (noise (* 7 x)))))
                           xs)
                   :marker :filled-circle :size 3)))
  (with-style (d :dash '(4 3) :color :gray)
    (plot-line d 0 0 10 0))
  (with-color (d '(0.11 0.69 0.48 0.3))
    (plot-ellipse d 7.5 0.9 1.2 0.3 :angle 15 :fill t))
  (plot-arrow d 5.5 1.15 6.4 0.95)
  (draw-box d :x-label "time" :y-label "signal"))
```

![](images/lines-points.png)

## Points colored by value

`values-to-colors` turns data into colors through a colormap, and
`values-to-sizes` into marker sizes (area proportional to the value); both
also return the range they used, for `draw-colorbar`. `colorbar-viewport`
places a colorbar beside a viewport:

```{.lisp image="colored-points.png"}
(with-device (d :pdf "colored.pdf" :size 560 :aspect 0.6)
  (let* ((vp (make-viewport d 0.1 0.8 0.13 0.9))
         (xs (loop for k below 300 collect (+ 0.6 (* 0.25 (gaussian k)))))
         (zs (loop for x in xs
                   for k from 0
                   collect (max 0.01 (+ (* 1.5 (- x 0.4))
                                        (* 0.15 (gaussian (+ k 999)))))))
         (ys (loop for z in zs
                   for k from 0
                   collect (+ 19 (* 4 z) (* 0.8 (gaussian (+ k 3333)))))))
    (set-window vp 0 1.3 25 17)                ; magnitudes: brighter up
    (multiple-value-bind (colors lo hi)
        (values-to-colors zs :colormap :plasma :vmin 0 :vmax 1.2)
      (plot-points vp xs ys :marker :filled-circle :colors colors
                            :sizes (values-to-sizes ys :min-size 6
                                                       :max-size 1.5))
      (draw-colorbar (colorbar-viewport vp) :colormap :plasma
                                            :vmin lo :vmax hi
                                            :label "redshift"))
    (draw-box vp :x-label "color index" :y-label "magnitude")))
```

![](images/colored-points.png)

## Error bars and bands

`plot-error-bars` draws error bars (`:yerr`, `:xerr`, or `:yerr-lower` and
`:yerr-upper` for unequal ones) with caps `:cap-size` points wide, and
`plot-fill-between` fills the area between two curves, or a curve and a number.
The [Tutorial](#tutorial-a-complete-figure) shows both.

## Histograms

`plot-histogram` bins values and draws them as a step outline (`:style
:step`), a filled area (`:filled`) or separate bars (`:bars`). `:bins` is a
number, a list of edges or `:auto`; `:normalize` is `:density` (area one) or
`:fraction`; `:cumulative` sums the bins. `histogram` returns the edges and
counts without drawing:

```{.lisp image="histograms.png"}
(with-device (d :pdf "histograms.pdf" :size 600 :aspect 0.4)
  (let ((a (normal-sample 2000 0 1 0))
        (b (normal-sample 1500 1.5 0.7 5000))
        (grid (make-panel-grid d 1 2 :left 0.08 :right 0.98
                                     :bottom 0.17 :top 0.88 :wspace 0.1)))
    (let ((vp (aref grid 0 0)))
      (set-window vp -4 4 0 0.65)
      (with-color (vp '(0.16 0.47 0.84 0.4))
        (plot-histogram vp a :bins 40 :range '(-4 4)
                             :style :filled :normalize :density))
      (with-color (vp '(0.92 0.41 0.2 0.4))
        (plot-histogram vp b :bins 40 :range '(-4 4)
                             :style :filled :normalize :density))
      (draw-box vp :x-label "x" :y-label "density"
                   :title ":filled, :normalize :density")
      (draw-legend vp '(("A" :fill (0.16 0.47 0.84 0.4))
                        ("B" :fill (0.92 0.41 0.2 0.4)))))
    (let ((vp (aref grid 0 1)))
      (set-window vp -4 4 0 1.05)
      (with-style (vp :color "#6250d6" :line-width 1.2)
        (plot-histogram vp a :bins 80 :range '(-4 4)
                             :cumulative t :normalize :fraction))
      (draw-box vp :x-label "x" :title ":cumulative t"))))
```

![](images/histograms.png)

`plot-histogram2d` bins points on a grid and shows the counts as an image; it
returns the range for a colorbar.

## Bar charts

`bar-chart` draws a whole chart in a viewport: its window, the bars, the axes
with the category names, and a legend for several series. Data is one list of
values, or a list of series. `:mode :stacked` piles the series; `:orientation
:horizontal` lays the bars sideways; `:errors` adds error bars; `:value-labels`
writes the values at the bar ends:

```{.lisp image="bar-charts.png"}
(with-device (d :pdf "bars.pdf" :size 640 :aspect 0.42)
  (let ((grid (make-panel-grid d 1 2 :left 0.08 :right 0.98
                                     :bottom 0.15 :top 0.88 :wspace 0.1)))
    (bar-chart (aref grid 0 0) '("g" "r" "i" "z")
               '((412 655 598 431) (380 702 640 470))
               :names '("night 1" "night 2") :errors '(25 30)
               :value-label "detections" :title "grouped")
    (bar-chart (aref grid 0 1) '("Q1" "Q2" "Q3" "Q4")
               '((40 55 30 62) (25 20 35 28))
               :mode :stacked :value-labels t
               :names '("granted" "director's")
               :value-label "hours" :title "stacked")))
```

![](images/bar-charts.png)

`plot-bars` draws bars at any positions in an existing window, for building
other charts. `category-ticks` names categories on an axis (positions 0, 1, 2,
...), and `:category-angle` slants long names.

## Box plots and violin plots

`plot-box-plot` draws a box from the first to the third quartile with the
median, whiskers to the farthest values within 1.5 times the box's length, and
the values beyond as points; `:means` marks the means. `plot-violins` draws
each distribution's estimated density, mirrored, with a box or quartile lines
inside. Both place datasets at given positions, so the axes are drawn
separately, here with `category-ticks`:

```{.lisp image="box-violin.png"}
(with-device (d :pdf "distributions.pdf" :size 640 :aspect 0.42)
  (let* ((names '("A" "B" "C" "D"))
         (data (list (normal-sample 200 0.8 0.08 100)
                     (normal-sample 200 1.1 0.2 1000)
                     (append (normal-sample 120 0.75 0.07 3000)
                             (normal-sample 120 1.35 0.09 4000))
                     (normal-sample 30 1.0 0.15 5000)))
         (positions '(0 1 2 3))
         (colors (category-colors 4))
         (grid (make-panel-grid d 1 2 :left 0.08 :right 0.98
                                      :bottom 0.13 :top 0.88 :wspace 0.08)))
    (let ((vp (aref grid 0 0)))
      (set-window vp -0.6 3.6 0.4 1.8)
      (plot-box-plot vp positions data :colors colors :means t)
      (draw-box vp :x-ticks (category-ticks names) :ticks '(:left)
                   :y-label "value" :title "box plots"))
    (let ((vp (aref grid 0 1)))
      (set-window vp -0.6 3.6 0.4 1.8)
      (plot-violins vp positions data :colors colors)
      (draw-box vp :x-ticks (category-ticks names) :ticks '(:left)
                   :title "violins"))))
```

![](images/box-violin.png)

`box-stats` returns a sample's quartiles, whiskers and outliers, and `kde` its
density estimate.

## Step plots and stacked areas

`plot-step` draws a staircase through points, stepping at each point
(`:where :post`), before it (`:pre`) or halfway (`:mid`). Given one more x than
y, the x values are bin edges. `:fill` fills beneath it with a color.

`plot-stacked-area` stacks several series and fills each layer in its color,
with thin white lines between layers. `:normalize t` shows percentages,
`:baseline :wiggle` with `:order :inside-out` makes a streamgraph, `:labels t`
writes the names inside the layers where they fit, and the result is a list of
legend entries, top layer first:

```{.lisp image="areas.png"}
(defun bump (x center width height)
  (* height (exp (- (/ (expt (- x center) 2) width)))))

(with-device (d :pdf "areas.pdf" :size 640 :aspect 0.42)
  (let ((grid (make-panel-grid d 1 2 :left 0.08 :right 0.98
                                     :bottom 0.15 :top 0.88 :wspace 0.1))
        (years (loop for y from 2000 to 2024 collect y)))
    (let ((vp (aref grid 0 0))
          (edges (loop for k from 0 to 20 collect (* 30 k)))
          (counts (loop for k below 20
                        collect (round (+ 40 (* 6 (gaussian (+ 300 k)))
                                          (bump k 8 4 70))))))
      (set-window vp 0 600 0 130)
      (with-style (vp :color "#2a78d6" :line-width 1.2)
        (plot-step vp edges counts :fill '(0.16 0.47 0.84 0.2)))
      (draw-box vp :x-label "time (s)" :y-label "counts"
                   :title "plot-step over bin edges"))
    (let ((vp (aref grid 0 1))
          (series
            (list (mapcar (lambda (y) (+ 5 (* 0.2 (- y 2000)))) years)
                  (mapcar (lambda (y) (bump y 2008 30 8)) years)
                  (mapcar (lambda (y) (/ 12 (+ 1 (exp (/ (- 2016 y) 2)))))
                          years))))
      (set-window vp 2000 2024 0 100)
      (plot-stacked-area vp years series
                         :names '("survey A" "survey B" "survey C")
                         :normalize t :labels t)
      (draw-box vp :x-label "year" :y-label "share (%)"
                   :title "stacked, :normalize t"))))
```

![](images/areas.png)

`stack-series` returns the layers and the stack's range without drawing, to
set a window first.

## Pie and donut charts

`pie-chart` draws a whole chart in a viewport: wedges from 12 o'clock
clockwise, percentages inside the wedges with room for them, and names
outside with leader lines. `:inner` makes a donut, with `:center-text` in its
hole; `:labels nil` leaves the names to a legend, using the entries it returns:

```{.lisp image="pies.png"}
(with-device (d :pdf "pies.pdf" :size 640 :aspect 0.42)
  (pie-chart (make-viewport d 0.0 0.48 0.03 0.95) '(41 23 17 11 8)
             :names '("photometry" "astrometry" "spectroscopy" "imaging"
                      "other")
             :title "telescope time")
  (let ((vp (make-viewport d 0.52 0.78 0.08 0.9)))
    (draw-legend vp (pie-chart vp '(1406 612 288 194)
                               :names '("main belt" "NEOs" "Trojans"
                                        "comets")
                               :inner 0.55 :center-text "2500" :labels nil
                               :title "orbits")
                 :position :outside-right)))
```

![](images/pies.png)

## Heatmaps

`heatmap` draws a matrix (a 2D array or a list of rows) as colored cells with
each value written in black or white, whichever shows on the cell, and row and
column names on the axes. For values around a center, such as correlations,
`:center` makes a diverging colormap symmetric about it; `nil` cells are shown
in gray:

```{.lisp image="heatmap.png"}
(with-device (d :pdf "heatmap.pdf" :size 520 :aspect 0.7)
  (let ((names '("g - r" "r - i" "mag" "period")))
    (heatmap (make-viewport d 0.16 0.78 0.08 0.8)
             '((1 0.82 -0.31 0.12)
               (0.82 1 -0.27 nil)
               (-0.31 -0.27 1 -0.52)
               (0.12 nil -0.52 1))
             :row-names names :col-names names :cols-on :top
             :colormap :coolwarm :center 0
             :colorbar t :colorbar-label "r"
             :value-format (lambda (v) (format nil "~,2F" v)))))
```

![](images/heatmap.png)

## Images and colorbars

`plot-image` draws a 2D array through a colormap. `:vmin` and `:vmax` set the
values at the ends of the colormap (by default the data's range, or the central
`:percentile` of it); `:scale` is `:linear`, `:sqrt`, `:log`, `:asinh` or
`:square` for faint detail; `:x-range` and `:y-range` place the image's outer
edges in plot coordinates (by default, pixel centers fall on whole numbers).
It returns the range used, for `draw-colorbar`:

```{.lisp image="image.png"}
(defun star (i j x y flux sigma)
  "A star of FLUX at pixel X, Y, as seen at pixel I, J."
  (* flux (exp (/ (+ (expt (- i x) 2) (expt (- j y) 2))
                  (* -2 sigma sigma)))))

(defun ring (i j x y radius height)
  (let ((r (sqrt (+ (expt (- i x) 2) (expt (- j y) 2)))))
    (* height (exp (- (expt (/ (- r radius) 4) 2))))))

(with-device (d :pdf "image.pdf" :size 520 :aspect 0.7)
  (let ((vp (make-viewport d 0.1 0.8 0.1 0.92))
        (image (make-array '(120 160))))
    (dotimes (j 120)
      (dotimes (i 160)
        (setf (aref image j i)
              (+ 10 (star i j 60 70 1000 3) (ring i j 110 50 25 25)
                 (* 3 (noise (+ i (* 1000 j))))))))
    (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
    (multiple-value-bind (lo hi)
        (plot-image vp image :colormap :inferno :scale :asinh
                             :vmin 0 :vmax 400)
      (draw-colorbar (colorbar-viewport vp) :colormap :inferno :scale :asinh
                                            :vmin lo :vmax hi
                                            :label "counts"))
    (draw-box vp :x-label "x (pixels)" :y-label "y (pixels)")))
```

![](images/image.png)

`:transform` places an image by an affine transform from pixel to plot
coordinates, rotated or sheared if need be, and `:wcs` through a world
coordinate system (see [Astronomy](#astronomy)). `:interpolation :bilinear`
smooths enlarged pixels.

## Contours

`plot-contours` draws contour lines of a 2D array, at `:levels` or about `:n`
round levels, placed as `plot-image` places the array. `:labels t` writes the
levels along the lines (`:curved` bends them along the line), `:colormap`
colors each level, and `:negative-dash` dashes the negative ones.
`plot-filled-contours` fills the bands between levels, and returns them for
`draw-colorbar :levels`:

```{.lisp image="contours.png"}
(with-device (d :pdf "contours.pdf" :size 560 :aspect 0.62)
  (let ((vp (make-viewport d 0.1 0.8 0.12 0.92))
        (z (make-array '(81 101)))
        (x-range '(-5.05 5.05))
        (y-range '(-4.05 4.05)))
    (dotimes (j 81)
      (dotimes (i 101)
        (let ((x (- (/ i 10.0) 5)) (y (- (/ j 10.0) 4)))
          (setf (aref z j i) (+ (* (sin x) (cos y)) (* 0.1 x))))))
    (set-window-equal-scales vp -5 5 -4 4)
    (let ((levels (plot-filled-contours vp z :x-range x-range
                                             :y-range y-range
                                             :colormap :viridis :n 10)))
      (draw-colorbar (colorbar-viewport vp) :colormap :viridis
                                            :levels levels))
    (with-color (vp :black)
      (plot-contours vp z :x-range x-range :y-range y-range :n 10
                          :labels t :negative-dash '(3 2)))
    (draw-box vp :x-label "x" :y-label "y")))
```

![](images/contours.png)

`contour-lines` returns the lines of one level as coordinate vectors, for other
uses.

## Vector fields and streamlines

`plot-vectors` draws an arrow for each vector (u, v) at each point (x, y).
`:pivot` puts the point at the arrow's tail, middle or tip; `:heads nil` draws
plain segments, for orientations such as polarization; `:colormap` colors by
length. `plot-vector-field` samples a function of x and y, returning u and v,
on a grid, and scales the arrows to the grid. `plot-streamlines` draws evenly
spaced streamlines of such a function, with arrowheads showing the direction:

```{.lisp image="vectors.png"}
(defun vortex (x y)
  "The flow of two opposite vortices at (-1, 0) and (1, 0)."
  (let ((u 0.15) (v 0))
    (loop for (x0 gamma) in '((-1 1) (1 -1))
          do (let* ((dx (- x x0)) (r2 (+ (* dx dx) (* y y) 0.15)))
               (incf u (/ (* gamma (- y)) r2))
               (incf v (/ (* gamma dx) r2))))
    (values u v)))

(with-device (d :pdf "vectors.pdf" :size 640 :aspect 0.42)
  (let ((grid (make-panel-grid d 1 2 :left 0.06 :right 0.97
                                     :bottom 0.12 :top 0.88 :wspace 0.08)))
    (let ((vp (aref grid 0 0)))
      (set-window-equal-scales vp -3 3 -2 2)
      (plot-vector-field vp #'vortex '(-3 3) '(-2 2)
                         :n 18 :colormap :viridis)
      (draw-box vp :title "plot-vector-field"))
    (let ((vp (aref grid 0 1)))
      (set-window-equal-scales vp -3 3 -2 2)
      (with-style (vp :color "#2a78d6" :line-width 0.8)
        (plot-streamlines vp #'vortex '(-3 3) '(-2 2)))
      (draw-box vp :title "plot-streamlines"))))
```

![](images/vectors.png)

## Polar plots

`set-window-polar` gives a viewport a polar mapping; after it, every plotting
function takes `:coords :polar`, with points as an angle in degrees and a
radius. Lines and polygons follow the angle. The mapping sets where angle 0
points (`:zero :east`, the mathematical convention, or `:north` for bearings
and position angles), the `:direction`, the radius at the center (`:r-min`,
which need not be 0) and at the rim, and optionally a `:sector`.
`draw-polar-grid` draws the rim, circles and spokes with their labels, and
`plot-polar-bars` draws wedges, which stack with `:bottoms`:

```{.lisp image="polar.png"}
(with-device (d :pdf "polar.pdf" :size 640 :aspect 0.48)
  (let ((grid (make-panel-grid d 1 2 :left 0.03 :right 0.97
                                     :bottom 0.04 :top 0.9 :wspace 0.06))
        (angles (loop for a from 0 to 360 collect a)))
    (let ((vp (aref grid 0 0)))
      (set-window-polar vp 2)
      (draw-polar-grid vp :title "r = 1 + 0.8 cos θ")
      (with-style (vp :color "#2a78d6" :line-width 1.5)
        (plot-polyline vp angles
                       (mapcar (lambda (a)
                                 (+ 1 (* 0.8 (cos (* a (/ pi 180))))))
                               angles)
                       :coords :polar)))
    (let ((vp (aref grid 0 1))
          (bearings (loop for k below 8 collect (* 45 k)))
          (calm '(3 2 1 1 2 4 6 5))
          (windy '(2 1 0.5 0.5 1 3 5 3)))
      (set-window-polar vp 12 :zero :north :direction :clockwise)
      (draw-polar-grid vp :angle-step 45
                          :angle-labels '("N" "NE" "E" "SE"
                                          "S" "SW" "W" "NW")
                          :title "wind rose")
      (plot-polar-bars vp bearings calm :color "#2a78d6")
      (plot-polar-bars vp bearings windy :bottoms calm :color "#eb6834"))))
```

![](images/polar.png)

## Corner plots

`corner-plot` shows every pair of parameters of a set of samples, such as those
of a Markov chain Monte Carlo fit, as the corner.py package does: each
parameter's histogram on the diagonal, titled with its median and the spread to
its 16th and 84th percentiles, and each pair's density below as contours
enclosing 1 and 2 sigma of the samples. `:truths` marks known values:

```{.lisp image="corner.png"}
(with-device (d :pdf "corner.pdf" :size 520 :aspect 1)
  (let ((a '()) (b '()) (c '()))
    (dotimes (k 4000)
      (let ((z1 (gaussian (+ 10000 k)))
            (z2 (gaussian (+ 20000 k)))
            (z3 (gaussian (+ 30000 k))))
        (push (+ 2.75 (* 0.02 z1)) a)
        (push (+ 0.18 (* 0.015 (+ (* 0.6 z1) (* 0.8 z2)))) b)
        (push (+ 9.3 (* 0.12 z3) (* 0.3 z2 z2)) c)))
    (corner-plot d (list a b c) :names '("a (AU)" "e" "i (°)")
                                :truths '(2.75 0.18 9.5)
                                :left 0.14 :bottom 0.13 :top 0.94)))
```

![](images/corner.png)

## 3D surfaces

`make-view3d` sets up a view of a box of data from an azimuth and an
elevation; `plot-surface` draws a surface z = f(x, y) given as a 2D array,
colored by height or by another array, shaded, with contour lines on it if
asked; `draw-axes3d` draws the box, grid and labeled axes, the part behind
the surface first:

```{.lisp image="surface.png"}
(defun sombrero (x y)
  (let ((r (sqrt (+ (* x x) (* y y)))))
    (if (zerop r) 1d0 (/ (sin (* 2 r)) (* 2 r)))))

(with-device (d :pdf "surface.pdf" :size 520 :aspect 0.8)
  (let* ((z (make-array '(41 41)))
         (view (make-view3d (make-viewport d 0.03 0.97 0.03 0.97)
                            :x-range '(-3 3) :y-range '(-3 3)
                            :z-range '(-0.5 1)
                            :azimuth -60 :elevation 30)))
    (dotimes (j 41)
      (dotimes (i 41)
        (setf (aref z j i) (sombrero (- (* 0.15 i) 3) (- (* 0.15 j) 3)))))
    (draw-axes3d view :part :back :grid t)
    (plot-surface view z :colormap :viridis :shade 0.5)
    (draw-axes3d view :part :front :x-label "x" :y-label "y" :z-label "z")))
```

![](images/surface.png)

`plot3d-polyline`, `plot3d-points` and `plot3d-text` draw lines, markers and
text at 3D points, and `plot-floor-contours` draws contours on the floor of the
box.

## Date and time axes

`time-ticks` is a tick generator for an axis of times: Modified Julian Dates
(`:scale :mjd`, the default), Julian Dates (`:jd`), Unix seconds (`:unix`) or
Lisp universal time (`:universal`). It puts ticks at round calendar steps, from
seconds to centuries, and labels them briefly: the year at January, the month
at its first day, the date at midnight. `calendar-to-mjd`, `mjd-to-calendar`,
`iso-to-mjd`, `time-to-unix` and `unix-to-time` convert times:

```{.lisp image="time-axis.png"}
(with-device (d :pdf "time.pdf" :size 600 :aspect 0.42)
  (let ((start (iso-to-mjd "2024-03-01"))
        (end (iso-to-mjd "2024-07-01")))
    (set-window d start end 15.5 13.5)
    (with-color (d "#2a78d6")
      (plot-points d (loop for k below 60 collect (+ start (* 2 k)))
                   (loop for k below 60
                         collect (+ 14.5 (* 0.5 (sin (* k 0.4)))
                                    (* 0.1 (noise k))))
                   :marker :filled-circle :size 2.5))
    (draw-box d :x-ticks (time-ticks :scale :mjd) :y-label "magnitude"
                :x-label "2024 (UTC)")))
```

![](images/time-axis.png)
