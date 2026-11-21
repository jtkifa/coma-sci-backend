# Tutorial: a complete figure

This chapter builds a figure of the kind that goes into a paper: measurements
with error bars, a model through them with its uncertainty, a legend, and a
second panel of residuals below, sharing the time axis. Each step adds to the
previous one, and each block of code is complete, so it can be run as it is.

## The data

The measurements are the flux of a source that fades, with a flare. A small
function makes repeatable pseudo-random noise, so the figure is the same every
time:

```lisp
(defun noise (k)
  "A repeatable pseudo-random number between -0.5 and 0.5."
  (- (mod (* 43758.5453d0 (sin (* 12.9898d0 k))) 1) 0.5))

(defun model (time)
  "The fading flux."
  (+ 2 (* 6 (exp (- (/ time 6))))))

(defparameter *times*
  (loop for k from 0 below 40 collect (+ 0.5 (* 0.5 k))))

(defparameter *errors*
  (mapcar (lambda (time) (+ 0.3 (* 0.04 time))) *times*))

(defparameter *fluxes*
  (loop for time in *times*
        for error in *errors*
        for k from 0
        collect (+ (model time)
                   (* 2 error (noise k))
                   (if (<= 12 time 13.5) 2.5 0))))
```

## Points with error bars

```{.lisp image="tutorial-1.png"}
(with-device (d :pdf "light-curve.pdf" :size 640 :aspect 0.6)
  (set-window d 0 20.5 0 10)
  (with-color (d "#2a78d6")
    (plot-error-bars d *times* *fluxes* :yerr *errors*)
    (plot-points d *times* *fluxes* :marker :filled-circle :size 3))
  (draw-box d :x-label "time (days)" :y-label "flux (mJy)"))
```

![](images/tutorial-1.png)

`plot-error-bars` draws a bar of plus and minus each error at each point;
`:yerr` gives the vertical errors (one number for all, or a list), `:xerr` the
horizontal ones, and `:yerr-lower`, `:yerr-upper` different errors below and
above. `plot-points` draws a marker at each point: `:marker` names its shape
(`:filled-circle`, `:square`, `:triangle`, `:star` and more; see
[Markers](#markers)), and `:size` is about its radius in points.

## A model with its uncertainty

The model is a curve sampled finely. Its uncertainty is a band, filled between
two curves with `plot-fill-between` in a translucent color, drawn first so the
data lie on top:

```{.lisp image="tutorial-2.png"}
(defun samples (f x1 x2 &optional (n 200))
  "Values XS and YS: F at N + 1 points from X1 to X2."
  (let ((xs (loop for k from 0 to n collect (+ x1 (* k (/ (- x2 x1) n))))))
    (values xs (mapcar f xs))))

(with-device (d :pdf "light-curve.pdf" :size 640 :aspect 0.6)
  (set-window d 0 20.5 0 10)
  (multiple-value-bind (xs ys) (samples #'model 0 20.5)
    (with-color (d '(0.92 0.41 0.2 0.25))
      (plot-fill-between d xs (mapcar (lambda (y) (+ y 0.8)) ys)
                         :y2 (mapcar (lambda (y) (- y 0.8)) ys)))
    (with-style (d :color "#eb6834" :line-width 1.5)
      (plot-polyline d xs ys)))
  (with-color (d "#2a78d6")
    (plot-error-bars d *times* *fluxes* :yerr *errors*)
    (plot-points d *times* *fluxes* :marker :filled-circle :size 3))
  (annotate d 12.75 6.3 "flare" 16 8.2)
  (draw-box d :x-label "time (days)" :y-label "flux (mJy)")
  (draw-legend d '(("observed" :marker :filled-circle :color "#2a78d6")
                   ("model" :line t :color "#eb6834" :line-width 1.5)
                   ("model ±0.8" :fill (0.92 0.41 0.2 0.25)))))
```

![](images/tutorial-2.png)

Colors are given as names (`:red`, `:dark-gray`), hexadecimal strings
(`"#2a78d6"`), or lists of red, green, blue and an optional opacity, each from 0
to 1. `annotate` writes text with an arrow to a point. `draw-legend` lists
entries, each a label followed by what its sample shows: `:marker`, `:line`
(with `:line-width` and `:dash`), or `:fill`. It places itself where it covers
least of the plot unless given a `:position`.

## Two panels

A residuals panel needs its own rectangle on the page, a *viewport*.
`make-viewport` makes one, from its left, right, bottom and top edges as
fractions of the page (0 to 1). Every plotting function then takes the
viewport in place of the device and draws in it:

```{.lisp image="tutorial-3.png"}
(with-device (d :pdf "light-curve.pdf" :size 640 :aspect 0.75)
  (let ((top (make-viewport d 0.12 0.95 0.36 0.93 :window '(0 20.5 0 10)))
        (bottom (make-viewport d 0.12 0.95 0.1 0.36
                               :window '(0 20.5 -3.5 3.5)))
        (residuals (mapcar (lambda (time flux) (- flux (model time)))
                           *times* *fluxes*)))
    ;; the light curve
    (multiple-value-bind (xs ys) (samples #'model 0 20.5)
      (with-style (top :color "#eb6834" :line-width 1.5)
        (plot-polyline top xs ys)))
    (with-color (top "#2a78d6")
      (plot-error-bars top *times* *fluxes* :yerr *errors*)
      (plot-points top *times* *fluxes* :marker :filled-circle :size 3))
    (draw-box top :numbers '(:left) :y-label "flux (mJy)"
                  :omit-end-numbers '(:y-low))
    (draw-legend top '(("observed" :marker :filled-circle :color "#2a78d6")
                       ("model" :line t :color "#eb6834" :line-width 1.5)))
    ;; the residuals
    (with-style (bottom :color :gray :dash '(4 3))
      (plot-line bottom 0 0 20.5 0))
    (with-color (bottom "#2a78d6")
      (plot-error-bars bottom *times* residuals :yerr *errors*)
      (plot-points bottom *times* residuals :marker :filled-circle :size 3))
    (draw-box bottom :x-label "time (days)" :y-label "residual" :y-step 2)))
```

![](images/tutorial-3.png)

Each viewport has its own *window*, the data range it shows, given here with
`:window` as a list of x from, x to, y from, y to. The panels share their x
range, so only the bottom one numbers it: `draw-box` takes `:numbers`, the
sides that get numbers, and `:omit-end-numbers`, here leaving out the top
panel's lowest number where it would meet the bottom panel's highest.
`:y-step` sets the interval between numbered ticks.

For grids of equal panels, `make-panel-grid` makes all the viewports at once;
see [Panels](#panels-and-insets).

## Saving in other formats

The output type and file name are the first two arguments of `with-device`;
nothing else changes. A PNG for a web page or a presentation, sharp on
high-resolution screens:

```{.lisp .norun}
(with-device (d :png "light-curve.png" :size 640 :aspect 0.75 :resolution 2)
  ...)
```

With `:resolution 2`, the PNG has twice as many pixels across, and everything
in it, lines and text included, is drawn twice as large, so it shows as the
same plot as the PDF.

## What to read next

The [Concepts](#concepts) chapter explains what is behind these steps: the
coordinate systems, viewports and windows, and how styles and text work.
[Charts](#charts) shows the other kinds of plot.
