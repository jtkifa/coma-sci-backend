# Getting started

cl-cairoplot is a Common Lisp library for scientific plots: line and scatter
plots, error bars, histograms, bar and pie charts, images and contours, vector
fields, polar and 3D plots, and sky maps with astronomical projections and
world coordinate systems. It is modeled on PGPLOT, the plotting library long
used in astronomy, and draws with Cairo, so the same code writes PDF,
PostScript, SVG and PNG files, draws into memory, or shows plots live in a
window.

This chapter covers loading the library, a first plot, and the kinds of output.

## Requirements

- A Common Lisp implementation with ASDF. The library is tested with SBCL on
  macOS and Linux.
- The Cairo C library, and the Lisp systems `cl-cairo2`, `cffi` and
  `float-utils`.
- `cl-cairo-hershey-fonts`, for the Hershey stroke fonts that are the default
  typeface.
- For plots in a window only: the system `cl-ez-gtk-server` and GTK-server
  built for GTK 4 (see [Plots in a window](#plots-in-a-window)).

## Loading the library

The system is `cl-cairoplot`:

```{.lisp .norun}
(asdf:load-system "cl-cairoplot")
```

The package is `cl-cairoplot`, with the nickname `cairoplot`. The examples in
this manual are written in a package that uses it:

```{.lisp .norun}
(defpackage #:my-plots
  (:use #:cl #:cl-cairoplot))

(in-package #:my-plots)
```

## A first plot

```{.lisp image="first-plot.png"}
(with-device (d :pdf "first-plot.pdf" :size 600 :aspect 0.7)
  (let ((xs (loop for i from 0 to 200 collect (/ i 20.0))))
    (set-window d 0 10 -1.2 1.2)
    (with-style (d :color "#2a78d6" :line-width 2)
      (plot-polyline d xs (mapcar #'sin xs)))
    (draw-box d :x-label "x" :y-label "sin x" :title "A first plot")))
```

![](images/first-plot.png)

`with-device` opens a *device*, here a PDF file 600 points wide and 0.7 times
as high, runs its body, and closes the device, which finishes the file. Inside:

- `set-window` sets the range of data shown: x from 0 to 10, y from -1.2
  to 1.2. These *plot coordinates* are the ones the plotting functions take.
- `with-style` sets the color and line width for the drawing in its body.
- `plot-polyline` joins the points `xs`, `ys` with lines.
- `draw-box` draws the frame, ticks, numbers, axis labels and title.

Every plotting function takes the device as its first argument. The plot is
drawn in a rectangle of the page that leaves room for the numbers and labels:
the device's *default viewport*. Plots with several panels place their own
viewports (see [Viewports](#viewports-and-windows)).

## Kinds of output

The first argument of `with-device` and `open-device` is the output type, the
second the destination:

| Output type | Destination | Notes |
|---|---|---|
| `:pdf` | a file name | vector graphics; sizes in points (1/72 inch) |
| `:ps` | a file name | PostScript |
| `:svg` | a file name | for web pages |
| `:png` | a file name | pixels; the size is in pixels |
| `:buffer` | a pixel buffer | draws into memory, see `make-octet-pixel-buffer` |
| `:gtk` | a window title | a live window, from the system `cl-cairoplot-gtk` |

`:size` is the width, in points or pixels (default 600), and `:aspect` the
height as a fraction of the width (default 1). `:background` is the color the
page starts with (default white; `nil` for transparent).

Instead of `with-device`, a device can be opened and closed separately, which
is convenient at the REPL:

```{.lisp .norun}
(defvar *d* (open-device :png "plot.png" :size 800 :aspect 0.75))
(set-window *d* 0 1 0 1)
(plot-points *d* '(0.2 0.5 0.8) '(0.3 0.6 0.4))
(draw-box *d*)
(close-device *d*)
```

## A plot in a window

With the system `cl-cairoplot-gtk` loaded, the output type `:gtk` opens a
window, and everything drawn shows up in it as it is drawn:

```{.lisp .norun}
(asdf:load-system "cl-cairoplot-gtk")

(defvar *w* (open-device :gtk "my plot" :size 700 :aspect 0.7))
(set-window *w* 0 10 -1 1)
(plot-polyline *w* '(0 2 4 6 8 10) '(0 0.8 -0.5 0.3 -0.9 0))
(draw-box *w*)
```

The chapter [Plots in a window](#plots-in-a-window) describes it further.

## Where to go next

- [Tutorial](#tutorial-a-complete-figure) builds a figure with error bars, a
  model, a legend and a residuals panel, step by step.
- [Concepts](#concepts) explains devices, the coordinate systems, viewports,
  styles and text.
- [Charts](#charts) shows each kind of chart with its code.
- [Astronomy](#astronomy) covers sky projections, coordinate frames and images
  with a world coordinate system (WCS).
- [Function reference](#function-reference) lists every function.
- The directory `examples/` of the source has a larger example for each
  feature: `(asdf:load-system "cl-cairoplot-examples")` then
  `(cl-cairoplot-examples:make-examples)` writes them to `example-plots/`.
