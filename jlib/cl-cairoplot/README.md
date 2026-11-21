# cl-cairoplot

A Common Lisp library for scientific plots, modeled on PGPLOT and drawn with
Cairo: line and scatter plots, error bars, histograms, bar, pie and stacked area
charts, box and violin plots, heatmaps, images and contours, vector fields,
polar, corner and 3D plots, and sky maps with FITS projections, coordinate
frames and WCS-placed images. The same code writes PDF, PostScript, SVG and PNG
files, draws into memory, or shows plots live in a window.

```lisp
(asdf:load-system "cl-cairoplot")
(use-package :cl-cairoplot)

(with-device (d :pdf "plot.pdf" :size 600 :aspect 0.7)
  (let ((xs (loop for i from 0 to 200 collect (/ i 20.0))))
    (set-window d 0 10 -1.2 1.2)
    (plot-polyline d xs (mapcar #'sin xs))
    (draw-box d :x-label "x" :y-label "sin x")))
```

## Documentation

The manual is in [docs/](docs/src/):

- [Getting started](docs/src/01-getting-started.md)
- [Tutorial: a complete figure](docs/src/02-tutorial.md)
- [Concepts](docs/src/03-concepts.md): devices, coordinates, viewports, style, text, axes
- [Charts](docs/src/04-charts.md): every kind of chart, with its code
- [Astronomy](docs/src/05-astronomy.md): projections, frames, WCS images
- [Plots in a window](docs/src/06-windows.md) (system `cl-cairoplot-gtk`)
- [Troubleshooting](docs/src/07-troubleshooting.md)
- [Function reference](docs/src/10-reference.md), generated from the docstrings

`docs/build.sh` builds it as HTML (`docs/html/`) and PDF (`docs/cl-cairoplot.pdf`)
with Pandoc, after running every code block in it, which also makes its
pictures.

## Examples

`examples/` has a larger example for each feature:

```lisp
(asdf:load-system "cl-cairoplot-examples")
(cl-cairoplot-examples:make-examples)    ; writes example-plots/*.pdf
```

## Systems

- `cl-cairoplot`: the library. Needs `cl-cairo2` (and the Cairo C library),
  `cffi`, `float-utils` and `cl-cairo-hershey-fonts`.
- `cl-cairoplot-gtk` (in `gtk/`): the `:gtk` output type, plots in a window;
  needs `cl-ez-gtk-server` and GTK-server for GTK 4.
- `cl-cairoplot-examples` (in `examples/`): the examples.
