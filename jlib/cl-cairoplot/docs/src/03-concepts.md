# Concepts

This chapter explains how a plot is put together: devices, the coordinate
systems, viewports and their windows, and how style, text and axes work. The
later chapters build on it.

## Devices

A *device* is something to draw on: a PDF, PostScript, SVG or PNG file, a
pixel buffer in memory, or a window. `open-device` opens one and
`close-device` finishes and closes it; `with-device` does both around its body,
closing the device even if the body signals an error:

```{.lisp .norun}
(with-device (d :pdf "plot.pdf" :size 600 :aspect 0.75)
  ...)
```

`:size` is the width of the page in points (1/72 inch), or in pixels for PNG;
`:aspect` is the height as a fraction of the width. Text sizes and line widths
are in the same units, so a 10-point label is 10 points high in the PDF.
`:background` sets the color of the page (`nil` leaves a PNG transparent), and
`:font` the font text starts in.

For PNG, `:resolution` gives the pixels per unit: with `:size 600
:resolution 2` the image is 1200 pixels wide, and everything is drawn twice as
large, so it is the same plot with sharper detail. Windows (`:gtk`) take the
resolution of their screen.

A device can also draw into memory: `make-octet-pixel-buffer` makes a buffer
of pixels in a Lisp array, and `make-foreign-pixel-buffer` one in foreign
memory (or on memory given by a pointer). Opening a device of type `:buffer` on
a buffer draws into its pixels, for programs that pass images on, such as a
camera display or a web server. Its size is the buffer's, divided by
`:resolution` if given: a buffer 1200 pixels wide opened with `:resolution 2`
is a page 600 units wide.

## Coordinate systems

Every function that takes a position takes it in one of these coordinate
systems, chosen with the keyword argument `:coords`:

| `:coords` | Units | Origin and extent |
|---|---|---|
| `:plot` (the default) | the data | the *window* of the current viewport |
| `:device` | fractions of the page | 0 to 1 across and 0 to 1 up, from the lower left |
| `:world` | fractions of the page width | 0 to 1 across, 0 to the aspect up: equal units in x and y |
| `:canvas` | points or pixels | from the upper left, y downward, as Cairo |
| `:sky` | degrees of right ascension and declination | through the viewport's sky projection (see [Astronomy](#astronomy)) |
| `:polar` | degrees of angle, and radius | through the viewport's polar mapping (see [Polar plots](#polar-plots)) |

Most drawing is in `:plot` coordinates. `:device` is convenient for things
placed on the page, such as a title above several panels; `:world` for shapes
that must stay round however the page is shaped:

```{.lisp image="coordinates.png"}
(with-device (d :pdf "coordinates.pdf" :size 560 :aspect 0.5)
  (set-window d 0 100 0 50)
  (plot-polyline d '(0 25 50 75 100) '(10 40 20 45 30))
  (draw-box d :title "drawn in :plot coordinates")
  (plot-text d 0.5 0.03 "this line is placed in :device coordinates"
             :coords :device :halign :center)
  (with-color (d '(0.92 0.41 0.2 0.6))
    (plot-circle d 0.85 0.3 0.04 :coords :world :fill t)))
```

![](images/coordinates.png)

`convert-coords` converts a point from one system to another, for example to
find where a data point lies on the page.

## Viewports and windows

A *viewport* is a rectangle of the page with its own *window*: the range of
plot coordinates mapped onto the rectangle. `make-viewport` takes the
rectangle's left, right, bottom and top edges in `:device` coordinates (or
`:world`, with `:coords :world`), and optionally `:window`:

```{.lisp .norun}
(make-viewport d 0.1 0.9 0.15 0.85 :window '(0 10 -1 1))
```

`set-window` changes the window later:

```{.lisp .norun}
(set-window vp 0 10 -1 1)      ; x from 0 to 10, y from -1 to 1
```

A window may run backward, x from high to low (right ascension, which
increases to the left) or y from high to low (magnitudes, which are brighter
when smaller). `set-window-equal-scales` sets a window and shrinks the
viewport about its center so one unit is the same length in x and in y, so
circles are round; `viewport-window` reads the window back.

Every device starts with a viewport, the *default viewport*, that leaves room
at the sides for numbers, labels and a title. A plot with one panel needs no
other: `(set-window d ...)` sets its window, and everything given the device
draws in it.

### Devices and viewports as targets

Plotting functions take either a device or a viewport as their first argument.
Given a viewport, they draw in it; given a device, they draw in the device's
*selected* viewport, which is the default viewport unless another has been
selected. Each call takes care of drawing in its viewport's rectangle and
clipping to it, so viewports need no other setup:

```{.lisp image="viewports.png"}
(with-device (d :pdf "viewports.pdf" :size 640 :aspect 0.45)
  (let ((left (make-viewport d 0.08 0.46 0.15 0.88
                             :window '(0 10 -1.5 1.5)))
        (right (make-viewport d 0.58 0.96 0.15 0.88)))
    (plot-polyline left '(0 2 4 6 8 10) '(0 1.9 -1 0.5 -0.2 0))
    (draw-box left :title "a curve, clipped at the window")
    (set-window-equal-scales right -2 2 -1 1)
    (plot-circle right 0 0 0.8)
    (draw-box right :title "equal scales: a round circle")))
```

![](images/viewports.png)

`(setf (device-viewport d) vp)` selects a viewport, so that later calls given
the device draw in it. `with-viewport` selects one for its body only, and also
sets the drawing up for it once, which is slightly faster for many small
calls:

```{.lisp .norun}
(with-viewport (vp)
  (dotimes (k 1000)
    (plot-point d (random 1.0) (random 1.0))))
```

A viewport may be rotated (`:angle` in degrees), and everything in it, frame
and text included, turns with it. Its `:clipping-region` is by default its
rectangle; `nil` turns clipping off, and any clipping region (see
[Clipping](#clipping)) may be given.

### Panels and insets

`make-panel-grid` makes a grid of viewports, row 0 at the top, filling the
page between margins with gaps `:hspace` and `:wspace` between them. With no
gaps the panels touch, and `panel-number-sides` and `panel-omitted-ends` give
`draw-box` the sides to number and the numbers to leave out so they do not
collide. `draw-panel-label` writes a label such as "(a)" in a corner:

```{.lisp image="panels.png"}
(with-device (d :pdf "panels.pdf" :size 640 :aspect 0.55)
  (let ((grid (make-panel-grid d 2 3 :left 0.09 :right 0.97
                                     :bottom 0.12 :top 0.95
                                     :hspace 0 :wspace 0
                                     :window '(0 10 -1.3 1.3))))
    (dotimes (row 2)
      (dotimes (col 3)
        (let ((vp (aref grid row col))
              (k (+ (* 3 row) col 1)))
          (plot-polyline vp (loop for i to 100 collect (/ i 10.0))
                         (loop for i to 100
                               collect (sin (* k 0.3 (/ i 10.0)))))
          (draw-box vp :numbers (panel-number-sides grid row col)
                       :omit-end-numbers (panel-omitted-ends grid row col)
                       :x-label (and (= row 1) "x")
                       :y-label (and (= col 0) "y"))
          (draw-panel-label vp (format nil "(~(~a~))"
                                       (code-char (+ 96 k)))))))))
```

![](images/panels.png)

`add-inset` makes a small viewport inside another, placed by fractions of the
parent (`:coords :fraction`, the default) or in its plot coordinates
(`:coords :plot`). It clears its background, and with `:zoom` marks the region
it shows in the parent with a rectangle and connecting lines
(`draw-zoom-indicator`):

```{.lisp image="inset.png"}
(with-device (d :pdf "inset.pdf" :size 560 :aspect 0.55)
  (let ((xs (loop for i to 1000 collect (/ i 100.0))))
    (set-window d 0 10 -1.2 1.6)
    (flet ((f (x) (+ (sin x) (* 0.08 (sin (* 25 x))))))
      (plot-polyline d xs (mapcar #'f xs))
      (draw-box d :title "a zoom inset")
      (let ((inset (add-inset d 0.34 0.66 0.64 0.97
                              :window '(1 2.2 0.75 1.15) :zoom t)))
        (plot-polyline inset xs (mapcar #'f xs))
        (draw-box inset :size 8)))))
```

![](images/inset.png)

## Style

Lines, fills and text are drawn in the current *style*: color, line width,
dash pattern, and line cap and join. `with-style` sets any of them for its
body and restores them afterward; `with-color`, `with-line-width` and
`with-dash` set one each. All take a device or a viewport:

```{.lisp .norun}
(with-style (d :color :red :line-width 2 :dash '(6 3))
  (plot-polyline d xs ys))
```

A dash pattern is a list of lengths, on and off, in points. `set-color`,
`set-line-width` and `set-dash` change the style until changed again.

### Colors

A color is given in any of these forms:

- a name: `:black`, `:white`, `:red`, `:green`, `:blue`, `:cyan`, `:magenta`,
  `:yellow`, `:orange`, `:purple`, `:pink`, `:brown`, `:gray`, `:dark-gray`,
  `:light-gray`, `:very-light-gray`, `:navy`, `:teal`, `:olive`, `:maroon` and
  a few more; `define-named-color` adds others;
- a hexadecimal string, `"#2a78d6"`, or with opacity, `"#2a78d680"`;
- a list of red, green, blue and an optional opacity, each 0 to 1:
  `'(0.2 0.5 0.8)` or `'(0.2 0.5 0.8 0.3)`.

For several series, `*category-colors*` lists eight colors that remain
distinguishable to readers with color-vision deficiencies, and
`(category-colors n)` gives the first N. Charts such as bar charts and stacked
areas use them in order by default.

Overlapping translucent shapes darken where they overlap. To draw a group of
shapes with one uniform opacity instead, draw them inside
`with-opacity-group`.

### Colormaps

A *colormap* maps a value to a color, for images, contours, heatmaps and points
colored by value. The named colormaps are `:viridis`, `:plasma`, `:inferno`,
`:magma` (perceptually uniform), `:gray`, `:heat` (as PGPLOT's), `:coolwarm`
(diverging, for values above and below a center) and `:rainbow`.
`reverse-colormap` runs one backward, and `gradient-colormap`,
`diverging-colormap` and `table-colormap` make new ones.

## Text

`plot-text` writes text at a point:

```{.lisp .norun}
(plot-text d 5 0.5 "label"
           :halign :center :valign :bottom :angle 30 :size 12)
```

`:halign` is `:left`, `:center` or `:right` (or a fraction of the width left of
the point), `:valign` is `:baseline`, `:bottom`, `:center` or `:top`, `:angle`
turns the text counterclockwise in degrees, and `:size` is its height in
points (by default a fortieth of the page's smaller side, see
`default-text-size`).

### Fonts

The default font is a Hershey font: Hershey fonts are stroke fonts, drawn as
lines, as in the original PGPLOT. The named fonts are the Hershey fonts
`:normal` (the default, a simple sans-serif), `:roman`, `:italic`, `:script`
and `:astrology`, and the system fonts `:sans` (Helvetica), `:serif` (Times)
and `:mono` (Courier). `with-font` sets a font for its body, and
`:font` sets one for one call:

```{.lisp .norun}
(with-font (d :sans :size 14)
  (plot-text d 0.5 0.5 "Helvetica"))
(plot-text d 0.5 0.4 "Hershey roman" :font :roman)
```

`make-system-font` and `make-hershey-font` make other fonts. A character a
font lacks is drawn in the first of `*fallback-fonts*` that has it: Greek
letters and symbols from Hershey's Greek font, planet and zodiac symbols from
its astrology font, and anything else, such as Å, from the system font.

### Superscripts and subscripts

Text is a string or a list of *runs*: strings, or lists of a string and
keywords. `:scale` multiplies the size, `:rise` raises the run by that many
times the font size (negative lowers it), and `:font` changes the font:

```{.lisp image="text.png"}
(with-device (d :pdf "text.pdf" :size 560 :aspect 0.3)
  (set-window d 0 1 0 1)
  (plot-text d 0.03 0.75 '("E = mc" ("2" :scale 0.6 :rise 0.5)) :size 20)
  (plot-text d 0.4 0.75 '("H" ("2" :scale 0.6 :rise -0.25) "O") :size 20)
  (plot-text d 0.7 0.75 '("10" ("-3" :scale 0.6 :rise 0.5)
                          " W m" ("-2" :scale 0.6 :rise 0.5))
             :size 20)
  (plot-text d 0.03 0.3 "α δ λ Ω ± × ° ☉ ♃ ♄" :size 20)
  (with-halo (:color :white)
    (with-color (d "#2a78d6")
      (plot-rectangle d 0.62 0.1 0.98 0.5 :fill t))
    (plot-text d 0.8 0.3 "with a halo"
               :halign :center :valign :center :size 18)))
```

![](images/text.png)

A *halo* outlines text, markers and contour lines in a contrasting color, so
they read over an image. `with-halo` turns halos on for its body; text and
markers also take `:halo`. A halo spec is a named halo (`:default`, `:crisp`,
`:dark`, `:dark-crisp`, `:glow`), a color, or keywords for `make-halo`
(`:color`, `:width`, `:opacity`, `:soft`).

## Axes

`draw-box` draws the frame, ticks, numbers and labels around a viewport:

```{.lisp .norun}
(draw-box vp :x-label "time (s)" :y-label "flux" :title "Title"
             :ticks '(:bottom :top :left :right) :numbers '(:bottom :left)
             :tick-direction :in :minor t :grid nil)
```

- `:ticks` and `:numbers` list the sides that get ticks and numbers;
  `:tick-direction` is `:in`, `:out` or `:both`; `:minor` turns minor ticks on
  or off; `:grid` draws grid lines (`t`, or `:minor` for minor ones too).
- `:x-log` and `:y-log` make logarithmic axes; the window is then in powers of
  ten (a window from 0 to 3 spans 1 to 1000), and data must be given as
  logarithms.
- `:x-step`, `:y-step` set the interval between numbered ticks, and
  `:x-subdivisions`, `:y-subdivisions` the number of minor intervals.
- `:x-format`, `:y-format` are functions from a value to its text.
- `:x-numbers` can be an angle, to slant long labels; `:y-numbers :vertical`
  writes the y numbers along the axis.
- `:x-ticks`, `:y-ticks` replace the ticks with a tick generator:
  `category-ticks` for named categories, `sexagesimal-ticks` for right
  ascension and declination, `time-ticks` for dates and times.
- `:zero-axes` draws the lines x = 0 and y = 0.

`draw-secondary-axis` puts a second scale on the top or right side, given a
function from the window's coordinate to the other unit:

```{.lisp image="axes.png"}
(with-device (d :pdf "axes.pdf" :size 600 :aspect 0.45)
  (let ((left (make-viewport d 0.09 0.45 0.17 0.85 :window '(0 3 -2 2)))
        (right (make-viewport d 0.59 0.95 0.17 0.85
                              :window '(3000 9000 0 1))))
    ;; a log-log axis: a power law is a straight line
    (plot-line left 0 1.5 3 -1.8)
    (draw-box left :x-log t :y-log t :grid :minor :x-label "frequency (Hz)"
                   :y-label "power" :title "log axes")
    ;; wavelength below, frequency above
    (plot-polyline right (loop for w from 3000 to 9000 by 100 collect w)
                   (loop for w from 3000 to 9000 by 100
                         collect (exp (- (expt (/ (- w 6000) 1200) 2)))))
    (draw-box right :ticks '(:bottom :left :right)
                    :x-label "wavelength (Å)")
    (draw-secondary-axis right :top (lambda (w) (/ 299792.458 w))
                         :label "frequency (THz)")))
```

![](images/axes.png)

[Date and time axes](#date-and-time-axes) and the
[Astronomy](#astronomy) chapter show the other tick generators.

## Markers

`plot-points` and `plot-point` draw markers. Their names are listed by
`(marker-names)`; PGPLOT's symbol numbers work too (`*pgplot-markers*`), and
`define-marker` defines new ones. Planet, Sun and Moon symbols are in
`*astronomical-symbols*`:

```{.lisp image="markers.png"}
(with-device (d :pdf "markers.pdf" :size 600 :aspect 0.5)
  (set-window d 0 5 0 6.5)
  (loop for name in (append (marker-names)
                            '(:sun :moon :jupiter :saturn :mars :earth))
        for k from 0
        for x = (+ 0.15 (mod k 5))
        for y = (- 6 (* 0.9 (floor k 5)))
        do (plot-point d x y :marker name :size 6)
           (plot-text d (+ x 0.15) y (string-downcase name)
                      :valign :center :size 8))
  (draw-box d :ticks '() :numbers '() :frame nil))
```

![](images/markers.png)

`:size` is about a marker's radius in points. `:colors` and `:sizes`, lists,
give each point its own color and size; `values-to-colors` and
`values-to-sizes` make them from data (see
[Points colored by value](#points-colored-by-value)).

## Clipping

Drawing in a viewport is clipped to its rectangle. `with-clipping` clips to a
*region* as well, for its body: `rectangle-region`, `circle-region`,
`ellipse-region` and `polygon-region` (in any coordinates), combined with
`region-union`, `region-intersection` and `region-outside`:

```{.lisp image="clipping.png"}
(with-device (d :pdf "clipping.pdf" :size 400 :aspect 0.6)
  (set-window-equal-scales d -2 2 -1.2 1.2)
  (with-clipping (d (region-intersection
                     (circle-region 0 0 1.1)
                     (region-outside (circle-region 0 0 0.5))))
    (loop for y from -1.2 to 1.2 by 0.1
          do (with-color (d (list 0.2 (/ (+ y 1.2) 2.4) 0.8))
               (plot-rectangle d -2 y 2 (+ y 0.1) :fill t))))
  (draw-box d :title "an annulus as a clipping region"))
```

![](images/clipping.png)
