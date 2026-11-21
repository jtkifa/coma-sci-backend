# Astronomy

cl-cairoplot draws maps of the sky in the projections of the FITS world
coordinate system (WCS) standard, places images by their WCS, overlays
catalogs by right ascension and declination, and draws coordinate grids in the
equatorial, galactic and ecliptic frames. This chapter covers each.

## Sky projections

A sky plot is drawn in the plane of a *projection*: plot coordinates are the
plane's x and y in degrees, as FITS intermediate world coordinates, with x
increasing to the east. `make-projection` makes one from a code and a
reference point (longitude and latitude in degrees):

| Code | Projection | Use |
|---|---|---|
| `:tan` | gnomonic | an image from a camera; a field up to tens of degrees |
| `:sin` | orthographic | a hemisphere, as seen from far away; radio interferometry |
| `:arc` | zenithal equidistant | distances from the center are true |
| `:zea` | zenithal equal area | areas are true; the whole sky in a disk |
| `:stg` | stereographic | small shapes are true |
| `:ait` | Hammer-Aitoff | the whole sky in an ellipse, equal area |
| `:mol` | Mollweide | the whole sky in an ellipse, equal area |
| `:car` | plate carrée | longitude and latitude as x and y |

A FITS CTYPE such as `"RA---SIN"` or `"GLON-AIT"` is accepted as the code too.

`set-window-projection` gives a viewport a projection and a window around its
reference point, with equal scales and east to the left; without a size, the
window is the projection's whole boundary, and drawing is clipped to it.
`set-window-tan` does the same with a half-width and half-height in degrees.
After it, every plotting function takes `:coords :sky`, with positions as right
ascension and declination in degrees, and `draw-sky-grid` draws lines of
constant right ascension and declination with sexagesimal labels:

```{.lisp image="sky-field.png"}
(defun noise (k)
  (- (mod (* 43758.5453d0 (sin (* 12.9898d0 k))) 1) 0.5))

(with-device (d :pdf "field.pdf" :size 520 :aspect 0.85)
  (let ((vp (make-viewport d 0.15 0.92 0.12 0.92))
        (ras (loop for k below 60 collect (+ 150 (* 0.6 (noise k)))))
        (decs (loop for k below 60
                    collect (+ 2.2 (* 0.6 (noise (+ k 100)))))))
    (set-window-tan vp (make-projection :tan 150 2.2) 0.32)
    (with-color (vp "#2a78d6")
      (plot-points vp ras decs :coords :sky
                               :marker :filled-circle :size 2.5))
    (with-color (vp "#eb6834")
      (plot-circle vp 150.1 2.3 0.08 :coords :sky :round t))
    (draw-sky-grid vp :size 10)))
```

![](images/sky-field.png)

`sky-to-plot` and `plot-to-sky` convert between positions on the sky and the
plot plane, and `sky-to-plane` and `plane-to-sky` do the same for a
projection without a viewport.

For a small field drawn without a projection, `set-window-sky` sets a window of
right ascension and declination directly, with right ascension shortened by the
cosine of the declination, and `sexagesimal-ticks` labels an axis in hours or
degrees, minutes and seconds:

```{.lisp .norun}
(set-window-sky vp 150 2.2 0.2)
(draw-box vp :x-ticks (sexagesimal-ticks :unit :hours)
             :y-ticks (sexagesimal-ticks :unit :degrees :sign t))
```

## All-sky maps

The all-sky projections show the whole sky in an ellipse or a rectangle.
Centered on a latitude other than zero, they are tilted, so the center is in the
middle with its frame's north pole straight up. Without a size,
`set-window-projection` fits the whole sky, and `draw-sky-grid` labels the
lines around the boundary. Here, a few bright stars and the ecliptic:

```{.lisp image="allsky.png"}
(with-device (d :pdf "allsky.pdf" :size 600 :aspect 0.55)
  (let ((vp (make-viewport d 0.05 0.95 0.08 0.9)))
    (set-window-projection vp (make-projection :mol 180))
    ;; the ecliptic, as dots placed by their ecliptic longitude
    (let ((ras '()) (decs '()))
      (loop for lon from 0 below 360 by 3
            do (multiple-value-bind (ra dec)
                   (frame-to-equatorial :ecliptic lon 0)
                 (push ra ras)
                 (push dec decs)))
      (with-color (vp "#2a78d6")
        (plot-points vp ras decs :coords :sky
                                 :marker :filled-circle :size 1.2)))
    (with-color (vp "#eb6834")
      (plot-points vp '(101.29 95.99 213.92 279.23 88.79)
                   '(-16.72 -52.70 19.18 38.78 7.41)
                   :coords :sky :marker :filled-star :size 5))
    (draw-sky-grid vp :size 9)))
```

![](images/allsky.png)

## Coordinate frames

A projection is in one of three frames: `:equatorial` (right ascension and
declination, the default), `:galactic` or `:ecliptic`, given as the fourth
argument of `make-projection` or by a CTYPE such as `"GLON-AIT"`.
`:coords :sky` positions are always right ascension and declination, whatever
the frame. `draw-sky-grid` takes `:coordinates` to draw the grid of another
frame, so a second call overlays, say, an equatorial grid on a galactic map:

```{.lisp image="frames.png"}
(with-device (d :pdf "frames.pdf" :size 600 :aspect 0.55)
  (let ((vp (make-viewport d 0.05 0.95 0.08 0.9)))
    (set-window-projection vp (make-projection :ait 0 0 :galactic))
    (draw-sky-grid vp :size 9)
    (with-style (vp :line-width 0.8)
      (draw-sky-grid vp :coordinates :equatorial :labels nil :frame nil
                        :grid-color '(0.92 0.41 0.2 0.7)))))
```

![](images/frames.png)

`equatorial-to-frame` and `frame-to-equatorial` convert positions between the
frames, for example to place the Galactic center by its galactic coordinates:

```{.lisp .norun}
(multiple-value-bind (ra dec) (frame-to-equatorial :galactic 0 0)
  (plot-point vp ra dec :coords :sky :marker :plus))
```

## Images with a WCS

A *WCS* maps the pixels of an image to the sky. `make-wcs` makes one from
FITS-style values, and `wcs-from-header` from a list of FITS keywords:

```{.lisp .norun}
(make-wcs :crval '(150 2.2) :crpix '(80.5 60.5)
          :cdelt '(-0.001 0.001) :crota 25)

(wcs-from-header '(:ctype1 "RA---TAN" :crval1 150 :crval2 2.2
                   :crpix1 80.5 :crpix2 60.5
                   :cd1_1 -0.001 :cd1_2 0 :cd2_1 0 :cd2_2 0.001))
```

`plot-image` with `:wcs` places an image on the viewport's projection. If the
WCS is linear in the same projection, as most optical images in TAN are, the
image is drawn exactly by an affine transform, rotated if need be; otherwise
it is resampled onto the plot (`reproject-image`). `plot-contours` and
`plot-filled-contours` take `:wcs` too, mapping each point of each contour
through it:

```{.lisp image="wcs-image.png"}
(with-device (d :pdf "wcs.pdf" :size 520 :aspect 0.85)
  (let* ((wcs (make-wcs :crval '(150 2.2) :crpix '(80.5 60.5)
                        :cdelt '(-0.002 0.002) :crota 25))
         (image (make-array '(120 160)))
         (vp (make-viewport d 0.15 0.92 0.12 0.92)))
    (dotimes (j 120)
      (dotimes (i 160)
        (setf (aref image j i)
              (+ 10
                 (* 600 (exp (/ (+ (expt (- i 70) 2) (expt (- j 55) 2))
                                -30)))
                 (* 200 (exp (/ (+ (expt (- i 120) 2) (expt (- j 85) 2))
                                -12)))
                 (* 3 (noise (+ i (* 1000 j))))))))
    (set-window-tan vp (wcs-projection wcs) 0.2)
    (plot-image vp image :wcs wcs :colormap :gray :scale :asinh
                         :vmin 0 :vmax 400 :reverse t)
    (with-color (vp "#2a78d6")
      (plot-contours vp image :wcs wcs :levels '(50 150 400)))
    (draw-sky-grid vp :size 10)))
```

![](images/wcs-image.png)

The image is drawn rotated by the WCS's 25°, inside a grid that stays aligned
with right ascension and declination.

### Your own WCS classes

Any object can serve as a WCS if it has methods on the generic functions
`wcs-pixel-to-sky` and `wcs-sky-to-pixel` (pixels counted from 0), and
optionally `wcs-plane-transform` for images that are linear in a projection.
A WCS with distortion, or one from another library, then works in
`plot-image` and the contour functions. The system `cairoplot-wcs` in
`astro/cairoplot-wcs/` does this for the WCS structures that the `astro/wcs`
library reads from FITS files.

## Symbols

The markers include the astronomical symbols, `*astronomical-symbols*`: the
Sun, the Moon, the planets and others, drawn from the Hershey astrology font:

```{.lisp .norun}
(plot-point vp ra dec :coords :sky :marker :jupiter :size 8)
```

Text can contain them too, as characters (☉ ☽ ☿ ♀ ♁ ♂ ♃ ♄ and the zodiac
signs), drawn from the same font.
