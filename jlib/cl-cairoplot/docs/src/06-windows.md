# Plots in a window

The system `cl-cairoplot-gtk` adds the output type `:gtk`: a device that draws
into a window on the screen, showing each plotting call as it happens. It is a
separate system so that GTK is loaded only by programs that use it; the rest
of cl-cairoplot does not depend on it.

## Requirements

- The system `cl-ez-gtk-server`, a Common Lisp toolkit for GTK 4 user
  interfaces, with its canvas (`cl-ez-gtk-server/canvas`).
- GTK-server built for GTK 4 (`gtk-server-gtk4` on the `PATH`), as described
  in the manual of cl-ez-gtk-server.

## Opening a window

```{.lisp .norun}
(asdf:load-system "cl-cairoplot-gtk")
```

`open-device` with the output type `:gtk` and a window title opens a window
`:size` by `:size` times `:aspect` points large, starting GTK-server if it is
not running. Everything else works as for other devices:

```{.lisp image="window.png"}
(defvar *w* (open-device :gtk "my graph" :size 640 :aspect 0.62))

(let ((xs (loop for i from 0 to 200 collect (/ i 20.0))))
  (set-window *w* 0 10 -1.2 1.2)
  (with-style (*w* :color "#2a78d6" :line-width 2)
    (plot-polyline *w* xs (mapcar #'sin xs)))
  (with-style (*w* :color "#eb6834" :line-width 2 :dash '(6 4))
    (plot-polyline *w* xs (mapcar #'cos xs)))
  (draw-box *w* :x-label "x" :y-label "y" :title "sin and cos")
  (draw-legend *w* '(("sin x" :line t :color "#2a78d6" :line-width 2)
                     ("cos x" :line t :color "#eb6834" :line-width 2
                              :dash (6 4)))))

(close-device *w*)
```

![The window's contents](images/window.png)

Each plotting call appears in the window as soon as it is made, from the REPL
or from a running program, so a plot can be built up interactively, or show
data as they arrive:

```{.lisp .norun}
(plot-points *w* '(2 4 6 8) '(0.5 -0.5 0.5 -0.5)
             :marker :filled-circle :size 5)
```

## Reading the cursor

`read-cursor` waits for a click or a key press in the window, as PGPLOT's
`PGCURS` does, and returns the position in plot coordinates, the key, and the
viewport under the pointer:

```{.lisp .norun}
(multiple-value-bind (x y key viewport) (read-cursor *w*)
  (format t "~a at ~,3f, ~,3f~%" key x y))
```

The key is `:left`, `:middle` or `:right` for a mouse button, or the key's
character (`#\q`) or a keyword (`:escape`, `:return`, `:left` for the arrow
key...). The coordinates are those of the viewport under the pointer, or of
`:viewport` if given, whichever panel the pointer is over.

`:mode` draws a *rubber band* while waiting, as PGPLOT's `PGBAND` does: a
`:line` from `:anchor` to the pointer, a `:rectangle` with a corner at
`:anchor`, or `:horizontal`, `:vertical` or `:crosshair` lines through the
pointer. The band is drawn over a copy of the picture, so the plot is left as
it was. Choosing a region of a plot by its two corners, for example:

```{.lisp .norun}
(multiple-value-bind (x1 y1) (read-cursor *w*)
  (multiple-value-bind (x2 y2)
      (read-cursor *w* :mode :rectangle :anchor (list x1 y1))
    (set-window *w* (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2))))
```

`:timeout` gives up after that many seconds, returning `nil`, as does closing
the window. `(cl-cairoplot-gtk:cursor-demo)` marks points by clicking, with a
rubber band from the last one, until `q` is pressed.

`locate-point` converts a point on the page to the plot coordinates of the
viewport there, for programs that track the pointer themselves.

## Erasing

`erase-device` clears the whole page to its background, as PGPLOT's `PGERAS`
does, and `erase-viewport` clears one viewport's rectangle, so a live plot can
be drawn again with new data. Viewports keep their windows. Clearing a viewport
also clears the ticks inside its frame, so its box is drawn again after the new
contents:

```{.lisp .norun}
(erase-device *w*)                       ; a fresh page

(erase-viewport vp)                      ; one panel only
(plot-polyline vp xs new-ys)
(draw-box vp)
```

Both work on every kind of device; on a PDF they paint over the page, while
`next-page` starts a new one.

## Sharpness on high-resolution screens

On a high-resolution screen, such as a Mac's Retina display, each point of the
window is two pixels across. The window's picture then has two pixels per
point too, and everything is drawn at twice the size, so the plot is as sharp
as the screen. The window finds the screen's scale factor itself; `:resolution`
sets it instead, for example `:resolution 1` for a picture of one pixel per
point:

```{.lisp .norun}
(open-device :gtk "my graph" :size 640 :aspect 0.62 :resolution 1)
```

Sizes, text and line widths are in points whatever the resolution, so the plot
looks the same, only sharper or softer.

## Closing

- `close-device` finishes the device: the window keeps showing the final
  picture, and closing the window then removes it. The end of `with-device`
  does the same, so a plot drawn in `with-device` stays on the screen.
- `(cl-cairoplot-gtk:close-window device)` closes the device, if it is still
  open, and removes the window at once.
- Closing the window with the mouse while the device is still open only hides
  it, so a program drawing into it can go on safely;
  `(cl-cairoplot-gtk:show-window device)` shows it again.

## How it works

The window holds a canvas of cl-ez-gtk-server: a picture whose pixels are
memory shared between Lisp and GTK-server, shown shrunk to the window's size in
points. The device draws straight into those pixels, as a `:buffer` device
draws into a pixel buffer, at the resolution of the window. A task in GTK's
event loop checks every `cl-cairoplot-gtk:*refresh-interval*` seconds (0.05 by
default) whether plotting calls have changed the picture, by the count
`device-changes` that every plotting call increments, and shows it if so.

`(cl-cairoplot-gtk:demo)` opens a window and draws four panels into it one
after another, a curve growing point by point in the first.

## Other output types

The `:gtk` output type is an instance of a general mechanism: `open-device`
hands an output type it does not draw itself to the generic function
`output-type-buffer`, whose methods return the pixel buffer to draw in and the
class of the device; the buffer's size, over the size asked for, sets the
device's resolution. A system providing another kind of output adds a method
on it, and a method on `device-opened` and `close-device` for what happens when
the device opens and closes. `*output-type-systems*` lists the systems that
provide output types, so that using one before loading its system gives an
error naming it.
