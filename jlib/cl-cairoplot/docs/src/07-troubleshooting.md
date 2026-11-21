# Troubleshooting

## Loading

**The Cairo library is not found.** `cl-cairo2` loads the Cairo C library
through CFFI. If it is installed but not found, add its directory to
`cffi:*foreign-library-directories*` before loading, for example
`(push "/opt/local/lib/" cffi:*foreign-library-directories*)` with MacPorts or
`"/opt/homebrew/lib/"` with Homebrew.

**`Unknown output type :gtk`.** The `:gtk` output type is in the separate
system `cl-cairoplot-gtk`, which must be loaded first:
`(asdf:load-system "cl-cairoplot-gtk")`.

## Drawing

**Nothing appears, or everything is in the wrong place.** Plotting functions
draw in plot coordinates, the window of the viewport they are given (or of the
device's selected viewport). Check that a window has been set with
`set-window` or `:window`, and that the data lie inside it. Shapes outside the
window are clipped away.

**A curve drawn in a panel appears in another panel.** A plotting function
given the device draws in the device's selected viewport, by default the
default viewport, not in a viewport made with `make-viewport`. Give the
viewport itself to the plotting functions, `(plot-polyline vp xs ys)`, or
select it with `(setf (device-viewport d) vp)` or `with-viewport`.

**Circles look like ellipses.** The window's x and y scales differ. Use
`set-window-equal-scales`, or `:round t` for `plot-circle`, which keeps the
circle round on the page whatever the scales.

**A type error mentions `double-float`.** Arrays made with `:element-type
'double-float` accept only double floats; a literal such as `0.5` or a
computation on single floats gives a single float. Write `0.5d0`, convert with
`(float x 1d0)`, or make the array without an element type.

**Numbers on a log axis are wrong.** On a log axis, the window and the data are
in powers of ten: a window from 0 to 3 spans 1 to 1000, and a value of 100 is
plotted at 2. Pass `(log x 10)`.

## Text

**A character is missing.** Each Hershey font has only some characters; others
come from `*fallback-fonts*` (Hershey Greek, Hershey astrology, then the system
font). A character no font has is left out. Use a system font with `:font
:sans`, or add a font that has it to `*fallback-fonts*`.

**Text is too small or too large.** Text sizes are in points (or pixels for
PNG). The default is a fortieth of the page's smaller side, so it changes with
`:size`; give `:size` to `plot-text`, `draw-box` and the other functions that
write text to fix it.

## Output

**A PNG looks blurred on a high-resolution screen.** Open it with
`:resolution 2`: the picture has twice the pixels, with everything drawn at
twice the size.

**The PDF changes every time, although the plot does not.** PDF files record
when they were made. `(set-document-metadata d :create-date "")` leaves the
date out, so the same plot gives the same file.

**A window does not appear.** Check that GTK-server runs: `gtk-server-gtk4 -v`
in a terminal. If it is installed elsewhere than the `PATH`, set
`ezgtk:*gtk-server-program*` to its full path before opening the window.
