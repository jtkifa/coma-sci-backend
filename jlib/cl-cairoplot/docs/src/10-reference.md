# Function reference

This chapter is generated from the docstrings, grouped by the source file that defines
each function, macro, variable and class.  Names in capitals in the descriptions are
arguments, as is usual in Lisp.  All are in the package `cl-cairoplot` (nickname
`cairoplot`), except the windows section, which is `cl-cairoplot-gtk`.

## Devices and coordinates

#### `*default-viewport*` *variable*

MAKE-VIEWPORT arguments for the viewport a device starts with
selected: room at the sides for DRAW-BOX's numbers, labels and title.

#### `*output-type-systems*` *variable*

Output types other systems provide, with the system to load for each.

#### `(canvas-to-device device cx cy)`

The :DEVICE point of the page canvas point (CX,CY).

#### `(close-device device)`

Finish DEVICE's output and close it.

#### `(convert-coords device x y from to)`

Convert the point (X,Y) from coordinates FROM to coordinates TO.

#### `(coords-transform device coords)`

The affine transform (see viewport.lisp) from COORDS to :CANVAS.

#### `(device-aspect plot-device)`

Height / width.

#### `(device-changes plot-device)`

Counts plotting calls, so a device shown on
screen knows when to show it again. `setf` changes it.

#### `(device-entered-viewport plot-device)`

The VIEWPORT cairo is set up for, or NIL for
the page (see viewport.lisp). `setf` changes it.

#### `(device-opened device)`

Called on a device OPEN-DEVICE has just opened; returns it.

#### `(device-to-canvas device x y)`

The page canvas point of the :DEVICE point (X,Y).

#### `(device-to-world device x y)`

The :WORLD point of the :DEVICE point (X,Y).

#### `(device-viewport plot-device)`

The selected VIEWPORT, for :PLOT coordinates. `setf` changes it.

#### `(device-viewports plot-device)`

The viewports made on the device, so ERASE-DEVICE
can forget what was drawn in them. `setf` changes it.

#### `(erase-device device &optional color)`

Clear DEVICE's whole page to COLOR (default its background; NIL makes
a PNG or buffer transparent), as PGPLOT's PGERAS.  Viewports keep
their windows, and forget what was drawn in them, so legends are
placed afresh.  A window shows the cleared page at once.  (On PDF and
PostScript this paints over the page; NEXT-PAGE starts a new one.)

#### `(from-canvas device coords cx cy)`

Convert the :CANVAS point (CX,CY) to COORDS.

#### `(open-device output-type destination &key size aspect background font resolution)`

Open a plot device; OUTPUT-TYPE and DESTINATION are as for OPEN-CANVAS,
or another output type a loaded system provides (see
OUTPUT-TYPE-BUFFER), such as :GTK with a window title.  SIZE is the
width in points (pixels for :PNG and on screen) and ASPECT is
height/width.  RESOLUTION is pixels per unit of SIZE for a :PNG
image (default 1, see OPEN-CANVAS) or a :BUFFER device, whose size
is its buffer's divided by it; for other output types it defaults to
what suits them, e.g. the screen's scale factor for :GTK.  The device
starts with a viewport made from *DEFAULT-VIEWPORT* selected.

#### `(output-type-buffer output-type destination width height &key resolution)`

For an OUTPUT-TYPE OPEN-CANVAS does not draw itself,
such as :GTK (a window, from system cl-cairoplot-gtk): values the
PIXEL-BUFFER that the device draws in, the device's class (a subclass
of PLOT-DEVICE) and more initargs for it.  The device is WIDTH x
HEIGHT units; the buffer has RESOLUTION pixels per unit, or with
RESOLUTION NIL as many as the output type chooses (e.g. 2 on a
high-resolution screen), the same in x and y.

#### `plot-device` *class*

A canvas with an aspect ratio, coordinate systems and
viewports to plot in: what OPEN-DEVICE returns.

#### `(read-cursor device &key viewport mode anchor timeout)`

Wait for a click or a key press in DEVICE's window and
return values X, Y (in the :PLOT coordinates of VIEWPORT, by default
the viewport under the pointer), the KEY (:LEFT, :MIDDLE or :RIGHT
for a mouse button, else the key's character or a keyword such as
:ESCAPE) and the viewport, as PGPLOT's PGCURS and PGBAND.  MODE draws
a rubber band while waiting, from ANCHOR, a list (X Y) of plot
coordinates: :LINE from it, :RECTANGLE with a corner at it, and
without an anchor :HORIZONTAL, :VERTICAL or :CROSSHAIR lines through
the pointer; :NONE (the default) draws nothing.  After TIMEOUT
seconds, or if the window is closed, returns NIL.  Only devices on
the screen have a cursor, such as :GTK windows.

#### `(to-canvas device coords x y)`

Convert the point (X,Y) in COORDS (:PLOT, :WORLD, :DEVICE or :CANVAS,
or :SKY, right ascension and declination in degrees through the
current viewport's projection, or :POLAR, angle in degrees and
radius, see polar.lisp) to :CANVAS, the current drawing space.

#### `(with-device (var output-type destination &rest keys) &body body)` *macro*

Bind VAR to a device opened with OPEN-DEVICE, closing it afterward.

#### `(world-to-device device x y)`

The :DEVICE point of the :WORLD point (X,Y).

## Viewports, windows and targets

#### `(canvas-to-plot viewport cx cy)`

The :PLOT point of the point (CX,CY) of VIEWPORT's local frame (the
inverse of PLOT-TO-CANVAS).

#### `(defplot name (target &rest lambda-list) &body body)` *macro*

DEFUN for a function whose first argument may be a device or a
viewport.  If TARGET is named VIEWPORT, BODY sees a viewport (the
device's selected one when given a device).  If it is named DEVICE,
BODY sees the device, drawing as WITH-TARGET does with the function's
COORDS parameter (or :PLOT if it has none).

#### `(erase-viewport viewport &optional color)`

Clear VIEWPORT's rectangle (turned with it, if it is rotated) to
COLOR, by default its device's background (NIL: transparent), and
forget what was drawn in it, so legends are placed afresh.  The
numbers and labels DRAW-BOX drew outside the rectangle stay, but
ticks inside it and the inner half of its frame line go: call
DRAW-BOX again after drawing the new contents.

#### `(locate-point device cx cy &optional viewport)`

The page canvas point (CX,CY) of DEVICE as values X Y in the :PLOT
coordinates of VIEWPORT, by default the most recently made viewport
containing it (else the device's selected viewport), and that
viewport.

#### `(make-viewport device x1 x2 y1 y2 &key coords window angle clipping-region)`

Make a viewport of DEVICE spanning X1..X2, Y1..Y2 in COORDS (:DEVICE
or :WORLD), with WINDOW (X1 X2 Y1 Y2) in :PLOT coordinates, the whole
rotated by ANGLE degrees counterclockwise about its center.

#### `(plot-to-canvas viewport x y)`

Plot coordinates to the viewport's local frame (the drawing space
inside WITH-VIEWPORT).

#### `(set-transform viewport a b c d e f &key coords)`

Make :PLOT coordinates (X,Y) map to (A*X + B*Y + E, C*X + D*Y + F) in
COORDS (:WORLD, :DEVICE or :CANVAS), measured as if the viewport were
unrotated (its ANGLE then turns the result).  Use :WORLD, whose units
are equal in x and y, for rotations without skew.

#### `(set-window viewport x1 x2 y1 y2)`

Map :PLOT coordinates X1..X2, Y1..Y2 onto VIEWPORT's rectangle.

#### `(set-window-equal-scales viewport x1 x2 y1 y2 &key x-scale)`

Set the window to X1..X2, Y1..Y2 and shrink the viewport about its
center so one :PLOT unit is the same length in x and y (as PGWNAD),
or, with X-SCALE, so one x unit is X-SCALE times as long as one y
unit (e.g. cos(dec) for right ascension, see SET-WINDOW-SKY).

#### `(set-window-sky viewport ra dec half-width &optional half-height)`

Set VIEWPORT's window to a patch of sky centered on RA, DEC (degrees),
HALF-WIDTH by HALF-HEIGHT degrees on the sky, with right ascension
increasing to the left (east) and scales equal on the sky at DEC: an
RA degree is cos(DEC) as long as a Dec degree.  This is exact only
at DEC; use it for fields small enough that RA scale varies little
across them.  Returns the RA half-range, HALF-WIDTH / cos(DEC).

#### `(target-device target)`

The device of TARGET, a device or a viewport.

#### `(target-viewport target)`

TARGET if it is a viewport, else the device's selected viewport.

#### `viewport` *class*

A rectangle of a device's page with a window of :PLOT coordinates
mapped onto it (see the top of viewport.lisp).

#### `(viewport-angle viewport)`

Degrees counterclockwise about the center. `setf` changes it.

#### `(viewport-axis-aligned-p viewport)`

True if plot x runs along the viewport's horizontal edges and plot y
along its vertical ones (whatever the viewport's own ANGLE).

#### `(viewport-clipping-region viewport)`

A CLIPPING-REGION, :VIEWPORT for the
viewport's rectangle, or NIL for no clipping. `setf` changes it.

#### `(viewport-contains-p viewport cx cy)`

True if the page canvas point (CX,CY) is inside VIEWPORT's (possibly
rotated) rectangle.

#### `(viewport-device viewport)`

The device of a VIEWPORT.

#### `(viewport-polar viewport)`

The mapping for :POLAR coordinates (polar.lisp). `setf` changes it.

#### `(viewport-projection viewport)`

A sky projection for :SKY coordinates (sky.lisp). `setf` changes it.

#### `(viewport-rectangle viewport &optional coords)`

The viewport's rectangle, before rotation, as values X1 X2 Y1 Y2 in
COORDS (:DEVICE or :WORLD on the page, or :CANVAS for its own frame).

#### `(viewport-region viewport)`

The viewport's own rectangle as a clipping region, in its local frame.

#### `(viewport-transform viewport &optional coords)`

The transform from :PLOT coordinates to COORDS, as values A B C D E F
(see SET-TRANSFORM).

#### `(viewport-window viewport)`

The :PLOT coordinates at the viewport's edges, as values X1 X2 Y1 Y2:
x at the left and right edges and y at the bottom and top.  If the
plot is rotated or skewed within the viewport, the bounding box
(X1 < X2, Y1 < Y2) of the plot coordinates in the rectangle instead.

#### `(with-target (var &optional coords) &body body)` *macro*

Rebind the variable VAR, a device or a viewport, to its device and
run BODY in the viewport VAR names or, for :PLOT or :SKY COORDS (NIL
meaning :PLOT), the device's selected viewport, entering it for BODY
unless it already is.

#### `(with-viewport (viewport &key clip) &body body)` *macro*

Run BODY with VIEWPORT (or a device's selected viewport) selected and
entered on its device: drawing in its (possibly rotated) frame and,
if CLIP, clipped to its clipping region.  Restores the previous
viewports, transform, clip and drawing state afterward.

## Panels and insets

#### `(add-inset viewport x1 x2 y1 y2 &key coords window background zoom)`

Make and return a viewport inside VIEWPORT spanning X1..X2, Y1..Y2 in
COORDS: :FRACTION of VIEWPORT's rectangle (0..1 across and up) or
VIEWPORT's :PLOT coordinates.  WINDOW (X1 X2 Y1 Y2) sets its window.
BACKGROUND (NIL for none) fills it, hiding what is drawn beneath.
ZOOM (with WINDOW) draws DRAW-ZOOM-INDICATOR.

#### `(draw-panel-label viewport text &key position size halo)`

Write TEXT, e.g. "(a)", just inside a corner (:UPPER-LEFT,
:UPPER-RIGHT, :LOWER-LEFT, :LOWER-RIGHT) of VIEWPORT.

#### `(draw-zoom-indicator viewport inset &key color line-width)`

Mark on VIEWPORT the region INSET shows (INSET's window, in
VIEWPORT's plot coordinates) with a rectangle, and join its corners
to the matching corners of INSET with lines that cross neither.

#### `(make-panel-grid device rows cols &key left right bottom top hspace wspace window)`

A ROWS x COLS array of viewports of DEVICE (row 0 at the top),
spanning LEFT..RIGHT and BOTTOM..TOP in :DEVICE coordinates with
gaps HSPACE between rows and WSPACE between columns (fractions of the
page).  WINDOW (X1 X2 Y1 Y2), if given, is set on every panel.

#### `(panel-number-sides grid row col &key x-sides y-sides)`

The sides of panel ROW, COL that get numbers when panels share axes:
X-SIDES on the bottom row, Y-SIDES in the left column.

#### `(panel-omitted-ends grid row col)`

For DRAW-BOX's :OMIT-END-NUMBERS: where touching panels meet, the
number at the left end of x (all but the first column) and the top
end of y (all but the top row), which would collide with the
neighbor's.

## Drawing in any coordinates

#### `(plot-circle device xc yc r &key fill coords round follow-skew)`

Stroke, or fill if FILL, a circle centered at (XC,YC) in COORDS.
By default R is a radius in COORDS in both x and y, so the result is
an ellipse on the page wherever the x and y scales of COORDS differ
(e.g. :DEVICE on a non-square page, or most :PLOT windows).  With
ROUND, it is a true circle on the page whose radius is R measured
along the x axis of COORDS.  FOLLOW-SKEW is as for PLOT-ELLIPSE.

#### `(plot-ellipse device xc yc a b &key fill coords angle a-scale b-scale follow-skew)`

Stroke, or fill if FILL, an ellipse centered at (XC,YC) in COORDS
with semi-axes A and B.  Think of it as drawn in two steps:
1. A lies along the x axis of COORDS and B exactly perpendicular
to it on the page, each measured in x units of COORDS if its
scale (A-SCALE, B-SCALE) is :X or in y units if :Y.  The
defaults (:X and :Y) give the ellipse whose semi-axes are A in
x and B in y; A-SCALE and B-SCALE both :X with A = B gives a
true circle.
2. The ellipse is rotated ANGLE degrees counterclockwise on the
page about its center, keeping its shape.
So the shape never depends on the angle, even when the x and y
scales of COORDS differ.
If COORDS are skewed (a SET-TRANSFORM with shear), FOLLOW-SKEW adds
3. The ellipse is skewed like the rest of the plot, as if steps 1
and 2 happened in the plot before it was skewed.  At ANGLE 0 with
the default scales this is exactly the image of the ellipse
(x/A)^2 + (y/B)^2 = 1 of COORDS.
Without skew, FOLLOW-SKEW changes nothing.  With COORDS :SKY, only
the center is in right ascension and declination; A and B are in
:PLOT units (degrees on the projection plane).

#### `(plot-line device x1 y1 x2 y2 &key coords)`

Stroke the line from (X1,Y1) to (X2,Y2) in COORDS. In :SKY, a straight
line between the projected points; in :POLAR, a curve following the
angle.

#### `(plot-polygon device xs ys &key fill coords angle about follow-skew)`

Stroke, or fill if FILL, the closed polygon with vertices XS, YS.
ANGLE rotates it, keeping its shape, by that many degrees
counterclockwise on the page about ABOUT, a list (X Y) in COORDS,
by default the mean of the vertices.  With FOLLOW-SKEW and skewed
COORDS, it is rotated in the plot's unskewed geometry instead, then
skewed with the plot.

#### `(plot-polyline device xs ys &key coords)`

Stroke line segments joining the points of sequences XS and YS.

#### `(plot-rectangle device x1 y1 x2 y2 &key fill coords angle about follow-skew)`

Stroke, or fill if FILL, the rectangle with corners (X1,Y1), (X2,Y2).
ANGLE rotates it, keeping its shape, by that many degrees
counterclockwise on the page about ABOUT, a list (X Y) in COORDS,
by default its center.  With FOLLOW-SKEW and skewed COORDS, it is
rotated in the plot's unskewed geometry instead, then skewed with
the plot.

#### `(plot-text device x y string &key coords angle halign valign font size halo)`

Draw STRING anchored at (X,Y) in COORDS, as PUT-TEXT, except that
ANGLE is in degrees counterclockwise (as in PGPLOT).  HALO outlines
it (see WITH-HALO).  Returns the text's width in canvas units.

#### `(with-plot-rotation (device degrees x y &key coords follow-skew) &body body)` *macro*

Run BODY with all drawing, text included, rotated DEGREES
counterclockwise on the page about the point (X,Y) in COORDS.  The
rotation is applied on the page (to cairo's transform), so shapes
keep their shape however unequal the x and y scales of COORDS are,
and line widths are unaffected.  Coordinates inside BODY are
interpreted as if unrotated.
With FOLLOW-SKEW, and COORDS skewed by SET-TRANSFORM, the rotation
happens in the plot's unskewed geometry and is then skewed like the
rest of the plot (see plot-device.lisp).  That transform is not
rigid, so lines stroked and text drawn inside BODY are sheared too.

## Markers

#### `*astronomical-symbols*` *variable*

Names of the astronomical symbol markers, in a conventional order.

#### `*pgplot-markers*` *variable*

PGPLOT symbol number -> marker name.  20-27 are circles of increasing
size; -3 to -31 filled regular polygons.  10, 14, 15 and 19 only
approximate PGPLOT's glyphs.

#### `(define-marker name &rest parts)`

Define the marker NAME (a keyword) for PLOT-POINTS from PARTS, as the
built-in markers are defined (see the top of markers.lisp).

#### `(marker-names &optional all)`

Names of the geometric markers, sorted; with ALL, also the
astronomical symbols (see *ASTRONOMICAL-SYMBOLS*).

#### `(plot-point device x y &rest keys &key marker size angle coords font halo ink)`

Draw one marker at (X,Y); keywords as for PLOT-POINTS.

#### `(plot-points device xs ys &key marker size angle coords font halo colors sizes ink)`

Draw MARKER at each point of the sequences XS, YS in COORDS (as
PGPLOT's PGPT).  MARKER is a marker name, PGPLOT symbol number or
function (see the top of markers.lisp).  SIZE, about the marker's
radius in canvas units, defaults to 0.3 of DEFAULT-TEXT-SIZE.  ANGLE
turns each marker by that many degrees counterclockwise.  FONT is
the font spec for glyph markers (astronomical symbols).  HALO
outlines the markers (see WITH-HALO; default width 0.18 SIZE).
COLORS and SIZES, sequences, give each point its own color spec and
size (see VALUES-TO-COLORS and VALUES-TO-SIZES); a NIL color uses the
current one.  INK NIL draws only the halos.

## Axes

#### `(axis-ticks lo hi &key log step subdivisions target format)`

Major ticks as a list of (POSITION TEXT) and minor tick positions for
an axis over LO..HI.  FORMAT, a function of the value (10^position on
a log axis) returning text, overrides the labels.

#### `(category-ticks categories &key start)`

A tick generator for DRAW-BOX's :X-TICKS or :Y-TICKS naming
CATEGORIES (strings or runs) at the positions START, START + 1, ...
(as BAR-CHART, PLOT-BOX-PLOT and PLOT-VIOLINS place them), with no
minor ticks.

#### `(default-text-size canvas)`

The text size used when none is given: a fortieth of the smaller of
CANVAS's width and height.

#### `(draw-box viewport &key frame ticks numbers minor tick-direction grid zero-axes x-log y-log x-step y-step x-subdivisions y-subdivisions x-format y-format x-numbers y-numbers x-label y-label title size font omit-end-numbers x-ticks y-ticks)`

Draw a frame, ticks, numbers and labels around VIEWPORT, rotated with
it if it is rotated.
TICKS and NUMBERS list the sides (:BOTTOM :TOP :LEFT :RIGHT) that get
them.  TICK-DIRECTION is :IN, :OUT or :BOTH; MINOR enables minor
ticks.  GRID is NIL, T (major) or :MINOR (major and minor), drawn in
light gray.  ZERO-AXES draws the lines x=0 and y=0 when inside.
X-LOG/Y-LOG make log axes (window in log10 units).  X-STEP/Y-STEP set
the major interval and X-SUBDIVISIONS/Y-SUBDIVISIONS the minor
intervals per major; X-FORMAT/Y-FORMAT are functions from a value to
its text.  Y-NUMBERS is :HORIZONTAL or :VERTICAL (along the axis);
X-NUMBERS is :HORIZONTAL (a number that would overlap the one before
is skipped) or an angle in degrees to slant them, ending at their
ticks (for long category names, see CATEGORY-TICKS).
X-LABEL, Y-LABEL and TITLE are text (strings or runs).
OMIT-END-NUMBERS lists window ends (:X-LOW :X-HIGH :Y-LOW :Y-HIGH)
whose numbers are left out, e.g. where touching panels meet.
X-TICKS and Y-TICKS, functions of the window's (LO HI) returning
values MAJORS ((POSITION TEXT) ...) and MINORS (positions) as
AXIS-TICKS does, replace the standard ticks, e.g. SEXAGESIMAL-TICKS.  SIZE is the
text size; FONT a font spec.  Lines use the current style.

#### `(draw-frame viewport)`

Draw VIEWPORT's rectangle, rotated with it, in the current style.

#### `(draw-secondary-axis viewport side function &key inverse label ticks format size font minor tick-direction)`

Draw an axis on SIDE (:TOP or :RIGHT) of VIEWPORT in other units:
FUNCTION maps the window's x (for :TOP) or y (for :RIGHT) to the
secondary value and must be monotonic; INVERSE, if given, maps back
(else it is found numerically).  Ticks come from TICKS (a generator
as for DRAW-BOX's :X-TICKS) or round secondary values (FORMAT as
AXIS-TICKS), with numbers outside and LABEL beyond them.  Ask
DRAW-BOX not to tick and number that side.

#### `(sexagesimal-text value unit resolution sign)`

Text for VALUE (hours or degrees) to RESOLUTION seconds: runs with
raised h m s for :HOURS, or degrees, ' and " for :DEGREES (or
:LONGITUDE, wrapped to 0..360), with a + sign if SIGN and VALUE is
positive.

#### `(sexagesimal-ticks &key unit target sign)`

A tick generator for DRAW-BOX's :X-TICKS or :Y-TICKS on an axis in
degrees: about TARGET ticks at round sexagesimal steps, labeled in
UNIT :HOURS (degrees / 15, as right ascension), :DEGREES (as
declination, with SIGN adding + to positive values) or :LONGITUDE
(degrees wrapped to 0..360).

## Date and time axes

#### `(calendar-to-mjd year month day &optional hour minute second)`

The MJD of the UTC date and time.

#### `(iso-to-mjd string)`

The MJD of an ISO 8601 UTC date or date-time, "2024-03-14",
"2024-03-14T21:30" or "2024-03-14 21:30:15.5".

#### `(mjd-to-calendar mjd)`

Values year, month, day, hour, minute, second (UTC) of MJD.

#### `(time-ticks &key scale target utc-offset context)`

A tick generator for DRAW-BOX's :X-TICKS or :Y-TICKS on an axis of
times in SCALE (:MJD, :JD, :UNIX or :UNIVERSAL; see the top of
time-axes.lisp): about TARGET ticks at round calendar steps, labeled
concisely in UTC + UTC-OFFSET hours, the first tick with CONTEXT.

#### `(time-to-unix value scale)`

VALUE in the time SCALE as Unix seconds.

#### `(unix-to-time seconds scale)`

Unix SECONDS in the time SCALE.

## Legends

#### `(draw-legend viewport entries &key position size font frame background frame-color)`

Draw a legend of ENTRIES (see the top of legend.lisp) for VIEWPORT.
POSITION is :BEST (where it covers least of the plot), :UPPER-RIGHT,
:UPPER-LEFT, :LOWER-RIGHT, :LOWER-LEFT, :UPPER-CENTER, :LOWER-CENTER,
:CENTER-LEFT, :CENTER-RIGHT or :CENTER (inside the viewport),
:OUTSIDE-RIGHT (beside it), or a list (X Y) in :DEVICE coordinates
for the box's upper left corner.  SIZE is the text size (default 0.8
of DEFAULT-TEXT-SIZE).  BACKGROUND fills the
box (NIL for none) and FRAME outlines it in FRAME-COLOR.

## Error bars, areas, histograms and arrows

#### `(annotate device x y text tx ty &key coords gap head-size halo halign valign size font)`

Write TEXT at (TX,TY) with an arrow from it to the point (X,Y), both
in COORDS.  The arrow starts GAP canvas units outside the text's box.

#### `(histogram values &key bins range weights)`

Bin VALUES (a sequence; NaNs ignored).  BINS is a number of equal
bins, a sequence of bin edges, or :AUTO (the Freedman-Diaconis rule,
at least 1 bin).  RANGE, a list (LO HI), limits the bins (default the
data's range); values outside are dropped, and the last bin includes
its upper edge.  WEIGHTS, a sequence, weights each value.  Returns
values EDGES (N+1) and COUNTS (N), double-float vectors.

#### `(histogram2d xs ys &key bins x-range y-range weights)`

Bin the points XS, YS (NaNs ignored) on a grid: BINS is a number for
both axes or a list (NX NY); X-RANGE and Y-RANGE (lists (LO HI))
default to the data's.  WEIGHTS weights each point.  Returns values
COUNTS, a double-float array (NY NX) with rows along y as PLOT-IMAGE
expects, X-EDGES and Y-EDGES.

#### `(plot-arrow device x1 y1 x2 y2 &key coords head-size head-angle fill both)`

Draw an arrow from (X1,Y1) to (X2,Y2) in COORDS, with a head HEAD-SIZE
canvas units long (default 0.6 of DEFAULT-TEXT-SIZE) and HEAD-ANGLE
degrees each side, filled unless FILL is NIL; BOTH puts a head at
each end.

#### `(plot-error-bars device xs ys &key yerr xerr yerr-lower yerr-upper xerr-lower xerr-upper cap-size coords halo)`

Draw error bars at the points XS, YS.  YERR (a number or a sequence)
gives symmetric y errors; YERR-LOWER and YERR-UPPER asymmetric ones
(overriding YERR on their side); likewise XERR, XERR-LOWER,
XERR-UPPER.  Each bar ends in a cap CAP-SIZE canvas units across
(default 0.4 of DEFAULT-TEXT-SIZE; 0 for none).  HALO outlines the
bars (see WITH-HALO).

#### `(plot-fill-between device xs y1s &key y2 coords outline)`

Fill the area between the curves (XS, Y1S) and (XS, Y2) with the
current color; Y2 is a sequence, or a number for a baseline (default
0).  OUTLINE also strokes both curves.

#### `(plot-histogram device values &key bins range weights edges counts style normalize cumulative baseline bar-width coords)`

Bin VALUES (see HISTOGRAM; or give EDGES and COUNTS directly) and draw
the histogram with the current style.  STYLE :STEP outlines it,
:FILLED fills it (the current color; outline it separately), :BARS
draws separate bars BAR-WIDTH of each bin wide.  NORMALIZE :DENSITY
divides by the total and the bin width (area 1), :FRACTION by the
total.  CUMULATIVE sums the counts.  BASELINE is the bottom (use a
small positive number on a log axis).  Returns the EDGES and the
COUNTS drawn.

#### `(plot-histogram2d device xs ys &key bins x-range y-range weights colormap vmin vmax scale reverse empty interpolation coords)`

Bin the points XS, YS (see HISTOGRAM2D) and draw the counts as an
image through COLORMAP (VMIN default 0, VMAX the largest count;
SCALE, REVERSE as PLOT-IMAGE).  Empty bins are left transparent
unless EMPTY is T (then they take the colormap's low end).  Returns
VMIN, VMAX and the COUNTS array.

#### `(values-to-colors values &key colormap vmin vmax scale reverse)`

A list of colors (R G B), one per element of VALUES, through COLORMAP
with VMIN, VMAX (default the values' range), SCALE and REVERSE as for
PLOT-IMAGE; NaNs give NIL.  Second and third values: VMIN and VMAX,
for DRAW-COLORBAR.

#### `(values-to-sizes values &key min-size max-size vmin vmax)`

A list of marker sizes, one per element of VALUES, from MIN-SIZE at
VMIN to MAX-SIZE at VMAX (default the values' range), with marker
area proportional to the value.  Second and third values: VMIN, VMAX.

## Bar charts, box plots and violin plots

#### `*category-colors*` *variable*

Series colors in order: blue, orange, aqua, yellow, magenta, green,
violet, red (a palette checked for color-vision deficiency, adjacent
pairs distinct).

#### `(bar-chart viewport categories data &key names mode orientation colors width errors value-labels value-format value-range category-angle category-label value-label title legend grid size gap-color)`

Draw a bar chart filling VIEWPORT: its window, bars, axes, category
names and (for several named series) legend.  CATEGORIES are the
names (strings or runs) along the category axis; DATA is one series
(a list of numbers, one per category) or a list of series.  MODE
:GROUPED puts each category's series side by side within WIDTH;
:STACKED piles them (positive values up, negative down).
ORIENTATION :VERTICAL (columns, categories along x) or :HORIZONTAL
(categories down the y axis, first at the top).  NAMES label the
series in the legend at LEGEND (a DRAW-LEGEND position, by default
:BEST, or NIL).
COLORS default to *CATEGORY-COLORS* in order.  ERRORS, one spec per
series (as for PLOT-BARS), adds error bars (grouped mode).
VALUE-LABELS writes values at the bar ends (totals when stacked).
VALUE-RANGE (LO HI) sets the value axis (default from the data, with
room for labels).  CATEGORY-ANGLE slants the category names (see
DRAW-BOX's X-NUMBERS).  CATEGORY-LABEL, VALUE-LABEL and TITLE name
the axes and chart; GRID draws lines across the value axis.  SIZE is
the text size.  Returns VIEWPORT.

#### `(box-stats values &key whisker)`

A plist of the box-plot statistics of VALUES (NaNs dropped): :N,
:MEAN, :MEDIAN, :Q1, :Q3, :LOW and :HIGH (the whisker ends: the most
extreme values within WHISKER times the interquartile range of the
box) and :OUTLIERS (the rest).

#### `(category-colors n &optional colors)`

N series colors: COLORS if given, else the first N of *CATEGORY-COLORS*.

#### `(kde values &key bandwidth points cut)`

A Gaussian kernel density estimate of VALUES: values GRID and
DENSITY (vectors of POINTS), the grid spanning the data plus CUT
bandwidths each side, the density integrating to 1.

#### `(plot-bars device positions values &key width bottoms orientation color colors gap-color gap-width outline errors cap-size value-labels value-format texts label-size bars coords)`

Draw a bar at each of POSITIONS (along the category axis) from its
bottom (BOTTOMS, a number or sequence) to bottom + its VALUE, WIDTH
wide (a number or sequence).  ORIENTATION :VERTICAL (columns) or
:HORIZONTAL (bars along x).  Fill: COLORS (a color per bar), else
COLOR, else the current color.  GAP-COLOR outlines each bar
GAP-WIDTH canvas units wide (NIL for none), separating touching bars;
OUTLINE, a color, strokes it with the current line width.  ERRORS (a
number, a sequence, or a sequence of (LOWER UPPER)) adds error bars
at the ends, CAP-SIZE wide.  VALUE-LABELS writes each value, by
VALUE-FORMAT (or the strings TEXTS), just beyond its bar's end (and
error bar), in LABEL-SIZE text.  BARS NIL draws only the labels and
error bars.

#### `(plot-box-plot device positions datasets &key width orientation colors color fill-alpha whisker show-outliers outlier-marker outlier-size means mean-marker coords)`

Draw a box-and-whisker plot of each of DATASETS (sequences of
numbers) at POSITIONS: a box from the first to the third quartile
with a line at the median, whiskers to the most extreme values within
WHISKER times the box's height, and the outliers beyond as
OUTLIER-MARKERs (if SHOW-OUTLIERS).  MEANS marks the mean with MEAN-MARKER.  Each box is
WIDTH wide, in its color (COLORS, one per dataset, else COLOR, else
the current color): filled at FILL-ALPHA and outlined.  ORIENTATION
:VERTICAL or :HORIZONTAL.  Returns the BOX-STATS of each dataset.

#### `(plot-violins device positions datasets &key width orientation colors color fill-alpha bandwidth cut scale inner coords)`

Draw a violin for each of DATASETS at POSITIONS: its kernel density
estimate (KDE, with BANDWIDTH, default Silverman's rule, extending CUT
bandwidths beyond the data) mirrored about the position.  SCALE
:WIDTH makes every violin WIDTH wide at its widest; :AREA gives them
equal areas (the widest WIDTH wide); :COUNT makes areas proportional
to the number of values.  INNER :BOX draws a thin interquartile box,
whiskers and a white median dot; :QUARTILES dashed lines at the
quartiles; NIL nothing.  Colors as for PLOT-BOX-PLOT, filled at
FILL-ALPHA.  Returns the BOX-STATS of each dataset.

#### `(quantile sorted q)`

The Q quantile (0..1) of the SORTED vector, interpolating linearly.

## Step plots and stacked areas

#### `(plot-stacked-area device xs series &key names colors baseline normalize order step where gap-color gap-width labels label-size coords)`

Stack SERIES (sequences of values >= 0 at XS, the first at the
bottom, or in ORDER, see STACK-ORDER; see STACK-SERIES for BASELINE
and NORMALIZE) and fill each layer in its series' color: COLORS, or
*CATEGORY-COLORS* in order.  Layers are separated by GAP-WIDTH lines
of GAP-COLOR.  STEP draws them as staircases (WHERE as for
STEP-POINTS; with one more X than values per series, XS are bin
edges).  LABELS writes each of NAMES inside its layer where it fits
(at LABEL-SIZE, in the current color).  Returns legend entries for
DRAW-LEGEND, top layer first, and the LOW and HIGH of the stack.

#### `(plot-step device xs ys &key where fill baseline coords)`

Stroke the staircase through XS, YS (see STEP-POINTS for WHERE and
bin edges) in the current style.  FILL, a color (T for the current
one), first fills under it down to BASELINE.  Returns the corners as
two lists.

#### `(stack-order series order)`

The indices of SERIES in stacking order, bottom first: ORDER NIL for
as given, :INSIDE-OUT (see the top of the file), or a list of
indices.

#### `(stack-series series &key baseline normalize order)`

Stack SERIES (a list of equally long sequences of values >= 0, the
first at the bottom, or in ORDER, see STACK-ORDER) into layers.  BASELINE is :ZERO, :CENTER (the
stack symmetric about zero) or :WIGGLE (a streamgraph: the baseline
chosen to minimize the layers' slopes, after Byron & Wattenberg 2008,
for evenly spaced points, centered about zero on average).  NORMALIZE
T scales each column to total 100 (percentages), a number to total
that.  Returns values LAYERS, a list of (LOWER UPPER) double-float
vectors, and the LOW and HIGH of the whole stack.

#### `(step-points xs ys &key where)`

The corners of the staircase through the points XS, YS as two lists,
stepping at each point (WHERE :POST), before it (:PRE) or halfway
between points (:MID).  If XS has one more element than YS, XS are
bin edges and each value spans its bin (WHERE is ignored).

## Pie and donut charts

#### `(color-luminance spec)`

Relative luminance of the color SPEC, 0 (black) .. 1 (white).

#### `(pie-chart viewport values &key names colors inner start clockwise gap-color gap-width percentages percent-format labels center-text size title)`

Draw a pie (or, with INNER a fraction of the radius, a donut) of
VALUES in VIEWPORT, whose window it sets.  NAMES label the wedges
outside (LABELS NIL leaves them to a legend: the entries are
returned); COLORS default to *CATEGORY-COLORS*.  Wedges start at
START degrees (90: 12 o'clock) and run CLOCKWISE, separated by
GAP-WIDTH lines of GAP-COLOR.  PERCENTAGES writes each wedge's share,
formatted with PERCENT-FORMAT (given the rounded percentage), where
it fits.  CENTER-TEXT goes in a donut's hole; TITLE above.  SIZE is
the text size.  Returns legend entries for DRAW-LEGEND.

## Heatmaps

#### `(heatmap viewport matrix &key row-names col-names colormap vmin vmax center scale reverse annotate value-format gap-color gap-width nan-color colorbar colorbar-label col-angle x-label y-label title size cols-on)`

Draw MATRIX (a 2D array or list of rows; NaN or NIL cells in
NAN-COLOR) as a heatmap filling VIEWPORT, whose window it sets: cell
(row R, column C) is centered on x = C, y = R, row 0 at the top.
Cells are colored through COLORMAP with SCALE and REVERSE (as
PLOT-IMAGE) over VMIN..VMAX (default the data's range, made symmetric
about CENTER if given) and separated by GAP-WIDTH lines of GAP-COLOR.
ANNOTATE writes each value with VALUE-FORMAT (a function of the
value) in black or white for contrast.  ROW-NAMES and COL-NAMES label
the axes (COLS-ON :BOTTOM or :TOP; COL-ANGLE slants them).  COLORBAR
draws one beside, with COLORBAR-LABEL.  Returns VMIN and VMAX.

## Images and colorbars

#### `(colorbar-viewport viewport &key side width gap)`

A viewport for a colorbar beside VIEWPORT on SIDE (:RIGHT, :LEFT,
:TOP or :BOTTOM), WIDTH thick and GAP away (fractions of the page
width), turned with VIEWPORT if it is rotated.

#### `(draw-colorbar viewport &key colormap vmin vmax scale reverse label orientation size levels colors)`

Draw a colorbar for VMIN..VMAX through COLORMAP and SCALE (as given
to PLOT-IMAGE) filling VIEWPORT, with numbers along it and LABEL.
With LEVELS (as returned by PLOT-FILLED-CONTOURS), draw discrete
bands between them instead, colored as there (COLORMAP or COLORS).
ORIENTATION :VERTICAL puts the numbers on the right and LABEL on
top; :HORIZONTAL puts both below.

#### `(image-limits array &key percentile)`

Values VMIN VMAX of the non-NaN elements of ARRAY: the minimum and
maximum, or with PERCENTILE (e.g. 99) the limits of the central
PERCENTILE percent of the values.

#### `(plot-image device array &key colormap vmin vmax percentile scale reverse nan-color x-range y-range origin transform interpolation coords wcs)`

Draw the 2D ARRAY through COLORMAP (see the top of image.lisp).
VMIN, VMAX: data values at the ends of the colormap (default from
IMAGE-LIMITS with PERCENTILE).  SCALE: :LINEAR, :SQRT, :SQUARE, :LOG,
:ASINH or a function of 0..1.  REVERSE flips the colormap.  X-RANGE,
Y-RANGE: lists (X1 X2), (Y1 Y2) of the image's outer edges in COORDS
(default pixel centers at integer coordinates).  ORIGIN :LOWER puts
row 0 at Y1, :UPPER at Y2.  TRANSFORM, an affine (or list (A B C D
E F)) taking pixel centers (column i, row j, 0-based) to COORDS as
x = A i + B j + E, y = C i + D j + F, places the image instead,
rotated or sheared if need be (e.g. by a WCS CD matrix, see
WCS-IMAGE-TRANSFORM).  WCS, any object following the WCS protocol in
sky.lisp, places the image on the current viewport's sky projection
(COORDS :PLOT), reprojecting it if need be.  INTERPOLATION :NEAREST
or :BILINEAR.  Returns the VMIN and VMAX used, for DRAW-COLORBAR.

#### `(scale-function scale)`

The function of a fraction 0..1 named by SCALE: :LINEAR, :SQRT,
:SQUARE, :LOG (log(1 + 1000 f) / log 1001, as DS9) or :ASINH
(asinh(10 f) / asinh 10); or SCALE itself if a function.

## Contours

#### `(contour-levels array n &key vmin vmax)`

About N round levels strictly between VMIN and VMAX (default the
range of ARRAY).

#### `(contour-lines array level)`

Contours of the 2D ARRAY at LEVEL as a list of (XS YS CLOSED-P), XS and
YS double-float vectors in pixel coordinates (column, row) of ARRAY.
CLOSED-P lines end where they began (the first point is not repeated).

#### `(filled-contour-levels array n)`

About N round bands spanning ARRAY's range: N+1-ish level edges from
a round number at or below the minimum to one at or above the maximum.

#### `(plot-contours device array &key levels n x-range y-range origin transform wcs coords color colormap negative-dash labels label-size label-spacing label-format halo)`

Draw contours of the 2D ARRAY at LEVELS (default about N round
levels, see CONTOUR-LEVELS), placed as PLOT-IMAGE places ARRAY
(X-RANGE, Y-RANGE, ORIGIN, TRANSFORM or WCS, COORDS); TRANSFORM may
also be a function of the pixel (column, row) returning the COORDS
point, and WCS maps every vertex through the image's WCS onto the
current viewport's sky projection.
Style: COLOR, a color spec or a list cycled over the levels (default
the current color); or COLORMAP, coloring each level by its place in
the range of LEVELS.  NEGATIVE-DASH, a dash pattern, dashes contours
below zero.  Line width is the current one.
LABELS writes each level along its contours, with a gap in the line:
T (or :STRAIGHT) as straight text along the line, :CURVED with each
character following the curve.  Labels go about every LABEL-SPACING
canvas units (default a third of the page width) at the least-bent
spot nearby, skipping sharp bends and spots that would overlap
another label, in LABEL-SIZE text (default 0.6 of DEFAULT-TEXT-SIZE).
The text is LABEL-FORMAT applied to the level, or the level with
enough decimals.  HALO outlines lines and labels (see WITH-HALO;
default width 0.7 of the line width).  Returns the levels drawn.

#### `(plot-filled-contours device array &key levels n colormap colors x-range y-range origin transform wcs coords)`

Fill the bands between consecutive LEVELS of the 2D ARRAY (default
about N round bands covering its range, see FILLED-CONTOUR-LEVELS),
each band colored by COLORMAP at its middle or by the list COLORS.
Values below the first level are left unfilled; values above the
last get the last band's color.  The array is placed as PLOT-IMAGE
places it (X-RANGE, Y-RANGE, ORIGIN, or TRANSFORM or WCS as for
PLOT-CONTOURS, COORDS), with values at the pixel centers.  Returns
the LEVELS, for DRAW-COLORBAR :LEVELS.

## Vector fields

#### `(plot-streamlines device function x-range y-range &key density arrows head-size min-length coords)`

Draw streamlines of FUNCTION (X Y -> U V) over X-RANGE and Y-RANGE,
lists (LO HI), about 30 x DENSITY lines across (see above), in the
current style.  ARROWS puts an arrowhead (HEAD-SIZE canvas units,
default 0.5 of DEFAULT-TEXT-SIZE) midway along each line.  Lines
crossing fewer than MIN-LENGTH cells are dropped.

#### `(plot-vector-field device function x-range y-range &rest keys &key n ny scale pivot coords &allow-other-keys)`

Draw arrows of FUNCTION, of X and Y returning values U and V, at the
centers of an N by NY grid (NY default from the window's shape) over
X-RANGE and Y-RANGE, lists (LO HI).  SCALE defaults to make the
longest arrow 0.9 of a cell.  Arrows are centered on the grid points
(PIVOT :MIDDLE).  Other keywords go to PLOT-VECTORS.

#### `(plot-vectors device xs ys us vs &key scale pivot heads head-size head-angle colormap vmin vmax colors coords)`

Draw an arrow for each vector (U,V) of sequences US, VS at the points
XS, YS, SCALE plot units per unit of U and V.  PIVOT puts the point at
the arrow's :TAIL, :MIDDLE or :TIP.  HEADS NIL draws plain segments
(orientations, e.g. polarization).  HEAD-SIZE is in canvas units
(default 0.5 of DEFAULT-TEXT-SIZE).  COLORMAP colors the arrows by
length, VMIN..VMAX (default the lengths' range); COLORS gives each a
color instead; otherwise they are in the current style.  Returns
VMIN and VMAX of the lengths, for DRAW-COLORBAR.

## Polar plots

#### `(draw-polar-grid viewport &key angle-step radii r-target angle-labels r-format r-label-angle grid-color frame labels size title)`

Draw VIEWPORT's polar grid: circles at RADII (default round values,
about R-TARGET) and spokes every ANGLE-STEP degrees in GRID-COLOR,
the rim (FRAME) in the current style, and with LABELS the angles
around the rim (ANGLE-LABELS, a list of texts for the spokes in
order, or default "30°") and the radii (as ticks are labeled, or by
R-FORMAT, a function of the value) along the spoke at R-LABEL-ANGLE
(default halfway to the first spoke).  SIZE is the text size
(default 0.7 of DEFAULT-TEXT-SIZE); TITLE goes above.

#### `(plot-polar-bars device angles values &key width bottoms color colors gap-color gap-width)`

Draw a wedge for each of VALUES centered on ANGLES (degrees), WIDTH
degrees wide (default 0.9 of the angles' spacing), from BOTTOMS (a
radius or a sequence) out to BOTTOM + VALUE, so successive calls with
BOTTOMS stack them.  Each is filled with COLOR, its element of
COLORS, or the current color, and outlined in GAP-COLOR.

#### `(plot-to-polar viewport x y)`

Values ANGLE (degrees, 0..360) and RADIUS of the :PLOT point X, Y.

#### `(polar-to-plot viewport angle radius)`

Values: the :PLOT coordinates of ANGLE (degrees), RADIUS through
VIEWPORT's polar mapping.

#### `(set-window-polar viewport r-max &key r-min zero direction sector margin)`

Give VIEWPORT a polar mapping (see the top of polar.lisp) from R-MIN
at the center to R-MAX at the rim, and a window with equal scales
around the disk or SECTOR, with MARGIN (a fraction of the radius) for
labels.  Drawing is clipped to the disk or sector.  Returns VIEWPORT.

## Corner plots

#### `(corner-plot device samples &key names bins ranges truths sigmas smooth color truth-color points titles left right bottom top space size)`

Draw a corner plot of SAMPLES, a list with a sequence of values for
each parameter (all equally long), filling LEFT..RIGHT, BOTTOM..TOP
of DEVICE (see the top of corner.lisp).  NAMES label the parameters;
RANGES, a list of (LO HI) or NIL for each, default to the central 99%
of the samples widened by 10%; BINS bins each axis.  The 2D densities are smoothed by SMOOTH
bins and contoured at SIGMAS, filled in shades of COLOR; POINTS draws
the samples outside the outer contour.  TRUTHS, a list of values (NIL
to skip one), are marked in TRUTH-COLOR.  TITLES puts each median and
spread over its histogram.  Returns the grid of viewports (only those
on and below the diagonal are drawn).

## 3D surfaces

#### `(draw-axes3d view &key part grid x-label y-label z-label size box tick-length)`

Draw the box, grid, ticks, numbers and labels of VIEW's axes, as
matplotlib places them: x and y along the floor edges nearest the
camera, z up the leftmost vertical edge.  PART :BACK draws what lies
behind a surface (back box edges and, with GRID, grid lines on the
floor and the two back walls); :FRONT the rest; :ALL both.  SIZE is
the text size (default 0.8 of DEFAULT-TEXT-SIZE); TICK-LENGTH is a
fraction of the box.  Lines use the current style.

#### `(draw-box3d view &key part)`

Stroke the edges of the data box with the current style: PART :BACK
(the three edges meeting at the corner farthest from the camera, to
draw before a surface), :FRONT (the others, after it) or :ALL.

#### `(make-view3d viewport &key x-range y-range z-range azimuth elevation box-aspect distance margin)`

A 3D view of the box X-RANGE x Y-RANGE x Z-RANGE (lists (LO HI)) into
VIEWPORT, whose window is set to fit the projected box (plus MARGIN, a
fraction) with equal scales.  See the top of plot3d.lisp.

#### `(plot-floor-contours view array &rest keys &key x-range y-range z &allow-other-keys)`

Draw contours of ARRAY (nodes as PLOT-SURFACE) on the plane at height
Z (default the bottom of the box), before the surface.  Other
keywords are as PLOT-CONTOURS (:LEVELS, :N, :COLOR, :COLORMAP,
:NEGATIVE-DASH), except labels.

#### `(plot-surface view array &key x-range y-range colormap vmin vmax scale reverse color-array shade light smooth edges edge-color edge-width fill contours contour-color contour-colormap contour-width)`

Draw the surface z = ARRAY[row, column], rows along y and columns
along x, the grid nodes spanning X-RANGE and Y-RANGE (default the
view's).
Color: each cell (or, with SMOOTH, each node) through COLORMAP by
height, or by COLOR-ARRAY (same shape) if given, with VMIN, VMAX,
SCALE and REVERSE as for PLOT-IMAGE.  SHADE (0..1) darkens surface
turned away from LIGHT, a direction in data-box axes; NIL for none.
SMOOTH blends colors across cells (mesh gradients) instead of flat
facets.  EDGES strokes cell edges in EDGE-COLOR, EDGE-WIDTH wide;
FILL NIL fills cells with the background instead, a hidden-line
wireframe (both imply flat cells).
CONTOURS (a list of levels, a number of levels, or T) draws contour
lines on the surface, hidden correctly behind nearer parts of it, in
CONTOUR-COLOR (default the current color) or colored by
CONTOUR-COLORMAP, CONTOUR-WIDTH wide.
Cells are drawn back to front.  Returns the VMIN and VMAX used.

#### `(plot3d-points view xs ys zs &rest keys &key marker size angle font halo)`

Draw markers at the 3D points XS, YS, ZS; keywords as PLOT-POINTS.

#### `(plot3d-polyline view xs ys zs)`

Stroke line segments joining the 3D points XS, YS, ZS.

#### `(plot3d-text view x y z text &rest keys &key halign valign angle font size halo)`

Draw TEXT at the 3D point (X,Y,Z), upright on the page; keywords as
PLOT-TEXT.

#### `(project3d view x y z)`

Values: the :PLOT coordinates SX SY of data point (X,Y,Z), and its
DEPTH (larger is nearer the camera).

#### `view3d` *class*

A view of a 3D box of data into a viewport, from an azimuth and an
elevation (see MAKE-VIEW3D).

#### `(view3d-azimuth view3d)`

The azimuth of a VIEW3D.

#### `(view3d-elevation view3d)`

The elevation of a VIEW3D.

#### `(view3d-viewport view3d)`

The viewport of a VIEW3D.

#### `(view3d-x-range view3d)`

The x range of a VIEW3D.

#### `(view3d-y-range view3d)`

The y range of a VIEW3D.

#### `(view3d-z-range view3d)`

The z range of a VIEW3D.

## Sky projections and WCS

#### `ait-projection` *class*

The Hammer-Aitoff (AIT) projection: equal area, the sky in a 2:1
ellipse with curved parallels.

#### `allsky-projection` *class*

A projection of the whole sky: Hammer-Aitoff, Mollweide or plate
carree (see MAKE-PROJECTION).

#### `arc-projection` *class*

The zenithal equidistant (ARC) projection: distances from the
reference point are true.

#### `car-projection` *class*

The plate carree (CAR) projection: RA and Dec as x and y.

#### `(draw-projection-boundary viewport)`

Draw the edge of VIEWPORT's projected sky, if it has one, in the
current style (clipped to the viewport's rectangle).

#### `(draw-sky-grid viewport &key grid ra-target dec-target size font grid-color grid-width frame coordinates labels lon-sides lat-sides x-label y-label tick-length)`

Draw lines of constant longitude and latitude in COORDINATES (a
frame, :EQUATORIAL, :GALACTIC or :ECLIPTIC; default the projection's
own) through VIEWPORT's projection (if GRID), the frame (if FRAME),
and sexagesimal labels (if LABELS): right ascension in hours, other
longitudes in degrees.  A second call with other COORDINATES
overlays a second grid, e.g. galactic over equatorial, with its
labels on other sides (LON-SIDES '(:TOP), LAT-SIDES '(:RIGHT)) and
FRAME NIL.
Usually each longitude line (about RA-TARGET of them, default 4) is
labeled at the first edge in LON-SIDES it crosses, each latitude
line (about DEC-TARGET, default 4) at the first in LAT-SIDES, with
ticks TICK-LENGTH canvas units long (default 0.6 of SIZE), and the
axes titled X-LABEL and Y-LABEL (default from COORDINATES; NIL for
none) beside the first of LON-SIDES and LAT-SIDES.
When the projection's whole boundary is in the window (as after
SET-WINDOW-PROJECTION without a size), the frame is that boundary
instead, longitude lines (default about 8) are labeled beyond their
ends at the boundary, or below the equator if they run pole to pole,
and latitude lines (default about 6) left of their left ends, or on
their lowest point if they close around a pole.  SIZE is the text size
(default DEFAULT-TEXT-SIZE).  Lines use GRID-COLOR, GRID-WIDTH.

#### `(equatorial-to-frame frame ra dec)`

Values: the longitude and latitude in FRAME of RA, DEC (degrees).

#### `(frame-to-equatorial frame lon lat)`

Values RA DEC (degrees) of longitude LON, latitude LAT in FRAME.

#### `(make-projection code ra0 &optional dec0 frame)`

The projection named by CODE (:TAN :SIN :ARC :ZEA :STG, or all-sky
:AIT :MOL :CAR, or a FITS CTYPE such as "RA---SIN" or
"GLON-AIT") about longitude RA0, latitude DEC0 (degrees; an
all-sky projection about DEC0 not 0 is tilted, its frame's north
pole straight up from the center) in FRAME (:EQUATORIAL, :GALACTIC or
:ECLIPTIC; default from a CTYPE, else :EQUATORIAL).

#### `(make-tan-projection ra0 dec0)`

A gnomonic (TAN) projection about RA0, DEC0 (degrees):
(MAKE-PROJECTION :TAN RA0 DEC0).

#### `(make-wcs &key crval crpix cd cdelt crota projection frame)`

A WCS from FITS-style values: CRVAL (RA DEC, or longitude and
latitude in FRAME), CRPIX (X Y), and either CD (CD1_1 CD1_2 CD2_1
CD2_2) or CDELT (D1 D2) with CROTA (degrees), in PROJECTION (a code
or CTYPE for MAKE-PROJECTION, default :TAN) and FRAME (default from a
CTYPE, else :EQUATORIAL).  CDELT1 is normally negative (east to the
left).

#### `mol-projection` *class*

The Mollweide (MOL) projection: equal area, the sky in a 2:1 ellipse
with straight parallels.

#### `(plane-to-sky projection x y)`

Values RA DEC (degrees) of the plane point X, Y;
NaNs outside the projection's boundary.

#### `(plot-to-sky viewport x y)`

Values RA DEC of the :PLOT point X, Y through VIEWPORT's projection.

#### `(projection-boundary projection)`

The edge of the projected sky as lists XS YS of a
closed polygon in the plane, or NIL if the projection is unbounded.

#### `(projection-dec0 sky-projection)`

The dec0 of a SKY-PROJECTION.

#### `(projection-frame sky-projection)`

The frame of a SKY-PROJECTION.

#### `(projection-ra0 sky-projection)`

The ra0 of a SKY-PROJECTION.

#### `(reproject-image array wcs viewport &key interpolation max-size resolution)`

Resample the 2D ARRAY, with WCS, onto a grid in VIEWPORT's :PLOT
coordinates covering the part of its window the image overlaps.  The
grid step is RESOLUTION plot units (default the image's pixel size at
its center, or 1/600 of the window if that is smaller), at most
MAX-SIZE samples a side.  INTERPOLATION
:NEAREST or :BILINEAR; outside the image, NaN.  Returns the new array
and the X-RANGE and Y-RANGE (outer edges) to plot it with.

#### `(set-window-projection viewport projection &optional half-width half-height)`

Give VIEWPORT the sky PROJECTION and a window HALF-WIDTH by
HALF-HEIGHT degrees (in the plane) about its reference point, east
(increasing RA) to the left, with equal scales.  Without HALF-WIDTH,
the window is the projection's whole boundary, and drawing in the
viewport is clipped to the boundary.  Returns VIEWPORT.

#### `(set-window-tan viewport projection half-width &optional half-height)`

SET-WINDOW-PROJECTION with a window HALF-WIDTH by HALF-HEIGHT.

#### `sin-projection` *class*

The orthographic (SIN) projection: the hemisphere about the reference
point as seen from far away.

#### `sky-projection` *class*

A projection of the sky about the reference point
RA0, DEC0 (degrees: longitude and latitude in FRAME) onto a plane
with coordinates in degrees, x increasing toward increasing
longitude.  SKY-TO-PLANE and PLANE-TO-SKY take and give equatorial
RA and Dec whatever the frame.

#### `(sky-to-plane projection ra dec)`

Values X Y (degrees) of the sky position RA, DEC
(degrees); X increases toward the east (increasing RA).  NaNs where
the projection does not reach.

#### `(sky-to-plot viewport ra dec)`

Values: the :PLOT coordinates of RA, DEC through VIEWPORT's projection.

#### `stg-projection` *class*

The stereographic (STG) projection: conformal, so small shapes are
true; circles on the sky project to circles.

#### `tan-projection` *class*

The gnomonic (TAN) projection: a camera's focal plane.  Great circles
project to straight lines; it reaches less than 90 degrees out.

#### `wcs` *class*

A linear world coordinate system: FITS pixel
(CRPIX, 1-based) is at the projection's reference point, and the CD
matrix (CD1_1 CD1_2 CD2_1 CD2_2, degrees per pixel) maps pixel
offsets to the projection plane.

#### `(wcs-cd wcs)`

The cd of a WCS.

#### `(wcs-crpix wcs)`

The crpix of a WCS.

#### `(wcs-from-header header)`

A WCS from a plist of FITS keywords, e.g. (:CRVAL1 150 :CRVAL2 2.2
:CRPIX1 80.5 :CRPIX2 60.5 :CD1_1 ... ) or with :CDELT1 :CDELT2
:CROTA2, and :CTYPE1 naming the projection and frame (default
"RA---TAN"; e.g. "GLON-CAR" for a galactic map).

#### `(wcs-image-transform wcs viewport)`

How pixel centers (column, row, 0-based) of an image with WCS map to
VIEWPORT's :PLOT coordinates, for PLOT-CONTOURS's and
PLOT-FILLED-CONTOURS's :TRANSFORM (PLOT-IMAGE takes :WCS instead):
an affine when the WCS is linear in VIEWPORT's projection, else a
function of the pixel returning the plot point.

#### `(wcs-pixel-scale wcs projection i j)`

The side, in PROJECTION's plane, of a square with the area of pixel
I, J of an image with WCS.

#### `(wcs-pixel-to-plane wcs i j)`

Plane coordinates of 0-based pixel (column I, row J).

#### `(wcs-pixel-to-sky wcs i j)`

Values RA DEC (degrees) of 0-based pixel (column I, row J).

#### `(wcs-plane-transform wcs projection)`

The affine from 0-based pixel centers of an image
with WCS to the plane of PROJECTION, if WCS is exactly linear there;
else NIL (the image must be reprojected).

#### `(wcs-projection wcs)`

The projection of a WCS.

#### `(wcs-sky-to-pixel wcs ra dec)`

Values: the 0-based pixel (column, row) at RA, DEC
(NaNs where the WCS cannot see it).

#### `zea-projection` *class*

The zenithal equal area (ZEA, Lambert) projection: areas are true;
the whole sky fits in a disk.

#### `zenithal-projection` *class*

A projection about a reference point, symmetric around it: TAN,
SIN, ARC, ZEA or STG (see MAKE-PROJECTION).

## Clipping regions

#### `(apply-clip device region)`

Intersect DEVICE's clip with REGION (NIL: no change).
Scope it with WITH-SAVED-STATE, or use WITH-CLIPPING.

#### `(circle-region xc yc r &key coords round follow-skew)`

The region inside the circle PLOT-CIRCLE draws with these arguments.

#### `clipping-region` *class*

Abstract base class of clipping regions.

#### `ellipse-region` *class*

The inside of an ellipse, as ELLIPSE-REGION makes it.

#### `(ellipse-region xc yc a b &key coords angle a-scale b-scale follow-skew)`

The region inside the ellipse PLOT-ELLIPSE draws with these arguments.

#### `polygon-region` *class*

The inside of a polygon, as POLYGON-REGION makes it.

#### `(polygon-region xs ys &key coords)`

The region inside the polygon with vertices XS, YS in COORDS.

#### `rectangle-region` *class*

The inside of a rectangle, as RECTANGLE-REGION makes it.

#### `(rectangle-region x1 y1 x2 y2 &key coords)`

The region inside the rectangle with corners (X1,Y1) and (X2,Y2) in
COORDS.

#### `(region-intersection &rest regions)`

The region inside all of REGIONS.

#### `(region-outside region)`

The region outside REGION.

#### `(region-union &rest regions)`

The region inside any of REGIONS.

#### `(with-clipping (device region) &body body)` *macro*

Run BODY with drawing on DEVICE also clipped to REGION.  DEVICE may be
a viewport, entered for BODY, whose :PLOT coordinates REGION is in.

## Colors

#### `*named-colors*` *variable*

Keyword -> COLOR.

#### `color` *structure*

A color: RED, GREEN, BLUE and ALPHA (opacity), each 0..1 (see
PARSE-COLOR).

#### `(color-alpha instance)`

The alpha of a COLOR. `setf` changes it.

#### `(color-blue instance)`

The blue of a COLOR. `setf` changes it.

#### `(color-green instance)`

The green of a COLOR. `setf` changes it.

#### `(color-p object)`

True if OBJECT is a COLOR.

#### `(color-red instance)`

The red of a COLOR. `setf` changes it.

#### `(define-named-color name red green blue &optional alpha)`

Name a color: the keyword NAME then designates RED, GREEN, BLUE and
ALPHA (0..1), e.g. in WITH-COLOR.

#### `(make-color red green blue &optional alpha)`

A color of RED, GREEN, BLUE and ALPHA (opacity), each 0..1.

#### `(parse-color spec)`

Return the COLOR designated by SPEC.

## Colormaps

#### `*named-colormaps*` *variable*

The colormaps by name (keywords), as DEFINE-COLORMAP defines them.

#### `(argb-word r g b a)`

A 32-bit premultiplied ARGB word from components in 0..1.

#### `colormap` *structure*

NAME, and FUNCTION mapping a fraction 0..1 to values R G B in 0..1.

#### `(colormap-function instance)`

The function of a COLORMAP. `setf` changes it.

#### `(colormap-lut spec &optional n)`

N opaque colors sampling colormap SPEC evenly, as 32-bit ARGB words
(alpha in the top byte, as cairo's image format).

#### `(colormap-name instance)`

The name of a COLORMAP. `setf` changes it.

#### `(colormap-names)`

The names of the defined colormaps, sorted.

#### `(colormap-rgb colormap fraction)`

Values R G B of COLORMAP at FRACTION, clamped to 0..1.

#### `(define-colormap name function)`

Name a colormap made from FUNCTION (fraction -> R G B).

#### `(diverging-colormap color1 color2 &key name)`

Moreland diverging colormap from color spec COLOR1 to COLOR2.

#### `(gradient-colormap colors &key name)`

A colormap through the color specs COLORS at equal steps, linear in RGB.

#### `(make-colormap name function)`

A colormap called NAME whose FUNCTION maps a fraction 0..1 to values R G B.

#### `(parse-colormap spec)`

Return the COLORMAP designated by SPEC (see the top of this file).

#### `(reverse-colormap spec)`

The colormap SPEC (a name or colormap) running the other way, light
and dark exchanged.

#### `(table-colormap name table)`

A colormap interpolating linearly in the N x 3 array TABLE.

## Fonts, text and halos

#### `*fallback-fonts*` *variable*

Font specs tried, in order, for characters missing from a font.
Hershey "greek" has Greek letters and symbols such as ×;
"astrology" has the sun, moon, planet, node and zodiac symbols;
the system :SANS font covers the rest, e.g. Å and accented letters.

#### `*halo*` *variable*

The default halo spec for text, markers and contours (NIL: none).

#### `*hershey-space-width*` *variable*

Default word space of Hershey fonts, as a fraction of the em.  The
fonts' own space is 0.43 em, which makes text look gappy (Helvetica's
is 0.28 em); NIL keeps the font's own.

#### `*named-fonts*` *variable*

Keyword -> FONT.

#### `*named-halos*` *variable*

Keyword -> HALO.

#### `(apply-font canvas font)`

Make FONT the current font of CANVAS.

#### `(copy-font font &key size name pen-width space-width family slant weight &allow-other-keys)`

A copy of FONT with the given slots changed, e.g.
(COPY-FONT FONT :SIZE 20).

#### `(copy-halo halo &key color width opacity soft layers)`

A copy of HALO with the given parts changed.

#### `(current-font canvas)`

The font CANVAS draws text in.

#### `(define-named-font name font)`

Name a font: the keyword NAME then designates FONT, e.g. in WITH-FONT.

#### `(define-named-halo name halo)`

Name a halo: the keyword NAME then designates HALO, e.g. in WITH-HALO.

#### `(do-halo-layers (canvas spec default-width color width) &body body)` *macro*

If halo SPEC is not NIL, run BODY once per layer with COLOR and WIDTH
bound, drawing opaque halos that are blended in at the layer's
opacity.  BODY should stroke with STROKE-HALO.

#### `font` *class*

Abstract base class of fonts.

#### `(font-size font)`

Em size in canvas units.

#### `halo` *structure*

An outline around text, markers and lines (see MAKE-HALO and
WITH-HALO).

#### `(halo-color instance)`

The color of a HALO. `setf` changes it.

#### `(halo-layer-list halo default-width)`

HALO's layers as a list of (WIDTH OPACITY) in canvas units, using
DEFAULT-WIDTH if the halo has none.

#### `(halo-layers instance)`

The layers of a HALO. `setf` changes it.

#### `(halo-opacity instance)`

The opacity of a HALO. `setf` changes it.

#### `(halo-p object)`

True if OBJECT is a HALO.

#### `(halo-width instance)`

The width of a HALO. `setf` changes it.

#### `hershey-font` *class*

A Hershey stroke font, drawn as lines (see MAKE-HERSHEY-FONT).

#### `(hershey-font-name hershey-font)`

The name of a HERSHEY-FONT.

#### `(hershey-font-pen-width hershey-font)`

Stroke width as a fraction of the em, or NIL
for the cl-cairo-hershey-fonts default.

#### `(hershey-font-space-width hershey-font)`

Word space as a fraction of the em, or NIL
for *HERSHEY-SPACE-WIDTH*.

#### `(layout-text canvas text)`

Lay out TEXT in the current font.  Return a list of segments
(X STRING FONT RISE), in canvas units from the start of the
baseline, and the total width.

#### `(make-halo &key color width opacity soft layers)`

A halo of COLOR reaching WIDTH beyond the ink (NIL: by size), OPACITY
next to it.  SOFT gives the three-layer fading profile, NIL a single
crisp layer; LAYERS gives any profile instead (see the top of this
section).

#### `(make-hershey-font name &key size pen-width space-width)`

A Hershey (stroke) font NAME, such as "futural", "timesr" or
"greek", at SIZE; PEN-WIDTH is the stroke width as a fraction of the
size, SPACE-WIDTH the width of a space.

#### `(make-system-font family &key size slant weight)`

SLANT is :NORMAL, :ITALIC or :OBLIQUE; WEIGHT is :NORMAL or :BOLD.

#### `(parse-font spec)`

Return the FONT designated by SPEC.

#### `(parse-halo spec)`

The HALO designated by SPEC, or NIL (see the top of this section).

#### `(put-text canvas x y text &key halign valign angle font size halo)`

Draw TEXT (a string or runs) anchored at (X,Y), rotated by ANGLE
radians about it.  HALIGN is :LEFT, :CENTER, :RIGHT, or the fraction
of the width left of the anchor.  VALIGN is :BASELINE, :BOTTOM
(descent), :CENTER or :TOP (ascent).  FONT and SIZE, if given, apply
to this text only.  HALO outlines the text (see WITH-HALO; default
width 0.08 em).  Returns the text's width.

#### `(set-font canvas spec &key size)`

Make the font designated by SPEC current, at SIZE if given.

#### `(stroke-halo canvas color width &optional base-width)`

Stroke (and consume) the current path in COLOR, BASE-WIDTH + 2 WIDTH
wide with round joins and caps: a halo WIDTH beyond a path of
BASE-WIDTH, or beyond the edge of a filled area if BASE-WIDTH is 0.

#### `system-font` *class*

A font of the operating system, drawn by Cairo (see
MAKE-SYSTEM-FONT).

#### `(system-font-family system-font)`

The family of a SYSTEM-FONT.

#### `(system-font-slant system-font)`

The slant of a SYSTEM-FONT.

#### `(system-font-weight system-font)`

The weight of a SYSTEM-FONT.

#### `(text-size canvas text)`

Return values WIDTH ASCENT DESCENT of TEXT (a string or runs) in the
current font.  WIDTH is the advance; ASCENT and DESCENT are the
font's, so they do not vary with the characters in TEXT.

#### `(with-font (canvas spec &key size) &body body)` *macro*

Run BODY with the font designated by SPEC (at SIZE if given),
restoring the previous font afterward.

#### `(with-halo (&rest spec) &body body)` *macro*

Run BODY with halos around text, markers and contours.  SPEC is
empty (the :DEFAULT halo), MAKE-HALO keywords, as
(WITH-HALO (:COLOR :BLACK :OPACITY 0.6) ...), or one halo spec
form, as (WITH-HALO (:DARK) ...) or (WITH-HALO (*MY-HALO*) ...).

## Canvases, paths and drawing state

#### `*default-font*` *variable*

Font spec (see PARSE-FONT in fonts.lisp) a new canvas starts with.

#### `(arc canvas xc yc radius angle1 angle2)`

Arc of increasing angle (radians) from ANGLE1 to ANGLE2.  With y down,
increasing angle runs clockwise on the page.

#### `(arc-negative canvas xc yc radius angle1 angle2)`

Add a circular arc as ARC does, but from ANGLE1 to ANGLE2 in the
direction of decreasing angle.

#### `(as-canvas designator)`

The canvas DESIGNATOR stands for: a canvas itself, or
something drawn on one (a viewport stands for its device).  The
WITH-STYLE family of macros accepts any designator.

#### `canvas` *class*

Something to draw on with Cairo: a PDF, PostScript, SVG or PNG
file, or a pixel buffer (see OPEN-CANVAS).

#### `(canvas-destination canvas)`

The destination of a CANVAS.

#### `(canvas-font canvas)`

The FONT last made current by APPLY-FONT,
saved and restored with the drawing state. `setf` changes it.

#### `(canvas-height canvas)`

The height of a CANVAS.

#### `(canvas-open-p canvas)`

True until CANVAS is closed.

#### `(canvas-output-type canvas)`

The output type of a CANVAS.

#### `(canvas-resolution canvas)`

Pixels per canvas unit of a :PNG canvas.

#### `(canvas-width canvas)`

The width of a CANVAS.

#### `(char-glyph-index canvas char)`

The current font's glyph index for CHAR; 0 means it has no glyph.
(A missing glyph cannot be detected from its extents: system fonts
draw it as a box with a normal advance.)

#### `(clip canvas)`

Intersect the clip region with the current path, consuming the path.

#### `(clip-extents canvas)`

Bounding box of the clip region as values X1 Y1 X2 Y2.

#### `(clip-preserve canvas)`

Intersect the clip with the current path as CLIP does, keeping the
path.

#### `(close-canvas canvas)`

Finish the output and release the canvas.  Safe to call twice.

#### `(close-path canvas)`

Close the current subpath with a line back to its start.

#### `(current-point canvas)`

Return the current point as two values, or NIL if there is none.

#### `(curve-to canvas x1 y1 x2 y2 x3 y3)`

Cubic Bezier from the current point to (X3,Y3) with control points 1, 2.

#### `(draw-argb-image canvas pixels &key filter)`

Paint PIXELS, a (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (H W)) of premultiplied
ARGB words (see ARGB-WORD), so that pixel (row r, column c) covers
the unit square [c,c+1] x [r,r+1] of the current user space.  Set the
transform first to place it.  FILTER :NEAREST shows pixels as
squares; :BILINEAR smooths between their centers.

#### `(draw-circle canvas xc yc radius &key fill)`

Stroke, or fill if FILL, the circle of RADIUS about the canvas point
(XC,YC).

#### `(draw-line canvas x1 y1 x2 y2)`

Stroke a line between the canvas points (X1,Y1) and (X2,Y2).

#### `(draw-polygon canvas xs ys &key fill)`

Stroke, or fill if FILL, the closed polygon with vertices XS, YS.

#### `(draw-polyline canvas xs ys)`

Stroke line segments joining the points of sequences XS and YS.

#### `(draw-rectangle canvas x y width height &key fill)`

Stroke, or fill if FILL, the rectangle with corner (X,Y), WIDTH and
HEIGHT in canvas units.

#### `(draw-text canvas x y string)`

Draw STRING with its baseline origin at (X,Y).

#### `(erase-canvas canvas &optional color)`

Paint the whole of CANVAS with COLOR (default its background),
replacing what was drawn; NIL makes it transparent.

#### `(fill-path canvas)`

Fill the current path with the current color (by the fill rule), then
clear the path.

#### `(fill-preserve canvas)`

Fill the current path as FILL-PATH does, keeping the path.

#### `(fill-rule canvas)`

The current fill rule, :WINDING or :EVEN-ODD.

#### `(flush-canvas canvas)`

Complete pending drawing; for a :BUFFER canvas, update the buffer.

#### `(font-extents canvas)`

Return values ASCENT DESCENT HEIGHT of the current font.

#### `foreign-pixel-buffer` *class*

A pixel buffer in foreign memory (see MAKE-FOREIGN-PIXEL-BUFFER).

#### `(free-pixel-buffer buffer)`

Release foreign memory allocated by MAKE-FOREIGN-PIXEL-BUFFER.

#### `(in-clip-p canvas x y)`

True if the canvas point (X,Y) is inside the current clip.

#### `(line-to canvas x y)`

Add a line from the current point to the canvas point (X,Y).

#### `(line-width canvas)`

The current line width.

#### `(make-foreign-pixel-buffer width height &key format stride pointer)`

Make a pixel buffer in foreign memory.  With POINTER, use that memory
(STRIDE defaults to the minimum); otherwise allocate zeroed memory,
to be released with FREE-PIXEL-BUFFER.

#### `(make-octet-pixel-buffer width height &key format data)`

Make a pixel buffer held in a Lisp vector.  DATA, if given, must be a
(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) that CFFI can share with foreign
code (see CFFI:MAKE-SHAREABLE-BYTE-VECTOR); its contents are kept.

#### `(move-to canvas x y)`

Begin a new subpath at the canvas point (X,Y).

#### `(new-path canvas)`

Start a new, empty path, discarding the current one.

#### `(new-sub-path canvas)`

Begin a new subpath without a current point, so the next ARC does not
join the previous one.

#### `(next-page canvas)`

Emit the current page and start a new one (PDF and PS only).

#### `octet-pixel-buffer` *class*

A pixel buffer in a Lisp array of octets (see
MAKE-OCTET-PIXEL-BUFFER).

#### `(open-canvas output-type destination &key width height background font resolution class initargs)`

Open a canvas of OUTPUT-TYPE drawing to DESTINATION:
:PDF, :PS, :SVG or :PNG   DESTINATION is a filename
:BUFFER                   DESTINATION is a PIXEL-BUFFER
WIDTH and HEIGHT are in points, or pixels for :PNG; a buffer's own
size is used instead.  A :PNG or :BUFFER canvas has RESOLUTION
pixels per unit (e.g. 2 for a sharp picture on a high-resolution
screen): a PNG image is RESOLUTION times WIDTH x HEIGHT pixels, a
buffer's pixels are RESOLUTION times the canvas's units, and
everything drawn, line widths and text included, is scaled up with
it.  BACKGROUND is a color spec painted on each page, or NIL to
leave it transparent (or a buffer's contents intact).
FONT is the initial font spec.  CLASS, a subclass of CANVAS, is
instantiated with the extra INITARGS (a plist).

#### `(paint canvas)`

Fill the whole clip region with the current color.

#### `(paint-mesh canvas patches)`

Paint PATCHES as one cairo mesh gradient: each is (XS YS COLORS), four
corners in user space and their colors as (R G B A) lists, with
colors blended smoothly across the patch.  Later patches paint over
earlier ones, and adjacent patches meet without antialiasing seams.

#### `pixel-buffer` *class*

Pixels in memory that a :BUFFER canvas draws into.

#### `(pixel-buffer-data octet-pixel-buffer)`

The data of a OCTET-PIXEL-BUFFER.

#### `(pixel-buffer-format pixel-buffer)`

The format of a PIXEL-BUFFER.

#### `(pixel-buffer-height pixel-buffer)`

The height of a PIXEL-BUFFER.

#### `(pixel-buffer-pointer foreign-pixel-buffer)`

The pointer of a FOREIGN-PIXEL-BUFFER.

#### `(pixel-buffer-size buffer)`

Size of the pixel data in bytes.

#### `(pixel-buffer-stride pixel-buffer)`

The stride of a PIXEL-BUFFER.

#### `(pixel-buffer-width pixel-buffer)`

The width of a PIXEL-BUFFER.

#### `(pixel-ref buffer x y)`

Return values R G B A (0-255, as stored, i.e. premultiplied) of the
pixel at column X, row Y.  Reflects drawing up to the last
FLUSH-CANVAS or CLOSE-CANVAS.

#### `(rectangle canvas x y width height)`

Add a closed rectangle subpath with corner (X,Y) and the given WIDTH
and HEIGHT, in canvas units.

#### `(rel-line-to canvas dx dy)`

Add a line from the current point to the point DX, DY from it.

#### `(rel-move-to canvas dx dy)`

Begin a new subpath DX, DY from the current point.

#### `(reset-clip canvas)`

Remove all clipping (prefer scoping clips with WITH-CLIP).

#### `(reset-transform canvas)`

Return to the canvas's native coordinates (WIDTH x HEIGHT units,
whatever its resolution).

#### `(rotate canvas radians)`

Rotate canvas coordinates by RADIANS (clockwise on the page, as canvas
y runs down).

#### `(scale canvas sx &optional sy)`

Scale canvas coordinates by SX and SY (default SX); line widths scale
too.

#### `(set-color canvas color)`

Set the drawing color from a color spec (see PARSE-COLOR).

#### `(set-dash canvas dashes &optional offset)`

DASHES is a sequence of alternating on/off lengths; NIL or empty is solid.

#### `(set-document-metadata canvas &key title author subject keywords creator create-date)`

Set PDF document metadata (ignored for other output types).  Each is
a string.  CREATE-DATE, ISO 8601 like "2026-10-31T12:00:00Z",
defaults to the time of writing; "" omits it, which makes the
output byte-for-byte reproducible.

#### `(set-fill-rule canvas rule)`

RULE is :WINDING or :EVEN-ODD; it governs filling and clipping.

#### `(set-font-size canvas size)`

SIZE is the em size in canvas units.

#### `(set-hershey-font canvas name &key pen-width)`

Use the Hershey font NAME (e.g. "rowmans").  PEN-WIDTH, a fraction
of the em, applies to stroke fonts.

#### `(set-line-cap canvas cap)`

CAP is :BUTT, :ROUND or :SQUARE.

#### `(set-line-join canvas join)`

JOIN is :MITER, :ROUND or :BEVEL.

#### `(set-line-width canvas width)`

Set the line width, in canvas units (points for vector output, pixels
for images).

#### `(set-system-font canvas family &key slant weight)`

Use the system font FAMILY (e.g. "DejaVu Sans").  SLANT is :NORMAL,
:ITALIC or :OBLIQUE; WEIGHT is :NORMAL or :BOLD.

#### `(show-text canvas string)`

Draw STRING with its baseline origin at the current point, advancing it.

#### `(stroke canvas)`

Draw the current path's outline in the current color and line style,
then clear the path.

#### `(stroke-preserve canvas)`

Draw the current path's outline as STROKE does, keeping the path.

#### `(text-extents canvas string)`

Return values X-BEARING Y-BEARING WIDTH HEIGHT X-ADVANCE Y-ADVANCE
of STRING in the current font.

#### `(text-path canvas string)`

Add the outlines of STRING's glyphs, in the current font, to the path
at the current point (stroke fonts give the outlines of their strokes).

#### `(transform canvas xx yx xy yy x0 y0)`

Apply the affine transform x' = XX*x + XY*y + X0, y' = YX*x + YY*y + Y0
to subsequent drawing (cairo's argument order).

#### `(translate canvas dx dy)`

Move the origin of canvas coordinates by DX, DY.

#### `(with-canvas (var output-type destination &rest keys) &body body)` *macro*

Bind VAR to a canvas opened with OPEN-CANVAS, closing it afterward.

#### `(with-clip (canvas &key fill-rule) (&body path-forms) &body body)` *macro*

Run BODY clipped to the path built by PATH-FORMS.  With FILL-RULE
:EVEN-ODD, overlapping subpaths cut holes, so e.g. a canvas-sized
rectangle plus a circle clips to the outside of the circle.

#### `(with-color (canvas color) &body body)` *macro*

Run BODY with the current color set to COLOR (see PARSE-COLOR),
restoring it afterward.

#### `(with-dash (canvas dashes) &body body)` *macro*

Run BODY with the dash pattern DASHES (see SET-DASH), restoring it
afterward.

#### `(with-line-width (canvas width) &body body)` *macro*

Run BODY with the line WIDTH (canvas units), restoring it afterward.

#### `(with-opacity-group (canvas opacity) &body body)` *macro*

Draw BODY into a separate layer, then blend the layer onto CANVAS at
OPACITY (0..1), so overlapping drawing in BODY is uniformly
translucent instead of darker where it overlaps.  At OPACITY 1, BODY
is simply drawn.

#### `(with-rotation (canvas radians &key x y) &body body)` *macro*

Run BODY rotated by RADIANS about the point (X,Y).

#### `(with-saved-state (canvas) &body body)` *macro*

Run BODY, then restore color, line style, font, transformation and clip.

#### `(with-scaling (canvas sx &optional sy) &body body)` *macro*

Run BODY scaled by SX, SY.  Note that this scales line widths too.

#### `(with-style (canvas &key color line-width dash line-cap line-join) &body body)` *macro*

Run BODY with the given style settings, restoring them afterward.
Omitted settings are left unchanged.  CANVAS may be a viewport (see
AS-CANVAS).

#### `(with-translation (canvas dx dy) &body body)` *macro*

Run BODY with canvas coordinates moved by DX, DY, restoring them
afterward.

## Windows (system cl-cairoplot-gtk)

#### `*band-color*` *variable*

The color READ-CURSOR draws rubber bands in.

#### `*refresh-interval*` *variable*

Seconds between checks for new drawing to show.

#### `(close-window device)`

Close DEVICE if it is still open, and destroy its window.

#### `(device-canvas gtk-device)`

The canvas of a GTK-DEVICE.

#### `(device-window gtk-device)`

The window of a GTK-DEVICE.

#### `gtk-device` *class*

A plot device drawing into a window's canvas.

#### `(show-window device)`

Show DEVICE's window again after it was closed while still open.

#### `(cursor-demo &key size)`

Mark points by clicking in a new window, a rubber band joining the
last one to the pointer, until q or Escape is pressed (or the window
is closed).  Returns the points as a list of (X Y).

#### `(demo &key size pause)`

Draw a few panels into a new window, PAUSE seconds apart, and return
the (closed) device; its window stays up until closed.
