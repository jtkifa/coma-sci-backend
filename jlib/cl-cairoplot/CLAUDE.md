## A common plotting package for cl-cairo, modeled after cl-pgplot but hopefully cleaner



* Produce clean pdf, ps, png, svg plots using a straightforward lisp-centric interface
    * avoid code duplication
    * put the cairo dependent stuff in (maybe) just one file cairo-graphics.lisp, and construct abstractions like (draw-line ..) (set-color )  (get-font ) (write-text ) that hide the cairo-ish aspects from the rest
    * use lisp macros (with-font ..) (with-color ...) etc 
* A similar one exists in $LISP_LIB/astro/fits-stamp-cairo/cairo-plotting.lisp 
* there will be a PLOT device - with a cairo CTX, that can be OPENED and CLOSED -  this will be one lisp file
    * contains cairo context (probably abstracted in cairo-graphics.lisp)
    * have an aspect ratio 1:R defaulting to 1:1
    * have DEVICE coords from 0,1,0,1
    * have WORLD coordinates of 0,1,0,R  where R is the aspect ratio - transformation  WORLD to DEVICE is trivial
    * PLOT coordinates will be handled by a VIEWPORT object
	   *  has a linear transformation to transform DEVICE and WORLD (inline func)
	   *  has a CLIPPING-REGION - an object that constrains output to be clipped to boundaries (not necessarily rectangular) and defaulting  at first to whole plot
		   *  a CLIPPING-REGION should specify what coord system it is (WORLD, DEVICE, PLOT)
		   *  how to define clipping regions? with various cairo paths?   should have subtypes, maybe  CLIPPING-REGION, RECTANGULAR-CLIPPING-REGION, <SOME-PATH>-CLIPPING-REGION
		   * can we clip to OUTSIDE of a region?  A donut region?  a composite clipping region?  
     *   a (with-clipping clipping-region ...) macro
*   There should be a fonts.lisp file that will support both Hershey fonts ($LISP_LIB/jlib/jk-hershey) and regular system fonts, and the two should be abstracted into different font classes for similar rendering
    *   eventually, a simple escaping language for greek? and other symbols?  or a LaTex like parser? for sub/super?  or a PGPLOT like parser?    
    *   or a function (latex-string "latex-escaped-stuff") that gets turned into a unicode string
*  The file cairo-graphics should have a bunch of inline functions to abstract away the cairo stuff.     Ideally, the rest of the system doesn't even know abou cairo specifically (allows drop in replacement)
    *   with-font, with-line-width, with-color, with-rotation ... any ideas?
*  Later will work on plotting functions
    * an axis file, with axes, proper label spacing, numbers, labels, boxes  

## Examples (keep up to date)

* System `cl-cairoplot-examples` (`examples/cl-cairoplot-examples.asd`): `examples/examples.lisp` is the framework
  (`define-example`, `make-examples`, `list-examples`) and shared data; every other file in `examples/` is one example,
  named after it, with a header describing it and listing the cl-cairoplot functions it uses.  The `.asd` lists the
  files in example order.
* `(cl-cairoplot-examples:make-examples)` writes them to `example-plots/*.pdf`; `:type :png` and `:only '(name ...)` are available.
  PDFs are reproducible (no timestamp), so only examples that really change show up in git.
* The `:gtk` output type (plots live in a window) is the separate system `cl-cairoplot-gtk` in `gtk/`, so GTK is
  loaded only on request; its example is `(cl-cairoplot-gtk:demo)` in `gtk/demo.lisp`, not part of `make-examples`.
* Keep examples simple and friendly: a single plot uses the device's default viewport (`set-window d ...`, every
  call on `d`); panels pass their viewport to every call (`(plot-polyline vp ...)`, `(with-color (vp ...) ...)`,
  `(draw-box vp ...)`).  Use `with-viewport` only where it is needed (e.g. 3D surfaces drawn unclipped).
* The manual is in `docs/` (chapters in `docs/src/`, built by `docs/build.sh`, which regenerates the function
  reference from the docstrings and runs every code block, making the pictures in `docs/images/`).  Keep it up to
  date with features, give every exported function a docstring, and check that `docs/build.sh` reports
  no undocumented names, that all code blocks run, and that no code line is too wide (76 characters at most).
* **Whenever a feature is added or changed, add or update an example for it** (a new file plus a line in the `.asd`),
  rebuild with `make-examples`, and look at the output (render as PNG to inspect).

		   
			


	
