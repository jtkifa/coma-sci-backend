# cl-cairo-hershey-fonts

Draw text with the Hershey fonts from `jlib/jk-hershey` (`hershey-fonts`)
in cl-cairo2, as thread-safe Cairo user fonts.

```lisp
(asdf:load-system "cl-cairo-hershey-fonts")

(cairo-hershey:use-font "rowmans" :size 40)               ; any hershey-fonts name
(cl-cairo2:move-to 20 60)
(cl-cairo2:show-text "Hello")
(cairo-hershey:use-font "rowmans" :size 40 :pen-width 0.02) ; thinner pen
```

- Stroke fonts (the 32 JHF fonts: `rowmans`, `timesr`, `scriptc`, `greekc`,
  `cyrillic`, `japanese`, `astrology`, ...) are stroked with a round pen;
  `:pen-width` is a fraction of the em (default 0.056, "Medium").
- Outline fonts (AVHershey, e.g. `SimplexMedium`) are filled.
- Text is Unicode (`"αβγ"`, `"☉♃"`); `text-extents` etc. work normally.
- Thread-safe across separate contexts/surfaces: the callbacks use no
  shared mutable state (unlike cl-cairo2's `user-font-face`).

## Future direction

Intended as the text component of a general cl-cairo2 graphing package to
replace the antiquated PGPLOT (`cl-pgplot/`). The Hershey fonts are what
PGPLOT itself uses, so plots could keep PGPLOT's look (and its Greek and
astronomical symbols) while gaining Cairo's PNG/PDF/SVG output and
antialiasing.

`astro/fits-stamp-cairo` is stable and deliberately does **not** use this
package; it keeps its toy "DejaVu Sans" fonts.
