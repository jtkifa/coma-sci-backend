# Hershey fonts (.jhf) — original single-stroke data

Source: https://github.com/kamalmostafa/hershey-fonts (`hershey-fonts/*.jhf`,
commit 1356bf2f, 2018-01-01), which in turn took them from
http://emergent.unpythonic.net/software/hershey. See `hershey.txt` for the
original Usenet distribution notes.

## Required acknowledgements

- The Hershey Fonts were originally created by Dr. A. V. Hershey while
  working at the U. S. National Bureau of Standards.
- The format of the Font data in this distribution was originally created by
  James Hurt, Cognition, Inc., 900 Technology Park Drive, Billerica, MA 01821
  (mit-eddie!ci-dandelion!hurt)

The data may be converted into any other format except the format distributed
by the U.S. NTIS.

## Building

    (asdf:load-system "hershey-fonts/convert")
    (hershey-convert:build-all-jhf-fonts)   ; -> data/SFDBIN/<stem>.sfdbin

Fonts are registered under their file stem, e.g. `(hershey-fonts:get-font "rowmans")`.
Coordinates are scaled to the same 1000/em units as the AVHershey fonts.

## TrueType

    (asdf:load-system "hershey-fonts/ttf")
    (hershey-ttf:build-all-ttf-fonts)       ; -> data/TTF/*.ttf

For each font this writes `Hershey<Name>-Medium.ttf` (strokes thickened with
a round pen, `hershey-ttf:*pen-width*` = 56 units, the AVHershey Medium
weight) and `Hershey<Name>SingleLine-Regular.ttf` (zero-area out-and-back
paths for plotters/lasers/CAM; nearly invisible on screen). Write one font
with `(hershey-ttf:write-ttf font path :mode :stroked)` or `:single-line`.

## Fonts

| Stem | Style | Mapping |
|---|---|---|
| rowmans, futural | Roman Simplex | ASCII |
| rowmand, futuram | Roman Duplex | ASCII |
| rowmant | Roman Triplex | ASCII |
| timesr / timesrb | Roman Complex / Triplex (bold) | ASCII |
| timesi / timesib | Italic Complex / Triplex | ASCII |
| scripts / scriptc, cursive | Script Simplex / Complex | ASCII |
| gothiceng, gothicger, gothicita | Gothic English / German / Italian | ASCII |
| gothgbt, gothgrt, gothitt | Gothic triplex variants | ASCII |
| greeks / greekc | Greek Simplex / Complex | Unicode Greek |
| greek / timesg | Greek Simplex / Complex (Symbol-font layout) | Unicode Greek |
| cyrillic | Cyrillic Complex, full alphabet (no Ё; Й drawn without breve) | Unicode Cyrillic |
| cyrilc_1 | Cyrillic Complex, А–Щ only | Unicode Cyrillic |
| japanese | 26 kanji, hiragana, katakana | Unicode |
| astrology | Sun, Moon, planets, nodes, zodiac (no Libra) + italic Latin | Unicode symbols |
| markers, mathlow, mathupp, meteorology, music, symbolic | Symbols | ASCII slots (font-specific, like a Symbol font) |

Slot layouts are defined in `*jhf-layouts*` in `hershey-convert.lisp`.
