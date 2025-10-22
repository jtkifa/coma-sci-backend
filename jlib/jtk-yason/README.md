JTK-YASON
=====

JTK-YASON is a modified version of YASON (see below).  All changes are
placed under original license.


> YASON is a Common Lisp library for encoding and decoding data in the
> [JSON](https://raw.github.com/hanshuebner/clixdoc/master/clixdoc.xsl)
> interchange format.  JSON is used as a lightweight alternative to
> XML.  YASON has the sole purpose of encoding and decoding data and
> does not impose any object model on the Common Lisp application that
> uses it.

Please proceed to the [Documentation](http://hanshuebner.github.io/yason)


Changes from YASON to JTK-YASON:

* allow parsing of :nan :plus-infinity :minus-infinity by setting  *allow-nan* ; this involved a rewrite of parse-number

* perform float validation to avoid parsing and interning eg 0.34++

* allow an external float parser by setting *yason-float-parser* to a function
that takes a buffer and returns a float

* *yason-float-type* sets the float type that YASON parses to by default, when using standard Lisp read-from-string float parser (double-float by default)

