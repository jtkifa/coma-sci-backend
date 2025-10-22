

JTK  modification to yason

1. change parse-constant in parse.lisp to parse NaN to JTK-YASON:NAN,
   and Inf/Infinity to JTK-YASON:PLUS-INFINITY or MINUS-INFINITY
   if *ALLOW-NAN* is T

2. use parse-integer and parse-float in parse-number, instead of read,
   and always parse to double-float
