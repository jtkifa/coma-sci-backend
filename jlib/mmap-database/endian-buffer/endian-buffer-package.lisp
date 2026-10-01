(in-package :cl-user)

(defpackage :endian-buffer
  (:use :cl :cffi)
  (:export #:+host-endianness+
           #:swap-16 #:swap-32 #:swap-64
           #:peek-int8   #:peek-uint8   #:poke-int8   #:poke-uint8
           #:peek-int16  #:peek-uint16  #:poke-int16  #:poke-uint16
           #:peek-int32  #:peek-uint32  #:poke-int32  #:poke-uint32
           #:peek-int64  #:peek-uint64  #:poke-int64  #:poke-uint64
           #:peek-single-float          #:poke-single-float
           #:peek-double-float          #:poke-double-float
           #:peek-string                #:poke-string))