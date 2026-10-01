(in-package :endian-buffer)

(declaim (inline swap-16 swap-32 swap-64))

(defun swap-16 (val)
  (declare (type (unsigned-byte 16) val) (optimize (speed 3) (safety 0)))
  (logand #xFFFF (+ (ash (logand val #xFF) 8) (ash val -8))))

(defun swap-32 (val)
  (declare (type (unsigned-byte 32) val) (optimize (speed 3) (safety 0)))
  (logand #xFFFFFFFF
          (+ (ash (logand val #x000000FF) 24)
             (ash (logand val #x0000FF00) 8)
             (ash (logand val #x00FF0000) -8)
             (ash val -24))))

(defun swap-64 (val)
  (declare (type (unsigned-byte 64) val) (optimize (speed 3) (safety 0)))
  (logand #xFFFFFFFFFFFFFFFF
          (+ (ash (logand val #x00000000000000FF) 56)
             (ash (logand val #x000000000000FF00) 40)
             (ash (logand val #x0000000000FF0000) 24)
             (ash (logand val #x00000000FF000000) 8)
             (ash (logand val #x000000FF00000000) -8)
             (ash (logand val #x0000FF0000000000) -24)
             (ash (logand val #x00FF000000000000) -40)
             (ash val -56))))

(defconstant +host-endianness+
  #+little-endian :little
  #+big-endian :big
  #-(or little-endian big-endian)
  (cffi:with-foreign-object (ptr :uint32)
    (setf (cffi:mem-ref ptr :uint32) #x01020304)
    (if (= (cffi:mem-ref ptr :uint8) #x01) :big :little)))

(declaim (inline peek-int8 peek-uint8 poke-int8 poke-uint8
                 peek-int16 peek-uint16 poke-int16 poke-uint16
                 peek-int32 peek-uint32 poke-int32 poke-uint32
                 peek-int64 peek-uint64 poke-int64 poke-uint64
                 peek-single-float poke-single-float
                 peek-double-float poke-double-float
                 peek-string poke-string))

(defun peek-int8 (ptr) (cffi:mem-ref ptr :int8))
(defun peek-uint8 (ptr) (cffi:mem-ref ptr :uint8))
(defun poke-int8 (x ptr) (setf (cffi:mem-ref ptr :int8) x))
(defun poke-uint8 (x ptr) (setf (cffi:mem-ref ptr :uint8) x))

(defun peek-int16 (ptr target)
  (let ((raw (cffi:mem-ref ptr :uint16)))
    (if (eq target +host-endianness+)
        (cffi:mem-ref ptr :int16)
        (let ((swapped (swap-16 raw)))
          (if (logbitp 15 swapped) (- swapped #x10000) swapped)))))

(defun peek-uint16 (ptr target)
  (let ((raw (cffi:mem-ref ptr :uint16)))
    (if (eq target +host-endianness+) raw (swap-16 raw))))

(defun poke-int16 (x ptr target)
  (let ((raw (logand #xFFFF x)))
    (setf (cffi:mem-ref ptr :uint16)
          (if (eq target +host-endianness+) raw (swap-16 raw)))))

(defun poke-uint16 (x ptr target)
  (let ((raw (logand #xFFFF x)))
    (setf (cffi:mem-ref ptr :uint16)
          (if (eq target +host-endianness+) raw (swap-16 raw)))))

(defun peek-int32 (ptr target)
  (let ((raw (cffi:mem-ref ptr :uint32)))
    (if (eq target +host-endianness+)
        (cffi:mem-ref ptr :int32)
        (let ((swapped (swap-32 raw)))
          (if (logbitp 31 swapped) (- swapped #x100000000) swapped)))))

(defun peek-uint32 (ptr target)
  (let ((raw (cffi:mem-ref ptr :uint32)))
    (if (eq target +host-endianness+) raw (swap-32 raw))))

(defun poke-int32 (x ptr target)
  (let ((raw (logand #xFFFFFFFF x)))
    (setf (cffi:mem-ref ptr :uint32)
          (if (eq target +host-endianness+) raw (swap-32 raw)))))

(defun poke-uint32 (x ptr target)
  (let ((raw (logand #xFFFFFFFF x)))
    (setf (cffi:mem-ref ptr :uint32)
          (if (eq target +host-endianness+) raw (swap-32 raw)))))

(defun peek-int64 (ptr target)
  (let ((raw (cffi:mem-ref ptr :uint64)))
    (if (eq target +host-endianness+)
        (cffi:mem-ref ptr :int64)
        (let ((swapped (swap-64 raw)))
          (if (logbitp 63 swapped) (- swapped #x10000000000000000) swapped)))))

(defun peek-uint64 (ptr target)
  (let ((raw (cffi:mem-ref ptr :uint64)))
    (if (eq target +host-endianness+) raw (swap-64 raw))))

(defun poke-int64 (x ptr target)
  (let ((raw (logand #xFFFFFFFFFFFFFFFF x)))
    (setf (cffi:mem-ref ptr :uint64)
          (if (eq target +host-endianness+) raw (swap-64 raw)))))

(defun poke-uint64 (x ptr target)
  (let ((raw (logand #xFFFFFFFFFFFFFFFF x)))
    (setf (cffi:mem-ref ptr :uint64)
          (if (eq target +host-endianness+) raw (swap-64 raw)))))

(defun peek-single-float (ptr target)
  (if (eq target +host-endianness+)
      (cffi:mem-ref ptr :float)
      (let ((raw (cffi:mem-ref ptr :uint32)))
        (cffi:with-foreign-object (tmp :uint32)
          (setf (cffi:mem-ref tmp :uint32) (swap-32 raw))
          (cffi:mem-ref tmp :float)))))

(defun poke-single-float (x ptr target)
  (if (eq target +host-endianness+)
      (setf (cffi:mem-ref ptr :float) x)
      (cffi:with-foreign-object (tmp :float)
        (setf (cffi:mem-ref tmp :float) x)
        (setf (cffi:mem-ref ptr :uint32) (swap-32 (cffi:mem-ref tmp :uint32))))))

(defun peek-double-float (ptr target)
  (if (eq target +host-endianness+)
      (cffi:mem-ref ptr :double)
      (let ((raw (cffi:mem-ref ptr :uint64)))
        (cffi:with-foreign-object (tmp :uint64)
          (setf (cffi:mem-ref tmp :uint64) (swap-64 raw))
          (cffi:mem-ref tmp :double)))))

(defun poke-double-float (x ptr target)
  (if (eq target +host-endianness+)
      (setf (cffi:mem-ref ptr :double) x)
      (cffi:with-foreign-object (tmp :double)
        (setf (cffi:mem-ref tmp :double) x)
        (setf (cffi:mem-ref ptr :uint64) (swap-64 (cffi:mem-ref tmp :uint64))))))

(defun peek-string (ptr n)
  (declare (type fixnum n))
  (let ((res (make-string n :initial-element #\NUL))
        (end-found nil))
    (dotimes (i n)
      (let ((code (cffi:mem-aref ptr :uint8 i)))
        (if (= code 0) (setf end-found t))
        (unless end-found
          (setf (char res i) (code-char code)))))
    (if end-found (subseq res 0 (position #\NUL res)) res)))

(defun poke-string (x ptr n)
  (declare (type fixnum n) (type string x))
  (let ((len (length x)))
    (dotimes (i n)
      (if (< i len)
          (setf (cffi:mem-aref ptr :uint8 i) (char-code (char x i)))
          (setf (cffi:mem-aref ptr :uint8 i) 0)))))