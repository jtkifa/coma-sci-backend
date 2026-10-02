(in-package :mmapped-table)


#|
====================================================================================================
               MDBF (MAPPED DATABASE BINARY FORMAT) ENGINE - CORE STORAGE SYSTEM
====================================================================================================
Module:       mmapped-table.lisp
Author:       Database Infrastructure Core
Architecture: Memory-Mapped I/O (POSIX mmap), Zero-Copy Unrolled Macros, Bare-Metal CFFI
Concurrency:  POSIX Advisory Locking (flock) for inter-process, Recursive Mutexes for intra-thread
Integrity:    Fletcher-16 Row Checksums & Byte-Aligned Null/Presence State Flagging



==============================================================================
MMAPPED-TABLE STORAGE ENGINE - INTERFACE DOCUMENTATION
==============================================================================

CLOS ARCHITECTURE OVERVIEW:

              +-----------------------------------------+
              |          Common Lisp User Space         |
              |     (Direct Access to CLOS Instances)   |
              +--------------------+--------------------+
                                   |
             +---------------------v---------------------+
             |            CLOS Instance Layer            |
             |   (Direct Struct Slotted Memory Views)    |
             +---------------------+---------------------+
                                   |
             +---------------------v---------------------+
             |        Low-Level Pointer Layer            |
             |    (%TABLE-PEEK-MACRO / Raw Pointer Math) |
             +---------------------+---------------------+
                                   |
           +-------------------------------------------------+
           |                Memory-Mapped File               |
           |  [ Header [ Row 0  ] [ Row 1  ] ... [ Row N ] ] |
           +-------------------------------------------------+



==============================================================================
1. TABLE CREATION & LIFECYCLE
==============================================================================

* CREATE-BINARY-TABLE (db-path &key type initial-capacity)
  - Purpose: Performs sequential on-disk allocation. Generates the master
    schema control frame and creates physical zero-padded space layout 
    matching your computed stride capacity matrix.

* OPEN-BINARY-TABLE (db-path &key type read-only threadsafe verify-integrity)
  - Purpose: Maps the backing store file into system virtual space using mmap.
    Initializes internal structural schemas and maps the active pointer bases.
  - READ-ONLY: Open for reading only; uses shared file lock, no thread mutex.
  - THREADSAFE (default T): When read-write, auto-lock accessor methods for thread safety.
  - VERIFY-INTEGRITY (default T): Scan all rows and verify checksums on open.

* CLOSE-BINARY-TABLE (table)
  - Purpose: Unmaps the virtual memory frame safely, flushing remaining dirty 
cache blocks to native block storage and reclaiming OS descriptors.

* VERIFY-BINARY-TABLE-INTEGRITY (db-path &key verbose) 
   - Purpose:  Check the integrity of a binary table, and return (values T POPULATED-ROWS)
     if good, or (values NIL error-string) if bad.


EXAMPLE USAGE:
------------------------------------------------------------------------------
;; 0. Define your table schema matching your system configuration
;;    This creates: struct ALL-TYPES-TABLE-REC with accessors like
;;    MAKE-ALL-TYPES-TABLE-REC, ALL-TYPES-TABLE-REC-ID, etc.
(mmapped-table:define-binary-table all-types-table
  (id          :uint64)
  (small-idx   :int8)
  (flag        :uint8)
  (short-val   :int16)
  (port-num    :uint16)
  (sys-status  :int32)
  (counter     :uint32)
  (checksum    :int64)
  (reading-f   :single-float)
  (reading-d   :double-float)
  (serial-str  :string :length 12)
  (payload-raw :blob :length 16))

;; Optional: customize the record struct name with :record-name
;; (mmapped-table:define-binary-table (my-table :record-name my-rec) ...)

;; 1. Create a fresh binary file allocated for 50,000 rows
(mmapped-table:create-binary-table "/tmp/test.db"
                                   :type 'all-types-table
                                   :initial-capacity 50000)

;; 2. Open the binary table and perform operations
(let ((table (mmapped-table:open-binary-table "/tmp/test.db" :type 'all-types-table)))
  (unwind-protect
       (let ((rec (make-all-types-table-rec :id 42 :small-idx 1 :flag 0
                                            :short-val -100 :port-num 8080
                                            :sys-status 0 :counter 1
                                            :checksum 123456 :reading-f 3.14
                                            :reading-d 2.718d0
                                            :serial-str "TEST"
                                            :payload-raw (make-array 16 :element-type '(unsigned-byte 8)))))
         ;; Write a record to row 0
         (mmapped-table:save-row table 0 rec)

         ;; Append a new row (returns the row index)
         (mmapped-table:append-row table rec)

         ;; Read a row back (optionally reuse a struct with :fill-obj)
         (let ((loaded (mmapped-table:load-row table 0)))
           (format t "Loaded ID: ~A~%" (all-types-table-rec-id loaded))))

    ;; 3. Always clean up and safely unmap file descriptors when finished
    (mmapped-table:close-binary-table table)))
------------------------------------------------------------------------------


==============================================================================
2. CORE CLOS DATA ACCESS AND MUTATION OPERATIONS
==============================================================================

Below, OBJ is an object with the fields given above in the example,
with structure name <TABLE-TYPE>-REC, in above example ALL-TYPES-TABLE-REC.

For example, (make-all-table-rec :id 99 :flag 12 :short-val -1 :port-num  88 :sys-status 1234
                                  .... ) ;; etc


Under the standardized CLOS layout pattern, structural operations use 
polymorphic generic functions that adapt cleanly to the target schema class types.

* APPEND-ROW (table obj)
  - Purpose: Advanced fast-path append tool that handles dynamic ingestion.
  - Mechanics:
      1. Inspects the master header metadata of the table to fetch the 
         current high-water mark for active populated records.
      2. Uses the current active row count as the destination index.
      3. Passes execution down to SAVE-ROW internally to commit the properties 
         of the CLOS object.
      4. Increments the master header row tracker on disk and returns the 
         newly assigned absolute row index slot.

* SAVE-ROW (table row-idx obj)
  - Purpose: Commits a CLOS record instance directly to the raw pointer layout
    at a target index location within the file mapping.
  - Mechanics:
      1. Performs a strict boundary guard check to guarantee that row-idx
         does not break past the maximum allocated capacity bounds.
      2. Evaluates the unrolled slot maps of the CLOS object, unboxing the values
         and blitting them down to raw memory locations via unrolled pre-compiled
         byte offsets.
      3. Calculates an internal fletcher16 error correction value over the
         modified binary chunk and packs it inside the tail validation segment.
      4. Sets the status flag to 1 (valid).

* NULLIFY-ROW (table row-idx)
  - Purpose: Securely delete a row by zeroing all data and status flag.
  - Mechanics:
      1. Zeros the entire row (data, status flag, and checksum).
      2. LOAD-ROW will return NIL for this row.
      3. Use SAVE-ROW to write new valid data to this row index.

* LOAD-ROW (table row-idx &key fill-obj)
  - Purpose: Pulls data out of a specific memory-mapped row into a clean, 
    manipulable CLOS instance wrapper.
  - Mechanics:
      1. Enforces strict read security by asserting that row-idx is cleanly less 
         than the current active record high-water mark.
      2. Scans the validation status indicator at the tail layout of the block 
         to make sure the row contains valid initialized data.
      3. Computes a cyclic fletcher16 verification sequence against the raw 
         bytes, comparing it directly to the stored checksum matrix to eliminate 
         unnoticed data corruption.
      4. Translates raw little-endian bytes directly into CLOS slot values using 
         native pointer arithmetic. If a reusable object is passed via :fill-obj, 
         the slots are written completely in-place, reducing Garbage 
         Collection allocation costs.   BLOBs are re-filled but 
         strings are created afresh for CFFI reasons (and unknown length).
==============================================================================

================================================================
MMAPPED-TABLE STORAGE ENGINE - INTERNAL DETAILS
================================================================


----------------------------------------------------------------------------------------------------
1. ARCHITECTURAL OVERVIEW
----------------------------------------------------------------------------------------------------
This module implements a hyper-performance, zero-allocation database engine built directly on top
of POSIX memory-mapped files (`mmap`). By binding raw memory pages directly into the process's 
virtual address space, the operating system kernel handles paging, disk synchronization, and 
caching automatically. Data hydration handles record mutations by reading and writing straight 
to foreign memory pointers, bypassing standard Lisp Garbage Collection overhead.

----------------------------------------------------------------------------------------------------
2. BINARY STORAGE FILE LAYOUT (MDBF SPECIFICATION)
----------------------------------------------------------------------------------------------------
All multi-byte numeric values in the file header are encoded in Network Byte Order (LITTLE-ENDIAN).
The file architecture is strictly structured into two primary regions: The File Header and the 
Contiguous Row Data Strides.

A. THE HEADER SCHEMA (Offset 0 to H-SIZE)
   - Bytes 0-3  : Magic Signature -> Always 4 bytes ASCII: "MDBF"
   - Byte 4     : Layout Format Version -> Currently 1 (uint8)
   - Bytes 5-8  : Max Allocated Rows Capacity -> (uint32, Little-Endian)
   - Bytes 9-12 : Active/Populated Row Count   -> (uint32, Little-Endian)
   - Bytes 13-14: Column Count Descriptor      -> (uint16, Little-Endian)
   - Bytes 15+  : Column Definition Array (4 bytes per column definition block):
                  * Byte 0  : Type ID Descriptor (e.g., 1=:int8, 2=:uint8, 11=:string, etc.)
                  * Byte 1  : Reserved Flags -> Set to #x00
                  * Bytes 2-3: Length Parameter -> (uint16, Little-Endian). Used for Strings/Blobs.

B. CONTIGUOUS ROW DATA STRIDES (Offset H-SIZE onwards)
   Rows are laid out back-to-back. The size of an individual row (The Stride) is calculated at 
   compile-time by summing the user's defined data columns AND automatically appending an implicit 
   3-byte safety tail.

   [ ... User-Defined Columns ... ] [ Implicit Status Flag ] [ Implicit Checksum Slot ]
   |<------ DATA-LENGTH ---------->|<- 1 Byte (:uint8) ->|<- 2 Bytes (:uint16) ->|
   |<--------------------------- TOTAL STRIDE ---------------------------------->|

----------------------------------------------------------------------------------------------------
3. ADVANCED INTEGRITY & STATE ENGINE MECHANICS
----------------------------------------------------------------------------------------------------
To achieve high resilience without sacrificing microsecond execution speeds, this engine implements
two internal runtime safety firewalls directly into the unrolled `load-*` and `save-*` pipelines:

A. ROW STATE FLAGGING (NULL / UNSET RECORD PROTECTION)
   Because raw binary files have no native concept of "nothing" (unallocated spaces default to #x00),
   the engine tracks empty states via an implicit trailing `status-offset` byte.
   - Status #x00 (NULL): The row is empty or deleted. The generated `load-<NAME>-row` routine will
     instantly short-circuit and return pure Lisp NIL without instantiating objects or reading garbage.
   - Status #x01 (SET) : The row contains live data. The reader proceed to process column layout offsets.

B. FAST IN-PLACE FLETCHER-16 CHECKSUMMING
   To catch bit-rot, unbacked hardware pages, or localized data corruption, an optimized 16-bit
   Fletcher checksum is processed across the `DATA-LENGTH` section of the row upon save. 
   During record loads, the checksum is recalculated over the foreign pointer segment using a heavily
   optimized bit-shifted "End-Around Carry" loop to bypass heavy hardware `mod` division blocks. 
   If the freshly calculated checksum does not match the little-endian uint16 stored in the row's tail, 
   the engine halts execution and throws a hard Lisp condition before corrupt data hits downstream apps.

----------------------------------------------------------------------------------------------------
4. CONCURRENCY: MULTI-PROCESS & MULTI-THREAD SAFETY
----------------------------------------------------------------------------------------------------

A. INTER-PROCESS LOCKING (flock)
Operating system virtual memory mappings are highly volatile if external processes truncate files
while active connections are held (triggering unbacked memory reads and fatal `SIGBUS` hardware crashes).
To guarantee data safety, this module enforces intent-based POSIX File Locking (`flock`):
- Read-Only Mode : Acquires a Shared Lock (`LOCK_SH | LOCK_NB`). Infinite parallel readers are permitted.
- Read-Write Mode: Acquires an Exclusive Lock (`LOCK_EX | LOCK_NB`). Only one process can modify the file.
Any collision or locking failure aborts gracefully via clean Lisp errors rather than system crashes.

B. INTRA-PROCESS THREAD LOCKING (mutex)
For multi-threaded access within a single process, the table object contains a recursive mutex.
Thread locking behavior is controlled by the :THREADSAFE and :READ-ONLY options to OPEN-BINARY-TABLE:

- READ-ONLY=T: No thread mutex used. Multiple reader threads are inherently safe since no writes occur.
- READ-ONLY=NIL, THREADSAFE=T (default): All accessor methods (load-row, save-row, append-row,
  nullify-row) automatically acquire the mutex. Use WITH-TABLE-LOCKED to batch multiple operations
  under a single lock acquisition (the recursive lock permits nested acquisition).
- READ-ONLY=NIL, THREADSAFE=NIL: No automatic mutex locking. User takes responsibility for ensuring
  single-threaded access or providing external synchronization. Use for maximum performance when
  thread safety is guaranteed by application design.

----------------------------------------------------------------------------------------------------
5. CRITICAL PERFORMANCE WARNINGS & COMPILER USE
----------------------------------------------------------------------------------------------------
- BARE-METAL MACROS vs RUNTIME ROUTINES:
  The internal macros `%table-peek-macro` and `%table-poke-macro` are stripped entirely of type-checking
  and safety logic to emit bare-metal machine loops. They DO NOT execute Fletcher checksum validation or
  NULL-row checks. For production data pipelines, always utilize the compiler-generated wrappers:
  `load-<NAME>-row` and `save-<NAME>-row`.
  
- COMPILER OPTIMIZATION NOTES:
  The integrity loops depend entirely on strict compile-time declarations. Modifying optimization levels
  or removing type declarations (`type fixnum`, `type cffi:foreign-pointer`) will cause the compiler
  to fall back to generic Lisp type handlers, reducing read performance significantly.
====================================================================================================




================================================================
PERFORMANCE NOTES
================================================================


Tested about 3x faster than SQLITE for table creation, and 7x faster
for retrieval.  However, does not support any querying capability besides
row index.




|#

;; --- Foreign System Function Bindings ---
(cffi:defcfun ("open" posix-open) :int (pathname :string) (flags :int) (mode :int))
(cffi:defcfun ("ftruncate" posix-ftruncate) :int (fd :int) (length :long))
(cffi:defcfun ("mmap" posix-mmap) :pointer (addr :pointer) (length :size) (prot :int) (flags :int) (fd :int) (offset :long))
(cffi:defcfun ("munmap" posix-munmap) :int (addr :pointer) (length :size))
(cffi:defcfun ("close" posix-close) :int (fd :int))

;; --- Native POSIX File Locking Bindings ---
(cffi:defcfun ("flock" posix-flock) :int (fd :int) (operation :int))

;; --- Compile-Time Type Mechanics ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-keyword-to-id (type)
    (case type
      (:int8 1)  (:uint8 2)  (:int16 3) (:uint16 4)
      (:int32 5) (:uint32 6) (:int64 7) (:uint64 8)
      (:single-float 9) (:double-float 10) (:string 11)
      (:blob 12)
      (t (error "Unknown binary type: ~A" type))))
  
  (defun type-id-to-size (id &optional param)
    (case id
      ((1 2) 1) ((3 4) 2) ((5 6 9) 4) ((7 8 10) 8)
      ((11 12) param)
      (t (error "Invalid Type ID: ~A" id)))))

;; --- Constants ---
;; Use eval-when + boundp guard to avoid SBCL redefinition error on reload:
;; cffi pointers are not EQL across loads even with same address.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +map-failed+
    (if (boundp '+map-failed+)
        (symbol-value '+map-failed+)
        (cffi:make-pointer #xFFFFFFFFFFFFFFFF))))

;; ====================================================================================================
;;               CLOS METACLASS CONTROL PLANE (No Instances, Pure Schema Blueprint)
;; ====================================================================================================
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass binary-table-class (c2mop:standard-class)
    ((db-fields      :initarg :db-fields      :accessor table-class-fields      :initform nil)
     (db-stride      :initarg :db-stride      :accessor table-class-stride      :initform 0)
     (db-data-length :initarg :db-data-length :accessor table-class-data-length :initform 0)
     (db-header-size :initarg :db-header-size :accessor table-class-header-size :initform 0)
     (db-loader-fn   :initarg :db-loader-fn   :accessor table-class-loader-fn   :initform nil)
     (db-saver-fn    :initarg :db-saver-fn    :accessor table-class-saver-fn    :initform nil)
     (db-constructor :initarg :db-constructor :accessor table-class-constructor :initform nil))
    (:documentation "Custom metaclass that embeds physical storage specifications directly into the class object."))

  (defmethod c2mop:validate-superclass ((class binary-table-class) (superclass c2mop:standard-class))
    t)

  (defmethod initialize-instance :around ((class binary-table-class) &rest initargs
					  &key db-fields db-stride db-data-length db-header-size
					  db-loader-fn db-saver-fn db-constructor
					  &allow-other-keys)
    (let ((clean-fields (if (and (listp db-fields)
				 (listp (car db-fields))
				 (eq (caar db-fields) 'quote))
			    (cadr (car db-fields)) db-fields))
          (clean-stride (if (listp db-stride) (car db-stride) db-stride))
          (clean-data-length (if (listp db-data-length) (car db-data-length) db-data-length))
          (clean-header (if (listp db-header-size) (car db-header-size) db-header-size)))
      (apply #'call-next-method class
             :db-fields clean-fields
             :db-stride clean-stride
             :db-data-length clean-data-length
             :db-header-size clean-header
             :db-loader-fn db-loader-fn
             :db-saver-fn db-saver-fn
             :db-constructor db-constructor
             initargs)))

  (defmethod reinitialize-instance :around ((class binary-table-class)
					    &rest initargs
					    &key db-fields db-stride db-data-length db-header-size
					    db-loader-fn db-saver-fn db-constructor
					    &allow-other-keys)
    (let ((clean-fields (if (and (listp db-fields)
				 (listp (car db-fields))
				 (eq (caar db-fields) 'quote))
			    (cadr (car db-fields)) db-fields))
          (clean-stride (if (listp db-stride) (car db-stride) db-stride))
          (clean-data-length (if (listp db-data-length) (car db-data-length) db-data-length))
          (clean-header (if (listp db-header-size) (car db-header-size) db-header-size)))
      (apply #'call-next-method class
             :db-fields clean-fields
             :db-stride clean-stride
             :db-data-length clean-data-length
             :db-header-size clean-header
             :db-loader-fn db-loader-fn
             :db-saver-fn db-saver-fn
             :db-constructor db-constructor
             initargs))))

(defun get-table-meta (class-symbol)
  (let* ((resolved-symbol (if (eq (symbol-package class-symbol) (find-package :keyword))
                              (find-symbol (symbol-name class-symbol) *package*)
                              class-symbol))
         (class (find-class (or resolved-symbol class-symbol) nil)))
    (unless (typep class 'binary-table-class)
      (error "Type ~S (resolved to ~S) is not a valid defined binary table class."
             class-symbol resolved-symbol))
    class))

;; --- Runtime Session Object ---
(defstruct mapped-table
  fd
  base-ptr
  mapped-length
  schema-symbol
  file-path
  (read-only-p nil)
  (threadsafe-p t)
  (io-lock (bt:make-recursive-lock "table-io-lock")))

(defmacro with-table-locked ((table) &body body)
  "Explicitly acquire the table's mutex for a batch of operations.
Uses a recursive lock, so nested calls (including from accessor methods) are safe."
  `(bt:with-recursive-lock-held ((mapped-table-io-lock ,table))
     ,@body))

(defmacro with-table-maybe-locked ((table) &body body)
  "Acquire the table's mutex only if the table is read-write AND threadsafe.
Read-only tables never lock (no writes possible).
Tables opened with :threadsafe nil never lock (user takes responsibility)."
  (let ((tbl (gensym "TABLE")))
    `(let ((,tbl ,table))
       (if (and (not (mapped-table-read-only-p ,tbl))
                (mapped-table-threadsafe-p ,tbl))
           (bt:with-recursive-lock-held ((mapped-table-io-lock ,tbl))
             ,@body)
           (progn ,@body)))))

;; An empty blob object for initializing structs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *empty-blob* (make-array 0 :element-type '(unsigned-byte 8))))

;; ====================================================================================================
;;                                    UNIFIED PARSING MATH
;; ====================================================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Row integrity tail: 1 byte status flag + 2 byte Fletcher-16 checksum
  (defconstant +row-integrity-tail-size+ 3)

  (defun %compute-schema-from-arg-fields (arg-fields)
    "Transforms raw macro field specifications into an organized plist blueprint.
Returns (values field-schema data-length total-stride) where:
  - field-schema: list of field plists with :name, :type-id, :param, :offset, :size
  - data-length: size of user data portion (before integrity tail)
  - total-stride: full row size including integrity tail (data-length + 3)"
    (let ((current-offset 0)
          (schema-accumulator '()))
      (dolist (field arg-fields)
        (let* ((name (first field))
               (spec (cdr field))
               (type-raw (if (listp spec) (first spec) spec))
               (param (if (listp spec) (getf (cdr spec) :length 0) 0))
               (type-id (type-keyword-to-id type-raw))
               (size (if (member type-raw '(:string :blob))
                         param
                         (type-id-to-size type-id))))
          (push (list :name name
                      :type-id type-id
                      :param param
                      :offset current-offset
                      :size size)
                schema-accumulator)
          (incf current-offset size)))
      ;; Return schema, data-length, and total stride (data + integrity tail)
      (values (nreverse schema-accumulator)
              current-offset
              (+ current-offset +row-integrity-tail-size+)))))



;; ====================================================================================================
;;                        INTERNAL PERFORMANCE LOGIC (Fletcher-16 Checksum Processing Engine)
;; ====================================================================================================
(declaim (inline %compute-fletcher16-ptr))
(defun %compute-fletcher16-ptr (ptr data-length)
  "Computes a standard 16-bit Fletcher checksum over a raw foreign pointer boundary space."
  (declare (type cffi:foreign-pointer ptr)
           (type fixnum data-length)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((sum1 0)
        (sum2 0))
    (declare (type fixnum sum1 sum2)) ; Shift to fixnums for ultra-lean bit masking
    (dotimes (i data-length)
      (declare (type fixnum i))
      ;; Accumulate raw byte values directly
      (setf sum1 (+ sum1 (cffi:mem-aref ptr :uint8 i))
            sum2 (+ sum2 sum1))
      
      ;; Periodically or instantly fold overflow blocks to prevent integer littlenum promotion
      ;; This mimics modulo 255 at a fraction of the hardware cost
      (setf sum1 (+ (logand sum1 #xff) (ash sum1 -8))
            sum2 (+ (logand sum2 #xff) (ash sum2 -8))))
    
    ;; Final reduction pass to completely clear any lingering overflow bits
    (setf sum1 (+ (logand sum1 #xff) (ash sum1 -8))
          sum2 (+ (logand sum2 #xff) (ash sum2 -8)))
    
    ;; Handle the rare edge case where the sum reduces to exactly 255, matching true modulo rules
    (when (= sum1 255) (setf sum1 0))
    (when (= sum2 255) (setf sum2 0))
    
    (logior (ash sum2 8) sum1)))


;; ====================================================================================================
;;                        TOP-LEVEL SYNTAX ORCHESTRATION LAYER
;; ====================================================================================================

;; --- Unified Polymorphic Database Control Plane ---

(defgeneric load-row (table row-idx &key fill-obj)
  (:documentation "The universal reading protocol for all memory-mapped data files.
   Bypasses object instantiation if a pre-allocated structural buffer is provided via :fill-obj."))

(defgeneric save-row (table row-idx obj)
  (:documentation "Write a struct to a row in a memory-mapped table.
Calculates and stamps Fletcher-16 checksum, sets status flag to valid (1)."))

(defgeneric append-row (table struct-obj)
  (:documentation "Appends a runtime struct record straight into the mapped table, resizing if necessary."))

(defgeneric nullify-row (table row-idx)
  (:documentation "Securely delete a row by zeroing all data and setting status to 0.
After nullification, load-row returns NIL for this row.
Use save-row to write new valid data to this row index."))

;; --- Generic Method Dispatchers ---
;; These methods look up loader/saver functions from the table's metaclass

(defmethod load-row ((table mapped-table) row-idx &key fill-obj)
  (with-table-maybe-locked (table)
    (let* ((meta (get-table-meta (mapped-table-schema-symbol table)))
           (loader-fn (table-class-loader-fn meta)))
      (unless loader-fn
        (error "No loader function registered for table type ~A" (mapped-table-schema-symbol table)))
      (funcall loader-fn table row-idx fill-obj))))

(defmethod save-row ((table mapped-table) row-idx obj)
  (when (mapped-table-read-only-p table)
    (error "Cannot save-row: table ~A is opened read-only" (mapped-table-file-path table)))
  (with-table-maybe-locked (table)
    (let* ((meta (get-table-meta (mapped-table-schema-symbol table)))
           (saver-fn (table-class-saver-fn meta)))
      (unless saver-fn
        (error "No saver function registered for table type ~A" (mapped-table-schema-symbol table)))
      (funcall saver-fn table row-idx obj))))

(defmethod nullify-row ((table mapped-table) row-idx)
  "Securely delete a row by zeroing all data and setting status flag to 0."
  (when (mapped-table-read-only-p table)
    (error "Cannot nullify-row: table ~A is opened read-only" (mapped-table-file-path table)))
  (with-table-maybe-locked (table)
    (let* ((meta (get-table-meta (mapped-table-schema-symbol table)))
           (header-size (table-class-header-size meta))
           (stride (table-class-stride meta))
           (max-rows (get-header-max-rows table)))
      (when (>= row-idx max-rows)
        (error "Out of Bounds: row ~A exceeds max ~A" row-idx max-rows))
      (let* ((base-ptr (mapped-table-base-ptr table))
             (row-ptr (cffi:inc-pointer base-ptr (+ header-size (* row-idx stride)))))
        ;; Zero out entire row (data + status + checksum)
        (dotimes (i stride)
          (setf (cffi:mem-ref (cffi:inc-pointer row-ptr i) :uint8) 0)))
      t)))


(defmacro define-binary-table (name-spec &body fields)
  "Defines the layout and runtime access interfaces for a memory-mapped binary table.

NAME-SPEC can be either:
  - A symbol (table name), e.g., MY-TABLE
  - A list with options: (MY-TABLE &key record-name)
    :record-name - customize the struct name (default: <NAME>-REC)

Access the table using generic methods:
  (load-row table row-idx &key fill-obj) - Read a row into a struct (NIL if nullified)
  (save-row table row-idx obj)           - Write a struct to a row
  (append-row table obj)                 - Append a new row
  (nullify-row table row-idx)            - Soft-delete a row (load-row returns NIL)

Each row includes a 3-byte integrity tail:
  - 1 byte status flag (0=nullified/empty, 1=valid)
  - 2 byte Fletcher-16 checksum over the data portion

Example:
  (define-binary-table (my-table myt)  ;; MYT defaults to MY-TABLE-REC
        (i8      :int8)
        (u8      :uint8)
        (i16     :int16)
        (u16    :uint16)
        (i32     :int32)
        (u32    :uint32)
        (i64     :int64)
        (u64    :uint64)
        (flt     :single-float)
        (dbl       :double-float)
        (str  :string :length 12)
        (blb :blob :length 16))

creates table type MY-TABLE, and a struct MYT, with accessors MYT-I8, MYT-U8 ... MYT-BLB."
  ;; Parse name-spec to extract name and options
  (let* ((name (if (listp name-spec) (car name-spec) name-spec))
         (options (if (listp name-spec) (cdr name-spec) nil))
         (record-name-opt (getf options :record-name)))
    (multiple-value-bind (computed-fields data-length total-stride)
        (%compute-schema-from-arg-fields fields)
      (let* ((header-size (+ 15 (* 4 (length fields))))
             (base-string (symbol-name name))
             (struct-name (or record-name-opt
                              (intern (format nil "~A-REC" base-string) *package*)))
             (constructor (intern (format nil "MAKE-~A" (symbol-name struct-name)) *package*))

             (struct-slots (mapcar #'(lambda (f)
                                       (let* ((f-name (getf f :name))
                                              (type-id (getf f :type-id))
                                              (f-type (case type-id
                                                        (1 '(signed-byte 8)) (2 '(unsigned-byte 8))
                                                        (3 '(signed-byte 16)) (4 '(unsigned-byte 16))
                                                        (5 '(signed-byte 32)) (6 '(unsigned-byte 32))
                                                        (7 '(signed-byte 64)) (8 '(unsigned-byte 64))
                                                        (9 'single-float) (10 'double-float) (11 'string)
                                                        (12 '(simple-array (unsigned-byte 8) (*)))))
                                              (f-default (case type-id
                                                           (9 0.0f0) (10 0.0d0) (11 "")
                                                           (12 *empty-blob*) (otherwise 0))))
                                         `(,f-name ,f-default :type ,f-type)))
                                   computed-fields))
             ;; Build slot accessors list for use in lambdas
             (slot-accessors (mapcar #'(lambda (f)
                                         (let ((f-name (getf f :name)))
                                           (intern (format nil "~A-~A" (symbol-name struct-name) (symbol-name f-name)) *package*)))
                                     computed-fields)))
        ;; Generate code for loader and saver lambdas that will be stored in metaclass
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (defclass ,name ()
               ()
               (:metaclass binary-table-class)
               (:db-fields ',computed-fields)
               (:db-stride ,total-stride)
               (:db-data-length ,data-length)
               (:db-header-size ,header-size)))

           (defstruct ,struct-name ,@struct-slots)

           ;; Store accessor functions in the metaclass
           ;; LOADER: verifies checksum and status flag, returns NIL for nullified rows
           (setf (table-class-loader-fn (find-class ',name))
                 (lambda (table row-idx fill-obj)
                   (declare (optimize speed)
                            (type fixnum row-idx))
                   (block load-row-body
                     (let ((max-rows (get-header-max-rows table)))
                       (declare (type fixnum max-rows))
                       (when (>= row-idx max-rows)
                         (error "Out of Bounds: row ~A exceeds max ~A" row-idx max-rows)))
                     (let* ((base-ptr (mapped-table-base-ptr table))
                            (row-ptr (cffi:inc-pointer base-ptr (+ ,header-size (* row-idx ,total-stride)))))
                       (declare (type cffi:foreign-pointer base-ptr row-ptr))
                       ;; Check status flag (at offset data-length)
                       (let ((status-flag (cffi:mem-ref (cffi:inc-pointer row-ptr ,data-length) :uint8)))
                         (when (zerop status-flag)
                           ;; Row is nullified/empty - return NIL
                           (return-from load-row-body nil))
                         ;; Verify Fletcher-16 checksum - doesn't consume much time
                         (let* ((stored-checksum (endian-buffer:peek-uint16
                                                  (cffi:inc-pointer row-ptr ,(+ data-length 1)) :little))
                                (computed-checksum (%compute-fletcher16-ptr row-ptr ,data-length)))
                           (unless (= stored-checksum computed-checksum)
                             (error "Checksum mismatch at row ~A: stored=~4,'0X computed=~4,'0X"
                                    row-idx stored-checksum computed-checksum))))
                       ;; Read field values (all multi-byte types use little-endian for portability)
                       (let ((obj (or fill-obj (,constructor))))
                         ,@(mapcar #'(lambda (f accessor)
                                       (let* ((offset (getf f :offset))
                                              (type-id (getf f :type-id))
                                              (param (getf f :param))
                                              (field-ptr `(cffi:inc-pointer row-ptr ,offset)))
                                         `(setf (,accessor obj)
                                                ,(case type-id
                                                   ;; Single-byte: no endianness concern
                                                   (1 `(endian-buffer:peek-int8 ,field-ptr))
                                                   (2 `(endian-buffer:peek-uint8 ,field-ptr))
                                                   ;; Multi-byte integers: explicit little-endian
                                                   (3 `(endian-buffer:peek-int16 ,field-ptr :little))
                                                   (4 `(endian-buffer:peek-uint16 ,field-ptr :little))
                                                   (5 `(endian-buffer:peek-int32 ,field-ptr :little))
                                                   (6 `(endian-buffer:peek-uint32 ,field-ptr :little))
                                                   (7 `(endian-buffer:peek-int64 ,field-ptr :little))
                                                   (8 `(endian-buffer:peek-uint64 ,field-ptr :little))
                                                   ;; Floats: explicit little-endian
                                                   (9 `(endian-buffer:peek-single-float ,field-ptr :little))
                                                   (10 `(endian-buffer:peek-double-float ,field-ptr :little))
                                                   ;; String and blob: byte-level access
                                                   (11 `(endian-buffer:peek-string ,field-ptr ,param))
                                                   (12 `(let ((res (make-array ,param :element-type '(unsigned-byte 8))))
                                                          (dotimes (i ,param res)
                                                            (setf (aref res i) (cffi:mem-aref ,field-ptr :uint8 i)))))))))
                                   computed-fields slot-accessors)
                         obj)))))

           ;; SAVER: writes data, computes checksum, sets status flag to valid (1)
           (setf (table-class-saver-fn (find-class ',name))
                 (lambda (table row-idx obj)
                   (declare (optimize speed)
                            (type fixnum row-idx))
                   (let ((max-rows (get-header-max-rows table)))
                     (declare (type fixnum max-rows))
                     (when (>= row-idx max-rows)
                       (error "Out of Bounds: row ~A exceeds max ~A" row-idx max-rows)))
                   (let* ((base-ptr (mapped-table-base-ptr table))
                          (row-ptr (cffi:inc-pointer base-ptr (+ ,header-size (* row-idx ,total-stride)))))
                     (declare (type cffi:foreign-pointer base-ptr row-ptr))
                     ;; Write field values (all multi-byte types use little-endian for portability)
                     ,@(mapcar #'(lambda (f accessor)
                                   (let* ((offset (getf f :offset))
                                          (type-id (getf f :type-id))
                                          (param (getf f :param))
                                          (field-ptr `(cffi:inc-pointer row-ptr ,offset)))
                                     (case type-id
                                       ;; Single-byte: no endianness concern
                                       (1 `(endian-buffer:poke-int8 (,accessor obj) ,field-ptr))
                                       (2 `(endian-buffer:poke-uint8 (,accessor obj) ,field-ptr))
                                       ;; Multi-byte integers: explicit little-endian
                                       (3 `(endian-buffer:poke-int16 (,accessor obj) ,field-ptr :little))
                                       (4 `(endian-buffer:poke-uint16 (,accessor obj) ,field-ptr :little))
                                       (5 `(endian-buffer:poke-int32 (,accessor obj) ,field-ptr :little))
                                       (6 `(endian-buffer:poke-uint32 (,accessor obj) ,field-ptr :little))
                                       (7 `(endian-buffer:poke-int64 (,accessor obj) ,field-ptr :little))
                                       (8 `(endian-buffer:poke-uint64 (,accessor obj) ,field-ptr :little))
                                       ;; Floats: explicit little-endian
                                       (9 `(endian-buffer:poke-single-float (,accessor obj) ,field-ptr :little))
                                       (10 `(endian-buffer:poke-double-float (,accessor obj) ,field-ptr :little))
                                       ;; String and blob: byte-level access
                                       (11 `(endian-buffer:poke-string (,accessor obj) ,field-ptr ,param))
                                       (12 `(let ((val (,accessor obj)))
                                              (dotimes (i ,param)
                                                (setf (cffi:mem-aref ,field-ptr :uint8 i) (aref val i))))))))
                               computed-fields slot-accessors)
                     ;; Compute and write Fletcher-16 checksum over data portion
                     (let ((checksum (%compute-fletcher16-ptr row-ptr ,data-length)))
                       (endian-buffer:poke-uint16 checksum
                                                  (cffi:inc-pointer row-ptr ,(+ data-length 1))
                                                  :little))
                     ;; Write status flag = 1 (valid)
                     (setf (cffi:mem-ref (cffi:inc-pointer row-ptr ,data-length) :uint8) 1)
                     t)))

           (setf (table-class-constructor (find-class ',name))
                 #',constructor)

           ',name)))))


(defmethod append-row ((table mapped-table) struct-obj)
  (when (mapped-table-read-only-p table)
    (error "Cannot append-row: table ~A is opened read-only" (mapped-table-file-path table)))
  (with-table-maybe-locked (table)
    (let ((current-count (get-header-row-count table))
          (max-capacity (get-header-max-rows table)))
      (when (>= current-count max-capacity)
        (resize-mapped-table table (+ max-capacity 100)))
      (save-row table current-count struct-obj)
      (set-header-row-count table (1+ current-count))
      current-count)))


;; --- Metadata Header Trackers ---
(defun get-header-max-rows (table)
  (endian-buffer:peek-uint32 (cffi:inc-pointer (mapped-table-base-ptr table) 5) :little))

(defun set-header-max-rows (table val)
  (endian-buffer:poke-int32 val (cffi:inc-pointer (mapped-table-base-ptr table) 5) :little))

(defun get-header-row-count (table)
  (endian-buffer:peek-uint32 (cffi:inc-pointer (mapped-table-base-ptr table) 9) :little))

(defun set-header-row-count (table val)
  (endian-buffer:poke-int32 val (cffi:inc-pointer (mapped-table-base-ptr table) 9) :little))

;; ====================================================================================================
;;                        STORAGE ENGINE CORE OPERATIONS
;; ====================================================================================================
(defun create-binary-table (path &key type (initial-capacity 10))
  "Create a binary table file for a table TYPE created using DEFINE-BINARY-TABLE

Example:

  (create-binary-table \"/tmp/test.db\"  :type 'my-table-type :initial-capacity 50000)
"
  (let* ((meta (get-table-meta type))
         (fields (table-class-fields meta))
         (stride (table-class-stride meta))
         (h-size (table-class-header-size meta))
         (total-bytes (+ h-size (* initial-capacity stride)))
         (fd (posix-open path (logior o-rdwr o-creat) #o644)))
    (when (< fd 0) (error "Failed to create/open file: ~A" path))
    (posix-ftruncate fd total-bytes)
    (let ((ptr (posix-mmap (cffi:null-pointer) total-bytes (logior prot-read prot-write) map-shared fd 0)))
      (when (cffi:pointer-eq ptr +map-failed+)
        (posix-close fd)
        (error "mmap operation failed during creation."))
      (loop for char across "MDBF" for idx from 0 do (setf (cffi:mem-aref ptr :uint8 idx) (char-code char)))
      (setf (cffi:mem-aref ptr :uint8 4) 1)
      (endian-buffer:poke-int32 initial-capacity (cffi:inc-pointer ptr 5) :little)
      (endian-buffer:poke-int32 0 (cffi:inc-pointer ptr 9) :little)
      (endian-buffer:poke-int16 (length fields) (cffi:inc-pointer ptr 13) :little)
      (let ((field-ptr (cffi:inc-pointer ptr 15)))
        (dolist (f fields)
          (setf (cffi:mem-ref field-ptr :uint8) (getf f :type-id))
          (setf (cffi:mem-ref (cffi:inc-pointer field-ptr 1) :uint8) 0)
          (endian-buffer:poke-int16 (getf f :param) (cffi:inc-pointer field-ptr 2) :little)
          (setf field-ptr (cffi:inc-pointer field-ptr 4))))
      (posix-munmap ptr total-bytes)
      (posix-close fd)
      t)))

(defun open-binary-table (path &key type (read-only nil) (threadsafe t) (verify-integrity t))
  "Open a binary table at PATH using TYPE created using DEFINE-BINARY-TABLE, and validate
schema match.

When READ-ONLY is T:
  - File is opened read-only and mmap uses PROT_READ only
  - Acquires a shared lock (multiple readers allowed)
  - save-row, append-row, nullify-row will signal an error
  - No thread mutex used (multiple readers are safe without locking)

When READ-ONLY is NIL (default):
  - File is opened read-write with PROT_READ|PROT_WRITE
  - Acquires an exclusive lock (blocks other writers)

When THREADSAFE is T (default) and READ-ONLY is NIL:
  - All accessor methods (load-row, save-row, append-row, nullify-row) automatically
    acquire the table's mutex, making them safe to call from multiple threads
  - Use WITH-TABLE-LOCKED to batch multiple operations under one lock acquisition

When THREADSAFE is NIL:
  - No automatic mutex locking in accessor methods
  - User takes responsibility for ensuring single-threaded access or external locking

When VERIFY-INTEGRITY is T (default):
  - Performs full integrity scan of all rows (status flags and Fletcher-16 checksums)
  - Signals error if any corruption is detected
  - Adds ~0.8s overhead for 1M row database"
  (let* ((meta (get-table-meta type))
         (schema-fields (table-class-fields meta))
         (stride (table-class-stride meta))
         (h-size (table-class-header-size meta))
         (fd (posix-open path (if read-only o-rdonly o-rdwr) 0)))
    (when (< fd 0) (error "Could not open data file: ~A" path))
    (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
      (let* ((lock-mode (if read-only +lock-sh+ +lock-ex+))
             (lock-res (posix-flock fd (logior lock-mode +lock-nb+))))
        (when (< lock-res 0)
          (posix-close fd)
          (error "LOCK CONFLICT: Could not acquire ~A lock on '~A'."
                 (if read-only "Shared" "Exclusive") path)))
      (let* ((actual-file-bytes (file-length s))
             (mmap-prot (if read-only prot-read (logior prot-read prot-write)))
             (ptr (posix-mmap (cffi:null-pointer) actual-file-bytes mmap-prot map-shared fd 0)))
        (when (cffi:pointer-eq ptr +map-failed+)
          (posix-close fd)
          (error "Could not mmap the data layout into memory."))
        (unless (and (= (cffi:mem-aref ptr :uint8 0) (char-code #\M))
                     (= (cffi:mem-aref ptr :uint8 1) (char-code #\D))
                     (= (cffi:mem-aref ptr :uint8 2) (char-code #\B))
                     (= (cffi:mem-aref ptr :uint8 3) (char-code #\F)))
          (posix-munmap ptr actual-file-bytes)
          (posix-close fd)
          (error "Invalid layout footprint signature (Expected Magic: MDBF)."))
        (let* ((max-rows (endian-buffer:peek-uint32 (cffi:inc-pointer ptr 5) :little))
               (expected-minimum-bytes (+ h-size (* max-rows stride))))
          (when (< actual-file-bytes expected-minimum-bytes)
            (posix-munmap ptr actual-file-bytes)
            (posix-close fd)
            (error "CRITICAL DATABASE CORRUPTION: File '~A' has been truncated! ~%~
                   Expected minimum size: ~A bytes (based on capacity of ~A rows).~%~
                   Actual disk file size : ~A bytes."
                   path expected-minimum-bytes max-rows actual-file-bytes))
          (let ((disk-field-count (endian-buffer:peek-int16 (cffi:inc-pointer ptr 13) :little)))
            (unless (= disk-field-count (length schema-fields))
              (posix-munmap ptr actual-file-bytes)
              (posix-close fd)
              (error "Schema Mismatch: File has ~A columns, but CLOS layout defines ~A."
                     disk-field-count (length schema-fields)))
            (let ((field-ptr (cffi:inc-pointer ptr 15)))
              (dolist (f schema-fields)
                (let ((disk-type-id (cffi:mem-ref field-ptr :uint8))
                      (disk-param (endian-buffer:peek-uint16 (cffi:inc-pointer field-ptr 2) :little)))
                  (unless (and (= disk-type-id (getf f :type-id))
                               (= disk-param (getf f :param)))
                    (posix-munmap ptr actual-file-bytes)
                    (posix-close fd)
                    (error "Schema Integrity Violation: Field '~A' layout on disk does not match CLOS definition."
                           (getf f :name))))
                (setf field-ptr (cffi:inc-pointer field-ptr 4)))))
          (let ((table (make-mapped-table :fd fd :base-ptr ptr :mapped-length actual-file-bytes
                                          :schema-symbol type :file-path path
                                          :read-only-p read-only :threadsafe-p threadsafe)))
            (when verify-integrity
              (multiple-value-bind (ok err) (%verify-open-table-integrity table)
                (unless ok
                  (close-binary-table table)
                  (error "Database integrity check failed: ~A" err))))
            table))))))

(defun close-binary-table (table)
  "Close a binary table object TABLE."
  (posix-munmap (mapped-table-base-ptr table) (mapped-table-mapped-length table))
  (posix-flock (mapped-table-fd table) +lock-un+)
  (posix-close (mapped-table-fd table))
  (setf (mapped-table-base-ptr table) (cffi:null-pointer))
  t)

(defun resize-mapped-table (table new-max-rows)
  "Resize a binary TABLE to a new number of rows, NEW-MAX-ROWS."
  (let* ((meta (get-table-meta (mapped-table-schema-symbol table)))
         (new-length (+ (table-class-header-size meta) (* new-max-rows (table-class-stride meta)))))
    (posix-munmap (mapped-table-base-ptr table) (mapped-table-mapped-length table))
    (posix-ftruncate (mapped-table-fd table) new-length)
    (let ((new-ptr (posix-mmap (cffi:null-pointer) new-length (logior prot-read prot-write) map-shared (mapped-table-fd table) 0)))
      (when (cffi:pointer-eq new-ptr +map-failed+)
        (error "Failed to remap file layout configurations during automatic extension."))
      (setf (mapped-table-base-ptr table) new-ptr)
      (setf (mapped-table-mapped-length table) new-length)
      (set-header-max-rows table new-max-rows)
      t)))

(declaim (inline table-peek table-poke calculate-cell-ptr))
(defun calculate-cell-ptr (table row field-name-or-meta &key writing-p)
  (let* ((meta (get-table-meta (mapped-table-schema-symbol table)))
         (fields (table-class-fields meta))
         (active-rows (get-header-row-count table))
         (max-allocated (get-header-max-rows table))
         (static-p (listp field-name-or-meta))
         (field-offset (if static-p
                           (getf field-name-or-meta :offset)
                           (let ((field (find field-name-or-meta fields :key #'(lambda (x) (getf x :name)))))
                             (unless field (error "Field ~A does not exist in this schema configuration." field-name-or-meta))
                             (getf field :offset))))
         (type-id      (if static-p
                           (getf field-name-or-meta :type-id)
                           (let ((field (find field-name-or-meta fields :key #'(lambda (x) (getf x :name)))))
                             (getf field :type-id))))
         (param        (if static-p
                           (getf field-name-or-meta :param)
                           (let ((field (find field-name-or-meta fields :key #'(lambda (x) (getf x :name)))))
                             (getf field :param)))))
    
    (if writing-p
        (when (>= row max-allocated)
          (error "Row boundary exception: Requested write row ~A exceeds maximum allocated table capacity of ~A."
                 row max-allocated))
        (when (>= row active-rows)
          (error "Out of Bounds Read: Attempted to read row index ~A, but table only contains ~A populated rows."
                 row active-rows)))

    (let ((row-offset (+ (table-class-header-size meta) (* row (table-class-stride meta)) field-offset)))
      (values (cffi:inc-pointer (mapped-table-base-ptr table) row-offset) type-id param))))

(defun table-peek (table row field-name)
  "Given an open TABLE, and ROW number, and FIELD-NAME, return the value there."
  (with-table-maybe-locked (table)
    (multiple-value-bind (ptr type-id param) (calculate-cell-ptr table row field-name :writing-p nil)
      (case type-id
        (1 (endian-buffer:peek-int8 ptr)) (2 (endian-buffer:peek-uint8 ptr))
        (3 (endian-buffer:peek-int16 ptr :little)) (4 (endian-buffer:peek-uint16 ptr :little))
        (5 (endian-buffer:peek-int32 ptr :little)) (6 (endian-buffer:peek-uint32 ptr :little))
        (7 (endian-buffer:peek-int64 ptr :little)) (8 (endian-buffer:peek-uint64 ptr :little))
        (9 (endian-buffer:peek-single-float ptr :little)) (10 (endian-buffer:peek-double-float ptr :little))
        (11 (endian-buffer:peek-string ptr param))
        (12 (let ((res (make-array param :element-type '(unsigned-byte 8))))
              (loop for idx from 0 below param do (setf (aref res idx) (cffi:mem-ref ptr :uint8 idx))) res))))))

(defun table-poke (table row field-name value)
  "Given an open TABLE, and ROW number, and FIELD-NAME, insert VALUE there."
  (when (mapped-table-read-only-p table)
    (error "Cannot table-poke: table ~A is opened read-only" (mapped-table-file-path table)))
  (with-table-maybe-locked (table)
    (multiple-value-bind (ptr type-id param) (calculate-cell-ptr table row field-name :writing-p t)
      (let ((val value))
        (case type-id
          (1 (endian-buffer:poke-int8 val ptr)) (2 (endian-buffer:poke-uint8 val ptr))
          (3 (endian-buffer:poke-int16 val ptr :little)) (4 (endian-buffer:poke-uint16 val ptr :little))
          (5 (endian-buffer:poke-int32 val ptr :little)) (6 (endian-buffer:poke-uint32 val ptr :little))
          (7 (endian-buffer:poke-int64 val ptr :little)) (8 (endian-buffer:poke-uint64 val ptr :little))
          (9 (endian-buffer:poke-single-float val ptr :little)) (10 (endian-buffer:poke-double-float val ptr :little))
          (11 (endian-buffer:poke-string val ptr param))
          (12 (loop for idx from 0 below param do (setf (cffi:mem-ref ptr :uint8 idx) (aref val idx))))))
      (when (>= row (get-header-row-count table)) (set-header-row-count table (1+ row))) value)))

;; ====================================================================================================
;;               INTERNAL MACROS FOR STRUCT DATA PIPELINES (Bypasses CLOS at Runtime)
;; ====================================================================================================

(defmacro %table-peek-macro (table row offset type-id param)
  (let* ((table-var (gensym "TABLE"))
         (row-var   (gensym "ROW"))
         (schema-meta `(get-table-meta (mapped-table-schema-symbol ,table-var)))
         (ptr-form  `(cffi:inc-pointer
                      (mapped-table-base-ptr ,table-var)
                      (+ (table-class-header-size ,schema-meta)
                         (* ,row-var (table-class-stride ,schema-meta))
                         ,offset))))
    `(let ((,table-var ,table)
           (,row-var ,row))
       ;; FIX: Changed from checking against row count to checking max allocation capacity
       (when (>= ,row-var (get-header-max-rows ,table-var))
         (error "Out of Bounds Access: Attempted inline operation on row ~A which exceeds max capacity of ~A rows."
                ,row-var (get-header-max-rows ,table-var)))
       ,(case type-id
          (1 `(endian-buffer:peek-int8 ,ptr-form)) (2 `(endian-buffer:peek-uint8 ,ptr-form))
          (3 `(endian-buffer:peek-int16 ,ptr-form :little)) (4 `(endian-buffer:peek-uint16 ,ptr-form :little))
          (5 `(endian-buffer:peek-int32 ,ptr-form :little)) (6 `(endian-buffer:peek-uint32 ,ptr-form :little))
          (7 `(endian-buffer:peek-int64 ,ptr-form :little)) (8 `(endian-buffer:peek-uint64 ,ptr-form :little))
          (9 `(endian-buffer:peek-single-float ,ptr-form :little)) (10 `(endian-buffer:peek-double-float ,ptr-form :little))
          (11 `(endian-buffer:peek-string ,ptr-form ,param))
          (12 `(let ((res (make-array ,param :element-type '(unsigned-byte 8))))
                 (loop for idx from 0 below ,param do (setf (aref res idx) (cffi:mem-ref ,ptr-form :uint8 idx))) res))))))


(defmacro %table-poke-macro (table row offset type-id param value)
  (let* ((table-var (gensym "TABLE"))
         (row-var   (gensym "ROW"))
         (val-var    (gensym "VALUE"))
         (schema-meta `(get-table-meta (mapped-table-schema-symbol ,table-var)))
         (ptr-form  `(cffi:inc-pointer
                      (mapped-table-base-ptr ,table-var)
                      (+ (table-class-header-size ,schema-meta)
                         (* ,row-var (table-class-stride ,schema-meta))
                         ,offset))))
    `(let ((,table-var ,table)
           (,row-var ,row)
           (,val-var ,value))
       (when (>= ,row-var (get-header-max-rows ,table-var))
         (error "Out of Bounds Write: Attempted inline write to row ~A, which exceeds maximum table capacity of ~A. Use APPEND or call RESIZE-MAPPED-TABLE."
                ,row-var (get-header-max-rows ,table-var)))
       ,(case type-id
          (1 `(endian-buffer:poke-int8 ,val-var ,ptr-form)) (2 `(endian-buffer:poke-uint8 ,val-var ,ptr-form))
          (3 `(endian-buffer:poke-int16 ,val-var ,ptr-form :little)) (4 `(endian-buffer:poke-uint16 ,val-var ,ptr-form :little))
          (5 `(endian-buffer:poke-int32 ,val-var ,ptr-form :little)) (6 `(endian-buffer:poke-uint32 ,val-var ,ptr-form :little))
          (7 `(endian-buffer:poke-int64 ,val-var ,ptr-form :little)) (8 `(endian-buffer:poke-uint64 ,val-var ,ptr-form :little))
          (9 `(endian-buffer:poke-single-float ,val-var ,ptr-form :little)) (10 `(endian-buffer:poke-double-float ,val-var ,ptr-form :little))
          (11 `(endian-buffer:poke-string ,val-var ,ptr-form ,param))
          (12 `(loop for idx from 0 below ,param do (setf (cffi:mem-ref ,ptr-form :uint8 idx) (aref ,val-var idx)))))
       ,val-var)))

(defun truncate-mapped-table (table)
  "Truncate table to its actual row count, releasing unused allocated space."
  (let ((actual-rows (get-header-row-count table))
        (max-rows (get-header-max-rows table)))
    (when (< actual-rows max-rows)
      (resize-mapped-table table actual-rows))
    actual-rows))

;; this is not exported, so the akwardness of having to know the SAVER-FUNC
;; for a TABLE is invisible to users
(defun append-table-row (table struct-obj saver-func)
  (let ((current-count (get-header-row-count table))
        (max-capacity (get-header-max-rows table)))
    (when (>= current-count max-capacity)
      (resize-mapped-table table (+ max-capacity 100)))
    (funcall saver-func table current-count struct-obj)
    (set-header-row-count table (1+ current-count))
    current-count))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %verify-open-table-integrity (table &key (verbose nil))
  "Internal: verify integrity of an already-open table.
Returns (VALUES T ROW-COUNT) on success, (VALUES NIL ERROR-STRING) on failure."
  (let* ((schema-meta (get-table-meta (mapped-table-schema-symbol table)))
         (header-size (table-class-header-size schema-meta))
         (stride      (table-class-stride schema-meta))
         (max-rows    (get-header-max-rows table))
         (row-count   (get-header-row-count table))
         (expected-size (+ header-size (* max-rows stride)))
         (base-ptr    (mapped-table-base-ptr table))
         (db-path     (mapped-table-file-path table)))

    ;; 1. Validate File Length Alignment
    (with-open-file (stream db-path :element-type '(unsigned-byte 8))
      (let ((actual-size (file-length stream)))
        (unless (= actual-size expected-size)
          (return-from %verify-open-table-integrity
            (values nil (format nil "File size mismatch! Expected ~A bytes, found ~A bytes."
                                expected-size actual-size))))))

    ;; 2. Loop and validate individual rows up to the high-water mark
    (when verbose
      (format t "[AUDIT] Verifying ~A active rows out of ~A total slots...~%" row-count max-rows)
      (force-output))

    (dotimes (i row-count)
      (let* ((row-offset (+ header-size (* i stride)))
             (row-ptr    (cffi:inc-pointer base-ptr row-offset))
             ;; The status flag sits right before the 2-byte checksum at the tail of the stride
             (status-ptr (cffi:inc-pointer row-ptr (- stride 3)))
             (status-val (cffi:mem-ref status-ptr :uint8))
             ;; The stored checksum is a 16-bit uint at the very end of the stride
             (chk-ptr    (cffi:inc-pointer row-ptr (- stride 2)))
             (stored-chk (endian-buffer:peek-uint16 chk-ptr :little)))

        ;; Validate Status Flag (Must be 0 for nullified/empty, 1 for populated)
        (unless (or (= status-val 0) (= status-val 1))
          (return-from %verify-open-table-integrity
            (values nil (format nil "Corrupted row state flag (~A) detected at Row ~A" status-val i))))

        ;; If the row is flagged as populated, verify its Fletcher16 checksum
        (when (= status-val 1)
          ;; Compute checksum over the data columns portion (stride minus 3 bytes metadata tail)
          (let ((computed-chk (%compute-fletcher16-ptr row-ptr (- stride 3))))
            (unless (= computed-chk stored-chk)
              (return-from %verify-open-table-integrity
                (values nil (format nil "Fletcher16 Checksum mismatch at Row ~A! Stored: ~X, Computed: ~X"
                                    i stored-chk computed-chk))))))))

    (when verbose
      (format t "[AUDIT] PASSED. Database is 100% consistent. Active Rows: ~A/~A~%" row-count max-rows)
      (force-output))

    (values t row-count)))

(defun verify-binary-table-integrity (db-path &key type (verbose nil))
  "Scans a memory-mapped database file from top to bottom.
   Validates the file boundary size, header schema, and verifies every single
   allocated row's status flag and internal Fletcher16 checksum.

   Returns:
     On success: (VALUES T POPULATED-ROWS)
     On failure: (VALUES NIL \"Error description string\")"
  (when verbose
    (format t "~%[AUDIT] Starting full integrity scan on: ~A~%" db-path)
    (force-output))

  (unless (probe-file db-path)
    (return-from verify-binary-table-integrity
      (values nil "Target database file does not exist.")))

  ;; Open without verify-integrity to avoid recursion
  (let ((table (handler-case (open-binary-table db-path :type type :read-only t :verify-integrity nil)
                 (error (c)
                   (return-from verify-binary-table-integrity
                     (values nil (format nil "Failed to open table: ~A" c)))))))
    (unwind-protect
         (%verify-open-table-integrity table :verbose verbose)
      (close-binary-table table))))
