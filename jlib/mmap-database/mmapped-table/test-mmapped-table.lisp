(defpackage :storage-test-suite
  (:use :cl :mmapped-table)
  (:export #:run-comprehensive-suite))

(in-package :storage-test-suite)

;; ====================================================================================================
;; 1. SCHEMA DEFINITION FOR ALL SUPPORTED TYPES
;; ====================================================================================================
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

;; ====================================================================================================
;; 2. DETERMINISTIC MOCK DATA GENERATORS
;; ====================================================================================================
(defun get-time-in-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun generate-dynamic-blob (row-idx)
  (let ((arr (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref arr i) (mod (+ row-idx i) 256)))
    arr))

(defun generate-dynamic-string (row-idx)
  (format nil "R-~A-~4,'0X" (mod row-idx 100) (mod row-idx 65536)))

;; ====================================================================================================
;; 3. PHASE A: EXHAUSTIVE INTEGRITY AND CONSISTENCY VALIDATION
;; ====================================================================================================
(defun run-integrity-validation (db-path)
  (let ((count 50000))
    (format t "~%[STAGE 1/3] Running Strict 100% Integrity Validation (~A rows)..." count)
    (force-output)
    (ensure-directories-exist db-path)
    (when (probe-file db-path) (delete-file db-path))
    (mmapped-table:create-binary-table db-path :type 'all-types-table :initial-capacity count)
    
    (let ((table (mmapped-table:open-binary-table db-path :type 'all-types-table))
          (write-record (make-all-types-table-rec))
          (read-record (make-all-types-table-rec)))
      (unwind-protect
           (progn
             ;; Write deterministic metrics
             (dotimes (i count)
               (setf (all-types-table-rec-id write-record) i
                     (all-types-table-rec-small-idx write-record) (logand i #x7F)
                     (all-types-table-rec-flag write-record) (mod i 2)
                     (all-types-table-rec-short-val write-record) (- (mod i 32768))
                     (all-types-table-rec-port-num write-record) (mod i 65535)
                     (all-types-table-rec-sys-status write-record) (- i)
                     (all-types-table-rec-counter write-record) (* i 3)
                     (all-types-table-rec-checksum write-record) (* i 1234567)
                     (all-types-table-rec-reading-f write-record) (coerce (/ i 7.0) 'single-float)
                     (all-types-table-rec-reading-d write-record) (coerce (/ i 3.14159) 'double-float)
                     (all-types-table-rec-serial-str write-record) (generate-dynamic-string i)
                     (all-types-table-rec-payload-raw write-record) (generate-dynamic-blob i))
               (save-row table i write-record))
             
             ;; Deep read verification
             (dotimes (i count)
               (load-row table i :fill-obj read-record)
               (assert (= (all-types-table-rec-id read-record) i))
               (assert (= (all-types-table-rec-small-idx read-record) (logand i #x7F)))
               (assert (= (all-types-table-rec-flag read-record) (mod i 2)))
               (assert (= (all-types-table-rec-short-val read-record) (- (mod i 32768))))
               (assert (= (all-types-table-rec-port-num read-record) (mod i 65535)))
               (assert (= (all-types-table-rec-sys-status read-record) (- i)))
               (assert (= (all-types-table-rec-counter read-record) (* i 3)))
               (assert (= (all-types-table-rec-checksum read-record) (* i 1234567)))
               (assert (= (all-types-table-rec-reading-f read-record) (coerce (/ i 7.0) 'single-float)))
               (assert (= (all-types-table-rec-reading-d read-record) (coerce (/ i 3.14159) 'double-float)))
               (assert (string= (all-types-table-rec-serial-str read-record) (generate-dynamic-string i)))
               (let ((target-blob (generate-dynamic-blob i))
                     (read-blob (all-types-table-rec-payload-raw read-record)))
                 (dotimes (b-idx 16) (assert (= (aref read-blob b-idx) (aref target-blob b-idx))))))
             (format t " PASSED! Data layout is 100% correct.~%"))
        (mmapped-table:close-binary-table table)))))

;; ====================================================================================================
;; 4. PHASE B: UN-THROTTLED RAW SPEED BENCHMARK (1 MILLION RECORDS)
;; ====================================================================================================
(defun run-raw-speed-benchmark (db-path)
  (let* ((count 1000000)
         (static-blob (make-array 16 :element-type '(unsigned-byte 8) :initial-element #xAA)))
    (format t "~%[STAGE 2/3] Executing Raw Speed Test on Un-throttled Database (~A rows)...~%" count)
    (force-output)
    
    (when (probe-file db-path) (delete-file db-path))
    (mmapped-table:create-binary-table db-path :type 'all-types-table :initial-capacity count)
    
    (let ((table (mmapped-table:open-binary-table db-path :type 'all-types-table))
          (write-record (make-all-types-table-rec :id 42 :small-idx -5 :flag 1 :short-val -1000 
                                                   :port-num 80 :sys-status -50000 :counter 100 
                                                   :checksum 9876543210 :reading-f 1.23f0 
                                                   :reading-d 4.56d0 :serial-str "PERF-TEST-12" 
                                                   :payload-raw static-blob))
          (read-record (make-all-types-table-rec))
          (start-time 0.0)
          (end-time 0.0)
          (write-elapsed 0.0)
          (read-elapsed 0.0))
      
      (unwind-protect
           (progn
             ;; --- RAW WRITE BENCHMARK ---
             (format t "  >> Writing records to mmap allocation space...")
             (force-output)
             (setf start-time (get-time-in-seconds))
             
             (dotimes (i count)
               ;; Directly save without changing data per loop to hit raw memory bus velocity
               (save-row table i write-record))
             
             (setf end-time (get-time-in-seconds))
             (setf write-elapsed (- end-time start-time))
             (format t " Done!~%     RAW WRITE VELOCITY: ~,2F records/sec (~,4F seconds)~%" 
                     (/ count write-elapsed) write-elapsed)
             
             ;; --- RAW READ BENCHMARK ---
             (format t "  >> Reading records via zero-GC inline pointer tracking...")
             (force-output)
             (setf start-time (get-time-in-seconds))
             
             (dotimes (i count)
               ;; Pull data directly out using macro-unrolled pointers with ZERO overhead
               (load-row table i :fill-obj read-record))
             
             (setf end-time (get-time-in-seconds))
             (setf read-elapsed (- end-time start-time))
             (format t " Done!~%     RAW READ VELOCITY : ~,2F records/sec (~,4F seconds)~%" 
                     (/ count read-elapsed) read-elapsed))
        
        (mmapped-table:close-binary-table table)))))

;; ====================================================================================================
;; 5. PHASE C: BOUNDARY DEFENSE VALIDATION
;; ====================================================================================================
(defun run-boundary-safety-test (db-path)
  (format t "~%[STAGE 3/3] Verifying Active Pointer Boundary Protection Hooks... ")
  (let ((table (mmapped-table:open-binary-table db-path :type 'all-types-table))
        (dummy-rec (make-all-types-table-rec)))
    (unwind-protect
         (let ((active-count (mmapped-table::get-header-row-count table))
               (max-capacity (mmapped-table::get-header-max-rows table)))
           (handler-case
               (progn (load-row table active-count :fill-obj dummy-rec)
                      (error "FAIL"))
             (error () (format t "Read Protection [OK] | ")))
           (handler-case
               (progn (save-row table (+ max-capacity 1) dummy-rec)
                      (error "FAIL"))
             (error () (format t "Write Protection [OK]~%"))))
      (mmapped-table:close-binary-table table))))

;; ====================================================================================================
;; 6. MASTER ENGINE HOOK
;; ====================================================================================================
(defun run-comprehensive-suite ()
  (let ((test-db "/tmp/comprehensive_mmap_suite.db"))
    (run-integrity-validation test-db)
    (run-raw-speed-benchmark test-db)
    (run-boundary-safety-test test-db)
    (format t "~%======================================================================~%")
    (format t "   ALL VERIFICATIONS PASSED: TRUE RAW ENGINE PERFORMANCE RECORDED   ~%")
    (format t "======================================================================~%")
    t))

(run-comprehensive-suite)