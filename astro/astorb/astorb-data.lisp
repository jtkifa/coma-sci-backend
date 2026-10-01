;;;; astorb-data.lisp - Parse astorb text files and convert to mmap database

(in-package :astorb)

;; List of all astorb files, set during loading
(defvar *astorb-file-list* nil)
(defvar *astorb-file* nil)


(defun %aformat (&rest args)
  "Format only if *astorb-quiet* is false."
  (when (not *astorb-quiet*)
    (apply 'format args)))


;;; ============================================================================
;;; File List Management
;;; ============================================================================

(defun get-astorb-file-list ()
  "Return a list of astorb.dat.[gz] files, with the latest one first.
Files must be in *astorb-data-dir* and of the form astorb.dat.MJD or astorb.dat.MJD.gz"
  (bordeaux-threads:with-recursive-lock-held (*astorb-lock*)
    (mapcar
     (lambda (basename)
       (format nil "~A/~A" *astorb-data-dir* basename))
     (remove-if
      (lambda (file) (or (search ".mdbf" file)
                         (search ".fasl" file)))
      (sort (mapcar 'file-io:file-minus-dir
                    (mapcar 'namestring
                            (append
                             (directory (format nil "~A/astorb.dat.*" *astorb-data-dir*))
                             (directory (format nil "~A/astorb.dat.*.gz" *astorb-data-dir*)))))
            'string>)))))


(defun get-astorb-mdbf-file-list ()
  "Return a list of .mdbf database files, with the latest one first."
  (bordeaux-threads:with-recursive-lock-held (*astorb-lock*)
    (mapcar
     (lambda (basename)
       (format nil "~A/~A" *astorb-data-dir* basename))
     (sort (mapcar 'file-io:file-minus-dir
                   (mapcar 'namestring
                           (directory (format nil "~A/astorb.dat.*.mdbf" *astorb-data-dir*))))
           'string>))))


(defun describe-astorb-file (filename &key quiet (verbose-stream *astorb-info-output-stream*))
  "Describe current mjd file and return MJD of it."
  (let* ((basename (file-io:file-minus-dir filename))
         (i-mjd-start (or (position-if 'digit-char-p basename)
                          (error "NO MJD found in astorb file ~A~%" filename)))
         (mjd-elements (numio:parse-float basename
                                          :start i-mjd-start
                                          :junk-allowed t))
         (date (astro-time:mjd-to-ut-string mjd-elements)))
    (when (and verbose-stream (not quiet))
      (%aformat verbose-stream
                "ASTORB: Using file ~A ~% with MJD=~A and UT ~A~%"
                basename
                mjd-elements date))
    mjd-elements))


;;; ============================================================================
;;; String Scrubbing (same as original)
;;; ============================================================================

(defun %scrub-string (string)
  "Make string lowercase and remove non-alphanumeric chars."
  (declare (type (simple-array character (*)) string)
           (optimize speed))
  (let* ((nspc (loop
                 with n of-type (unsigned-byte 28) = 0
                 for c of-type base-char across string
                 when (not (and (typep c 'base-char) (alphanumericp c)))
                   do (incf n)
                 finally (return n)))
         (outstring (make-array (- (length string) nspc) :element-type 'base-char)))
    (loop
      with i of-type (unsigned-byte 28) = 0
      for c across string
      when (and (typep c 'base-char) (alphanumericp c))
        do
           (setf (aref outstring i) (char-downcase (the base-char c)))
           (incf i))
    outstring))


;;; ============================================================================
;;; Line Reading Utilities
;;; ============================================================================

(defmacro astorb-with-open-file ((stream-var filename) &body body)
  "Open a file either in gzip mode or other mode, in byte mode."
  `(let ((%file ,filename))
     (flet ((%astorb-with-open-file-body (,stream-var)
              ,@body))
       (if (string-utils:string-ends-with %file ".gz")
           (gzip-stream:with-open-gzip-file (%stream %file)
             (%astorb-with-open-file-body %stream))
           (with-open-file (%stream %file :element-type '(unsigned-byte 8))
             (%astorb-with-open-file-body %stream))))))


(defun astorb-read-line (stream &optional buffer)
  "Read an astorb line from byte stream."
  (declare (type stream stream)
           (type (or null string) buffer))
  (loop with buffer = (or buffer (make-string 2048))
        with nmax = (length buffer)
        for b = (read-byte stream nil nil)
        for i of-type fixnum from 0
        when (not b)
          do (return nil)
        do (let ((c (code-char b)))
             (cond ((char= c #\lf)
                    (return buffer))
                   ((char= c #\cr)
                    nil)
                   (t
                    (when (= i nmax) (error "Line too long in astorb stream."))
                    (setf (aref buffer i) c))))))


;;; ============================================================================
;;; Text to MDBF Conversion
;;; ============================================================================

(defun convert-astorb-text-to-mmap (astorb-file &key
                                               (verbose-stream *astorb-info-output-stream*)
                                               (progress-interval 10000)
                                               (initial-capacity 1400000))
  "Convert an astorb text or .gz file to a memory-mapped database.
INITIAL-CAPACITY is the initial row allocation (default 1.4M, enough for current astorb).
Returns the path to the created .mdbf file.

Uses atomic write pattern: writes to a .TMP file first, then renames on success.
If conversion fails, the temp file is deleted and no corrupt database is left behind."
  #+sbcl (sb-ext:gc :full t)
  (bordeaux-threads:with-recursive-lock-held (*astorb-lock*)
    (let* ((mjd-elements (describe-astorb-file astorb-file :quiet t))
           (mdbf-path (make-mdbf-filename-from-source astorb-file))
           (tmp-path (format nil "~A.TMP" mdbf-path))
           (tmpline (make-array 20 :element-type 'character))
           (buffer (make-string 512))
           (where nil)
           (ncurline 0)
           (nrows 0)
           (conversion-succeeded nil))
      (declare (ignorable where))

      (%aformat verbose-stream "ASTORB: Converting ~A~%" astorb-file)
      (%aformat verbose-stream "ASTORB: Writing to temp file: ~A~%" tmp-path)

      ;; Delete any leftover temp file from a previous failed attempt
      (when (probe-file tmp-path)
        (%aformat verbose-stream "ASTORB: Removing stale temp file~%")
        (delete-file tmp-path))

      ;; Create the mmap database at temp path
      (mmapped-table:create-binary-table tmp-path
                                         :type 'asteroid-orbit
                                         :initial-capacity initial-capacity)

      ;; Open the table for writing
      (let ((table (mmapped-table:open-binary-table tmp-path :type 'asteroid-orbit)))
        (unwind-protect
             (progn
               (astorb-with-open-file (s astorb-file)
                 (labels ((subline (line n1 n2)
                            (declare (type (simple-array character (*)) line)
                                     (type (unsigned-byte 16) n1 n2)
                                     (optimize speed))
                            (fill tmpline #\space)
                            (loop
                              for i of-type fixnum from n1 below n2
                              for j of-type fixnum from 0
                              do (setf (aref tmpline j) (aref line i)))
                            tmpline)
                          (grab-string (line n1 n2)
                            (declare (type (simple-array character (*)) line)
                                     (type (unsigned-byte 16) n1 n2)
                                     (optimize speed))
                            (string-trim #(#\tab #\space) (subseq line n1 n2)))
                          (grab-int (line n1 n2 &optional (default 0))
                            (declare (type (simple-array character (*)) line)
                                     (type (unsigned-byte 16) n1 n2)
                                     (optimize speed))
                            (or (ignore-errors (parse-integer (subline line n1 n2)))
                                default))
                          (grab-dbl (line n1 n2 &optional (default 0d0))
                            (declare (type (simple-array character (*)) line)
                                     (type (unsigned-byte 16) n1 n2)
                                     (optimize speed))
                            (or (ignore-errors (numio:parse-float (subline line n1 n2)))
                                default))
                          (grab-float (line n1 n2 &optional (default 0e0))
                            (declare (type (simple-array character (*)) line)
                                     (type (unsigned-byte 16) n1 n2)
                                     (optimize speed))
                            (or (ignore-errors
                                 (float (numio:parse-float (subline line n1 n2)) 1.0))
                                default)))

                   (multiple-value-bind (val err)
                       (ignore-errors
                        (loop
                          for line = (astorb-read-line s buffer)
                          while line
                          do
                             (incf nrows)
                             (setf ncurline nrows)

                             ;; Parse all fields
                             (let* ((astnum (progn (setf where "ASTNUM") (grab-int line 0 6)))
                                    (name-str (progn (setf where "NAME") (grab-string line 7 25)))
                                    (sname-str (progn (setf where "SNAME")
                                                      (if (plusp (length name-str))
                                                          (%scrub-string name-str)
                                                          "")))
                                    (hmag (progn (setf where "HMAG") (grab-float line 42 47)))
                                    (g-val (progn (setf where "G") (grab-float line 49 53)))
                                    (iras-km (progn (setf where "IRAS-KM") (grab-float line 55 64)))
                                    (iras-class (progn (setf where "IRAS-CLASS")
                                                       (grab-string line 65 67)))
                                    (code1 (progn (setf where "CODE1") (grab-int line 73 75)))
                                    (code2 (progn (setf where "CODE2") (grab-int line 77 79)))
                                    (code3 (progn (setf where "CODE3") (grab-int line 81 83)))
                                    (code4 (progn (setf where "CODE4") (grab-int line 85 87)))
                                    (code5 (progn (setf where "CODE5") (grab-int line 89 91)))
                                    (code6 (progn (setf where "CODE6") (grab-int line 93 95)))
                                    (orbarc (progn (setf where "ORBARC") (grab-float line 95 100)))
                                    (nobs-val (progn (setf where "NOBS") (grab-int line 101 105)))
                                    (epoch-osc (progn (setf where "EPOCH-OSC") (grab-int line 106 114)))
                                    (mean-anomaly (progn (setf where "MEAN-ANOM") (grab-dbl line 115 125)))
                                    (arg-peri (progn (setf where "ARG-PERI") (grab-dbl line 126 136)))
                                    (anode (progn (setf where "ANODE") (grab-dbl line 137 147)))
                                    (orbinc (progn (setf where "ORBINC") (grab-dbl line 148 157)))
                                    (ecc (progn (setf where "ECC") (grab-dbl line 158 168)))
                                    (a-val (progn (setf where "A") (grab-dbl line 170 181)))
                                    (orbit-date (progn (setf where "ORBIT-DATE") (grab-int line 182 190))))

                               ;; Create record and append
                               (let ((rec (make-asteroid-orbit-rec
                                           :astnum astnum
                                           :name name-str
                                           :sname sname-str
                                           :hmag hmag
                                           :g g-val
                                           :iras-km iras-km
                                           :iras-class iras-class
                                           :code1 code1
                                           :code2 code2
                                           :code3 code3
                                           :code4 code4
                                           :code5 code5
                                           :code6 code6
                                           :orbarc orbarc
                                           :nobs nobs-val
                                           :epoch-osc epoch-osc
                                           :mean-anomaly mean-anomaly
                                           :arg-peri arg-peri
                                           :anode anode
                                           :orbinc orbinc
                                           :ecc ecc
                                           :a a-val
                                           :orbit-date orbit-date)))
                                 (mmapped-table:append-row table rec)))

                             ;; Progress report
                             (when (and verbose-stream
                                        progress-interval
                                        (zerop (mod nrows progress-interval)))
                               (%aformat verbose-stream "ASTORB: Converted ~A rows~%" nrows))
                          finally (return t)))
                     (when (not val)
                       (error "ERROR ~A at line ~A: line is ~A" err ncurline buffer)))))

               ;; Truncate to exact size
               (let ((actual-rows (mmapped-table:truncate-mapped-table table)))
                 (%aformat verbose-stream "ASTORB: Final size: ~A rows~%" actual-rows))

               ;; Mark conversion as successful
               (setf conversion-succeeded t))

          ;; Always close the table
          (mmapped-table:close-binary-table table)

          ;; If conversion failed, delete the temp file
          (unless conversion-succeeded
            (%aformat verbose-stream "ASTORB: Conversion failed, removing temp file~%")
            (ignore-errors (delete-file tmp-path)))))

      ;; Only proceed if conversion succeeded
      (unless conversion-succeeded
        (error "ASTORB: Conversion failed, no database created"))

      ;; Atomic rename: temp file -> final file
      (when (probe-file mdbf-path)
        (%aformat verbose-stream "ASTORB: Replacing existing database~%")
        (delete-file mdbf-path))
      (rename-file tmp-path mdbf-path)

      #+sbcl (sb-ext:gc :full t)

      (%aformat verbose-stream "ASTORB: Conversion complete. Created ~A~%" mdbf-path)

      ;; Return path and MJD
      (values mdbf-path mjd-elements))))


;;; ============================================================================
;;; Database Loading and Initialization
;;; ============================================================================

(defun update-to-latest-astorb (&key
                                  (verbose-stream *astorb-info-output-stream*)
                                  (set-the-astorb t)
                                  (ntries 3))
  "Refresh the system to the newest astorb from Lowell, convert to mmap format,
and optionally load it."
  (let ((astorb-file
          (retrieve-newest-astorb-file/iterate
           :ntries ntries
           :verbose-stream (and (not *astorb-quiet*) verbose-stream))))
    (multiple-value-bind (mdbf-path mjd-elements)
        (convert-astorb-text-to-mmap astorb-file :verbose-stream verbose-stream)
      (when set-the-astorb
        (with-astorb-lock
          (when *the-astorb*
            (close-astorb-database *the-astorb*))
          (setf *the-astorb* (open-astorb-database mdbf-path :epoch-mjd mjd-elements))
          (setf *astorb-file* mdbf-path)))
      mdbf-path)))


(defun read-astorb-on-initialization (&key (verbose-stream *astorb-info-output-stream*))
  "Initialize the astorb database on package load."
  (let* ((mdbf-file-list (get-astorb-mdbf-file-list))
         (mdbf-file (first mdbf-file-list))
         (astorb-file-list (get-astorb-file-list))
         (astorb-file (first astorb-file-list)))
    (cond
      ;; Case 1: We have an existing mdbf file
      (mdbf-file
       (let ((mjd-elements (describe-astorb-file mdbf-file :verbose-stream verbose-stream)))
         (with-astorb-lock
           (setf *astorb-file-list* mdbf-file-list)
           (setf *astorb-file* mdbf-file)
           (setf *the-astorb* (open-astorb-database mdbf-file :epoch-mjd mjd-elements)))
         (%aformat verbose-stream "ASTORB: Opened database ~A~%" mdbf-file)))

      ;; Case 2: We have a text file but no mdbf - convert it
      (astorb-file
       (%aformat verbose-stream "ASTORB: No .mdbf file found. Converting from text file.~%")
       (multiple-value-bind (mdbf-path mjd-elements)
           (convert-astorb-text-to-mmap astorb-file :verbose-stream verbose-stream)
         (with-astorb-lock
           (setf *astorb-file-list* (list mdbf-path))
           (setf *astorb-file* mdbf-path)
           (setf *the-astorb* (open-astorb-database mdbf-path :epoch-mjd mjd-elements)))))

      ;; Case 3: No files - try to download if configured
      (*read-astorb-on-load*
       (if *download-astorb-automatically*
           (progn
             (%aformat verbose-stream "ASTORB: No database present. Downloading from Lowell.~%")
             (when (not (update-to-latest-astorb :verbose-stream verbose-stream))
               (error "ASTORB: Failed to download astorb database from web.")))
           (%aformat verbose-stream
                     "ASTORB: Warning - no astorb database present and auto-download disabled.~%")))

      (t
       (%aformat verbose-stream
                 "ASTORB: Warning - no astorb database present and not auto-loading.~%")))))


(eval-when (:load-toplevel)
  (when (and *read-astorb-on-load* (not *the-astorb*))
    (read-astorb-on-initialization)))
