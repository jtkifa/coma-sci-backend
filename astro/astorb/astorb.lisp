;;;; astorb.lisp - Memory-mapped asteroid orbit database
;;;;
;;;; This file defines the binary table schema for storing asteroid orbits
;;;; and the wrapper struct providing the astorb API.

(in-package :astorb)

;;; ============================================================================
;;; Binary Table Schema Definition
;;; ============================================================================

(mmapped-table:define-binary-table asteroid-orbit
  ;; Asteroid catalog number (0 for unnumbered)
  (astnum       :uint32)
  ;; Name (up to 19 chars in data)
  (name         :string :length 20)
  ;; Scrubbed lowercase name for searching
  (sname        :string :length 20)
  ;; H magnitude
  (hmag         :single-float)
  ;; Phase slope parameter G
  (g            :single-float)
  ;; IRAS diameter in km
  (iras-km      :single-float)
  ;; IRAS classification (single letter, possibly with question mark)
  (iras-class   :string :length 3)
  ;; Classification codes
  (code1        :uint8)
  (code2        :uint8)
  (code3        :uint8)
  (code4        :uint8)
  (code5        :uint8)
  (code6        :uint8)
  ;; Orbital arc in days
  (orbarc       :single-float)
  ;; Number of observations
  (nobs         :uint32)
  ;; Epoch of osculation YYYYMMDD
  (epoch-osc    :uint32)
  ;; Mean anomaly (degrees)
  (mean-anomaly :double-float)
  ;; Argument of perihelion (degrees)
  (arg-peri     :double-float)
  ;; Longitude of ascending node (degrees)
  (anode        :double-float)
  ;; Orbital inclination (degrees)
  (orbinc       :double-float)
  ;; Eccentricity
  (ecc          :double-float)
  ;; Semi-major axis (AU)
  (a            :double-float)
  ;; Date of orbit computation YYYYMMDD
  (orbit-date   :uint32))


;;; ============================================================================
;;; Wrapper Struct for API Compatibility
;;; ============================================================================

(defstruct (astorb (:conc-name astorb-))
  "Wrapper providing accessor API over mmap database.
The TABLE slot holds the mmapped-table:mapped-table instance."
  (table nil)
  (file-path nil :type (or null string))
  (epoch-of-elements 0d0 :type double-float))

(defmethod print-object ((obj astorb) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A rows, file=~A"
            (if (astorb-table obj)
                (mmapped-table:get-header-row-count (astorb-table obj))
                0)
            (or (astorb-file-path obj) "none"))))


;;; ============================================================================
;;; Row Count Accessor
;;; ============================================================================

(defun astorb-n (astorb)
  "Return the number of asteroid records in the database."
  (if (astorb-table astorb)
      (mmapped-table:get-header-row-count (astorb-table astorb))
      0))


;;; ============================================================================
;;; Field Accessors via table-peek
;;; ============================================================================

(defun astorb-astnum (astorb idx)
  "Return asteroid number for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'astnum))

(defun astorb-name (astorb idx)
  "Return asteroid name for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'name))

(defun astorb-sname (astorb idx)
  "Return scrubbed (lowercase, alphanumeric only) name for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'sname))

(defun astorb-hmag (astorb idx)
  "Return H magnitude for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'hmag))

(defun astorb-g (astorb idx)
  "Return phase slope G for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'g))

(defun astorb-iras-km (astorb idx)
  "Return IRAS diameter in km for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'iras-km))

(defun astorb-iras-class (astorb idx)
  "Return IRAS class character code for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'iras-class))

(defun astorb-code1 (astorb idx)
  "Return code1 for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'code1))

(defun astorb-code2 (astorb idx)
  "Return code2 for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'code2))

(defun astorb-code3 (astorb idx)
  "Return code3 for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'code3))

(defun astorb-code4 (astorb idx)
  "Return code4 for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'code4))

(defun astorb-code5 (astorb idx)
  "Return code5 for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'code5))

(defun astorb-code6 (astorb idx)
  "Return code6 for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'code6))

(defun astorb-orbarc (astorb idx)
  "Return orbital arc in days for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'orbarc))

(defun astorb-nobs (astorb idx)
  "Return number of observations for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'nobs))

(defun astorb-epoch-osc (astorb idx)
  "Return epoch of osculation (YYYYMMDD) for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'epoch-osc))

(defun astorb-mean-anomaly (astorb idx)
  "Return mean anomaly in degrees for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'mean-anomaly))

(defun astorb-arg-peri (astorb idx)
  "Return argument of perihelion in degrees for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'arg-peri))

(defun astorb-anode (astorb idx)
  "Return longitude of ascending node in degrees for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'anode))

(defun astorb-orbinc (astorb idx)
  "Return orbital inclination in degrees for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'orbinc))

(defun astorb-ecc (astorb idx)
  "Return eccentricity for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'ecc))

(defun astorb-a (astorb idx)
  "Return semi-major axis in AU for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'a))

(defun astorb-orbit-date (astorb idx)
  "Return orbit computation date (YYYYMMDD) for row IDX."
  (mmapped-table:table-peek (astorb-table astorb) idx 'orbit-date))


;;; ============================================================================
;;; Database File Operations
;;; ============================================================================

(defun make-mdbf-filename-from-source (source-filename)
  "Generate the .mdbf filename from the source astorb text filename."
  (let* ((base (if (string-utils:string-ends-with source-filename ".gz")
                   (subseq source-filename 0 (- (length source-filename) 3))
                   source-filename)))
    (format nil "~A.mdbf" base)))

(defun open-astorb-database (mdbf-path &key (epoch-mjd 0d0))
  "Open an existing astorb database file and return an astorb struct.
Opens read-only since the database is only queried, never modified after creation."
  (let ((table (mmapped-table:open-binary-table mdbf-path :type 'asteroid-orbit :read-only t)))
    (make-astorb :table table
                 :file-path mdbf-path
                 :epoch-of-elements (float epoch-mjd 1d0))))

(defun close-astorb-database (astorb)
  "Close the memory-mapped database."
  (when (and astorb (astorb-table astorb))
    (mmapped-table:close-binary-table (astorb-table astorb))
    (setf (astorb-table astorb) nil))
  t)
