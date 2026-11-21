;;;; time-axes.lisp — date and time axes
;;;;
;;;; An axis in time has :PLOT coordinates in one of the time SCALEs
;;;;   :MJD        modified Julian date (days; MJD 0 = 1858-11-17 0h)
;;;;   :JD         Julian date (days)
;;;;   :UNIX       seconds since 1970-01-01 0h
;;;;   :UNIVERSAL  Common Lisp universal time (seconds since 1900)
;;;; all read as UTC (UTC-OFFSET hours shifts the labels to local time).
;;;; (TIME-TICKS :SCALE :MJD) is a tick generator for DRAW-BOX's :X-TICKS
;;;; or :Y-TICKS: about TARGET ticks at round calendar steps, from
;;;; seconds to centuries (on month and year starts, and days of the
;;;; month 1, 1+n, ...), labeled concisely:
;;;;
;;;;   years    2023  2024  2025
;;;;   months   Oct 2024  Nov  Dec  2025  Feb        (the year at January)
;;;;   days     Mar 10  12  14  ...  30  Apr  3      (the month at its start)
;;;;   hours    20:00  22:00  Mar 15  02:00          (the date at midnight)
;;;;
;;;; with the first tick of days and months carrying the next larger
;;;; unit for context (give the date of clock times in the axis label).
;;;; CALENDAR-TO-MJD, MJD-TO-CALENDAR and ISO-TO-MJD convert dates.

(in-package :cl-cairoplot)

(defparameter +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;;; ============================================================
;;; Calendar arithmetic (proleptic Gregorian, UTC)
;;; ============================================================

(defun days-from-civil (y m d)
  "Days since 1970-01-01 of the date Y-M-D (H. Hinnant's algorithm)."
  (let* ((y (if (<= m 2) (1- y) y))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (1- d)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun civil-from-days (days)
  "Values year, month, day of DAYS (an integer) since 1970-01-01."
  (let* ((z (+ days 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (doy (- doe (- (+ (* 365 yoe) (floor yoe 4)) (floor yoe 100))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (d (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
         (m (if (< mp 10) (+ mp 3) (- mp 9))))
    (values (+ yoe (* era 400) (if (<= m 2) 1 0)) m d)))

(defun days-in-month (y m)
  (- (days-from-civil (if (= m 12) (1+ y) y) (if (= m 12) 1 (1+ m)) 1)
     (days-from-civil y m 1)))

(defconstant +mjd-unix-epoch+ 40587 "The MJD of 1970-01-01.")

(defun calendar-to-mjd (year month day &optional (hour 0) (minute 0) (second 0))
  "The MJD of the UTC date and time."
  (+ +mjd-unix-epoch+ (days-from-civil year month day)
     (/ (+ (* 3600 hour) (* 60 minute) second) 86400d0)))

(defun mjd-to-calendar (mjd)
  "Values year, month, day, hour, minute, second (UTC) of MJD."
  (let* ((seconds (* (- mjd +mjd-unix-epoch+) 86400d0)))
    (unix-to-calendar seconds)))

(defun unix-to-calendar (seconds)
  (multiple-value-bind (days s) (floor seconds 86400)
    (multiple-value-bind (y m d) (civil-from-days days)
      (multiple-value-bind (h rest) (floor s 3600)
        (multiple-value-bind (mi sec) (floor rest 60)
          (values y m d h mi sec))))))

(defun calendar-to-unix (y m d &optional (h 0) (mi 0) (s 0))
  (+ (* 86400 (days-from-civil y m d)) (* 3600 h) (* 60 mi) s))

(defun iso-to-mjd (string)
  "The MJD of an ISO 8601 UTC date or date-time, \"2024-03-14\",
   \"2024-03-14T21:30\" or \"2024-03-14 21:30:15.5\"."
  (flet ((field (start end) (parse-integer string :start start :end end)))
    (let* ((y (field 0 4)) (m (field 5 7)) (d (field 8 10))
           (n (length string))
           (h (if (>= n 13) (field 11 13) 0))
           (mi (if (>= n 16) (field 14 16) 0))
           (s (if (>= n 19)
                  (let ((*read-default-float-format* 'double-float))
                    (let ((v (read-from-string string t nil :start 17)))
                      (check-type v real)
                      v))
                  0)))
      (calendar-to-mjd y m d h mi s))))

(defun time-to-unix (value scale)
  "VALUE in the time SCALE as Unix seconds."
  (ecase scale
    (:mjd (* (- value +mjd-unix-epoch+) 86400d0))
    (:jd (* (- value 2440587.5d0) 86400d0))
    (:unix (float value 1d0))
    (:universal (float (- value 2208988800) 1d0))))

(defun unix-to-time (seconds scale)
  "Unix SECONDS in the time SCALE."
  (ecase scale
    (:mjd (+ +mjd-unix-epoch+ (/ seconds 86400d0)))
    (:jd (+ 2440587.5d0 (/ seconds 86400d0)))
    (:unix seconds)
    (:universal (+ seconds 2208988800))))

;;; ============================================================
;;; Ticks
;;; ============================================================

(defparameter +time-steps+
  ;; (unit count approximate-seconds minor-subdivisions)
  '((:second 1 1 1) (:second 2 2 2) (:second 5 5 5) (:second 10 10 5) (:second 15 15 3)
    (:second 30 30 3)
    (:minute 1 60 4) (:minute 2 120 4) (:minute 5 300 5) (:minute 10 600 5)
    (:minute 15 900 3) (:minute 30 1800 3)
    (:hour 1 3600 4) (:hour 2 7200 4) (:hour 3 10800 3) (:hour 6 21600 6) (:hour 12 43200 4)
    (:day 1 86400 4) (:day 2 172800 2) (:day 5 432000 5) (:day 10 864000 10)
    (:month 1 2629746 0) (:month 2 5259492 2) (:month 3 7889238 3) (:month 6 15778476 6)
    (:year 1 31556952 4) (:year 2 63113904 2) (:year 5 157784760 5) (:year 10 315569520 5)
    (:year 20 631139040 4) (:year 50 1577847600 5) (:year 100 3155695200 5)
    (:year 200 6311390400 4) (:year 500 15778476000 5) (:year 1000 31556952000 5))
  "Candidate major steps, each with its minor subdivisions (months
   and years are subdivided by calendar months).")

(defun choose-time-step (span target)
  (or (find-if (lambda (s) (<= (/ span (third s)) (* 1.4 target))) +time-steps+)
      (car (last +time-steps+))))

(defun fixed-step-times (lo hi step &optional (origin 0))
  "Multiples of STEP seconds (counted from ORIGIN) within LO..HI."
  (loop for k from (ceiling (- lo origin) step) to (floor (- hi origin) step)
        collect (+ origin (* k step))))

(defun month-start-times (lo hi every)
  "Unix times of the starts of months, every EVERY months (Jan, then
   each EVERY-th), within LO..HI."
  (multiple-value-bind (y m) (unix-to-calendar lo)
    (loop with year = y and month = m
          for time = (calendar-to-unix year month 1)
          while (<= time hi)
          when (and (>= time lo) (zerop (mod (1- month) every)))
            collect time
          do (if (= month 12) (setf month 1 year (1+ year)) (incf month)))))

(defun year-start-times (lo hi every)
  (let ((y0 (unix-to-calendar lo)) (y1 (unix-to-calendar hi)))
    (loop for y from (* every (floor y0 every)) to y1 by every
          for time = (calendar-to-unix y 1 1)
          when (<= lo time hi) collect time)))

(defun day-of-month-times (lo hi every)
  "Unix times of midnights on days of the month 1, 1+EVERY, ... (none
   within EVERY/2 days of the next month's 1st) within LO..HI."
  (multiple-value-bind (y m) (unix-to-calendar lo)
    (loop with year = y and month = m
          while (<= (calendar-to-unix year month 1) hi)
          nconc (let ((dim (days-in-month year month)))
                  (loop for d from 1 to dim by every
                        for time = (calendar-to-unix year month d)
                        when (and (<= lo time hi) (or (= d 1) (>= (- dim d -1) (/ every 2))))
                          collect time))
          do (if (= month 12) (setf month 1 year (1+ year)) (incf month)))))

(defun time-label (seconds unit first)
  "The concise label of a tick at SECONDS (local Unix time) for steps
   of UNIT: the larger unit where it changes, and with FIRST, context
   (for days and months; clock times show the date only at midnight)."
  (multiple-value-bind (y m d h mi s) (unix-to-calendar seconds)
    (let ((mon (aref +month-names+ (1- m)))
          (sec (round s)))
      (ecase unit
        (:year (format nil "~D" y))
        (:month (cond ((= m 1) (format nil "~D" y))
                      (first (format nil "~A ~D" mon y))
                      (t mon)))
        (:day (cond ((and (= m 1) (= d 1)) (format nil "~D" y))
                    ((= d 1) mon)
                    (first (format nil "~A ~D" mon d))
                    (t (format nil "~D" d))))
        ((:hour :minute :second)
         (let ((clock (if (eq unit :second)
                          (format nil "~2,'0D:~2,'0D:~2,'0D" h mi sec)
                          (format nil "~2,'0D:~2,'0D" h mi))))
           (if (and (zerop h) (zerop mi) (zerop sec))
               (format nil "~A ~D" mon d)
               clock)))))))

(defun time-ticks (&key (scale :mjd) (target 5) (utc-offset 0) (context t))
  "A tick generator for DRAW-BOX's :X-TICKS or :Y-TICKS on an axis of
   times in SCALE (:MJD, :JD, :UNIX or :UNIVERSAL; see the top of
   time-axes.lisp): about TARGET ticks at round calendar steps, labeled
   concisely in UTC + UTC-OFFSET hours, the first tick with CONTEXT."
  (lambda (a b)
    (let* ((offset (* 3600 utc-offset))
           ;; local Unix seconds, a millisecond wider so ticks on the ends
           ;; survive rounding through the scale
           (lo (- (+ offset (time-to-unix (min a b) scale)) 1d-3))
           (hi (+ (+ offset (time-to-unix (max a b) scale)) 1d-3)))
      (destructuring-bind (unit count seconds sub) (choose-time-step (- hi lo) target)
        (let* ((majors (ecase unit
                         ((:second :minute :hour) (fixed-step-times lo hi seconds))
                         (:day (if (= count 1)
                                   (fixed-step-times lo hi 86400)
                                   (day-of-month-times lo hi count)))
                         (:month (month-start-times lo hi count))
                         (:year (year-start-times lo hi count))))
               (minors (ecase unit
                         ((:second :minute :hour :day)
                          (if (and (eq unit :day) (> count 1))
                              (fixed-step-times lo hi 86400)
                              (fixed-step-times lo hi (/ seconds sub))))
                         (:month (if (> count 1) (month-start-times lo hi 1) '()))
                         (:year (cond ((= count 1) (month-start-times lo hi 3))
                                      (t (year-start-times lo hi (/ count sub))))))))
          (flet ((back (u) (unix-to-time (- u offset) scale)))
            (values (loop for u in majors
                          for k from 0
                          collect (list (back u) (time-label u unit (and context (zerop k)))))
                    (mapcar #'back (set-difference minors majors :test #'=)))))))))
