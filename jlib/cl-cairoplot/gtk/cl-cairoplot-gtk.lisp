;;;; cl-cairoplot-gtk.lisp — the :GTK output type: plots in a window
;;;;
;;;;   (open-device :gtk "title" :size 800 :aspect 0.75)
;;;;
;;;; opens a window (starting GTK-server if needed) SIZE x SIZE*ASPECT
;;;; points large, holding a canvas with RESOLUTION pixels per point: by
;;;; default the screen's scale factor, 2 on a Retina display, so the
;;;; plot is as sharp as the screen.  The device draws straight into the
;;;; canvas's pixels, memory GTK-server shares with Lisp, and a task in
;;;; GTK's event loop shows them whenever plotting calls have changed
;;;; them (DEVICE-CHANGES), at most every REFRESH-INTERVAL seconds: plots
;;;; appear as they are drawn, from the REPL or a program.
;;;;
;;;; Closing the window while the device is open only hides it
;;;; (SHOW-WINDOW brings it back), so drawing goes on safely.
;;;; CLOSE-DEVICE, or the end of WITH-DEVICE, shows the final picture and
;;;; leaves the window up; closing it then destroys it.  CLOSE-WINDOW
;;;; closes the device if need be and destroys the window at once.
;;;;
;;;; READ-CURSOR waits for a click or key press in the window (see the
;;;; generic function in plot-device.lisp).  The canvas's pointer and key
;;;; handlers, run in GTK's event thread, queue events while a
;;;; READ-CURSOR is waiting; it takes them in the calling thread.  A
;;;; rubber band is drawn over a copy of the pixels saved when it starts,
;;;; restored before each new band and at the end, so the plot beneath is
;;;; left as it was.

(defpackage #:cl-cairoplot-gtk
  (:use #:cl)
  (:local-nicknames (#:cp #:cl-cairoplot) (#:ez #:cl-ez-gtk-server))
  (:export #:gtk-device #:device-window #:device-canvas #:show-window #:close-window
           #:*refresh-interval* #:*band-color* #:demo #:cursor-demo))

(in-package #:cl-cairoplot-gtk)

(defvar *refresh-interval* 0.05
  "Seconds between checks for new drawing to show.")

(defvar *band-color* '(0.85 0.1 0.1)
  "The color READ-CURSOR draws rubber bands in.")

;;; Events from the window, for READ-CURSOR

(defstruct (cursor-events (:constructor make-cursor-events ()))
  (lock (bt:make-lock "cursor events"))
  (condition (bt:make-condition-variable))
  (queue '())
  (listening nil))                      ; true while a READ-CURSOR waits

(defun note-event (events event)
  "Queue EVENT, a list (:PRESS X Y BUTTON), (:MOTION X Y), (:KEY KEY) or
   (:CLOSE), if a READ-CURSOR is waiting."
  (bt:with-lock-held ((cursor-events-lock events))
    (when (cursor-events-listening events)
      (setf (cursor-events-queue events) (nconc (cursor-events-queue events) (list event)))
      (bt:condition-notify (cursor-events-condition events)))))

(defun take-events (events deadline)
  "Wait until EVENTS has some, and return them all; :TIMEOUT once the
   internal real time DEADLINE (or NIL for none) has passed."
  (bt:with-lock-held ((cursor-events-lock events))
    (loop
      (let ((queue (cursor-events-queue events)))
        (when queue
          (setf (cursor-events-queue events) '())
          (return queue)))
      (let ((left (and deadline
                       (/ (- deadline (get-internal-real-time))
                          internal-time-units-per-second))))
        (when (and left (<= left 0))
          (return :timeout))
        ;; wake now and then, in case a notification is missed
        (bt:condition-wait (cursor-events-condition events) (cursor-events-lock events)
                           :timeout (if left (min left 0.5) 0.5))))))

(defclass gtk-device (cp:plot-device)
  ((window :initarg :window :reader device-window)
   (canvas :initarg :canvas :reader device-canvas)
   (events :initarg :events :reader device-events)
   (task :initform nil :documentation "The event loop task that refreshes.")
   (shown :initform -1 :documentation "DEVICE-CHANGES when last shown."))
  (:documentation "A plot device drawing into a window's canvas."))

(defun ensure-gtk-server ()
  (unless (and ez:*connection* (ez:connection-alive-p ez:*connection*))
    (ez:start-gtk-server)))

(defmethod cp:output-type-buffer ((type (eql :gtk)) title width height &key resolution)
  (ensure-gtk-server)
  (let* ((events (make-cursor-events))
         (window (make-instance 'ez:window :title (or title "cl-cairoplot")
                                           :default-width width :default-height height
                                           :resizable nil :hide-on-close t
                                           :on-close (lambda (&rest data)
                                                       (declare (ignore data))
                                                       (note-event events '(:close))))))
    ;; shown first, so it knows the scale factor of its screen
    (setf (ez:visible window) t)
    (let* ((resolution (or resolution (max 1 (ez:scale-factor window))))
           (pixel-width (round (* resolution width)))
           (pixel-height (round (* resolution height)))
           ;; RESOLUTION pixels per point, shown WIDTH x HEIGHT points large
           (canvas (make-instance
                    'ez:canvas :size (list pixel-width pixel-height)
                               :can-shrink t :content-fit :fill
                               :width-request width :height-request height
                               :on-press (lambda (window widget &key x y button &allow-other-keys)
                                           (declare (ignore window widget))
                                           (note-event events (list :press x y button)))
                               :on-motion (lambda (window widget &key x y &allow-other-keys)
                                            (declare (ignore window widget))
                                            (note-event events (list :motion x y)))
                               :on-key (lambda (window widget &key key &allow-other-keys)
                                         (declare (ignore window widget))
                                         (note-event events (list :key key))))))
      (ez:add-child window canvas)
      (values (cp:make-foreign-pixel-buffer pixel-width pixel-height
                                            :pointer (ez:canvas-pixels canvas))
              'gtk-device
              (list :window window :canvas canvas :events events)))))

(defun refresh-if-changed (device)
  "Show DEVICE's canvas if plotting changed it since it was last shown.
   Returns the seconds until the next check, or NIL to stop checking."
  (with-slots (canvas shown) device
    (when (ez:canvas-surface canvas)        ; NIL once the canvas is gone
      (let ((changes (cp:device-changes device)))
        (unless (= changes shown)
          (setf shown changes)
          (ez:refresh canvas)))
      *refresh-interval*)))

(defmethod cp:device-opened ((device gtk-device))
  (with-slots (window task) device
    (ez:refresh (device-canvas device))     ; the background
    (setf (ez:visible window) t
          task (ez:add-loop-task (lambda () (refresh-if-changed device)))))
  device)

(defun show-window (device)
  "Show DEVICE's window again after it was closed while still open."
  (setf (ez:visible (device-window device)) t)
  device)

(defmethod cp:close-device ((device gtk-device))
  (with-slots (window canvas task) device
    (cp:flush-canvas device)
    (call-next-method)                      ; no more drawing into the pixels
    (when task
      (ez:remove-loop-task task)
      (setf task nil))
    (when (ez:canvas-surface canvas)
      (ez:refresh canvas)
      ;; leave a shown window up, now to be destroyed when closed;
      ;; destroy one already closed (hidden)
      (if (ez:visible window)
          (setf (ez:hide-on-close window) nil)
          (ez:destroy window))))
  device)

(defun close-window (device)
  "Close DEVICE if it is still open, and destroy its window."
  (when (cp:canvas-open-p device)
    (cp:close-device device))
  (let ((window (device-window device)))
    (when (ez:canvas-surface (device-canvas device))   ; not destroyed yet
      (ez:destroy window)))
  nil)

;;; ============================================================
;;; Reading the cursor
;;; ============================================================

(defun save-pixels (device)
  "A copy of DEVICE's pixels, as a foreign pointer, to free with FREE-PIXELS."
  (let* ((buffer (cp:canvas-destination device))
         (size (cp:pixel-buffer-size buffer))
         (copy (cffi:foreign-alloc :uint8 :count size)))
    (cp:flush-canvas device)
    (cffi:foreign-funcall "memcpy" :pointer copy :pointer (cp:pixel-buffer-pointer buffer)
                                   :size size :pointer)
    copy))

(defun restore-pixels (device copy)
  "Put back DEVICE's pixels saved in COPY by SAVE-PIXELS."
  (let ((buffer (cp:canvas-destination device)))
    (cffi:foreign-funcall "memcpy" :pointer (cp:pixel-buffer-pointer buffer) :pointer copy
                                   :size (cp:pixel-buffer-size buffer) :pointer)
    ;; cairo must not trust what it may have cached of the pixels
    (cl-cairo2:surface-mark-dirty (cp::canvas-surface device))
    (incf (cp:device-changes device))))

(defun draw-band (device mode anchor x y viewport)
  "Draw MODE's rubber band in VIEWPORT for the pointer at plot X, Y."
  (multiple-value-bind (x1 x2 y1 y2) (cp:viewport-window viewport)
    (let ((cp::*record-footprint* nil))     ; not part of the plot
      (cp:with-style (viewport :color *band-color* :line-width 1)
        (ecase mode
          (:line (cp:plot-line viewport (first anchor) (second anchor) x y))
          (:rectangle (cp:plot-rectangle viewport (first anchor) (second anchor) x y))
          (:horizontal (cp:plot-line viewport x1 y x2 y))
          (:vertical (cp:plot-line viewport x y1 x y2))
          (:crosshair (cp:plot-line viewport x1 y x2 y)
                      (cp:plot-line viewport x y1 x y2)))))))

(defun button-key (button)
  (case button (1 :left) (2 :middle) (3 :right) (t button)))

(defmethod cp:read-cursor ((device gtk-device) &key viewport (mode :none) anchor timeout)
  (check-type mode (member :none :line :rectangle :horizontal :vertical :crosshair))
  (when (and (member mode '(:line :rectangle)) (null anchor))
    (error "Rubber band ~S needs an :ANCHOR (X Y)." mode))
  (unless (cp:canvas-open-p device)
    (error "~A is closed." device))
  (let* ((events (device-events device))
         (canvas (device-canvas device))
         ;; pointer coordinates are in the widget's points; the page's units
         ;; match them unless the window was made another size
         (widget-width (ez:gtk-call "gtk_widget_get_width" :int '(:widget) canvas))
         (scale (if (plusp widget-width) (/ (cp:canvas-width device) widget-width) 1))
         (band-viewport (or viewport (cp:device-viewport device)))
         (deadline (and timeout (+ (get-internal-real-time)
                                   (round (* timeout internal-time-units-per-second)))))
         (saved nil)
         (last nil))                        ; the last pointer position seen
    (flet ((result (cx cy key)
             (multiple-value-bind (x y vp) (cp:locate-point device cx cy viewport)
               (return-from cp:read-cursor (values x y key vp)))))
      ;; the keyboard goes to the canvas
      (ez:gtk-call "gtk_widget_grab_focus" :bool '(:widget) canvas)
      (bt:with-lock-held ((cursor-events-lock events))
        (setf (cursor-events-queue events) '()
              (cursor-events-listening events) t))
      (unwind-protect
           (progn
             (unless (eq mode :none)
               (setf saved (save-pixels device)))
             (loop
               (let ((batch (take-events events deadline)))
                 (when (eq batch :timeout)
                   (return nil))
                 (dolist (event batch)
                   (ecase (first event)
                     (:motion (setf last (list (* scale (second event)) (* scale (third event)))))
                     (:press (destructuring-bind (x y button) (rest event)
                               (result (* scale x) (* scale y) (button-key button))))
                     (:key
                      (if last
                          (result (first last) (second last) (second event))
                          ;; no position seen yet: the middle of the viewport
                          (multiple-value-bind (x1 x2 y1 y2) (cp:viewport-window band-viewport)
                            (return-from cp:read-cursor
                              (values (/ (+ x1 x2) 2) (/ (+ y1 y2) 2) (second event)
                                      band-viewport)))))
                     (:close (return-from cp:read-cursor nil))))
                 ;; a new rubber band for the latest position
                 (when (and saved last)
                   (restore-pixels device saved)
                   (multiple-value-bind (x y vp)
                       (cp:locate-point device (first last) (second last)
                                        (if (member mode '(:line :rectangle))
                                            band-viewport
                                            viewport))
                     (draw-band device mode anchor x y vp))))))
        (bt:with-lock-held ((cursor-events-lock events))
          (setf (cursor-events-listening events) nil
                (cursor-events-queue events) '()))
        ;; the plot as it was, without the band
        (when saved
          (restore-pixels device saved)
          (cffi:foreign-free saved))))))
