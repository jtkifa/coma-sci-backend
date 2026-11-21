;;;; fonts-and-text.lisp — example fonts-and-text
;;;;
;;;; The named fonts, PUT-TEXT alignment about an anchor, rich text runs
;;;; (superscripts) and fallback for characters a Hershey font lacks.
;;;;
;;;; Uses: with-font, plot-text, with-style, plot-line

(in-package :cl-cairoplot-examples)

(define-example fonts-and-text (d :aspect 0.8)
  "The named fonts, PUT-TEXT alignment about an anchor, rich text runs
   (superscripts) and fallback for characters a Hershey font lacks."
  (page-title d "Fonts and text")
  (loop for name in '(:normal :roman :italic :script :sans :serif :mono)
        for y = 0.88 then (- y 0.055)
        do (with-font (d name :size 18)
             (plot-text d 0.05 y (format nil "~(~S~): The quick brown fox 0123" name)
                        :coords :device)))
  (plot-text d 0.05 0.47 '("E = mc" ("2" :scale 0.6 :rise 0.5)
                           "   10" ("-3" :scale 0.6 :rise 0.5)
                           "   H" ("2" :scale 0.6 :rise -0.25) "O"
                           "   αβγ ×   ☉ ♃ ♄ ♇ ♈ ♓")
             :coords :device :size 18)
  (flet ((cross (x y)
           (with-style (d :color :red :line-width 0.5)
             (plot-line d (- x 0.01) y (+ x 0.01) y :coords :device)
             (plot-line d x (- y 0.012) x (+ y 0.012) :coords :device))))
    (loop for ha in '(:left :center :right)
          for x from 0.15 by 0.35
          do (loop for va in '(:top :center :baseline :bottom)
                   for y = 0.36 then (- y 0.08)
                   do (cross x y)
                      (plot-text d x y (format nil "~(~A/~A~)" ha va)
                                 :coords :device :halign ha :valign va :size 13)))))
