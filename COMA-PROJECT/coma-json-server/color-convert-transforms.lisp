;; define the actual color transforms

(in-package coma-json-server)

(eval-when (:load-toplevel)
  (setf *color-transforms-list* nil)) ;; clear this before filling it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; phot-transforms/ps1.lisp

;;(defun gsdss-from-grps1 (gps1 rps1)
;;  (%ps1-transform gps1 gps1 rps1 0.013 0.145 0.019))
(add-color-transform
 (make-color-transform
  :provides :gsdss
  :needs '(:gps1 :rps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1)
	  (%color-eval-with-errors 'phot-transforms:gsdss-from-grps1
				   (vector  gps1   rps1)
				   (vector egps1  erps1)))))
;;(defun rsdss-from-grps1 (gps1 rps1)
;;  (%ps1-transform rps1 gps1 rps1 
;;		  (+ -0.001 0.0233) ;; correction to Tonry et al
;;		  0.004 0.007))
(add-color-transform
 (make-color-transform
  :provides :rsdss
  :needs '(:gps1 :rps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1)
	  (%color-eval-with-errors 'phot-transforms:rsdss-from-grps1
				   (vector  gps1   rps1)
				   (vector egps1  erps1)))))


;;(defun isdss-from-grips1 (gps1 rps1 ips1)
;;  (%ps1-transform ips1 gps1 rps1 -0.005 0.011 0.010))	      
(add-color-transform
 (make-color-transform
  :provides :isdss
  :needs '(:gps1 :rps1 :ips1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1 ips1 eips1)
	  (%color-eval-with-errors 'phot-transforms:isdss-from-grips1
				   (vector  gps1   rps1   ips1)
				   (vector egps1  erps1  eips1)))))

;; (defun zsdss-from-grzps1 (gps1 rps1 zps1)
;;   (%ps1-transform zps1 gps1 rps1 0.013 -0.039 -0.012))
(add-color-transform
 (make-color-transform
  :provides :zsdss
  :needs '(:gps1 :rps1 :zps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1 zps1 ezps1)
	  (%color-eval-with-errors 'phot-transforms:zsdss-from-grzps1
				   (vector  gps1   rps1   zps1)
				   (vector egps1  erps1  ezps1)))))

;; (defun zsdss-from-gryps1 (gps1 rps1 yps1)
;;   (%ps1-transform yps1 gps1 rps1 -0.031 0.111 0.004))
(add-color-transform
 (make-color-transform
  :provides :zsdss
  :needs '(:gps1 :rps1 :yps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1 yps1 eyps1)
	  (%color-eval-with-errors 'phot-transforms:zsdss-from-gryps1
				   (vector  gps1   rps1   yps1)
				   (vector egps1  erps1  eyps1)))))


;; (defun rsdss-from-grwps1 (gps1 rps1 wps1)
;;   (%ps1-transform wps1 gps1 rps1 -0.024 -0.149 0.155))
(add-color-transform
 (make-color-transform
  :provides :rsdss
  :needs '(:gps1 :rps1 :wps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1 wps1 ewps1)
	  (%color-eval-with-errors 'phot-transforms:rsdss-from-grwps1
				   (vector  gps1   rps1   wps1)
				   (vector egps1  erps1  ewps1)))))


;; (defun Bj-from-grps1 (gps1 rps1)
;;   (%ps1-transform gps1 gps1 rps1 0.212 0.556 0.034))
(add-color-transform
 (make-color-transform
  :provides :bj
  :needs '(:gps1 :rps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1)
	  (%color-eval-with-errors 'phot-transforms:bj-from-grps1
				   (vector  gps1   rps1)
				   (vector egps1  erps1)))))

;; (defun Vj-from-grps1/r (gps1 rps1)
;;   "This version uses rps1 as the mag to correct with color"
;;   (%ps1-transform rps1 gps1 rps1 0.005 0.462 0.013))
(add-color-transform
 (make-color-transform
  :provides :vj
  :needs '(:gps1 :rps1)
  :func (lambda (gps1 egps1 rps1 erps1)
	  (%color-eval-with-errors 'phot-transforms:vj-from-grps1/r
				   (vector  gps1   rps1)
				   (vector egps1  erps1)))))

;; (defun Rc-from-grps1 (gps1 rps1)
;;   (%ps1-transform rps1 gps1 rps1 -0.137 -0.108 -0.029))
(add-color-transform
 (make-color-transform
  :provides :rc
  :needs '(:gps1 :rps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1)
	  (%color-eval-with-errors 'phot-transforms:rc-from-grps1
				   (vector  gps1   rps1)
				   (vector egps1  erps1)))))

;; (defun Ic-from-grips1 (gps1 rps1 ips1)
;;   (%ps1-transform ips1 gps1 rps1 -0.366 -0.136 -0.018))
(add-color-transform
 (make-color-transform
  :provides :ic
  :needs '(:gps1 :rps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1 ips1 eips1)
	  (%color-eval-with-errors 'phot-transforms:ic-from-grips1
				   (vector  gps1   rps1   ips1)
				   (vector egps1  erps1  eips1)))))

;; (defun Vj-from-grwps1 (gps1 rps1 wps1)
;;   (%ps1-transform wps1 gps1 rps1 -0.021 0.299 0.187))
(add-color-transform
 (make-color-transform
  :provides :vj
  :needs '(:gps1 :rps1 :wps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1 wps1 ewps1)
	  (%color-eval-with-errors 'phot-transforms:vj-from-grwps1
				   (vector  gps1   rps1   wps1)
				   (vector egps1  erps1  ewps1)))))

;; (defun Vj-from-grps/g (gps1 rps1)
;;   "This version uses gps1 as the mag to correct with color"
;;   (%ps1-transform gps1 gps1 rps1 0.005 -0.536 0.011))

(add-color-transform  ;;  this is a duplicate gps1,rps1->vj and won't get added
 (make-color-transform
  :provides :vj
  :needs '(:gps1 :rps1)
  :name "Tonry PS1"
  :func (lambda (gps1 egps1 rps1 erps1)
	  (%color-eval-with-errors 'phot-transforms:vj-from-grps1/g
				   (vector  gps1   rps1   rps1)
				   (vector egps1  erps1  erps1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; phot-transforms/sdss.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Favor Lupton - the others will replace them only if Lupton is absent
(add-color-transform
 (make-color-transform 
  :provides :bj
  :needs '(:usdss :gsdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-u+g-to-bj/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))
;;
(add-color-transform
 (make-color-transform 
  :provides :bj
  :needs '(:gsdss :rsdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-g+r-to-bj/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))

(add-color-transform
 (make-color-transform 
  :provides :vj
  :needs '(:usdss :gsdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-u+g-to-vj/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))
;;
(add-color-transform
 (make-color-transform 
  :provides :vj
  :needs '(:gsdss :rsdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-g+r-to-vj/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))

(add-color-transform
 (make-color-transform 
  :provides :rc
  :needs '(:gsdss :rsdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-g+r-to-rc/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))
;;
(add-color-transform
 (make-color-transform 
  :provides :rc
  :needs '(:rsdss :isdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-r+i-to-rc/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))


(add-color-transform
 (make-color-transform 
  :provides :ic
  :needs '(:rsdss :isdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-r+i-to-ic/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))
;;
(add-color-transform
 (make-color-transform 
  :provides :ic
  :needs '(:isdss :zsdss)
  :name "Lupton SDSS"
  :func (lambda (m1 em1 m2 em2) ;; rename for simplicity
	  (%color-eval-with-errors
	   'phot-transforms:sloan-i+z-to-ic/lupton
	   (vector m1 m2)
	   (vector em1 em2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (defun sloan-gr-to-VB/fukugita  (g r)
;;   "convert sloan g,r to V,B - from Fukugita et al AJ 111 1748 - error is
;; their sample set is about 0.03 +/- 0.03"
;;   (let ((gg (+ g  0.12d0))
;; 	(rr (+ r -0.11d0)))
;;     (values
;;      (+ (* 0.467d0 gg) (* 0.533d0 rr))    ;; V
;;      (+ (* 1.419d0 gg) (* -0.419d0 rr)))));; B

(add-color-transform
 (make-color-transform 
  :provides :vj
  :needs '(:gsdss :rsdss)
  :name "Fukugita SDSS"
  :func (lambda (gsdss egsdss rsdss ersdss)
	  (%color-eval-with-errors
	   (lambda (g r)
	     (nth-value 0 ;; Vj
			(phot-transforms:sloan-gr-to-vb/fukugita g r)))
	   (vector gsdss rsdss)
	   (vector egsdss ersdss)))))
(add-color-transform
 (make-color-transform 
  :provides :bj
  :needs '(:gsdss :rsdss)
  :name "Fukugita SDSS"
  :func (lambda (gsdss egsdss rsdss ersdss)
	  (%color-eval-with-errors
	   (lambda (g r)
	     (nth-value 1 ;; Bj
			(phot-transforms:sloan-gr-to-vb/fukugita g r)))
	   (vector gsdss rsdss)
	   (vector egsdss ersdss)))))



;; (defun VB-to-sloan-gr/fukugita (v b)
;;   "convert V,B to Sloan g,r - from Fukugita et al AJ 111 1748 -
;; derived by inverting above - error is their sample set is about
;; 0.03 +/- 0.03"
;;   (values
;;    (+ v (* 0.56d0  (- b v)) -0.12d0)   ;; gsdss
;;    (+ v (* -0.49d0 (- b v))  0.11d0))) ;; rsdss
(add-color-transform
 (make-color-transform 
  :provides :gsdss
  :needs '(:vj :bj)
  :name "Fukugita SDSS"
  :func (lambda (vj evj bj ebj)
	  (%color-eval-with-errors
	   (lambda (v b)
	     (nth-value 0 ;; gsdss
			(phot-transforms:vb-to-sloan-gr/fukugita v b)))
	   (vector vj  bj)
	   (vector evj ebj)))))
(add-color-transform
 (make-color-transform 
  :provides :rsdss
  :needs '(:vj :bj)
  :name "Fukugita SDSS"
  :func (lambda (vj evj bj ebj)
	  (%color-eval-with-errors
	   (lambda (v b)
	     (nth-value 1 ;; rsdss
			(phot-transforms:vb-to-sloan-gr/fukugita v b)))
	   (vector vj  bj)
	   (vector evj ebj)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; phot-transforms/atlas.lisp


;; add the same transform from ATLAS to AB,PS1,SDSS as synonyms
(let ((transform
	(make-color-transform 
	 :provides :catlas
	 :needs '(:gab :rab)
	 :func (lambda (gab egab rab erab)
		  (%color-eval-with-errors
		   'phot-transforms:catlas-from-gab+rab 
		   (vector gab rab)
		   (vector egab erab))))))
  (add-color-transform transform)
  (add-color-transform
   (make-synonym-color-transform transform :needs '(:gsdss :rsdss)))
  (add-color-transform
   (make-synonym-color-transform transform :needs '(:gps1 :rps1))))


(let ((transform
	(make-color-transform 
	 :provides :oatlas
	 :needs '(:rab :iab)
	 :name "Atlas"
	 :func (lambda (rab erab iab eiab)
		  (%color-eval-with-errors
		   'phot-transforms:oatlas-from-rab+iab
		   (vector rab iab)
		   (vector erab eiab))))))
  (add-color-transform transform)
  (add-color-transform
   (make-synonym-color-transform transform :needs '(:rsdss :isdss)))
  (add-color-transform
   (make-synonym-color-transform transform :needs '(:rps1 :ips1))))


(let ((transform
	(make-color-transform 
	 :provides :gab
	 :needs '(:catlas :oatlas)
	 :name "Atlas"
	 :func (lambda (catlas ecatlas oatlas eoatlas)
		  (%color-eval-with-errors 
		   'phot-transforms:gab-from-catlas+oatlas
		   (vector catlas oatlas)
		   (vector ecatlas eoatlas))))))
  (add-color-transform transform)
  (add-color-transform
   (make-synonym-color-transform transform :provides :gsdss))
  (add-color-transform
   (make-synonym-color-transform transform :provides :gps1)))


(let ((transform
	(make-color-transform 
	 :provides :rab
	 :needs '(:catlas :oatlas)
	 :name "Atlas"
	 :func (lambda (catlas ecatlas oatlas eoatlas)
		  (%color-eval-with-errors
		   'phot-transforms:rab-from-catlas+oatlas
		   (vector catlas oatlas)
		   (vector ecatlas eoatlas))))))
  (add-color-transform transform)
  (add-color-transform
   (make-synonym-color-transform transform :provides :rsdss))
  (add-color-transform
   (make-synonym-color-transform transform :provides :rps1)))


(let ((transform
	(make-color-transform 
	 :provides :iab
	 :needs '(:catlas :oatlas)
	 :name "Atlas"
	 :func (lambda (catlas ecatlas oatlas eoatlas)
		  (%color-eval-with-errors
		   'phot-transforms:iab-from-catlas+oatlas
		   (vector catlas oatlas)
		   (vector ecatlas eoatlas))))))
  (add-color-transform transform)
  (add-color-transform
   (make-synonym-color-transform transform :provides :isdss))
  (add-color-transform
   (make-synonym-color-transform transform :provides :ips1)))

(add-color-transform
 (make-color-transform 
  :provides :bj
  :needs '(:catlas :oatlas)
  :name "Atlas"
  :func (lambda (catlas ecatlas oatlas eoatlas)
	  (%color-eval-with-errors
	   'phot-transforms:bj-from-catlas+oatlas
	   (vector catlas oatlas)
	   (vector ecatlas eoatlas)))))

(add-color-transform
 (make-color-transform 
  :provides :vj
  :needs '(:catlas :oatlas)
  :name "Atlas"
  :func (lambda (catlas ecatlas oatlas eoatlas)
	  (%color-eval-with-errors
	   'phot-transforms:vj-from-catlas+oatlas
	   (vector catlas oatlas)
	   (vector ecatlas eoatlas)))))
 
(add-color-transform
 (make-color-transform 
  :provides :rc
  :needs '(:catlas :oatlas)
  :name "Atlas"
  :func (lambda (catlas ecatlas oatlas eoatlas)
	  (%color-eval-with-errors
	   'phot-transforms:rc-from-catlas+oatlas
	   (vector catlas oatlas)
	   (vector ecatlas eoatlas)))))

(add-color-transform
 (make-color-transform 
  :provides :ic
  :needs '(:catlas :oatlas)
  :name "Atlas"
  :func (lambda (catlas ecatlas oatlas eoatlas)
	  (%color-eval-with-errors
	   'phot-transforms:ic-from-catlas+oatlas
	   (vector catlas oatlas)
	   (vector ecatlas eoatlas)))))





 
