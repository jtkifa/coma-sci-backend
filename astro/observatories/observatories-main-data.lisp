

(in-package observatories)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hand-added extra observatories, not in IRAF list
(defparameter *hand-added-obs-list*
  (list

   ;; the geocentric observatory has an altitude such that the routine
   ;; SLA_GEOC returns R=0,Z=0 of zero from the earth's center, which
   ;; should cause SLA_PLANTE and SLA_PLANTU to apply a zero geocentric
   ;; correction
   (make-observatory
    :id "geocenter"
    :name "Geocenter of Earth"
    :obscode "500"
    :generic t
    :wlongitude 0d0
    :latitude 0d0
    :altitude -6378140d0
    :timezone 0.0)
   
    (make-observatory
     :id "DCT"
     :name "Discovery Channel Telescope"
     :obscode "G37"
     :wlongitude 111.42222222222223d0
     :latitude 34.74444444444445d0
     :altitude 2360
     :timezone 7.0)

   ;; from PS1 headers
   (make-observatory 
    :id "ps1"
    :name "Pan-STARRS1"
    :obscode "F51"
    :wlongitude (* 15 10.4170602777931d0) ;;  hours to degrees
    :latitude  20.7070994446013d0
    :altitude  3048.0d0
    :timezone 10.0)
   ;;
   ;; approximate position from cfht
   (make-observatory
    :id "mko" ;; generic MKO must be first so 568 refers to it
    :name "Generic MKO, based on CFHT"
    :obscode "568"
    :generic t
    :wlongitude 155.47166666666666d0
    :latitude 19.826666666666668d0
    :altitude 4215.0d0
    :timezone 10.0)
   ;;
   ;; approximate position from cfht
   (make-observatory
    :id "uh88"
    :name "University of Hawaii 2.2m"
    :obscode "568"
    :wlongitude 155.47166666666666d0
    :latitude 19.826666666666668d0
    :altitude 4215.0d0
    :timezone 10.0)

   ;; 
   (make-observatory
    :id "yunnan"
    :name "Yunnan Astronomical Observatory, China"
    :obscode "286"
    :wlongitude -102.7833333333333d0 ;; minute precision (wikipedia)
    :latitude  25.033333333333335d0  ;; minute precision (wikipedia)
    :altitude 2014d0
    :timezone  -8.0)
   ;;
   ;; wise, israel - 40 inch telescope
   (make-observatory
    :id "Wise"
    :name "Wise Observatory, Israel"
    :obscode "097"
    :wlongitude -34.763333333333335d0
    :latitude  30.59583333333333d0
    :altitude 875d0
    :timezone -3.0
    )

   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following are taken from iraf observatory file
(defparameter *iraf-obs-list*
  (list
   (make-observatory
    :id "mgo"
    :name "Mount Graham Observatory"
    :obscode "G83"
    :generic t
    :wlongitude 109.89166666666668d0
    :latitude 32.70166666666667d0
    :altitude 3181.0d0
    :timezone 7.0)
   (make-observatory
    :id "tno"
    :name "Turkiye National Observatory"
    :generic t
    :wlongitude -30.353055555555557d0
    :latitude 36.824444444444445d0
    :altitude 2554.9999999999995d0
    :timezone -2.0)
   (make-observatory
    :id "euo"
    :name "Ege University Observatory"
    :generic t
    :wlongitude -27.274999999999977d0
    :latitude 38.39833333333333d0
    :altitude 795.0d0
    :timezone -2.0)
   (make-observatory
    :id "sln"
    :name "SLN - Catania Astrophysical Observatory"
    :generic t
    :wlongitude -14.973333333333358d0
    :latitude 37.69166666666667d0
    :altitude 1725.0d0
    :timezone -1.0)
   (make-observatory
    :id "paranal"  ;; generic Paranal goes FIRST
    :name "European Southern Observatory: Paranal"
    :obscode "309"
    :generic t
    :wlongitude 70.40333333333334d0
    :latitude -24.625d0
    :altitude 2635.0d0
    :timezone 4.0)
   (make-observatory
    :id "esovlt"
    :name "European Southern Observatory, VLT, Paranal"
    :obscode "309"
    :wlongitude 70.40220000000001d0
    :latitude -24.625300000000003d0
    :altitude 2648.0d0
    :timezone 4.0)
   (make-observatory
    :id "lasilla" ;; generic goes FIRST 
    :name "European Southern Observatory: La Silla"
    :obscode "809"
    :generic t
    :wlongitude 70.73d0
    :latitude -29.25666666666666d0
    :altitude 2347.0d0
    :timezone 4.0)
   (make-observatory
    :id "eso36m"
    :name "European Southern Observatory, 3.6m Telescope, La Silla"
    :obscode "809"
    :wlongitude 70.72961277777777d0
    :latitude -29.257170555555547d0
    :altitude 2400.0d0
    :timezone 4.0)
   (make-observatory
    :id "esontt"
    :name "European Southern Observatory, NTT, La Silla"
    :obscode "809"
    :wlongitude 70.73174222222222d0
    :latitude -29.255122222222212d0
    :altitude 2374.9999999999995d0
    :timezone 4.0)
   (make-observatory
    :id "gemini-south"
    :name "Gemini South Observatory"
    :obscode "I11"
    :generic t
    :wlongitude 70.72333333333333d0
    :latitude -30.228333333333353d0
    :altitude 2737.0d0
    :timezone 4.0)
   (make-observatory
    :id "gemini-north"
    :name "Gemini North Observatory"
    :obscode "568" ;; mko
    :wlongitude 155.46904675d0
    :latitude 19.8238015d0
    :altitude 4213.4d0
    :timezone 10.0)
   (make-observatory
    :id "osn"
    :name "Observatorio de Sierra Nevada"
    :obscode "J86"
    :generic t
    :wlongitude 3.3847222222222224d0
    :latitude 37.064166666666665d0
    :altitude 2895.9999999999995d0
    :timezone -1.0)
   (make-observatory
    :id "whitin"
    :name "Whitin Observatory, Wellesley College"
    :obscode "W83"
    :generic t
    :wlongitude 71.30583299999999d0
    :latitude 42.295d0
    :altitude 32.0d0
    :timezone 5.0)
   (make-observatory
    :id "fmo"
    :name "Fan Mountain Observatory"
    :generic t
    :obscode "I18"
    :wlongitude 78.69333333333334d0
    :latitude 37.87833333333333d0
    :altitude 565.9999999999999d0
    :timezone 5.0)
   (make-observatory
    :id "lmo"
    :name "Leander McCormick Observatory"
    :generic t
    :obscode "780"
    :wlongitude 78.52333333333333d0
    :latitude 38.03333333333333d0
    :altitude 264.0d0
    :timezone 5.0)
   (make-observatory
    :id "holi"
    :name "Observatorium Hoher List (Universitaet Bonn) - Germany"
    :obscode "017"
    :generic t
    :wlongitude 6.85d0
    :latitude 50.162760000000006d0
    :altitude 541.0d0
    :timezone -1.0)
   (make-observatory
    :id "ca"
    :name "Calar Alto Observatory"
    :obscode "493"
    :generic t
    :wlongitude 2.5462499999999997d0
    :latitude 37.22361111111111d0
    :altitude 2168.0d0
    :timezone -1.0)
   (make-observatory
    :id "DSAZ"
    :name "Deutsch-Spanisches Observatorium Calar Alto - Spain"
    :obscode "493"
    :wlongitude 2.5462499999999997d0
    :latitude 37.22361111111111d0
    :altitude 2168.0d0
    :timezone -1.0)
   (make-observatory
    :id "bgsuo"
    :name "Bowling Green State Univ Observatory"
    :obscode NIL
    :generic t
    :wlongitude 83.65916666666668d0
    :latitude 41.37833333333333d0
    :altitude 225.0d0
    :timezone 5.0)
   (make-observatory
    :id "irtf"
    :name "NASA Infrared Telescope Facility"
    :obscode "568"
    :wlongitude 155.47199899999998d0
    :latitude 19.826217999999997d0
    :altitude 4167.999999999999d0
    :timezone 10.0)
   (make-observatory
    :id "rozhen"
    :name "National Astronomical Observatory Rozhen - Bulgaria"
    :obscode NIL
    :wlongitude -24.743888888888875d0
    :latitude 41.69305555555555d0
    :altitude 1759.0000000000002d0
    :timezone -2.0)
   (make-observatory
    :id "bosque"
    :name "Estacion Astrofisica Bosque Alegre, Cordoba"
    :obscode "822"
    :generic t
    :wlongitude 64.54583333333333d0
    :latitude -31.598333333333358d0
    :altitude 1250.0d0
    :timezone 3.0)
   (make-observatory
    :id "casleo"
    :name "Complejo Astronomico El Leoncito, San Juan"
    :obscode "808"
    :generic t
    :wlongitude 69.3d0
    :latitude -31.79916666666668d0
    :altitude 2551.9999999999995d0
    :timezone 3.0)
   (make-observatory
    :id "saao"
    :name "South African Astronomical Observatory"
    :obscode "B31"
    :generic t
    :wlongitude -20.810694444444437d0
    :latitude -32.37944444444446d0
    :altitude 1798.0000000000002d0
    :timezone -2.0)
   (make-observatory
    :id "lna"
    :name "Laboratorio Nacional de Astrofisica - Brazil"
    :obscode nil
    :wlongitude 45.5825d0
    :latitude -22.534444444444432d0
    :altitude 1864.0d0
    :timezone 3.0)
   (make-observatory
    :id "oro"
    :name "Oak Ridge Observatory"
    :obscode "801"
    :generic t
    :wlongitude 71.55814444444444d0
    :latitude 42.50526111111111d0
    :altitude 184.0d0
    :timezone 5.0)
   (make-observatory
    :id "flwo1"
    :name "Whipple Observatory"
    :obscode "696"
    :generic t
    :wlongitude 110.8775d0
    :latitude 31.680944444444446d0
    :altitude 2320.0d0
    :timezone 7.0)
   (make-observatory
    :id "iao"
    :name "Indian Astronomical Observatory, Hanle"
    :obscode "N50"
    :generic t
    :wlongitude 281.0358299999999d0
    :latitude 32.7794d0
    :altitude 4500.0d0
    :timezone -5.5)
   (make-observatory
    :id "HCT"
    :name "HCT at Indian Astronomical Observatory, Hanle"
    :obscode "N50"
    :wlongitude 281.0358299999999d0
    :latitude 32.7794d0
    :altitude 4500.0d0
    :timezone -5.5)
   (make-observatory
    :id "vbo"
    :name "Vainu Bappu Observatory"
    :obscode "220"
    :wlongitude 281.1734d0
    :latitude 12.576659999999997d0
    :altitude 725.0d0
    :timezone -5.5)
   (make-observatory
    :id "lowell"
    :name "Lowell Observatory"
    :obscode "688" ;; Lowell Anderson Mesa
    :generic t
    :wlongitude 111.535d0
    :latitude 35.096666666666664d0
    :altitude 2198.0d0
    :timezone 7.0)
   (make-observatory
    :id "apo"
    :name "Apache Point Observatory"
    :obscode "705"
    :wlongitude 105.82d0
    :latitude 32.78d0
    :altitude 2798.0d0
    :timezone 7.0)
   (make-observatory
    :id "ekar"
    :name "Mt. Ekar 182 cm. Telescope"
    :obscode "098"
    :generic t
    :wlongitude -11.581133333333298d0
    :latitude 45.84858888888889d0
    :altitude 1413.6899999999996d0
    :timezone -1.0)
   (make-observatory
    :id "keck"
    :name "W. M. Keck Observatory"
    :obscode "568"
    :wlongitude 155.47833333333332d0
    :latitude 19.828333333333333d0
    :altitude 4159.999999999999d0
    :timezone 10.0)
   (make-observatory
    :id "BAO"
    :name "Beijing XingLong Observatory"
    :obscode "327"
    :wlongitude -117.57499999999999d0
    :latitude 40.39333333333333d0
    :altitude 950.0d0
    :timezone -8.0)
   (make-observatory
    :id "bmo"
    :name "Black Moshannon Observatory"
    :obscode nil
    :wlongitude 78.005d0
    :latitude 40.92166666666667d0
    :altitude 738.0d0
    :timezone 5.0)
   (make-observatory
    :id "NOV"
    :name "National Observatory of Venezuela"
    :obscode nil
    :wlongitude 70.86666666666666d0
    :latitude 8.79d0 
    :altitude 3610.0d0
    :timezone 4.0)
   (make-observatory
    :id "mdm"
    :name "Michigan-Dartmouth-MIT Observatory"
    :obscode "695" ;; used generic Kitt Peak
    :wlongitude 111.61666666666666d0
    :latitude 31.95d0
    :altitude 1938.5d0
    :timezone 7.0)
   (make-observatory
    :id "Palomar"
    :name "The Hale Telescope"
    :obscode "675"
    :wlongitude 116.863d0
    :latitude 33.356d0
    :altitude 1706.0000000000002d0
    :timezone 8.0)
   (make-observatory
    :id "tona"
    :name "Observatorio Astronomico Nacional, Tonantzintla"
    :obscode nil
    :wlongitude 98.31388888888888d0
    :latitude 19.032777777777778d0
    :altitude nil
    :timezone 8.0)
   (make-observatory
    :id "spm"
    :name "Observatorio Astronomico Nacional, San Pedro Martir"
    :obscode nil
    :wlongitude 115.48694444444445d0
    :latitude 31.029166666666665d0
    :altitude 2829.9999999999995d0
    :timezone 7.0)
   (make-observatory
    :id "dao"
    :name "Dominion Astrophysical Observatory"
    :obscode "790"
    :generic t
    :wlongitude 123.41666666666667d0
    :latitude 48.52166666666667d0
    :altitude 229.0d0
    :timezone 8.0)
   (make-observatory
    :id "mtbigelow"
    :name "Catalina Observatory: 61 inch telescope"
    :obscode "698"
    :wlongitude 110.73166666666667d0
    :latitude 32.416666666666664d0
    :altitude 2510.0d0
    :timezone 7.0)
   (make-observatory
    :id "lco"
    :name "Las Campanas Observatory"
    :obscode "304"
    :generic t
    :wlongitude 70.70166666666667d0
    :latitude -29.00333333333333d0
    :altitude 2282.0d0
    :timezone 4.0)
   (make-observatory
    :id "mcdonald"
    :name "McDonald Observatory"
    :obscode "711"
    :generic t
    :wlongitude 104.0216667d0
    :latitude 30.671666700000003d0
    :altitude 2074.9999999999995d0
    :timezone 6.0)
   (make-observatory
    :id "aao"
    :name "Anglo-Australian Observatory"
    :obscode "413" ;; this is a guess
    :generic nil ;; because of Siding Spring
    :wlongitude -149.0660861111111d0
    :latitude -31.27703888888891d0
    :altitude 1164.0000000000002d0
    :timezone -10.0)
   (make-observatory
    :id "sso"
    :name "Siding Spring Observatory"
    :obscode "413"
    :generic t
    :wlongitude -149.06119444444445d0
    :latitude -31.273361111111114d0
    :altitude 1149.0d0
    :timezone -10.0)
   (make-observatory
    :id "mso"
    :name "Mt. Stromlo Observatory"
    :obscode "414"
    :generic t
    :wlongitude -149.02433333333335d0
    :latitude -35.32065d0
    :altitude 767.0d0
    :timezone -10.0)
   (make-observatory
    :id "lapalma"
    :name "Roque de los Muchachos, La Palma"
    :obscode "950"
    :generic t
    :wlongitude 17.88d0
    :latitude 28.758333333333333d0
    :altitude 2327.0d0
    :timezone 0.0)
   (make-observatory
    :id "CFHT"
    :name "Canada-France-Hawaii Telescope"
    :obscode "568"
    :wlongitude 155.47166666666666d0
    :latitude 19.826666666666668d0
    :altitude 4215.0d0
    :timezone 10.0)
   (make-observatory
    :id "Subaru"
    :name "Subaru 8m telescope"
    :obscode "568"
    :wlongitude 155.47166666666666d0
    :latitude 19.826666666666668d0
    :altitude 4215.0d0
    :timezone 10.0)
   (make-observatory
    :id "mmto"
    :name "MMT Observatory"
    :obscode "696"
    :generic NIL ;; whipple
    :wlongitude 110.885d0
    :latitude 31.688333333333333d0
    :altitude 2600.0d0
    :timezone 7.0)
   (make-observatory
    :id "lick"
    :name "Lick Observatory"
    :obscode "662"
    :generic t
    :wlongitude 121.63666666666667d0
    :latitude 37.343333333333334d0
    :altitude 1290.0d0
    :timezone 8.0)
   (make-observatory
    :id "paranal"
    :name "European Southern Observatory: Paranal"
    :obscode "309"
    :wlongitude 70.40333333333334d0
    :latitude -24.625d0
    :altitude 2635.0d0
    :timezone 4.0)
   (make-observatory
    :id "ctio"
    :name "Cerro Tololo Interamerican Observatory"
    :obscode "807"
    :generic t
    :wlongitude 70.815d0
    :latitude -30.165277780000004d0
    :altitude 2215.0d0
    :timezone 4.0)
   (make-observatory
    :id "kpno"
    :name "Kitt Peak National Observatory"
    :obscode "695":generic t
    :wlongitude 111.6d0
    :latitude 31.963333333333335d0
    :altitude 2120.0d0
    :timezone 7.0)))


(defparameter *obs-list*
  (append
   *hand-added-obs-list*
   *iraf-obs-list*
   (read-mpc-obseratory-list)
   ))

;; fill the obs hash but avoid duplicated observatory ids (so we favor
;; those not from MPC - ie those with timezone -  because they are first in list)
(defun fill-obs-hash ()
  (clrhash *obs-hash*)
  (loop 
    with h = *obs-hash*
    for obs in *obs-list*
    for obscode = (observatory-obscode obs)
    when (not (gethash (observatory-id obs) h)) ;; not in hash
      do (setf (gethash (observatory-id obs) h) obs)
    do
       (cond
	 ;; do nothing for NIL obscode
	 ((not obscode)
	  nil)
	 ;; hash the FIRST instace by obscode
	 ((not (gethash obscode h))
	  (setf (gethash obscode h) obs))
	 ;; else favor this one if it is generic
	 ((and (observatory-generic obs)
	       (not (observatory-generic  (gethash obscode h))))
	  (setf (gethash obscode h) obs))
	 ;; else an error if we have 2 generic
	 ((and (observatory-generic obs)
	       (observatory-generic  (gethash obscode h)))
	  (error "OBSERVATORIES: OBS-CODE=~A has two generic observatories:~% ~A and ~A"
		 obscode obs  (gethash obscode h))))))
	  
   

(eval-when (:load-toplevel :execute)
  (fill-obs-hash))
