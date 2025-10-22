
(in-package coma-json-server)

(defconstant +au/m+ 1.495978707d11) ;; AU in m
(defconstant +au/km+ 1.495978707d8) ;; AU in km

(defconstant +m-sun+ 1.9885d30) ;; kg

(defconstant +g-mks+ 6.67430d-11) ;; G gravity in MKS units

(defconstant +gm-sun+ (* +m-sun+ +g-mks+))

(defconstant +day/sec+ #.(* 24 3600))

(defconstant +year/sec+ #.(* 24 3600 365.2422))





