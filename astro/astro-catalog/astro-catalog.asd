



;; asdf file for astro-catalog


(asdf:defsystem astro-catalog
  :depends-on (numio drakma astro-obj file-io cl-fad run-program pconfig
		     cfitsio astro-time astro-coords logger refcat fare-csv
		     jk-parse-float)
  ;;
  :components
  ((:file "astro-catalog-package" :depends-on ())
   (:file "utils" :depends-on ("astro-catalog-package"))
   (:file "cdscatalog" :depends-on ("astro-catalog-package"))
   (:file "astro-catalog-class" :depends-on ("astro-catalog-package" "utils"))
   (:file "merge-catalogs" :depends-on ("astro-catalog-class"))
   (:file "merge-different-catalogs" :depends-on ("astro-catalog-class"))
   (:file "clean-astro-catalog" :depends-on
	  ("astro-catalog-package" "astro-catalog-class"))
   (:module "catalogs"
    :pathname "catalogs"
    :depends-on ("astro-catalog-package" "astro-catalog-class")
    :components ((:file "usno-b1-catalog")
		 (:file "sdss-catalog")
		 (:file "sdss7-catalog" :depends-on ("sdss-catalog"))
		 (:file "sdss8-catalog" :depends-on ("sdss-catalog"))
		 (:file "sdss9-catalog" :depends-on ("sdss-catalog"))
		 (:file "2mass-catalog")
		 (:file "refcat-catalog") ;; GAIA + PS1 + 2MASS, local
		 (:file "gaia-catalog")
		 (:file "psps-catalog")))
     (:file "catalog-cache" :depends-on 
	    ("astro-catalog-package" "astro-catalog-class"))
     (:file "ldac-fits" :depends-on
	    ("astro-catalog-package" "astro-catalog-class"))
     (:file "ldac-cache" :depends-on
	    ("astro-catalog-package" "astro-catalog-class" "ldac-fits"))
     (:file "ldac-tiling" :depends-on
	    ("astro-catalog-package" "astro-catalog-class" "ldac-fits"))
     (:file "misc" :depends-on
	    ("astro-catalog-package" "astro-catalog-class"))
	    
     ))
     



