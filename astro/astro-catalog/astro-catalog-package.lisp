
;; routines to download astronomical catalogs from their web sites

(defpackage astro-catalog
  (:use #:cl) 
  (:export
   ;; astro-catalog-class - generic methods 
   #:astro-catalog #:get-value #:get-astro-catalog-vector #:astro-catalog-fields
   #:is-valid-field #:object-mag
   #:object-ra #:object-dec #:object-ra-err #:object-dec-err
   #:object-ra-dec #:object-proper-motions
   #:object-type  #:object-id
   #:astro-catalog-n #:astro-catalog-data #:available-mags #:ra-center 
   #:dec-center #:radius-deg
   ;; usno-b-catalog.lisp
   #:read-usno-b1-catalog
   #:usno-b1-catalog 
   #:read-usno-b1-catalog-object
   #:read-usno-b1-catalog-to-file
   #:get-catalog-column ;; used to get columns by name after retrieval
   ;; clean-astro-catalog.lisp
   #:purge-astro-catalog-by-is-ok
   ;; sdss-catalog.lisp
   #:sdss-catalog ;; parent class
   ;; sdss7-catalog.lisp
   #:read-sdss7-catalog
   #:sdss7-catalog
   #:read-sdss7-catalog-object
   ;; sdss8-catalog.lisp
   #:read-sdss8-catalog
   #:sdss8-catalog
   #:read-sdss8-catalog-object
   ;; sdss9-catalog.lisp
   #:read-sdss9-catalog
   #:sdss9-catalog
   #:read-sdss9-catalog-object
   ;; 2mass-catalog.lisp
   #:2mass-point-source-catalog
   #:read-2mass-point-source-catalog
   #:read-2mass-point-source-catalog-object
   ;; gaia-dr1-catalog.lisp
   #:gaia-dr1-catalog
   #:read-gaia-dr1-catalog
   #:read-gaia-dr1-catalog-object
   #:gaia-dr2-catalog
   #:read-gaia-dr2-catalog
   #:read-gaia-dr2-catalog-object
   #:gaia-dr3-catalog
   #:read-gaia-dr3-catalog
   #:read-gaia-dr3-catalog-object
   ;; refcat-catalog.lisp (ATLAS)
   #:refcat-catalog
   #:read-refcat-catalog
   #:read-refcat-catalog-object
   ;; psps-catalog.lisp
   #:read-psps-3pi-catalog
   #:psps-3pi-catalog
   #:psps-3pi-mean-psf-mag-catalog 
   #:psps-3pi-mean-kron-mag-catalog
   #:psps-3pi-stack-psf-mag-catalog
   #:psps-3pi-stack-kron-mag-catalog
   #:read-psps-3pi-catalog-object
   ;; catalog-cache.lisp
   #:*allowed-cache-catalog-types* 
   #:get-cached-catalog-object 
   #:catalog-cache #:catalog-cache-n-catalogs #:catalog-cache-n-catalogs-max
   #:clear-catalog-cache
   #:describe-catalog-cache
   ;; ldac-fits.lisp
   #:write-catalog-to-fits-ldac
   #:read-catalog-from-fits-ldac
   ;; ldac-cache.lisp
   #:ldcat-cat #:ldcac-cat-p #:ldcac-cat-ra0 #:ldcac-cat-dec0
   #:ldcac-cat-radius  #:ldcac-cat-catalog-type
   #:ldcac-cache #:ldac-cache-p
   #:build-ldac-cache
   #:refresh-ldac-cache
   #:find-fully-overlapping-ldac-cat-in-ldac-cache
   #:find-all-overlapping-ldac-cats-in-ldac-cache
   #:make-astro-catalog-from-overlaps-in-ldac-cache
   #:make-ldac-fits-from-overlaps-in-ldac-cache
   ;; ldac-tiling.lisp
   #:retrieve-ldac-catalog-tiling-for-pointing
   ;; merge-catalogs.lisp
   #:merge-catalogs-around-ra-dec
   ;; merge-different-catalogs.lisp 
   #:merged-catalog
   #:merge-different-catalogs
   ;; misc.lisp
   #:list-objects-near-location
   #:describe-object-at-index
   ))

(in-package astro-catalog) 

