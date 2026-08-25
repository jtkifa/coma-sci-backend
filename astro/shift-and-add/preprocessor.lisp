#|

define an image preprocessor (like background subtraction)
that is run on the current active fits list, and returns
a parallel list of new processed fits files

|#


(in-package shift-and-add)


;; parent image preproc-class
(defclass image-preproc ()
  ())


(defgeneric run-image-preproc (image-preproc saaplan fits-working-list)
  (:documentation "Run the image preprocessing steps described by 
IMAGE-PREPROC on FITS-WORKING-LIST, returning a new fits list of
pre-processed files."))

(defmethod run-image-preproc ((image-preproc image-preproc)
			      (saaplan saaplan)
			      fits-working-list)
  (error "Can't run a parent class IMAGE-PREPROC."))
