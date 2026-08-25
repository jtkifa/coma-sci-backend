
#|

Parent class of image weighters, which define a way of creating image weights.


A weighter is called as

(run-weight-generation image-weighter saaplan fits-list)




|#

(in-package shift-and-add)
 

(defclass image-weighter ()
  ())

(defgeneric run-weight-generation (image-weighter saaplan fits-list &key if-exists badpix-function-list)
  (:documentation "Create a set of weight images for fits-list.  Return a list of weight
files congrument to FITS-LIST, possibly NIL.

BADPIX-FUNCTION-LIST is a list of badpix functions as defined in INSTRUMENT-ID.  They are not extracted
from working images because the data to make them may be lost when the image extension is stripped out."))


(defmethod run-weight-generation ((image-weighter image-weighter)
				  (saaplan saaplan)
				  fits-list
				  &key (if-exists :overwrite) badpix-function-list)
  (declare (ignorable image-weighter saaplan fits-list if-exists badpix-function-list))
  (error "Cannot run-weight-generation for parent class image-weighter."))
