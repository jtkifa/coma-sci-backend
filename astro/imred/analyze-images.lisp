
;; analyze a fits file to pick out its nature
;; extension by extension.  The standard data
;; structure is an extdesc, and a description
;; consists of a list of these

(in-package imred)


(defstruct extdesc
  (n-ext nil)
  (type nil) ;; :image or :table 
  (reduce-p nil) ;; T if this extension is to be reduced (NIL for
		 ;; tables and empty images)
  (image-size nil) ;; actual size of array
  (image-type nil) ;; :float, etc
  (object-type nil)  ;; BIAS, etc
  (flat-group nil)   ;; identifer for flat type, eg filter name
  (fringe-group nil) ;; identifer for fringe type, eg filter name
  (gain nil)
  ;;
  (trimsec nil)
  (statsec nil)
  (overscans nil))
  
  

(defun extdesc-compatible-p (extdesc1 extdesc2)
  "Are two EXTDESC structures compatible in size?, ignoring non-image ones"
  (or 
   ;; if BOTH are not images, then it is OK
   (and (not (eq (extdesc-type extdesc1) :image))
	(not (eq (extdesc-type extdesc2) :image)))
   (and 
    (equalp (extdesc-image-size extdesc1) (extdesc-image-size extdesc2))
    (equalp (extdesc-trimsec extdesc1) (extdesc-trimsec extdesc2)))))



(defun extdesc-lists-compatible-p (extdesc-list-1 extdesc-list-2)
  "Are two EXTDESC structure lists compatible?"
  (declare (type list extdesc-list-1 extdesc-list-2))
  (and 
   (= (length extdesc-list-1) (length extdesc-list-2)))
   ;; map to list and require each element is T
   (every 'identity (mapcar #'extdesc-compatible-p
			    extdesc-list-1 extdesc-list-2)))



;; get the fringe group and check if it is one of the desired
;; fringe gropus
(defun %get-fringe-group (reduction-plan ff)
  (let ((fringe-group
	  (funcall (reduction-plan-fringe-group-func reduction-plan) ff)))
    (if (find fringe-group (reduction-plan-desired-fringe-groups reduction-plan)
	      :test (lambda (x y) (equalp (string x) (string y))))
	fringe-group)))


(defun build-extdesc-at-current-hdu (ff &key (reduction-plan *default-reduction-plan*))
  (let* ((is-image-ext
	   (and (eq (cf:fits-file-current-hdu-type ff) :image)
		(eql (cf:fits-file-current-image-ndims ff) 2)))
	 (extnum (cf:fits-file-current-hdu-num ff))
	 (trimsec (when is-image-ext
		    (%get-trimsec-at-extension ff reduction-plan extnum)))
	 (statsec-raw (when is-image-ext
			(instrument-id:get-statsec-for-fits
			 ff :extension extnum)))
	 (statsec (when statsec-raw
		    (instrument-id:convert-imagesec-to-trimmed-imagesec
		     statsec-raw trimsec)))
	 (overscans (when is-image-ext
		      (%get-overscans-at-extension ff reduction-plan extnum)))
	 (object-type (instrument-id:get-object-type-for-fits ff))
	 ;;
	 (extdesc  
	   (make-extdesc 
	    :n-ext (cf:fits-file-current-hdu-num ff)
	    :type (cf:fits-file-current-hdu-type ff)
	    :image-size  (when is-image-ext
			    (cf:fits-file-current-image-size ff))
	    :image-type  (when is-image-ext
			   (cf:fits-file-current-image-type ff))
	    :gain (when is-image-ext (instrument-id:get-gain-for-fits
				      ff :extension (cf:fits-file-current-hdu-num ff)))
	    :object-type object-type
	    :flat-group (when (and is-image-ext
				   ;; bias doesn't have a flat-group
				   (not (eq object-type :bias)))
			  (funcall (reduction-plan-flat-group-func reduction-plan) ff))
	    :fringe-group (when (and is-image-ext
				     ;; fringe doesn't have a flat-group
				     (not (eq object-type :bias)))
			    (%get-fringe-group reduction-plan ff))
	    ;; this is now fixed to use the instrument-id version of trimsec
	    :trimsec trimsec
	    :statsec statsec
	    :overscans overscans)))
    (setf (extdesc-reduce-p extdesc) is-image-ext)
    extdesc))


(defun build-extdesc-list-for-fits (fits &key (reduction-plan *default-reduction-plan*))
  (cf:with-open-fits-file (fits ff :mode :input)
    (loop 
       for ihdu from 1 to (cf:fits-file-num-hdus ff)
       for extdesc 
	 = (progn
	     (cf:move-to-extension ff ihdu)
	     (multiple-value-bind (ed err)
		 (progn ; ignore-errors
		  (build-extdesc-at-current-hdu ff :reduction-plan reduction-plan))
	       (when (not ed)
		 (imred-log-error
		  (format nil
			  "Failed to build EXTDESC (extension descriptor) for ~A - dropping image; error is ~A"
			  fits err)
		  :die nil))
	       ed))
       when extdesc
	 collect extdesc)))





(defun all-images-compatible-p (fits-list 
				&key (reduction-plan *default-reduction-plan*))
"Return (VALUES T/NIL EXTDESC-LIST) if all images are compatible"
  (let ((ed-list (mapcar 
		  (lambda (fits) 
		    (build-extdesc-list-for-fits 
		     fits
		     :reduction-plan reduction-plan))
		  fits-list)))
    (loop 
       with ed1 = (first ed-list)
       for ed in (cdr ed-list)
       when (not (extdesc-lists-compatible-p ed1 ed))
       do (return (values nil ed-list))
       finally
	 (return (values t ed-list)))))
