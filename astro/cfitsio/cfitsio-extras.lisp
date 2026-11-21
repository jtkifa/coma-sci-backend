
(in-package cfitsio)


;; primitive early version that just copies extension, without
;; headers or decompression option.
(defunL extract-fits-extension/simple
    (input-fits-file output-fits-file &key (extension nil) (morekeys 0)
     (overwrite nil) (throw-error t))
  "Copy a fits extension from one fits file to a new fits file
Exampes:
   (extract-fits-extension \"INPUT.fits[5]\" \"OUTPUT_NEW.fits\")
   (extract-fits-extension \"INPUT.fits\" \"OUTPUT_NEW.fits\" :extension 5)
   (extract-fits-extension \"INPUT.fits[5]\" \"OUTPUT_NEW.fits\" 
                           :morekeys 10 :overwrite t)
The last line creates 10 more slots for keywords and overwrites an existing 
output file.

NOTE: if the extension is a table (including a compressed image!), the
routine creates a PRIMARY + EXTENSION fits file. This is a result of FITS 
rules that require a primary extension to be an image."
  (cf:with-open-fits-file (input-fits-file ffin :mode :input)
    (when extension (cf:move-to-extension ffin extension))
    (cf:with-new-fits-file (output-fits-file ffout :overwrite overwrite)
      (copy-current-extension ffin ffout :morekeys morekeys
					 :throw-error throw-error))))





(defunL extract-fits-extension
    (input-fits-file output-fits-file &key
     (extension nil)
     (morekeys 0)
     (overwrite nil)
     (throw-error t)
     (preserve-primary-extension t)
     (copy-primary-headers :inherit)
     (decompress nil))
  "Copy a fits extension from one fits file to a new fits file
Exampes:
   (extract-fits-extension \"INPUT.fits[5]\" \"OUTPUT_NEW.fits\")
   (extract-fits-extension \"INPUT.fits\" \"OUTPUT_NEW.fits\" :extension 5)
   (extract-fits-extension \"INPUT.fits[5]\" \"OUTPUT_NEW.fits\" 
                           :morekeys 10 :overwrite t)

The last line creates 10 more slots for keywords and overwrites an existing 
output file.

PRESERVE-PRIMARY-EXTENSION, by default T, first copies the primary
extension with headers, then adds the desired extension.  Disabling
this extracts only the requested extension, if possible (ie, if it's a
non-compressedq image).  However this works only if extension is
specified as a keyword, because using 'INPUT.fits[n]' format causes
cfitsio to see only the opened extension.

COPY-PRIMARY-HEADERS can be T, NIL, or the default :INHERIT; it
determines whether primary headers are copied into an extension
if (and only if) PRESERVE-PRIMARY-EXTENSION was false.  The :INHERIT
case copies them only if the header INHERIT=T in the primary
extension.

DECOMPRESS will enable decompression of the image.

NOTE: if the extension is a table (including a compressed image,
without DECOMPRESS=T), the routine always creates a PRIMARY +
EXTENSION fits file. This is a result of FITS rules that require a
primary extension to be an ordinary uncompressed image."
  (declare (type (member t nil :inherit) copy-primary-headers))
  ;;
  (cf:maybe-with-open-fits-file (input-fits-file ffin :mode :input)
    (let ((has-primary-extension
	    (eql 0 (read-fits-header ffin "NAXIS" :extension 1)))
	  (inherit  (read-fits-header ffin "INHERIT" :extension 1))
	  (compressed-image-p
	    (and (equalp (cf:read-fits-header ffin "TTYPE1" :extension extension)
			 "COMPRESSED_DATA")
		 (cf:read-fits-header ffin "NAXIS1" :extension extension))))

      (setf inherit t)
      (when (and has-primary-extension
		 (eql extension 1))
	(error "ERROR - this image has a primary extension, but it is requested to extract extension 1.  It is not permitted to extract just the data-free primary extension."))
          
      ;;(format t "compressed-p: ~A~%" compressed-image-p)
      (cf:with-new-fits-file (output-fits-file ffout :overwrite overwrite)

	;; preserve the primary extension if requested
	(when (and preserve-primary-extension has-primary-extension)
	  (cf:with-fits-extension (ffin 1) ;; ensure we're in ext 1 (primary)
	    (copy-current-extension ffin ffout :morekeys morekeys
					       :throw-error throw-error)
	    (write-fits-header ffout "NEXTEND" 2))) ;; 2 extensions,  primary and requested

	;; now move to the extension to be copied
	(when extension (cf:move-to-extension ffin extension))

	;; function to copy headers from primary to new extension, ignoring certain
	;; special headers, and headers that are already present. The new extension
	;; headers may be given, or are read from new fits file.    
	(flet ((copy-inherited-primary-headers (&optional ext-headers)
		 (let* ((primary-headers (read-fits-header-list ffin :extension 1))
			(ext-headers (or ext-headers
					 (read-fits-header-list ffout))))
		   (loop for (name value comment) in primary-headers
			 for reject = (or (member name '("SIMPLE" "BITPIX"
							 "NAXIS" "COMMENT"
							 "NEXTEND"
							 "BZERO" "BSCALE")
						  :test 'equalp)
					  (eql 0 (search "NAXIS" name))) 
			 for is-already-present = (find name ext-headers :test 'equalp :key 'first)
			 when (not (or reject is-already-present))
			   do
			      ;;(format t "Adding header ~A=~A~%" name value)
			      (cf:write-fits-header ffout name value :comment comment)))))
	  (cond
	    ;; not decompressing a compressed image - this will create a 2nd extension
	    ((and compressed-image-p (not decompress))
	     (copy-current-extension ffin ffout :morekeys morekeys
						:throw-error throw-error)
	     ;; this is a weird case of having an empty primary, putting
	     ;; inherited primary headers into the exension. Not the
	     ;; right way, but for completeness we include it.
	     (when (not preserve-primary-extension)
	       (when inherit (copy-inherited-primary-headers))))
	    ;;
	    ;; a non-compressed image will be its own primary, UNLESS
	    ;; preserve-primary-extension was set
	    ((not compressed-image-p)
	     (copy-current-extension ffin ffout :morekeys morekeys
						:throw-error throw-error)
	     (when (not preserve-primary-extension)
	       (when inherit (copy-inherited-primary-headers))))
	    ;;
	    ;; otherwise we are decompressing a compressed image, requiring special handling
	    (t
	     (let* ((bitpix (or (cf:read-fits-header ffin "ZBITPIX")
				(error "ZBITPIX header not found - can't decompress.")))
		    ;;(naxis (cf:read-fits-header ffin "ZNAXIS"))
		    (bzero (cf:read-fits-header ffin "BZERO"))
		    (bscale (cf:read-fits-header ffin "BSCALE"))
		    (imtype (get-read-type-for-bitpix bitpix))
		    ;; read data with no scaling (BZERO=0, BSCALE=1)
		    (imsec (cf:read-image-section ffin :type imtype
						       :bzero 0d0 :bscale 1d0)))

	      
	       (cf:add-image-to-fits-file  
		ffout
		(cond ((eq imtype :unsigned-byte-8)  :short)
		      ((eq imtype :unsigned-byte-16) :ushort)
		      ((eq imtype :signed-byte-16)   :short)
		      ((eq imtype :unsigned-byte-32) :long)
		      ((eq imtype :single-float)     :float)
		      ((eq imtype :double-float)     :double)
		      (t (error "Can't handle imtype ~A~%" imtype)))
		(cf:image-section-lp imsec)
		:create-data
		(cf:image-section-data imsec))
	       ;; and set the correct BSCALE/BZERO from original image
	       (when bzero (cf:write-fits-header ffout "BZERO"  bzero))
	       (when bscale (cf:write-fits-header ffout "BSCALE" bscale))

	       ;; Copy over headers - have to do it manually for decompression case
	       ;; 
	       (let* ((headers-in (read-fits-header-list ffin :extension extension))
		      (exclude-headers ;; remove compression-related headers
			'("FZALGOR" "FZTILE" "FZQVALUE" "FZQMETHD" "FZDTHRSD"
			  "FZINT2F" "FZHSCALE" "TTYPE1" "TFORM1" "ZIMAGE"
			  "ZBITIX" "PCOUNT" "GCOUNT" "ZNAME1" "ZNAME2"
			  "ZCMPTYPE" "TFIELDS" "ZBITPIX" "XTENSION" "SIMPLE" "BITPIX" "BZERO" "BSCALE"))
		      (headers-in-cleaned
			(loop for header in headers-in
			      for hname = (first header)
			      when (not
				    (or (member hname exclude-headers :test 'equalp)
					(eql 0 (search "ZNAXIS" hname :test 'equalp))
					(eql 0 (search "ZTILE" hname :test 'equalp))
					(eql 0 (search "ZVAL" hname :test 'equalp))
					(eql 0 (search "NAXIS" hname :test 'equalp))
					)) ;; ZNAXIS, ZNAXISi
				collect header)))
		 (loop for (name value comment) in headers-in-cleaned
		       do (cond ((equalp name "COMMENT")
				 (write-fits-comment ffout value))
				(t
				 (write-fits-header ffout name value :comment comment)))))
	       ;;
	       ;; copy headers only if primary extension was not preserved
	       (when (not preserve-primary-extension)
		 ;; copy according to COPY-PRIMARY-HEADERS={t,:inherit}
		 (when (or
			(and inherit (eq copy-primary-headers :inherit)) ;; only if inherit?
			(eq copy-primary-headers t)) ;; always copy
		   (copy-inherited-primary-headers)))
	       
	       ))))))))

		   
	

(defunL extract-image-subsection
    (input-fits-file output-fits-file &key
     (fp nil)
     (copy-primary-headers t)
     (copy-extension-headers t)
     (lp nil)
     (type :float)
     (extension nil) 
     (overwrite nil))
  "Copy an image subsection to a new image, preserving WCS.  

COPY-PRIMARY-HEADERS is NIL, T (copy-all), or a function that returns 
  T if a header should be copied.  Special headers like NAXIS,BITPIX
  are handled separately.
COPY-EXTENSION-HEADERS is similar.
"
  (declare (type (or symbol function (member  t))
		 copy-primary-headers copy-extension-headers))
  (cf:with-open-fits-file (input-fits-file ffin :mode :input)
    (when extension (move-to-extension ffin extension))
    (when (not (and
		(eq (fits-file-current-hdu-type ffin) :image)
		(eql (fits-file-current-image-ndims ffin) 2)))
      (error "Not a 2d input image"))
    (let* ((imsec (read-image-section
		   ffin :fp fp :lp lp :read-wcs t :type type))
	   (arr (image-section-data imsec)))
      
      (cf:with-new-fits-file (output-fits-file ffout
			      :overwrite overwrite
			      :make-primary-headers t)
	;; maybe copy primary headers
	(when copy-primary-headers
	  (cf:move-to-extension ffin 1)
	  (loop with hdr-list = (read-fits-header-list ffin)
		for hdr in hdr-list
		for key = (first hdr) and val = (second hdr) and com = (third hdr)
		when (and
		      ;; no special headers
		      (not (member key '("NAXIS" "NAXIS1" "NAXIS2"
					 "SIMPLE" "BITPIX" "EXTEND")
				   :test 'equalp)) 
		      (or (eq copy-primary-headers t)
			  (funcall copy-primary-headers key)))
		  do (cf:write-fits-header ffout key val :comment com)))
	  
	(add-image-to-fits-file 
	 output-fits-file type
	 (vector (array-dimension arr 1) (array-dimension arr 0))
	 :create-data arr )

	(cf:move-to-extension ffout 2)
	(when (and copy-extension-headers)
	  (when extension (cf:move-to-extension ffin extension))
	  (loop with hdr-list = (read-fits-header-list ffin)
		for hdr in hdr-list
		for key = (first hdr) and val = (second hdr) and com = (third hdr)
		when (and
		      (not (member key '("NAXIS" "NAXIS1" "NAXIS2"
					 "SIMPLE" "BITPIX" "EXTEND")
				   :test 'equalp))
		      (not (wcs-header-p key))
		      (or (eq copy-extension-headers t)
			  (funcall copy-extension-headers key)))
		  do (cf:write-fits-header ffout key val :comment com)))
	
	;; write wcs 2nd to clobber original
	(when (image-section-wcs imsec)
	  (write-wcs (image-section-wcs imsec) ffout :extension 2))))))
  


(defun concatenate-fits-extensions (input-file-list output-fits-file &key (overwrite nil))
  "Concatenate a list of fits files in INPUT-FILE-LIST to OUTPUT-FITS-FILE.  Inputs may be
files or CF:FITS-FILE structures." 
  (cf:with-new-fits-file (output-fits-file ffout :overwrite overwrite)
    (loop for fits-in in input-file-list 
	  do (copy-current-extension fits-in ffout :throw-error t))))


;; write an image section for an array or a imsec
#+nil ;; new version allows headers, important for writing a big block of headers
(defunL write-2d-image-to-new-fits-file-old (a fits-name &key (overwrite nil)
					(wcs nil) 
					(type :float)
					(close t))
  "Write an image array A to a new fits file. Write the WCS if is given"
 
  (declare (type (or (simple-array single-float (* *))
		     (simple-array double-float (* *))
		     (simple-array (signed-byte 32) (* *))
		     (simple-array (unsigned-byte 32) (* *))
		     (simple-array (signed-byte 16) (* *))
		     (simple-array (unsigned-byte 16) (* *))
		     (simple-array (signed-byte 8) (* *))
		     (simple-array (unsigned-byte 8) (* *))
		     (simple-array (signed-byte 1) (* *))
		     (simple-array (unsigned-byte 1) (* *)))
		 a)
	   (type string fits-name)
	   (type (member :byte :short :long :float :double) type))

  (when (and (probe-file fits-name)
	     (not overwrite))
    (error "File ~A already exists and OVERWRITE is NIL" fits-name))
  ;;
  (let ((ff nil)
	(naxes (vector (array-dimension a 1) (array-dimension a 0)))
	(success nil))
    (unwind-protect 
	 (progn
	   (setf ff (cf:create-fits-file fits-name :overwrite overwrite))
	   (add-image-to-fits-file ff type naxes :create-data a)
	   (when wcs (write-wcs wcs ff))
	   (setf success t))
      ;;
      ;; in case of failure or if CLOSE is T, close file
      (progn
	(when (and ff (or (not success) close))
	  (close-fits-file ff))))
      
    ff))

(defunL write-2d-image-to-new-fits-file (a fits-name &key (overwrite nil)
					 (wcs nil) 
					 (type :float)
					 (headers nil)
					 (primary-hdu nil)
					 (close t))
  "Write an image array A to a new fits file. Write the WCS if is
given.  Headers can written as well, with HEADERS consisting of
 ((KEY VALUE [COMMENT) ....).

If PRIMARY-HDU is set, then create a NAXIS=0 primary extension; this may
also be list of headers to put into the primary extension."
 
  (declare (type (or (simple-array single-float (* *))
		     (simple-array double-float (* *))
		     (simple-array (signed-byte 32) (* *))
		     (simple-array (unsigned-byte 32) (* *))
		     (simple-array (signed-byte 16) (* *))
		     (simple-array (unsigned-byte 16) (* *))
		     (simple-array (signed-byte 8) (* *))
		     (simple-array (unsigned-byte 8) (* *))
		     (simple-array (signed-byte 1) (* *))
		     (simple-array (unsigned-byte 1) (* *)))
		 a)
	   (type string fits-name)
	   (type (member :byte :short :long :float :double) type))

  (when (and (probe-file fits-name)
	     (not overwrite))
    (error "File ~A already exists and OVERWRITE is NIL" fits-name))
  ;;
  (let ((ff nil)
	(bitpix (type-to-bitpix type))
	(naxes (vector (array-dimension a 1) (array-dimension a 0)))
	(success nil))

    (flet ((write-headers (hdrs)
	     (loop for head in hdrs
		   for key = (first head) and val = (second head) and comment = (third head)
		   when (not
			 (member key '("SIMPLE" "BITPIX" "NAXIS"
				       "NAXIS1" "NAXIS2"
				       "BZERO" "SCALE"
				       ;; these can result from compressed
				       ;; images and will confuse ds9
				       "XTENSION" "TTYPE1" "TFORM1" "PCOUNT"
				       "ZIMAGE"
				       "ZCMPTYPE" "ZNAME1" "ZTILE1" "ZTILE2")
				 :test 'equalp))
		     do
			(cf:write-fits-header ff key val :comment comment))))
      ;;
      (unwind-protect 
	   (progn
	     (setf ff (cf:create-fits-file fits-name :overwrite overwrite
						     :make-empty-primary primary-hdu))
	     (when (listp primary-hdu)
	       (write-headers primary-hdu))
	     (if primary-hdu
		 (add-image-to-fits-file 
		  ff type
		  (vector (array-dimension a 1) (array-dimension a 0)))
		 ;; else just write headers
		 (progn
		   ;; add headers before image
		   (cf:write-fits-header ff "SIMPLE" T)
		   (cf:write-fits-header ff "BITPIX" bitpix) ;; will get changed
		   (cf:write-fits-header ff "NAXIS" 2)
		   (cf:write-fits-header ff "NAXIS1" (array-dimension a 1))
		   (cf:write-fits-header ff "NAXIS2" (array-dimension a 0))))
	     (write-headers headers)
	     (when wcs (write-wcs wcs ff))
	     
	     (write-image-cube-for-fits-pointer
	      (fits-file-fptr ff)
	      #(1 1) naxes ;; FP and LP
	      a  :throw-error t)
	     
	     (setf success t))
	;;
	;; in case of failure or if CLOSE is T, close file
	(progn
	  (when (and ff (or (not success) close))
	    (close-fits-file ff))))
      
      ff)))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines to iterate over image sections, block by block


;; helper function for iterate-over-image-sections
(defun %iterate-pixels (func nxmin nxmax nymin nymax 
			&key (nx-block 1024) (ny-block 1024)
			(nx-overlap 0) (ny-overlap 0))
  "Call function (FUNC IX0 IX1 IY0 IY1) so that all values of
NXMIN<=IX<=NXMAX and NYMIN<=IY<=NYMAX are covered, in steps of NX, so
that IX1-IX0=NX, and IY1-IY0=NY.  Overlap X and Y by NX-OVERLAP,
NY-OVERLAP"
  (loop 
   for iy0 from nymin to nymax by (- ny-block ny-overlap)
   for iy1 = (min (+ iy0 ny-block -1) nymax)
   do
   (loop 
    for ix0 from nxmin to nxmax by (- nx-block nx-overlap)
    for ix1 = (min (+ ix0 nx-block -1) nxmax)
    do
    (funcall func ix0 ix1 iy0 iy1)
    ;; terminate x loop when outer boundary crossed the edge
    until (= ix1 nxmax))
   ;; terminate y loop when outer boundary crosses the edgge
   until (= iy1 nymax)))



;; helper function for iterate-over-image-sections
(defun %get-image-bounds (fits xmin xmax ymin ymax)
  (if (and xmin xmax ymin ymax) ;; if all value supplied, don't bother with file
      (values xmin xmax ymin ymax)
      ;; if a FITS-FILE, read the image size
      (if (cf:fits-file-p fits)
	  (progn
	    (if (not (eq (cf:fits-file-current-hdu-type fits) :image))
		(error "fits file type is not :IMAGE - cannot get image size"))
	    (if (not (cf:fits-file-current-image-size fits))
		(error "fits file type is not IMAGE"))
	    (let ((size-vec (cf:fits-file-current-image-size fits)))
	      (values (or xmin 1)
		      (or xmax (aref size-vec 0) )
		      (or ymin 1)
		      (or ymax (aref size-vec 1)))))
	  ;; if it is a string, open fits file and use that
	  (cf:with-open-fits-file (fits ff :mode :input)
	    (%get-image-bounds ff xmin xmax ymin ymax)))))       


(defun %force-gc () ;; force garbage collection to clean up the memory a bit during iteration
  #+sbcl (sb-ext:gc :gen 1)
  nil)

(deftype %indexnum () '(integer 0 #.(ash most-positive-fixnum -1)))

(defmacro iterate-over-image-sections 
    ((imsec-var-list fits-list &key 
		     ;; optional vars to bind to arrays
		     array-var-list 
		     ;; the block size
		     (nx-block 1024) (ny-block 1024)
		     ;; first and last pixels - by default, 
					;  the bounds of the image
		     nx-min nx-max
		     ny-min ny-max
		     ;; how much overlap (this is HALF the overlap amount)
		     (nx-half-overlap 0)
		     (ny-half-overlap 0)
		     ;; if specified, the following
		     ;;  variables are bound to
		     ;;  the current starting and ending 
		     ;;  pixels in ENTIRE image, 
		     ;;  of current image section
		     ix0-var ix1-var iy0-var iy1-var
		     ;; variable that say if this is the starting/ending block
		     is-x-start-var is-x-end-var is-y-start-var is-y-end-var
		     ;; variables that give the NON-OVERLAPPED x,y range in
		     ;; the sub-image array coordinates
		     ix-im0-var ix-im1-var iy-im0-var iy-im1-var
		     ;;
		     ;; if T, then ix-im0-var etc don't extend to edges
		     ;; but stay a half-overlap distance from the edge
		     (empty-overlap-frame-at-edges nil)
		     (image-type :single-float) 
		     (read-wcs t)  
		     (recycle-arrays t)
		     (throw-error t))
     &body body)
  "Macro to iterate over images, reading an image subsection from each
image in FITS-LIST in parallel, binding the image section to a
variable in IMSEC-VAR-LIST. Each element of FITS-LIST is evaluated
once. 

If ARRAY-VAR-LIST is set to a list of variable names, then these
variables are bound to the two-dimensional numerical array of the
image in (IMAGE-SECTION-DATA IMAGE-SECTION). Arrays are indexed 
as (AREF ARRAY IY IX) as usual.

The images are read in blocks of size NX-BLOCK*NY-BLOCK.  If either 
of NX-BLOCK or NY-BLOCK is NIL, then the entire usable image is used.
The most efficient use is to read the entire X width in narrow strips
of Y (ie, set only NY-BLOCK, and keep NX-BLOCK as NIL).

By default, if one or more of keyword arguments NX-MIN, NX-MAX,
NY-MIN, NY-MAX are not specified, the unspecified ones are taken from
the size of the first image.

If keyword argument IMAGE-TYPE is a type like :SINGLE-FLOAT, then all
images are read as this type. IMAGE-TYPE can also be a list parallel
to the list of image-sections and images.

The keyword variables IX0-VAR, IX1-VAR, IY0-VAR, IY1-VAR, if
specified, are variable names that will be bound to the current pixel
range, and made available to the body of the macro. These are 1-based
coordinates, and are not trimmed for overlaps. 

The keywords variables NX-HALF-OVERLAP, NY-HALF-OVERLAP are the
half-overlap of adjacent reads, by default zero.

The keyword vars IX0-VAR IX1-VAR IY0-VAR IY1-VAR are optional variable
names that are bound to the current total image coordinates read into
the image sections.

The keyword vars IS-X-START-VAR IS-X-END-VAR IS-Y-START-VAR
IS-Y-END-VAR are optional variable names that are bound to booleans
telling whether or not the current image sections are the respective
edges (start/end) of the iamge.


If keyword EMPTY-OVERLAP-FRAME-AT-EDGES is set to T (NIL by default)
then the iteration over IX0-VAR, etc avoids the edges by the half-overlap
distances NX-HALF-OVERLAP, NY-HALF-OVERLAP.  

If keyword var RECYCLE-ARRAYS is T, then re-use the arrays for reading
the image section. This is the default.  RECYCLE-ARRAYS is evaluated
once every loop, but should really be T or NIL.

The keyword vars IX-IM0-VAR IX-IM1-VAR IY-IM0-VAR IY-IM1-VAR are
optional variable names to bind to the starting and ending indices
within the current array or image section, taking into account the
half-overlaps. In other words, iterating over the inclusive range
[IX-IM0-VAR, IX-IM1-VAR] guarantees that each pixel x index is hit
exactly once, regardless of slice size or overlap values.

INDICES: NX-MIN NX-MAX NY-MIN NY-MAX and IX0-VAR IX1-VAR IY0-VAR IY1-VAR 
         are 1-based like FITS.

         IX-IM0-VAR IX-IM1-VAR IY-IM0-VAR IY-IM1-VAR are 0-based for
         Lisp arrays Arrays are indexed as (AREF ARRAY IY IX) as usual

         To obtain the current absolute (FITS 1-based) coordinates
         from Lisp (image section array 0-based) coordinates, use 
           IX-FITS = IX-LISP + IX-IM0-VAR 
           IY-FITS = IY-LISP + IY-IM0-VAR 
         ie, when IX-LISP=0, the IX-FITS has value IX-IM0-VAR 
         

 
Example:

 (iterate-over-image-sections 
   ((imsec1 imsec2)        ;; variables to which to bind image sections
    (fitsfile1 fitsfile2)  ;; fits files to read
    :nx-min 1 :nx-max 1024 ;; range of pixels - optional
    :ny-min 1 :ny-max 1024 ;;    
    :nx-half-overlap 10 :ny-half-overlap 10 ;; overlap by 10 pixels
    ;; optionally bind the current pixel range in parent image to these variables
    :ix0-var ix0 :iy0-var iy0 :ix1-var ix1 :iy1-var iy1 
    :image-type :single-float ;; by default
     read-wcs t :throw-error t)
 ;; the body
 (various things to do to imsec1 and imsec 2) ...) "
  
  (when (not (= (length imsec-var-list) (length fits-list)))
    (error 
     "Error in macro CFITSIO:ITERATE-OVER-IMAGE-SECTIONS - length of imsec-var-list must match length of fits-list"))

  (when (and array-var-list
	     (not (= (length array-var-list) (length fits-list))))
    (error 
     "Error in macro CFITSIO:ITERATE-OVER-IMAGE-SECTIONS - length of array-var-list must match length of fits-list")) 

  (when (and (listp image-type) (not (= (length image-type) (length fits-list))))
    (error 
     "Error in macro CFITSIO:ITERATE-OVER-IMAGE-SECTIONS - image-type is a list (not just a symbol), but its length differs from length of fits-list."))


  (let ((%func-name (gensym "do-one-block-func"))
	(%nx-block-var (gensym "nx-block"))
	(%ny-block-var (gensym "ny-block")) 
	(image-type-list (if (listp image-type) 
			     image-type
			     (make-list (length imsec-var-list) 
					:initial-element image-type )))
	(%nx-overlap-var (gensym "nx-overlap"))
	(%ny-overlap-var (gensym "ny-overlap"))
	(%nx-half-overlap-var (gensym "nx-half-overlap"))
	(%ny-half-overlap-var (gensym "ny-half-overlap"))
	(%nx-min-var (gensym "nx-min"))
	(%ny-min-var (gensym "ny-min"))
	(%nx-max-var (gensym "nx-max"))
	(%ny-max-var (gensym "ny-max"))
	(%fits-vars (mapcar (lambda (dummy)
			      (declare (ignore dummy))
			      (gensym "fits")) 
			    fits-list)) 
	;;
	(%ix0-var (or ix0-var (gensym "ix0")))
	(%ix1-var (or ix1-var (gensym "ix1")))
	(%iy0-var (or iy0-var (gensym "iy0")))
	(%iy1-var (or iy1-var (gensym "iy1")))
	;;
	(%nx-edge-overlap-var (gensym "x-edge-overlap"))
	(%ny-edge-overlap-var (gensym "y-edge-overlap"))
	;;
	(%is-x-start-var (or is-x-start-var (gensym "is-x-start")))
	(%is-x-end-var (or is-x-end-var (gensym "is-x-end")))
	(%is-y-start-var (or is-y-start-var (gensym "is-y-start")))
	(%is-y-end-var (or is-y-end-var (gensym "is-y-end")))
	;;
	(%ix-im0-var (or ix-im0-var (gensym "ix-im0")))
	(%ix-im1-var (or ix-im1-var (gensym "ix-im1")))
	(%iy-im0-var (or iy-im0-var (gensym "iy-im0")))
	(%iy-im1-var (or iy-im1-var (gensym "iy-im1"))) )
	
    ;; check if all the array types are OK
    (loop for type in image-type-list 
	  when (not (get-atype-for-type type))
	  do (error "Error in macro CFITSIO:ITERATE-OVER-IMAGE-SECTIONS. ~A is not a valid FITS array type."
		    type))
		    

    ;; in LET, bind the fits var to a variable, once
    `(let ,(loop for fv in %fits-vars and f in fits-list
		  collect (list fv f))
      ;;
      (multiple-value-bind (,%nx-min-var ,%nx-max-var ,%ny-min-var ,%ny-max-var)
	  (%get-image-bounds ,(first %fits-vars) ,nx-min ,nx-max ,ny-min ,ny-max)
	;;
	(let* ((,%nx-block-var (or ,nx-block (1+ (- ,%nx-max-var ,%nx-min-var))))
	       (,%ny-block-var (or ,ny-block (1+ (- ,%ny-max-var ,%ny-min-var))))
	       (,%nx-half-overlap-var ,nx-half-overlap)
	       (,%ny-half-overlap-var ,ny-half-overlap)
	       (,%nx-overlap-var (* 2 ,%nx-half-overlap-var))
	       (,%ny-overlap-var (* 2 ,%ny-half-overlap-var))
	       ;; declare imsecs here so we save them from pass to pass
	       ,@(loop for var in imsec-var-list collect var))
	  ;;
	  (flet ((,%func-name (,%ix0-var ,%ix1-var ,%iy0-var ,%iy1-var)
		   (declare (type %indexnum ,%ix0-var ,%ix1-var ,%iy0-var ,%iy1-var))
		   ;; set imsec vars here - LET and then SETF is to allow
		   ;; reuse of the data arrays
		   ,@(loop for var in imsec-var-list
			   for fits in %fits-vars
			   for k from 0
			   for type  = (nth k image-type-list)  
			   collect
			   (list 'setf var 
				 `(cf:read-image-section 
				   ,fits 
				   :fp (vector ,%ix0-var ,%iy0-var)
				   :lp (vector ,%ix1-var ,%iy1-var) 
				   :read-wcs ,read-wcs
				   :data-array (when (and ,recycle-arrays ,var)
						 (image-section-data ,var))
				   :throw-error ,throw-error
				   :type ,type)))
		   (let* ((,%is-x-start-var (= ,%ix0-var ,%nx-min-var))
			  (,%is-x-end-var   (= ,%ix1-var ,%nx-max-var))
			  (,%is-y-start-var (= ,%iy0-var ,%ny-min-var))
			  (,%is-y-end-var   (= ,%iy1-var ,%ny-max-var))
			  ;;
			  (,%nx-edge-overlap-var
			   (if ,empty-overlap-frame-at-edges ,%nx-half-overlap-var 0))
			  (,%ny-edge-overlap-var 
			   (if ,empty-overlap-frame-at-edges ,%ny-half-overlap-var 0))
			  ;; 
			  (,%ix-im0-var (if ,%is-x-start-var 
					    ,%nx-edge-overlap-var
					    ,%nx-half-overlap-var))
			  (,%ix-im1-var (- ,%ix1-var ,%ix0-var 
					   (if ,%is-x-end-var 
					       ,%nx-edge-overlap-var
					       ,%nx-half-overlap-var)))
			  (,%iy-im0-var (if ,%is-y-start-var 
					    ,%ny-edge-overlap-var 
					    ,%ny-half-overlap-var))
			  (,%iy-im1-var (- ,%iy1-var ,%iy0-var 
					   (if ,%is-y-end-var 
					       ,%nx-edge-overlap-var 
					       ,%ny-half-overlap-var))))
		     (declare (type %indexnum ,%ix-im0-var  ,%ix-im1-var  ,%iy-im0-var  ,%iy-im1-var
				    ,%nx-edge-overlap-var ,%ny-edge-overlap-var)
			      (ignorable ,%is-x-start-var ,%is-x-end-var
					 ,%is-y-start-var ,%is-y-end-var
					 ,%ix-im0-var  ,%ix-im1-var  ,%iy-im0-var  ,%iy-im1-var))
		     ;;
		     (let ,(loop for var in array-var-list
				 for ivar in imsec-var-list
				 collect
				 (list var `(cf:image-section-data ,ivar)))
		       ;;
		       ;; type-declare the arrays if we are binding to them
		       ,@(when array-var-list
			       (loop for image-type in image-type-list
				     for array in array-var-list
				     for lisp-type = (get-atype-for-type image-type)
				     collect   `(declare 
						 (type 
						  (simple-array ,lisp-type (* *))
						  ,array))))
		       ;;
		       ;; and make the arrays ignorable 
		       ,@(when array-var-list
			       (loop for array in array-var-list
				     collect   `(declare (ignorable ,array))))
		       
		       
		       (%force-gc)
		       ,@body
		       (%force-gc)))))
	    (%iterate-pixels #',%func-name
			     ,%nx-min-var ,%nx-max-var 
			     ,%ny-min-var ,%ny-max-var 
			     :nx-block ,%nx-block-var :ny-block ,%ny-block-var
			     :nx-overlap ,%nx-overlap-var 
			     :ny-overlap ,%ny-overlap-var)))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a test function that makes an image and fills it with 1.0 as it iterates
;; any value other than 1.0 indicates an error
(defun test-image-iterator (&key (fits-file "iterator-test.fits")
			    (xsize 1024) (ysize 999)
			    (nx-min nil) (nx-max nil)
			    (ny-min nil) (ny-max nil)
			    (nx-block 345) (ny-block 167) ;; size of blocking
			    (empty-overlap-frame-at-edges nil)
			    (nx-half-overlap 17) (ny-half-overlap 43))
  ;;
  (cf:write-2d-image-to-new-fits-file 
   (make-array (list ysize xsize) :element-type 'single-float
	       :initial-element 0.0) fits-file :overwrite :t)
  ;;
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (cfitsio:iterate-over-image-sections 
	((imsec-a )  
	 (ff)
	 :array-var-list (a1)
	 :image-type (:single-float)
	 :nx-block nx-block :ny-block ny-block 
	 :nx-min nx-min :nx-max nx-max 
	 :ny-min ny-min :ny-max ny-max 
	 :nx-half-overlap nx-half-overlap
	 :ny-half-overlap ny-half-overlap
	 :ix0-var ix0 :ix1-var ix1
	 :iy0-var iy0 :iy1-var iy1
	 ;;
	 :is-x-start-var is-x-start    :is-x-end-var is-x-end
	 :is-y-start-var is-y-start    :is-y-end-var is-y-end 
	 :empty-overlap-frame-at-edges empty-overlap-frame-at-edges
	 ;;
	 :ix-im0-var ix-im0 :ix-im1-var ix-im1
	 :iy-im0-var iy-im0 :iy-im1-var iy-im1)
      (format t "~13A  == array: ~4D ~4D ~4D ~4D == global: ~4D ~4D ~4D ~4D   start/end x:~A/~A y:~A/~A~%"
	      (array-dimensions a1)
	      ix-im0 ix-im1 iy-im0 iy-im1
	      ix0 ix1 iy0 iy1
	      is-x-start is-x-end   is-y-start is-y-end
	      )
	     (loop for ix from ix-im0 to ix-im1
		   do
		   (loop for iy from iy-im0 to iy-im1
			 do
			 (incf (aref a1 iy ix) 1.0)))
	     (cf:write-back-image-section imsec-a)))
  ;;
  (let* ((imsec (cf:read-image-section fits-file :type :single-float))
	 (data (cf:image-section-data imsec)))
    (loop for i below (array-total-size data)
	  for x = (row-major-aref data i)
	  when (not (= x 1.0))
	  do 
	  (format t "~A FAILED at element ~A~% ~A~%" fits-file i
		  (if (some 'identity (list nx-min nx-max ny-min ny-max  EMPTY-OVERLAP-FRAME-AT-EDGES))
		      " But this might be OK because some of NX-MIN, etc are not NIL, or  EMPTY-OVERLAP-FRAME-AT-EDGES is set"
		      ""))
	  (return nil)
	  finally
	  (format t "PASSED test~%"))))
		    
			    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defunL print-fits-table (fits &key (ext nil) (stream t) (nmax 100))
  "Print out a fits table in simple form. Throw error if more elements than NMAX"
  (cf:with-open-fits-file (fits ff)
    (when ext (cf:move-to-extension ff ext))
    (when (not (member (cf:fits-file-current-hdu-type ff)
		       '(:binary-tbl :ascii-tbl)))
      (error "Current HDU is not a table"))
    (when (> (cf:fits-file-current-table-nrows ff) nmax)
      (error "Too many rows to print"))
    (loop
     for name across (cf:fits-file-current-table-columns ff)
     do
     (multiple-value-bind (val nullv) 
	 (cf:read-column-from-fits-table ff name)
       (format stream "~A~%" name)
       (format stream "  values   ~S~%" val)
       (format stream "  NotNull  ~A~%" nullv)))))


(defgeneric list-extensions (ff)
  (:documentation   "Create a list extensions in a fits file as
   ((NUM NAME TYPE IMAGE-DIMENSION-OR-TABLE-COLUMNS) ...)"))


(defmethodL list-extensions ((ff fits-file))
  (let ((nhdu0 (fits-file-current-hdu-num ff))
	(ext-list nil))
    (loop
       for ihdu from 1 to (cf:fits-file-num-hdus ff)
       do
	 (cf:move-to-extension ff ihdu)
	 do (push 
	     (list (fits-file-current-hdu-num ff)
		   (fits-file-current-hdu-name ff)
		   (fits-file-current-hdu-type ff)
		   (cond ((eq (fits-file-current-hdu-type ff) :image)
			  (fits-file-current-image-size ff))
			 ((or (eq (fits-file-current-hdu-type ff) :ascii-tbl)
			      (eq (fits-file-current-hdu-type ff) :binary-tbl))
			  (fits-file-current-table-columns ff))))
	     ext-list))
    (setf ext-list (nreverse ext-list))

    (move-to-extension ff nhdu0)
    ext-list)) 
  

(defmethodL list-extensions ((ff string))
  (with-open-fits-file (ff fits-file)
    (list-extensions fits-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiline text headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; need to encode text because CFITSIO/FITS doesn't handle all chars nicely
(flet ((is-normal-char (c)
	 (or (alphanumericp c) (find c " !@#$^&*()[]:;'\|<>,.?/"))))
  
  (defun %multiline-encode-string (str)
    (declare (type string str))
    (with-output-to-string (s)
	       (loop for c across str
		     do (cond ((is-normal-char c)
			       (write-char c s))
			      (t
			       (when (> (char-code c) 255)
				 (error "Only 0-255 ascii characters allowed"))
			       (format s  "%~2,'0x" (char-code c)))))))

  (defun %multiline-decode-string (str)
    (declare (type string str))
     (with-output-to-string (s)
       (loop with n = (length str)
	     with i = 0
	     until (= i n)
	     for c = (aref str i) 
	     do (cond ((char= c #\%)
		       (write-char (code-char 
				    (parse-integer str :start (1+ i) :end (+ i 3) :radix 16))
				   s)
		       (incf i 3))
		      (t
		       (write-char c s)
		       (incf i 1))))))

  )


(defgeneric delete-multiline-header (ff key)
  (:documentation "Delete a multinline header consisting of 
Keys KEY.COMMENT KEY.PART1 KEY.PART2"))
  
(defmethodl delete-multiline-header ((ff fits-file) (key string))
  (cf:delete-fits-header ff (format nil "~A.COMMENT" key))
  (loop for i from 1
	for keyi = (format nil "~A.PART~D" key i)
	for old =  (cf:read-fits-header ff keyi :throw-error t)
	when old
	  do (cf:delete-fits-header ff keyi)
	until (not old)))


(defmethodl delete-multiline-header ((ff string) (key string))
  (with-open-fits-file (ff fits-file :mode :io)
    (delete-multiline-header fits-file key)))

(defgeneric write-multiline-header (ff key val &key comment)
  (:documentation "Writes a multiline header, splitting it into
  KEY.PART1 KEY.PART2, etc.  Uses simple %xx encoding scheme to
  preserve special chars.  If :COMMENT keyword is set, then the comment
  is given by KEY.COMMENT"))

(defmethodL write-multiline-header ((ff fits-file) (key string) (val string) &key (comment nil))
  ;; first delete the old header as long as it exists
  (delete-multiline-header ff key)
  (let ((slices ;; chop up the input string
	  (loop with encval = (%multiline-encode-string val)
		with len = 40 and n = (length encval)
		for istart = 0 then (+ istart len)
		for iend = (min n (+ istart len))
		until (>= istart n)
		;; surround slice with | to avoid clipping by cfitsio
		for slice = (concatenate 'string "|" (subseq encval istart iend) "|")
		collect slice)))
    (when comment 
      (cf:write-fits-header ff (format nil "~A.COMMENT" key) (format nil "~A" comment)))
    (loop for slice in slices 
	  for i from 1
	  for keyi = (format nil "~A.PART~D" key i)
	  do (cf:write-fits-header ff keyi slice :throw-error t  :update t))))



(defmethodL write-multiline-header ((ff string) (key string) (val string) &key (comment nil))
  (with-open-fits-file (ff fits-file :mode :io)
    (write-multiline-header fits-file key val :comment comment)))


(defgeneric read-multiline-header (ff key)
  (:documentation "Reads and concatenates a multiline text header
 KEY.PART1 KEY.PART2, etc."))

(defmethodL read-multiline-header ((ff fits-file) (key string))
  (let* ((have-contents nil)
	 (string-to-decode
	   (with-output-to-string (sout)
	    (loop for i from 1
		  for keyi = (format nil "~A.part~D" key i)
		  for val =  (cf:read-fits-header ff keyi :throw-error t)
		  when val
		    do (setf have-contents t)
		       (let ((nval (length val)))
			 (when (not (and (char= (aref val 0) #\|)
					 (char= (aref val (1- nval)) #\|)))
			   (error "multiline component ~A not wrapped in |" val))
			 (write-string (subseq val 1 (1- nval)) sout))
		  until (not val)))))
    (when have-contents
      (%multiline-decode-string string-to-decode))))
     
  

(defmethodL read-multiline-header ((ff string) (key string))
  (with-open-fits-file (ff fits-file :mode :input)
    (read-multiline-header fits-file key)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse-image-section-string (string)
  "Parse eg  \"[1:1024,2:2048]\" and return #(1 2024 2 2048)"
  (declare (type string string))
  (labels ((do-err (err-msg)
	   (error "Bad image section ~A: ~A - should be [x1:x2,y1:y2]" string err-msg))
	   (get-int (m1 m2 err-msg)
	     (loop for i from m1 to (1- m2)
		 when (not (digit-char-p (aref string i)))
		   do (do-err err-msg))
	     (parse-integer string :start m1 :end m2 :junk-allowed nil)))
    ;;
    (let ((n (length string))
	  mcol mcom x1 x2 y1 y2)
      (when (not (and (char= (aref string 0) #\[)
		      (char= (aref string (1- n)) #\])))
      (do-err "No enclosing square brackets"))
      (setf mcol (or (position #\: string) (do-err "No first colon")))
      (setf x1 (get-int 1 mcol "Bad X1"))
      (setf mcom (or (position #\, string :start mcol)
		     (do-err "No comma")))
      (setf x2 (get-int (1+ mcol) mcom  "Bad X2"))
      (setf mcol (or (position #\: string :start mcom)
		     (do-err "No second colon")))
      (setf y1 (get-int (1+ mcom) mcol "Bad Y1"))
      (setf y2 (get-int (1+ mcol) (1- n) "Bad Y2"))

      (vector x1 x2 y1 y2))))
	   



;; the following routine is essentially READ-IMAGE-SECTION
;; but allowing for reading beyond the limits of the image
;;
(defun read-fits-image-subsection-into-array
    (fits-file nxmin nymin nxmax nymax
     &key (out-of-bounds-value 0.0) null-value (extension 1))
  "Read pixels (one-based indices) in range [nxmin:nxmax,nymin,nymax] into
a single float image.  If the array bounds are out of bounds, then fill
pixels with OUT-OF-BOUNDS-VALUE.  Returns
  (VALUES FLOAT-ARRAY FLAG-BITARRAY WCS)
where FLOAT-ARRAY is an single float image array, BIT-ARRAY
is 1 where there is a valid image pixel, WCS is the world coordinate
system (one-based indices) for this subimage."
  (let* (;; can't use NAXIS1,2 headers because of commpressed images, so use imsize
	 (imsize (if (typep fits-file 'fits-file )
		     (fits-file-current-image-size fits-file)
		     (with-open-fits-file (fits-file ff)
		       (cf:move-to-extension ff extension)
		       (fits-file-current-image-size ff))))
	 (naxis1 (aref imsize 0))
         (naxis2 (aref imsize 1))
         (nx1 (max 1 nxmin))
         (nx2 (min naxis1 nxmax))
         (ny1 (max 1 nymin))
         (ny2 (min naxis2 nymax))
         (mx (- nxmax nxmin -1)) ;; size of output array
         (my (- nymax nymin -1))
         (arrout (make-array (list my mx)
			     :element-type 'single-float
			     :initial-element (float out-of-bounds-value 1.0)))
	 (bitarray (make-array (list my mx)
			       :element-type 'bit
			       :initial-element 0)))
    ;;
    ;; only read image when some of it is in bounds
    (when (and (and (<= nxmin naxis1)
		    (>= nxmax 1))
	       (and (<= nymin naxis2)
		    (>= nymax 1)))
      (let* ((imsec (cf:read-image-section 
                     fits-file
                     :type :single-float
		     :null-value null-value
                     :fp (vector nx1 ny1)
                     :lp (vector nx2 ny2)))
             (arrin (cf:image-section-data imsec)))
	     
	;;
	(declare (type (simple-array single-float (* *)) arrin arrout))
	;;
	(loop
	  with iyoff of-type fixnum = (- ny1 nymin)
	  with ixoff of-type fixnum = (- nx1 nxmin)
	  for ix of-type fixnum below (array-dimension arrin 1)
	  do (loop for iy below (array-dimension arrin 0)
		   do (setf (aref arrout
				  (+ iy iyoff) (+ ix ixoff))
			    (aref arrin iy ix))
		      (setf (aref bitarray
				  (+ iy iyoff) (+ ix ixoff))
			    1)))))

    ;; make a shifted wcs
    (let* ((wcs-whole-image (cf:read-wcs fits-file))
	   (wcs (when wcs-whole-image
		  (%shift-wcs-for-fp wcs-whole-image
				     (vector nxmin nymin)))))
      ;; if totally out of bounds, no image was read, and the initial value
      ;; with bitpix=[0], arrout=[out-of-bounds-value] is returned
      (values arrout bitarray wcs))))
