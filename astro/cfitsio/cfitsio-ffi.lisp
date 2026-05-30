;; define the FFI interface to CFITSIO
 
;; CFITSIO VERSION SENSITIVE - This is v 3.005 beta - structures may
;; change with version!
(in-package cfitsio)

(eval-when (:compile-toplevel)
  ;; enable some workarounds for case of no typed arrays
  #+abcl (pushnew :no-typed-arrays *features*)
  )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first load the libraries - cffi:*foreign-library-directories* should
;; be set correctly

(eval-when (:load-toplevel)
  
  (cffi:define-foreign-library libcfitsio
    (:darwin (:or "libcfitsio.10.dylib" "/opt/local/lib/libcfitsio.10.dylib"
		  "/opt/local/lib/libcfitsio.dylib" "libcfitsio.dylib"))
    (:unix (:or
	    "libcfitsio.so.10.4.6.3-DOCKER_VERSION" ;; for COMA
	    "libcfitsio.so.10.4.6.3" ;; latest
		"libcfitsio.so.10" "libcfitsio.so")))
  
  (cffi:define-foreign-library libwcs
    (:darwin (:or "libwcs.dylib" "/opt/local/lib/libwcs.dylib"))
    (:unix (:or "libwcs.so")))

  (cffi:use-foreign-library libcfitsio)
  (cffi:use-foreign-library libwcs)
  )
 


#+ccl (format t "WARNING: This will not work on CCL because some routines are
  LONG-LONG but CCL expects them to be defined as LONG.  Must figure this out.")
	
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:load-toplevel :execute :compile-toplevel)

  (defconstant +FLEN_FILENAME+ 1025)
  (defconstant +FLEN_KEYWORD+ 72)
  (defconstant +FLEN_CARD+ 81)
  (defconstant +FLEN_VALUE+ 71)
  (defconstant +FLEN_COMMENT+ 73)
  (defconstant +FLEN_ERRMSG+ 81)
  (defconstant +FLEN_STATUS+ 31)
  ;;
  (defconstant +TBIT+ 1)
  (defconstant +TBYTE+ 11)
  (defconstant +TLOGICAL+ 14)
  (defconstant +TSTRING+ 16)
  (defconstant +TUSHORT+ 20)
  (defconstant +TSHORT+ 21)
  (defconstant +TUINT+ 30)
  (defconstant +TINT+ 31)
  (defconstant +TULONG+ 40)
  (defconstant +TLONG+ 41)
  (defconstant +TINT32BIT+ 41)
  (defconstant +TFLOAT+ 42)
  (defconstant +TLONGLONG+ 81)
  (defconstant +TDOUBLE+ 82)
  (defconstant +TCOMPLEX+ 83) 
  (defconstant +TDBLCOMPLEX+ 163)
  ;;
  (defconstant +TYP_STRUC_KEY+ 10)
  (defconstant +TYP_CMPRS_KEY+ 20)
  (defconstant +TYP_SCAL_KEY+ 30)
  (defconstant +TYP_NULL_KEY+ 40)
  (defconstant +TYP_DIM_KEY+ 50)
  (defconstant +TYP_RANG_KEY+ 60)
  (defconstant +TYP_UNIT_KEY+ 70)
  (defconstant +TYP_DISP_KEY+ 80)
  (defconstant +TYP_HDUID_KEY+ 90)
  (defconstant +TYP_CKSUM_KEY+ 100)
  (defconstant +TYP_WCS_KEY+ 110)
  (defconstant +TYP_REFSYS_KEY+ 120)
  (defconstant +TYP_COMM_KEY+ 130)
  (defconstant +TYP_CONT_KEY+ 140)
  (defconstant +TYP_USER_KEY+ 150)
  ;;
  (defconstant +BYTE_IMG+ 8)
  (defconstant +SHORT_IMG+ 16)
  (defconstant +LONG_IMG+ 32)
  (defconstant +LONGLONG_IMG+ 64)
  (defconstant +FLOAT_IMG+ -32)
  (defconstant +DOUBLE_IMG+ -64)
  (defconstant +USHORT_IMG+ 20)
  (defconstant +ULONG_IMG+ 40)
  (defconstant +IMAGE_HDU+ 0)
  (defconstant +ASCII_TBL+ 1)
  (defconstant +BINARY_TBL+ 2)
  (defconstant +ANY_HDU+ -1)
  (defconstant +READONLY+ 0)
  (defconstant +READWRITE+ 1)
  (defconstant +FLOATNULLVALUE+ -9.11912E-36)
  (defconstant +DOUBLENULLVALUE+ -9.1191291391491E-36)
  (defconstant +MAX_COMPRESS_DIM+ 6)
  (defconstant +RICE_1+ 11)
  (defconstant +GZIP_1+ 21)
  (defconstant +PLIO_1+ 31)
  (defconstant +HCOMPRESS_1+ 41)
  (defconstant +TRUE+ 1)
  (defconstant +FALSE+ 0)
  (defconstant +CASESEN+ 1)
  (defconstant +CASEINSEN+ 0)
  (defconstant +MAXHDU+ 1000)
  (defconstant +GT_ID_ALL_URI+ 0)
  (defconstant +GT_ID_REF+ 1)
  (defconstant +GT_ID_POS+ 2)
  (defconstant +GT_ID_ALL+ 3)
  (defconstant +GT_ID_REF_URI+ 11)
  (defconstant +GT_ID_POS_URI+ 12)
  (defconstant +OPT_RM_GPT+ 0)
  (defconstant +OPT_RM_ENTRY+ 1)
  (defconstant +OPT_RM_MBR+ 2)
  (defconstant +OPT_RM_ALL+ 3)
  (defconstant +OPT_GCP_GPT+ 0)
  (defconstant +OPT_GCP_MBR+ 1)
  (defconstant +OPT_GCP_ALL+ 2)
  (defconstant +OPT_MCP_ADD+ 0)
  (defconstant +OPT_MCP_NADD+ 1)
  (defconstant +OPT_MCP_REPL+ 2)
  (defconstant +OPT_MCP_MOV+ 3)
  (defconstant +OPT_MRG_COPY+ 0)
  (defconstant +OPT_MRG_MOV+ 1)
  (defconstant +OPT_CMT_MBR+ 1)
  (defconstant +OPT_CMT_MBR_DEL+ 11)
  (defconstant +VALIDSTRUC+ 555)
  (defconstant +InputCol+ 0)
  (defconstant +InputOutputCol+ 1)
  (defconstant +OutputCol+ 2)
  (defconstant +SKIP_TABLE+ -104)
  (defconstant +SKIP_IMAGE+ -103)
  (defconstant +SKIP_NULL_PRIMARY+ -102)
  (defconstant +USE_MEM_BUFF+ -101)
  (defconstant +OVERFLOW_ERR+ -11)
  (defconstant +PREPEND_PRIMARY+ -9)

  ;; compression constants
  (defconstant +GZIP_1+      21)
  (defconstant +GZIP_2+      22)
  (defconstant +PLIO_1+      31)
  (defconstant +HCOMPRESS_1+ 41)
  (defconstant +BZIP2_1+     51);; not publicly supported;
  (defconstant +NOCOMPRESS+  -1)


  
  (defmacro deferror (symbol err-code)
    `(progn
       (defconstant ,symbol ,err-code)))
  
  (deferror +SAME_FILE+ 101)
  (deferror +TOO_MANY_FILES+ 103)
  (deferror +FILE_NOT_OPENED+ 104)
  (deferror +FILE_NOT_CREATED+ 105)
  (deferror +WRITE_ERROR+ 106)
  (deferror +END_OF_FILE+ 107)
  (deferror +READ_ERROR+ 108)
  (deferror +FILE_NOT_CLOSED+ 110)
  (deferror +ARRAY_TOO_BIG+ 111)
  (deferror +READONLY_FILE+ 112)
  (deferror +MEMORY_ALLOCATION+ 113)
  (deferror +BAD_FILEPTR+ 114)
  (deferror +NULL_INPUT_PTR+ 115)
  (deferror +SEEK_ERROR+ 116)
  (deferror +BAD_URL_PREFIX+ 121)
  (deferror +TOO_MANY_DRIVERS+ 122)
  (deferror +DRIVER_INIT_FAILED+ 123)
  (deferror +NO_MATCHING_DRIVER+ 124)
  (deferror +URL_PARSE_ERROR+ 125)
  (deferror +RANGE_PARSE_ERROR+ 126)
  (deferror +SHARED_ERRBASE+ 150)
  (deferror +SHARED_BADARG+   (+ +SHARED_ERRBASE+ 1))
  (deferror +SHARED_NULPTR+   (+ +SHARED_ERRBASE+ 2))
  (deferror +SHARED_TABFULL+  (+ +SHARED_ERRBASE+ 3))
  (deferror +SHARED_NOTINIT+  (+ +SHARED_ERRBASE+ 4))
  (deferror +SHARED_IPCERR+   (+ +SHARED_ERRBASE+ 5))
  (deferror +SHARED_NOMEM+    (+ +SHARED_ERRBASE+ 6))
  (deferror +SHARED_AGAIN+    (+ +SHARED_ERRBASE+ 7))
  (deferror +SHARED_NOFILE+   (+ +SHARED_ERRBASE+ 8))
  (deferror +SHARED_NORESIZE+ (+ +SHARED_ERRBASE+ 9))
  (deferror +HEADER_NOT_EMPTY+ 201)
  (deferror +KEY_NO_EXIST+ 202)
  (deferror +KEY_OUT_BOUNDS+ 203)
  (deferror +VALUE_UNDEFINED+ 204)
  (deferror +NO_QUOTE+ 205)
  (deferror +BAD_KEYCHAR+ 207)
  (deferror +BAD_ORDER+ 208)
  (deferror +NOT_POS_INT+ 209)
  (deferror +NO_END+ 210)
  (deferror +BAD_BITPIX+ 211)
  (deferror +BAD_NAXIS+ 212)
  (deferror +BAD_NAXES+ 213)
  (deferror +BAD_PCOUNT+ 214)
  (deferror +BAD_GCOUNT+ 215)
  (deferror +BAD_TFIELDS+ 216)
  (deferror +NEG_WIDTH+ 217)
  (deferror +NEG_ROWS+ 218)
  (deferror +COL_NOT_FOUND+ 219)
  (deferror +BAD_SIMPLE+ 220)
  (deferror +NO_SIMPLE+ 221)
  (deferror +NO_BITPIX+ 222)
  (deferror +NO_NAXIS+ 223)
  (deferror +NO_NAXES+ 224)
  (deferror +NO_XTENSION+ 225)
  (deferror +NOT_ATABLE+ 226)
  (deferror +NOT_BTABLE+ 227)
  (deferror +NO_PCOUNT+ 228)
  (deferror +NO_GCOUNT+ 229)
  (deferror +NO_TFIELDS+ 230)
  (deferror +NO_TBCOL+ 231)
  (deferror +NO_TFORM+ 232)
  (deferror +NOT_IMAGE+ 233)
  (deferror +BAD_TBCOL+ 234)
  (deferror +NOT_TABLE+ 235)
  (deferror +COL_TOO_WIDE+ 236)
  (deferror +COL_NOT_UNIQUE+ 237)
  (deferror +BAD_ROW_WIDTH+ 241)
  (deferror +UNKNOWN_EXT+ 251)
  (deferror +UNKNOWN_REC+ 252)
  (deferror +END_JUNK+ 253)
  (deferror +BAD_HEADER_FILL+ 254)
  (deferror +BAD_DATA_FILL+ 255)
  (deferror +BAD_TFORM+ 261)
  (deferror +BAD_TFORM_DTYPE+ 262)
  (deferror +BAD_TDIM+ 263)
  (deferror +BAD_HEAP_PTR+ 264)
  (deferror +BAD_HDU_NUM+ 301)
  (deferror +BAD_COL_NUM+ 302)
  (deferror +NEG_FILE_POS+ 304)
  (deferror +NEG_BYTES+ 306)
  (deferror +BAD_ROW_NUM+ 307)
  (deferror +BAD_ELEM_NUM+ 308)
  (deferror +NOT_ASCII_COL+ 309)
  (deferror +NOT_LOGICAL_COL+ 310)
  (deferror +BAD_ATABLE_FORMAT+ 311)
  (deferror +BAD_BTABLE_FORMAT+ 312)
  (deferror +NO_NULL+ 314)
  (deferror +NOT_VARI_LEN+ 317)
  (deferror +BAD_DIMEN+ 320)
  (deferror +BAD_PIX_NUM+ 321)
  (deferror +ZERO_SCALE+ 322)
  (deferror +NEG_AXIS+ 323)
  (deferror +NOT_GROUP_TABLE+ 340)
  (deferror +HDU_ALREADY_MEMBER+ 341)
  (deferror +MEMBER_NOT_FOUND+ 342)
  (deferror +GROUP_NOT_FOUND+ 343)
  (deferror +BAD_GROUP_ID+ 344)
  (deferror +TOO_MANY_HDUS_TRACKED+ 345)
  (deferror +HDU_ALREADY_TRACKED+ 346)
  (deferror +BAD_OPTION+ 347)
  (deferror +IDENTICAL_POINTERS+ 348)
  (deferror +BAD_GROUP_ATTACH+ 349)
  (deferror +BAD_GROUP_DETACH+ 350)
  (deferror +BAD_I2C+ 401)
  (deferror +BAD_F2C+ 402)
  (deferror +BAD_INTKEY+ 403)
  (deferror +BAD_LOGICALKEY+ 404)
  (deferror +BAD_FLOATKEY+ 405)
  (deferror +BAD_DOUBLEKEY+ 406)
  (deferror +BAD_C2I+ 407)
  (deferror +BAD_C2F+ 408)
  (deferror +BAD_C2D+ 409)
  (deferror +BAD_DATATYPE+ 410)
  (deferror +BAD_DECIM+ 411)
  (deferror +NUM_OVERFLOW+ 412)
  (deferror +BAD_DATE+ 420)
  (deferror +PARSE_SYNTAX_ERR+ 431)
  (deferror +PARSE_BAD_TYPE+ 432)
  (deferror +PARSE_LRG_VECTOR+ 433)
  (deferror +PARSE_NO_OUTPUT+ 434)
  (deferror +PARSE_BAD_COL+ 435)
  (deferror +PARSE_BAD_OUTPUT+ 436)
  (deferror +ANGLE_TOO_BIG+ 501)
  (deferror +BAD_WCS_VAL+ 502)
  (deferror +WCS_ERROR+ 503)
  (deferror +BAD_WCS_PROJ+ 504)
  (deferror +NO_WCS_KEY+ 505)
  (deferror +APPROX_WCS_KEY+ 506)
  (deferror +NO_CLOSE_ERROR+ 999)
  (deferror +NGP_ERRBASE+ 360)
  (deferror +NGP_OK+ 0)
  (deferror +NGP_NO_MEMORY+ (+ +NGP_ERRBASE+ 0))
  (deferror +NGP_READ_ERR+ (+ +NGP_ERRBASE+ 1))
  (deferror +NGP_NUL_PTR+ (+ +NGP_ERRBASE+ 2))
  (deferror +NGP_EMPTY_CURLINE+ (+ +NGP_ERRBASE+ 3))
  (deferror +NGP_UNREAD_QUEUE_FULL+ (+ +NGP_ERRBASE+ 4))
  (deferror +NGP_INC_NESTING+ (+ +NGP_ERRBASE+ 5))
  (deferror +NGP_ERR_FOPEN+ (+ +NGP_ERRBASE+ 6))
  (deferror +NGP_EOF+ (+ +NGP_ERRBASE+ 7))
  (deferror +NGP_BAD_ARG+ (+ +NGP_ERRBASE+ 8))
  (deferror +NGP_TOKEN_NOT_EXPECT+ (+ +NGP_ERRBASE+ 9))
  
  ) ;; end of eval-when



 




(eval-when (:load-toplevel :execute :compile-toplevel)
  (defconstant +maxhdu+ 1000)
  (defconstant +max-compress-dim+ 6)
  
  ;; most dimensions an image can have
  (defconstant +max-image-dims+ 100)
  ;; longest a dimension can be 
  (defconstant +max-image-dim+ (expt 2 25)))

;; note that strings are not simple by default in newer sbcl
;; so the following is needed to make simple strings
(defun make-simple-string (n &key (initial-element #\null))
  (make-array n :element-type 'base-char :initial-element initial-element))


;; returns 1/0 if cfitsio compiled to be reentrant
(waaf:sbdefine-alien-routine ("fits_is_reentrant" fits-is-reentrant) :int)
;;
(defun cfitsio-is-multithreaded ()
  (= (fits-is-reentrant) 1))

(waaf:sbdefine-alien-routine ("ffvers" fits-get-version) :void (version :float :out))
;; 
(defun cfitsio-version ()
  "Return loaded version of cfitiso as list of integers
 (MAJOR MINOR REVISION), with a returned second value being
the original floating point representation from ffvers()."
  (multiple-value-bind (dummy f)
      (fits-get-version)
    (declare (type float f)
	     (ignore dummy))
    (multiple-value-bind (vmaj frac1) (floor f)
      (multiple-value-bind (vmin frac2)  (floor (* 100 frac1))
	(multiple-value-bind (rev)  (round (* 100 frac2))
	  (values (list vmaj vmin rev)
		  f))))))

;; note different C name - "fits_open_file" is a C macro referring to "ffopen"
;; returns (values status filts-file-pointer-int status)
(waaf:sbdefine-alien-routine ("ffopen" fits-open-file-raw) :int 
  (fptr :pointer-int :out) 
  (filename :string)
  (mode :int)
  (status :int :in-out)) 

(waaf:sbdefine-alien-routine ("ffdopn" fits-open-data-raw) :int 
  (fptr :pointer-int :out) 
  (filename :string)
  (mode :int)
  (status :int :in-out))

(waaf:sbdefine-alien-routine ("fftopn" fits-open-table-raw) :int 
  (fptr :pointer-int :out)
  (filename :string)
  (mode :int)
  (status :int :in-out))

(waaf:sbdefine-alien-routine ("ffinit" fits-create-file-raw) :int 
  (fptr :pointer-int :out) 
  (filename :string)
  (status :int :in-out))

(waaf:sbdefine-alien-routine ("ffclos" fits-close-file-raw) :int 
  (fptr :pointer-int :in) 
  (status :int :in-out))


(waaf:sbdefine-alien-routine ("ffdelt" fits-delete-file-raw) :int 
  (fptr :pointer-int :in) 
  (status :int :in-out))

;; missing
;;   fits_file_name, fits_file_mode, fits_url_type
 
;; routines to move to an hdu
(waaf:sbdefine-alien-routine ("ffmahd" fits-movabs-hdu-raw) :int 
  (fptr :pointer-int :in) 
  (hdunum :unsigned-int :in)
  (hdutype :unsigned-int :out) 
  (status :int :in-out))

;; relative move
(waaf:sbdefine-alien-routine ("ffmrhd" fits-movrel-hdu-raw) :int 
  (fptr :pointer-int :in) 
  (nmove :int :in)
  (hdutype :unsigned-int :out) 
  (status :int :in-out))

;; named move
(waaf:sbdefine-alien-routine ("ffmnhd" fits-movnam-hdu-raw) :int 
  (fptr :pointer-int :in)
  (hdutype :int :in)  ;; this time, HDUTYPE is IN because it specified
  (extname :string)
  (extver :int)
  (status :int :in-out))

;; total number of hdus in file
(waaf:sbdefine-alien-routine ("ffthdu" fits-get-num-hdus-raw) :int 
  (fptr :pointer-int :in)
  (hdnum :int :out)
  (status :int :in-out))  

;; current hdu in file
(waaf:sbdefine-alien-routine ("ffghdn" fits-get-hdu-num-raw) :int 
  (fptr :pointer-int :in)
  (hdnum :int :out))

;; get hdu type
(waaf:sbdefine-alien-routine ("ffghdt" fits-get-hdu-type-raw) :int 
  (fptr :pointer-int :in)
  (hdutype :int :out)
  (status :int :in-out))

;; delete current hdu
(waaf:sbdefine-alien-routine ("ffdhdu" fits-delete-current-hdu-raw) :int 
  (fptr :pointer-int :in)
  (hdutype :int :out)
  (status :int :in-out))

;; copy current hdu from one file to another
(waaf:sbdefine-alien-routine ("ffcopy" fits-copy-hdu-raw) :int 
  (infptr :pointer-int :in) 
  (outfptr :pointer-int :in) 
  (morekeys :int :in)
  (status :int :in-out))

;; copy data (img,table) from one fits file CHDU to another CHDU
(waaf:sbdefine-alien-routine ("ffcpdt" fits-copy-data-raw) :int
  (infptr :pointer-int :in) 
  (outfptr :pointer-int :in) 
  (status :int :in-out))

;; copy data (img,table) from one fits file CHDU to another CHDU
(waaf:sbdefine-alien-routine ("ffcphd" fits-copy-header-raw) :int
  (infptr :pointer-int :in) 
  (outfptr :pointer-int :in) 
  (status :int :in-out))


;; write a key
(waaf:sbdefine-alien-routine ("ffpky" fits-write-key-raw) :int
  (fptr :pointer-int :in)
  (datatype :int :in)
  (keyname :string)
  (valueptr (* :int)) ;; a pointer 
  ;; note that comment can never be NIL because CFFI does not understand
  ;; NIL=NULLPOINTER for strings
  (comment :string)
  (status :int :in-out))

;; update a key
(waaf:sbdefine-alien-routine ("ffuky" fits-update-key-raw) :int
  (fptr :pointer-int :in)
  (datatype :int :in)
  (keyname :string)
  (valueptr (* :int)) ;; a pointer
  (comment :string)
  (status :int :in-out))

;; write a key with null value
(waaf:sbdefine-alien-routine ("ffpkyu" fits-write-null-key-raw) :int
  (fptr :pointer-int :in)
  (keyname :string)
  (comment :string)
  (status :int :in-out))

;; update a key with null value
(waaf:sbdefine-alien-routine ("ffukyu" fits-update-null-key-raw) :int
  (fptr :pointer-int :in)
  (keyname :string)
  (comment :string)
  (status :int :in-out))

;; write comment and history keywords
(waaf:sbdefine-alien-routine ("ffpcom" fits-write-comment-raw) :int
  (fptr :pointer-int :in)
  (comment :string)  ;; OK here, because comment not an array below
                      ;; see comment above in fits-update-header-raw
  (status :int :in-out))

(waaf:sbdefine-alien-routine ("ffphis" fits-write-history-raw) :int
  (fptrfind :long :in)
  (history :string) ;; ok here, we don't treat history as an array below
  (status :int :in-out))

;; write the current date
(waaf:sbdefine-alien-routine ("ffpdat" fits-write-date-raw) :int
  (fptr :pointer-int :in)
  (status :int :in-out))

;; write/update a whole card
(waaf:sbdefine-alien-routine
 ("ffprec" fits-write-record-raw) :int
  (fptr :pointer-int :in)
  (card :string)
  (status :int :in-out))
(waaf:sbdefine-alien-routine
 ("ffucrd" fits-update-card-raw) :int
  (fptr :pointer-int :in)
  (keyname :string)
  (card :string)
  (status :int :in-out))

(waaf:sbdefine-alien-routine
 ("ffgrec" fits-read-record-raw) :int
 (fptr :pointer-int :in)
 (keynum :int :in)
 (card (* char))
 (status :int :in-out))


(waaf:sbdefine-alien-routine
 ("ffhdr2str" fits-hdr2str-raw) :int
 (fptr :pointer-int :in)
 (nocomments :int :in)
 (exclist (* :void))  ;; actually **char
 (nexc :int :in)			     
 (header (* :void) :out) ;; a pointer that gets set to a string
 (nkeys :int :out)
 (status :int :in-out))


(waaf:sbdefine-alien-routine
 ("ffcnvthdr2str" fits-convert-hdr2str-raw) :int
 (fptr :pointer-int :in)
 (nocomments :int :in)
 (exclist (* :void))  ;; actually **char
 (nexc :int :in)			     
 (header (* :void) :out) ;; a pointer that gets set to a string
 (nkeys :int :out)
 (status :int :in-out)) 

(waaf:sbdefine-alien-routine
 ("fffree" fits-free-memory-raw) :int
 (header (* :char))
 (status :int :in-out))


;; delete a keyword by name - supports wildcards
(waaf:sbdefine-alien-routine ("ffdkey" fits-delete-key-raw) :int
  (fptr :pointer-int :in)
  (key :string)
  (status :int :in-out))

;;;;;;;;; fill in more stuff here - stuff to create new extensions

;; return number of existing keywords and the space avail. for more keywords
(waaf:sbdefine-alien-routine ("ffghsp" fits-get-hdrspace-raw) :int 
  (fptr :pointer-int :in)
  (keysexist :int :out)
  (morekeys  :int :out)
  (status :int :in-out))

;; read one key
(waaf:sbdefine-alien-routine ("ffgky" fits-read-key-raw) :int 
  (fptr :pointer-int :in)
  (datatype :int)
  (keyname :string)
  (value (* :int))  ;; use as a void *
  (comment (* :char)) ;; we must allocate this, or use NULL=NIL
  (status :int :in-out))

;; read one header line parsed into three components
(waaf:sbdefine-alien-routine ("ffgkey" fits-read-key-fields-raw) :int 
  (fptr :pointer-int :in)
  (keyname :string)
  (value-string (* :char)) ;; essentially a void *
  (comment (* :char)) ;; we must allocate this, or use NULL=NIL
  (status :int :in-out))

;; read nth key parsed into three components
(waaf:sbdefine-alien-routine ("ffgkyn" fits-read-keyn-raw) :int 
  (fptr :pointer-int :in)
  (keynum :unsigned-int :in)
  (keyname (* :char))
  (value-string (* :char)) ;; essentially a void *
  (comment (* :char)) ;; we must allocate this, or use NULL=NIL
  (status :int :in-out))

;; read a whole card
(waaf:sbdefine-alien-routine ("ffgcrd" fits-read-card-raw) :int 
  (fptr :pointer-int :in)
  (keyname :string)
  (card (* :char))
  (status :int :in-out))

;; get error string
(waaf:sbdefine-alien-routine ("ffgerr" fits-get-errstatus-raw) :void
  (status :int)
  (err_text (* :char)))

;; clear error stack
(waaf:sbdefine-alien-routine ("ffcmsg" fits-clear-errmsg-raw) :void
  )

;; get the image type (bitpix)
(waaf:sbdefine-alien-routine ("ffgidt" fits-get-img-type-raw) :int
  (fptr :pointer-int :in)
  (bitpix :int :out)
  (status :unsigned-int :in-out))
  
;; get the number of image dimensions
(waaf:sbdefine-alien-routine ("ffgidm" fits-get-img-dim-raw) :int
  (fptr :pointer-int :in)
  (naxis :int :out)
  (status :unsigned-int :in-out))

;; get size of the dimensions and put it into a :long array
(waaf:sbdefine-alien-routine ("ffgisz" fits-get-img-size-raw) :int
  (fptr :pointer-int :in)
  (maxdim :unsigned-int :in)
  (naxes (* :long))
  (status :unsigned-int :in-out))
  
;; read a rectangular subset of an image
(waaf:sbdefine-alien-routine ("ffgsv" fits-read-subset-raw) :int
  (fptr :pointer-int :in)
  (datatype :int)
  (fpixel (* :int)) ;; first pixel coords #(x0 y0)
  (lpixel (* :int)) ;; last  pixel coords #(x1 y1)
  (inc (* :int))    ;; increment in each dimension (use #(1 1 1..) ?)
  (nulval-ptr (* :int))
  (array (* :int))
  (anynul :unsigned-int :out)    ;; did we see any NULL?
  (status :unsigned-int :in-out))

;; write a rectangular subset of an image
(waaf:sbdefine-alien-routine ("ffpss" fits-write-subset-raw) :int
  (fptr :pointer-int :in)
  (datatype :int)
  (fpixel (* :int)) ;; first pixel coords #(x0 y0)
  (lpixel (* :int)) ;; last  pixel coords #(x1 y1)
  (array  (* :int)) ;; void  pointer
  (status :unsigned-int :in-out))


;; create an image inside a fits file - either primary
;; or a new extension
(waaf:sbdefine-alien-routine ("ffcrim" fits-create-image-raw) :int
  (fptr :pointer-int :in)
  (bitpix :int :in)
  (naxis :int :in)
  (naxes (* :long))
  (status :unsigned-int :in-out))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compare strings to see if they match, including wildcards
(waaf:sbdefine-alien-routine ("ffcmps" fits-compare-string-raw) :int
  (templt :string :in)
  (string :string :in)
  (casein :int :in) ;; case sensitive?
  (match :unsigned-int :out)
  (exact :unsigned-int :out))


;; get the type of a keyword value - returns one of :chars C,L,I,F,X
(waaf:sbdefine-alien-routine ("ffdtyp" fits-get-keytype-raw) :int
  (value :string :in)
  (char :int :out)
  (status :unsigned-int :in-out))


;; oh bugger...this table stuff is just tedious
;; get number of rows/cols in a table
(waaf:sbdefine-alien-routine ("ffgncl" fits-get-num-cols-raw) :int
  (fptr :pointer-int :in)		       
  (ncols :unsigned-int :out)
  (status :unsigned-int :in-out))

(waaf:sbdefine-alien-routine ("ffgnrw" fits-get-num-rows-raw) :int
  (fptr :pointer-int :in)		       
  (nrows :unsigned-int :out)
  (status :unsigned-int :in-out))

(waaf:sbdefine-alien-routine ("ffgcno" fits-get-colnum-raw) :int
  (fptr :pointer-int :in)
  (casein :int)
  (templt :string :in)
  (colnum :unsigned-int :out)
  (status :unsigned-int :in-out))

(waaf:sbdefine-alien-routine ("ffgcnn" fits-get-colname-raw) :int
  (fptr :pointer-int :in)
  (casein :int)
  (templt :string :in)
  (colname  (* :char))
  (colnum  :unsigned-int :in-out)
  (status :unsigned-int :in-out))


(waaf:sbdefine-alien-routine ("ffgtcl" fits-get-coltype-raw) :int
  (fptr :pointer-int :in)
  (colnum :int)
  (typecode :int :in-out)  ;; in/out because they can be NULL
  (repeat :long :in-out)
  (width :long :in-out)
  (status :unsigned-int :in-out))


(waaf:sbdefine-alien-routine ("ffgcdw" fits-get-col-display-width-raw) :int
  (fptr :pointer-int :in)
  (colnum :int)		       
  (width :int :out)
  (status :int :in-out))

(waaf:sbdefine-alien-routine ("ffgcv" fits-read-col-raw) :int
  (fptr :pointer-int :in)
  (datatype :int)
  (colnum :int)
  (firstrow :long-long)
  (firstelem :long-long)
  (nelements :long-long)
  (nulval (* :char)) ;; essentially void*
  (array  (* :char)) ;; essentially void*
  (anynul :int :out)
  (status :unsigned-int :in-out)) 

 
(waaf:sbdefine-alien-routine ("ffgcf" fits-read-colnull-raw) :int
  (fptr :pointer-int :in)
  (datatype :int)
  (colnum :int)
  (firstrow :long-long) ;; FIXME - these don't work in CCL because it wants :LONG
  (firstelem :long-long)   ;; even though the lengths are the same as in sbcl.  HUH?
  (nelements :long-long)
  (array  (* :char)) ;; essentially void*
  (nularray (* :char)) ;; really a char *, in this case
  (anynul :int :out)
  (status :unsigned-int :in-out))


(waaf:sbdefine-alien-routine ("ffpcl" fits-write-col-raw) :int
  (fptr :pointer-int :in)
  (datatype :int)
  (colnum :int)
  (firstrow :long-long)
  (firstelem :long-long)
  (nelements :long-long)
  (array  (* :char)) ;; essentially void*
  (status :unsigned-int :in-out))

 
;; NOTE SIMILAR NAME TO FITS-WRITE-COL-NULL-RAW
(waaf:sbdefine-alien-routine ("ffpcn" fits-write-colnull-raw) :int
  (fptr :pointer-int :in)
  (datatype :int)
  (colnum :int)
  (firstrow :long-long)
  (firstelem :long-long)
  (nelements :long-long)
  (array  (* :int)) ;; essentially void*
  (nulval (* :int)) ;; essentially void*
  (status :unsigned-int :in-out))

;; write NULL to entire specified colnum, in specified rows
;; NOTE SIMILAR NAME TO FITS-WRITE-COLNULL-RAW
(waaf:sbdefine-alien-routine ("ffpclu" fits-write-col-null-raw) :int
  (fptr :pointer-int :in)
  (colnum :int)
  (firstrow :long-long)
  (firstelem :long-long)
  (nelements :long-long)
  (status :unsigned-int :in-out))

;; fill every column with null values, in specified rows
(waaf:sbdefine-alien-routine ("ffprwu" fits-write-nullrows-raw) :int
  (fptr :pointer-int :in)
  (firstrow :long-long)
  (nelements :long-long)
  (status :unsigned-int :in-out))

(waaf:sbdefine-alien-routine  ("ffcrtb" fits-create-tbl-raw) :int 
  (fptr :pointer-int  :in)
  (tbltype :int)
  (naxis2 :long-long);; number of rows
  (tfields :int)   ;; guess that this is the number of cols
  (ttype (* :int)) ;; actually *char[], or **char
  (tform (* :int)) ;; actually *char[], or **char
  (tunit (* :int)) ;; actually *char[], or **char
  (extname :string)
			      (status :unsigned-int :in-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scaling parameters

;; set different BZERO, BSCALE from header
(waaf:sbdefine-alien-routine ("ffpscl" fits-set-bscale-raw) :int
  (fptr :pointer-int  :in)
  (scale :double :in)			     
  (zero  :double :in)
  (status :unsigned-int :in-out))

(waaf:sbdefine-alien-routine ("fftscl" fits-set-tscale-raw) :int
  (fptr :pointer-int  :in)
  (colnum :int :in)
  (scale :double :in)			     
  (zero  :double :in)
  (status :unsigned-int :in-out))


(waaf:sbdefine-alien-routine ("ffpnul" fits-set-imgnull-raw) :int
   (fptr :pointer-int  :in)
   (nulval :long-long :in)
   (status :unsigned-int :in-out))


(waaf:sbdefine-alien-routine ("ffsnul" fits-set-atblnull-raw) :int
   (fptr :pointer-int  :in)
   (colnum :int :in)
   (nulstr :string :in)
   (status :unsigned-int :in-out))

(waaf:sbdefine-alien-routine ("fftnul" fits-set-tblnull-raw) :int
   (fptr :pointer-int  :in)
   (colnum :int :in)
   (nulval :long-long :in)
   (status :unsigned-int :in-out))



	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compression routines - not used yet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(waaf:sbdefine-alien-routine 
 ("fits_set_compression_type" fits-set-compression-type-raw) :int
 (fptr :pointer-int :in)
 (comptype :int :in)
 (status :int :in-out))

(waaf:sbdefine-alien-routine 
 ("fits_get_compression_type" fits-get-compression-type-raw) :int
 (fptr :pointer-int :in)
 (comptype :int :in-out)
 (status :int :in-out))


(waaf:sbdefine-alien-routine 
 ("fits_set_tile_dim" fits-set-tile-dim-raw) :int
 (fptr :pointer-int :in)
 (ndim :int :in)
 (tilesize :long :in)
 (status :int :in-out))

(waaf:sbdefine-alien-routine 
 ("fits_get_tile_dim" fits-get-tile-dim-raw) :int
 (fptr :pointer-int :in)
 (ndim :int :in)
 (tilesize :long :in-out)
 (status :int :in-out))


(waaf:sbdefine-alien-routine 
 ("fits_set_quantize_level" fits-set-quantize-level-raw) :int
 (fptr :pointer-int :in)
 (qlevel :float :in)
 (status :int :in-out))

(waaf:sbdefine-alien-routine 
 ("fits_get_quantize_level" fits-get-quantize-level-raw) :int
 (fptr :pointer-int :in)
 (qlevel :float :in-out)
 (status :int :in-out))


(waaf:sbdefine-alien-routine 
 ("fits_set_hcomp_scale" fits-set-hcomp-scale-raw) :int
 (fptr :pointer-int :in)
 (scale :float :in)
 (status :int :in-out))

(waaf:sbdefine-alien-routine 
 ("fits_get_hcomp_scale" fits-get-hcomp-scale-raw) :int
 (fptr :pointer-int :in)
 (scale :float :in-out)
 (status :int :in-out))


(waaf:sbdefine-alien-routine 
 ("fits_set_hcomp_smooth" fits-set-hcomp-smooth-raw) :int
 (fptr :pointer-int :in)
 (smooth :float :in)
 (status :int :in-out))

(waaf:sbdefine-alien-routine 
 ("fits_get_hcomp_smooth" fits-get-hcomp-smooth-raw) :int
 (fptr :pointer-int :in)
 (smooth :float :in-out)
 (status :int :in-out))


(waaf:sbdefine-alien-routine
 ("fits_img_compress" fits-image-compress-raw) :int 
  (infptr :pointer-int :in)
  (outfptr :pointer-int :in)
  (status :int :in-out))

(waaf:sbdefine-alien-routine 
 ("fits_img_decompress" fits-image-decompress-raw) :int 
  (infptr :pointer-int :in)
  (outfptr :pointer-int :in)
  (status :int :in-out))

(waaf:sbdefine-alien-routine 
 ("fits_is_compressed_image" fits-is-compressed-image-raw) :int 
  (fptr :pointer-int :in)
  (status :int :in-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define some locking mechanisms because cfitsio is not 
;; necessarily thread safe (though it might be when compiled correctly,
;; we won't risk it)
(defvar *cfitsio-lock* (bordeaux-threads:make-recursive-lock "cfitsio-lock"))

;; if this variable is T, then recursive locking is applied to cfitsio calls
(defparameter *threadlock-cfitsio-calls* t) 
(defparameter *cfitsio-is-single-threaded* (not (cfitsio-is-multithreaded)))

;; put a thread lock around cfitsio calls, if requested by variable
;; *threadlock-cfitsio-calls*, or if this build of cfitsio is single-threaded
(defmacro %with-cfitsio-lock (&body body)
  (let ((func-sym (gensym "with-cfitsio-lock-func-")))
  `(flet ((,func-sym () ,@body)) ;; internal function that calls the body
     (if (or *threadlock-cfitsio-calls* *cfitsio-is-single-threaded*)
	 (bordeaux-threads:with-recursive-lock-held (*cfitsio-lock*) (,func-sym))
	 (,func-sym)))))


;; a wrapper for defun that ensures locking
(defmacro defunL (fname (&rest args) &body body)
  (let ((real-body body)
	(declare-list nil)
	(defun-doc nil))
    (when (stringp (first body))
      (setf defun-doc (car real-body))
      (setf real-body (cdr real-body)))
    (when (eq (caar real-body) 'declare)
      (setf declare-list (car real-body))
      (setf real-body (cdr real-body)))
    
  `(defun ,fname (,@args)  
    ,@(append
       (if defun-doc (list defun-doc))
       (if declare-list (list declare-list))
       (list `(%with-cfitsio-lock 
	       ,@real-body))))))

(defmacro defmethodL (fname (&rest args) &body body)
  (let ((real-body body)
	(declare-list nil)
	(defun-doc nil))
    (when (stringp (first body))
      (setf defun-doc (car real-body))
      (setf real-body (cdr real-body)))
    (when (eq (caar real-body) 'declare)
      (setf declare-list (car real-body))
      (setf real-body (cdr real-body)))
    
  `(defmethod ,fname (,@args) 
    ,@(append
       (if defun-doc (list defun-doc))
       (if declare-list (list declare-list))
       (list `(%with-cfitsio-lock  
	       ,@real-body))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some stuff from cffi

(defmacro %with-foreign-string ((var n) &body body)
  `(cffi:with-foreign-pointer (,var ,n) ,@body))

(defmacro %foreign-string-ref (string-ptr index)
  `(code-char (cffi:mem-ref ,string-ptr :char ,index)))

(defmacro %foreign-string-set (string-ptr index value)
  `(setf (cffi:mem-ref ,string-ptr :char ,index) (char-code ,value)))

(defmacro %foreign-pointer-to-int (pointer)
  `(cffi:pointer-address ,pointer))

;; trivial-garbage is used for finalization because it was ripped out of cffi
(defmacro %finalize (object func)
  `(tg:finalize ,object ,func))

(defmacro %cancel-finalization (object)
  `(tg:cancel-finalization  ,object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; higher level routines

(defunL fits-get-errstatus (status)
  (declare (type unsigned-byte status))
  (let ((s (make-simple-string 31))
	(status (logand status #xFFFF)))
    (waaf:with-array-as-foreign-pointer (s s-ptr :string)
     (fits-get-errstatus-raw status s-ptr))
    (clip-string-at-null s)))
  

(defun finalize-fits-file-pointer (object fptr)
  "a wrapper for finalization, to be used for closing fits file
pointers contained inside garbage collected objects"
  (%finalize object  (lambda () 
		       (%with-cfitsio-lock
			(close-fits-file-pointer fptr :throw-error nil)))))
 

(defun cancel-finalization (object)
  (%cancel-finalization object))
  
(defun to-simple-string (string) ;; convert to a simple string
  (declare (type string string))
  (cond ((simple-string-p string)
	 string)
	(t (loop with s = (make-simple-string (length string))
		 for i of-type (unsigned-byte 28) below (length string)
		 do (setf (aref s i) (aref string i))
		 finally (return s)))))

(defun to-nt-string (string) ;; convert to a simple null-terminated string
  (declare (type string string))
  (loop with s = (make-simple-string (1+ (length string)))
		 for i of-type (unsigned-byte 28) below (length string)
		 do (setf (aref s i) (aref string i))
		 finally
		 (setf (aref s (1- (length s))) #\null)
		 (return s)))


;; clip a string before first #\null
(defun clip-string-at-null (s)
  (declare (type simple-string s))
  (let ((n (position #\null s)))
    (if n (subseq s 0 n) s)))

    
  
;; internal function to throw an error if status is non-zero and
;; throw-error is true, else return NIL or the error string
;; extra is an extra argument to append at end, like a filename
(defunL handle-error-in-status (status where throw-error &optional extra)
  (let (err-string)
    (when (and throw-error (not (zerop status)))
      (when (not (zerop status))
	(setf err-string
	      (format nil "ERROR ~D in cfitsio package at ~A: ~A ~A~A" status
		      where (fits-get-errstatus status)
		      (if extra " -- " "")
		      (if extra extra "")))
	(fits-clear-errmsg-raw) ;; clear other errors
	;; note - err-string can have ~ char so have to do ~A
	(when throw-error (error "~A" err-string))))
    (cond ((zerop status) nil)
	  (t err-string))))


(defunL fits-compare-string (template string &optional (case-sensitive nil))
  "match string to template, where template can include wildcards
returns (values match exact-match)"
  (declare (type string template string))
  (multiple-value-bind (dummy match exact)
      (fits-compare-string-raw (to-simple-string template)
			       (to-simple-string string) 
			       (if case-sensitive 1 0))
    (declare (ignorable dummy))
    (values (= +TRUE+ match)
	    (= +TRUE+ exact))))


(defunL open-fits-file-pointer
  (file-name &key (read-only t) (throw-error t))
  "Open a fits file and return a pointer to a fitsfile.
This is a little dangerous, because there is no garbage collection
mechanism for the open file pointer
returns (values (or NIL fits-pointer) status error-string)"
  (declare (type string file-name))
  (let ((mode-int (if read-only +READONLY+ +READWRITE+))
	(file-name (to-simple-string file-name))
	err-string dummy fptr status)
    (declare (ignorable dummy))
    (multiple-value-setq (dummy fptr status)
      (fits-open-file-raw file-name mode-int 0))
    (setf err-string
	  (handle-error-in-status status "open-fits-file-pointer"
				  throw-error file-name))
    (values (if (zerop status) fptr nil)
	    status err-string)))
    

(defunL close-fits-file-pointer (fptr &key (throw-error t))
  "close a fits file pointer, 
returns (values (or t nil) status error-string)"
  (declare (type waaf:machine-pointer fptr))
  (multiple-value-bind (dummy status)
      (fits-close-file-raw fptr 0)
    (declare (ignorable dummy))
    (let ((err-string (handle-error-in-status
		       status "close-fits-file-pointer" throw-error)))
      (values (zerop status) status err-string))))
    

(defunL get-header-space-for-pointer (fptr &key (throw-error t))
  "Return (VALUES N-KEYS-EXISTING N-ROOM-FOR-MORE-KEYS
                  STATUS ERR-STRING) 
for a fits pointer."
  (declare (type waaf:machine-pointer fptr))
  (multiple-value-bind (dummy n-keysexist n-morekeys status)
      (fits-get-hdrspace-raw fptr 0)
    (let ((err-string
	    (handle-error-in-status status "get-header-space-for-pointer"
				    throw-error)))
       (values n-keysexist n-morekeys status err-string))))

  

(defunL get-record-line-for-pointer (fptr n &key (throw-error t))
  "Get record line N for FPTR - ie, the entire fits card as a string, 
including the key."
  (declare (type waaf:machine-pointer fptr)
	   (type (unsigned-byte 24) n))
    (let ((card (make-simple-string 81 :initial-element #\space))
	  dummy status err-string)
    (declare (ignorable dummy))
      (waaf:with-array-as-foreign-pointer
	  (card  card-ptr :string :copy-from-foreign t)
     (multiple-value-setq (dummy status)
	 (fits-read-record-raw fptr n card-ptr 0)))
    (cond
     ((not (= 0 status)) ;; an error
      (setf err-string
	    (handle-error-in-status status "get-record-line-for-pointer"
				    throw-error n))
      (values nil status err-string))
     (t ;; good result
      (values (clip-string-at-null card)
	      0 nil))))) ;; nil is non error-string


(defunL fits-free-memory (ptr &key (throw-error t))
  (let (err-string)
    (multiple-value-bind (dummy status)
	(fits-free-memory-raw ptr 0)
      (declare (ignore dummy))
      (cond
      ((not (= 0 status)) ;; an error
       (setf err-string
	     (handle-error-in-status status "fits-free-memory"
				     throw-error))
       (values nil status err-string))
      (t
       (values t 0 nil))))))
 

(defunL get-full-headers-as-string-for-pointer
    (fptr &key (exclude-comments t)
     (convert t) (output :lisp-string)
     (throw-error t))
  "Get all of the headers at current location in FPTR as a single
string.  If CONVERT is T (default) use its_convert_hdr2str, else
use fitse_hdr2str.

OUTPUT can be LISP-STRING, or C-STRING, in which case output is not
deallocated, and must be deallocated with free or FITS_FREE_MEMORY

Returns:  (VALUES LISP-OR-C-STRING N-HEADERS ERRORCODE ERRORSTRING)

"
  (declare (type waaf:machine-pointer fptr)
	   (type (member :c-string :lisp-string) output))
  (let (err-string)
    (multiple-value-bind (dummy header-ptr nkeys status)
	(if convert ;; NIL,0 means 'no excluded headers'
	    (fits-convert-hdr2str-raw fptr (if exclude-comments 1 0)
				      (cffi:null-pointer) 0 0)
	    (fits-hdr2str-raw fptr (if exclude-comments 1 0)
			      (cffi:null-pointer) 0 0))
      (declare (ignore dummy))
      (cond
	((not (= 0 status)) ;; an error
	 (setf err-string
	       (handle-error-in-status status "get-full-headers-as-string"
				       throw-error))
	 (values nil nil status err-string))
	(t ;; good result
	 (cond ((eq output :c-string)
		(values header-ptr nkeys 0 nil))
	       ((eq output :lisp-string)
		(let ((lisp-string (cffi:foreign-string-to-lisp header-ptr)))
		  (fits-free-memory header-ptr :throw-error throw-error)
		  (values lisp-string nkeys 0 nil)))))))))
      
	   


(defunL get-header-line-for-pointer (fptr key &key (throw-error t))
  "get the header line corresponding to KEY, or return NIL if none found,
return (values header-line error-code error-string)"
  (declare (type waaf:machine-pointer fptr)
	   (type string key))
  (let ((key (to-simple-string key)) 
	(card (make-simple-string 81 :initial-element #\space))
	dummy status err-string)
    (declare (ignorable dummy))
    (waaf:with-array-as-foreign-pointer (card  card-ptr :string :copy-from-foreign t)
     (multiple-value-setq (dummy status)
	 (fits-read-card-raw fptr key card-ptr 0)))
    (cond
     ((= status +KEY_NO_EXIST+) ;; handle this special case
      (values nil +KEY_NO_EXIST+  (format nil "key ~A does not exist" key)))
     ((not (= 0 status)) ;; an error
      (setf err-string
	    (handle-error-in-status status "get-header-line-for-pointer"
				    throw-error key))
      (values nil status err-string))
     (t ;; good result
      (values (clip-string-at-null card)
	      0 nil))))) ;; nil is non error-string


(defunL fits-get-keytype (value-string &key (throw-error t))
  "get the keytype for a value string 
returns (values type-symbol status error-string)
 where type-symbol is one of
 :COMPLEX :LONG :DOUBLE :STRING :LOGICAL :UNKNOWN
this is very primitive, and does  not check validity of fields, so
we might as well use our Lisp version instead"
  (let (dummy dtype status err-string type-symbol)
    (declare (ignorable dummy))
    (multiple-value-setq (dummy dtype status)
      (fits-get-keytype-raw value-string 0))
    (setf err-string
	  (handle-error-in-status
	   status "fits-get-keytype" throw-error value-string))
    ;;
    (setf type-symbol
	  (case dtype
	    (#\C :string)
	    (#\L :logical)
	    (#\I :long)
	    (#\F :double)
	    (#\X :complex)
	    (t   :unknown)))
    ;;
    (values type-symbol status err-string)))
     

(defunL get-header-line-value-and-comment-strings-for-pointer
  (fptr key &key (throw-error t))
  "get the header line value and comment corresponding to KEY, where
KEY is a string
- return NIL if none found, or return
 (values value-string comment-string error-code error-string)"
  (declare (type waaf:machine-pointer fptr)
	   (type string key))
  (let ((key (to-simple-string key))
	(value-string (make-simple-string 81 :initial-element #\space))
	(comment-string (make-simple-string 81 :initial-element #\space))
	dummy status err-string)
    (declare (ignorable dummy))
    (waaf:with-arrays-as-foreign-pointers 
	((comment-string comment-string-ptr :string  :copy-from-foreign t) 
	 (value-string value-string-ptr :string :copy-from-foreign t))
      (multiple-value-setq (dummy status)
        (fits-read-key-fields-raw
	 fptr key value-string-ptr comment-string-ptr 
	 0)))
    (cond
     ((= status +KEY_NO_EXIST+) ;; handle this special case
      (values nil nil status
	      (format nil "key ~A does not exist" key)))
     ((not (= 0 status)) ;; an error
      (setf err-string
	    (handle-error-in-status status "get-header-line-for-pointer"
				    throw-error key))
      (values nil nil status err-string))
     (t ;; good result
      (values
       (clip-string-at-null value-string)
       (clip-string-at-null comment-string)
       0 nil))))) ;; nil is non error-string





(defunL get-header-line-fields-for-keynum-for-pointer
  (fptr keynum &key (throw-error t))
  "get the header line value and comment corresponding to KEY, where
KEY is a string
- return NIL if none found, or return
 (values key-string value-string comment-string status error-string)"
  (declare (type waaf:machine-pointer fptr)
	   (type (unsigned-byte 28) keynum))
  (let ((key-string     (make-simple-string 81 :initial-element #\space))
	(value-string   (make-simple-string 81 :initial-element #\space))
	(comment-string (make-simple-string 81 :initial-element #\space))
	dummy status err-string)
    (declare (ignorable dummy))
    (waaf:with-arrays-as-foreign-pointers 
	((value-string value-string-ptr :string :copy-from-foreign t)
	 (key-string key-string-ptr :string :copy-from-foreign t)
	 (comment-string comment-string-ptr :string :copy-from-foreign t))
     (multiple-value-setq (dummy status)
       (fits-read-keyn-raw
	  fptr keynum key-string-ptr
	  value-string-ptr comment-string-ptr
	  0)))
    (cond
     ((= status +KEY_OUT_BOUNDS+) ;; handle this special case
      (values nil nil nil status
	      (format nil "key number ~A is out of bounds" keynum)))
     ((not (= 0 status)) ;; an error
      (setf err-string
	    (handle-error-in-status status "get-header-line-for-pointer"
				    throw-error keynum))
      (values nil nil nil status err-string))
     (t ;; good result
      (values
       (clip-string-at-null key-string)
       (clip-string-at-null value-string)
       (clip-string-at-null comment-string)
       0 nil))))) ;; nil is non error-string




(defunL delete-header-for-pointer (fptr key &key (throw-error t))
  "delete the header named by KEY from the fits file fptr
returns (values success status error-string) - T on successful
deletion or no-key-found, else NIL"
  (declare (type waaf:machine-pointer fptr)
	   (type string key))
  (let (err-string)
    (multiple-value-bind (dummy status)
	(fits-delete-key-raw fptr key 0)
      (declare (ignorable dummy))
      (when (not (or (zerop status) (= status +KEY_NO_EXIST+)))
	(setf err-string
	      (handle-error-in-status status "delete-header-for-pointer"
				      throw-error key)))
      ;; +KEY_NO_EXIST+ is return on success
      (when (= status +KEY_NO_EXIST+) (setf err-string "Key not found"))
      ;; status is ZERO if something was deleted, but +KEY_NO_EXIST+
      ;; if we tried to delete a non-existent key
      (values (or (zerop status) (= status +KEY_NO_EXIST+))
	      status err-string))))

 
    

(defunL read-header-value-for-pointer (fptr key type
				       &key
				       (throw-error t))
					    
  "read the value VALUE for key KEY into open fits file fptr,
TYPE is one of :COMPLEX :LONG :DOUBLE :STRING :LOGICAL
- warning: seems to throw unintuitive errors if KEY not found
  inside type conversions, so be sure we find key
- returns (values val found status err-string)
"
  (declare (type waaf:machine-pointer fptr)
	   (type string key)
	   (type (member :complex :long :longlong :double :string :logical) type))

  ;; set up vvec, a specialized array - for a complex, it is
  ;; the first two values.  we support the following tpes
  (let* ((comment (make-simple-string 80 :initial-element #\space))  
	 (vvec
	  (cond ((eq type :complex)
		 (make-array 1 :element-type '(complex double-float) :initial-element #C(0d0 0d0)))
		((eq type :long) 
		 (make-array 1 :element-type waaf:+lisp-long-type+ :initial-element 0))
		((eq type :longlong) 
		 (make-array 1 :element-type waaf:+lisp-llong-type+ :initial-element 0))
		((eq type :logical)
		 (make-array 1 :element-type '(signed-byte 32) :initial-element 0))
		((eq type :double)
		 (make-array 1 :element-type 'double-float :initial-element 0d0))
		((eq type :string) (make-simple-string 
				    80 :initial-element #\space))))
	 (type-code
	  (cond ((eq type :complex) +TDBLCOMPLEX+)
		((eq type :long) +TLONG+)
		((eq type :longlong) +TLONGLONG+)
		((eq type :logical) +TLOGICAL+)
		((eq type :double) +TDOUBLE+)
		((eq type :string) +TSTRING+)))
	 return-value dummy status error-string)
    ;;
    (declare (ignorable dummy))
    ;; 
    (flet ((call-fits-read-key-raw (vvec-ptr comment-ptr)
	     (multiple-value-setq (dummy status)
	       (fits-read-key-raw
		fptr type-code key
		vvec-ptr comment-ptr 0))))
      ;; need to handle all cases of type specially
      (cond 
	((eq type :complex) 
	 (waaf:with-arrays-as-foreign-pointers 
	     ((vvec vvec-ptr :double :complex t 
		    :lisp-type (complex double-float))
	      (comment comment-ptr :string))
	   (call-fits-read-key-raw vvec-ptr comment-ptr)))
	((eq type :long)
	 (waaf:with-arrays-as-foreign-pointers 
	     ((vvec vvec-ptr :long 
		    :lisp-type  #.waaf:+lisp-long-type+)
	      (comment comment-ptr :string))
	   (call-fits-read-key-raw vvec-ptr comment-ptr)))
	((eq type :longlong)
	 (waaf:with-arrays-as-foreign-pointers 
	     ((vvec vvec-ptr :long 
		    :lisp-type  #.waaf:+lisp-llong-type+)
	      (comment comment-ptr :string))
	   (call-fits-read-key-raw vvec-ptr comment-ptr)))
	((eq type :logical)
	 (waaf:with-arrays-as-foreign-pointers 
	     ((vvec vvec-ptr :int32 :lisp-type (signed-byte 32))
	      (comment comment-ptr :string))
	   (call-fits-read-key-raw vvec-ptr comment-ptr)))
	((eq type :double)
	 (waaf:with-arrays-as-foreign-pointers 
	     ((vvec vvec-ptr :double :lisp-type double-float)
	      (comment comment-ptr :string))
	   (call-fits-read-key-raw vvec-ptr comment-ptr)))
	((eq type :string)
	 (waaf:with-arrays-as-foreign-pointers 
	     ((vvec vvec-ptr :string)
	      (comment comment-ptr :string))
	   (call-fits-read-key-raw vvec-ptr comment-ptr)))))
	

    ;;
    ;; handle special case of key_no_exist
    (when (= status +KEY_NO_EXIST+)
      (return-from read-header-value-for-pointer
	(values nil nil nil ;; value comment found
		+KEY_NO_EXIST+
		(format nil "Key ~A not found" key))))
    ;;
    ;; now handle errors
    (setf error-string
	  (handle-error-in-status
	   status "read-header-value-for-pointer"
	   throw-error (format nil " key=~S type=~A" key type)))
    ;;
    ;; and set up the return value
    (when (zerop status)
      (setf return-value
	    (cond ((eq type :complex) (aref vvec 0))
		  ((eq type :long) (aref vvec 0))
		  ((eq type :longlong) (aref vvec 0))
		  ((eq type :logical) (not (zerop (aref vvec 0))))
		  ((eq type :double) (aref vvec 0))
		  ((eq type :string) (clip-string-at-null vvec)))))
      ;;
    (values return-value ;; null if error
	    (if comment (clip-string-at-null comment))
	    (zerop status) ;; T if we we found the keyword
	    status
	    error-string)))
    

;; a horribly complicated kludge to write big integers, because some
;; telescopes like CFHT don't limit themselves to 64 bit integers
(defun %write-header-value-bigint-for-fptr (fptr key value comment update)
  (declare (type string key comment)
	   (type integer value))
  (when (zerop (length key)) (error "Bad key - zero chars"))
  (let* ((not-hierarch (and (<= (length key) 8)
			   (every (lambda (c) (or (alphanumericp c)
						  (member c '(#\_ #\-))))
				  key)))
	 (keystr (cond (not-hierarch 
			(concatenate 'string (string-upcase key)
				     (make-string (- 8 (length key)) 
						  :initial-element #\space)))
		       (t (format nil "HIERARCH ~A" key))))
	 (keyval (format nil "~A= ~D" keystr value))
	 (fullcard (format nil "~A / ~A" keyval comment)))
    (when (> (length keyval) 80)
      (error "The FITS CARD <~A> is over 80 chars long - cannot express the integer ~A"
	     keyval value))
    (let (dummy status)
        (cond 
	  (update
	   (multiple-value-setq (dummy status)
	     (fits-update-card-raw fptr key fullcard 0)))
	  (t
	   (multiple-value-setq (dummy status)
	     (fits-write-record-raw fptr fullcard 0))))
      (values dummy status))))





(defunL write-header-value-for-pointer (fptr key value comment
				       &key
				       (throw-error t)
				       ;; if null-value is T, VALUE is ignored and
				       ;; the keyword is valueles
				       (null-value nil) 
				       (update t))
					    
  "write the value VALUE for key KEY into open fits file fptr,
comment can be NIL.   If NULL-VALUE is true, then the VALUE is ignored
and only the key and comment are written"
  (declare (type waaf:machine-pointer fptr)
	   (type string key)
	   (type (or null string) comment))
  ;; put the value into vvec, a specialized array - for a complex, it is
  ;; the first two values.  we support the following tpes
  (let* ((comment (if comment  ;; must always have comment because cffi does
		      (to-simple-string comment) ;; not undertand nil string!!
		      ""))
	 (key (to-simple-string key))
	 (type
	  (cond (null-value nil)
		((complexp value) :complex)
		((typep value '#.waaf:+lisp-long-type+) :long)
		((typep value '#.waaf:+lisp-llong-type+) :longlong)
		((integerp value) :bigint);; have to do big integers as a raw card
		((realp value) :double)
		((or (eq value T) (eq value NIL)) :logical)
		((stringp value) :string)
		((symbolp value) :string)
		(t (error
		    "Cannot handle value ~A of type <~A>"
		    value (type-of value)))))
	 (vvec 
	  (cond (null-value nil)
		((eq type :complex)
		 (make-array
		  1 :element-type '(complex double-float)
		  :initial-contents (list (complex (float (realpart value) 1d0)
						   (float (imagpart value) 1d0)))))
		((eq type :long)
		 (make-array 1 :element-type waaf:+lisp-long-type+
			     :initial-element value))
		((eq type :longlong)
		 (make-array 1 :element-type waaf:+lisp-llong-type+
			       :initial-element value))
		((eq type :logical) ;; logical=int=32bit  for both 32 & 64
		 (make-array 1 :element-type '(signed-byte 32)
			     :initial-element (if value 1 0)))
		((eq type :double)
		 (make-array 1 :element-type 'double-float
			     :initial-element (float value 1d0)))
		((eq type :string)
		 ;; must null-terminate bc we are passing string as address
		 (to-nt-string (if (symbolp value)
				   (symbol-name value) value)))))
	 (type-code
	  (cond (null-value nil)
		((eq type :complex) +TDBLCOMPLEX+)
		((eq type :long) +TLONG+)
		((eq type :longlong) +TLONGLONG+)
		((eq type :logical) +TLOGICAL+)
		((eq type :double) +TDOUBLE+)
		((eq type :string) +TSTRING+)))
	 dummy status error-string)
    ;;
    (declare (ignorable dummy))
    ;;
    (if (not null-value)
	(flet ((call-fits-write-key-raw (vvec-ptr)
		 (cond
		   (update 
		    (multiple-value-setq (dummy status)
		      (fits-update-key-raw
		       fptr type-code key vvec-ptr comment 0)))
		   (t
		    (multiple-value-setq (dummy status)
		      (fits-write-key-raw 
		       fptr type-code key vvec-ptr comment 0))))))
	(cond 
	  ;; special case for huge integers - we write the card directly
	  ((eq type :bigint)
	   (multiple-value-setq (dummy status)
	     (%write-header-value-bigint-for-fptr fptr key value comment update)))
	  ;;
	  ;; other more normal cases
	  ;;
	  ((eq type :complex)
	   (waaf:with-arrays-as-foreign-pointers 
	       ((vvec vvec-ptr :double :complex t 
		      :lisp-type (complex double-float)))
	     (call-fits-write-key-raw vvec-ptr)))
	  ((eq type :long)
	   (waaf:with-arrays-as-foreign-pointers 
	       ((vvec vvec-ptr :long :lisp-type #.waaf:+lisp-long-type+))
	     (call-fits-write-key-raw vvec-ptr)))
	  ((eq type :longlong)
	   (waaf:with-arrays-as-foreign-pointers 
	       ((vvec vvec-ptr :long :lisp-type #.waaf:+lisp-llong-type+))
	     (call-fits-write-key-raw vvec-ptr)))
	  ((eq type :logical)
	   (waaf:with-arrays-as-foreign-pointers 
	       ((vvec vvec-ptr :int32))
	     (call-fits-write-key-raw vvec-ptr)))
	  ((eq type :double)
	   (waaf:with-arrays-as-foreign-pointers 
	       ((vvec vvec-ptr :double))
	     (call-fits-write-key-raw vvec-ptr)))
	  ((eq type :string)
	   (waaf:with-arrays-as-foreign-pointers 
	       ((vvec vvec-ptr :string))
	     (call-fits-write-key-raw vvec-ptr)))))
	;;
	;; else the case of no val
	(cond
	  (update
	   (multiple-value-setq (dummy status)
	      (fits-update-null-key-raw
	       fptr key comment 0)))
	  (t
	   (multiple-value-setq (dummy status)
	     (fits-write-null-key-raw
	      fptr key comment 0)))))
    ;;
    ;; now handle errors
    (setf error-string
	  (handle-error-in-status
	   status "write-header-value-for-pointer"
	   throw-error (format nil " key=~S value=~A" key value)))
    ;;
    (values (zerop status) status error-string)))


(defunL write-comment-for-pointer (fptr comment &key (throw-error t))
  "Write a fits COMMENT into a header"
  (declare (type waaf:machine-pointer fptr)
	   (type (or symbol string) comment))
  (let ((comment (to-simple-string comment))
	(status 0)
	(dummy 0)
	(error-string))
    (declare (ignorable dummy))
    ;;
    (progn ;; 
     (multiple-value-setq (dummy status)
       (fits-write-comment-raw fptr comment 0)))
    ;;    
    (setf error-string
	  (handle-error-in-status
	   status "write-comment-for-pointer"
	   throw-error (format nil "comment=~A" comment)))
    (values (zerop status) status error-string)))


(defunL write-history-for-pointer (fptr history &key (throw-error t))
  "Write a fits HISTORY into a header"
  (declare (type waaf:machine-pointer fptr)
	   (type (or symbol string) history))
  (let ((history (to-simple-string history))
	(status 0)
	(dummy 0)
	(error-string))
    (declare (ignorable dummy))
    ;;
    (progn
     (multiple-value-setq (dummy status)
       (fits-write-history-raw fptr history 0)))
    ;;    
    (setf error-string
	  (handle-error-in-status
	   status "write-history-for-pointer"
	   throw-error (format nil "history=~A" history)))
    (values (zerop status) status error-string)))
    




(defunL go-to-extension-for-pointer (fptr extension 
				     &key
				     (relative nil) 
				     (hdu-type :any-hdu)
				     (extver 0)
				     (throw-error t))
  "go to an extension (a string or pointer), return NIL if no such extension
or T if extension found
 - EXTVER is the extension version, used when moving by name. ignored if 0
 - HDUTYPE is one of :image-hdu :ascii-tbl :binary-tbl :any-hdu
 - RELATIVE is to move by relative number; EXTENSION must be an integer
 - returns (values (or t nil) status error-string)
"
  (declare (type (or (unsigned-byte 20) string) extension)
	   (type waaf:machine-uint extver)
	   (type waaf:machine-pointer fptr)
	   (type (member :image-hdu :ascii-tbl :binary-tbl :any-hdu)
		 hdu-type))
  (when (and (integerp extension)
	     (zerop extension))
    (error "extension 0 not allowed"))
  (let ((hdu-type-n (cond ((eq hdu-type :any-hdu) +ANY_HDU+)
			  ((eq hdu-type :image-hdu) +IMAGE_HDU+)
			  ((eq hdu-type :ascii-tbl) +ASCII_TBL+)
			  ((eq hdu-type :binary-tbl) +BINARY_TBL+)))
	dummy status error-string)
    (declare (ignorable dummy))
    (cond
      (relative
       (when (not (integerp extension))
	 (error "If RELATIVE is True then EXTENSION must be an integer."))
       (multiple-value-setq (dummy status)
	 (fits-movrel-hdu-raw fptr extension 0)))
       ;; case of integer hdu
      ((integerp extension)
      (multiple-value-setq (dummy status)
	(fits-movabs-hdu-raw fptr extension 0)))
     ;; case of string (named) hdu
     (t
      (multiple-value-setq (dummy status)
	(fits-movnam-hdu-raw fptr hdu-type-n
			     (to-simple-string extension)
			     extver 0))))
    ;;
    (setf status (logand status #xffff)) ;; WHY IS THIS NEEDED?
    (cond
     ;; extension not found
     ((= status +UNKNOWN_EXT+) ;; special case of unknown extension
      (values nil +UNKNOWN_EXT+
	      (format nil "Unknown extension ~S" extension)))
     ;; other errors - WEIRD - errors seem to start at 100, but we get an error of 2
     ;; during a valid move.  So the following hack solves this.
     ((> status 100) 
      (setf error-string
	    (handle-error-in-status
	     status "go-to-extension-for-pointer"
	     throw-error (format nil " extension=~S" extension)))
      (values nil status error-string))
     ;; success
     (t
      (values t status nil))))) 
      
    
;; get the current hdu number 
(defunL get-current-extension-for-fits-pointer (fptr)
  "Get the current fits extension number,
returns only the hdu number, starting at 1 - no error reporting"
  (declare (type waaf:machine-pointer fptr))          
  (multiple-value-bind (dummy hdunum)
      (fits-get-hdu-num-raw fptr)
    (declare (ignorable dummy))
    hdunum))

;; get the current hdu number 
(defunL get-hdu-type-for-fits-pointer (fptr &key (throw-error t))
  "Get the hdu type  - can be :image :ascii-tbl or :binary-tbl"
  (declare (type waaf:machine-pointer fptr))          
  (multiple-value-bind (dummy hdutype-num status)
      (fits-get-hdu-type-raw fptr 0)
    (declare (ignorable dummy))
    (let ((err-string (handle-error-in-status
			 status "get-hdu-type-for-fits-pointer"
			 throw-error))
	  (hdutype (when (zerop status)
		     (case hdutype-num
		       (#.+IMAGE_HDU+ :image)
		       (#.+ASCII_TBL+ :ascii-tbl)
		       (#.+BINARY_TBL+ :binary-tbl)
		       (t :unknown)))))
      (values hdutype status err-string))))


(defunL delete-current-hdu-for-fits-pointer (fptr &key (throw-error t))
  "delete the current HDU in a fits pointer returns the new HDU type,
- can be :image :ascii-tbl or :binary-tbl"
  (declare (type waaf:machine-pointer fptr))          
  (multiple-value-bind (dummy hdutype-num status)
      (fits-delete-current-hdu-raw fptr 0)
    (declare (ignorable dummy))
    (let ((err-string (handle-error-in-status
			 status "delete-current-hdu-for-fits-pointer"
			 throw-error))
	  (hdutype (when (zerop status)
		     (case hdutype-num
		       (#.+IMAGE_HDU+ :image)
		       (#.+ASCII_TBL+ :ascii-tbl)
		       (#.+BINARY_TBL+ :binary-tbl)
		       (t :unknown)))))
      (values hdutype status err-string))))


(defunL copy-hdu-from-fits-pointer-to-fits-pointer (fptr-in fptr-out
						   &key 
						   (morekeys 0)
						   (throw-error t))
  "Copy current HDU from FPTR-IN to FPTR-OUT. The latter must be in
writable mode. Keyword MOREKEYS requests this many more keyword slots."
  (declare (type waaf:machine-pointer fptr-in fptr-out)
	   (type (unsigned-byte 20) morekeys))
  (multiple-value-bind (dummy status)
      (fits-copy-hdu-raw fptr-in fptr-out morekeys 0)
    (declare (ignorable dummy))
    (let ((err-string (handle-error-in-status
		       status "copy-hdu-from-fits-pointer-to-fits-pointer "
		       throw-error)))
      (values status err-string))))


(defunL copy-hdu-data-from-fits-pointer-to-fits-pointer
    (fptr-in fptr-out &key (throw-error t))
  "Copy current HDU data (image or table) from FPTR-IN to FPTR-OUT"
  (multiple-value-bind (dummy status)
      (fits-copy-data-raw fptr-in fptr-out 0)
    (declare (ignorable dummy))
    (let ((err-string (handle-error-in-status
		       status "copy-data-from-fits-pointer-to-fits-pointer "
		       throw-error)))
      (values status err-string))))


(defunL copy-header-from-fits-pointer-to-fits-pointer
    (fptr-in fptr-out &key (throw-error t))
  "Copy current HDU header (image or table) from FPTR-IN to FPTR-OUT.  If
target header is not empty, a new header is created."
  (multiple-value-bind (dummy status)
      (fits-copy-header-raw fptr-in fptr-out 0)
    (declare (ignorable dummy))
    (let ((err-string (handle-error-in-status
		       status "copy-header-from-fits-pointer-to-fits-pointer "
		       throw-error)))
      (values status err-string))))



(defunL get-number-of-extensions-for-fits-pointer (fptr &key (throw-error t))
  "Get the number of HDUs for a fits file pointer"
  (declare (type waaf:machine-pointer fptr))          
  (multiple-value-bind (dummy hdunum status)
      (fits-get-num-hdus-raw fptr 0)
    (declare (ignorable dummy))
    (let ((err-string (handle-error-in-status
			 status "get-number-of-extensions-for-fits-pointer"
			 throw-error)))
      (values hdunum status err-string))))




;; get the number of hdus in file
(defunL get-number-of-extension-for-fits-pointer (fptr &key (throw-error t))  
  "Get the current fits extension number,
returns only the hdu number, starting at 1 - no error reporting!"
  (declare (type waaf:machine-pointer fptr))          
  (multiple-value-bind (dummy hdunum status)
      (fits-get-num-hdus-raw fptr 0)
    (declare (ignorable dummy))
    (let ((error-string (handle-error-in-status
			 status "get-number-of-extension-for-fits-pointer"
			 throw-error)))
      (values (if (zerop status) hdunum nil)
	      status error-string))))


(defun bitpix-to-type (bitpix)
  (declare (type signed-byte bitpix))
  (cond ((= bitpix +BYTE_IMG+)   :byte)
	((= bitpix +SHORT_IMG+)  :short)
	((= bitpix +USHORT_IMG+) :ushort)
	((= bitpix +LONG_IMG+)   :long)
	((= bitpix +FLOAT_IMG+)  :float)
	((= bitpix +DOUBLE_IMG+) :double)
	(t (error "Unknown bitpix=~A" bitpix))))

(defun type-to-bitpix (type)
  (cond ((eq type :byte)   +BYTE_IMG+)
	((eq type :short)  +SHORT_IMG+)
	((eq type :ushort)  +USHORT_IMG+)
	((eq type :long)   +LONG_IMG+)
	((eq type :float)  +FLOAT_IMG+)
	((eq type :double) +DOUBLE_IMG+)
	(t (error "Unknown type=~A" type))))


(defunL get-image-type-for-fits-pointer (fptr &key (throw-error t))
  "return the image type - one of :byte :short :ushort :long :float :double"
    (declare (type waaf:machine-pointer fptr))          
    (multiple-value-bind (dummy bitpix status)
	(fits-get-img-type-raw fptr 0)
      (declare (ignorable dummy))
      (let ((error-string (handle-error-in-status
			   status "get-image-type-for-fits-pointer"
			   throw-error))
	    (type (when (zerop status) (bitpix-to-type bitpix))))
	(values
	 (and (zerop status) type)
	 status
	 error-string))))
	

(defunL get-num-dims-for-fits-pointer (fptr &key (throw-error t))
  "return the number of image dimensions"
    (declare (type waaf:machine-pointer fptr))          
    (multiple-value-bind (dummy naxis status)
	(fits-get-img-dim-raw fptr 0)
      (declare (ignorable dummy))
      (let ((error-string (handle-error-in-status
			   status "get-image-num-dims-for-fits-pointer"
			   throw-error)))
	(values
	 (and (zerop status) naxis)
	 status
	 error-string))))


(defunL get-image-size-for-fits-pointer (fptr maxdim &key (throw-error t))
  "return a vector of maxdims length with the length of each dimension"
  (declare (type waaf:machine-pointer fptr))
  (let ((naxes (make-array maxdim :element-type 'waaf:machine-long :initial-element 0))
	dummy status error-string)
    (declare (ignorable dummy))
    (waaf:with-array-as-foreign-pointer 
	(naxes naxes-ptr :long :copy-from-foreign t) 
     (multiple-value-setq (dummy status)
       (fits-get-img-size-raw fptr maxdim naxes-ptr 0)))
    (setf error-string
	  (handle-error-in-status
	   status "get-image-size-for-fits-pointer"
	   throw-error))
    (values
     (and (zerop status) naxes)
     status
     error-string)))


(defmacro fixnum-p (n)
  `(typep ,n 'fixnum))

(defunL read-image-cube-for-fits-pointer (fptr fp lp array &key (throw-error t)
					       #+no-typed-arrays (array-type nil)
					       (null-value nil))
  "read an image cube from the first pixel to the last pixel, of
image fptr. FP and LP are vectors with the pixel coordinates
of the first and last pixels.  array is a uniform array of some type.
 - returns (values array status error-string)
 - WARNING - if array is of type (unsigned-byte 8), but data are signed, the
   values will be bogus

If NULL-VALUE is set, then this will be placed into array elements matching the
FITS BLANK keyword.  If not set, null values are ignored.  But NULL-VALUE=0
is a special case in cfitsio, equivalent to no null value."
  (declare (type waaf:machine-pointer fptr)
	   (type vector fp lp)
	   (type simple-array array))
;  (print (list fptr fp lp array throw-error))
;  (error "stopping")
  (when (not (= (length fp) (length lp)))
    (error "FP and LP of different lengths"))
  (when (not (every (lambda (x) (and (fixnum-p x) (plusp x))) fp))
    (error "FP=~A has bogus elements. Must be positive integers" fp))
  (when (not (every (lambda (x) (and (fixnum-p x) (plusp x))) lp))
    (error "LP=~A has bogus elements. Must be positive integers" lp))
  (when (and null-value
	     (not (equalp (type-of null-value) (array-element-type array))))
    (error "The type of NULL-VALUE is ~A and does not match the array-element-type of the data array, which is ~A" (type-of null-value) (array-element-type array)))
  (let* ((npix 0) ;; number of pix needed
	 (npix-avail (array-total-size array)) ;; number in array
	 (ndims (length fp))
	 (fp (coerce fp  '(simple-array waaf:machine-long (*))))
	 (lp (coerce lp  '(simple-array waaf:machine-long (*))))
	 (inc (make-array ndims :element-type 'waaf:machine-long
			  :initial-element 1))
	 (bad-array-type nil)
	 (null-value-vec (if null-value
			     (make-array 1 
					 :element-type (array-element-type array)
					 :initial-element null-value)))
	 dummy status anynul err-string
	 )
    ;;
    (declare (ignorable dummy anynul))
    ;; count if we have enough bytes
    (loop
     with ntot of-type unsigned-byte = 1
     for i = 1 then (1+ i)
     for n1 across fp
     for n2 across lp
     do
     (if (> n1 n2) (error "fp[~d]=~d  >= lp[~d]=~d" i n1 i n2))
     (setf ntot (* ntot (1+ (- n2 n1))))
     finally (setf npix ntot))
    ;;
    (if (< npix-avail npix)
	(error "array must have ~d pixels, but it has only ~d"
	       npix npix-avail))
    ;;
    (waaf:with-arrays-as-foreign-pointers 
	((fp  fp-ptr  :long     :lisp-type #.waaf:+lisp-long-type+) 
	 (lp  lp-ptr  :long     :lisp-type #.waaf:+lisp-long-type+) 
	 (inc inc-ptr :long     :lisp-type #.waaf:+lisp-long-type+))
      (flet ((call-fits-read-subset-raw (array-ptr array-type null-val-ptr)
	       (multiple-value-setq (dummy anynul status)
		 (fits-read-subset-raw
		  fptr array-type
		  fp-ptr lp-ptr inc-ptr
		  null-val-ptr ;; may be 0 to ignore null val
		  array-ptr
		  0))))

	;; a macro to expand out the call, maybe creating a null-val-ptr
	(macrolet ((cfrsw (lisp-type ffitype cfcode)
		     `(waaf:with-array-as-foreign-pointer
			  (array array-ptr ,ffitype :lisp-type ,lisp-type)
			(if (not null-value-vec)
			    (call-fits-read-subset-raw array-ptr ,cfcode (cffi:null-pointer))
			    (waaf:with-array-as-foreign-pointer
				(null-value-vec null-val-ptr ,ffitype :lisp-type ,lisp-type)
			      (call-fits-read-subset-raw array-ptr ,cfcode null-val-ptr)))))
		   (array-typep (array type)
		     #-no-typed-arrays `(typep ,array ,type)
		     #+no-typed-arrays `(equal (cadr ,type) array-type)))
			    
			  
	;; now handle special case for each array type - yuck
	  (cond 
	    ((array-typep array '(simple-array (unsigned-byte 8)))
	     (print "usb8")
	     (cfrsw (unsigned-byte 8) :uint8 +TBYTE+))
	    ;;
	    ((array-typep array '(simple-array (signed-byte 8)))
	     (print "sb8")
	     (cfrsw (signed-byte 8) :int8 +TBYTE+))
	    ;;
	    ((array-typep array '(simple-array (unsigned-byte 16)))
	     (cfrsw (unsigned-byte 16) :uint16 +TUSHORT+))
	    ;;
	    ((array-typep array '(simple-array (signed-byte 16)))
	     (cfrsw (signed-byte 16) :int16 +TSHORT+))
	    ;;
	    ;; watch out! long can be 64 bits, so we must us TUINT for 32
	    ;; this should be valid for both 32 and 64 bit waaf:machines
	    ((array-typep array '(simple-array (unsigned-byte 32)))
	     (cfrsw (unsigned-byte 32) :uint32 +TUINT+))
	    ;;
	    ((array-typep array '(simple-array (signed-byte 32)))
	     (cfrsw (signed-byte 32) :int32 +TINT+))
	    ;;
	    ((array-typep array '(simple-array (signed-byte 64)))
	     (cfrsw (signed-byte 64) :int64 +TLONGLONG+))
	    ;;
	    ((array-typep array '(simple-array single-float))
	     (cfrsw single-float :float +TFLOAT+))
	    ;;
	    ((array-typep array '(simple-array double-float))
	     (cfrsw double-float :double +TDOUBLE+))
	    ;;
	    (t 
	     (setf bad-array-type t))))))

    (when bad-array-type 
      (error "Bad array type ~A" (type-of array)))

    ;;
    (setf err-string
	  (handle-error-in-status
	   status "read-image-cube-for-fits-pointer" throw-error
	   (format nil "fp=~A lp=~A" fp lp)))
    ;;
    (values array status err-string)))


(defunL write-image-cube-for-fits-pointer
  (fptr fp lp array &key (throw-error t))
  "write an image cube from the first pixel to the last pixel, of
image fptr. FP and LP are vectors with the pixel coordinates
of the first and last pixels.  Array is a specialized array of some type.
 - returns (values array status error-string)
 - WARNING - if array is of type (unsigned-byte 8), but data are signed, the
   values will be bogus"
  (declare (type waaf:machine-pointer fptr)
	   (type vector fp lp)
	   (type simple-array array))
  (when (not (= (length fp) (length lp)))
    (error "FP and LP of different lengths"))
  (when (not (every (lambda (x) (and (fixnum-p x) (plusp x))) fp))
    (error "FP=~A has bogus elements. Must be positive integers" fp))
  (when (not (every (lambda (x) (and (fixnum-p x) (plusp x))) lp))
    (error "LP=~A has bogus elements. Must be positive integers" lp))
  
  (let* ((npix 0) ;; number of pix needed
	 (npix-avail (array-total-size array)) ;; number in array
	 (fp (coerce fp  '(simple-array waaf:machine-long (*))))
	 (lp (coerce lp  '(simple-array waaf:machine-long (*))))
	 (bad-array-type nil)
	 dummy status err-string
	 )
    ;;
    (declare (ignorable dummy))
    ;; count if we have enough bytes
    (loop
     with ntot of-type unsigned-byte = 1
     for i = 1 then (1+ i)
     for n1 across fp
     for n2 across lp
     do
     (if (> n1 n2) (error "fp[~d]=~d  >= lp[~d]=~d" i n1 i n2))
     (setf ntot (* ntot (1+ (- n2 n1))))
     finally (setf npix ntot))
    ;; 
    (if (< npix-avail npix)
	(error "array must have ~d pixels, but it has only ~d"
	       npix npix-avail))
    ;;
    (waaf:with-arrays-as-foreign-pointers 
	((fp fp-ptr :long     :lisp-type #.waaf:+lisp-long-type+) 
	 (lp lp-ptr :long     :lisp-type #.waaf:+lisp-long-type+))

      (flet ((call-fits-write-subset-raw (array-ptr array-type)
	       (multiple-value-setq (dummy status)
		 (fits-write-subset-raw
		  fptr array-type
		  fp-ptr lp-ptr array-ptr
		  0))))
		;; now handle special case for each array type - yuck
	(cond 
	  ;; handle generic arrays first - just make it double float in FFI space
	  
	  (#-no-typed-arrays (eq (array-element-type array) t)
	   #+no-typed-arrays t ;; always do this for lisps without typed arrays
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :double :lisp-type T)
	     (call-fits-write-subset-raw array-ptr +TDOUBLE+)))
	  ;;
	  ;; bit arrays get converted to a byte array before being written
	  ;; because fits doesn't support bits
	  ((typep array '(simple-array (unsigned-byte 1)))
	   (let ((byte-array (make-array (array-dimensions array)
					 :element-type '(unsigned-byte 8))))
	     (loop for i below (array-total-size array)
		   do (setf (row-major-aref byte-array i)
			    (row-major-aref array i)))
	     (waaf:with-array-as-foreign-pointer 
		 (byte-array array-ptr :uint8 :lisp-type (unsigned-byte 8))
	       (call-fits-write-subset-raw array-ptr +TBYTE+))))
	  ;;
	  ((typep array '(simple-array (unsigned-byte 8)))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :uint8 :lisp-type (unsigned-byte 8))
	     (call-fits-write-subset-raw array-ptr +TBYTE+)))
	  ;;
	  ((typep array '(simple-array (signed-byte 8)))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :int8 :lisp-type (signed-byte 8))
	     (call-fits-write-subset-raw array-ptr +TBYTE+)))
	  ;;
	  ((typep array '(simple-array (unsigned-byte 16)))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :uint16 :lisp-type (unsigned-byte 16))
	     (call-fits-write-subset-raw array-ptr +TUSHORT+)))
	  ;;
	  ((typep array '(simple-array (signed-byte 16)))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :int16 :lisp-type (signed-byte 16))
	     (call-fits-write-subset-raw array-ptr +TSHORT+)))
	  ;;
	  ;; watch out! long can be 64 bits, so we must us TUINT for 32
	  ;; this should be valid for both 32 and 64 bit waaf:machines
	  ((typep array '(simple-array (unsigned-byte 32)))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :uint32 :lisp-type (unsigned-byte 32))
	     (call-fits-write-subset-raw array-ptr +TUINT+)))
	  ;;
	  ((typep array '(simple-array (signed-byte 32)))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :int32 :lisp-type (signed-byte 32))
	     (call-fits-write-subset-raw array-ptr +TINT+)))
	  ;;
	  ((typep array '(simple-array (signed-byte 64)))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :int64 :lisp-type (signed-byte 64))
	     (call-fits-write-subset-raw array-ptr +TLONGLONG+)))
	  ;;
	  ((typep array '(simple-array single-float))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :float :lisp-type single-float)
	     (call-fits-write-subset-raw array-ptr +TFLOAT+)))
	  ;;
	  ((typep array '(simple-array double-float))
	   (waaf:with-array-as-foreign-pointer 
	       (array array-ptr :double :lisp-type double-float)
	     (call-fits-write-subset-raw array-ptr +TDOUBLE+)))
	  ;;
	  (t 
	   (setf bad-array-type t)))))

      (when bad-array-type 
	(error "Bad array type ~A" (type-of array)))
    
    ;;
    (setf err-string
	  (handle-error-in-status
	   status "write-image-cube-for-fits-pointer" throw-error
	   (format nil "fp=~A lp=~A" fp lp)))
    ;;
    (values array status err-string)))

	 


(defunL create-fits-file-pointer (filename template-filename
					  &key
					  (throw-error nil)
					  (overwrite t))
  "create a new file and filename.  If TEMPLATE-FILENAME is given
then use it as the tempate to set the initial headers.
If OVERWRITE is T, then append a '!' to the filename to force
overwriting of file
"
  (declare (type string filename)
	   (type (or null string) template-filename))
  (let ((filename (concatenate 'string
			       (if overwrite "!" "")
			       filename
			       (if template-filename
				   (concatenate
				    'string
				    "(" template-filename ")")
				 "")))
	dummy fptr status err-string)
    (declare (ignorable dummy))
    (multiple-value-setq (dummy fptr status)
      (fits-create-file-raw filename 0))
    (setf err-string
	  (handle-error-in-status
	   status "create-fits-file-pointer" throw-error 
	   (format nil "filename=~A template=~A" filename template-filename)))
    (values fptr status err-string)))



(defunL create-fits-image-for-fits-file-pointer (fptr type naxes
						      &key (throw-error t))
  "create a new image for a FPTR; returns (values success status err-string)"
  (declare (type waaf:machine-pointer fptr)
	   (type (member :byte :short :ushort :long :float :double) type)
	   (type vector naxes))
  (when (not (every (lambda (n)
		      (and (typep n 'unsigned-byte) (<= n  +max-image-dim+)))
		    naxes))
    (error "every element of naxes is not < ~D" +max-image-dim+))
  (when (> (length naxes) +max-image-dims+)
    (error "too many dimensions (~D) in image - only ~D allowed"
	   (length naxes) +max-image-dims+))
  (let ((bitpix (type-to-bitpix type))
	(naxis (length naxes))
	;; NAXES is LONG, which is 32 bit or 64bit and ILP-64bit
	(naxes (coerce naxes '(simple-array waaf:machine-long (*))))
	dummy status err-string)
    (declare (ignorable dummy))
    ;;
    ;; DO NOT CREATE HEADERS - ffcrim creates our image headers -
    ;; adding headers manually seriously screws things up, creating 2
    ;; extensions for some twisted reason
    ;(write-header-value-for-pointer fptr "SIMPLE" t nil)
    ;(write-header-value-for-pointer fptr "BITPIX" bitpix nil)
    ;(write-header-value-for-pointer fptr "NAXIS" 2 nil)
    ;(write-header-value-for-pointer fptr "NAXIS1" (aref naxes 0) nil)
    ;(write-header-value-for-pointer fptr "NAXIS2" (aref naxes 1) nil)
    ;;
    (waaf:with-array-as-foreign-pointer 
	(naxes naxes-ptr :long :lisp-type #.waaf:+lisp-long-type+)
      ;;
      (multiple-value-setq (dummy status)
	(fits-create-image-raw fptr bitpix
			       naxis naxes-ptr 0)))
    (setf err-string
	  (handle-error-in-status
	   status "create-fits-image-for-fits-file-pointer" throw-error 
	   (format nil "type=~A naxes=~A" type naxes)))
    (values (zerop status) status err-string)))
	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defunL get-table-ncols-for-fits-pointer (fptr &key (throw-error t))
  "return the number of table columns"
    (declare (type waaf:machine-pointer fptr))          
    (multiple-value-bind (dummy ncols status)
	(fits-get-num-cols-raw fptr 0)
      (declare (ignorable dummy))
      (let ((error-string (handle-error-in-status
			   status "get-table-ncols-for-fits-pointer"
			   throw-error)))
	(values
	 (and (zerop status) ncols)
	 status
	 error-string))))

(defunL get-table-nrows-for-fits-pointer (fptr &key (throw-error t))
  "return the number of table rows"
    (declare (type waaf:machine-pointer fptr))          
    (multiple-value-bind (dummy nrows status)
	(fits-get-num-rows-raw fptr 0)
      (declare (ignorable dummy))
      (let ((error-string (handle-error-in-status
			   status "get-table-nrows-for-fits-pointer"
			   throw-error)))
	(values
	 (and (zerop status) nrows)
	 status
	 error-string))))

(defunL get-table-colnum-for-fits-pointer (fptr template-string
					  &key (throw-error t)
					  (case-sensitive nil)
					  (status 0))
  "return the column number matching the template string for a fits table - STATUS
keyword may be needed to cycle through column names matching the same template-string,
with throw-error disabled, and catching status flags equal to +COL_NOT_UNIQUE+"
  (declare (type waaf:machine-int status)
	   (type waaf:machine-pointer fptr)
	   (type string template-string))
  (multiple-value-bind (dummy ncol status)
      (fits-get-colnum-raw fptr
			   (if case-sensitive +CASESEN+ +CASEINSEN+)
			   template-string
			   status)
     (declare (ignorable dummy))
     (let ((error-string (handle-error-in-status
			  status "get-table-colnum-for-fits-pointer"
			  throw-error)))
       	(values
	 (and (or (zerop status) (= status +COL_NOT_UNIQUE+)) ncol)
	 status
	 error-string))))


(defunL get-table-colname-for-fits-pointer (fptr colnum &key
					   (template-string nil)
					   (throw-error t) (case-sensitive nil)
					   (status 0))
  "return the table column name for a column number - if template-string is
not specified, then the colnum is used to form it, and the column corresponding
to colnum is returned."
  (declare (type waaf:machine-int colnum status)
	   (type waaf:machine-pointer fptr)
	   (type (or null string) template-string))
  ;; our educated guess from the C headers is that a table column name is
  ;; shorter than 70 chars, so let's give it 80
  (let ((header-out (make-simple-string 80 :initial-element #\space))
	(template-string (or template-string (format nil "~D" colnum)))
	(dummy nil)
	(status status))
    (declare (ignorable dummy))
    (setf (aref header-out 79) #\null)
    (waaf:with-array-as-foreign-pointer (header-out header-out-ptr :string)
     (multiple-value-setq (dummy colnum status)
       (fits-get-colname-raw  fptr
			      (if case-sensitive +CASESEN+ +CASEINSEN+)
			      template-string
			      header-out-ptr
			      colnum
			      status)))
    (let ((error-string (handle-error-in-status
			 status "get-table-colname-for-fits-pointer"
			 throw-error)))
      (values
       (clip-string-at-null header-out)
       (and (or (zerop status) (= status +COL_NOT_UNIQUE+)) status)
       error-string))))

      
;; conversion of internal cfitsio table type codes to our type symbols
(defparameter *table-typecode-to-typesym-relation*
  '((#.+TSTRING+     . :string)
    (#.+TSHORT+      . :short)
    (#.+TUSHORT+     . :ushort)
    (#.+TLONG+       . :long)
    (#.+TLONGLONG+   . :long-long)
    (#.+TFLOAT+      . :float)
    (#.+TDOUBLE+     . :double-float)
    (#.+TLOGICAL+    . :logical)
    (#.+TBIT+        . :bit)
    (#.+TBYTE+       . :byte)
    (#.+TCOMPLEX+    . :complex)
    (#.+TDBLCOMPLEX+ . :double-complex)))

(defun table-typecode-to-typesym (typecode)
  (loop for pair in  *table-typecode-to-typesym-relation*
       when (eql (car pair) typecode)
       do (return (cdr pair))
       finally (return nil)))

(defun table-typesym-to-typecode (typesym)
  (loop for pair in  *table-typecode-to-typesym-relation*
       when (eql (cdr pair) typesym)
       do (return (car pair))
       finally (return nil)))

   


(defunL get-table-coltype-for-fits-pointer (fptr colnum &key (throw-error t))
  "return the column type of table column colnum - full return values are
  (values typecode-symbol variable-length-col? repeat width status
         error-string)"
  (declare (type waaf:machine-int colnum)
	   (type waaf:machine-pointer fptr))
  (multiple-value-bind (dummy typecode repeat width status)
      (fits-get-coltype-raw fptr colnum 1 1 1 0)
    (declare (ignorable dummy))
      (let ((error-string (handle-error-in-status
			   status "get-table-nrows-for-fits-pointer"
			   throw-error)))
	(let* ((variable-length-col (minusp typecode))
	       (typecode (abs typecode))
	       (typesym (table-typecode-to-typesym typecode)))
	  (values
	   (and (zerop status) typesym)
	   variable-length-col
	   repeat
	   width	      
	   status
	   error-string)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table reading - this is a pain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to pass an array of strings (char pointers) we need to allocate a
;; copy of the strings in foreign space, then at the end copy over to
;; lisp space

(defunL configure-vector-of-char-pointers (v foreign-string-ptr string-arr)
"helper function for WITH-STRING-VECTOR-AS-FOREIGN-STRING; sets up pointer vector
V and copies strings, separated by zero byte, into FOREIGN-STRING-PTR"
  (declare (type (simple-array waaf:machine-pointer (*)) v)
	   (type array string-arr)
	   (type cffi:foreign-pointer foreign-string-ptr))
  (loop
     with n = (length v)
     with j of-type (unsigned-byte 28) = 0 ;; index in foreign string
     with iptr of-type waaf:machine-pointer 
       = (%foreign-pointer-to-int foreign-string-ptr)
     for i of-type (unsigned-byte 28) below n
     for str of-type string = (row-major-aref string-arr  i)
     do 
       (setf (aref v i) iptr)
       (loop 
	  for c across str
	  do 
	    (%foreign-string-set foreign-string-ptr j c)
	    (incf j)
	  finally 
	    (%foreign-string-set foreign-string-ptr j #.(code-char 0))
	    (incf j))
       ;;
       (incf iptr (1+ (length str))) ;; 1+ to account for null  termination
     finally (return v)))
 
(defunL copy-vector-of-char-pointers-back (foreign-string-ptr string-arr)
  "helper function for WITH-STRING-VECTOR-AS-FOREIGN-STRING; copies strings in
FOREIGN-STRING-PTR back into STRING-ARR" 
  (declare (type array string-arr)
	   (type  cffi:foreign-pointer foreign-string-ptr))
  (loop
     with j of-type (unsigned-byte 28) = 0 ;; index in foreign string
     with iptr of-type waaf:machine-pointer = (%foreign-pointer-to-int foreign-string-ptr)
     for i from 0 below (array-total-size string-arr)
     for str = (row-major-aref string-arr i)
     do 
       (loop 
	  for istr below (length str)
	  do (setf (aref str istr)
		   (%foreign-string-ref foreign-string-ptr j))
	    (incf j))
       (incf j) ;; the zero termination in the foreign string
       (incf iptr (1+ (length str)))
     finally (return string-arr)))
  


(defmacro with-string-array-as-foreign-string
    ((pointer-vec-var string-vec)
     &body body)
  "Given a vector of strings STRING-VEC, allocate a block of
foreign memory, and fill it with the strings (null terminated).
Make POINTER-VEC-VAR be a vector of pointer-integers to the
coresponding strings inside the foreign string. At termination,
the values in the foreign string are copied back over to the
string vector, and the foreign string is deallocated."
  (let ((ntot-var (gensym "ntot-"))
	(string-var (gensym "string-"))
	(i-var (gensym "i-"))
	(foreign-string-var (gensym "foreign-string-")))
    `(let ((,ntot-var (loop
			for ,i-var below (array-total-size ,string-vec)
			for ,string-var = (row-major-aref  ,string-vec ,i-var)
			sum (1+ (length ,string-var))))
	   (,pointer-vec-var (make-array 
			      (array-total-size ,string-vec) 
			      :element-type 'waaf:machine-pointer :initial-element 0)))
       (%with-foreign-string (,foreign-string-var ,ntot-var)
	 (configure-vector-of-char-pointers 
	  ,pointer-vec-var ,foreign-string-var ,string-vec)
	 ,@body
	 (copy-vector-of-char-pointers-back ,foreign-string-var ,string-vec)))))
  
(defmacro with-string-array-as-foreign-string-nil
    ((pointer-vec-var string-vec)
     &body body)
  "Like WITH-STRING-ARRAY-AS-FOREIGN-STRING but if STRING-VEC is
NIL, then make POINTER-VEC-VAR a zero-length array of pointers,
which waaf:with-arrays-as-foreign-pointers will convert to a NIL
pointer "
  `(if (not ,string-vec)
     (let ((,pointer-vec-var (make-array 0 :element-type  'waaf:machine-pointer :initial-element 0)))
       ,@body)
     ;; else normal form
     (with-string-array-as-foreign-string (,pointer-vec-var ,string-vec) ,@body)))


 
(defunL create-table-extension-for-fits-pointer 
    (fptr tbltype naxis2 ;; nrows=naxis2
     ttype tform tunit
     extname &key (throw-error t))
  (declare  (type waaf:machine-pointer fptr)
	    (type integer tbltype)
	    (type unsigned-byte naxis2)
	    (type vector ttype tform)
	    (type (or null vector) tunit)
	    (type (or null string) extname))
  (with-string-array-as-foreign-string (ttype-vec ttype)
    (with-string-array-as-foreign-string (tform-vec tform)
      ;; here, if TUNIT is NIL, then an empty pointer-vec is returned, which
      ;; should be turned into a null  pointer by waaf:with-arrays-as-foreign-pointers 
      (with-string-array-as-foreign-string-nil (tunit-vec tunit)
	(waaf:with-arrays-as-foreign-pointers 
	    ((ttype-vec ttype-vec-ptr  
			:pointer-int :lisp-type waaf:machine-pointer)
	     (tform-vec tform-vec-ptr
			:pointer-int :lisp-type waaf:machine-pointer)
	     (tunit-vec tunit-vec-ptr
			:pointer-int :lisp-type waaf:machine-pointer))


	  (multiple-value-bind (dummy status)
	      (fits-create-tbl-raw fptr tbltype  0; naxis2
				   (length ttype)
				   ttype-vec-ptr tform-vec-ptr tunit-vec-ptr
				   ;; for some reason opemcl chokes on
				   ;; a null pointer here, giving
				   ;; segfault, even though it is
				   ;; allowed by cfitsio
				   (or extname  (cffi:null-pointer))
				   0)
	    (declare (ignorable dummy))
	    
	    (let ((error-string 
		    (handle-error-in-status
		     status "create-table-extension-for-fits-pointer"
		     throw-error)))
	      (values status error-string)))))))) 

 


;; read fits column - special case of reading colum for the string case
(defunL read-table-column-for-fits-pointer--string (fptr colnum 
						   firstrow nelements
						   string-size
						   &key
						   (null-value "")
						   ;; which element, if a vector
						   (firstelem 1) 
						   (return-null-array nil)
						   (throw-error t)
						   (string-trim-nulls t)
						   )
  (declare (type waaf:machine-pointer fptr)
	   ;; our sizes are smaller than those allowed by cfitsio
	   (type (unsigned-byte 28) firstrow firstelem nelements)
	   (type (or null simple-string) null-value))
  ;;
  (if (> (length null-value) string-size)
      (error "The null value string <~A> is longer than the returned string size ~d"
	     null-value string-size))
  ;;
  (if (and (not null-value) (not return-null-array))
      (error "a null-value was not supplied when return-null-array is NIL"))
  ;;
  ;; test the string size against what cfitsio tells us
  (multiple-value-bind (dum width status)
      (fits-get-col-display-width-raw fptr colnum 0)
    (declare (ignorable dum status))
    ;; if string-size is null, get it from cfitsio
    (setf string-size (or string-size width))
    ;; The following is removed because width contains the total width of
    ;; a string column, but it could be several strings wide (repetitions).
;;    (when (not (>= string-size width))
;;      (error "fits-get-col-display-width says that this string is
    ;;~A long, but you gave a string length of ~A." width string-size))
    )
  ;;
  (let ((string-vec (make-array nelements))
	(vnull-contents (when (not return-null-array)
			  (vector null-value)))
	(nullarray (when return-null-array (make-simple-string nelements)))
	dummy
	anynul
	status
	error-string
	)
    (declare (ignorable dummy))
    (loop for i below nelements
	 ;; make the string one longer for null termination by cfitsio
	 do (setf (aref string-vec i) (make-simple-string (1+ string-size))))

    (cond
      ;;
      (return-null-array
       (with-string-array-as-foreign-string (v string-vec)
	 (waaf:with-arrays-as-foreign-pointers 
	     ((v v-ptr  :pointer-int     :lisp-type waaf:machine-pointer)
	      (nullarray nullarray-ptr :string))
	   (multiple-value-setq (dummy anynul status)
	     (fits-read-colnull-raw  fptr +TSTRING+ colnum
				     firstrow firstelem nelements
				     v-ptr nullarray-ptr
				     0)))))
      ;;
      (t
       (with-string-array-as-foreign-string (v string-vec)
	 (with-string-array-as-foreign-string (vnull vnull-contents)
	   (waaf:with-arrays-as-foreign-pointers 
	       ((v v-ptr  :pointer-int  :lisp-type waaf:machine-pointer)
		(vnull vnull-ptr  :pointer-int :lisp-type waaf:machine-pointer))
          (multiple-value-setq (dummy anynul status)
	    (fits-read-col-raw fptr +TSTRING+ colnum
			       firstrow firstelem nelements
			       vnull-ptr v-ptr
			       0)))))))

    ;; we seem to get status=1 for undefined table elements
    ;; but this isn't really an error, so we get rid of this 
    ;; error code
    ;;(when (= status 1) (setf status 0))
    ;;
    (setf error-string (handle-error-in-status
			status "get-table-nrows-for-fits-pointer--string"
			throw-error))
    ;; string-trim any nulls
    (when string-trim-nulls
      (setf string-vec
	    (map 'vector (lambda (str) (string-trim '(#\null) str)) string-vec)))
    (cond
      (return-null-array
       (values
	string-vec
	;; nullarray has T/NIL if the array is/is-not defined
	(map 'vector (lambda (c) (not (char= c #.(code-char 1)))) nullarray)
	anynul
	status
	error-string))
      (t
       (values
	string-vec
	anynul
	status
	error-string)))))

(defun convert-type-sym-to-lisp-type (type-sym)
  (cond ((eq type-sym :byte) '(unsigned-byte 8))
	((eq type-sym :short) '(signed-byte 16))
	((eq type-sym :ushort) '(unsigned-byte 16))
	((eq type-sym :long) '(signed-byte 32))
	((eq type-sym :float) 'single-float)
	((eq type-sym :double) 'double-float)))
	
    

;; helper function that runs func under correct pointer wrapping
(defun %run-func-fitscol-1 (array array-type func-of-aptr)
  (cond 
    ((equal array-type '(signed-byte 8))
     (waaf:with-arrays-as-foreign-pointers 
	     ((array aptr :int8  :lisp-type (signed-byte 8)))
	   (funcall func-of-aptr aptr)))
    ((equal array-type '(unsigned-byte 8))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :uint8  :lisp-type (unsigned-byte 8)))
       (funcall func-of-aptr aptr)))
    ;;
    ((equal array-type '(signed-byte 16))
     (waaf:with-arrays-as-foreign-pointers 
	     ((array aptr :int16  :lisp-type (signed-byte 16)))
	   (funcall func-of-aptr aptr)))
    ((equal array-type '(unsigned-byte 16))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :uint16  :lisp-type (unsigned-byte 16)))
       (funcall func-of-aptr aptr)))
    ;
    ((equal array-type '(signed-byte 32))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :int32  :lisp-type (signed-byte 32)))
       (funcall func-of-aptr aptr)))
    ((equal array-type '(unsigned-byte 32))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :uint32  :lisp-type (unsigned-byte 32)))
       (funcall func-of-aptr aptr)))
    ;;
    ;; no unsigned 64
    ((equal array-type '(signed-byte 64))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :int64  :lisp-type (signed-byte 64)))
       (funcall func-of-aptr aptr)))
    ;;
    ((equal array-type 'single-float)
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :float  :lisp-type single-float))
       (funcall func-of-aptr aptr)))
    ;;
    ((equal array-type 'double-float)
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :double  :lisp-type double-float))
       (funcall func-of-aptr aptr)))
    ;;
    ((equal array-type '(complex single-float))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :float  :lisp-type (complex single-float) :complex t))
       (funcall func-of-aptr aptr)))
    ;;
    ((equal array-type '(complex double-float))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array aptr :double  :lisp-type (complex double-float) :complex t))
       (funcall func-of-aptr aptr)))
    ;;
    (t
     (error "type ~A fell through - should not happen" array-type))))
	
;; similar function but for 2 arrays of the same array-type
(defun %run-func-fitscol-2 (array1 array2 array-type func-of-aptr1-aptr2)
  (cond 
    ((equal array-type '(signed-byte 8))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :int8  :lisp-type (signed-byte 8))
	  (array2 aptr2 :int8  :lisp-type (signed-byte 8)))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ((equal array-type '(unsigned-byte 8))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :uint8  :lisp-type (unsigned-byte 8))
	  (array2 aptr2 :uint8  :lisp-type (unsigned-byte 8)))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;;
    ((equal array-type '(signed-byte 16))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :int16  :lisp-type (signed-byte 16))
	  (array2 aptr2 :int16  :lisp-type (signed-byte 16)))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ((equal array-type '(unsigned-byte 16))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :uint16  :lisp-type (unsigned-byte 16))
	  (array2 aptr2 :uint16  :lisp-type (unsigned-byte 16)))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;
    ((equal array-type '(signed-byte 32))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :int32  :lisp-type (signed-byte 32))
	  (array2 aptr2 :int32  :lisp-type (signed-byte 32)))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ((equal array-type '(unsigned-byte 32))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :uint32  :lisp-type (unsigned-byte 32))
	  (array2 aptr2 :uint32  :lisp-type (unsigned-byte 32)))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;;
    ;; no unsigned 64
    ((equal array-type '(signed-byte 64))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :int64  :lisp-type (signed-byte 64))
	  (array2 aptr2 :int64  :lisp-type (signed-byte 64)))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;;
    ((equal array-type 'single-float)
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :float  :lisp-type single-float)
	  (array2 aptr2 :float  :lisp-type single-float))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;;
    ((equal array-type 'double-float)
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :double  :lisp-type double-float)
	  (array2 aptr2 :double  :lisp-type double-float))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;;
    ((equal array-type '(complex single-float))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :float  :lisp-type (complex single-float) :complex t)
	  (array2 aptr2 :float  :lisp-type (complex single-float) :complex t))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;;
    ((equal array-type '(complex double-float))
     (waaf:with-arrays-as-foreign-pointers 
	 ((array1 aptr1 :double  :lisp-type (complex double-float) :complex t)
	  (array2 aptr2 :double  :lisp-type (complex double-float) :complex t))
       (funcall func-of-aptr1-aptr2 aptr1 aptr2)))
    ;;
    (t
     (error "type ~A fell through - should not happen" array-type))))
		


;; given a keyword typecode, return the apprpriate vector,
;; the cfitsio +TYPECODE+ and a null vector as well

(defun make-fitscol-array (type-sym n nullval have-nullval)
  (multiple-value-bind (tcode type initial-element)
      (cond ((eq type-sym :short)
	     (values +TSHORT+ '(signed-byte 16) 0))
	    ((eq type-sym :ushort)
	     (values +TSHORT+ '(unsigned-byte 16) 0))
	    ((eq type-sym :long)
	     (values +TLONG+ waaf:+lisp-long-type+ 0))
	    ((eq type-sym :ulong)
	     (values +TULONG+ waaf:+lisp-ulong-type+ 0))
	    ((eq type-sym :long-long)
	     (values +TLONGLONG+ '(signed-byte 64) 0))
	    ((eq type-sym :float)
	     (values +TFLOAT+ 'single-float 0e0))
	    ((eq type-sym :double-float)
	     (values +TDOUBLE+ 'double-float 0d0))
	    ((eq type-sym :byte)
	     (values +TBYTE+ '(unsigned-byte 8) 0))
	    ((eq type-sym :complex)
	     (values +TCOMPLEX+ '(complex single-float) #(0.0 0.0)))
	    ((eq type-sym :double-complex)
	     (values +TDBLCOMPLEX+ '(complex double-float) #C(0d0 0d0)))
	    ((eq type-sym :logical)
	     (values +TLOGICAL+ '(unsigned-byte 8) 0))
	    ((eq type-sym :bit)
	     (values +TBYTE+ '(unsigned-byte 8) 0)))
    (values
     (make-array n :element-type type :initial-element initial-element)
     tcode
     (if have-nullval (make-array 1 :element-type type :initial-element nullval))
     type)))

    
;; same as make-fitscol-array but fill with data from data-vec - note that data-vec

(defun make-fitscol-array-and-fill (type-sym n nullval have-nullval data-arr)
  (multiple-value-bind (v type-code vnull type)
      (make-fitscol-array type-sym n nullval have-nullval)
    (loop 
       with array-type = (array-element-type v)
       for i below (array-total-size data-arr)
       for x = (row-major-aref data-arr i)
       for xx = (if (eq type-sym :logical) 
		    (if x 1 0)
		    (coerce x array-type))
       do (setf (aref v i) xx))
    (values v type-code vnull type)))


; read fits column - special case of reading colum for the string case
(defunL read-table-column-for-fits-pointer (fptr colnum 
					   firstrow nelements
					   type-sym
					   &key
					   (null-value nil)
					   ;; which element, if a vector
					   (firstelem 1) 
					   (return-null-array nil)
					   (throw-error t)
					   (string-trim-nulls t)
					   (string-size nil)
					   )
  (declare (type waaf:machine-pointer fptr)
	   ;; our sizes are smaller than those allowed by cfitsio
	   (type (unsigned-byte 28) firstrow firstelem nelements))
  ;;
  ;; handle the string case specially
  (when (eq type-sym :string)
    (return-from  read-table-column-for-fits-pointer
      (read-table-column-for-fits-pointer--string
       fptr colnum firstrow
       nelements string-size
       :null-value null-value
       :firstelem firstelem
       :return-null-array return-null-array
       :throw-error throw-error
       :string-trim-nulls string-trim-nulls)))
  ;;
  (let ((nullarray (when return-null-array (make-simple-string nelements)))
	v
	vnull 
	anynul
	dummy
	status
	error-string
	typecode array-type)
    ;;
    (declare (ignorable dummy))
    ;; set up the return arrays
    (multiple-value-setq (v typecode vnull array-type)
      (make-fitscol-array type-sym nelements null-value
			  ;; the following says if we are using a NULL value
			  (not return-null-array))) 
     (cond
       (return-null-array ;; in this case nullarray is a simple string
	(waaf:with-arrays-as-foreign-pointers
	    ((nullarray nullarray-ptr :string))
	  ;; we run the lambda form in a function that chooses the correct array copiers
	  (%run-func-fitscol-1 v array-type
			       (lambda (v-ptr)
				 (multiple-value-setq (dummy anynul status) 
				   (fits-read-colnull-raw  fptr typecode colnum
							   firstrow firstelem nelements
							   v-ptr nullarray-ptr
							   0))))))
       (t
	;; we run the lambda form in a function that chooses the correct array copiers
	(%run-func-fitscol-2 v vnull array-type 
			     (lambda (v-ptr vnull-ptr)
			       (multiple-value-setq (dummy anynul status)
				 (fits-read-col-raw fptr typecode colnum
						    firstrow firstelem nelements
						    vnull-ptr v-ptr
						    0)))))) 
       ;;
     ;;
     (when (= status 1) (setf status 0)) ;; status=1 is a non error
     (setf error-string (handle-error-in-status
			status "read-table-column-for-fits-pointer"
			throw-error))


    ;; in the case of a LOGICAL type, we need to convert
    (when (eq type-sym :logical)
      (setf v (map 'vector (lambda (n) (not (= n 0))) v)))    
    ;;
    ;; README - this changed for the case of RETURN-NULL-ARRAY=NULL  - now it returns NULL,
    ;; but before 2014-09-11 it returned one less term in (VALUES ...)
    (values
     v
     ;; nullarray has T/NIL if the array is/is-not defined
     (when return-null-array
       (map 'vector (lambda (c) (not (char= c #.(code-char 1)))) nullarray))
     anynul
     status 
     error-string)))

	
  
  

(defunL write-table-column-for-fits-pointer (fptr data-arr colnum 
					    firstrow num-elements
					    type-sym &key
					    null-value
					    firstelem 
					    (throw-error t))
  ;;
  (cond ((eq type-sym :string) ;; special string case
	 (write-table-column-for-fits-pointer--string       
	  fptr data-arr colnum
	  firstrow num-elements
	  type-sym 
	  :null-value null-value
	  :firstelem firstelem
	  :throw-error throw-error)) 
	;;
	(t ;; non-string case 
	 (let (dummy status v vnull type-code error-string array-type)   
	   ;;
	   (declare (ignorable dummy))
	   (multiple-value-setq (v type-code vnull array-type)
	     (make-fitscol-array-and-fill 
	      type-sym num-elements null-value null-value data-arr))
	   (when (not null-value)
	     (setf vnull (make-array 0 :element-type array-type))) ;; so that vnull-ptr is NULL 
	   ;;
	   ;; run fits-colnull-raw using the %run-func-fitscol-2 helper, which
	   ;; knows about which with-arrays... to use
	   (%run-func-fitscol-2 
	      v vnull array-type
	      (lambda (v-ptr vnull-ptr)
		(multiple-value-setq (dummy status)
		  (fits-write-colnull-raw fptr type-code
					  colnum firstrow firstelem num-elements 
					  v-ptr vnull-ptr 0))))
	   ;;
	   (setf error-string (handle-error-in-status
				 status "get-table-column-for-fits-pointer"
				 throw-error))
	     (values status error-string)))))

  
(defunL write-table-column-for-fits-pointer--string (fptr data-arr colnum
						    firstrow num-elements
						    type-sym &key
						    null-value
						    firstelem 
						    (throw-error t))
  ;;
  (when (not (eq type-sym :string)) (error "TYPE-SYM is not a string"))
  (when (and null-value (not (stringp null-value))) (error "NULL-VALUE is not a string"))
  ;;
  (let ((type-code +TSTRING+)
	(error-string nil)
	(vnull-contents (if null-value (vector null-value))))
    ;;
    (with-string-array-as-foreign-string (v data-arr)
      (with-string-array-as-foreign-string-nil (vnull vnull-contents)
	(waaf:with-arrays-as-foreign-pointers 
	    ((v v-ptr  :pointer-int   :lisp-type waaf:machine-pointer)
	     (vnull vnull-ptr  :pointer-int :lisp-type waaf:machine-pointer))
	  
	  (multiple-value-bind (dummy status)
	      (fits-write-colnull-raw fptr type-code
				      colnum firstrow firstelem num-elements 
				      v-ptr vnull-ptr 0)
	    (declare (ignorable dummy))
	    (setf error-string (handle-error-in-status
				status "get-table-column-for-fits-pointer"
				throw-error))
	    (values status error-string)))))))	
				   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fits-set-bscale (fptr bscale bzero &key (throw-error t))
  (declare (type double-float bscale bzero))
  (let ((error-string nil))
    (multiple-value-bind (dummy status)
	(fits-set-bscale-raw fptr bscale bzero 0)
      (declare (ignorable dummy))
      (setf error-string (handle-error-in-status
			  status "fits-set-bscale"
			  throw-error))
      (values status error-string))))

;; fits-set-tscale, etc not yet written

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compression - others left to implement

(defun fits-is-compressed-image (fptr &key (throw-error t))
   (let ((error-string nil))
    (multiple-value-bind (compressed-bool status)
	(fits-is-compressed-image-raw fptr 0)
      (declare (ignorable dummy))
      (setf error-string (handle-error-in-status
			  status "fits-is-compressed-image"
			  throw-error))
      (values
       compressed-bool ;; 1 or 0
       status error-string))))


;; WARNING - this does not do what you would imagine!  This is for
;; WRITING compressed images.  For examining/reading images, one needs to use
;; headers. See https://heasarc.gsfc.nasa.gov/fitsio/c/c_user/node41.html
(defun fits-get-compression-type (fptr  &key (throw-error t))
  (let ((error-string nil))
    (multiple-value-bind (dummy comptype status)
	(fits-get-compression-type-raw fptr 0 0)
      (declare (ignorable dummy))
      (print (list dummy comptype status))
      (setf error-string (handle-error-in-status
			  status "fits-get-compresion-type"
			  throw-error))
      (values
       comptype
       status error-string))))


