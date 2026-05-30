
#|
 
make various configuration files for terapix programs

 %make-scamp-conf-file

 %make-sextractor-conf-file
 %make-sextractor-param-file
 %make-sextractor-nnw-file
 %make-sextractor-conv-file 

 %make-swarp-conf-file

|#




(in-package terapix)


;; possible ones that work with client:
;;  vizier.u-strasbg.fr
;;  vizier.nao.ac.jp
;;  vizier.cfa.harvard.edu
;;  cocat1.u-strasbg.fr - this one was down Wed Oct 5 2016
;; 
(defparameter *default-vizier-catalog-server*
;  "vizier.u-strasbg.fr"
;  "vizier.cfa.harvard.edu"
  "cocat1.u-strasbg.fr"
  )


(defun %make-scamp-conf-file-plot-section (checkplot-dev  output-dir)
  (cond ((equalp checkplot-dev "NULL")
	 "
# Sometimes there are runtime bugs of random strings for CHECKPLOT_DEV 
#   so try removing CHECKPLOT_* completely
CHECKPLOT_DEV          NULL
CHECKPLOT_TYPE 
CHECKPLOT_NAME

")
	(t
	 (format nil 
		 "# pdf apparently requires ImageMagick, an unwanted complication 
CHECKPLOT_DEV          ~A             # NULL, XWIN, TK, PS, PSC, XFIG, PNG,
                                       # or JPEG
CHECKPLOT_TYPE         FGROUPS,DISTORTION,ASTR_INTERROR2D,ASTR_INTERROR1D,ASTR_REFERROR2D,ASTR_REFERROR1D,ASTR_CHI2,PHOT_ERROR
# plot files should have a dummy suffix because otherise SCAMP
#   will become confused by any dots in full file path of plot file
CHECKPLOT_NAME         ~Afgroups.XX,~Adistort.XX,~Aastr_interror2d.XX,~Aastr_interror1d.XX,~Aastr_referror2d.XX,~Aastr_referror1d.XX,~Aastr_chi2.XX,~Apsphot_error.XX # Check-plot filename(s)"
		  checkplot-dev
		  output-dir output-dir output-dir output-dir output-dir
		  output-dir  output-dir  output-dir))))
 

(defun %make-scamp-conf-file
    (&key (conf-filename "default.conf")
       (output-dir ".") ;; for plots and xml
       (xml-filename "scamp.xml")
       (checkplot-dev "NULL")
       (scamp-save-refcatalog "N")
       (astrometric-catalog-path "./scamp-astrometric-catalogs")
       (astref-catalog "2MASS")
       (astrefcat-name "NONE") ;; if FILE
       (catalog-server *default-vizier-catalog-server*)
       (astref-mag-low 10)
       (astref-mag-high 25) ;; mag limits in astrometric catalog
       (mergedoutcat-name "scamp_merged.cat")
       (mergedoutcat-type "NONE")
       (aheader-suffix "ahead") ;; extra input headers
       (header-suffix "head")   ;; output headers
       (pixscale-maxerr 1.03)
       (posangle-maxerr 5.0)
       (position-maxerr 2.0) ;; arcmin
       (crossid-radius  3.0) ;; arcsec
       (fwhm-threshold-low 2) ;; in pix, for good detection
       (fwhm-threshold-high 20)
       (match-flipped "N") ;; allow flipped axes?
       (verbose-type "NORMAL")
       (distort-degrees 1)
       (flags-mask #x0fc))  ;; sextractor flags; this is the default
  
  ;; fix the output dir because of scamp's peculiar behavior
  (when (member output-dir '("." ".."))
    (setf output-dir ""))
  (when (find "." output-dir)
    (format t "WARNING  - the plots will not be output where you expect them
because SCAMP plot filnames don't like <.>s inside them"))


  (with-open-file (sout conf-filename :direction :output :if-exists :supersede)
    (format sout
	    "
# Default configuration file for SCAMP 1.3.11
# EB 2007-08-08
#
 
#----------------------------- Field grouping ---------------------------------
 
FGROUP_RADIUS          1.0             # Max dist (deg) between field groups
 
#---------------------------- Reference catalogs ------------------------------
 
REF_SERVER             ~A # Internet addresses of catalog servers
ASTREF_CATALOG         ~A              # NONE, FILE, USNO-A1, USNO-A2, USNO-B1,
                                       # GSC-1.3, GSC-2.2, UCAC-1, UCAC-2,
                                       # 2MASS, SDSS-R3, or SDSS-R5
ASTREF_BAND            DEFAULT         # Photom. band for astr.ref.magnitudes
                                       # or DEFAULT, BLUEST, or REDDEST
ASTREFCAT_NAME         ~A     # Local astrometric reference catalogs
ASTREFCENT_KEYS        X_WORLD,Y_WORLD # Local ref.cat.centroid parameters
ASTREFERR_KEYS         ERRA_WORLD, ERRB_WORLD, ERRTHETA_WORLD
                                       # Local ref.cat.error ellipse parameters
ASTREFMAG_KEY          MAG             # Local ref.cat.magnitude parameter
SAVE_REFCATALOG        ~A              # Save ref catalogs in FITS-LDAC format?
REFOUT_CATPATH         ~A              # Save path for reference catalogs
ASTREFMAG_LIMITS       ~F, ~F          # magnitude range for astref catalog
 
#--------------------------- Merged output catalogs ---------------------------
 
MERGEDOUTCAT_NAME      ~A       # Merged output catalog filename
MERGEDOUTCAT_TYPE      ~A       # NONE, ASCII_HEAD, ASCII, FITS_LDAC
 
#----------------------------- Pattern matching -------------------------------
 
MATCH                  Y               # Do pattern-matching (Y/N) ?
MATCH_NMAX             0               # Max.number of detections for MATCHing
                                       # (0=auto)
PIXSCALE_MAXERR        ~F             # Max scale-factor uncertainty
POSANGLE_MAXERR        ~F             # Max position-angle uncertainty (deg)
POSITION_MAXERR        ~F             # Max positional uncertainty (arcmin)
MATCH_RESOL            0               # Matching resolution (arcsec); 0=auto
MATCH_FLIPPED          ~A               # Allow matching with flipped axes?
#MOSAIC_TYPE            UNCHANGED       # UNCHANGED, SAME_CRVAL, SHARE_PROJAXIS,
                                       # FIX_FOCALPLANE or LOOSE
 
MOSAIC_TYPE            LOOSE

#---------------------------- Cross-identification ----------------------------
 
CROSSID_RADIUS         ~F             # Cross-id initial radius (arcsec)
 
#---------------------------- Astrometric solution ----------------------------
 
SOLVE_ASTROM           Y               # Compute astrometric solution (Y/N) ?
ASTRINSTRU_KEY         FILTER,QRUNID   # FITS keyword(s) defining the astrom
#STABILITY_TYPE         INSTRUMENT      # EXPOSURE, GROUP, INSTRUMENT or FILE
STABILITY_TYPE         EXPOSURE
CENTROID_KEYS          XWIN_IMAGE,YWIN_IMAGE # Cat. parameters for centroiding
CENTROIDERR_KEYS       ERRAWIN_IMAGE,ERRBWIN_IMAGE,ERRTHETAWIN_IMAGE
                                       # Cat. params for centroid err ellipse
DISTORT_KEYS           XWIN_IMAGE,YWIN_IMAGE # Cat. parameters or FITS keywords
DISTORT_GROUPS         1,1             # Polynom group for each context key
DISTORT_DEGREES        ~D              # Polynom degree for each group
FLAGS_MASK             ~D              # sextractor flag to accept/reject star
 
#---------------------------- Photometric solution ----------------------------
 
SOLVE_PHOTOM           N               # Compute photometric solution (Y/N) ?
MAGZERO_OUT            0.0             # Magnitude zero-point(s) in output
MAGZERO_INTERR         0.01            # Internal mag.zero-point accuracy
MAGZERO_REFERR         0.03            # Photom.field mag.zero-point accuracy
PHOTINSTRU_KEY         FILTER          # FITS keyword(s) defining the photom.
MAGZERO_KEY            PHOT_C          # FITS keyword for the mag zero-point
EXPOTIME_KEY           EXPTIME         # FITS keyword for the exposure time (s)
AIRMASS_KEY            AIRMASS         # FITS keyword for the airmass
EXTINCT_KEY            PHOT_K          # FITS keyword for the extinction coeff
PHOTOMFLAG_KEY         PHOTFLAG        # FITS keyword for the photometry flag
PHOTFLUX_KEY           FLUX_AUTO       # Catalog param. for the flux measurement
PHOTFLUXERR_KEY        FLUXERR_AUTO    # Catalog parameter for the flux error
 
#------------------------------- Check-plots ----------------------------------
# warning - there seem to be bugs in check-plots that crash scamp, so might
# be best to run without them in general

~A
 
#------------------------------ Miscellaneous ---------------------------------
#                      JTK - found that lower can be better for SN 
SN_THRESHOLDS          8.0,100.0   # S/N thresholds (in sigmas) for all and
                                    # high-SN sample
FWHM_THRESHOLDS        ~A, ~A       # FWHM thresholds (in pixels) for sources
AHEADER_SUFFIX         .~A          # Filename extension for additional
                                    # INPUT headers
HEADER_SUFFIX          .~A          # Filename extension for OUTPUT headers
VERBOSE_TYPE           ~A           # QUIET, NORMAL, LOG or FULL
WRITE_XML              Y            # Write XML file (Y/N)?
XML_NAME               ~A/~A        # Filename for XML output
NTHREADS               1            # 1 single thread
"
	    catalog-server
	    astref-catalog
	    astrefcat-name
	    scamp-save-refcatalog
	    astrometric-catalog-path
	    astref-mag-low astref-mag-high
	    mergedoutcat-name  mergedoutcat-type
	    pixscale-maxerr
	    posangle-maxerr
	    position-maxerr
	    match-flipped
	    crossid-radius
	    distort-degrees
	    flags-mask
	    ;; output dirs for plots and xml output-dir output-dir
	    (%make-scamp-conf-file-plot-section checkplot-dev output-dir)
	    fwhm-threshold-low 	    fwhm-threshold-high
	    aheader-suffix header-suffix 
	    verbose-type
	    output-dir
	    xml-filename
	    )))




(defun %make-sextractor-conf-file
    (&key 
       (detect-threshold 2.0)
       (analysis-threshold 2.0)
       (gain 1.0)
       (pixel-scale 1.0)
       (checkimage-types-and-names '()) 
       (satur-level 20000.0)
       (mag-zeropoint 0.0)
       (deblend-mincont 0.005)
       (deblend-nthresh 32)
       (conf-filename "default.sex")
       (phot-apertures '(10.0))	  ;; diameter of apertures
       (phot-autoparams '(2.5 3.5)) ;; MAG_AUTO controls, scaling param
       ;; k of first momement, and minimum RMIN
       (param-filename "default.param")
       (catalog-filename "default.cat")
       (conv-filename  "default.conv")
       (back-size 256) ;; a bit larger for comets
       (back-filtersize 3)
       (nnw-filename   "default.nnw")
       (verbose-type "NORMAL")
       (weight-image nil)
       (weight-type  "NONE")
       (flag-image nil)
       (flag-type "OR")
       (nthreads 0))

  (with-open-file (sout conf-filename :direction :output :if-exists :supersede)
    (format sout
	    "# Default configuration file for SExtractor 2.3b2
CATALOG_NAME    ~A 
CATALOG_TYPE    FITS_LDAC
PARAMETERS_NAME ~A
 
 
DETECT_TYPE     CCD      

DETECT_MINAREA  3     
DETECT_THRESH   ~A   
ANALYSIS_THRESH ~A  
THRESH_TYPE     RELATIVE 
 
FILTER          Y     
FILTER_NAME     ~A    
 
DEBLEND_NTHRESH ~A        
DEBLEND_MINCONT ~A
 
CLEAN           Y         
CLEAN_PARAM     1.0       
 
MASK_TYPE       CORRECT      
 
 
PHOT_APERTURES  ~A      
PHOT_AUTOPARAMS ~A
 
SATUR_LEVEL     ~A
 
MAG_ZEROPOINT   ~A    
MAG_GAMMA       4.0    
GAIN            ~A    
PIXEL_SCALE     ~,4F    
 
 
SEEING_FWHM     1.2    
STARNNW_NAME    ~A     
 
# change to a wider background mesh size to accommodate comets 
BACK_SIZE       ~A      
BACK_FILTERSIZE ~A      
 
BACKPHOTO_TYPE  GLOBAL 

# check images 
~A
 
MEMORY_OBJSTACK 2000      
MEMORY_PIXSTACK 200000    
MEMORY_BUFSIZE  1024      
 
VERBOSE_TYPE    ~A   

# WEIGHT_IMAGE and WEIGHT_TYPE if desired
~A
~A

# # FLAG_IMAGE and WEIGHT_TYPE if desired
~A
~A

# these no longer seem to exist in modern sextractor
#INTERP MAXXLAG         2               # interpolate over 2 pixels at most 
#INTERP MAXYLAG         2

NTHREADS      ~D # 0 to set automatically
"
	    catalog-filename
	    param-filename
	    detect-threshold
	    analysis-threshold
	    conv-filename
	    deblend-nthresh
	    deblend-mincont
	    (format nil "~{~F~^,~}" phot-apertures) ;; print as "1.0,2.0,3.0"
	    (format nil "~F,~F" (first phot-autoparams) 
		    (second phot-autoparams))
	    satur-level
	    mag-zeropoint
	    gain
	    pixel-scale
	    nnw-filename
	    back-size
	    back-filtersize
	    ;; generate two lines for check images
	    (if checkimage-types-and-names
		(with-output-to-string (s)
		  (write-string "CHECKIMAGE_TYPE " s)
		  (loop for first = T then NIL
			for pair in checkimage-types-and-names
			for type = (first pair)
			do (format s " ~A ~A" (if first "" ",") type))
		  (terpri s)
		  (write-string "CHECKIMAGE_NAME " s)
		  (loop for first = T then NIL
			for pair in checkimage-types-and-names
			for name = (second pair)
			do (format s " ~A ~A" (if first "" ",") name))
		  (terpri s))
		"")
	    verbose-type
	    ;;
	    (if  weight-image (format  nil "WEIGHT_IMAGE ~A" weight-image) "")
	    (if  weight-image (format  nil "WEIGHT_TYPE ~A" weight-type) "")
	    ;;
	    (if  flag-image (format  nil "FLAG_IMAGE ~A" flag-image) "")
	    (if  flag-image (format  nil "FLAG_TYPE ~A" flag-type) "")
	    ;;
	    nthreads
	    )))


    

(defun %make-sextractor-param-file (&key (param-filename "default.param") 
				      (n-phot-apertures 1)
				      (extra-output-parameters nil))
  "Make a param output file.  N-MAG-APER is the number of aperture magnitudes."
    
  (with-open-file (sout param-filename :direction :output :if-exists :supersede)
    (write-string 
     "NUMBER

FLUX_AUTO
FLUXERR_AUTO
MAG_AUTO
MAGERR_AUTO

FLUX_BEST
FLUXERR_BEST
MAG_BEST
MAGERR_BEST

FWHM_IMAGE
FWHM_WORLD

"
     sout)

    (format sout "# ~D photometric aperture(s) were specified in configuration~%" n-phot-apertures)
    (loop for i from 1 to n-phot-apertures
	  do (format sout "FLUX_APER(~D)~%FLUXERR_APER(~D)~%MAG_APER(~D)~%MAGERR_APER(~D)~%" i i i i))

    (write-string "

#THRESHOLD
#MU_THRESHOLD
FLUX_MAX
#MU_MAX
#ISOAREA_IMAGE
#ISOAREA_WORLD

#XMIN_IMAGE
#YMIN_IMAGE
#XMAX_IMAGE
#YMAX_IMAGE

#RMS profiles 
CXX_IMAGE 
CYY_IMAGE
CXY_IMAGE
A_IMAGE
B_IMAGE
THETA_IMAGE
CXX_WORLD
CYY_WORLD
CXY_WORLD
A_WORLD
B_WORLD
THETA_WORLD

X_IMAGE
Y_IMAGE
XWIN_IMAGE
YWIN_IMAGE

X_WORLD 
Y_WORLD
ALPHA_J2000
DELTA_J2000
ERRX2_WORLD
ERRY2_WORLD
ERRXY_WORLD
# windowed coords give better astrometry
ALPHAWIN_J2000
DELTAWIN_J2000
ERRX2WIN_WORLD
ERRY2WIN_WORLD
ERRXYWIN_WORLD

ERRAWIN_IMAGE
ERRBWIN_IMAGE
ERRTHETAWIN_IMAGE

# peak (brightest pix) coords are good for comets
XPEAK_IMAGE  
YPEAK_IMAGE  
XPEAK_WORLD  
YPEAK_WORLD  
ALPHAPEAK_J2000 
DELTAPEAK_J2000 

FLAGS
FLAGS_WEIGHT
FLUX_RADIUS

# gaussian weighted signal to noise
SNR_WIN

# flags from flag image - flag image must exist to use
#IMAFLAGS_ISO(1)
#NIMAFLAGS_ISO(1)

#CLASS_STAR
#VIGNET(5,5)
" 
     sout)
    ;; now write any extra pameters specified
    (loop for param in extra-output-parameters
	  do (write-line param sout))))


(defun %make-sextractor-nnw-file (&key (nnw-filename "default.nnw"))
  (with-open-file (sout nnw-filename :direction :output :if-exists :supersede)
    (write-string
"
# Neural Network Weights for the SExtractor star/galaxy classifier (V1.3)
# inputs:       9 for profile parameters + 1 for seeing.
# outputs:      ``Stellarity index'' (0.0 to 1.0)
# Seeing FWHM range: from 0.025 to 5.5'' (images must have 1.5 < FWHM < 5 pixels)
# Optimized for Moffat profiles with 2<= beta <= 4.

 3 10 10  1

-1.56604e+00 -2.48265e+00 -1.44564e+00 -1.24675e+00 -9.44913e-01 -5.22453e-01  4.61342e-02  8.31957e-01  2.15505e+00  2.64769e-01
 3.03477e+00  2.69561e+00  3.16188e+00  3.34497e+00  3.51885e+00  3.65570e+00  3.74856e+00  3.84541e+00  4.22811e+00  3.27734e+00

-3.22480e-01 -2.12804e+00  6.50750e-01 -1.11242e+00 -1.40683e+00 -1.55944e+00 -1.84558e+00 -1.18946e-01  5.52395e-01 -4.36564e-01 -5
.30052e+00
 4.62594e-01 -3.29127e+00  1.10950e+00 -6.01857e-01  1.29492e-01  1.42290e+00  2.90741e+00  2.44058e+00 -9.19118e-01  8.42851e-01 -4
.69824e+00
-2.57424e+00  8.96469e-01  8.34775e-01  2.18845e+00  2.46526e+00  8.60878e-02 -6.88080e-01 -1.33623e-02  9.30403e-02  1.64942e+00 -1
.01231e+00
 4.81041e+00  1.53747e+00 -1.12216e+00 -3.16008e+00 -1.67404e+00 -1.75767e+00 -1.29310e+00  5.59549e-01  8.08468e-01 -1.01592e-02 -7
.54052e+00
 1.01933e+01 -2.09484e+01 -1.07426e+00  9.87912e-01  6.05210e-01 -6.04535e-02 -5.87826e-01 -7.94117e-01 -4.89190e-01 -8.12710e-02 -2
.07067e+01
-5.31793e+00  7.94240e+00 -4.64165e+00 -4.37436e+00 -1.55417e+00  7.54368e-01  1.09608e+00  1.45967e+00  1.62946e+00 -1.01301e+00  1
.13514e-01
 2.20336e-01  1.70056e+00 -5.20105e-01 -4.28330e-01  1.57258e-03 -3.36502e-01 -8.18568e-02 -7.16163e+00  8.23195e+00 -1.71561e-02 -1
.13749e+01
 3.75075e+00  7.25399e+00 -1.75325e+00 -2.68814e+00 -3.71128e+00 -4.62933e+00 -2.13747e+00 -1.89186e-01  1.29122e+00 -7.49380e-01  6
.71712e-01
-8.41923e-01  4.64997e+00  5.65808e-01 -3.08277e-01 -1.01687e+00  1.73127e-01 -8.92130e-01  1.89044e+00 -2.75543e-01 -7.72828e-01  5
.36745e-01
-3.65598e+00  7.56997e+00 -3.76373e+00 -1.74542e+00 -1.37540e-01 -5.55400e-01 -1.59195e-01  1.27910e-01  1.91906e+00  1.42119e+00 -4
.35502e+00

-1.70059e+00 -3.65695e+00  1.22367e+00 -5.74367e-01 -3.29571e+00  2.46316e+00  5.22353e+00  2.42038e+00  1.22919e+00 -9.22250e-01 -2
.32028e+00


 0.00000e+00 
 1.00000e+00 
"
sout)))


(defun %make-sextractor-conv-file (&key (conv-filename "default.conv"))
  (with-open-file (sout conv-filename :direction :output :if-exists :supersede)
    (write-string
"CONV NORM
# 3x3 ``all-ground'' convolution mask with FWHM = 2 pixels.
1 2 1
2 4 2
1 2 1
" sout)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; '(a b c) ==> "a,b,c"
(defun %print-listify (list &key (separator " "))
  (with-output-to-string (s)
    (loop for a in list and b = (cdr list) then (cdr b)
       do (format s "~A~A" a (if b separator "")))))

;; headers copied by default - this is inadequate because
;; different instruments will have different special headers
;; that are needed by INSTRUMENT-ID
(defparameter *copy-keywords-default*
  '("OBJECT" "OBJ" "DATE" "DATE-OBS" "UT" "UT-OBS" "UTDATE" "RA" "DEC"
    "AIRMASS" "HA" "ST" "ZD" "OBSTYPE"
    "INSTRUMENT" "FILTER" "FILT" "MJD" "MJD-DATE" "MJD-OBS" "MJDATE"
    "EXPTIME" "LATITUDE" "LONGITUD" "TELESCOP" "INSTRUME" "DETECTOR"
    "IMAGETYP" "EPOCH" "EQUINOX" "FILTER1" "FILTER2"
    "EXTNAME" "DET-ID"
    "EPOMJD";; ESADC2 and other ESA?
    ))
  
(defun %make-swarp-conf-file
    (&key 
       (conf-filename "default.swarp")
       (imageout-name "coadd.fits")
       (weightout-name "coadd.weight.fits")
       (header-suffix "head")
       ;; weights during coaddition
       (weight-type "NONE")
       (weight-suffix ".weight.fits")
       (weight-image nil)
       ;;
       (combine "Y")
       (combine-type "MEDIAN")
       (center-type "ALL")
       (blank-badpixels "Y")
       ;; satlev might not do anything
       (satlev-keyword "SATURATE")
       (satlev-default 55000)
       ;;
       (center "00:00:00.0, +00:00:00.0")
       (pixel-scale-type "MEDIAN")
       (pixel-scale 0)			     ;; 0=automatic
       (image-size 0)			     ;; 0=automatic
       (resample-dir ".")
       (resampling-type "LANCZOS3")
       (fscale-default 1.0)
       (gain-default 0.0)
       (subtract-back "Y")
       (delete-tmpfiles "Y")
       (copy-keywords *copy-keywords-default*)
       (verbose-type "NORMAL")
       (nthreads 0)
       (nopenfiles-max #+darwin 200 #-darwin 512) ;; macintosh limit is low
       )
  (with-open-file (sout conf-filename :direction 
			:output :if-exists :supersede)
    (format sout
	    "# Default configuration file for SWarp 2.17.1
# EB 2008-05-22
#
#----------------------------------- Output -----------------------------------
IMAGEOUT_NAME          ~A      # Output filename
WEIGHTOUT_NAME         ~A      # Output weight-map filename
 
HEADER_ONLY            N               # Only a header as an output file (Y/N)?
HEADER_SUFFIX          .~A           # Filename extension for additional headers
 
#------------------------------- Input Weights --------------------------------
 
WEIGHT_TYPE            ~A            # BACKGROUND,MAP_RMS,MAP_VARIANCE
                                     # or MAP_WEIGHT
WEIGHT_SUFFIX          ~A           # Suffix to use for weight-maps
WEIGHT_IMAGE           ~A              # Weightmap filename if suffix not used
                                       # (all or for each weight-map)
 
#------------------------------- Co-addition ----------------------------------
 
COMBINE                ~A               # Combine resampled images (Y/N)?
COMBINE_TYPE           ~A          # MEDIAN,AVERAGE,MIN,MAX,WEIGHTED,CHI2
                                       # or SUM
 
#-------------------------------- Astrometry ----------------------------------
 
CELESTIAL_TYPE         NATIVE          # NATIVE, PIXEL, EQUATORIAL,
                                       # GALACTIC,ECLIPTIC, or SUPERGALACTIC
PROJECTION_TYPE        TAN             # Any WCS projection code or NONE
PROJECTION_ERR         0.001           # Maximum projection error (in output
                                       # pixels), or 0 for no approximation
CENTER_TYPE            ~A             # MANUAL, ALL or MOST
CENTER                 ~A # Coordinates of the image center
PIXELSCALE_TYPE        ~A          # MANUAL,FIT,MIN,MAX or MEDIAN
PIXEL_SCALE            ~F             # Pixel scale
IMAGE_SIZE             ~A               # Image size (0 = AUTOMATIC)
 
#-------------------------------- Resampling ----------------------------------
 
RESAMPLE               Y               # Resample input images (Y/N)?
RESAMPLE_DIR           ~A               # Directory path for resampled images
RESAMPLE_SUFFIX        .resamp.fits    # filename extension for resampled images
 
RESAMPLING_TYPE        ~A        # NEAREST,BILINEAR,LANCZOS2,LANCZOS3
                                       # or LANCZOS4 (1 per axis)
OVERSAMPLING           0               # Oversampling in each dimension
                                       # (0 = automatic)
INTERPOLATE            Y               # Interpolate bad input pixels (Y/N)?
                                       # (all or for each image)
 
FSCALASTRO_TYPE        FIXED           # NONE,FIXED, or VARIABLE
FSCALE_KEYWORD         FLXSCALE        # FITS keyword for the multiplicative
                                       # factor applied to each input image
FSCALE_DEFAULT         ~F             # Default FSCALE value if not in header
 
GAIN_KEYWORD           GAIN            # FITS keyword for effect. gain (e-/ADU)
GAIN_DEFAULT           ~F             # Default gain if no FITS keyword found
BLANK_BADPIXELS        ~A 
#--------------------------- Background subtraction ---------------------------
 
SUBTRACT_BACK          ~A               # Subtraction sky background (Y/N)?
                                       # (all or for each image)

BACK_TYPE              AUTO            # AUTO or MANUAL
                                       # (all or for each image)
BACK_DEFAULT           0.0             # Default background value in MANUAL
                                       # (all or for each image)
BACK_SIZE              128             # Background mesh size (pixels)
                                       # (all or for each image)
BACK_FILTERSIZE        3               # Background map filter range (meshes)
                                       # (all or for each image)
 
#------------------------------ Memory management -----------------------------
 
VMEM_DIR               .               # Directory path for swap files
VMEM_MAX               8192            # Maximum amount of virtual memory (MB)
MEM_MAX                512             # Maximum amount of usable RAM (MB)
COMBINE_BUFSIZE        64              # RAM dedicated to co-addition(MB)
 
#------------------------------ Miscellaneous ---------------------------------
SATLEV_KEYWORD         ~A               # saturation keyword
SATLEV_DEFAULT         ~A               # saturation level if no keyword

DELETE_TMPFILES        ~A               # Delete temporary resampled FITS files
                                       # (Y/N)?
COPY_KEYWORDS          ~A          # List of FITS keywords to propagate
                                       # from the input to the output headers
WRITE_FILEINFO         N               # Write information about each input
                                       # file in the output image header?

# no use for xml
WRITE_XML              N               # Write XML file (Y/N)?
XML_NAME               swarp.xml       # Filename for XML output
VERBOSE_TYPE           ~A              # QUIET,NORMAL or FULL
 
NTHREADS               ~D
NOPENFILES_MAX         ~A             
"
	    imageout-name
	    weightout-name
	    header-suffix
	    weight-type 
	    weight-suffix 
	    (if (not weight-image)
		""
		(if (listp weight-image)
		    (%print-listify weight-image)
		    weight-image))
	    combine
	    combine-type
	    center-type
	    (cond ((stringp center) center)
		  ((and (typep center 'sequence) (= (length center) 2))
		   (format nil "~A,~A" (elt center 0) (elt center 1)))
		  (t
		   (error "CENTER=~A is not a string or a sequence of length 2" center)))
	    pixel-scale-type
	    pixel-scale
	    ;; image size can be 1 or 2 dims
	    (cond ((integerp image-size)
		   image-size)
		  ((typep image-size 'sequence)
		   (format nil "~A, ~A" (elt image-size 0) (elt image-size 1)))
		  (t (error "invalid image-size ~A" image-size)))
	    ;;
	    resample-dir
	    resampling-type
	    fscale-default
	    gain-default
	    blank-badpixels
	    subtract-back
	    ;; it looks like SATLEV might not do anything
	    satlev-keyword satlev-default
	    delete-tmpfiles
	    (%print-listify copy-keywords :separator ",")
	    verbose-type
	    nthreads
	    nopenfiles-max
	    )))
