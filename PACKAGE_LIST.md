# Complete Package List

Complete inventory of all packages in the coma-sci-backend repository.

## Main Application
1. **coma-sci-backend** - Top-level system definition
2. **coma-sci-backend/coma-json-server** - JSON web service for astronomical computations

## Astronomy Packages (42 packages)

### Image Processing & Reduction
1. imutils - Image processing utilities
2. instrument-id - Camera/instrument identification
3. imred - Image reduction pipeline
4. cfitsio - FITS file I/O bindings
5. fits-stamp-cairo - FITS image stamp creation

### Astrometry & WCS
6. wcs - World Coordinate System support
7. terapix - Astrometry and photometry (SExtractor, SCAMP)
8. sky-project - Sky projection algorithms

### Ephemeris & Orbits
9. slalib - SLALIB positional astronomy library bindings
10. slalib-ephem - Ephemeris calculations using SLALIB
11. kepler-orbit - Keplerian orbit calculations
12. orbital-elements - Orbital element representations
13. orbital-elements-parse - Parsing orbital elements
14. orbital-mech - Orbital mechanics utilities
15. jpl-horizons - JPL Horizons ephemeris service client
16. mpc - Minor Planet Center data access

### Photometry & Calibration
17. phot-calib - Photometric calibration
18. phot-transforms - Color transformations
19. landolt-standards - Landolt standard star data
20. orbphot - Automated photometry on moving objects

### Object Identification & Catalogs
21. small-body-name - Small body name parsing
22. small-body-identify - Object identification
23. astorb - Asteroid orbital database
24. asteroids - Asteroid utilities
25. astro-catalog - Star catalog access
26. id-ss-object-at-pos - Identify solar system objects at position
27. brute-force-comets - Comet identification (sub-package)
28. astro-obj - Astronomical object representations

### Coordinate Systems & Time
29. astro-coords - Coordinate transformations
30. astro-time - Time system conversions
31. ra-dec - Right ascension/declination utilities
32. precess - Precession calculations
33. observatories - Observatory coordinates database

### Specialized Analysis
34. shift-and-add - Co-addition of moving object images
35. isochrones-isodynes - Syndyne/synchrone modeling
36. simple-comet-activity-detector - Comet activity detection
37. a-f-rho - Afρ dust production calculations
38. magnitude-of-sun - Solar magnitudes
39. mpc-packed-desig - MPC packed designation format

### Instrument-Specific
40. gmos-proc - Gemini GMOS processing (sub-package)
41. suprime-cam-fix - Subaru SuprimeCam fixes (sub-package)
42. hypersuprime-cam-fix - Subaru HyperSuprimeCam fixes (sub-package)

## Utility Libraries (29 packages in jlib/)

### Mathematical Operations
1. matrix - Matrix operations
2. llsq - Linear least squares fitting
3. powell - Powell optimization
4. golden-section - Golden section search
5. linterp - Linear interpolation
6. cubic-spline - Cubic spline interpolation
7. three-vector - 3D vector operations

### Statistics
8. bootstrap - Bootstrap statistics
9. bayes-outlier - Bayesian outlier detection
10. stats - Statistical functions
11. nintegrate - Numerical integration

### Image Processing Utilities
12. fastmedian - Fast median filtering
13. fftw3lib - FFTW3 FFT library bindings
14. nrwavelets - Numerical Recipes wavelet transforms

### Data Structures
15. kdtree-jk - K-d tree spatial indexing

### I/O and Parsing
16. file-io - File I/O utilities
17. csv-read - CSV file reading
18. numio - Numeric I/O
19. string-utils - String manipulation utilities
20. jk-parse-float - Fast float parsing
21. float-utils - Floating point utilities
22. cxml-jtk - CXML utilities

### System Utilities
23. jutils - General utilities
24. logger - Logging facility
25. jk-cache - Caching utilities
26. pconfig - Configuration management
27. random - Random number utilities
28. shellsort-jk - Shell sort implementation
29. units - Physical units handling

## Plotting
1. cl-cairo2 - 2d graphics package exporting PNG, PDF, PS, TIFF, SVG, JPEG, for stamps using Lisp package 

## External Packages
1. lparallel - Parallel processing library
2. re (massung-regexp) - Regular expression library
3. cl-html-parse-walker - HTML parsing utilities

## Quicklisp Dependencies (installed via Quicklisp)

Core dependencies loaded during Docker build:
1. **yason** - JSON encoding/decoding (v20250622+)
2. **alexandria** - Common utilities
3. **hunchentoot** - Web server
4. **drakma** - HTTP client
5. **cl-ppcre** - Perl-compatible regular expressions
6. **cffi** - Foreign function interface
7. **bordeaux-threads** - Portable threading
8. **cl-fad** - File and directory utilities
9. **cxml** - XML parsing
10. **xmls** - Simple XML parser
11. **fare-csv** - CSV parsing
12. **md5** - MD5 hashing
13. **salza2** - Compression library
14. **lparallel** - Parallel processing
15. **cl-cairo2** - cairo2 graphics interface for stamps


## Native Libraries (C/C++/Fortran)

Built from source during Docker build:
1. **libcfitsio.so** - FITS file I/O (with --enable-reentrant)
2. **libslalib.so** - SLALIB astronomical library (Fortran, Jan's modified Starlink version)
3. **libsolkep.so** - Simple Kepler orbit solver
4. **libnrwavelets.so** - Numerical Recipes wavelet transforms

Installed from Ubuntu packages:
5. **libxpa.so** / **libxpa1** - XPA inter-process communication
6. **libwcs.so** / **libwcs8** - WCS (World Coordinate System)
7. **libfftw3.so** - Fast Fourier Transform library
9. **source-extractor** - SExtractor (TERAPIX)
10. **cairo** - cairo 2D graphis library for making stamps
10. **scamp** - SCAMP astrometric calibration (TERAPIX)
11. **swarp** - SWarp image resampling (TERAPIX)

## Build Tools

1. **SBCL** - Steel Bank Common Lisp (from Ubuntu packages)
2. **Quicklisp** - Common Lisp package manager
3. **buildapp** - Creates standalone executables from SBCL applications
