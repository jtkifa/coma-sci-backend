FROM ubuntu:24.04

RUN apt-get update && apt-get install -y --no-install-recommends \
  wget build-essential curl git ca-certificates \
  zlib1g-dev libcurl4-openssl-dev libssl-dev
RUN apt-get install -y gfortran libgfortran5 pgplot5 libfftw3-double3 libfftw3-single3 libfftw3-dev
# Create symlinks for CFFI to find FFTW3 libraries
RUN (ln -s /usr/lib/aarch64-linux-gnu/libfftw3.so /usr/lib/libfftw3.so || \
  ln -s /usr/lib/x86_64-linux-gnu/libfftw3.so /usr/lib/libfftw3.so || true) && \
  (ln -s /usr/lib/aarch64-linux-gnu/libfftw3f.so /usr/lib/libfftw3f.so || \
  ln -s /usr/lib/x86_64-linux-gnu/libfftw3f.so /usr/lib/libfftw3f.so || true) && \
  (ln -s /usr/lib/aarch64-linux-gnu/libfftw3l.so /usr/lib/libfftw3l.so || \
  ln -s /usr/lib/x86_64-linux-gnu/libfftw3l.so /usr/lib/libfftw3l.so || true)
# Additional libraries from original dynamic-libraries (XPA, WCS)
RUN apt-get install -y libxpa-dev libxpa1 xpa-tools wcslib-dev libwcs8

# ------------------------------------------------------------------
# Build and install CFITSIO with reentrant (thread-safe) support
# ------------------------------------------------------------------
WORKDIR /usr/local/src

RUN wget https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio_latest.tar.gz --no-check-certificate && \
  tar xzf cfitsio_latest.tar.gz && \
  cd cfitsio-* && \
  ./configure --prefix=/usr/local --enable-reentrant --enable-shared  && \
  make -j"$(nproc)" && \
  make install && \
  ldconfig && \
  cd /usr/local/src && rm -rf cfitsio cfitsio_latest.tar.gz

# ------------------------------------------------------------------
# Install TERAPIX reqs and other astro sw via astromatic  
# ------------------------------------------------------------------
RUN apt-get install -y source-extractor swarp scamp libatlas-base-dev libblas-dev liblapack-dev


# ------------------------------------------------------------------
# Compile and install SLALIB from C++ port (slalib-cpp)
# ------------------------------------------------------------------
# WORKDIR /usr/local/src
# RUN apt-get install -y git cmake && \
#   git clone --depth 1 https://github.com/cyberhull/slalib-cpp.git && \
#   cd slalib-cpp && \
#   mkdir build && cd build && \
#   cmake -DBUILD_SHARED_LIBS=ON .. && \
#   make && \
#   cp lib/libslalib.so /usr/local/lib/libslalib.so && \
#   ldconfig && \
#   cd /usr/local/src && rm -rf slalib-cpp

# ------------------------------------------------------------------
# Compile and install SLALIB from Fortran modded Jan Kleyna source
# ------------------------------------------------------------------
WORKDIR /usr/local/src
RUN git clone --depth 1 https://github.com/wakatara/slalib.starlink.modded.git && \
  cd slalib.starlink.modded && \
  make && \
  cp libslalib.so /usr/local/lib/libslalib.so && \
  cp slalib.h /usr/local/include/slalib.h && \
  ldconfig && \
  cd /usr/local/src && rm -rf slalib.starlink.modded

# ------------------------------------------------------------------
# Build and install simple-kepler-solver (Kepler orbit solver)
# Archival copy of Mehmet Atakan Gürkan's sol_kep code
# Used by Jan Kleyna in COMA sci-backend for isochrone/isodyne calculations
# ------------------------------------------------------------------
WORKDIR /usr/local/src
RUN git clone --depth 1 https://github.com/wakatara/simple-kepler-solver.git && \
  cd simple-kepler-solver && \
  make && \
  make install && \
  ldconfig && \
  cd /usr/local/src && rm -rf simple-kepler-solver

# ------------------------------------------------------------------
# Install SBCL and Quicklisp
# ------------------------------------------------------------------
RUN apt-get install -y sbcl

# Install Quicklisp
WORKDIR /root
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
  sbcl --non-interactive \
  --load quicklisp.lisp \
  --eval '(quicklisp-quickstart:install :path "/root/quicklisp")' && \
  rm quicklisp.lisp

# Install buildapp for creating executables
RUN sbcl --non-interactive \
  --load /root/quicklisp/setup.lisp \
  --eval '(ql:quickload :buildapp)' \
  --eval '(buildapp:build-buildapp "/usr/local/bin/buildapp")'

# ------------------------------------------------------------------
# Compile and install cdsclient from CDS official source
# ------------------------------------------------------------------
WORKDIR /usr/local/src
RUN wget http://cdsarc.cds.unistra.fr/ftp/pub/sw/cdsclient.tar.gz && \
  tar xzf cdsclient.tar.gz && \
  cd cdsclient-* && \
  ./configure --prefix=/usr/local && \
  make && \
  make install && \
  cd /usr/local/src && rm -rf cdsclient-* cdsclient.tar.gz


# ------------------------------------------------------------------
# Create directory structure for sci-backend data volume mounts
# ------------------------------------------------------------------
RUN mkdir -p /data/support/sci-backend/catalogs \
  /data/support/config \
  /data/support/sci-backend/cache \
  /data/support/sci-backend/orbits \
  /data/support/sci-backend/work && \
  chmod -R 755 /data

# ------------------------------------------------------------------
# Copy and build coma-backend-jtk Lisp application
# ------------------------------------------------------------------
WORKDIR /root/coma-backend-jtk

# Copy the entire coma-backend-jtk source tree from current directory
# Note: nrwavelets is present in this codebase but build step is commented out (extraneous)
COPY . .

# Set LISP_LIB environment variable (required by Jan's jk-datadir package)
ENV LISP_LIB=/root/coma-backend-jtk
ENV LISP_LIB_DATADIR=/data/support/sci-backend

# Build nrwavelets library from C source (Daubechies wavelets from Numerical Recipes)
# This enables wavelet-based image filtering in imutils package
WORKDIR /root/coma-backend-jtk/jlib/nrwavelets
RUN make && make install

# Set library path so CFFI can find nrwavelets.so during build
ARG LD_LIBRARY_PATH=""
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

RUN sbcl --non-interactive \
  --load /root/quicklisp/setup.lisp \
  --eval '(ql:quickload :yason)' \
  --eval '(ql:quickload :alexandria)' \
  --eval '(ql:quickload :hunchentoot)' \
  --eval '(ql:quickload :drakma)' \
  --eval '(ql:quickload :cl-ppcre)' \
  --eval '(ql:quickload :cffi)' \
  --eval '(ql:quickload :bordeaux-threads)' \
  --eval '(ql:quickload :cl-fad)' \
  --eval '(ql:quickload :cxml)' \
  --eval '(ql:quickload :xmls)' \
  --eval '(ql:quickload :fare-csv)' \
  --eval '(ql:quickload :md5)' \
  --eval '(ql:quickload :salza2)' \
  --eval '(ql:quickload :lparallel)'

# ------------------------------------------------------------------
# Download ASTORB asteroid orbit database at build time
# Docker named volume will be initialized with this content on first container start
# FASL will be compiled at runtime and stored in the same volume for persistence
# ------------------------------------------------------------------
WORKDIR /root/coma-backend-jtk
RUN echo "Downloading latest ASTORB database from Lowell Observatory..." && \
  mkdir -p /data/support/sci-backend/astorb && \
  cd /data/support/sci-backend/astorb && \
  MJD=$((( $(date +%s) / 86400 ) + 40587 - 1)) && \
  echo "Using MJD: $MJD (Lowell is 1 day behind)" && \
  wget --timeout=60 --tries=3 -O astorb.dat.${MJD}.gz https://ftp.lowell.edu/pub/elgb/astorb.dat.gz && \
  echo "ASTORB download complete ($(du -h astorb.dat.${MJD}.gz | cut -f1))." && \
  echo "ASTORB ready for Docker volume initialization" && \
  ls -lh astorb.dat.${MJD}.gz

# Build the coma-json-server executable with buildapp
# First try to load with debugging to get backtrace if it fails
RUN sbcl --noinform --non-interactive \
  --load /root/quicklisp/setup.lisp \
  --eval '(asdf:initialize-source-registry (quote (:source-registry (:tree "/root/coma-backend-jtk/") :inherit-configuration)))' \
  --eval '(handler-bind ((error (lambda (c) (format t "~%~%ERROR: ~A~%~%BACKTRACE:~%" c) (sb-debug:print-backtrace :count 50) (sb-ext:exit :code 1)))) (asdf:load-system :coma-sci-backend))' \
  && echo "System loaded successfully!" \
  || (echo "Failed to load system - see backtrace above" && exit 1)

# If loading succeeded, build with buildapp
# Use --dynamic-space-size 4096 (4GB in megabytes) for ASTORB FASL compilation
RUN buildapp --output /usr/local/bin/coma-sci-backend \
  --dynamic-space-size 4096 \
  --asdf-tree /root/coma-backend-jtk \
  --load-system coma-sci-backend \
  --entry coma-sci-backend:main

# Make executable
RUN chmod +x /usr/local/bin/coma-sci-backend

# Set working directory
WORKDIR /root

# Copy entrypoint script to handle SBCL heap size configuration at runtime
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

# Expose port
EXPOSE 5054

# Use entrypoint script to pass --dynamic-space-size flag at runtime
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["/usr/local/bin/coma-sci-backend"]

