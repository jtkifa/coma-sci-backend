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
# Copy and build coma-backend Lisp application
# ------------------------------------------------------------------
WORKDIR /root/coma-backend

# Copy the entire coma-backend source tree from current directory
# Note: nrwavelets is present in this codebase but build step is commented out (extraneous)
COPY . .

# Set LISP_LIB environment variable, the top level of Lisp source tree
ENV LISP_LIB=/root/coma-backend
#
# SET LISP_LIB_DATADIR environment variable, where dowloaded and cached Lisp system
#   data is placedd
ENV LISP_LIB_DATADIR=/data/support/sci-backend
RUN mkdir -p $LISP_LIB_DATADIR

# Set the directory where fasls (compiled lisp files, akin to .so or .o files) go
#  when compiled by asdf - the default would be $HOME/cache/common-lisp
ENV LISP_CACHE_DIR=/usr/local/cache/common-lisp
RUN mkdir -p $LISP_CACHE_DIR

# location of Lisp user init file
ENV SBCLRC=$LISP_LIB/sbclrc.lisp

# Build nrwavelets library from C source (Daubechies wavelets from Numerical Recipes)
# This enables wavelet-based image filtering in imutils package
WORKDIR /root/coma-backend/jlib/nrwavelets
RUN make && make install

# Set library path so CFFI can find nrwavelets.so during build
#  - if no LD_LIBRARY_PATH in buildtime, specify a null one
ARG LD_LIBRARY_PATH=""
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH


WORKDIR /root/coma-backend

# download required quicklisp packages
RUN sbcl  --end-toplevel-options \
  --non-interactive \
  --userinit $SBCLRC \
  --load ./quicklisp-package-download.lisp

# ------------------------------------------------------------------
# Download ASTORB asteroid orbit database at build time
# Docker named volume will be initialized with this content on first container start
# FASL will be compiled at runtime and stored in the same volume for persistence
# ------------------------------------------------------------------
RUN echo "Downloading latest ASTORB database from Lowell Observatory..." && \
  mkdir -p /data/support/sci-backend/astorb && \
  cd /data/support/sci-backend/astorb && \
  MJD=$((( $(date +%s) / 86400 ) + 40587 - 1)) && \
  echo "Using MJD: $MJD (Lowell is 1 day behind)" && \
  wget --timeout=60 --tries=3 -O astorb.dat.${MJD}.gz https://ftp.lowell.edu/pub/elgb/astorb.dat.gz && \
  echo "ASTORB download complete ($(du -h astorb.dat.${MJD}.gz | cut -f1))." && \
  echo "ASTORB ready for Docker volume initialization" && \
  ls -lh astorb.dat.${MJD}.gz


# compile the fasl files for coma-json-server
#  dynamic-space is set to allow compilation of giant astorb fasl
RUN sbcl --dynamic-space-size 4096 \
    	 --non-interactive \
      	 --userinit $SBCLRC \
      	 --eval '(asdf:load-system "coma-json-server")'





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

