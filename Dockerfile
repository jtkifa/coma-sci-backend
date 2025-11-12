FROM ubuntu:24.04

# ------------------------------------------------------------------
# DEFINITIONS OF IMPORTANT VARIABLES AND LOCATIONS  ########
# ------------------------------------------------------------------

# main directory where we copy the source tree
ENV COMA_BACKEND_DIR=/root/coma-backend


# BACKEND_DATADIR is persistent directory of sci-backend, defined
#  in docker-compose.yml
ENV BACKEND_DATADIR=/data/support/sci-backend

# LISP_LIB, the top level of Lisp source tree
ENV LISP_LIB=$COMA_BACKEND_DIR
# LISP_LIB_DATADIR is where certain lisp system put persistent data
ENV LISP_LIB_DATADIR=$BACKEND_DATADIR/lisp-data

# LISP_CACHE_DIR is where fasls (compiled lisp files, akin to .so or .o files) go
#  when compiled by asdf - the default would be $HOME/.cache/common-lisp
ENV LISP_CACHE_DIR=/root/.cache/common-lisp/
# location of Lisp user init file
ENV SBCLRC=$LISP_LIB/sbclrc.lisp


# Web server is on COMA_PORT
ENV COMA_PORT=5054

# Don't download and compile astorb in build phase by default, but
# make it possible via ARG GET_ASTORB=TRUE
ARG GET_ASTORB="FALSE"
ENV GET_ASTORB=${GET_ASTORB}

# Run a Lisp listener (REPL), for diagnostics.  Connect using Emacs SLIME.
ARG RUN_LISP_LISTENER="TRUE"
ENV RUN_LISP_LISTENER=${RUN_LISP_LISTENER}
ENV LISP_LISTENER_PORT=5055

# ------------------------------------------------------------------
# Rquired OS packages
# ------------------------------------------------------------------

RUN apt-get update && apt-get install -y --no-install-recommends \
  wget build-essential curl git ca-certificates \
  zlib1g-dev libcurl4-openssl-dev libssl-dev rsync
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


# install SBCL
RUN apt-get install -y sbcl


# ------------------------------------------------------------------
# Create directory structure for sci-backend data volume mounts
# ------------------------------------------------------------------
RUN mkdir -p $BACKEND_DATADIR/catalogs \
  /data/support/config \
  $BACKEND_DATADIR/cache \
  $BACKEND_DATADIR/orbits \
  $BACKEND_DATADIR/work && \
  chmod -R 755 /data

# ------------------------------------------------------------------
# Copy and build coma-backend Lisp application
# ------------------------------------------------------------------


RUN mkdir -p $LISP_CACHE_DIR

WORKDIR $COMA_BACKEND_DIR
# Copy the entire coma-backend source tree from current directory
# Note: nrwavelets is present in this codebase but build step is commented out (extraneous)
COPY . .

# ------------------------------------------------------------------
# Install Quicklisp
# ------------------------------------------------------------------


# download required quicklisp packages
RUN echo "Installing quicklisp package system and downloading quicklisp packages"
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl  \
    --non-interactive \
    --userinit $SBCLRC \
    --load ./quicklisp-package-download.lisp
RUN echo "Done loading quicklisp libraries"



# Build nrwavelets library from C source (Daubechies wavelets from Numerical Recipes)
# This enables wavelet-based image filtering in imutils package
WORKDIR $LISP_LIB/jlib/nrwavelets/c-src/
RUN make && make install

# Set library path so CFFI can find nrwavelets.so during build
#  - if no LD_LIBRARY_PATH in buildtime, specify a null one
ENV LD_LIBRARY_PATH=/usr/local/lib

################################################################
# compile the fasl files for coma-json-server
#  dynamic-space is set to allow compilation of giant astorb fasl
WORKDIR $COMA_BACKEND_DIR
# if argument GET_ASTORB=TRUE then
# astorb will be downloaded from Lowell and compiled into
# a fasl in /opt/lisp-data/astorb.  Otherwise, this step
# will happen at run-time.


# back up datadir variable
ENV LISP_LIB_DATADIR_SAVE=$LISP_LIB_DATADIR
# set LISP_LIB_DATADIR to build location inside container
#   from where it will be copied when running container
ENV LISP_LIB_DATADIR=/opt/lisp-data

# delete any old astorb if building new astorb
RUN if [ ${GET_ASTORB}x=TRUEx ] ; then rm -rf /opt/lisp-data/astorb  ; fi
#
# build coma-json-server, including download and compilation of astorb
RUN ./astro/COMA-PROJECT/Scripts/coma-json-server -help
# restore  LISP_LIB_DATADIR to its location on a Docker volume
ENV LISP_LIB_DATADIR=$LISP_LIB_DATADIR_SAVE
ENV LISP_LIB_DATADIR_SAVE=""
################################################################






# Set working directory
WORKDIR $COMA_BACKEND_DIR

# Expose port
EXPOSE $COMA_PORT
EXPOSE $LISP_LISTENER_PORT

# Use entrypoint script to pass --dynamic-space-size flag at runtime
RUN chmod +x "./docker-entrypoint.sh"

ENTRYPOINT ["./docker-entrypoint.sh"]

# a test-string for ENTRYPOINT
CMD ["coma-json-server"]

