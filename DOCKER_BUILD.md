# Docker Build Instructions for coma-sci-backend

## Overview

This is Jan Kleyna's COMA Science Backend repository with integrated Docker support for containerized deployment.

## Docker Build Architecture

### Self-Contained Repository

All build artifacts and source code are in one location:
```
/Users/daryl/Code/UHawaii/coma-sci-backend/
├── Dockerfile              # Docker build configuration
├── docker-compose.yml      # Docker Compose orchestration
├── docker-entrypoint.sh    # Container startup script
├── coma-sci-backend.asd    # ASDF system definition
├── coma-json-server/       # Main application code
├── astro/                  # Astronomy packages
├── jlib/                   # Utility libraries
└── ...                     # Other packages
```

**Build context**: Current directory (`.`)

## Build Process

### 1. Native Libraries (Built First)

The following libraries are built from source during the Docker build:

**Custom-built libraries:**
- **CFITSIO** - Thread-safe FITS I/O (--enable-reentrant)
- **SLALIB** - Jan Kleyna's modified Starlink version
- **kdtree** - Spatial indexing
- **concaveman** - Concave hull computation
- **tsnnls** - Sparse non-negative least squares
- **simple-kepler-solver** (solkep) - Kepler orbit propagation
- **CDS client** - Catalog access

**From Ubuntu packages:**
- XPA, WCS, libcairo,  TERAPIX tools (SExtractor, SCAMP, etc.)
- FFTW3
- PGPLOT   (now needed only for phot-calib diagnostic plots)

### 2. Lisp Application Build

The Dockerfile:
1. Installs SBCL from Ubuntu packages
2. Installs Quicklisp
3. Installs buildapp via Quicklisp
4. **Copies entire repository** (COPY . . from build context)
5. Sets environment variables:
   - `LISP_LIB=/root/coma-backend-jtk`
   - `LISP_LIB_DATADIR=/data/support/sci-backend`
6. Builds nrwavelets C library
7. Loads all Quicklisp dependencies
8. Downloads ASTORB database from Lowell Observatory
9. Builds executable with buildapp:
   - Entry point: `coma-sci-backend:main`
   - Output: `/usr/local/bin/coma-sci-backend`
   - Dynamic space size: 4GB (for compilation)
   - ASDF tree: `/root/coma-backend-jtk`

### 3. Container Startup and Runtime Initialization

#### Docker Entrypoint Script (`docker-entrypoint.sh`)
1. Copies ASTORB database from `/opt/astorb/` to `/data/support/sci-backend/astorb/`
   - Only if not already present in volume
   - Preserves ASTORB across container restarts via named volume
2. Launches executable with `--dynamic-space-size 8192` (8GB heap)
   - Required for ASTORB FASL compilation at runtime

#### Application Entry Point (`coma-sci-backend:main`)
The application starts with `coma-sci-backend:main` which:
1. Parses environment variables (`COMA_PORT`, `COMA_HOST`)
2. **Performs runtime initialization**:
   - Initializes lparallel kernel (4 worker threads)
   - Initializes ASTORB file list
   - Attempts to retrieve newest ASTORB file (if not cached)
   - Loads ASTORB asteroid database (compiles FASL on first run)
   - Initializes orbit elements for small body identification
3. Launches Hunchentoot web server
4. Runs indefinitely

**Note:** Most small datasets (observatory data, Landolt standards, small body names, comet data) are loaded at compile-time via `eval-when` blocks. Only lparallel and ASTORB require runtime initialization.

## Docker-Specific Code Modifications

### Key Design Decisions

**Runtime vs Compile-Time Loading Strategy:**
- **Compile-time** (via eval-when): Small datasets that can be compiled into executable
  - Observatory data, Landolt standards, small body names, comet data
  - These are now loaded at compile-time and work correctly
- **Runtime** (in main.lisp): Large datasets and thread initialization
  - lparallel kernel (buildapp limitation: cannot save core with threads)
  - ASTORB database (~200MB compressed, ~1GB FASL)
  - Orbit elements (derived from ASTORB)

### Critical Files Modified

**Runtime initialization only (2 files):**
1. `astro/small-body-identify/sbid-search.lisp` - **lparallel threads commented out** (MUST stay disabled - buildapp limitation)
2. `astro/astorb/astorb.lisp` - **ASTORB loading commented out** (too large for image)

**Added runtime initialization (1 file):**
- `coma-json-server/main.lisp` - Entry point with runtime init for lparallel and ASTORB

**Fixed YASON API (1 file):**
- `coma-json-server/dispatcher.lisp` - Removed deprecated `yason:*yason-float-type*`

**Added package exports:**
- Various package files export runtime initialization functions

**Note:** Most eval-when blocks are now ENABLED and work correctly. Only lparallel and ASTORB require runtime loading.

## Build Command

```bash
cd /Users/daryl/Code/UHawaii/coma-sci-backend
docker-compose build
```

Or using docker build directly:
```bash
cd /Users/daryl/Code/UHawaii/coma-sci-backend
docker build -t sci-backend:latest .
```

## Run Command

```bash
cd /Users/daryl/Code/UHawaii/coma-sci-backend
docker-compose up -d
```

Or using docker run:
```bash
docker run -d \
  -p 5054:5054 \
  -v sci-backend-data:/data/support/sci-backend \
  -e COMA_PORT=5054 \
  -e COMA_HOST=0.0.0.0 \
  -e LD_LIBRARY_PATH=/usr/local/lib \
  --name sci-backend \
  sci-backend:latest
```

## Data Files Strategy

### Included in Repository and Compiled Into Executable

The following small data files are in the repository and loaded at compile-time via `eval-when`:
- `astro/small-body-name/data/` - Small body name mappings
- `astro/landolt-standards/data/` - Landolt photometric standards
- `astro/observatories/data/` - Observatory coordinates
- `astro/id-ss-object-at-pos/data/` - Comet orbit data
- `astro/small-body-identify/data/` - Orbit element templates

These are compiled into the executable during Docker build.

### Downloaded at Build Time, Provided via Volume at Runtime

**ASTORB Database:**
- Downloaded from Lowell Observatory during Docker build
- Stored in image at `/data/support/sci-backend/astorb/`
- Copied to named volume `sci-backend-data` at container startup
- FASL cache persisted in same volume for fast subsequent startups

### Optional Volume Mounts for FITS Data

Telescope FITS data directories can be mounted as needed:
- Comment/uncomment volume mounts in `docker-compose.yml`
- Examples provided for common observatories (CFHT, Atlas, Gemini)

## Troubleshooting

### If build fails on Lisp compilation:

Check the buildapp output for missing dependencies or compilation errors in specific packages.

### If executable fails to start:

1. Check native libraries are loaded:
   ```bash
   docker run --rm sci-backend:latest ldd /usr/local/bin/coma-sci-backend
   ```

2. Check that runtime initialization completes:
   ```bash
   docker logs sci-backend
   ```
   Look for "Runtime initialization complete!" message

3. Check ASTORB database was copied:
   ```bash
   docker exec sci-backend ls -lh /data/support/sci-backend/astorb/
   ```

### If server starts but doesn't respond:

1. Verify port mapping matches COMA_PORT (5054)
2. Check logs: `docker logs sci-backend`
3. Test inside container:
   ```bash
   docker exec sci-backend curl -f http://localhost:5054/health
   ```
4. Test from host:
   ```bash
   curl -X POST http://localhost:5054/submit-json \
     -H "Content-Type: application/json" \
     -d '{"TYPE":"REQUEST","COMMAND":"HELLO","ID":"test"}'
   ```

## Maintaining Docker Compatibility

### Critical Requirements (MUST Preserve):

1. **lparallel initialization** in `sbid-search.lisp`
   - MUST stay commented out at compile-time
   - MUST be initialized at runtime in `main.lisp`
   - Reason: buildapp cannot save core with active threads

2. **ASTORB loading** in `astorb.lisp`
   - MUST stay commented out at compile-time
   - MUST be loaded at runtime in `main.lisp`
   - Reason: Too large for Docker image (~200MB compressed, ~1GB FASL)

3. **Runtime initialization** in `main.lisp`
   - Entry point must call lparallel and ASTORB initialization
   - Handles errors gracefully

4. **YASON API compatibility**
   - Don't use deprecated `yason:*yason-float-type*`
   - Compatible with modern YASON (20250622+)

### Working Correctly (No Changes Needed):

- All small dataset eval-when blocks work correctly
- Observatory data, Landolt standards, small body names, comet data
- These are compiled into executable at build time

### Repository Organization:

This is a self-contained repository:
- Dockerfile, docker-compose.yml, and source in same directory
- No external dependencies beyond system packages and Quicklisp
- Clean separation of build-time and runtime data
- Removed development-only directories (slime/, asdf/, INIT-FILES/)

### Deployment:

Repository is ready for GitHub and containerized deployment:
```bash
# GitHub repository: https://github.com/wakatara/coma-sci-backend
# Docker Hub: Could be published as wakatara/coma-sci-backend
```
