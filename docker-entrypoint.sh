#!/bin/bash
set -e

echo "====================================="
echo "COMA Sci-Backend Starting..."
echo "====================================="

LISP_DATADIR=/data/support/sci-backend/lisp-data
ASTORB_DATDIR=$LISP_DATADIR/astorb 

# Copy ASTORB database from build location to runtime location if needed
if [ -d "/opt/astorb" ] && [ "$(ls -A /opt/astorb)" ]; then
    echo "Checking ASTORB database..."
    mkdir -p $ASTORB_DATADIR

    # Copy any .gz files from /opt/astorb/ that don't exist in target
    for file in /opt/astorb/*.gz; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            if [ ! -f "${ASTORB_DATADIR}/$filename" ]; then
                echo "  Copying fresh ASTORB database: $filename"
                cp "$file" "$ASTORB_DATADIR/$filename"
                echo "  ASTORB database ready ($(du -h $ASTORB_DATADIR/$filename | cut -f1))"
            else
                echo "  ASTORB database already exists: $filename"
            fi
        fi
    done
else
    echo "Warning: No ASTORB database found at /opt/astorb/"
fi

echo "====================================="
echo "Starting COMA Sci-Backend executable"
echo "====================================="

# Launch the executable with increased heap size for ASTORB FASL compilation
# The author normally uses 8GB for ASTORB compilation
# The --dynamic-space-size flag must be passed at runtime to the SBCL-based executable
if [[ "$1" == "/usr/local/bin/coma-sci-backend" ]]; then
    echo "Launching with --dynamic-space-size 8192 (8GB heap)"
    exec "$1" --dynamic-space-size 8192 "${@:2}"
else
    # Execute other commands as-is
    exec "$@"
fi
