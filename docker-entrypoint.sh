#!/bin/bash
set -e

echo "====================================="
echo "COMA Sci-Backend Starting..."
echo "====================================="


ASTORB_DATADIR=$LISP_LIB_DATADIR/astorb 

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



if [[ "$1" == "coma-json-server" ]]; then
    exec ./astro/COMA-PROJECT/Scripts/coma-json-server \
	 -web-server -web-port $COMA_PORT -web-host "0.0.0.0"
else
    # Execute other commands as-is
    exec "$@"
fi
