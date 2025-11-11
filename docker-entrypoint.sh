#!/bin/bash
set -e

echo "====================================="
echo "COMA Sci-Backend Starting..."
echo "====================================="


ASTORB_DATADIR=$LISP_LIB_DATADIR/astorb 

# copy all Lisp data (like astorb.MJD.dat.gz and astorb..fasl) from
# build location to runtime location.  If they don't exist then they will be
# dowloaded and compiled at run time.

if [ -d $LISP_LIB_DATADIR ] && [ -d /opt/lisp-data ] ; then
    # trailing slash on source dir means copy contents to dest dir
    rsync -a /opt/lisp-data/ $LISP_LIB_DATADIR
fi

# If a more recent astorb.MJD.dat.gz is present in $ASTORB_DATADIR
# then the current fasl file will be ignored and the new one will be rebuilt.
# This will take about 5-10 minutes.



# set auto-download of astorb to true, so that if astorb
# isn't present, it will be retrieved and compiled
export GET_ASTORB=TRUE

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
