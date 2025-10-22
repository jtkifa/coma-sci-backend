#!/bin/sh

# uses sbcl in the shell variable $SBCL, and requires
# an initialization file in $SBCLRC, or uses user file

################################################################
# The ${SBCL+x} identifies empty variable SBCL=""
#
if [ -z "${SBCL+x}" ]; then
    sbcl_program=`which sbcl`
else
    sbcl_program=$SBCL
fi

if [ ! -x $sbcl_program ]; then
    echo "ERROR: $sbcl_program is not an executable"
    exit 1
fi

################################################################
# $SBCLRC is an optional environment variable for the sbcl init file.
# If not defined, then revert to $HOME/.sbclrc

if [ -z "${SBCLRC+x}" ]; then
    if [ ! -f $HOME/.sbclrc ]; then
       echo "Environment variable SBCLRC denonoting sbcl init file not defined and cannot find .sbclrc in home directory."
    else
	export SBCLRC=$HOME/.sbclrc
    fi
fi

if [ ! -f $SBCLRC ]; then
    echo "ERROR: $SBCLRC is not a file.  Specify valid environment variable $SBCLRC"
    exit 1
fi

################################################################

# the lispfile has the same name as shell script, but suffix ".lisp"
export SCRIPTFILE=$0
export LISPFILE=$0.lisp

if [ ! -f $LISPFILE ]; then
    echo "ERROR: $LISPFILE does not exist"
    exit 1
fi

################################################################

# this is the simple form; will not load system-wide init file
# -script as runtime is the same as
#  --noinform --disable-ldb --lose-on-corruption --end-runtime-options --script filename
exec $sbcl_program   --dynamic-space-size 8192  --script $LISPFILE  "$@"

      
