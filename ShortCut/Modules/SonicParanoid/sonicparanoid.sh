#!/usr/bin/env bash

# TODO does this one need special treatment when a run fails?

set -e

OUTPATH="$1"
CACHEDIR="$2"
SHAREDDIR="$3" # also a cache dir, but the shared one
DBDIR="$4"
INDIR="$5"
MODE="$6"  # TODO set based on fn name
DEBUG="$7" # TODO set based on config?

cd "$CACHEDIR" # TODO is this right?
sonicparanoid -sh "$SHAREDDIR" -db "$DBDIR" -i "$INDIR" -o "$CACHEDIR" -m "$MODE" -noidx $DEBUG -op > "${CACHEDIR}.out" 2> "${CACHEDIR}.err"
