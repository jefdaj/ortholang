#!/usr/bin/env bash

set -e

OUTPATH="$1"
SHAREDDIR="$2"
DBDIR="$3"
INDIR="$4"
OUTDIR="$5" # TODO is this right?
MODE='fast' # TODO set based on fn name
DEBUG='-d'  # TODO set based on config?

cd "$(dirname "$OUTPATH")" # TODO is this right?
sonicparanoid -sh "$SHAREDDIR" -db "$DBDIR" -i "$INDIR" -o "$OUTDIR" -m "$MODE" -noidx $DEBUG -op > "${OUTPATH}.out" 2> "${OUTPATH}.err"
ln -s "${OUTPATH}.out" "$OUTPATH"
