#!/usr/bin/env bash

set -e

RESPATH="$1"
OUTPATH="$(dirname "$RESPATH")/out"
ERRPATH="$(dirname "$RESPATH")/err"
FAPATH="$2"

# TODO temporary outpath then mv/ln?
muscle -clwstrict -in "$FAPATH" -out "${RESPATH}" > "$OUTPATH" 2> "$ERRPATH"

# code="$?"
# cp "$OUTPATH" /tmp/muscle-out.txt
# cp "$ERRPATH" /tmp/muscle-err.txt
# exit $code
