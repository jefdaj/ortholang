#!/usr/bin/env bash

set -e

OUTPATH="$1"
FAPATH="$2"

# TODO temporary outpath then mv/ln?
muscle -clwstrict -in "$FAPATH" -out "${OUTPATH}" > "${OUTPATH}.out" 2> "${OUTPATH}.err"
