#!/usr/bin/env bash

set -e

OUTPATH="$1"
FAPATH="$2"

hmmbuild "$OUTPATH" "$FAPATH" > "${OUTPATH}.out" 2> "${OUTPATH}.err"

# tweak for reproduciblity
egrep -v '^DATE' "$OUTPATH" > "${OUTPATH}.tmp" && mv "${OUTPATH}.tmp" "$OUTPATH"
