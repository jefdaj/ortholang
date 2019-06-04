#!/usr/bin/env bash

set -e

OUTPATH="$1" # note that we only use ${OUTPATH}.tmp for now
TSVPATH="$2"
COLNUM="$3"

cut -f "$COLNUM" "$TSVPATH" | sort | uniq > "${OUTPATH}.tmp" 2> "${OUTPATH}.err"
