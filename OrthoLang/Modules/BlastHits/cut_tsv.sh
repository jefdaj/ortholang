#!/usr/bin/env bash

set -e

OUTPATH="$1"
TSVPATH="$2"
COLNUM="$3"

cut -f "$COLNUM" "$TSVPATH" | sort | uniq > "$OUTPATH" 2> "${OUTPATH/.tmp/.err}"
[[ -s "$OUTPATH" ]] || echo "<<emptybht>>" > "$OUTPATH"
