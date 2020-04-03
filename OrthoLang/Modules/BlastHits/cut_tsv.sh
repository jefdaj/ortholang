#!/usr/bin/env bash

set -e

OUTDIR="$(dirname "$1")"
OUTPATH="${OUTDIR}/tmp" # will be deduped for final result
ERRPATH="${OUTDIR}/err"

TSVPATH="$2"
COLNUM="$3"

cut -f "$COLNUM" "$TSVPATH" | sort | uniq > "$OUTPATH" 2> "$ERRPATH"
[[ -s "$OUTPATH" ]] || echo "<<emptybht>>" > "$OUTPATH"
