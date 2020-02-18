#!/usr/bin/env bash

set -e

outdir="$(dirname "$1")"
faapath="$2"

# diamond requires a .dmnd extension on result here
mkdir -p "$outdir"
cd "$outdir"
diamond makedb --in "$faapath" --db result > out 2> err
mv result.dmnd result
