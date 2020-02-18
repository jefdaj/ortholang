#!/usr/bin/env bash

set -e

outdir="$(dirname "$1")"; shift
faapaths="$@"

mkdir -p "$outdir"
cd "$outdir"
cat $faapaths | diamond makedb --db result > out 2> err
sync
mv result.dmnd result
