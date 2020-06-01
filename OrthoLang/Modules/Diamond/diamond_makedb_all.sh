#!/usr/bin/env bash

set -e

outdir="$(dirname "$1")"; shift

# TODO go with the more standard approach? absolutize the input paths and pass as one here
faapaths="$@"

mkdir -p "$outdir"
cd "$outdir"

# TODO aha! bug is that we have to add back symlink resolving here:
# echo "faapaths: $faapaths"
# echo "faapaths contents:"
# cat $faapaths

cat $faapaths | diamond makedb --db result > out 2> err
sync
mv result.dmnd result
