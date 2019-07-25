#!/usr/bin/env bash

set -e

OUTPATH="$1" # TODO remove? no this should be tdir
ERRPATH="${OUTPATH/.out/.err}"
TDIR="$2"
BLASTCMD="$3" # TODO allow diamond, blast, or mmseqs for comparison?
NTHREADS="$(nproc)" # TODO any reason to get from haskell?

# echo "orthofinder: $(which orthofinder)"
cmd="orthofinder -f '$TDIR' -S '$BLASTCMD' -t '$NTHREADS' -a '$NTHREADS' > '$OUTPATH' 2> '$ERRPATH'"
# echo "cmd: '$cmd'"
eval "$cmd"
# echo "files generated:"
# find "$TDIR"
