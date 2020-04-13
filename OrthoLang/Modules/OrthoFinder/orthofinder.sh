#!/usr/bin/env bash

# set -e

OUTPATH="$1" # TODO remove? no this should be tdir
ERRPATH="${OUTPATH/.out/.err}"
TDIR="$2"
# BLASTCMD="$3" # TODO allow diamond, blast, or mmseqs for comparison?

# TODO ok, the problem is it's not picking up diamond... custom mechanism?
#      and also, maybe this should be configurable or have named versions
#      and another thing, maybe start your all-vs-all module from this code?
BLASTCMD="blast"

NTHREADS="$(nproc)" # TODO any reason to get from haskell?

# echo "which diamond? $(which diamond)"
# echo "which orthofinder? $(which orthofinder)"
orthofinder -f "$TDIR" -S "$BLASTCMD" -t "$NTHREADS" -a "$NTHREADS" > "$OUTPATH" 2> "$ERRPATH"

# code="$?"
# cp "$OUTPATH" /tmp/orthofinder-out.log
# cp "$ERRPATH" /tmp/orthofinder-err.log
# exit $code

# cmd="orthofinder -f '$TDIR' -S '$BLASTCMD' > '$OUTPATH' 2> '$ERRPATH'"
# echo "cmd: $cmd" | tee -a /tmp/orthofinder.log
# eval "$cmd"
