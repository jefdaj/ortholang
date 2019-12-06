#!/usr/bin/env bash

# TODO does this one need special treatment when a run fails?
# TODO mmseqs dbdir not implemented yet?

set -e

# OUTPATH="$1" # not used at all
CACHEDIR="$1"
SHAREDDIR="$2" # also a cache dir, but the shared one
# DBDIR="$3" # TODO remove?
INDIR="$4"
MODE="$5"  # TODO set based on fn name
DEBUG="$6" # TODO set based on config?

# unset PYTHONPATH

# cd "$CACHEDIR" # TODO is this right?
sonicparanoid -sh "$SHAREDDIR" -i "$INDIR" -o "$CACHEDIR" -m "$MODE" -noidx $DEBUG -op > "${CACHEDIR}.out" 2> "${CACHEDIR}.err"

# TODO do this in haskell?
cd "$CACHEDIR"
ln -s runs/run_*/ortholog_groups/overall.stats.tsv stats.tsv
