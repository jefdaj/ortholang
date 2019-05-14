#!/usr/bin/env bash

set -e

OUTPATH="$1"
DBPATH="$2" # same as prefix in the haskell code?
BLASTCMD="$3"
EDEC="$4" # this is the actual evalue, not a path
QPATH="$5"
PPATH="$6" # TODO why not QPATH?

DBDIR="$(dirname "$DBPATH")"
DBNAME="$(basename "$DBPATH")"

export BLASTDB="$(dirname "$DBDIR")" # TODO DBPATH?
cd "$DBDIR" # TODO remove?

# TODO query on stdin like the current haskell code?
# TODO should this .out + .err + mv thing be a regular practice everywhere?
# TODO and if so, maybe it should be done in haskell? only after it works a couple times the tedious way
$BLASTCMD -db "$DBNAME" -evalue "$EDEC" -outfmt 6 -query "$QPATH" > "${OUTPATH}.out" 2> "${OUTPATH}.err"
ln -s "$(basename ${OUTPATH}.out)" "$OUTPATH"
