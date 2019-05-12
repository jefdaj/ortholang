#!/usr/bin/env bash

set -e

OUTPATH="$1"
DBPATH="$2"
BLASTCMD="$3"
BLASTARGS="$4"
EDEC="$5" # this is the actual evalue, not a path
QPATH="$6"
PPATH="$7" # TODO why not QPATH?

export BLASTDB="$(dirname "$DBPATH")"
cd "$BLASTDB" # TODO remove?
DBNAME="$(basename "$DBPATH")"

# TODO query on stdin like the current haskell code?
# TODO should this .out + .err + mv thing be a regular practice everywhere?
# TODO and if so, maybe it should be done in haskell? only after it works a couple times the tedious way
$BLASTCMD $BLASTARGS -db "$DBNAME" -evalue "$EDEC" -outfmt 6 -query "$QPATH" > "${OUTPATH}.out" 2> "${OUTPATH}.err"
mv "${OUTPATH}.out" "$OUTPATH"
