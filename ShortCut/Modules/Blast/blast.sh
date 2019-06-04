#!/usr/bin/env bash

set -e

OUTPATH="$1"
BLASTCMD="$2"
EDEC="$3" # this is the actual evalue, not a path
QPATH="$4"
PPATH="$5" # TODO why not QPATH?

DBDIR="$(dirname "$PPATH")"
DBNAME="$(basename "$PPATH")"

export BLASTDB="$(dirname "$DBDIR")" # TODO DBPATH?
cd "$DBDIR" # TODO remove?

# TODO query on stdin like the current haskell code?
# TODO should this .out + .err + mv thing be a regular practice everywhere?
# TODO and if so, maybe it should be done in haskell? only after it works a couple times the tedious way
$BLASTCMD -db "$DBNAME" -evalue "$EDEC" -outfmt 6 -query "$QPATH" > "${OUTPATH}.out" 2> "${OUTPATH}.err"
if [[ -s "${OUTPATH}.out" ]]; then
  ln -s "$(basename ${OUTPATH}.out)" "$OUTPATH"
else
  echo "<<emptybht>>" > "$OUTPATH"
fi
