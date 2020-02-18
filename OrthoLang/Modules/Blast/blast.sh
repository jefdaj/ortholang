#!/usr/bin/env bash

set -e

OUTDIR="$(dirname "$1")" # TODO fix this pattern
BLASTCMD="$2"
EDEC="$3" # this is the actual evalue, not a path
QPATH="$4"
PPATH="$5" # TODO why not QPATH?

DBDIR="$(dirname "$PPATH")"
DBNAME="$(basename "$PPATH")"

cd "$DBDIR" # TODO remove?
export BLASTDB="$DBNAME" # TODO DBPATH?

# TODO query on stdin like the current haskell code?
# TODO should this .out + .err + mv thing be a regular practice everywhere?
# TODO and if so, maybe it should be done in haskell? only after it works a couple times the tedious way
$BLASTCMD -db "$DBNAME" -evalue "$EDEC" -outfmt 6 -query "$QPATH" > "${OUTDIR}/out" 2> "${OUTDIR}/err"
if [[ -z "${OUTDIR}/out" ]]; then
  echo "<<emptybht>>" > "${OUTDIR}/result"
else
  cd "$OUTDIR"
  ln -s out result
fi
