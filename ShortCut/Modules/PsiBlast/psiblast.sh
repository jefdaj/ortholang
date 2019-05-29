#!/usr/bin/env bash

set -e

TPATH="$1"; shift
CDIR="$1" ; shift
QPATH="$1"; shift
ESTR="$1" ; shift
DBPRE="$1"; shift
ARGS="$@"

export BLASTDB="$CDIR"
psiblast -query "$QPATH" -evalue "$ESTR" -db "$DBPRE" $ARGS "$TPATH" > "${TPATH}.out" 2> "${TPATH}.err" && touch "$TPATH"
