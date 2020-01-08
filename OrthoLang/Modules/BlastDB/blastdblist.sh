#!/usr/bin/env bash

# full db prefix is $TDIR/$DBNAME

set -e

TDIR="$1"
LISTTMP="$2"

cd "$TDIR"
blastdbget "$TDIR" > "$LISTTMP" 2> "${LISTTMP}.err"
ln -s "$LISTTMP" "${LISTTMP}.out"
