#!/usr/bin/env bash

# full db prefix is $TDIR/$DBNAME

set -e

TDIR="$1"
DBNAME="$2"

cd "$TDIR"
blastdbget -d "$DBNAME" . > "${DBNAME}.out" 2> "${DBNAME}.err"
