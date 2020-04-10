#!/usr/bin/env bash

# TODO rename makeblastdb-all.sh?

DBOUT="$1"
FIXEDPATHS="$2" # TODO does the quoting work in all situations?
DBTYPE="$3"
TITLE="$(basename $(dirname "$DBOUT"))" # TODO basename as a dependency?
DBOUT="$(dirname "$DBOUT")/result"

# echo "DBOUT: $DBOUT"
# echo "FIXEDPATHS: $FIXEDPATHS"
# echo "DBTYPE: $DBTYPE"
# echo "TITLE: $TITLE"

makeblastdb -in "$FIXEDPATHS" -out "$DBOUT" -title "$TITLE" -dbtype "$DBTYPE" > "${DBOUT}.out" 2> "${DBOUT}.err"
touch "$DBOUT" # TODO is this the best way?
