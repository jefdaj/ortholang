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

# Shake requires that we write something to the actual outpath,
# and the stdout summary seems a reasonable way to go. it also makes showing the db easier:
makeblastdb -in "$FIXEDPATHS" -out "$DBOUT" -title "$TITLE" -dbtype "$DBTYPE" > "$DBOUT" 2> "${DBOUT}.err"

# remove empty stderr files
[ ! -s "${DBOUT}.err" ] && rm -f "${DBOUT}.err"
