#!/usr/bin/env bash

# TODO rename makeblastdb-all.sh?

DBOUT="$1"
FIXEDPATHS="$2" # TODO does the quoting work in all situations?
DBTYPE="$3"

TITLE="$(basename "$DBOUT")" # TODO basename as a dependency?

makeblastdb -in "$FIXEDPATHS" -out "$DBOUT" -title "$TITLE" -dbtype "$DBTYPE" > "${DBOUT}.out" 2> "${DBOUT}.err"
