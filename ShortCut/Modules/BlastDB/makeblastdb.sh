#!/usr/bin/env bash

DBOUT="$1"
CDIR="$2" # TODO remove?
FIXEDPATHS="$3" # TODO does the quoting work in all situations?
DBTYPE="$4"
DBDIR="$(dirname "$DBOUT")"
TITLE="$(basename "$DBOUT")" # TODO basename as a dependency?

mkdir -p "$DBDIR"
cd "$CDIR"
makeblastdb -in "$FIXEDPATHS" -out "$DBOUT" -title "$TITLE" -dbtype "$DBTYPE"
